use crate::{burners::Burner, parse_smv_file};
use crate::{html::HtmlChild, html::HtmlElement, FdsFileExt};
use fds_input_parser::xb::HasXB;
use fds_input_parser::FdsFile;
use fds_input_parser::{decode::*, xb::MightHaveXB};
use std::ops::Deref;
use std::{
    cmp::Ordering,
    io::Read,
    path::{Path, PathBuf},
};

/// Verify both the input and the output and perform tests to see if the output
/// matches the input.
pub fn verify(smv_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let mut smv_contents = String::new();
    let mut f = std::fs::File::open(smv_path)?;
    f.read_to_string(&mut smv_contents)?;
    let smv_file = parse_smv_file(&smv_contents).expect("smv parsing failed").1;
    // Find the input file from the SMV file.
    let mut input_path = PathBuf::from(smv_path.parent().unwrap());
    input_path.push(smv_file.input_filename);
    let fds_data = fds_input_parser::parse_and_decode_fds_input_file(&input_path);
    let verification_result = verify_input(&fds_data);
    Ok(())
}

/// Verify an input file.
pub fn verify_input(fds_data: &FdsFile) -> VerificationResult {
    println!("{fds_data:#?}");
    VerificationResult::Tree(
        "Verification Tests".to_string(),
        vec![
            meshes_overlap_test(fds_data),
            reaction_tests(fds_data),
            burners_test(fds_data),
            parameters_test(fds_data),
            outputDataCoverage(fds_data),
            flowCoverage(fds_data),
            // leakage(fds_data),
            // devicesTest(fds_data),
            // spkDetCeilingTest(fds_data),
        ],
    )
}

pub fn print_verification_tree(tree: &VerificationResult, indent_level: usize) {
    match tree {
        VerificationResult::Result(name, res) => {
            let mut indentation = String::new();
            for _ in 0..indent_level {
                indentation.push(' ');
                indentation.push(' ');
            }
            print!("{}", indentation);
            match res {
                TestResult::Failure(s) => {
                    println!(
                        "{} [{}]: {}",
                        name,
                        ansi_term::Colour::Red.paint("Failure"),
                        s
                    )
                }
                TestResult::Warning(s) => {
                    println!(
                        "{} [{}]: {}",
                        name,
                        ansi_term::Colour::Yellow.paint("Warning"),
                        s
                    )
                }
                TestResult::Success(s) => {
                    println!(
                        "{} [{}]: {}",
                        name,
                        ansi_term::Colour::Green.paint("Success"),
                        s
                    )
                }
            }
        }
        VerificationResult::Tree(title, sub_tree) => {
            let mut indentation = String::new();
            for _ in 0..indent_level {
                indentation.push(' ');
                indentation.push(' ');
            }
            println!("{}{}:", indentation, title);
            for res in sub_tree {
                print_verification_tree(res, indent_level + 1)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ItemRef<'fds_data> {
    Id(&'fds_data str),
    Number(usize),
}

// pub trait HasId {
//     fn get_item_ref<'fds_data>(&'fds_data self, fds_data: &'fds_data FdsFile) -> ItemRef<'fds_data>;
// }

// impl HasId for Vent {
//     fn get_item_ref<'fds_data>(&'fds_data self, fds_data: &'fds_data FdsFile) -> ItemRef<'fds_data> {
//         self.id.unwrap_or_else
//     }
// }

pub enum VerificationTest {
    Test(Box<dyn Fn(&FdsFile) -> VerificationResult>),
    Tree(Vec<VerificationTest>),
}

#[derive(Debug, Clone)]
pub enum VerificationResult {
    Result(String, TestResult),
    Tree(String, Vec<VerificationResult>),
}

impl VerificationResult {
    pub fn worst_result(&self) -> Option<&TestResult> {
        match self {
            VerificationResult::Result(name, res) => Some(res),
            VerificationResult::Tree(_, res) => {
                let mut results: Vec<Option<&TestResult>> =
                    res.iter().map(|x| x.worst_result()).collect();
                results.sort();
                *(results.last()?)
            }
        }
    }
    pub fn to_html_outer(&self) -> HtmlElement {
        let mut ul = HtmlElement::new("ul".to_string());
        ul.attributes
            .insert("class".to_string(), "verification-list".to_string());
        ul.children.push(HtmlChild::Element(self.to_html()));
        ul
    }
    pub fn to_html(&self) -> HtmlElement {
        let mut li = HtmlElement::new("li".to_string());
        match self {
            VerificationResult::Result(name, res) => {
                let mut title = HtmlElement::new("strong".to_string());
                title.children.push(HtmlChild::String(name.to_string()));
                li.children.push(HtmlChild::Element(title));
                let mut category = HtmlElement::new("span".to_string());
                let colon = HtmlChild::String(":".to_string());
                match res {
                    TestResult::Failure(s) => {
                        category
                            .attributes
                            .insert("class".to_string(), "failure".to_string());
                        "[Failure]".to_string();
                        category
                            .children
                            .push(HtmlChild::String("[Failure]".to_string()));
                        li.attributes.insert(
                            "class".to_string(),
                            "test failureTest shown-test".to_string(),
                        );
                        li.attributes.insert(
                            "onclick".to_string(),
                            "toggle_visibility(arguments[0],this);".to_string(),
                        );
                        li.children.push(HtmlChild::Element(category));
                        li.children.push(colon);
                        li.children.push(HtmlChild::String(s.to_string()));
                    }
                    TestResult::Warning(s) => {
                        category
                            .attributes
                            .insert("class".to_string(), "warning".to_string());
                        "[Warning]".to_string();
                        category
                            .children
                            .push(HtmlChild::String("[Warning]".to_string()));
                        li.attributes.insert(
                            "class".to_string(),
                            "test warningTest shown-test".to_string(),
                        );
                        li.attributes.insert(
                            "onclick".to_string(),
                            "toggle_visibility(arguments[0],this);".to_string(),
                        );
                        li.children.push(HtmlChild::Element(category));
                        li.children.push(colon);
                        li.children.push(HtmlChild::String(s.to_string()));
                    }
                    TestResult::Success(s) => {
                        category
                            .attributes
                            .insert("class".to_string(), "success".to_string());
                        category
                            .children
                            .push(HtmlChild::String("[Success]".to_string()));
                        li.attributes.insert(
                            "class".to_string(),
                            "test successTest hidden-test".to_string(),
                        );
                        li.attributes.insert(
                            "onclick".to_string(),
                            "toggle_visibility(arguments[0],this);".to_string(),
                        );
                        li.children.push(HtmlChild::Element(category));
                        li.children.push(colon);
                        li.children.push(HtmlChild::String(s.to_string()));
                    }
                }
            }
            VerificationResult::Tree(name, sub_tree) => {
                // First we want to know if any of our children are failures, if so, the root is also a failure
                let mut div = HtmlElement::new("div".to_string());
                let mut title = HtmlElement::new("strong".to_string());
                title.children.push(HtmlChild::String(name.to_string()));
                let mut category = HtmlElement::new("span".to_string());
                let leaf_result = self.worst_result();
                match leaf_result {
                    Some(TestResult::Failure(_)) => {
                        category
                            .attributes
                            .insert("class".to_string(), "failure".to_string());
                        "[Failure]".to_string();
                        category
                            .children
                            .push(HtmlChild::String("[Failure]".to_string()));
                        li.attributes.insert(
                            "class".to_string(),
                            "test failureTest shown-test".to_string(),
                        );
                        li.attributes.insert(
                            "onclick".to_string(),
                            "toggle_visibility(arguments[0],this);".to_string(),
                        );
                    }
                    Some(TestResult::Warning(_)) => {
                        category
                            .attributes
                            .insert("class".to_string(), "warning".to_string());
                        "[Warning]".to_string();
                        category
                            .children
                            .push(HtmlChild::String("[Warning]".to_string()));
                        li.attributes.insert(
                            "class".to_string(),
                            "test warningTest shown-test".to_string(),
                        );
                        li.attributes.insert(
                            "onclick".to_string(),
                            "toggle_visibility(arguments[0],this);".to_string(),
                        );
                    }
                    Some(TestResult::Success(_)) | None => {
                        category
                            .attributes
                            .insert("class".to_string(), "success".to_string());
                        category
                            .children
                            .push(HtmlChild::String("[Success]".to_string()));
                        li.attributes.insert(
                            "class".to_string(),
                            "test successTest hidden-test".to_string(),
                        );
                        li.attributes.insert(
                            "onclick".to_string(),
                            "toggle_visibility(arguments[0],this);".to_string(),
                        );
                    }
                }
                div.children.push(HtmlChild::Element(title));
                div.children.push(HtmlChild::Element(category));
                div.children.push(HtmlChild::String(":".to_string()));
                li.children.push(HtmlChild::Element(div));
                let mut ul = HtmlElement::new("ul".to_string());
                for res in sub_tree {
                    ul.children.push(HtmlChild::Element(res.to_html()));
                }
                li.children.push(HtmlChild::Element(ul));
            }
        }
        li
    }
}

#[derive(Debug, Clone, Eq)]
pub enum TestResult {
    Success(String),
    Warning(String),
    Failure(String),
}

impl TestResult {
    fn index(&self) -> usize {
        match self {
            Self::Success(_) => 0,
            Self::Warning(_) => 1,
            Self::Failure(_) => 2,
        }
    }
}

impl Ord for TestResult {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index().cmp(&other.index())
    }
}

impl PartialOrd for TestResult {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for TestResult {
    fn eq(&self, other: &Self) -> bool {
        self.index() == other.index()
    }
}

// /// Check that the appropriate files are present.
// validateFilePresence :: [FilePath] -> [FilePath] -> [FilePath] -> IO Bool
// validateFilePresence relatedFiles' smvListedFiles' requiredFiles' = do
//     relatedFiles <- mapM canonicalizePath relatedFiles'
//     smvListedFiles <- mapM canonicalizePath smvListedFiles'
//     requiredFiles <- mapM canonicalizePath requiredFiles'
//     req <- mapM doesFileExist requiredFiles
//     smv <- mapM doesFileExist smvListedFiles
//     let excessFiles = relatedFiles \\ (smvListedFiles ++ requiredFiles)
//         exc = not $ null excessFiles
//     putStr "Required file presence: "
//     if all id req then putStrLn "Pass" else putStrLn "Fail"
//     putStr "SMV listed file presence: "
//     if all id smv then putStrLn "Pass" else putStrLn "Fail"
//     putStr "Presence of unlisted files: "
//     if exc then putStrLn "Pass" else putStrLn "Fail"
//     return $ all id [all id req, all id smv, exc]

// /// Get a list of files that are referenced in an SMV file.
// getSMVListedFiles :: FDSSimulation -> IO [FilePath]
// getSMVListedFiles simulation = do
//     smvData' <- parseSimulationSMVFile simulation
//     let smvData = case smvData' of
//             Left e -> error $ show e
//             Right x -> x
//     let dataFiles = smvDataFiles smvData
//         fileNames = map getDataFileName dataFiles
//         filePaths = map (\x-> joinPath [simDir simulation, x]) fileNames
//     return filePaths

// /// List all of the filenames in the simulation directory that match the CHID.
// gatherFilenames :: FDSSimulation -> IO [FilePath]
// gatherFilenames simulation = do
//     let pattern = compile ((simCHID simulation) ++ "*")
//     relatedFiles <- globDir [pattern] (simDir simulation)
//     return (concat relatedFiles)

fn ventHasFlow(fds_data: &FdsFile, vent: &Vent) -> bool {
    fn isLinkedToVent(vent: &Vent, hvac: &Hvac) -> bool {
        if let Some(vent_id) = vent.id.as_ref() {
            hvac.vent_id.as_ref() == Some(vent_id) || hvac.vent2_id.as_ref() == Some(vent_id)
        } else {
            false
        }
    }
    let linkedHVACs = fds_data
        .hvac
        .iter()
        .filter(|hvac| isLinkedToVent(vent, hvac));
    let isHVAC = linkedHVACs.count() != 0;
    let hasSurfFlow = vent.getSurfList(fds_data).iter().any(|s| surfHasFlow(s));
    println!("Vent: {:?} hasSurfFlow: {hasSurfFlow}", vent.id);
    isHVAC || hasSurfFlow
}

#[derive(Clone, Debug)]
pub enum SurfaceIdSpec<'fds_data> {
    Single(&'fds_data str),
    Triple(&'fds_data str, &'fds_data str, &'fds_data str),
    Six(
        &'fds_data str,
        &'fds_data str,
        &'fds_data str,
        &'fds_data str,
        &'fds_data str,
        &'fds_data str,
    ),
}

#[derive(Clone, Debug)]
pub enum SurfaceSpec<'fds_data> {
    Single(Option<&'fds_data Surf>),
    Triple(
        Option<&'fds_data Surf>,
        Option<&'fds_data Surf>,
        Option<&'fds_data Surf>,
    ),
    Six(
        Option<&'fds_data Surf>,
        Option<&'fds_data Surf>,
        Option<&'fds_data Surf>,
        Option<&'fds_data Surf>,
        Option<&'fds_data Surf>,
        Option<&'fds_data Surf>,
    ),
}

pub trait HasSurf {
    fn getSurfList<'fds_data>(
        &'fds_data self,
        fds_data: &'fds_data FdsFile,
    ) -> Vec<&'fds_data Surf>;

    fn getSurfs<'fds_data>(&'fds_data self, fds_data: &'fds_data FdsFile)
        -> SurfaceSpec<'fds_data>;

    fn getSurfIds(&self) -> SurfaceIdSpec;
}

impl HasSurf for Obst {
    fn getSurfList<'fds_data>(
        &'fds_data self,
        fds_data: &'fds_data FdsFile,
    ) -> Vec<&'fds_data Surf> {
        let surfs = match self.getSurfs(fds_data) {
            SurfaceSpec::Single(surf) => vec![surf],
            SurfaceSpec::Triple(surf1, surf2, surf3) => vec![surf1, surf2, surf3],
            SurfaceSpec::Six(surf1, surf2, surf3, surf4, surf5, surf6) => {
                vec![surf1, surf2, surf3, surf4, surf5, surf6]
            }
        };
        surfs.into_iter().flatten().collect()
    }

    fn getSurfs<'fds_data>(
        &'fds_data self,
        fds_data: &'fds_data FdsFile,
    ) -> SurfaceSpec<'fds_data> {
        getSurfsFromIds(fds_data, self.getSurfIds())
    }

    fn getSurfIds(&self) -> SurfaceIdSpec {
        if let Some(surf_id) = self.surf_id.as_deref() {
            SurfaceIdSpec::Single(surf_id)
        } else if let Some((a, b, c)) = self.surf_ids.as_ref() {
            SurfaceIdSpec::Triple(a, b, c)
        } else if let Some((a, b, c, d, e, f)) = self.surf_id6.as_ref() {
            SurfaceIdSpec::Six(a, b, c, d, e, f)
        } else {
            // TODO: be cautious of defaults here
            SurfaceIdSpec::Single("INERT")
        }
    }
}

impl HasSurf for Vent {
    fn getSurfList<'fds_data>(
        &'fds_data self,
        fds_data: &'fds_data FdsFile,
    ) -> Vec<&'fds_data Surf> {
        let surfs = match self.getSurfs(fds_data) {
            SurfaceSpec::Single(surf) => vec![surf],
            SurfaceSpec::Triple(surf1, surf2, surf3) => vec![surf1, surf2, surf3],
            SurfaceSpec::Six(surf1, surf2, surf3, surf4, surf5, surf6) => {
                vec![surf1, surf2, surf3, surf4, surf5, surf6]
            }
        };
        surfs.into_iter().flatten().collect()
    }

    // getSurfsObst :: HasSurf a => FDSFile -> a -> SurfaceSpec
    fn getSurfs<'fds_data>(
        &'fds_data self,
        fds_data: &'fds_data FdsFile,
    ) -> SurfaceSpec<'fds_data> {
        getSurfsFromIds(fds_data, self.getSurfIds())
    }

    fn getSurfIds(&self) -> SurfaceIdSpec<'_> {
        if let Some(surf_id) = self.surf_id.as_deref() {
            SurfaceIdSpec::Single(surf_id)
        } else {
            // TODO: be cautious of defaults here
            SurfaceIdSpec::Single("INERT")
        }
    }
}

fn getSurfsFromIds<'fds_data>(
    fds_data: &'fds_data FdsFile,
    idSpec: SurfaceIdSpec<'fds_data>,
) -> SurfaceSpec<'fds_data> {
    // where
    //     surfs = fdsFile_Surfs fdsData
    fn find<'fds_data>(
        fds_data: &'fds_data FdsFile,
        surf_id: &'fds_data str,
    ) -> Option<&'fds_data Surf> {
        let matching_surfs: Vec<_> = fds_data
            .surf
            .iter()
            .filter(|s| s.id.as_deref() == Some(surf_id))
            .collect();
        if matching_surfs.len() > 1 {
            // TODO: don't panic
            panic!("Multiple matching surfs")
        } else if let Some(s) = matching_surfs.first() {
            Some(*s)
        } else {
            None
        }
    }
    match idSpec {
        SurfaceIdSpec::Single(s1) => SurfaceSpec::Single(find(fds_data, s1)),
        SurfaceIdSpec::Triple(s1, s2, s3) => {
            SurfaceSpec::Triple(find(fds_data, s1), find(fds_data, s2), find(fds_data, s3))
        }
        SurfaceIdSpec::Six(s1, s2, s3, s4, s5, s6) => SurfaceSpec::Six(
            find(fds_data, s1),
            find(fds_data, s2),
            find(fds_data, s3),
            find(fds_data, s4),
            find(fds_data, s5),
            find(fds_data, s6),
        ),
    }
}

fn obstHasFlow(fds_data: &FdsFile, obst: &Obst) -> bool {
    let surfs = obst.getSurfList(fds_data);
    surfs.iter().any(|s| surfHasFlow(s))
}

fn surfHasFlow(surf: &Surf) -> bool {
    [
        surf.mlrpua.is_some(),
        // surf.mass_flux.is_some(),
        // surf.mass_flux_total.is_some(),
        // surf.mass_flux_var.is_some(),
        surf.hrrpua.is_some(),
        surf.vel.is_some(),
        // surf.vel_t.is_some(),
        surf.volume_flow.is_some(),
    ]
    .iter()
    .any(|s| *s)
}
// getFlowRate :: (HasXB a, HasSurf a) => FDSFile -> a -> Double
// getFlowRate fdsData burner =
//     let (xMin, xMax, yMin, yMax, zMin, zMax) =
//             getFlowRates fdsData burner
//     in sum [xMin, xMax, yMin, yMax, zMin, zMax]

// getFlowRates :: (HasXB a, HasSurf a) => FDSFile -> a
//     -> (Double, Double, Double, Double, Double, Double)
// getFlowRates fdsData nml =
//     -- TODO: this doesn't handle VENT namelists properly
//     let (xMinSurf, xMaxSurf, yMinSurf, yMaxSurf, zMinSurf, zMaxSurf) =
//             getSixSurfs fdsData nml
//         (xMinArea, xMaxArea, yMinArea, yMaxArea, zMinArea, zMaxArea) =
//             getSurfAreas fdsData nml
//     in
//         ( getFaceFlowRate fdsData xMinSurf xMinArea
//         , getFaceFlowRate fdsData xMaxSurf xMaxArea
//         , getFaceFlowRate fdsData yMinSurf yMinArea
//         , getFaceFlowRate fdsData yMaxSurf yMaxArea
//         , getFaceFlowRate fdsData zMinSurf zMinArea
//         , getFaceFlowRate fdsData zMaxSurf zMaxArea
//         )

/// Ensure that everage flow device is covered by a flow rate device. TODO: does not cover HVAC.
fn flowCoverage(fds_data: &FdsFile) -> VerificationResult {
    let name = "Flow Coverage Test".to_string();
    // it is also possible that other objects (such as OBST have flow)
    let vents = &fds_data.vent;
    let obsts = &fds_data.obst;
    let surfs = &fds_data.surf;
    // vents which may have a flow
    let ventsWithFlows: Vec<_> = vents
        .iter()
        .filter(|vent| ventHasFlow(fds_data, vent))
        .collect();
    // obsts that have surfaces with flows
    let obstWithFlows: Vec<_> = obsts
        .iter()
        .filter(|obst| obstHasFlow(fds_data, obst))
        .collect();
    // for each of the vents, ensure there is a flow device with the same
    // dimensions find those which do not
    let notCovered: Vec<_> = ventsWithFlows
        .iter()
        .filter(|vent| !hasFlowDevc(fds_data, vent))
        .collect();
    if notCovered.is_empty() {
        VerificationResult::Result(
            name,
            TestResult::Success("All Flows Vents and Obsts Measured".to_string()),
        )
    } else {
        let issues = notCovered
            .iter()
            .map(|vent| {
                VerificationResult::Result(
                    format!("Vent {:?} Flow Measurement", vent.id),
                    TestResult::Failure("No adequate flow measuring device".to_string()),
                )
            })
            .collect();
        VerificationResult::Tree(
            "The following objects have issues with their flow measurements".to_string(),
            issues,
        )
    }
}

fn leakage(fds_data: &FdsFile) -> VerificationResult {
    let testName = "Leakage Implementation Test".to_string();
    todo!()
    //     parts = fdsFile_Parts fdsData
    //     screenParts = filter isScreenPart parts
    //     isScreenPart part = case part_DRAG_LAW part of
    //         "SCREEN" -> True
    //         _ -> False
    //     hasInertOrDefaultSurf nml = case part_SURF_ID nml of
    //         Nothing -> True
    //         Just "INERT" -> True
    //         _ -> False
    // in if all (not . hasInertOrDefaultSurf) screenParts
    //         then Node (CompletedTest testName $ Success
    //             $ "No inert screens.")
    //             []
    //         else Node (CompletedTest testName $ Failure
    //             $ "PART uses the SCREEN drag law, but uses an INERT surface.")
    //             []
}
/// Ensure that no devices are stuck in solids.
fn devicesTest(fds_data: &FdsFile) -> VerificationResult {
    let testName = "Devices Stuck in Solids Test".to_string();
    todo!()
    //     stuckDevices = filter (fromMaybe False . stuckInSolid fdsData)
    //         $ fdsFile_Devcs fdsData
    // in if null stuckDevices
    //     then Node (CompletedTest testName $ Success $ "No stuck devices.") []
    //     else Node (CompletedTest testName $ Failure $ unlines
    //         $ map formatRes stuckDevices) []
    // where
    //     formatRes nml = "Device " <> getIdBound nml
    //         <> " is placed within a solid obstruction.\n    "
    //         -- <> T.unpack (pprint nml)
}
/// Ensure that sprinklers and smoke detectors are beneath a ceiling.
fn spkDetCeilingTest(fds_data: &FdsFile) -> VerificationResult {
    let testName = "Sprinklers and detectors immediately below ceiling".to_string();
    todo!()
    //     nonBeneathCeiling = filter
    //         (not . fromMaybe False . beneathCeiling fdsData)
    //         $ filter (\x-> isSprinkler fdsData x || isSmokeDetector fdsData x)
    //         $ fdsFile_Devcs fdsData
    // in if null nonBeneathCeiling
    //     then Node (CompletedTest testName $ Success $ "No distant devices.") []
    //     else Node (CompletedTest testName $ Failure $ unlines
    //         $ map formatRes nonBeneathCeiling) []
    // where
    //     formatRes nml = "Device " <> getIdBound nml
    //         <> " is not directly beneath the ceiling.\n    "
    //         -- <> T.unpack (pprint nml)
}
/// Take the xb dimensions of a vent and see if there is a flow vent with the
/// matching dimensions, or a device that references it as a duct node.
// hasFlowDevc :: FdsFile -> Vent -> Bool
fn hasFlowDevc(fds_data: &FdsFile, vent: &Vent) -> bool {
    let mut flow_devcs = fds_data
        .devc
        .iter()
        .filter(|devc| devc.quantity.as_deref() == Some("VOLUME FLOW"));
    let trackingFlowMatchingXB = flow_devcs.any(|devc| {
        if let (Some(xb_a), Some(xb_b)) = (vent.xb, devc.xb) {
            println!(
                "trackingFlowMatchingXB: Vent: {:?} devc: {:?}, {}",
                vent.id,
                devc.id,
                xb_a == xb_b
            );
            xb_a == xb_b
        } else {
            false
        }
    });
    // take only the devices which have a "DUCT_ID" parameter
    let ductIDDevices: Vec<_> = fds_data
        .devc
        .iter()
        .filter(|devc| devc.duct_id.is_some())
        .collect();
    // take only the devices where the "DUCT_ID" matches the flowing
    // namelist
    let relevantDuctIDDevices: Vec<_> = ductIDDevices
        .iter()
        .filter(|devc| {
            if let (Some(duct_id), (Some(flow_id))) = (devc.duct_id.as_deref(), vent.id.as_deref())
            {
                duct_id == flow_id
            } else {
                false
            }
        })
        .collect();
    // take only the devices that measure "DUCT VOLUME FLOW", and check that
    // the list is not null
    let trackingFlowViaDuctID = relevantDuctIDDevices
        .iter()
        .any(|devc| devc.quantity.as_deref() == Some("DUCT VOLUME FLOW"));
    println!(
            "Vent: {:?}\n  trackingFlowMatchingXB: {trackingFlowMatchingXB}\n  trackingFlowViaDuctID: {trackingFlowViaDuctID}",vent.id
        );
    trackingFlowMatchingXB || trackingFlowViaDuctID
}

// matchXBs :: (MightHaveXB a, MightHaveXB b) => a -> b -> Bool
// fn matchXbs(xb_a:Xb, xb_b:Xb) = case (tryGetXB nmlA, tryGetXB nmlB) of
//     (Just a, Just b) -> a == b
//     _ -> False

/// Check one obstruction and determine if it intersects any other namelists.
fn obst_intersects_with_others(fds_data: FdsFile) -> bool {
    todo!()
    // obstIntersectsWithOthers :: HasXB a => FdsFile -> a -> [Obst]
    // obstIntersectsWithOthers fdsData namelist =
    //     let
    //         obsts :: [Obst]
    //         obsts = fdsFile_Obsts fdsData
    //     in filter (nmlIntersect namelist) obsts
}

pub struct MeshIntersection {
    pub mesh_a: usize,
    pub mesh_b: usize,
}

impl MeshIntersection {
    pub fn new(i_a: usize, i_b: usize) -> Self {
        MeshIntersection {
            mesh_a: i_a,
            mesh_b: i_b,
        }
    }
}

/// Do any of the meshes overlap.
fn meshes_overlap_test(fds_data: &FdsFile) -> VerificationResult {
    // Clone a list of meshes.
    let mut meshes = fds_data.mesh.clone();
    let mut intersections = Vec::new();
    let mut index_a = 1;
    while let Some(mesh) = meshes.pop() {
        for (i, other_mesh) in meshes.iter().enumerate() {
            if mesh.intersect(&other_mesh) {
                intersections.push(MeshIntersection::new(index_a, i + 1))
            }
        }
        index_a += 1;
    }
    if intersections.is_empty() {
        VerificationResult::Result(
            "Mesh Intersections".to_string(),
            TestResult::Success("No Intersections".to_string()),
        )
    } else {
        let mut res = vec![];
        for intersection in intersections {
            res.push(VerificationResult::Result(
                "Mesh Intersections".to_string(),
                TestResult::Success(format!(
                    "Mesh {} intersects with {}",
                    intersection.mesh_a, intersection.mesh_b
                )),
            ));
        }
        VerificationResult::Tree("Mesh Intersections".to_string(), res)
    }
}

/// Test that the REAC properties are reasonable.
fn reaction_tests(fds_data: &FdsFile) -> VerificationResult {
    let soot_yield = soot_yield_test(fds_data);
    let co_yield = co_yield_test(fds_data);
    VerificationResult::Tree("Reaction Tests".to_string(), vec![soot_yield, co_yield])
}

fn soot_yield_test(fds_data: &FdsFile) -> VerificationResult {
    let name = "Soot Yield".to_string();
    let value = match fds_data.reac.len() {
        0 => {
            return VerificationResult::Result(
                name,
                TestResult::Failure("No Reaction Specified".to_string()),
            )
        }
        1 => fds_data.reac[0].soot_yield,
        _ => {
            return VerificationResult::Result(
                name,
                TestResult::Failure("Multiple Reactions Specified".to_string()),
            )
        }
    };
    if let Some(value) = value {
        if value == 0.1 || value == 0.07 {
            VerificationResult::Result(name, TestResult::Success(format!("{}", value)))
        } else {
            VerificationResult::Result(name, TestResult::Failure(format!("{}", value)))
        }
    } else {
        VerificationResult::Result(
            name,
            TestResult::Failure("No Soot Yield Specified".to_string()),
        )
    }
}

#[derive(Copy, Clone, Debug)]
pub enum COYieldTestSuccess {
    GoodValue(f64),
}

#[derive(Copy, Clone, Debug)]
pub enum COYieldTestFailure {
    NoReac,
    MultipleReacs,
    BadValue(f64),
    NoValue,
}

fn co_yield_test(fds_data: &FdsFile) -> VerificationResult {
    let name = "CO Yield".to_string();
    let value = match fds_data.reac.len() {
        0 => {
            return VerificationResult::Result(
                name,
                TestResult::Failure("No Reaction Specified".to_string()),
            )
        }
        1 => fds_data.reac[0].co_yield,
        _ => {
            return VerificationResult::Result(
                name,
                TestResult::Failure("Multiple Reactions Specified".to_string()),
            )
        }
    };
    if let Some(value) = value {
        if value == 0.05 {
            VerificationResult::Result(name, TestResult::Failure(format!("{}", value)))
        } else {
            VerificationResult::Result(name, TestResult::Success(format!("{}", value)))
        }
    } else {
        VerificationResult::Result(
            name,
            TestResult::Failure("No CO Yield Specified".to_string()),
        )
    }
}

fn parameters_test(fds_data: &FdsFile) -> VerificationResult {
    let name = "Input Verification Tests".to_string();
    let tests: Vec<fn(&FdsFile) -> VerificationResult> = vec![
        reac_tests, misc_tests, // , burnerTestsGroup
        dump_tests,
    ];
    let test_results = tests.into_iter().map(|test| test(fds_data)).collect();
    VerificationResult::Tree(name, test_results)
}

/// Test that the REAC properties are reasonable.
fn reac_tests(fds_data: &FdsFile) -> VerificationResult {
    let name = "REAC Properties".to_string();
    fn soot_yield_test(fds_data: &FdsFile, reac: &Reac) -> VerificationResult {
        let propName = "Soot Yield".to_string();
        let testName = "Soot Yield".to_string();
        let possibleValues = vec![0.07, 0.1];
        if let Some(value) = reac.soot_yield {
            if possibleValues.contains(&value) {
                VerificationResult::Result(
                    testName,
                    TestResult::Success(format!("{propName} was {value}, a recognised value.")),
                )
            } else {
                VerificationResult::Result(
                    testName,
                     TestResult::Failure(format!(
                         "{propName} was {value}, which is not one of the usual value of {possibleValues:?}."
                     )),
                 )
            }
        } else {
            VerificationResult::Result(
                testName,
                TestResult::Failure(format!("{propName} was not specified.")),
            )
        }
    }

    fn co_yield_test(fds_data: &FdsFile, reac: &Reac) -> VerificationResult {
        let propName = "CO Yield".to_string();
        let testName = "CO Yield".to_string();
        let possibleValues = vec![0.05];
        if let Some(value) = reac.soot_yield {
            if possibleValues.contains(&value) {
                VerificationResult::Result(
                    testName,
                    TestResult::Success(format!("{propName} was {value}, a recognised value.")),
                )
            } else {
                VerificationResult::Result(
                    testName,
                     TestResult::Failure(format!(
                         "{propName} was {value}, which is not one of the usual value of {possibleValues:?}."
                     )),
                 )
            }
        } else {
            VerificationResult::Result(
                testName,
                TestResult::Failure(format!("{propName} was not specified.")),
            )
        }
    }
    let tests: Vec<fn(&FdsFile, &Reac) -> VerificationResult> = vec![
        soot_yield_test,
        co_yield_test, // --           , chemicalFormula
    ];
    let specified_result = specified(fds_data, "REAC", |f| !f.reac.is_empty());
    let mut test_results = vec![specified_result];
    // TODO: deal with multiple REACs
    if let Some(reac) = fds_data.reac.first() {
        for test_result in tests.into_iter().map(|test| test(fds_data, reac)) {
            test_results.push(test_result);
        }
    }
    VerificationResult::Tree(name, test_results)
}

fn specified(fds_data: &FdsFile, nml: &str, exists: fn(&FdsFile) -> bool) -> VerificationResult {
    let name = format!("{nml} Namelist Specified");
    if exists(fds_data) {
        VerificationResult::Result(
            format!("{nml} Namelist Existence"),
            TestResult::Success(format!("{nml} namelist specified.")),
        )
    } else {
        VerificationResult::Result(
            format!("{nml} Namelist Existence"),
            TestResult::Failure(format!("No {nml} namelist not specified.")),
        )
    }
}

/// Test that the MISC properties are reasonable.
fn misc_tests(fds_data: &FdsFile) -> VerificationResult {
    let name = "MISC Properties".to_string();

    fn visibility_factor(fds_data: &FdsFile, misc: &Misc) -> VerificationResult {
        let name = "Visibility Factor".to_string();
        let vis = fds_data
            .misc
            .as_ref()
            .and_then(|misc| misc.visibility_factor);
        let visibility_factor = match vis {
            None => {
                return VerificationResult::Result(
                    "Visibility Factor Set".to_string(),
                    TestResult::Failure("Not Set".to_string()),
                )
            }
            Some(v) => v,
        };
        if visibility_factor == 3.0 || visibility_factor == 8.0 {
            VerificationResult::Result(
                "Visibility Factor Value".to_string(),
                TestResult::Success(format!(
                    "Visibility Factor is {visibility_factor}, a known value."
                )),
            )
        } else {
            VerificationResult::Result(
                "Visibility Factor Value".to_string(),
                TestResult::Failure(format!(
                    "Visibility Factor is {visibility_factor}. Known good visibility factors are 3 and 8."
                )),
            )
        }
    }

    fn maximum_visibility(fds_data: &FdsFile, misc: &Misc) -> VerificationResult {
        let vis = fds_data
            .misc
            .as_ref()
            .and_then(|misc| misc.maximum_visibility);

        let maximum_visibility = match vis {
            None => {
                return VerificationResult::Result(
                    "Maximum Visibility Set".to_string(),
                    TestResult::Failure("Not Set".to_string()),
                )
            }
            Some(v) => v,
        };
        if maximum_visibility <= 100.0 {
            VerificationResult::Result(
                "Maximum Visibility Value".to_string(),
                TestResult::Success(format!(
                    "Maximum Visibility is {maximum_visibility}, above 100."
                )),
            )
        } else {
            VerificationResult::Result(
                    "Maximum Visibility Value".to_string(),
                    TestResult::Failure(format!(
                        "Maximum Visibility is {maximum_visibility}. This is a low value and may cause issues when try to visualise results."
                    )),
                )
        }
    }
    let tests: Vec<fn(&FdsFile, &Misc) -> VerificationResult> =
        vec![visibility_factor, maximum_visibility];
    //       (summaryResults, testResults) = case (specified fdsData) of
    //         l@(Node (CompletedTest _ r@(Failure _)) _) -> (r, [])
    //         l@(Node (CompletedTest _ (Success _)) _) ->
    //           let
    //               Just misc = fdsFile_Misc fdsData
    //               testResults :: [Tree CompletedTest]
    //               testResults = pam (pam tests misc) fdsData
    //               summaryResults :: TestResult
    //               summaryResults = worstN testResults
    //           in (summaryResults, testResults) :: (TestResult, [Tree CompletedTest])
    //   in Node (CompletedTest testName summaryResults) testResults
    let specified_result = specified(fds_data, "MISC", |f| f.misc.as_ref().is_some());
    let mut test_results = vec![specified_result];
    if let Some(misc) = fds_data.misc.as_ref() {
        for test_result in tests.into_iter().map(|test| test(fds_data, misc)) {
            test_results.push(test_result);
        }
    }
    VerificationResult::Tree(name, test_results)
}
/// Test that the DUMP properties are reasonable.
fn dump_tests(fds_data: &FdsFile) -> VerificationResult {
    let name = "DUMP Properties".to_string();
    fn dt_restart_test(fds_data: &FdsFile, dump: &Dump) -> VerificationResult {
        let name = "Restart Interval".to_string();
        if let Some(ri) = dump.dt_restart {
            VerificationResult::Result(
                name,
                TestResult::Success(format!("Restart Interval is {ri}, any value is not set")),
            )
        } else {
            VerificationResult::Result(
                name,
                TestResult::Success("Restart Interval is not set".to_string()),
            )
        }
    }

    fn nframes_test(fds_data: &FdsFile, dump: &Dump) -> VerificationResult {
        let name = "Number of Frames".to_string();
        fn getSimTimes(fds_data: &FdsFile) -> (f64, f64) {
            if let Some(time) = fds_data.time {
                (time.t_begin.unwrap_or(0.0), time.t_end.unwrap_or(0.0))
            } else {
                (0.0, 1.0)
            }
        }
        if let Some(nframes) = dump.nframes {
            let (tStart, tEnd) = getSimTimes(fds_data);
            let simInterval = (tEnd - tStart).round() as i64;
            let s = simInterval % nframes;
            if s == 0 {
                VerificationResult::Result(
                    name,
                    TestResult::Success(format!(
                        "Value {nframes}, results in round number of frames"
                    )),
                )
            } else {
                VerificationResult::Result(
                    name,
                    TestResult::Failure(format!("Value {nframes} may result in clipped output")),
                )
            }
        } else {
            VerificationResult::Result(
                name,
                TestResult::Failure("NFRAMES not specified".to_string()),
            )
        }
    }
    //       (summaryResults, testResults) = case (specified fdsData) of
    //         l@(Node (CompletedTest _ r@(Failure _)) _) -> (r, [])
    //         l@(Node (CompletedTest _ (Success _)) _) ->
    //           let
    //               Just dump = fdsFile_Dump fdsData
    //               testResults :: [Tree CompletedTest]
    //               testResults = pam (pam tests dump) fdsData
    //               summaryResults :: TestResult
    //               summaryResults = worstN testResults
    //           in (summaryResults, testResults) :: (TestResult, [Tree CompletedTest])
    //   in Node (CompletedTest testName summaryResults) testResults
    let tests: Vec<fn(&FdsFile, &Dump) -> VerificationResult> = vec![dt_restart_test, nframes_test];
    let specified_result = specified(fds_data, "DUMP", |f| f.dump.as_ref().is_some());
    let mut test_results = vec![specified_result];
    if let Some(dump) = fds_data.dump.as_ref() {
        for test_result in tests.into_iter().map(|test| test(fds_data, dump)) {
            test_results.push(test_result);
        }
    }
    VerificationResult::Tree(name, test_results)
}

/// Test all burners.
fn burners_test(fds_data: &FdsFile) -> VerificationResult {
    let name = "Burner Tests".to_string();
    let burners = fds_data.burners();
    if burners.is_empty() {
        VerificationResult::Result(name, TestResult::Failure("No burners".to_string()))
    } else {
        let burner_test_results = burners
            .iter()
            .enumerate()
            .map(|(i, burner)| burner_test(fds_data, burner, i))
            .collect();
        VerificationResult::Tree(name, burner_test_results)
    }
}

/// Test a burner
fn burner_test(fds_data: &FdsFile, burner: &Burner, i: usize) -> VerificationResult {
    let source_froude = source_froude_test(burner);
    let ndr = ndr_test(fds_data, burner);
    let growth_rate = growth_rate_test(fds_data, burner);
    // let intersection = intersection_test(fds_data, burner);
    VerificationResult::Tree(
        format!(
            "Burner Test #{} ({})",
            i,
            burner.name.as_deref().unwrap_or("Unnamed")
        ),
        vec![
            source_froude,
            ndr,
            growth_rate,
            // intersection
        ],
    )
}

fn source_froude_test(burner: &Burner) -> VerificationResult {
    let name = "Source Froude".to_string();
    let max_threshold = 2.5_f64;
    let source_froude = burner.source_froude();
    if source_froude <= max_threshold {
        VerificationResult::Result(name, TestResult::Success(format!("{}", source_froude)))
    } else {
        VerificationResult::Result(name, TestResult::Failure(format!("{}", source_froude)))
    }
}

fn ndr_test(fds_data: &FdsFile, burner: &Burner) -> VerificationResult {
    let name = "Non-Dimensionalised Ratio".to_string();
    let ndrs = burner.ndr();
    if ndrs.len() == 1 {
        VerificationResult::Result(name, ndr_res(ndrs[0]))
    } else {
        let ndr_test_results = ndrs.into_iter().map(ndr_res).enumerate();
        let mut res = Vec::new();
        for (i, ndr_t) in ndr_test_results {
            res.push(VerificationResult::Result(format!("Panel {}", i), ndr_t));
        }
        VerificationResult::Tree(name, res)
    }
}

fn ndr_res(ndr: f64) -> TestResult {
    if ndr <= 4_f64 {
        TestResult::Success(format!("{}", ndr))
    } else {
        TestResult::Failure(format!("{}", ndr))
    }
}

fn intersection_test(fds_data: &FdsFile, burner: &Burner) -> VerificationResult {
    todo!()
}

/// Test the growth rate of a burner and check that it either matches a standard
/// growth rate, or a steady-state value within 20 s.
fn growth_rate_test(fds_data: &FdsFile, burner: &Burner) -> VerificationResult {
    // TODO: This requires understanding the burner and it's exposed surfaces
    // TODO: allow steady state curves
    let name = "Growth Rate".to_string();
    let tau_qs: Vec<_> = burner
        .panels
        .iter()
        .flat_map(|panel| panel.tau_q())
        .collect();
    let tau_q = if let Some(tau_q) = tau_qs.first() {
        *tau_q
    } else {
        return VerificationResult::Result(
            name,
            TestResult::Warning("No growth rate specified".to_string()),
        );
    };
    if !tau_qs.iter().all(|x| *x == tau_q) {
        // If all TAU_Qs are no the same, test fails
        return VerificationResult::Result(
            name,
            TestResult::Failure("Multiple different TAU_Q values".to_string()),
        );
    }
    let alpha = burner.max_hrr() / tau_q.abs().powi(2);

    let std_growth_rates = vec![
        crate::hrrs::GrowthRate::NFPASlow,
        crate::hrrs::GrowthRate::NFPAFast,
        crate::hrrs::GrowthRate::NFPAMedium,
        crate::hrrs::GrowthRate::NFPAUltrafast,
        crate::hrrs::GrowthRate::EurocodeSlow,
        crate::hrrs::GrowthRate::EurocodeMedium,
        crate::hrrs::GrowthRate::EurocodeFast,
        crate::hrrs::GrowthRate::EurocodeUltrafast,
    ];

    let std_growth_diffs: Vec<_> = std_growth_rates
        .into_iter()
        .map(|std_alpha| ((alpha - std_alpha.alpha()) / std_alpha.alpha()).abs())
        .collect();
    let mut min_diff = std_growth_diffs[0];
    for growth_diff in std_growth_diffs {
        if growth_diff < min_diff {
            min_diff = growth_diff;
        }
    }
    if min_diff < 0.01 {
        VerificationResult::Result(
            name,
            TestResult::Success("Alpha matches standard value".to_string()),
        )
    } else {
        VerificationResult::Result(
            name,
            TestResult::Failure("Alpha value deviates from standard values".to_string()),
        )
    }
}

//     /// Test that the burner does not intersect with any other obstructions.
//     intersectionTest :: Burner -> FdsFile -> Tree CompletedTest
//     intersectionTest burner fdsData = case getBurnerId burner of
//         // TODO: we should be able to uniquely identify each OBST
//         Nothing -> Node (CompletedTest testName $ Failure
//             $ "Cannot test burner intersection as burner does not have a name.")
//             []
//         Just burnerId ->
//             let
//                 isBurner nml = case getId nml of
//                     Nothing -> False
//                     Just x -> x == burnerId
//                 intersectsWith = filter (not . isBurner)
//                     $ obstIntersectsWithOthers fdsData burner
//             in if null intersectsWith
//                 then Node (CompletedTest testName $ Success
//                     $ "Burner does not intersect with other obstructions.") []
//                 else Node (CompletedTest testName $ Failure
//                     $ "Burner intersects wth the following obstructions: \n"
//                         ++ unlines (map (\nml-> indent $ (fromMaybe "(unknown)"
//                         $ getId nml)
//                         {- ++ " at " ++ showSourcePose nml -}) intersectsWith))
//                         []
//         where
//             testName = "Intersection"

// showSourcePose nml = "Line " <> show (sourceLine pos) <> ", Column "
//     <> show (sourceColumn pos) <> " of input file"
//     where pos = nml_location nml

// indent string = "--" ++ string
// sprinklerTestsGroup :: NamelistFile -> Tree CompletedTest
// sprinklerTestsGroup = \fdsData ->
//   let
//     testName = "Sprinklers"
//     sprinklers = getSprinklerDevcs fdsData
//     completedTests = map (sprinklerTestsIndividual fdsData) sprinklers
//   in case completedTests of
//     [] -> Node (CompletedTest testName (Warning "No burners present.")) completedTests
//     _  -> Node (CompletedTest testName (worstN completedTests)) completedTests

///Tests to apply to the various burners found in a model.
fn sprinkler_test() {
    unimplemented!()
    // sprinklerTestsIndividual :: NamelistFile -> Namelist -> Tree CompletedTest
    // sprinklerTestsIndividual fdsData sprinkler =
    //   let
    //       testName = "Sprinklers Tests for " ++ sprinklerName
    //       tests = pam tests' sprinkler
    //       testResults = pam tests fdsData
    //       summaryResults = worstN testResults
    //   in Node (CompletedTest testName summaryResults) testResults
    //   where
    //     tests' :: [(Namelist -> NamelistFile -> Tree CompletedTest)]
    //     tests' =
    //       [ temperatureTest
    //       ]
    //     sprinklerName = getIDBound sprinkler
}

pub struct Sprinkler {
    pub devc: Devc,
    pub prop: Prop,
}

impl Sprinkler {
    pub fn from_devc(devc: Devc, fds_file: &FdsFile) -> Self {
        let prop = {
            fds_file
                .prop
                .iter()
                .find(|&prop| prop.id.as_ref() == devc.prop_id.as_ref())
                .unwrap()
                .clone()
        };
        Self { devc, prop }
    }
    pub fn activation_temperature(&self) -> Option<f64> {
        self.prop.activation_temperature
    }
}

pub struct SmokeDetector {
    pub devc: Devc,
    pub prop: Prop,
}

impl SmokeDetector {
    pub fn from_devc(devc: Devc, fds_file: &FdsFile) -> Self {
        // let prop = {
        //     fds_file.props.iter().find(|&&prop| prop.id.as_ref() == devc.prop_id.as_ref()).unwrap().clone()
        // };
        // Self {
        //     devc,
        //     prop,
        // }
        todo!()
    }
    pub fn obscuration(&self) -> f64 {
        // self.prop.activation_temperature
        todo!()
    }
}

pub struct ThermalDetector {
    pub devc: Devc,
    pub prop: Prop,
}

impl ThermalDetector {
    pub fn from_devc(devc: Devc, fds_file: &FdsFile) -> Self {
        todo!()
        // let prop = {
        //     fds_file.props.iter().find(|&&prop| prop.id.as_ref() == devc.prop_id.as_ref()).unwrap().clone()
        // };
        // Self {
        //     devc,
        //     prop,
        // }
    }
    pub fn activation_temperature(&self) -> f64 {
        // self.prop.activation_temperature
        todo!()
    }
}

fn sprinkler_activation_temperature_test() {
    unimplemented!()
    //     temperatureTest :: Namelist -> NamelistFile -> Tree CompletedTest
    //     temperatureTest sprinkler fdsData =
    //       let
    //         testName = "Activation Temperature"
    //         q = maxBurnerHRR fdsData burner
    //         sF = sourceFroude q fuelArea
    //       in if sF <= maxThreshold
    //         then Node (CompletedTest testName $ Success $ "Conforms. " ++ show sF ++ " <= " ++ show maxThreshold) []
    //         else Node (CompletedTest testName $ Failure $ "Does not conform.") []
    //       where
    //         fuelArea = burnerArea burner
    //         maxThreshold = 2.5
}

// GUIDELINES

fn outputDataCoverage(fds_data: &FdsFile) -> VerificationResult {
    let name = "Output Data Coverage".to_string();
    let tests: Vec<fn(&FdsFile) -> VerificationResult> =
        vec![coDataCoverage, tempDataCoverage, visDataCoverage];
    let mut test_results = vec![];
    for test_result in tests.into_iter().map(|test| test(fds_data)) {
        test_results.push(test_result);
    }
    VerificationResult::Tree(name, test_results)
}

fn dataCoverage(fds_data: &FdsFile, value: &str, f: fn(&&Slcf) -> bool) -> VerificationResult {
    let name = format!("{value} Data Coverage");
    // Get all the slices relevant to this output type.
    let slices: Vec<&Slcf> = fds_data.slcf.iter().filter(f).collect();
    let tests: Vec<fn(&[&Slcf]) -> VerificationResult> =
        vec![xAxisCoverage, yAxisCoverage, zAxisCoverage];
    let mut test_results = vec![];
    for test_result in tests.into_iter().map(|test| test(&slices)) {
        test_results.push(test_result);
    }
    VerificationResult::Tree(name, test_results)
}

/// Carbon monoxide data coverage test.
fn coDataCoverage(fds_data: &FdsFile) -> VerificationResult {
    let f = |slice: &&Slcf| {
        slice.spec_id.as_deref() == Some("CARBON MONOXIDE")
            && slice.quantity.as_deref() == Some("VOLUME FRACTION")
    };
    dataCoverage(fds_data, "CO", f)
}

/// Tempearature data coverage test.
fn tempDataCoverage(fds_data: &FdsFile) -> VerificationResult {
    let f = |slice: &&Slcf| slice.quantity.as_deref() == Some("TEMPERATURE");
    dataCoverage(fds_data, "Temperature", f)
}

/// Soot Visibility data coverage test.
fn visDataCoverage(fds_data: &FdsFile) -> VerificationResult {
    let f = |slice: &&Slcf| slice.quantity.as_deref() == Some("VISIBILITY");
    dataCoverage(fds_data, "Visibility", f)
}

/// X-Axis coverage test
fn xAxisCoverage(slices: &[&Slcf]) -> VerificationResult {
    axisCoverage(slices, "X", |s| s.pbx.is_some())
}

/// Y-Axis coverage test
fn yAxisCoverage(slices: &[&Slcf]) -> VerificationResult {
    axisCoverage(slices, "Y", |s| s.pby.is_some())
}

/// Z-Axis coverage test
fn zAxisCoverage(slices: &[&Slcf]) -> VerificationResult {
    axisCoverage(slices, "Z", |s| s.pbz.is_some())
}

fn axisCoverage(slices: &[&Slcf], axis: &str, f: fn(&&Slcf) -> bool) -> VerificationResult {
    let name = format!("{axis} Axis Coverage");
    // TODO: check that the value for PB* is within the bounds of the model
    if slices.iter().any(f) {
        VerificationResult::Result(
            name,
            TestResult::Success(format!(
                "Full {axis} axis coverage of this value is present."
            )),
        )
    } else {
        VerificationResult::Result(
            name,
            TestResult::Failure(format!(
                "Full {axis} axis coverage of this value is not present."
            )),
        )
    }
}
