use crate::{burners::Burner, parse_smv_file};
use crate::{html::HtmlChild, html::HtmlElement, FDSFileExt};
use fds_input_parser::xb::HasXB;
use fds_input_parser::FDSFile;
use fds_input_parser::{decode::*, xb::MightHaveXB};
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
pub fn verify_input(fds_data: &FDSFile) -> VerificationResult {
    // let verification_tests = vec![
    //     parameterVerificationTests
    //     , outputDataCoverage
    //     , meshOverlapTests
    //     , flowCoverage
    //     , leakage
    //     , devicesTest
    //     , spkDetCeilingTest
    // ];
    VerificationResult::Tree(
        "Verification Tests".to_string(),
        vec![
            meshes_overlap_test(&fds_data),
            reaction_tests(&fds_data),
            burners_test(&fds_data),
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

pub enum VerificationTest {
    Test(Box<dyn Fn(&FDSFile) -> VerificationResult>),
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
                    res.into_iter().map(|x| x.worst_result()).collect();
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
                let s = match res {
                    TestResult::Failure(s) => {
                        li.attributes.insert(
                            "class".to_string(),
                            "test failureTest shown-test".to_string(),
                        );
                        li.attributes.insert(
                            "onclick".to_string(),
                            "toggle_visibility(arguments[0],this);".to_string(),
                        );
                        format!("{} [{}]: {}", name, "Failure", s)
                    }
                    TestResult::Warning(s) => {
                        li.attributes.insert(
                            "class".to_string(),
                            "test warningTest shown-test".to_string(),
                        );
                        li.attributes.insert(
                            "onclick".to_string(),
                            "toggle_visibility(arguments[0],this);".to_string(),
                        );
                        format!("{} [{}]: {}", name, "Warning", s)
                    }
                    TestResult::Success(s) => {
                        li.attributes.insert(
                            "class".to_string(),
                            "test successTest hidden-test".to_string(),
                        );
                        li.attributes.insert(
                            "onclick".to_string(),
                            "toggle_visibility(arguments[0],this);".to_string(),
                        );
                        format!("{} [{}]: {}", name, "Success", s)
                    }
                };

                li.children.push(HtmlChild::String(s));
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

// <div>
//                                     <strong>
//                                         Burners
//                                     </strong>
//                                     <span class="failure">
//                                         [Failed]
//                                     </span>
//                                     :
//                                     <ul>
//                                     </ul>
//                                 </div>

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

// /// Ensure that everage flow device is covered by a flow rate device/
// flowCoverage fdsData =
//     let
//         testName = "Flow Coverage Test"
//         // it is also possible that other objects (such as OBST have flow)
//         vents = fdsFile_Vents fdsData
//         obsts = fdsFile_Obsts fdsData
//         surfs = fdsFile_Surfs fdsData
//         // vents which may have a flow
//         ventsWithFlows = filter (ventHasFlow fdsData) vents
//         // obsts that have surfaces with flows
//         obstWithFlows = filter (obstHasFlow fdsData) vents
//         // for each of the vents, ensure there is a flow device with the same
//         // dimensions find those which do not
//         notCovered =  filter (not . (hasFlowDevc fdsData)) ventsWithFlows
//     in if null notCovered
//             then Node (CompletedTest testName $ Success
//                 $ "All flow devices have devcs.") []
//             else Node (CompletedTest testName $ Failure $ unlines
//                 $ map formatRes notCovered) []
//     where
//         formatRes nml = "Flow object " <> getIdBound nml
//             <> " does not have a flow tracking devices.\n    "
//             // ++ T.unpack (pprint nml)

// leakage fdsData =
//     let
//         testName = "Leakage Implementation Test"
//         parts = fdsFile_Parts fdsData
//         screenParts = filter isScreenPart parts
//         isScreenPart part = case part_DRAG_LAW part of
//             "SCREEN" -> True
//             _ -> False
//         hasInertOrDefaultSurf nml = case part_SURF_ID nml of
//             Nothing -> True
//             Just "INERT" -> True
//             _ -> False
//     in if all (not . hasInertOrDefaultSurf) screenParts
//             then Node (CompletedTest testName $ Success
//                 $ "No inert screens.")
//                 []
//             else Node (CompletedTest testName $ Failure
//                 $ "PART uses the SCREEN drag law, but uses an INERT surface.")
//                 []

// /// Ensure that no devices are stuck in solids.
// devicesTest :: FDSFile -> Tree CompletedTest
// devicesTest fdsData =
//     let
//         testName = "Devices Stuck in Solids Test"
//         stuckDevices = filter (fromMaybe False . stuckInSolid fdsData)
//             $ fdsFile_Devcs fdsData
//     in if null stuckDevices
//         then Node (CompletedTest testName $ Success $ "No stuck devices.") []
//         else Node (CompletedTest testName $ Failure $ unlines
//             $ map formatRes stuckDevices) []
//     where
//         formatRes nml = "Device " <> getIdBound nml
//             <> " is placed within a solid obstruction.\n    "
//             -- <> T.unpack (pprint nml)

// /// Ensure that sprinklers and smoke detectors are beneath a ceiling.
// spkDetCeilingTest :: FDSFile -> Tree CompletedTest
// spkDetCeilingTest fdsData =
//     let
//         testName = "Sprinklers and detectors below ceiling"
//         nonBeneathCeiling = filter
//             (not . fromMaybe False . beneathCeiling fdsData)
//             $ filter (\x-> isSprinkler fdsData x || isSmokeDetector fdsData x)
//             $ fdsFile_Devcs fdsData
//     in if null nonBeneathCeiling
//         then Node (CompletedTest testName $ Success $ "No distant devices.") []
//         else Node (CompletedTest testName $ Failure $ unlines
//             $ map formatRes nonBeneathCeiling) []
//     where
//         formatRes nml = "Device " <> getIdBound nml
//             <> " is not directly beneath the ceiling.\n    "
//             -- <> T.unpack (pprint nml)

// /// Take the xb dimensions of a vent and see if there is a flow vent with the
// /// matching dimensions, or a device that references it as a duct node.
// hasFlowDevc :: FDSFile -> Vent -> Bool
// hasFlowDevc fdsData namelist =
//     let
//         devcs = filter
//             (\nml->devc_QUANTITY nml == (Just "VOLUME FLOW"))
//             (fdsFile_Devcs fdsData)
//         trackingFlowMatchingXB = any (matchXBs namelist) devcs
//         // get all the devices
//         allDevcs = fdsFile_Devcs fdsData
//         // take only the devices which have a "DUCT_ID" parameter
//         ductIDDevices = filter (isJust . devc_DUCT_ID) allDevcs
//         // take only the devices where the "DUCT_ID" matches the flowing
//         // namelist
//         relevantDuctIDDevices = filter (\nml-> (Just True) == (do
//             ductId <- devc_DUCT_ID nml
//             flowId <- getId nml
//             pure (ductId == flowId))) ductIDDevices
//         // take only the devices that measure "DUCT VOLUME FLOW", and check that
//         // the list is not null
//         trackingFlowViaDuctID = not $ null $ filter
//             (\nml->devc_QUANTITY nml == (Just "DUCT VOLUME FLOW")) allDevcs
//     in trackingFlowMatchingXB || trackingFlowViaDuctID

// / Check one obstruction and determine if it intersects any other namelists.
// fn obst_intersects_with_others(fds_data: FDSFile, other: N) -> bool {

// }
// obstIntersectsWithOthers :: HasXB a => FDSFile -> a -> [Obst]
// obstIntersectsWithOthers fdsData namelist =
//     let
//         obsts :: [Obst]
//         obsts = fdsFile_Obsts fdsData
//     in filter (nmlIntersect namelist) obsts

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
fn meshes_overlap_test(fds_data: &FDSFile) -> VerificationResult {
    // Clone a list of meshes.
    let mut meshes = fds_data.meshes.clone();
    let mut intersections = Vec::new();
    let mut index_a = 1;
    loop {
        if let Some(mesh) = meshes.pop() {
            for (i, other_mesh) in meshes.iter().enumerate() {
                if mesh.intersect(&other_mesh) {
                    intersections.push(MeshIntersection::new(index_a, i + 1))
                }
            }
        } else {
            break;
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
fn reaction_tests(fds_data: &FDSFile) -> VerificationResult {
    let soot_yield = soot_yield_test(fds_data);
    let co_yield = co_yield_test(fds_data);
    VerificationResult::Tree("Reaction Tests".to_string(), vec![soot_yield, co_yield])
}

fn soot_yield_test(fds_data: &FDSFile) -> VerificationResult {
    let name = "Soot Yield".to_string();
    let value = match fds_data.reacs.len() {
        0 => {
            return VerificationResult::Result(
                name,
                TestResult::Failure("No Reaction Specified".to_string()),
            )
        }
        1 => fds_data.reacs[0].soot_yield,
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

fn co_yield_test(fds_data: &FDSFile) -> VerificationResult {
    let name = "CO Yield".to_string();
    let value = match fds_data.reacs.len() {
        0 => {
            return VerificationResult::Result(
                name,
                TestResult::Failure("No Reaction Specified".to_string()),
            )
        }
        1 => fds_data.reacs[0].co_yield,
        _ => {
            return VerificationResult::Result(
                name,
                TestResult::Failure("Multiple Reactions Specified".to_string()),
            )
        }
    };
    if let Some(value) = value {
        if value == 0.05 {
            VerificationResult::Result(name, TestResult::Success(format!("{}", value)))
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

#[derive(Copy, Clone, Debug)]
pub struct MiscTests {
    pub visibility_factor: Result<VisibilityFactorTestSuccess, VisibilityFactorTestFailure>,
    pub maximum_visibility: Result<MaximumVisibilityTestSuccess, MaximumVisibilityTestFailure>,
}

fn misc_tests(fds_data: &FDSFile) -> MiscTests {
    let visibility_factor = visibility_factor_test(fds_data);
    let maximum_visibility = maximum_visibility_test(fds_data);
    MiscTests {
        visibility_factor,
        maximum_visibility,
    }
}

#[derive(Copy, Clone, Debug)]
pub enum VisibilityFactorTestSuccess {
    GoodValue(f64),
}

#[derive(Copy, Clone, Debug)]
pub enum VisibilityFactorTestFailure {
    NoMisc,
    BadValue(f64),
}

fn visibility_factor_test(
    fds_data: &FDSFile,
) -> Result<VisibilityFactorTestSuccess, VisibilityFactorTestFailure> {
    let visibility_factor = match fds_data.misc {
        None => Err(VisibilityFactorTestFailure::NoMisc),
        Some(ref misc) => Ok(misc.visibility_factor),
    }?;
    if visibility_factor == 3.0 || visibility_factor == 8.0 {
        Ok(VisibilityFactorTestSuccess::GoodValue(visibility_factor))
    } else {
        Err(VisibilityFactorTestFailure::BadValue(visibility_factor))
    }
}

#[derive(Copy, Clone, Debug)]
pub enum MaximumVisibilityTestSuccess {
    GoodValue(f64),
}

#[derive(Copy, Clone, Debug)]
pub enum MaximumVisibilityTestFailure {
    NoMisc,
    BadValue(f64),
}

fn maximum_visibility_test(
    fds_data: &FDSFile,
) -> Result<MaximumVisibilityTestSuccess, MaximumVisibilityTestFailure> {
    let maximum_visibility = match fds_data.misc {
        None => Err(MaximumVisibilityTestFailure::NoMisc),
        Some(ref misc) => Ok(misc.maximum_visibility),
    }?;
    if maximum_visibility <= 100.0 {
        Ok(MaximumVisibilityTestSuccess::GoodValue(maximum_visibility))
    } else {
        Err(MaximumVisibilityTestFailure::BadValue(maximum_visibility))
    }
}

fn dump_tests(fds_data: &FDSFile) {
    let dt_restart_result = dt_restart_test(fds_data);
    let nframes = nframes_test(fds_data);
    unimplemented!()
}

fn dt_restart_test(fds_data: &FDSFile) {
    unimplemented!()
    //       dt_restart :: Dump -> FDSFile -> Tree CompletedTest
    //       dt_restart dump fdsData =
    //           let
    //               testName = "Restart Interval"
    //               nValue = dump_DT_RESTART dump
    //           in Node (CompletedTest testName $ Success
    //             $ "Value: " ++ show nValue ++ ".") []
}

fn nframes_test(fds_data: &FDSFile) -> VerificationResult {
    unimplemented!()
    //       nframes :: Dump -> FDSFile -> Tree CompletedTest
    //       nframes dump fdsData =
    //           let
    //               testName = "Number of Frames"
    //               nValue = dump_NFRAMES dump
    //           in if (mod (round simInterval :: Int) nValue)  == 0
    //                 -- TODO: check that simTime is whole number
    //               then Node (CompletedTest testName $ Success
    //                 $ "Value: " ++ show nValue ++ ".") []
    //               else Node (CompletedTest testName $ Success
    //                 $ "Value of " ++ show nValue
    //                 ++ " may result in clipped output.") []
}

/// Test all burners.
fn burners_test(fds_data: &FDSFile) -> VerificationResult {
    let name = "Burner Tests".to_string();
    let burners = fds_data.burners();
    if burners.is_empty() {
        VerificationResult::Result(name, TestResult::Failure("No burners".to_string()))
    } else {
        let burner_test_results = burners
            .iter()
            .enumerate()
            .map(|(i, burner)| burner_test(fds_data, &burner, i))
            .collect();
        VerificationResult::Tree(name, burner_test_results)
    }
}

/// Test a burner
fn burner_test(fds_data: &FDSFile, burner: &Burner, i: usize) -> VerificationResult {
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

fn ndr_test(fds_data: &FDSFile, burner: &Burner) -> VerificationResult {
    let name = "Non-Dimensionalised Ratio".to_string();
    let ndrs = burner.ndr();
    if ndrs.len() == 1 {
        VerificationResult::Result(name, ndr_res(ndrs[0]))
    } else {
        let ndr_test_results = ndrs.into_iter().map(|ndr| ndr_res(ndr)).enumerate();
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

fn intersection_test(fds_data: &FDSFile, burner: &Burner) -> VerificationResult {
    todo!()
}

/// Test the growth rate of a burner and check that it either matches a standard
/// growth rate, or a steady-state value within 20 s.
fn growth_rate_test(fds_data: &FDSFile, burner: &Burner) -> VerificationResult {
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
//     intersectionTest :: Burner -> FDSFile -> Tree CompletedTest
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
    pub fn from_devc(devc: Devc, fds_file: &FDSFile) -> Self {
        let prop = {
            fds_file
                .props
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
    pub fn from_devc(devc: Devc, fds_file: &FDSFile) -> Self {
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
    pub fn from_devc(devc: Devc, fds_file: &FDSFile) -> Self {
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

//--GUIDELINES
//
// outputDataCoverage :: FDSFile -> Tree CompletedTest
// outputDataCoverage fdsData =
//     let
//         testName = "Output Data Coverage"
//         outputSlices = fdsFile_Slcfs fdsData
//         tests =
//             [ coDataCoverage
//             , tempDataCoverage
//             , visDataCoverage
//             ]
//         testResults = pam (pam tests outputSlices) fdsData
//         summaryResults = worstN testResults
//   in Node (CompletedTest testName summaryResults) testResults

// genericDataCoverageTest slices =
// let relSlices = filter
// (\slice
// -> hasParameterValue "SPEC_ID" "carbon monoxide" slice
// && hasParameterValue "QUANTITY" "VOLUME FRACTION" slice
// ) slices
// in TestGroup (name ++ " Data Coverage")
// [ xAxisCoverage relSlices
// , yAxisCoverage relSlices
// , zAxisCoverage relSlices
// ]

// // |Carbon monoxide data coverage test.
// coDataCoverage slices fdsData =
//     let
//         testName = "CO Data Coverage"
//         coSlices = filter
//             (\slice
//             -> slcf_SPEC_ID slice == Just "CARBON MONOXIDE"
//             && slcf_QUANTITY slice == Just "VOLUME FRACTION"
//             ) slices
//         tests =
//             [ xAxisCoverage
//             , yAxisCoverage
//             , zAxisCoverage
//             ]
//         testResults = pam (pam tests coSlices) fdsData
//         summaryResults = worstN testResults
//     in Node (CompletedTest testName summaryResults) testResults

// // |Tempearature data coverage test.
// tempDataCoverage slices fdsData =
//     let
//         testName = "Temperature Data Coverage"
//         coSlices = filter
//             (\slice
//             -> slcf_QUANTITY slice == Just "TEMPERATURE"
//             ) slices
//         tests =
//             [ xAxisCoverage
//             , yAxisCoverage
//             , zAxisCoverage
//             ]
//         testResults = pam (pam tests coSlices) fdsData
//         summaryResults = worstN testResults
//     in Node (CompletedTest testName summaryResults) testResults

// // |Soot Visibility data coverage test.
// visDataCoverage slices fdsData =
//     let
//         testName = "Soot Visibiltity Data Coverage"
//         coSlices = filter
//             (\slice
//             -> slcf_QUANTITY slice == Just ("VISIBILITY" :: String)
//             ) slices
//         tests =
//             [ xAxisCoverage
//             , yAxisCoverage
//             , zAxisCoverage
//             ]
//         testResults = pam (pam tests coSlices) fdsData
//         summaryResults = worstN testResults
//     in Node (CompletedTest testName summaryResults) testResults

// xAxisCoverage slices fdsData =
//     let
//         testName = "X Axis Coverage"
//     in if not $ null $ filter (isJust . slcf_PBX) slices
//       then Node (CompletedTest testName $ Success
//         $ "Full X axis coverage of this value is present.") []
//       else Node (CompletedTest testName $ Warning
//         $ "Full X axis coverage of this value is not present.") []

// yAxisCoverage slices fdsData =
//     let
//         testName = "Y Axis Coverage"
//     in if not $ null $ filter (isJust . slcf_PBY) slices
//       then Node (CompletedTest testName $ Success
//         $ "Full Y axis coverage of this value is present.") []
//       else Node (CompletedTest testName $ Warning
//         $ "Full Y axis coverage of this value is not present.") []

// zAxisCoverage slices fdsData =
//     let
//         testName = "Z Axis Coverage"
//     in if not $ null $ filter (isJust . slcf_PBZ) slices
//       then Node (CompletedTest testName $ Success
//         $ "Full Z axis coverage of this value is present.") []
//       else Node (CompletedTest testName $ Warning
//         $ "Full Z axis coverage of this value is not present.") []
