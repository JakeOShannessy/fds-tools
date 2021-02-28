use chrono::prelude::*;
use csv;
use data_vector::DataVector;
use data_vector::Point;
use fute_core::parse_and_decode_fds_input_file;
use fute_core::{csv_parser::SmvValue, parse_smv_file};
use fute_core::{decode::*, summary::summarise_input};
use plotters::prelude::*;
use std::fs::File;
use std::io::prelude::*;
use std::{
    borrow::Cow,
    ffi::OsStr,
    ops::Range,
    path::{Path, PathBuf},
};
// /// Output the total number of cells simply as an integer (with newline). This
// /// is to make it trivially parseable.
// countCellsMachine1 path = do
//     Right inData <- (fmap decodeNamelistFile) <$> parseFDSFile path
//     let meshes = fdsFile_Meshes inData
//         nCells = sum $ map getNCells meshes
//     putStrLn (show nCells)

/// Output the number of cells for each mesh as a human readable table. This is
/// not machine readable.
pub fn count_cells(input_path: &Path) -> u64 {
    let fds_data = parse_and_decode_fds_input_file(input_path);
    let meshes: Vec<Mesh> = fds_data.meshes;
    let mut total_cells = 0;
    for mesh in meshes {
        total_cells += mesh.n_cells();
    }
    total_cells
    // let meshes = fdsFile_Meshes inData
    //     fmt = formatNum intFmt
    // let tab = Table
    //         (Group SingleLine
    //             [ Group NoLine (map (\(i,mesh)->Header (T.pack (show i))) (zip [1..] meshes))
    //             , Group NoLine [Header "Total"]
    //             ])
    //         (Group DoubleLine
    //             [ Group SingleLine [Header ("Mesh Id" :: T.Text), Header ("# Cells" :: T.Text)]
    //             //, Group SingleLine [Header "time test 1", Header "time test 2"]
    //             ])
    //         (map (\mesh->[T.pack (getIdBound mesh), (fmt $ getNCells mesh)]) meshes ++ [["",fmt (sum $ map getNCells meshes)]])
    //         //[
    //         //, [printf "%.2g kW" ((sum $ map simout_maxHRR $ setout_simOuts outA)/(fromIntegral (setout_nSims outA))), printf "%.2g kW" ((sum $ map simout_maxHRR $ setout_simOuts outB)/(fromIntegral (setout_nSims outB)))]
    //         //]
    // let
    //     s :: String
    //     s = Text.Tabular.AsciiArt.render T.unpack T.unpack T.unpack tab
    // putStrLn s
}
pub fn meshes(fds_path: &Path) {
    // use fute_core::FDSFileExt;
    use prettytable::{Attr, Cell, Row, Table};
    // use fute_core::FDSFile;
    // use fute_core::parse_and_decode_fds_input_file;
    use num_format::{Locale, ToFormattedString};

    let fds_file = parse_and_decode_fds_input_file(fds_path);
    let meshes = fds_file.meshes;
    let mut table = Table::new();
    table.set_titles(Row::new(vec![
        Cell::new("#").with_style(Attr::Bold),
        Cell::new("Mesh Id.").with_style(Attr::Bold),
        Cell::new("# Cells").with_style(Attr::Bold),
        Cell::new("I-J-K").with_style(Attr::Bold),
        Cell::new("Δx-Δy-Δz").with_style(Attr::Bold),
        Cell::new("Aspect Ratio").with_style(Attr::Bold),
    ]));
    let mut n_cells_total: u64 = 0;
    for (i, mesh) in meshes.iter().enumerate() {
        let xb = mesh.xb;
        let ijk = mesh.ijk;
        let n_cell = Cell::new(&format!("{}", i + 1));
        let id_cell = Cell::new(mesh.id.as_ref().unwrap_or(&"Unnamed MESH".to_string()));
        let n_cells = ijk.i * ijk.j * ijk.k;
        let quantity_cell = Cell::new(&n_cells.to_formatted_string(&Locale::en));
        let ijk_cell = Cell::new(&format!("{}-{}-{}", ijk.i, ijk.j, ijk.k));
        let dx = (xb.x2 - xb.x1) / (ijk.i as f64);
        let dy = (xb.y2 - xb.y1) / (ijk.j as f64);
        let dz = (xb.z2 - xb.z1) / (ijk.k as f64);
        let dxyz_cell = Cell::new(&format!("{:.2}-{:.2}-{:.2}", dx, dy, dz));
        let max_dx = if dx >= dy && dx >= dz {
            dx
        } else if dy >= dz {
            dy
        } else {
            dz
        };
        let min_dx = if dx <= dy && dx <= dz {
            dx
        } else if dy <= dz {
            dy
        } else {
            dz
        };
        let aspect_ratio_cell = Cell::new(&format!("{:.2}", max_dx / min_dx));
        table.add_row(Row::new(vec![
            n_cell,
            id_cell,
            quantity_cell,
            ijk_cell,
            dxyz_cell,
            aspect_ratio_cell,
        ]));
        n_cells_total += n_cells as u64;
    }
    table.add_row(Row::new(vec![
        Cell::new("Total"),
        Cell::new(""),
        Cell::new(&n_cells_total.to_formatted_string(&Locale::en)),
        Cell::new(""),
        Cell::new(""),
        Cell::new(""),
    ]));
    table.printstd();
}

// meshPermutations :: [a] -> [(a,a)]
// meshPermutations [_] = []
// meshPermutations (m:meshes) = (map (\n->(m,n)) meshes) ++ (meshPermutations meshes)

// isOverlappingNmlList :: [(Int, Mesh)] -> IO ()
// isOverlappingNmlList [_] = pure ()
// isOverlappingNmlList meshes = do
//     let ms = meshPermutations meshes
//         y = L.foldl' f [] ms
//     mapM_ printOverlapError y
//     where
//         f acc pair@((iA,mA),(iB,mB)) = if uncurry isOverlappingNml (mA,mB)
//             then (pair:acc)
//             else acc

// printOverlapError :: ((Int, Mesh),(Int, Mesh)) -> IO ()
// printOverlapError ((iA, meshA), (iB, meshB))
//     = printf "Mesh[%d]: %s overlaps with Mesh[%d]: %s\n" iA (getIdBound meshA) iB (getIdBound meshB)

// isOverlappingNml nmlA nmlB = isOverlappingXB (getXB nmlA) (getXB nmlB)

// isOverlappingXB :: XB -> XB -> Bool
// isOverlappingXB (XB x1A x2A y1A y2A z1A z2A) (XB x1B x2B y1B y2B z1B z2B) =
//     all (uncurry isOverlappingRange)
//         [ ((x1A,x2A), (x1B,x2B))
//         , ((y1A,y2A), (y1B,y2B))
//         , ((z1A,z2A), (z1B,z2B))
//         ]
// isOverlappingRange :: (Double, Double) -> (Double, Double) -> Bool
// isOverlappingRange (x1A,x2A) (x1B, x2B) = x2A > x1B && x1A < x2B

// /// Output mesh details as a human readable table. This is not machine readable.
// meshDetails path = do
//     inDataRaw <- (fmap decodeNamelistFile) <$> parseFDSFile path
//     let inData = case inDataRaw of
//             Left e -> error (show e)
//             Right x -> x
//     let meshes = fdsFile_Meshes inData
//         // check for overlapping meshes.
//         fmt = formatNum intFmt
//     isOverlappingNmlList (zip [1..] meshes)
//     let tab = Table
//             (Group SingleLine
//                 [ Group NoLine (map (\(i,mesh)->Header (T.pack (show i))) (zip [1..] meshes))
//                 , Group NoLine [Header "Total"]
//                 ])
//             (Group DoubleLine
//                 [ Group SingleLine [Header ("Mesh Id" :: T.Text), Header ("# Cells" :: T.Text), Header ("I-J-K" :: T.Text), Header ("dX-dY-dZ" :: T.Text), Header ("Aspect Ratio" :: T.Text)]
//                 ])
//             (map (\mesh->[T.pack (getIdBound mesh), (fmt $ getNCells mesh), ((\(i,j,k)->fmt i <> "-" <> fmt j <> "-" <> fmt k) $ getMeshIJK mesh), ((\(dx,dy,dz)->T.pack (printf "%.2f m" dx) <> "-" <> T.pack (printf "%.2f m" dy) <> "-" <> T.pack (printf "%.2f m" dz)) $ getMeshResolution mesh), T.pack (printf "%.2g" (getMeshSkew mesh))]) meshes ++ [["",fmt (sum $ map getNCells meshes)]])
//     let
//         s :: String
//         s = Text.Tabular.AsciiArt.render T.unpack T.unpack T.unpack tab
//     putStrLn s

// meshCheck path = do
//     Right x <-  verifyInputFile path
//     print x

// showHRR :: FilePath -> IO ()
// showHRR path = do
//     paths <- createHRRPlots path
//     case os of
//         "windows" -> runCommand $ "start " ++ head paths
//         "mingw32" -> runCommand $ "start " ++ head paths
//         "linux" -> runCommand $ "xdg-open " ++ head paths
//     return ()

pub fn peak_hrr(fds_path: &Path) {
    use fute_core::FDSFileExt;
    // use fute_core::FDSFile;
    // use fute_core::parse_and_decode_fds_input_file;
    let fds_file = parse_and_decode_fds_input_file(fds_path);
    let hrr: f64 = fds_file
        .burners()
        .iter()
        .map(|burner| burner.max_hrr())
        .sum();
    println!("{:.2} kW/m²", hrr);
}

// plotHRR :: FilePath -> IO ()
// plotHRR path = createHRRPlots path >> return ()
pub fn plot_hrr(smv_path: &Path) {
    println!("smv path: {:?}", smv_path);
    let mut file = File::open(smv_path).expect("Could not open smv file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Could not read smv file");
    let (_, smv_file) = parse_smv_file(&contents).expect("Could not parse smv file");
    // for csvf in smv_file.csvfs {
    //     println!("csvf: {:?}", csvf);
    // }
    let hrr_csvf = smv_file
        .csvfs
        .iter()
        .find(|csvf| csvf.type_ == "hrr")
        .expect("No HRR CSV file.");
    println!("csvfhrr: {:?}", hrr_csvf);
    let value = "HRR";
    let mut csv_file = File::open(&hrr_csvf.filename).expect("Could not open HRR file");
    // First we need to trim the first line from the csv
    // We start with a single byte buffer. This is a little hacky but it
    // works
    let mut buffer = [0; 1];
    loop {
        // Read a single byte off the start of the buffer
        let _n: usize = csv_file.read(&mut buffer).unwrap();

        // We have reached teh end of the line (works for CRLF and LF)
        // '\n' == 10
        if buffer[0] == 10 {
            break;
        }
    }
    // let mut csv_contents = String::new();
    // file.read_to_string(&mut contents).unwrap();
    // Build the CSV reader and iterate over each record.
    let mut rdr = csv::ReaderBuilder::new()
        .has_headers(true)
        .trim(csv::Trim::All)
        .from_reader(csv_file);
    let mut value_index_option: Option<usize> = None;
    let mut time_index_option: Option<usize> = None;
    {
        let headers = rdr.headers().unwrap();
        for (i, header) in headers.iter().enumerate() {
            if header == "Time" {
                time_index_option = Some(i);
            } else if header == value {
                value_index_option = Some(i);
            }
        }
    }
    let value_index = value_index_option.expect("value index");
    let time_index = time_index_option.expect("time index");
    let mut svg = String::new();
    let mut x_data = Vec::new();
    let mut y_data = Vec::new();

    for result in rdr.deserialize() {
        // The iterator yields Result<StringRecord, Error>, so we check the
        // error here.
        let record: Vec<f64> = result.unwrap();
        let t = record.get(time_index).expect("time val").clone();
        let v = record.get(value_index).expect("val").clone();
        x_data.push(t);
        y_data.push(v);
    }

    let title = "HRR".to_string();
    {
        let root = SVGBackend::with_string(&mut svg, (640, 480)).into_drawing_area();
        root.fill(&WHITE).unwrap();
        let mut chart = ChartBuilder::on(&root)
            .caption(title, ("Arial", 16).into_font())
            .margin(15)
            .x_label_area_size(50)
            .y_label_area_size(60)
            .build_ranged(0_f32..((1800_f32) as f32), 0_f32..3000_f32)
            // .build()
            .unwrap();

        chart
            .configure_mesh()
            .x_desc("Time (s)")
            .y_desc("HRR (kW)")
            .y_label_formatter(&|x| format!("{:.2e}", x))
            .draw()
            .unwrap();

        chart
            .draw_series(LineSeries::new(
                x_data
                    .iter()
                    .zip(y_data)
                    .map(|(x, y)| (*x as f32, y as f32)),
                &BLUE,
            ))
            .unwrap()
            // .label("Failure Line")
            .legend(|(x, y)| PathElement::new(vec![(x, y - 12), (x + 20, y - 12)], &BLUE));

        chart
            .configure_series_labels()
            .background_style(&WHITE.mix(0.8))
            .border_style(&BLACK)
            .margin(20)
            .draw()
            .unwrap();
    }
    std::fs::create_dir_all("verification").expect("could not create dir");
    let mut file = File::create("verification/hrr.svg").expect("Could not create file");
    file.write_all(&svg.as_bytes()).expect("write failed");
}

// createHRRPlots :: FilePath -> IO [FilePath]
// createHRRPlots path = do
//     fdsRaw <- (fmap decodeNamelistFile) <$> parseFDSFile path
//     let
//         fdsData = case fdsRaw of
//             Right x -> x
//             Left e -> error $ show e
//         chid = take 40 (fromJust $ getCHID fdsData)
//         simulation = FDSSimulation
//             { simDir = takeDirectory path
//             , simCHID = chid
//             }
//     // -- get HRR data source filepath
//     // let dataPath = hrrFilePath simulation
//     // -- read it
//     // theData <- readIn the csv
//     // get all data
//     theDataVectors <- getDataListAll simulation
//     let Just hrrVector = findDVectorPairByYNameMaybe theDataVectors ("kW", "HRR")
//         // chr = chart (chid ++ " - Realised HRR") eurocodeHRRChartConfig [hrrVector]
//     createDirectoryIfMissing True "charts"
//     paths <- produceChart "charts" eurocodeHRRChartConfig [hrrVector] (chid ++ " - Realised HRR") "HRR"
//     return paths

// verifyInput path = do
//     // res is the verification output
//     res <- verifyInputFile path
//     // Now we want to show the output. We want to default to console text, as
//     // the interactive case should be simple. It's less onerours to add flags
//     // in a programmatic use.
//     case res of
//         Left e -> print e
//         Right a  -> putStrLn $ produceVerificationFragment a
//         // Right a  -> T.putStrLn $ renderVerificationConsoleText a
pub fn verify_input(fds_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    use fute_core::html::Html;
    println!("verifying: {}", fds_path.display());
    let fds_data = fds_input_parser::parse_and_decode_fds_input_file(fds_path);
    fute_core::verify_input(&fds_data)?;
    let input_summary = summarise_input(&fds_data);
    let dir = "Charts";
    let mut chart_page_path = PathBuf::from(fds_path.parent().unwrap());
    chart_page_path.push(format!("Verification.html"));
    println!("about to create verification page");

    let html = input_summary.to_html();
    let mut f = std::fs::File::create(&chart_page_path).unwrap();
    // f.write_all(html.as_bytes()).unwrap();
    // write!(f,"{}", html);
    let mut s = String::new();
    html.render(&mut f).unwrap();
    // create_chart_page(&chart_page_path, charts);

    #[cfg(windows)]
    open_browser(&chart_page_path).unwrap();
    Ok(())
}

pub fn verify(smv_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    fute_core::verify(smv_path)?;
    Ok(())
}
// showInputVerification path = do
//     let
//         simulation = FDSSimulation
//             { simDir = takeDirectory path
//             , simCHID = takeBaseName path
//             }
//     verifPath <- produceInputPage (joinPath [simDir simulation, "verification"]) simulation
//     verifPathAbs <- makeAbsolute verifPath
//     putStrLn ("Opening: \"" ++ verifPathAbs ++ "\" in browser")
//     r <- openBrowser ("file://" ++ verifPathAbs)
//     if r
//         then do
//             putStrLn "Browser window opened"
//             threadDelay (3*1000*1000)
//         else do
//             putStrLn "Opening failed, press any key to continue"
//             void getChar

// renameSimulationInPath inPath = do
//     renameSimulation sim
//     where
//         inchid =  takeBaseName inPath
//         dir = takeDirectory inPath
//         sim = FDSSimulation {simDir = dir, simCHID = inchid}

// currentProgress path = do
//     let simulation = FDSSimulation
//             { simDir = takeDirectory path
//             , simCHID = takeBaseName path
//             }
//     Right outData <- parseSimulationOutFile simulation
//     currentProgress <- getCurrentProgressOut outData
//     C.putStrLn $ encode currentProgress

// instance ToJSON CurrentProgress where
//     toJSON currentProgress =
//         object
//             [ "EndTime" Aeson..= currentProgress_endTime currentProgress
//             , "LastSimTime" Aeson..= currentProgress_lastSimTime currentProgress
//             , "LastWallTime" Aeson..= currentProgress_lastWallTime currentProgress
//             , "CurrentWallTime" Aeson..= currentProgress_currentWallTime currentProgress
//             ]

// instance FromJSON CurrentProgress where
//     parseJSON (Aeson.Object v) = CurrentProgress A.<$>
//                                 v .: "EndTime" A.<*>
//                                 v .: "LastSimTime" A.<*>
//                                 v .: "LastWallTime" A.<*>
//                                 v .: "CurrentWallTime"
//     parseJSON _ = mzero

// plotOut path = do
//     let simulation = FDSSimulation
//             { simDir = takeDirectory path
//             , simCHID = takeBaseName path
//             }
//     Right outData <- parseSimulationOutFile simulation
//     // Extrat the run data, that is, timesteps.
//     let ts = timesteps outData
//         // Get the whole-domain quantities and make them into plottable vectors
//         // simTimes = DataVector "Simulation Time" "s" $ V.fromList $ map simTime ts
//         simTimes = map simTime ts
//         startTime = time $ head ts
//         simEndTime = simEnd $ miscellaneous outData
//         // wallTimes = DataVector "Wall Time" "-" $ V.fromList {- $ map (\s->realToFrac $ (time s) `diffUTCTime` startTime :: Double) -} $ map time ts
//         wallTimes = map time ts
//         runData = zip wallTimes simTimes
//         // runVector = DataVectorPair wallTimes simTimes
//         vectors = M.toList $ mkStepProps ts

//     produceRunChart "charts" utc simEndTime runData

//     mapM_ print $ (vectors)

//     createDirectoryIfMissing True "charts"
//     mapM_ (\(k,dp)->produceChart "charts" def [dp] (dataVectorName $ yVector dp) k) (vectors)

//     where
//         mkStepProps :: [TimeStep] -> M.Map String (DataVectorPair Double Double)
//         mkStepProps ts =
//             let props = M.map reverse $ L.foldl' mkStepPropsW M.empty ts
//             in M.mapWithKey toVecs props
//             where
//                 toVecs k vs =
//                     let (times,values) = unzip vs
//                     in DataVectorPair (DataVector "Simulation Time" "s" (V.fromList times)) (DataVector k "-" (V.fromList values))

//         mkStepPropsW :: M.Map String [(Double, Double)] -> TimeStep -> M.Map String [(Double, Double)]
//         mkStepPropsW acc timestep =
//             let
//                 pairs = map (\sp->(stepPropKey sp, toDouble $ stepPropValue sp)) $ timeStepStepProps timestep
//             in L.foldl' (\acc (name,val)->M.insertWith (++) name [(simTime timestep, val)] acc) acc pairs
//             where
//                 toDouble (ValueInt i) = fromIntegral i
//                 toDouble (ValueDouble d) = d

pub struct ChartResult {
    pub path: PathBuf,
}

pub fn chart_to_html(dir: &Path, chart: ChartResult) -> String {
    println!("dir: {:?}, path: {:?}", dir, chart.path);
    let p = chart.path.strip_prefix(dir).unwrap();
    let p_str = p.to_str().unwrap();
    // let url = url::Url::from_file_path(p).unwrap();
    // let p_str = url.as_str();
    format!("<img src=\"{}\"/>", p_str)
}

pub fn create_chart_page(path: &Path, charts: Charts) {
    // let mut html: String = String::new();
    let mut chart_strings = Vec::new();
    let run_chart = charts
        .run_chart_path
        .map(|rc| chart_to_html(path.parent().unwrap(), rc));
    if let Some(rc_string) = run_chart {
        chart_strings.push(rc_string);
    }
    for cr in charts.various.into_iter() {
        chart_strings.push(chart_to_html(path.parent().unwrap(), cr));
    }
    let html: String = chart_strings.into_iter().collect();
    let mut f = std::fs::File::create(&path).unwrap();
    f.write_all(html.as_bytes()).unwrap();
}

pub struct Charts {
    pub run_chart_path: Option<ChartResult>,
    pub various: Vec<ChartResult>,
}

impl Charts {
    pub fn new() -> Self {
        Self {
            run_chart_path: None,
            various: Vec::new(),
        }
    }
}

pub fn read_out(out_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let out_file = std::fs::File::open(out_path)?;
    let run_data = fute_core::out_parser::RunData::from_out_reader(out_file);
    println!("{}", serde_json::to_string_pretty(&run_data).unwrap());
    Ok(())
}

pub fn quick_chart(smv_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let dir = "Charts";
    println!("quick-charting: {:?}", smv_path);
    let outputs = fute_core::Outputs::new(PathBuf::from(smv_path));
    let mut charts: Charts = Charts::new();
    let smv_dir = PathBuf::from(outputs.smv_path.parent().unwrap());
    for csvf in outputs.smv.csvfs.iter() {
        println!("csvf: {:?}", csvf);

        let mut csv_file_path = PathBuf::new();
        csv_file_path.push(smv_dir.clone());
        csv_file_path.push(csvf.filename.clone());
        println!("about to get data");
        let csv_data = fute_core::csv_parser::CsvDataBlock::from_file(&csv_file_path)?;
        if csvf.type_ == "steps" {
            match csv_data.make_data_vector("Simulation Time", "Wall Time") {
                Some(mut run_vec) => {
                    // panic!("x_name: {:?}, {:?}", run_vec.x_name, run_vec.y_name);
                    run_vec.name = "Run Chart".to_string();
                    plot(
                        &smv_dir,
                        dir,
                        &mut charts,
                        outputs.smv.chid.to_string().clone(),
                        run_vec,
                    )
                }
                None => panic!("could not make run chart"),
            }
        }

        for dv in csv_data.default_vecs() {
            plot(
                &smv_dir,
                dir,
                &mut charts,
                outputs.smv.chid.to_string().clone(),
                dv,
            );
        }
    }
    let mut chart_page_path = PathBuf::from(smv_dir);
    chart_page_path.push(format!("Charts.html"));
    println!("about to create chart page");
    create_chart_page(&chart_page_path, charts);

    #[cfg(windows)]
    open_browser(&chart_page_path).unwrap();
    Ok(())
}

pub fn compare(vector_name: String, smv_paths: Vec<PathBuf>) {
    // let dir = "Charts";
    // println!("comparing: {:?} and {:?}", smv_path_a, smv_path_b);
    let vectors: Vec<DataVector<f64, SmvValue>> = smv_paths
        .into_iter()
        .map(|smv_path| get_vector_for_comparison(vector_name.clone(), smv_path))
        .collect();
    let mut chart_page_path = PathBuf::from(".");
    chart_page_path.push(format!("Comparison-{}.html", vector_name));
    println!("about to create chart page");
    let mut charts = Charts {
        run_chart_path: None,
        various: Vec::new(),
    };
    plot_multiple(
        &PathBuf::from("."),
        format!("{} Comparison", vector_name),
        ".",
        &mut charts,
        vectors,
    );
    create_chart_page(&chart_page_path, charts);
    #[cfg(windows)]
    open_browser(&chart_page_path).unwrap();
}

fn get_vector_for_comparison(vector_name: String, smv_path: PathBuf) -> DataVector<f64, SmvValue> {
    let outputs = fute_core::Outputs::new(smv_path);
    let chid = outputs.smv.chid.clone();
    // let charts: Charts = Charts::new();
    let smv_dir = PathBuf::from(outputs.smv_path.parent().unwrap());
    let mut vector = None;
    for csvf in outputs.smv.csvfs.iter() {
        println!("csvf: {:?}", csvf);

        let mut csv_file_path = PathBuf::new();
        csv_file_path.push(smv_dir.clone());
        csv_file_path.push(csvf.filename.clone());
        println!("about to get data");
        let csv_data = fute_core::csv_parser::CsvDataBlock::from_file(&csv_file_path).unwrap();

        for mut dv in csv_data.default_vecs() {
            if dv.name == vector_name {
                dv.name = chid.to_string();
                vector = Some(dv);
                break;
            }
        }
        if vector.is_some() {
            break;
        }
    }
    vector.unwrap()
}

fn plot_multiple(
    smv_dir: &Path,
    chart_name: String,
    dir: &str,
    charts: &mut Charts,
    dvs: Vec<DataVector<f64, SmvValue>>,
) {
    let mut path = PathBuf::from(smv_dir.clone());
    path.push(dir);
    std::fs::create_dir_all(&path).unwrap();
    // Mangle the filenames to ensure there are no forbidden windows
    // names.
    let f_name: Cow<String> = mangle(&chart_name);
    path.push(format!("{}.png", f_name));
    let cleaned_vecs: Vec<DataVector<f64, f64>> = dvs.into_iter().map(clean_f64_vec).collect();
    plot_dv(cleaned_vecs.iter().map(|x| x).collect(), chart_name, &path);
    charts.various.push(ChartResult { path: path.clone() });
}

fn clean_f64_vec(dv: DataVector<f64, SmvValue>) -> DataVector<f64, f64> {
    let new_values: Vec<Point<f64, f64>> = dv
        .values()
        .into_iter()
        .map(|point| {
            let new_y = match point.y {
                SmvValue::Float(f) => f,
                _ => panic!("not float"),
            };
            Point {
                x: point.x,
                y: new_y,
            }
        })
        .collect();
    DataVector::new(
        dv.name, dv.x_name, dv.y_name, dv.x_units, dv.y_units, new_values,
    )
}

pub fn hrr_vector(
    smv_path: &Path,
) -> Result<DataVector<f64, SmvValue>, Box<dyn std::error::Error>> {
    let outputs = fute_core::Outputs::new(PathBuf::from(smv_path));
    let smv_dir = PathBuf::from(outputs.smv_path.parent().unwrap());
    for csvf in outputs.smv.csvfs.iter() {
        if csvf.type_ != "hrr" {
            break;
        }

        let mut csv_file_path = PathBuf::new();
        csv_file_path.push(smv_dir.clone());
        csv_file_path.push(csvf.filename.clone());
        let csv_data = fute_core::csv_parser::CsvDataBlock::from_file(&csv_file_path)?;

        for dv in csv_data.default_vecs() {
            if dv.name == "HRR" {
                return Ok(dv);
            }
        }
    }
    panic!("no HRR")
}

fn plot(
    smv_dir: &Path,
    dir: &str,
    charts: &mut Charts,
    chid: String,
    dv: DataVector<f64, SmvValue>,
) {
    let mut path = PathBuf::from(smv_dir.clone());
    path.push(dir);
    std::fs::create_dir_all(&path).unwrap();
    // Mangle the filenames to ensure there are no forbidden windows
    // names.
    let f_name: Cow<String> = mangle(&dv.name);
    path.push(format!("{}.png", f_name));
    match dv.values()[0].y {
        SmvValue::Float(_) => {
            let vec = dv
                .values()
                .into_iter()
                .map(|v| match v.y {
                    SmvValue::Float(d) => data_vector::Point { x: v.x, y: d },
                    _ => panic!("type changes part way through vector"),
                })
                .collect();
            let new_dv =
                DataVector::new(dv.name, dv.x_name, dv.y_name, dv.x_units, dv.y_units, vec);
            if &new_dv.name == "HRR" {
                plot_dv_hrr(vec![&new_dv], &path);
            } else {
                plot_dv(
                    vec![&new_dv],
                    format!("{} {}", chid, new_dv.name.clone()),
                    &path,
                );
            }
            charts.various.push(ChartResult { path: path.clone() })
        }
        SmvValue::DateTime(_) => {
            let vec = dv
                .values()
                .into_iter()
                .map(|v| match v.y {
                    SmvValue::DateTime(d) => data_vector::Point { x: v.x, y: d },
                    _ => panic!("type changes part way through vector"),
                })
                .collect();
            let new_dv =
                DataVector::new(dv.name, dv.x_name, dv.y_name, dv.x_units, dv.y_units, vec);
            plot_runtime(&new_dv, &path);
            if new_dv.name.as_str() == "Run Chart" {
                charts.run_chart_path = Some(ChartResult { path: path.clone() })
            } else {
                charts.various.push(ChartResult { path: path.clone() })
            }
        }
        // Nothing to plot
        SmvValue::String(_) => (),
    }
}

fn mangle(s: &String) -> Cow<String> {
    #[cfg(windows)]
    match s.to_lowercase().as_str() {
        "con" | "prn" | "aux" | "nul" | "com0" | "com1" | "com2" | "com3" | "com4" | "com5"
        | "com6" | "com7" | "com8" | "com9" | "lpt0" | "lpt1" | "lpt2" | "lpt3" | "lpt4"
        | "lpt5" | "lpt6" | "lpt7" | "lpt8" | "lpt9" => Cow::Owned(format!("{}_", s)),
        _ => Cow::Borrowed(s),
    }
    #[cfg(not(windows))]
    Cow::Borrowed(s)
}

#[cfg(windows)]
fn open_browser(path: &Path) -> std::io::Result<bool> {
    {
        use std::os::windows::ffi::OsStrExt;
        fn to_u16s<S: AsRef<OsStr>>(s: S) -> std::io::Result<Vec<u16>> {
            fn inner(s: &OsStr) -> std::io::Result<Vec<u16>> {
                let mut maybe_result: Vec<u16> = s.encode_wide().collect();
                if maybe_result.iter().any(|&u| u == 0) {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        "strings passed to WinAPI cannot contain NULs",
                    ));
                }
                maybe_result.push(0);
                Ok(maybe_result)
            }
            inner(s.as_ref())
        }
        const SW_SHOW: winapi::ctypes::c_int = 5;
        let path = to_u16s(path)?;
        let operation = to_u16s("open")?;
        let result = unsafe {
            winapi::um::shellapi::ShellExecuteW(
                std::ptr::null_mut(),
                operation.as_ptr(),
                path.as_ptr(),
                std::ptr::null(),
                std::ptr::null(),
                SW_SHOW,
            )
        };
        Ok(result as usize > 32)
    }
}

fn plot_dv(data_vectors: Vec<&DataVector<f64, f64>>, title: String, path: &Path) {
    let mut x_min = data_vectors[0].values()[0].x as f32;
    let mut x_max = data_vectors[0].values()[0].x as f32;
    let mut y_min = data_vectors[0].values()[0].y as f32;
    let mut y_max = data_vectors[0].values()[0].y as f32;

    for vec in data_vectors.iter() {
        for p in vec.values().iter() {
            if (p.x as f32) < x_min {
                x_min = p.x as f32;
            }
            if (p.x as f32) > x_max {
                x_max = p.x as f32;
            }
            if (p.y as f32) < y_min {
                y_min = p.y as f32;
            }
            if (p.y as f32) > y_max {
                y_max = p.y as f32;
            }
        }
    }

    let y_min = if y_min < 0.0 { y_min } else { 0.0 };
    // let y_max = if y_max > 1.0 { y_min } else { 1.0 };
    let x_range = x_min..(x_max + x_max * 0.2);
    let y_diff = y_max - y_min;
    let y_range = if y_diff == 0.0 {
        (y_max - 0.5)..(y_max + 0.5)
    } else {
        (y_min - y_diff.abs() * 0.2)..(y_max + y_diff.abs() * 0.2)
    };
    let colors = vec![(62, 43, 88), (239, 121, 93), (255, 0, 0)]
        .into_iter()
        .cycle();
    {
        let root = BitMapBackend::new(path, (640, 480)).into_drawing_area();
        root.fill(&WHITE).unwrap();
        let mut chart = ChartBuilder::on(&root)
            .caption(title, ("Arial", 16).into_font())
            .margin(15)
            .x_label_area_size(50)
            .y_label_area_size(60)
            .build_ranged(x_range, y_range)
            .unwrap();

        chart
            .configure_mesh()
            .x_desc(format!(
                "{} ({})",
                data_vectors[0].x_name, data_vectors[0].x_units
            ))
            .y_desc(format!(
                "{} ({})",
                data_vectors[0].y_name, data_vectors[0].y_units
            ))
            .y_label_formatter(&|x| format!("{:.2e}", x))
            .draw()
            .unwrap();

        for (data_vector, color) in data_vectors.into_iter().zip(colors) {
            chart
                .draw_series(LineSeries::new(
                    data_vector
                        .values()
                        .iter()
                        .map(|p| (p.x as f32, p.y as f32)),
                    &RGBColor(color.0, color.1, color.2),
                ))
                .unwrap()
                .label(data_vector.name.as_str())
                .legend(move |(x, y)| {
                    PathElement::new(
                        vec![(x, y), (x + 20, y)],
                        &RGBColor(color.0, color.1, color.2),
                    )
                });
        }

        chart
            .configure_series_labels()
            .background_style(&WHITE.mix(0.8))
            .border_style(&BLACK)
            .margin(20)
            .draw()
            .unwrap();
    }
}

// TODO: needs more const functions
// const MEDIUM_ALPHA4: f64 = 1055.0 / 300.0_f64.powi(2);

pub struct GrowthAlphas {
    pub slow: f64,
    pub medium: f64,
    pub fast: f64,
    pub ultrafast: f64,
}

const SLOW_COLOR: (u8, u8, u8) = (0x00, 0x80, 0x00);
const MEDIUM_COLOR: (u8, u8, u8) = (0xFF, 0x00, 0x00);
const FAST_COLOR: (u8, u8, u8) = (0x00, 0xBF, 0xBF);
const ULTRAFAST_COLOR: (u8, u8, u8) = (0xBF, 0x80, 0xBF);

lazy_static::lazy_static! {
    static ref EUROCODE_GROWTH_RATES: GrowthAlphas = GrowthAlphas {
        slow: 1000.0 / 600.0_f64.powi(2),
        medium: 1000.0 / 300.0_f64.powi(2),
        fast: 1000.0 / 150.0_f64.powi(2),
        ultrafast: 1000.0 / 75.0_f64.powi(2),
    };
    static ref NFPA_GROWTH_RATES: GrowthAlphas = GrowthAlphas {
        slow: 1055.0 / 600.0_f64.powi(2),
        medium: 1055.0 / 300.0_f64.powi(2),
        fast: 1055.0 / 150.0_f64.powi(2),
        ultrafast: 1055.0 / 75.0_f64.powi(2),
    };
}

fn make_standard_curve(
    vector: &DataVector<f64, f64>,
    alpha: f64,
    name: String,
) -> DataVector<f64, f64> {
    let mut vector = vector.clone();
    for p in vector.iter_mut() {
        p.y = alpha * p.x.powi(2);
    }
    vector.name = name;
    vector
}

fn plot_dv_hrr(data_vectors: Vec<&DataVector<f64, f64>>, path: &Path) {
    let mut x_min = data_vectors[0].values()[0].x as f32;
    let mut x_max = data_vectors[0].values()[0].x as f32;
    let mut y_min = data_vectors[0].values()[0].y as f32;
    let mut y_max = data_vectors[0].values()[0].y as f32;

    let slow = make_standard_curve(
        data_vectors[0],
        EUROCODE_GROWTH_RATES.slow,
        "Slow".to_string(),
    );
    let medium = make_standard_curve(
        data_vectors[0],
        EUROCODE_GROWTH_RATES.medium,
        "Medium".to_string(),
    );
    let fast = make_standard_curve(
        data_vectors[0],
        EUROCODE_GROWTH_RATES.fast,
        "Fast".to_string(),
    );
    let ultrafast = make_standard_curve(
        data_vectors[0],
        EUROCODE_GROWTH_RATES.ultrafast,
        "Ultrafast".to_string(),
    );

    for vec in data_vectors.iter() {
        for p in vec.values().iter() {
            if (p.x as f32) < x_min {
                x_min = p.x as f32;
            }
            if (p.x as f32) > x_max {
                x_max = p.x as f32;
            }
            if (p.y as f32) < y_min {
                y_min = p.y as f32;
            }
            if (p.y as f32) > y_max {
                y_max = p.y as f32;
            }
        }
    }

    let title = data_vectors[0].name.clone();
    let y_min = if y_min < 0.0 { y_min } else { 0.0 };
    // let y_max = if y_max > 1.0 { y_min } else { 1.0 };
    let x_range = x_min..(x_max + x_max * 0.2);
    let y_range = if y_max - y_min == 0.0 {
        (y_max - 0.5)..(y_max + 0.5)
    } else {
        (y_min - y_min * 0.2)..(y_max + y_max * 0.2)
    };
    let colors = vec![(62, 43, 88), (239, 121, 93), (255, 0, 0)]
        .into_iter()
        .cycle();
    {
        let root = BitMapBackend::new(path, (640, 480)).into_drawing_area();
        root.fill(&WHITE).unwrap();
        let mut chart = ChartBuilder::on(&root)
            .caption(title, ("Arial", 16).into_font())
            .margin(15)
            .x_label_area_size(50)
            .y_label_area_size(60)
            .build_ranged(x_range, y_range)
            .unwrap();

        chart
            .configure_mesh()
            .x_desc(format!(
                "{} ({})",
                data_vectors[0].x_name, data_vectors[0].x_units
            ))
            .y_desc(format!(
                "{} ({})",
                data_vectors[0].y_name, data_vectors[0].y_units
            ))
            .y_label_formatter(&|x| format!("{:.2e}", x))
            .draw()
            .unwrap();

        for (data_vector, color) in data_vectors.into_iter().zip(colors) {
            chart
                .draw_series(LineSeries::new(
                    data_vector
                        .values()
                        .iter()
                        .map(|p| (p.x as f32, p.y as f32)),
                    &RGBColor(color.0, color.1, color.2),
                ))
                .unwrap()
                .label(data_vector.name.as_str())
                .legend(move |(x, y)| {
                    PathElement::new(
                        vec![(x, y), (x + 20, y)],
                        &RGBColor(color.0, color.1, color.2),
                    )
                });
        }

        chart
            .draw_series(LineSeries::new(
                slow.values().iter().map(|p| (p.x as f32, p.y as f32)),
                &RGBColor(SLOW_COLOR.0, SLOW_COLOR.1, SLOW_COLOR.2),
            ))
            .unwrap()
            .label(slow.name.as_str())
            .legend(move |(x, y)| {
                PathElement::new(
                    vec![(x, y), (x + 20, y)],
                    &RGBColor(SLOW_COLOR.0, SLOW_COLOR.1, SLOW_COLOR.2),
                )
            });

        chart
            .draw_series(LineSeries::new(
                medium.values().iter().map(|p| (p.x as f32, p.y as f32)),
                &RGBColor(MEDIUM_COLOR.0, MEDIUM_COLOR.1, MEDIUM_COLOR.2),
            ))
            .unwrap()
            .label(medium.name.as_str())
            .legend(move |(x, y)| {
                PathElement::new(
                    vec![(x, y), (x + 20, y)],
                    &RGBColor(MEDIUM_COLOR.0, MEDIUM_COLOR.1, MEDIUM_COLOR.2),
                )
            });

        chart
            .draw_series(LineSeries::new(
                fast.values().iter().map(|p| (p.x as f32, p.y as f32)),
                &RGBColor(FAST_COLOR.0, FAST_COLOR.1, FAST_COLOR.2),
            ))
            .unwrap()
            .label(fast.name.as_str())
            .legend(move |(x, y)| {
                PathElement::new(
                    vec![(x, y), (x + 20, y)],
                    &RGBColor(FAST_COLOR.0, FAST_COLOR.1, FAST_COLOR.2),
                )
            });

        chart
            .draw_series(LineSeries::new(
                ultrafast.values().iter().map(|p| (p.x as f32, p.y as f32)),
                &RGBColor(ULTRAFAST_COLOR.0, ULTRAFAST_COLOR.1, ULTRAFAST_COLOR.2),
            ))
            .unwrap()
            .label(ultrafast.name.as_str())
            .legend(move |(x, y)| {
                PathElement::new(
                    vec![(x, y), (x + 20, y)],
                    &RGBColor(ULTRAFAST_COLOR.0, ULTRAFAST_COLOR.1, ULTRAFAST_COLOR.2),
                )
            });

        chart
            .configure_series_labels()
            .background_style(&WHITE.mix(0.8))
            .border_style(&BLACK)
            .margin(20)
            .draw()
            .unwrap();
    }
}

fn plot_runtime(dv: &DataVector<f64, DateTime<Utc>>, path: &Path) {
    let mut x_min = dv.values()[0].x;
    let mut x_max = dv.values()[0].x;
    let mut y_min: DateTime<Utc> = dv.values()[0].y;
    let mut y_max: DateTime<Utc> = dv.values()[0].y;

    for p in dv.values().iter() {
        if (p.x) < x_min {
            x_min = p.x;
        }
        if (p.x) > x_max {
            x_max = p.x;
        }
        if (p.y) < y_min {
            y_min = p.y;
        }
        if (p.y) > y_max {
            y_max = p.y;
        }
    }

    let title = dv.name.clone();
    // let y_min = if y_min < 0.0 { y_min } else { 0.0 };
    // let y_max = if y_max > 1.0 { y_min } else { 1.0 };
    let x_range = x_min..(x_max + x_max * 0.2);
    let y_range: Range<DateTime<Utc>> = y_min..y_max;
    // let y_range: RangedDateTime<Utc> = plotters::coord::RangedDateTime(y_min,y_max);

    // println!("{:?}: x_range: {:?} y_range: {:?}", path, x_range, y_range);
    let mut colors = vec![(62, 43, 88), (239, 121, 93), (255, 0, 0)]
        .into_iter()
        .cycle();
    {
        let root = BitMapBackend::new(path, (640, 480)).into_drawing_area();
        root.fill(&WHITE).unwrap();
        let mut chart = ChartBuilder::on(&root)
            .caption(title, ("Arial", 16).into_font())
            .margin(15)
            .x_label_area_size(50)
            .y_label_area_size(60)
            .build_ranged(y_range, x_range)
            .unwrap();

        let x_label_style = plotters::style::FontDesc::new(
            plotters::style::FontFamily::SansSerif,
            12.0,
            plotters::style::FontStyle::Normal,
        )
        .transform(plotters::style::FontTransform::Rotate270);

        chart
            .configure_mesh()
            .x_desc(format!("{} ({})", dv.y_name, dv.y_units))
            .x_label_style(x_label_style)
            .y_desc(format!("{} ({})", dv.x_name, dv.x_units))
            // .y_label_formatter(&|x| format!("{:.2e}", x))
            .draw()
            .unwrap();
        let color = colors.next().unwrap();
        chart
            .draw_series(LineSeries::new(
                dv.values().iter().map(|p| (p.y, p.x)),
                &RGBColor(color.0, color.1, color.2),
            ))
            .unwrap()
            .label(dv.name.as_str())
            .legend(move |(x, y)| {
                PathElement::new(
                    vec![(x, y), (x + 20, y)],
                    &RGBColor(color.0, color.1, color.2),
                )
            });

        chart
            .configure_series_labels()
            .background_style(&WHITE.mix(0.8))
            .border_style(&BLACK)
            .margin(20)
            .draw()
            .unwrap();
    }
}
