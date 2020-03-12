use csv;
use fute_core::parse_and_decode_fds_input_file;
use fute_core::parse_smv_file;
use fute_core::decode::*;
use plotters::prelude::*;
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use data_vector::DataVector;
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
        total_cells += mesh.cells();
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
    use prettytable::{color, Attr, Cell, Row, Table};
    use fute_core::FDSFileExt;
    // use fute_core::FDSFile;
    use fute_core::parse_and_decode_fds_input_file;
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
        let n_cell = Cell::new(&format!("{}", i+1));
        let id_cell = Cell::new(mesh.id.as_ref().unwrap_or(&"Unnamed MESH".to_string()));
        let n_cells =  ijk.i*ijk.j*ijk.k;
        let quantity_cell = Cell::new(&n_cells.to_formatted_string(&Locale::en));
        let ijk_cell = Cell::new(&format!("{}-{}-{}", ijk.i,ijk.j,ijk.k));
        let dx = (xb.x2-xb.x1)/(ijk.i as f64);
        let dy = (xb.y2-xb.y1)/(ijk.j as f64);
        let dz = (xb.z2-xb.z1)/(ijk.k as f64);
        let dxyz_cell = Cell::new(&format!("{:.2}-{:.2}-{:.2}", dx,dy,dz));
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
        let aspect_ratio_cell = Cell::new(&format!("{:.2}", max_dx/min_dx));
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
    use fute_core::parse_and_decode_fds_input_file;
    let fds_file = parse_and_decode_fds_input_file(fds_path);
    let hrr: f64 = fds_file.burners().iter().map(|burner| burner.hrr).sum();
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
    let mut svg = Vec::new();
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
        let root = SVGBackend::with_buffer(&mut svg, (640, 480)).into_drawing_area();
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
    file.write_all(&svg).expect("write failed");
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

pub fn chart_to_html(chart: ChartResult)  -> String {
    format!("<img src=\"{}\"/>", chart.path.to_str().unwrap())
}

pub fn create_chart_page(path: &Path, charts: Vec<ChartResult>) {
    // let mut html: String = String::new();
    let cs: Vec<String> = charts.into_iter().map(chart_to_html).collect();
    let html: String = cs.into_iter().collect();
    let mut f = std::fs::File::create(&path).unwrap();
    f.write_all(html.as_bytes());
}

pub fn quick_chart(smv_path: &Path) {
    use std::process::Command;
    let dir = "Charts";
    println!("quick-charting: {:?}", smv_path);
    let outputs = fute_core::Outputs::new(PathBuf::from(smv_path));
    let mut charts = Vec::new();
    let smv_dir = PathBuf::from(outputs.smv_path.parent().unwrap());
    for csvf in outputs.smv.csvfs.iter() {
        println!("csvf: {:?}", csvf);
        if csvf.type_ == "steps" {
            continue;
        }
        let mut csv_file_path = PathBuf::new();
        csv_file_path.push(smv_dir.clone());
        csv_file_path.push(csvf.filename.clone());
        if let Ok(csv_data) = fute_core::csv_parser::get_csv_data(&csv_file_path) {
            // TODO: combine charts with the same type.
            for dv in csv_data {
                let mut path = PathBuf::from(smv_dir.clone());
                path.push(dir);
                std::fs::create_dir_all(&path).unwrap();
                path.push(format!("{}.png", dv.name));
                println!("charting to: {:?}", path);
                let xs = &dv.values().clone().into_iter().map(|p| p.x).collect();
                let ys = &dv.values().clone().into_iter().map(|p| p.y).collect();
                plot_dv(vec![&dv], &path, minimum(xs) as f32, maximum(xs) as f32, minimum(ys) as f32, maximum(ys) as f32);
                charts.push(ChartResult {
                    path: path.clone(),
                })
            }
        } else {
            println!("{} could not be parsed", csvf.type_);
        }
    }
    let mut chart_page_path = PathBuf::from(smv_dir);
    chart_page_path.push(format!("Charts.html"));
    create_chart_page(&chart_page_path, charts);
}


fn plot_dv(data_vectors: Vec<&DataVector<f64>>, path: &Path, x_min: f32, x_max: f32, y_min: f32, y_max: f32) {
    let title = data_vectors[0].name.clone();
    let y_min = if y_min < 0.0 { y_min } else { 0.0 };
    // let y_max = if y_max > 1.0 { y_min } else { 1.0 };
    let x_range = x_min..(x_max+x_max*0.2);
    let y_range = if y_max - y_min == 0.0 {
        (y_max-0.5)..(y_max+0.5)
    } else {
        (y_min-y_min*0.2)..(y_max+y_max*0.2)
    };
    println!("{:?}: x_range: {:?} y_range: {:?}", path, x_range, y_range);
    let colors = vec![(62,43,88), (239,121,93), (255,0,0)].into_iter().cycle();
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
            .x_desc(format!("{} ({})", data_vectors[0].x_name, data_vectors[0].x_units))
            .y_desc(format!("{} ({})", data_vectors[0].y_name, data_vectors[0].y_units))
            .y_label_formatter(&|x| format!("{:.2e}", x))
            .draw()
            .unwrap();

        for (data_vector, color) in data_vectors.into_iter().zip(colors) {
            chart
                .draw_series(LineSeries::new(
                    data_vector.values()
                        .iter()
                        .map(|p| (p.x as f32, p.y as f32)),
                    &RGBColor(color.0,color.1,color.2),
                ))
                .unwrap()
                .label(data_vector.name.as_str())
                .legend(move |(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], &RGBColor(color.0,color.1,color.2)));
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


pub fn maximum(samples: &Vec<f64>) -> f64 {
    if samples.len() == 0 {
        panic!("Cannot find the max of empty vec");
    }
    let mut max: f64 = samples[0];
    for s in samples {
        if *s > max {
            max = *s;
        }
    }
    max
}

pub fn minimum(samples: &Vec<f64>) -> f64 {
    if samples.len() == 0 {
        panic!("Cannot find the min of empty vec");
    }
    let mut min: f64 = samples[0];
    for s in samples {
        if *s < min {
            min = *s;
        }
    }
    min
}
