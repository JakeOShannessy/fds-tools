use fds_input_parser::parse_and_decode_fds_input_file;
use fute_core::parse_smv_file;
use std::path::Path;
use std::fs::File;
use std::io::prelude::*;
use csv;
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
    let meshes: Vec<fds_input_parser::decode::Mesh> = fds_data.meshes;
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

// peakHRR :: FilePath -> IO ()
// peakHRR path = do
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
//     theDataVectors <- getDataListAll simulation
//     let Just (DataVectorPair _ (DataVector _ _ valuesY)) = findDVectorPairByYNameMaybe theDataVectors ("kW", "HRR")
//         // chr = chart (chid ++ " - Realised HRR") eurocodeHRRChartConfig [hrrVector]
//     let maxHRR = V.maximum valuesY
//     printf "%.2f" maxHRR
//     return ()

// plotHRR :: FilePath -> IO ()
// plotHRR path = createHRRPlots path >> return ()
pub fn plot_hrr(smv_path: &Path) {
    let mut file = File::open(smv_path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let (_, smv_file) = parse_smv_file(&contents).unwrap();
    // for csvf in smv_file.csvfs {
    //     println!("csvf: {:?}", csvf);
    // }
    let hrr_csvf = smv_file.csvfs.iter().find(|csvf| csvf.type_ == "hrr").unwrap();
    println!("csvfhrr: {:?}", hrr_csvf);
    let value = "HRR";
    let csv_file = File::open(&hrr_csvf.filename).unwrap();
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
    // let mut data_vec = Vec::new();
    // for result in rdr.deserialize() {
    //     // The iterator yields Result<StringRecord, Error>, so we check the
    //     // error here.
    //     let record: Vec<f64> = result.unwrap();
    //     let t = record.get(time_index).expect("time val").clone();
    //     let v = record.get(value_index).expect("val").clone();
    //     data_vec.push(DataEntry { x: t, y: v });
    // }
    // let data_vector = DataVector {
    //     values: data_vec,
    //     name_x: "Time".to_string(),
    //     units_x: "s".to_string(),
    //     name_y: info.value.clone(),
    //     units_y: "(unknown)".to_string(),
    // };
    // ok(Json(data_vector))
    unimplemented!()
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

pub fn quick_chart(smv_path: &Path) {
    println!("quick-charting: {:?}", smv_path);
}
