module FDSUtilities.Parsing.SimulationData
    ( getDataList
    , DataVector(DataVector, dataVectorName, dataVectorUnits, dataVectorValues)
    , DataVectorPair(DataVectorPair, yVector, xVector)
    , FlexibleDataVector(..)
    , findDVectorPairByYName
    , findDVectorPairByYNameMaybe
    , getDataListAll
    , getOutLocProp
    , getOutProp
    , getOutPropTStep
    , readCaseCSVFiles
    , createTimeVectorPairs
    , findDVectorByName
    , readCSV
    , readCSV2DVector
    , possCreateVecs
    )
    where

import Control.Exception as E
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Csv
import Data.List
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import qualified Data.Vector.Generic as VG
import FDSUtilities.Types
import System.Directory
import System.FilePath

getDataList :: FDSSimulation -> [String] -> IO [DataVectorPair Double Double]
getDataList sim names' = do
    dataList <- E.catch (readCaseCSVFiles sim) handler
    return $ createTimeVectorPairs (singleVecs dataList)
    where
        names = "FDS_HRR_Time":names'   -- "FDS Time" was included here in the python version, was proly optional
        singleVecs :: [DataVector Double] -> [DataVector Double]
        singleVecs dList = foldr (func dList) [] names
        func :: [DataVector Double] -> String -> [DataVector Double] -> [DataVector Double]
        func dList name acc = ((\(Just x)->x) $ findDVectorByName dList name):acc

        handler :: SomeException -> IO [DataVector Double]
        handler e = do
            print e
            _ <- getLine
            return []

getDataListAll :: FDSSimulation -> IO [DataVectorPair Double Double]
getDataListAll sim = do
    dataList <- readCaseCSVFiles sim
    return $ createTimeVectorPairs dataList
    -- where
        -- names = "FDS_HRR_Time":names'   -- "FDS Time" was included here in the python version, was proly optional
        -- singleVecs :: [DataVector Double] -> [DataVector Double]
        -- singleVecs dList = foldr (func dList) [] names
        -- func :: [DataVector] -> String -> [DataVector Double] -> [DataVector Double]
        -- func dList name acc = (findDVectorByName dList name):acc


createTimeVectorPairs :: [DataVector Double] -> [DataVectorPair Double Double]
createTimeVectorPairs vectors = tVectorList
    where
        timeVector = case findDVectorByName vectors "Time" of
            Just x -> x
            Nothing -> case findDVectorByName vectors "FDS_HRR_Time" of -- for fds5
                Just x -> x
                Nothing -> error "Time vector not found."
        tVectorList :: [DataVectorPair Double Double]
        tVectorList = foldr func [] vectors
        func  (DataVector vecName a b) acc = if (vecName /= "FDS Time") && (vecName /= "FDS_HRR_Time") && (vecName /= "Time")
                                then (DataVectorPair timeVector (DataVector vecName a b)):acc
                                else acc

-- |Remove any empties from CSV
cleanCSV :: [[String]] -> [[String]]
cleanCSV csv = delete [""] csv

findDVectorByName :: [DataVector a] -> String -> Maybe (DataVector a)
findDVectorByName (v:vectors) searchName = if name == searchName then Just v else findDVectorByName vectors searchName
    where
        (DataVector name units datVals) = v
findDVectorByName [] name = Nothing

findDVectorPairByYName :: (V.Unbox a, V.Unbox b) => [DataVectorPair a b] -> String -> DataVectorPair a b
findDVectorPairByYName (v:vectors) searchName = if name == searchName then v else findDVectorPairByYName vectors searchName
    where
        (DataVectorPair _ (DataVector name _ _)) = v
findDVectorPairByYName [] name = error $ "findDVectorPairByYName: could not find " ++ name

findDVectorPairByYNameMaybe :: (V.Unbox a, V.Unbox b) => [DataVectorPair a b] -> (String,String) -> Maybe (DataVectorPair a b)
findDVectorPairByYNameMaybe (v:vectors) (searchUnits, searchName)
    = if name == searchName && units == searchUnits
        then Just v
        else findDVectorPairByYNameMaybe vectors (searchUnits, searchName)
    where
        (DataVectorPair _ (DataVector name units _)) = v
findDVectorPairByYNameMaybe [] _ = Nothing

-- getOutLocProp :: (MeshStep -> NumAndCoord) -> OutData -> [DataVectorPair Double Double] -- output a DataVectorPair for each mesh
-- getOutLocProp f outData = map (\(n, (t, prop)) -> DataVectorPair (DataVector "Time" "s" (V.fromList t)) (DataVector ("Mesh " ++ show n) "" (V.fromList prop))) tt
--     where
--         tSteps = timesteps outData
--         ff = map unzip $ transpose $ map worker tSteps
--         tt = zip [1..(length ff)] ff
--         worker :: TimeStep -> [(Double, Double)]
--         worker tStep = zip (repeat (simTime tStep)) props
--             where
--                 meshSteps = timeStepMeshes' tStep
--                 props = map ((\(NumAndCoord prop coord) -> prop) . f) meshSteps

getOutLocProp :: String -> OutData -> [DataVectorPair Double Double] -- output a DataVectorPair for each mesh
getOutLocProp propName outData = map (\(n, (t, prop)) -> DataVectorPair (DataVector "Time" "s" (V.fromList t)) (DataVector ("Mesh " ++ show (n :: Int)) "" (V.fromList prop))) tt
    where
        tSteps = timesteps outData
        ff = map unzip $ transpose $ map worker tSteps
        tt = zip [1..] ff
        worker :: TimeStep -> [(Double, Double)]
        worker tStep = zip (repeat (simTime tStep)) props
            where
                meshSteps = timeStepMeshes' tStep
                props = map (getOutLocPropSingle propName) meshSteps

-- TODO: remove this pattern match
getOutLocPropSingle :: String -> MeshStep -> Double
getOutLocPropSingle propName meshStep =
    let Just x = find (\stepProp->stepPropKey stepProp == propName) (meshStepProps meshStep)
        ValueDouble d = stepPropValue x
    in d

getOutProp :: (MeshStep -> Double) -> OutData -> [DataVectorPair Double Double] -- output a DataVectorPair for each mesh
getOutProp f outData = map (\(n, (t, prop)) -> DataVectorPair (DataVector "Time" "s" (V.fromList t)) (DataVector ("Mesh " ++ show (n :: Int)) "" (V.fromList prop))) tt
    where
        tSteps = timesteps outData
        ff = map unzip $ transpose $ map worker tSteps
        tt = zip [1..] ff
        worker :: TimeStep -> [(Double, Double)]
        worker tStep = zip (repeat (simTime tStep)) props
            where
                meshSteps = timeStepMeshes' tStep
                props = map f meshSteps

getOutPropTStep :: (TimeStep -> Double) -> OutData -> [DataVectorPair Double Double] -- output a DataVectorPair for each mesh
getOutPropTStep f outData = map (\(n, (t, prop)) -> DataVectorPair
    (DataVector "Time" "s" (V.fromList t))
    (DataVector ("Mesh " ++ show n) "" (V.fromList prop))
    ) tt
    where
        tSteps = timesteps outData
        vs :: ([Double], [Double])
        vs = unzip $ map (\tStep->(simTime tStep, f tStep)) tSteps
        tt :: [(Int, ([Double], [Double]))]
        tt = zip [1..] [vs]

readCaseCSVFiles :: FDSSimulation -> IO [DataVector Double]
readCaseCSVFiles sim = do
    hrrVectors <- possCreateVecs hrrPath
    devcVectors <- possCreateVecs devcPath
    massVectors <- possCreateVecs massPath
    evacVectors <- possCreateVecs evacPath
    ctrlVectors <- possCreateVecs ctrlPath
    return $ hrrVectors ++ devcVectors ++ massVectors ++ evacVectors ++ ctrlVectors
    where
        caseDir = simDir sim
        caseName = simCHID sim
        hrrPath  = joinPath [caseDir, caseName ++ "_hrr.csv"]
        devcPath = joinPath [caseDir, caseName ++ "_devc.csv"]
        massPath = joinPath [caseDir, caseName ++ "_mass.csv"]
        evacPath = joinPath [caseDir, caseName ++ "_evac.csv"]
        ctrlPath = joinPath [caseDir, caseName ++ "_ctrl.csv"]

possCreateVecs :: FilePath -> IO [DataVector Double]
possCreateVecs path = do
    exists <- doesFileExist path
    if exists then csv2Vectors path else return []

csv2Vectors :: FilePath -> IO [DataVector Double]
csv2Vectors path = readCSV2DVector path

-- |Read in csv as Data Vectors
readCSV2DVector :: FilePath -> IO [DataVector Double]
readCSV2DVector file = do
    (names, units, unalignedVals) <- readCSV file
    let
        cols :: [V.Vector Double]
        cols = retrans [] unalignedVals
        sets :: [(String, String, V.Vector Double)]
        sets = zip3 names units (reverse cols)
        col2Vec (name, units, values) = DataVector name units values
    return $ map col2Vec $ sets

-- TODO: remove the below and parse differently
-- | Convert from vector of rows (which are lists) to vector of columns (which are vectors)
retrans :: [V.Vector Double] -> [V.Vector Double] -> [V.Vector Double]
retrans doneVecs [] = doneVecs
retrans doneVecs todoVecs | not $ V.null $ headErr "retrans" todoVecs = retrans (cVec:doneVecs) remVecs
                          | otherwise = doneVecs
    where
        cVec = V.fromList $ map V.head todoVecs
        remVecs = map V.tail todoVecs
-- TODO: Modify cassava to allow for multiple header lines.
-- TODO: Also, return the values of the header.
-- TODO: Skip unrequested columns.
-- TODO: Assumes windows line endings
-- TODO: fix dodgy parsing of headers
readCSV :: FilePath -> IO ([String], [String], [V.Vector Double])
readCSV filepath = do
    fileData1 <- BL.readFile filepath
    let
        unitsLine = BL.takeWhile (/= (fromIntegral (ord '\r'))) fileData1
        fileDataWithoutUnits = BL.drop 1 $ (BL.dropWhile (/= (fromIntegral (ord '\n')))) fileData1
        nameLine = BL.takeWhile (/= (fromIntegral (ord '\r'))) fileDataWithoutUnits

        headerUnits = case decode NoHeader unitsLine of
            Right x -> headErr "readCSV" $ VB.toList x
            Left e -> error $ "Fail: " ++ show e ++ "\n" ++ show unitsLine
        headerNames = case decode NoHeader nameLine of
            Right x -> headErr "readCSV" $ VB.toList x
            Left e -> error $ "Fail: " ++ show e ++ "\n" ++ show nameLine

        fileData = BL.drop 1 $ (BL.dropWhile (/= (fromIntegral (ord '\n')))) fileDataWithoutUnits
    -- print headerNames
    let cs :: [V.Vector Double]
        cs = case decode NoHeader fileData :: Either String (VB.Vector (VB.Vector Double)) of
                Right records -> map VG.convert $ VG.toList records
                Left e -> error (filepath ++ "  ---  " ++ e)
    return (headerNames, headerUnits, cs)
