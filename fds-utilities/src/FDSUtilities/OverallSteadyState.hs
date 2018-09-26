module FDSUtilities.OverallSteadyState where


import Control.Lens
import Control.Monad
import Control.DeepSeq

import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
-- import Data.Colour
-- import Data.Colour.Names
-- import Data.Colour.SRGB
import Data.Default
import Data.Either
import Data.List
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Data.Array.Repa as R

import FDSUtilities.Parsing.SliceFile
import FDSUtilities.Parsing.SMVFile
import FDSUtilities.Parsing.PLOT3D
import FDSUtilities.Parsing
import qualified FDSUtilities.Plot as P
import FDSUtilities.Plot (chart)
import FDSUtilities.Types
import FDSUtilities.Types.Monitor

import GHC.Float

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import System.Directory
import System.FilePath
import System.FilePath.Glob as G
import System.IO

import qualified Text.ParserCombinators.Parsec as Parsec

plotQuantity pl3dData transform title filename longname =
    let
        quantityData = getQuantity pl3dData longname
        transformedQuantity = transform quantityData
        vector = DataVectorPair
            { xVector = DataVector
                { dataVectorName = "Time"
                , dataVectorUnits = "s"
                , dataVectorValues = VU.map fst transformedQuantity
                }
            , yVector = DataVector
                { dataVectorName = longname
                , dataVectorUnits = pl3dEntryUnits $ pl3dData_entry quantityData
                , dataVectorValues = VU.map snd transformedQuantity
                }
            }
    in P.produceChart "charts" def [vector] title filename


produceSteadyStateCharts :: ChartConfig -> SteadyStateAnalysis -> CompilationActionFunction
produceSteadyStateCharts chartConf analysisConf destDir simulation = do
    let dir = simDir simulation
        projCHID = simCHID simulation
    createDirectoryIfMissing True destDir
    parsedDataEither <- parseSimulationSMVFile simulation
    let parsedData = case parsedDataEither of
            Left e -> error $ show e
            Right p -> p
    let meshes = smvMeshes parsedData
        cellDims = createCellDimensionMap meshes
        meshNames = createMeshNameMap meshes
        dataFileEntries = smvDataFiles parsedData
        selectedDataFileEntries = dataFileEntryFilterSLCF (steadyStateAnalysis_dataFileFilter analysisConf) dataFileEntries
    let selectedDataFilePaths = map (\x->joinPath [dir, x]) $ map slcfFilename selectedDataFileEntries
    print "dataFileEntries"
    mapM_ print dataFileEntries
    print "selectedDataFileEntries"
    mapM_ print selectedDataFileEntries
    withLocs <- mapM (addLocationData dir parsedData) selectedDataFileEntries
    print "withLocs"
    mapM_ print withLocs
    let toRender = filter (steadyStateAnalysis_locationFilter analysisConf) withLocs
    let visPaths = takeTablePaths toRender
    individualPaths <- (liftM concat) $ mapM (parseSumAndRenderSlice chartConf cellDims meshNames destDir) toRender
    totalPaths <- (parseSumAndRenderSliceTotal chartConf destDir) visPaths

    let renderPaths = totalPaths ++ individualPaths
    return renderPaths

data SteadyStateAnalysis = SteadyStateAnalysis
    { steadyStateAnalysis_dataFileFilter :: SSDataFileFilter
    , steadyStateAnalysis_locationFilter :: ((Int, DataFileEntry, SliceDataHeader, (Double, Double, Double, Double, Double, Double), FilePath) -> Bool)
    }

data SSDataFileFilter = SSDataFileFilter
    { meshRule :: Int -> Bool
    , longNameRule :: String -> Bool
    , shortNameRule :: String -> Bool
    , unitsRule :: String -> Bool
    }

dataFileEntryFilterSLCF :: SSDataFileFilter ->  [DataFileEntry] -> [DataFileEntry]
dataFileEntryFilterSLCF f@(SSDataFileFilter meshRule longNameRule shortNameRule unitsRule) dataFileEntries = filter theFilter dataFileEntries
    where
        theFilter dataFileEntry = case dataFileEntry of -- TODO: this is unsatisfactory as it relies on the layout of SLCFDataFile
            SLCFDataFile meshNum filename longName shortName units aliaas ->
                                    let conditions =
                                            [ meshRule meshNum
                                            , longNameRule longName
                                            , shortNameRule shortName
                                            , unitsRule units
                                            ]
                                    in all id conditions
            _ -> False

produceCounts :: FilePath -> FDSSimulation -> SteadyStateAnalysis -> IO [FilePath]
produceCounts destDir simulation analysisConf = do
    let dir = simDir simulation
        projCHID = simCHID simulation
    createDirectoryIfMissing True destDir
    Right parsedData <- parseSimulationSMVFile simulation
    let meshes = smvMeshes parsedData
        {-meshTRNs = map smvMeshTRNs meshes-}
        cellDims = createCellDimensionMap meshes
        meshNames = createMeshNameMap meshes
        dataFileEntries = smvDataFiles parsedData
        sliceFileEntries = filter (isSLCFEntry) dataFileEntries
        selectedDataFileEntries = dataFileEntryFilterSLCF (steadyStateAnalysis_dataFileFilter analysisConf) dataFileEntries
    -- let selectedDataFilePaths = map (\x->joinPath [dir, x]) $ map slcfFilename selectedDataFileEntries
    -- let dataFileInfo = do
            -- locs <- mapM (addLocationData dir parsedData) selectedDataFileEntries
            -- where
                -- paths = map (\x-> (x, joinPath [dir, slcfFilename x])) selectedDataFileEntries
    print "dataFileEntries"
    print dataFileEntries
    mapM_ print selectedDataFileEntries
    withLocs <- mapM (addLocationData dir parsedData) selectedDataFileEntries
    mapM_ print withLocs
    let toRender = filter (steadyStateAnalysis_locationFilter analysisConf) withLocs
    let threshold = (< 10.0)
    {-mapM_ print toRender-}
    {-print $ length withLocs-}
    {-print $ length toRender-}
    -- let visPaths = takeTablePaths toRender
    {-print visPaths-}
    {-individualCounts <- mapM (\x -> parseAndCountSlice x (< 10.0)) visPaths-}
    individualPaths <- mapM (parseCountAndRenderSlice cellDims meshNames destDir threshold) toRender
    totalPath <- parseCountAndRenderSliceTotal cellDims meshNames destDir threshold toRender

    {-mapM_ print individualCounts-}
    -- mapM_ print individualPaths
    let renderPaths = totalPath ++ individualPaths
    return renderPaths

-- |Verify that each cell in the mesh has the same dimensions (no bias) and return the cell dimensions (x, y, z) in metres.
meshDimensions mesh = (x, y, z)
    where
        MeshTRNs trnX trnY trnZ = smvMeshTRNs mesh
        x = getDimension trnX
        y = getDimension trnY
        z = getDimension trnZ
        getDimension :: [(Int, Double)] -> Double
        getDimension trn = checkConsistency (guessDimension trn) trn
            where
                checkConsistency :: Double -> [(Int, Double)] -> Double
                checkConsistency guess (b:[]) = guess
                checkConsistency guess (a:b:trn) | eq 0.01 (subTRN b a) guess = checkConsistency guess (b:trn)   -- TODO: Remove hard coded tolerance
                                                 | otherwise = error $ "Task cannot be performed on meshes that contain biased cells.\n" ++ show (subTRN b a) ++ " /= " ++ show guess ++ "\n"
                guessDimension :: [(Int, Double)] -> Double
                guessDimension (a:b:trn) = subTRN b a
                subTRN :: (Int, Double) -> (Int, Double) -> Double
                subTRN b@(bn, bv) a@(an, av) = bv - av
        {-checkConsistency trn = foldl' -}

eq tol a b = tol > abs (a-b)


-- TODO: Verify the function below and ensure that the meshes correspond.
-- |Create a Map: meshNumber -> cell dimensions
createCellDimensionMap meshes = M.fromList $ zip [1..] dimensions
    where
        dimensions = map meshDimensions meshes
createMeshNameMap meshes = M.fromList $ zip [1..] names
    where
        names = map smvMeshName meshes

isSLCFEntry dataFileEntry = case dataFileEntry of
                                (SLCFDataFile _ _ _ _ _ _) -> True
                                _ -> False


addLocationData dir smvFile dataFileEntry@(SLCFDataFile
        { slcfMeshNum = meshNum
        , slcfFilename = filename
        , slcfLongName = longName
        , slcfShortName = shortName
        , slcfUnits = units
        })
        = do
            Right (sliceHeader@(SliceDataHeader lName sName units (i1, i2, j1, j2, k1, k2)), filepath) <- parseSingleSliceHeaderForTable (joinPath [dir, filename])
            let (x1, y1, z1) = case getCornerXYZ trns (i1, j1, k1) of
                                Left x -> error x
                                Right (x1, y1, z1) -> (x1, y1, z1)
                (x2, y2, z2) = case getCornerXYZ trns (i2, j2, k2) of
                                Left x -> error x
                                Right (x2, y2, z2) -> (x2, y2, z2)
                res = (meshNum, dataFileEntry, sliceHeader, (x1, x2, y1, y2, z1, z2), filepath)
            return $ res
            where
                meshes = smvMeshes smvFile
                mesh = meshes !! (meshNum - 1)
                trns = smvMeshTRNs mesh





-- dataFileFilterSliceTableEntry meshTRNs sliceTabl
getRealSliceLocations trns slice@(SliceDataHeader lName sName units (i1, i2, j1, j2, k1, k2)) = (x1, x2, y1, y2, z1, z2)
    where
        (x1, y1, z1) = case getCellLocation trns (i1, j1, k1) of
                        Left x -> error x
                        Right (x1, y1, z1) -> (x1, y1, z1)
        (x2, y2, z2) = case getCellLocation trns (i2, j2, k2) of
                        Left x -> error x
                        Right (x2, y2, z2) -> (x2, y2, z2)


createSliceFileTable filepaths = do
    (liftM rights) $ mapM parseSingleSliceHeaderForTable filepaths

filterTable short = filter (\((SliceDataHeader lName sName units location), filepath) -> sName == short)

sumSlice :: SliceDataSet -> DataVectorPair Double Double
sumSlice (SliceDataSet header dat) = DataVectorPair (DataVector "Time" "s" (V.fromList ts)) (DataVector "Sum" "?" (V.fromList ds))
    where
        sumSnapshot (Snapshot t d) = (float2Double t, float2Double $ R.sumAllS d)
        (ts, ds) = unzip $ map sumSnapshot dat

takeTablePaths = map (\(n, dataFileEntry, sliceHeader, location, filepath) -> filepath)

addDVectorPairs :: DataVectorPair Double Double -> DataVectorPair Double Double -> DataVectorPair Double Double
addDVectorPairs v1@(DataVectorPair x1@(DataVector _ _ x1Data) y1) v2@(DataVectorPair x2@(DataVector _ _ x2Data) y2)
    | checkVectorSameness v1 v2 = DataVectorPair x1 (addDVectors y1 y2)
    | otherwise = let (av1, av2) = shoreVectorPairs v1 v2
                  in if checkVectorSameness av1 av2 then addDVectorPairs av1 av2 else error $ "x vectors are different.\n" ++ showXDataSideBySide av1 av2 ++ "\n" ++ showLengths av1 av2  -- TODO: susceptible to infinite loop

showLengths v1@(DataVectorPair x1@(DataVector _ _ x1Data) y1) v2@(DataVectorPair x2@(DataVector _ _ x2Data) y2) = show (V.length x1Data) ++ " vs " ++ show (V.length x2Data)
checkVectorSameness v1@(DataVectorPair x1@(DataVector _ _ x1Data) y1) v2@(DataVectorPair x2@(DataVector _ _ x2Data) y2) = x1Data == x2Data
showXDataSideBySide v1@(DataVectorPair x1@(DataVector _ _ x1Data) y1) v2@(DataVectorPair x2@(DataVector _ _ x2Data) y2) = concatMap (\(x1, x2) -> if x1 == x2 then "" else show (x1,x2) ++ "\n") $ zip (V.toList x1Data) (V.toList x2Data)
-- | If one data vector has additional data at the end, trim it
shoreVectorPairs v1@(DataVectorPair x1@(DataVector x1Name x1Units x1Data) y1@(DataVector y1Name y1Units y1Data)) v2@(DataVectorPair x2@(DataVector x2Name x2Units x2Data) y2@(DataVector y2Name y2Units y2Data))
    = ((DataVectorPair (DataVector x1Name x1Units (V.take l x1Data)) (DataVector y1Name y1Units (V.take l y1Data))),
    (DataVectorPair (DataVector x2Name x2Units (V.take l x2Data)) (DataVector y2Name y2Units (V.take l y2Data))))
    where
        l = min (V.length x1Data) (V.length x2Data)
addDVectors :: DataVector Double -> DataVector Double -> DataVector Double
addDVectors (DataVector name1 units1 values1) (DataVector name2 units2 values2) =
    DataVector name1 units1 (V.zipWith (+) values1 values2) --TODO: check the names and unit values

sumDVectorPairs [] = error "sumDVectorPairs: No dataVectorPairs to sum."
sumDVectorPairs dataVectors = foldl1' addDVectorPairs dataVectors

parseAndSumSlice :: FilePath -> IO (SliceDataHeader, DataVectorPair Double Double)
parseAndSumSlice filepath = do
    (Right (header@(SliceDataHeader lName sName units location), parsedData)) <- parseSliceFileChunkedSum filepath
    {-print header-}
    let (ts, ds) = unzip parsedData
        summedVectors = DataVectorPair
            (DataVector "Time" "s" (V.fromList $ map float2Double ts))
            (DataVector "Raw" "m/cell" (V.fromList $ map float2Double ds))
    return (header, summedVectors)

-- parseAndCountSlice :: FilePath -> (Float -> Bool) -> Double -> IO (SliceDataHeader, DataVectorPair Double Double)
parseAndCountSlice threshold cellDimsTable (meshNum, dataFileEntry, sliceHeader, boundingBox, filepath) = do
    let (Just (cellDimension, y, z)) = M.lookup meshNum cellDimsTable
        _ = if cellDimension /= y then error "cells are skewed" else ()
        _ = if cellDimension /= z then error "cells are skewed" else ()
    (Right (header@(SliceDataHeader lName sName units location), parsedData)) <- parseSliceFileChunkedArea filepath threshold
    {-print header-}
    let (ts, ds) = unzip parsedData
        {-counts = map (\(t, vals) -> (t, length vals)) parsedData-}
        counts = DataVectorPair
            (DataVector
                "Time"
                "s"
                (V.map float2Double (V.fromList ts))
            )
            (DataVector
            "Untenable Area"
            "m^2"
            (V.fromList (map ((*cellDimension**2) . fromIntegral . length) ds))
            )
    return (header, counts)


-- parseSumAndRenderSliceTotal destDir filepaths = do
    -- -- let (Just name) = M.lookup meshNum meshNamesTable
        -- -- (Just (cellDimension, y, z)) = M.lookup meshNum cellDimsTable -- WARNING TODO: This will provide incorrect values for cells with any skew
    -- (headers, summedSlices) <- (liftM unzip) $ mapM parseAndSumSlice filepaths
    -- let totalSum = sumDVectorPairs summedSlices
        -- renderable = chart ("Total Lost Visibility") def [totalSum]
    -- -- return $ deepseq res res
    -- let filepath = joinPath [destDir, "Total" ++ ".png"]
    -- _ <- renderableToFile (FileOptions (1200,600) PNG) renderable filepath
    -- return filepath

parseSumAndRenderSliceTotal chartConf destDir filepaths = do
    -- let (Just name) = M.lookup meshNum meshNamesTable
        -- (Just (cellDimension, y, z)) = M.lookup meshNum cellDimsTable -- WARNING TODO: This will provide incorrect values for cells with any skew
    (headers, summedSlices) <-  (liftM unzip) $ mapM parseAndSumSlice filepaths
    let nCells = sum $ map meshNCells headers
    let totalSum = sumDVectorPairs summedSlices
        totalSumTAveraged = timeAverageDataVectorPair totalSum
        totalSumNorm = normaliseSumData nCells totalSum
        totalSumTAveragedNorm = normaliseSumData nCells totalSumTAveraged
    let renderableRaw  = chart ("Total Value") chartConf [totalSum, totalSumTAveraged]
        filepathRaw  = joinPath [destDir, "Total"]
    _ <- renderableToFile (FileOptions (800,400) SVG) (filepathRaw ++ ".svg") renderableRaw
    _ <- renderableToFile (FileOptions (800,400) PNG) (filepathRaw ++ ".png") renderableRaw
    return [filepathRaw]


parseSumAndRenderSlice chartConf cellDimsTable meshNamesTable destDir (meshNum, dataFileEntry, sliceHeader, boundingBox, filepath) = do
    let (Just name) = M.lookup meshNum meshNamesTable
        (Just (cellDimension, y, z)) = M.lookup meshNum cellDimsTable -- WARNING TODO: This will provide incorrect values for cells with any skew
        _ = if cellDimension /= y then error "cells are skewed" else ()
        _ = if cellDimension /= z then error "cells are skewed" else ()
    (header@(SliceDataHeader lName sName units (i1, i2, j1, j2, k1, k2)), summedVectorsRaw) <- parseAndSumSlice filepath
    let nCellsSlice = (i2-i1+1)*(j2-j1+1)*(k2-k1+1) -- TODO: use abs to account for the incorrect ordering of location data.  -- TODO: consider the effect of obstructions -- The number of cells in the slice
        -- summedVectorsTAverage = timeAverageDataVectorPair summedVectorsRaw
        -- summedVectorsNorm = normaliseSumData nCellsSlice summedVectorsRaw   -- TODO: catch NaNs and Infinitys
        -- summedVectorsTAveragedNorm = normaliseSumData nCellsSlice summedVectorsTAverage    -- TODO: Investigate whether the order of these operations is important. At least ensure consistency.
    -- print "nCellsSlice"
    -- print nCellsSlice
    -- print $ Data.List.take 100 $ (dataVectorValues . yVector) summedVectorsNorm
    let (SliceDataHeader lName sName units location) = header
    let renderableRaw  = chart (name ++ " " ++ show boundingBox ++ " - Mesh Lost Visibility") chartConf [{-summedVectorsRaw,-}summedVectorsRaw]
        -- renderableTAveraged = chart (name ++ " " ++ show boundingBox ++ " - Mesh Lost Visibility - Time Averaged") def [summedVectorsTAverage]

        -- renderableNorm = chart (name ++ " " ++ show boundingBox ++ " - Mesh Lost Visibility - Normalised") chartConf [{-summedVectorsNorm,-} summedVectorsNorm]


        -- renderableTAveragedNorm = chart (name ++ " " ++ show boundingBox ++ " - Mesh Lost Visibility - Time Averaged - Normalised") def [summedVectorsTAveragedNorm]
    -- return $ deepseq res res
    let filepathRaw  = joinPath [destDir, name ++ " - Mesh Steady State.svg"]
        -- filepathNorm = joinPath [destDir, name ++ " - Mesh Steady State - Normalised.svg"]
    -- putStrLn "summedVectorsTAverage"
    -- print $ summedVectorsTAverage
    _ <- renderableToFile (FileOptions (1200,600) SVG) filepathRaw renderableRaw
    -- _ <- renderableToFile (FileOptions (1200,600) SVG) renderableNorm filepathNorm
    return [filepathRaw {-, filepathNorm-}]

meshNCells meshHeader@(SliceDataHeader lName sName units (i1, i2, j1, j2, k1, k2)) = (i2-i1+1)*(j2-j1+1)*(k2-k1+1)

normaliseSumData nCellsSlice vectorPair =  vectorPair {yVector = newYVector}
    where
        oldYVector = yVector vectorPair
        yVals = dataVectorValues oldYVector
        newYVector = oldYVector {dataVectorValues = V.map (/(fromIntegral nCellsSlice)) yVals} -- , dataVectorName = "Normalised"}

timeAverageDataVectorPair :: DataVectorPair Double Double -> DataVectorPair Double Double
timeAverageDataVectorPair dvPair@(DataVectorPair (DataVector xName xUnits xValues) (DataVector yName yUnits yValues)) = (DataVectorPair (DataVector xName xUnits (V.fromList ts)) (DataVector (yName ++ " - Time Averaged") yUnits (V.fromList vs)))
    where
        zippedVals = zip (V.toList xValues) (V.toList yValues)
        (ts, vs) = unzip $ timeAverageZippedValues [] [] zippedVals
        -- TODO: the time values are not actually relevant for the averaging process, could safely ignore them.
timeAverageZippedValues :: [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)]
timeAverageZippedValues averaged _ [] = reverse averaged
timeAverageZippedValues averaged previous (curr@(currT, currV):upcoming) = timeAverageZippedValues ((currT,averageValue):averaged) (curr:previous) upcoming
    where
        windowSize = 81 :: Int
        prevSize = floor ((fromIntegral windowSize)/2)
        nextSize = (ceiling ((fromIntegral windowSize)/2)) - 1
        totalOther = prevSize + nextSize
        averageValue :: Double
        averageValue = let window = Data.List.take prevSize previous ++ [curr] ++ Data.List.take nextSize upcoming
                        in (tupSum window) / (fromIntegral (length window))
                            -- TODO: make explicit definition for how time averaging is defined.
        tupSum :: [(Double, Double)] -> Double
        tupSum = foldl' (\acc (_,y) -> acc + y) 0

applyToYVector :: (Double -> Double) -> DataVectorPair Double Double -> DataVectorPair Double Double
applyToYVector f vectorPair = vectorPair {yVector = newYVector}
    where
        oldYVector = yVector vectorPair
        yVals = dataVectorValues oldYVector
        newYVector = oldYVector {dataVectorValues = V.map f yVals}

parseCountAndRenderSlice cellDimsTable meshNamesTable destDir threshold entry@(meshNum, dataFileEntry, sliceHeader, boundingBox, filepath) = do
    let (Just name) = M.lookup meshNum meshNamesTable
    (header, summedVectors) <- parseAndCountSlice threshold cellDimsTable entry
    let (SliceDataHeader lName sName units location) = header
    let renderable = chart (name ++ " " ++ show boundingBox ++ " - Untenable Area") def [summedVectors]
    let outFilePath = (joinPath [destDir, name ++ "-UntenableArea.png"])
    _ <- renderableToFile (FileOptions (1200,600) SVG) outFilePath renderable
    return outFilePath

parseCountAndRenderSliceTotal cellDimsTable meshNamesTable destDir threshold entries = do
    (headers, summedSlices) <-  (liftM unzip) $ mapM (parseAndCountSlice threshold cellDimsTable) entries
    let totalSum = sumDVectorPairs summedSlices
    let renderableRaw  = chart ("Total Untenable Area") def [totalSum]
    let filepathRaw  = joinPath [destDir, "Total" ++ ".svg"]
    _ <- renderableToFile (FileOptions (1200,600) SVG) filepathRaw renderableRaw
    return [filepathRaw]

parseSliceFileChunkedArea :: FilePath -> (Float -> Bool) -> IO (Either String (SliceDataHeader, [(Float, [Float])]))
parseSliceFileChunkedArea filepath threshold = do
    lazyRead <- BL.readFile filepath
    let chunks = BL.toChunks lazyRead
        init = parse (parseSliceFileParserArea threshold) (head chunks)
        res = foldl' feed init (tail chunks)
    return $ eitherResult $ res `feed` B.empty

parseSliceFileChunkedSum :: FilePath -> IO (Either String (SliceDataHeader, [(Float, Float)]))
parseSliceFileChunkedSum filepath = do
    lazyRead <- BL.readFile filepath
    let chunks = BL.toChunks lazyRead
        init = parse parseSliceFileParserSum (head chunks)
        res = foldl' feed init (tail chunks)
    return $ eitherResult $ res `feed` B.empty

parseSingleSliceHeaderForTable :: FilePath -> IO (Either String (SliceDataHeader, FilePath))
parseSingleSliceHeaderForTable filepath = withFile filepath ReadMode $ \fHndl -> do
    rawData <- B.hGet fHndl 200 -- TODO: check that this is the right length for the header, change to parse as the file is read and only read the necessary amount
    let header = parseSliceHeader rawData
    return $ case header of
                Left x -> Left "Parse error."
                Right x -> Right (x, filepath)

-- parseSingleSliceHeader :: FilePath -> IO (Either String SliceDataHeader)
-- parseSingleSliceHeader filepath = do
    -- rawData <- B.readFile filepath
    -- let res = parseSliceHeader rawData
    -- print res
    -- return $ deepseq res res

parseSingleSliceHeader :: FilePath -> IO (Either String SliceDataHeader)
parseSingleSliceHeader filepath = withFile filepath ReadMode $ \fHndl -> do
    rawData <- B.hGet fHndl 200
    let res = parseSliceHeader rawData
    {-print res-}
    return $ deepseq res res

parseSingleSlice :: FilePath -> IO (Either String SliceDataSet)
parseSingleSlice filepath = parseSliceFile filepath

printSuccess (Right x) = putStrLn "Success"
printSuccess (Left x) = do
    putStr "Failure\t"
    print x
