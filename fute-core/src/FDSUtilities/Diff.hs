module FDSUtilities.Diff where
{-
import Data.Array.Repa ((:.)(..), Z(..))
import qualified Data.Array.Repa as R
import FDSUtilities.Parsing
import FDSUtilities.Parsing.SliceFile
import FDSUtilities.Parsing.SMVFile
import FDSUtilities.Types
import qualified Data.ByteString as B
import Data.List
-- import VisCalcDS
-- import Data.Array.Repa.Algorithms.Pixel
-- import Data.Array.Repa.Algorithms.ColorRamp
-- import Data.Array.Repa.IO.BMP	
-- import Data.Array.Repa.IO.Timing

fineSimulation = FDSSimulation {chid = "IKEAMarsdenRF", simDir = "IKEAMarsdenRFFine"}
coarseSimulation = FDSSimulation {chid = "IKEAMarsdenRF", simDir = "IKEAMarsdenRFCoarse"}

fineSliceFiles =
    [ "IKEAMarsdenRFFine\\IKEAMarsdenRF_0001_09.sf"
    -- , "IKEAMarsdenRF_0003_11.sf"
    , "IKEAMarsdenRFFine\\IKEAMarsdenRF_0006_06.sf"
    , "IKEAMarsdenRFFine\\IKEAMarsdenRF_0013_03.sf"
    , "IKEAMarsdenRFFine\\IKEAMarsdenRF_0014_06.sf"
    , "IKEAMarsdenRFFine\\IKEAMarsdenRF_0015_03.sf"
    , "IKEAMarsdenRFFine\\IKEAMarsdenRF_0010_09.sf"
    , "IKEAMarsdenRFFine\\IKEAMarsdenRF_0012_03.sf"
    ]
coarseSliceFile = "IKEAMarsdenRFCoarse\\IKEAMarsdenRF_0001_15.sf"
fineCellSize = 
    [ 0.5
    , 0.5
    , 0.5
    , 0.125
    , 0.5
    , 0.5
    , 0.5
    ]
testSlice = "D:\\Data\\s131251_IKEA Local\\RunFDS\\IKEAMarsdenMF1\\IKEAMarsdenMF1_0001_09.sf"
-- The algorithm should be as follows:
 -- * A master mesh is created which is based purely on coordinates.
    -- This mesh should be created based on the largest bounding dimensions
    -- of each model.
 -- * The resolution of this master mesh is fixed and can be set. Initially this
    -- resolution should be obtained from the finest resolution of the fine mesh.
 -- * The data from the first mesh should be loaded into this mesh.
 -- * The data from the second mesh should be loaded into a similar mesh.
 -- * These meshes should be subtracted as appropriate.
 -- * It is still necessary that the requested data line up. Don't know what the margin of error should be.
 -- * Initially this should only use slice files.
   

type MeshMask = R.Array R.U R.DIM3 Bool
type MeshData = R.Array R.U R.DIM3 Float
type Location = R.DIM3 -> (Float, Float, Float)
data Mesh = Mesh MeshMask MeshData Location


-- |Create an empty mesh of a certain resolution.
createEmpty :: (Float, Float, Float) -> Bounds3 -> Mesh
createEmpty (dx, dy, dz) bounds = Mesh (R.fromListUnboxed shape (repeat False)) (R.fromListUnboxed  shape (repeat 0)) location
    where
        nx = ceiling $ (bounds3_maxX bounds - bounds3_minX bounds) / dx
        ny = ceiling $ (bounds3_maxY bounds - bounds3_minY bounds) / dy
        nz = ceiling $ (bounds3_maxZ bounds - bounds3_minZ bounds) / dz
        location (Z :. i :. j :. k) = ((fromIntegral i)*dx, (fromIntegral j)*dy, (fromIntegral k)*dz)
        shape = (Z :. (nx :: Int) :. (ny :: Int) :. (nz :: Int))

-- |Read a slice into a full 3d mesh of a certain resolution.
-- readSliceMesh simulation timeframe file = do
    -- (SliceDataSet longName shortName units ijkBounds values) <- readSlice timeframe file
    -- -- get mesh dimensions from SMV file
    -- Right smvData <- parseSimulationSMVFile simulation
    -- let meshes = smvMeshes smvData
        -- trns = map smvMeshTRNs meshes
    -- let bounds = Bounds3 x1 x2 y1 y1 z1 z2
    -- let empy = createEmpty (dx,dy,dz) bounds

-- The following are functions to apply to mesh trns
--
-- |Get the bounds of a single TRN.
getXYZBound :: [(Int, Double)] -> (Double, Double)
getXYZBound trn = (vMin, vMax)
    where
        (_, vMin) = head trn
        (_, vMax) = last trn

-- |Get the 3D bounds from a set of TRN values.
getXYZBounds :: MeshTRNs -> Bounds3
getXYZBounds meshTRNs@(MeshTRNs trnx trny trnz) = Bounds3 xMin xMax yMin yMax zMin zMax
    where
        (xMin, xMax) = getXYZBound trnx
        (yMin, yMax) = getXYZBound trny
        (zMin, zMax) = getXYZBound trnz

-- |Get the 3D bounds of a SMVMesh.
-- getMeshBounds :: SMVMesh -> Bounds3



readSlice :: Float -> FilePath -> SliceDataSet
readSlice timeframe file = do
    fileData <- B.readFile file
    return $ case parseSliceFileTime timeframe fileData of
        Left e -> error $ show e
        Right x -> x

-- - readData timeframe file = do
    -- fileData <- B.readFile file
    -- return $ case parseSliceFileRepaTime timeframe fileData of
                -- Left e -> error e

data Bounds3 = Bounds3
    { bounds3_minX :: Float
    , bounds3_maxX :: Float
    , bounds3_minY :: Float
    , bounds3_maxY :: Float
    , bounds3_minZ :: Float
    , bounds3_maxZ :: Float
    }

-- reduceBy4
    -- :: R.Array R.U R.DIM2 Float
    -- -> R.Array R.U R.DIM2 Float
-- reduceBy4 array =
    -- R.computeS $ R.traverse array (\(R.Z R.:. i R.:. j)->(R.Z R.:. (i `div` 4 +1) R.:. (j `div` 4 +1))) calculateValue
    -- where
        -- -- makeSquare f p@(R.Z R.:. i R.:. j) = translate (r2 location) $ rect yLength xLength # lw 0.0 # fc colour
        -- calculateValue f p@(R.Z R.:. i R.:. j) = f (R.Z R.:. (i*4) R.:. (j*4))
        
-- trim1
    -- :: R.Array R.U R.DIM2 Float
    -- -> R.Array R.U R.DIM2 Float
-- trim1 array =
    -- R.computeS $ R.traverse array (\(R.Z R.:. i R.:. j)->(R.Z R.:. (i - 1) R.:. j)) calculateValue
    -- where
        -- -- makeSquare f p@(R.Z R.:. i R.:. j) = translate (r2 location) $ rect yLength xLength # lw 0.0 # fc colour
        -- calculateValue f p@(R.Z R.:. i R.:. j) = f (R.Z R.:. i R.:. j)
        
-- takeSlice :: R.Array R.U R.DIM3 Float ->  R.Array R.U R.DIM2 Float
-- takeSlice x = R.computeS $ R.traverse x (\(R.Z R.:. i0 R.:. j0 R.:. k0) -> (R.Z R.:. i0 R.:. k0)) travFunc
    -- where
        -- travFunc = (\f (R.Z R.:. iN R.:. kN) -> f (R.Z R.:. iN R.:. 0 R.:. kN))
        
-- multiplyImage
    -- :: Int
    -- -> R.Array R.U R.DIM2 Float
    -- -> R.Array R.U R.DIM2 Float
-- multiplyImage n array = R.computeS $ R.traverse array (\(R.Z R.:. i0 R.:. j0) -> (R.Z R.:. (i0*n) R.:. (j0*n))) travFunc
    -- where
        -- travFunc = (\f (R.Z R.:. iN R.:. jN) -> f (R.Z R.:. (iN `div` n) R.:. (jN `div` n)))
        
-- widenImage
    -- :: Int
    -- -> R.Array R.U R.DIM2 Float
    -- -> R.Array R.U R.DIM2 Float
-- widenImage n array = R.computeS $ R.traverse array (\(R.Z R.:. i0 R.:. j0) -> (R.Z R.:. (i0) R.:. (j0*n))) travFunc
    -- where
        -- travFunc = (\f (R.Z R.:. iN R.:. jN) -> f (R.Z R.:. (iN) R.:. (jN `div` n)))
                
-- readData timeframe file = do
    -- fileData <- B.readFile file
    -- return $ case parseSliceFileRepaTime timeframe fileData of
                -- Left e -> error e
                -- Right x -> x
                
-- minMaxSix (a1, a2, b1, b2, c1, c2) = (aMin, aMax, bMin, bMax, cMin, cMax)
    -- where
        -- aMin = min a1 a2
        -- aMax = max a1 a2
        -- bMin = min b1 b2
        -- bMax = max b1 b2
        -- cMin = min c1 c2
        -- cMax = max c1 c2
        
-- -- |Find the id numbers of the meshes that intersect withour area of interest.
-- findRelevantMeshNums smvData xb = ns
    -- where
        -- meshes = smvMeshes smvData
        -- meshBounds = map getMeshBounds (map smvMeshTRNs meshes)
        -- overlap = map (xbOverlap xb) meshBounds
        -- ns = foldl' (\acc (n,bool) -> if bool then (n:acc) else acc) [] $ zip [1..] overlap
        
-- -- |Find the slice files that are in the relevant meshes and are of the right variables.
-- --  This cannot take into account the location of the slice within the mesh, as that data
-- --  must be parsed from the slice file directly.
-- relevantSliceEntries smvData meshNums sliceTypes = filter filterFunc dataFileEntries
    -- where
        -- dataFileEntries = smvDataFiles smvData
        -- filterFunc x = isSliceEntry x && (slcfShortName x) `elem` sliceTypes && (slcfMeshNum x) `elem` meshNums
        
-- relevantSliceEntriesWithXB simulation smvData relSliceEntries targetXB = do
    -- sliceHeaders <- getSliceHeadersSpecific simulation relSliceEntries
    -- let meshes = smvMeshes smvData
    -- let pairedSlices = zip relSliceEntries sliceHeaders
        -- addSliceXB x@(sliceEntry,sliceHeader) = (sliceEntry, sliceHeader, getSliceXB smvData x)
        -- pairedSlicesWithXB = map addSliceXB pairedSlices
        -- relSlices = filter (\(_,_,xb)->xbOverlap xb targetXB)pairedSlicesWithXB
    -- return relSlices
        

-- -- TODO: verify this function
-- xbOverlap xb1 xb2 
    -- =  xMax1 > xMin2
    -- && xMax2 > xMin1
    -- && yMax1 > yMin2
    -- && yMax2 > yMin1
    -- && zMax1 > zMin2
    -- && zMax2 > zMin1
    -- where
        -- (xMin1, xMax1, yMin1, yMax1, zMin1, zMax1) = minMaxSix xb1
        -- (xMin2, xMax2, yMin2, yMax2, zMin2, zMax2) = minMaxSix xb2
        
        
-- -- TODO: sliceHeaders must be in the same order as the data file entries
-- createSliceLocationTable smvData sliceHeaders = zip sliceDataFileEntries xbs
    -- where
        -- dataFileEntries = smvDataFiles smvData
        -- sliceDataFileEntries = filter isSliceEntry dataFileEntries
        -- xbs = map (getSliceXB smvData) $ zip sliceDataFileEntries sliceHeaders
        

-- getSliceXB :: SMVFile -> (DataFileEntry, SliceDataHeader) -> (Double, Double, Double, Double, Double, Double)
-- getSliceXB smvData (sliceFileEntry, sliceHeader@(SliceDataHeader _ _ _ ijkBounds))  = getSliceXB' mesh ijkBounds
    -- where
        -- meshes = smvMeshes smvData
        -- meshNum = slcfMeshNum sliceFileEntry
        -- mesh = meshes !! (meshNum - 1) -- TODO: check that meshes are in the correct order.

-- getSliceXB' :: SMVMesh -> (Int, Int, Int, Int, Int, Int) -> (Double, Double, Double, Double, Double, Double)
-- getSliceXB' mesh sliceIJKBounds = (xMin, xMax, yMin, yMax, zMin, zMax)
    -- where
        -- meshTRNs = smvMeshTRNs mesh
        -- (iMin, iMax, jMin, jMax, kMin, kMax) = minMaxSix sliceIJKBounds
        -- (xMin, yMin, zMin) = case getCornerXYZ meshTRNs (iMin, jMin, kMin) of
                                -- Left e -> error e
                                -- Right x -> x
        -- (xMax, yMax, zMax) = case getCornerXYZ meshTRNs (iMax, jMax, kMax) of
                                -- Left e -> error e
                                -- Right x -> x

-- getData (SliceDataSetRepa header [SnapshotRepa time array]) = array
-- getHeader (SliceDataSetRepa header [SnapshotRepa time array]) = header

-- -- TODO: find all relevant slices. for given dimensions.
    
-- main = do
    -- fineSliceDataRaw <- mapM (readData 360) fineSliceFiles
    -- coarseSliceDataRaw <- readData 360 coarseSliceFile
    -- let fineSliceData = map (takeSlice . getData) fineSliceDataRaw
        -- coarseSliceData = takeSlice $ getData coarseSliceDataRaw
        -- fineSliceHeaders = map getHeader fineSliceDataRaw
        -- coarseSliceHeader = getHeader coarseSliceDataRaw
        -- newFine = reduceBy4 (fineSliceData !! 3)
        -- newEnd = drop 4 fineSliceData
        -- newStart = take 3 fineSliceData
        -- newList' = newStart ++ [newFine] ++ newEnd
        -- newList = map trim1 $ newStart ++ [newFine] ++ newEnd
        -- (_:_:_:_:_:_:t7:[]) = map R.transpose newList'
        -- (t1:t2:t3:t4:t5:t6:_:[]) = map R.transpose newList
        -- fineArray = R.transpose $ R.append (R.append (R.append (R.append (R.append (R.append t1 t2) t3) t4) t5) t6) t7
        -- coarseArray = coarseSliceData
        -- diffArray = R.computeS $ R.zipWith (-) fineArray coarseArray :: R.Array R.U R.DIM2 Float
        -- errorArray = R.computeS $ R.zipWith (/) diffArray fineArray :: R.Array R.U R.DIM2 Float
        -- maxError = R.foldAllS max (-9999) errorArray
        -- minError = R.foldAllS min (9999) errorArray
        -- -- fineSliceDataSingle = (foldr R.append (head newList) (tail newList))
    -- -- mapM_ (print . R.extent) newList
    -- -- print $ R.extent fineArray
    -- -- print $ R.extent coarseArray
    -- -- mapM_ print fineSliceHeaders
    -- -- print $ sort $ R.toList diffArray
    -- let minScale = -0.1
        -- maxScale = 0.1
        -- width = 2
    -- let scaleArr :: R.Array R.U R.DIM2 Float
        -- scaleArr = R.fromListUnboxed (R.Z R.:. (101::Int) R.:. (width::Int)) $ concat $ map (replicate width) ([minScale,minScale+((maxScale-minScale)/100)..maxScale] :: [Float])
    -- let testArr :: R.Array R.U R.DIM2 Float
        -- testArr = R.fromListUnboxed (R.Z R.:. (101::Int) R.:. (width::Int)) $ concat $ map (replicate width) (replicate 101 (-0.4) :: [Float])
    -- arrScaleOut     <- R.computeP
                    -- $  R.map rgb8OfFloat
                    -- $  R.map (rampColorHotToCold minScale maxScale) $ widenImage 3 $ multiplyImage 5 scaleArr
    -- arrTestOut     <- R.computeP
                    -- $  R.map rgb8OfFloat
                    -- $  R.map (rampColorHotToCold minScale maxScale) $ widenImage 3 $ multiplyImage 5 testArr
    -- arrImageOut     <- R.computeP
                    -- $  R.map rgb8OfFloat
                    -- $  R.map (rampColorHotToCold minScale maxScale) $ multiplyImage 4 errorArray

    -- writeImageToBMP "out.bmp" arrImageOut
    -- writeImageToBMP "scale.bmp" arrScaleOut
    -- writeImageToBMP "test.bmp" arrTestOut
    -- -- print maxError
    -- -- print minError
    -- smvDataFine <- parseSimulationSMVFile fineSimulation
    -- let bounds = map getMeshBounds (map smvMeshTRNs (smvMeshes smvDataFine))
    -- -- print bounds
    -- let targetXB = (-114, 0, 12, 12, 0, 7)
    -- let relMeshNums = findRelevantMeshNums smvDataFine targetXB
        -- relSliceEntries = relevantSliceEntries smvDataFine relMeshNums ["VIS_Soot"]
    -- properRelSlices <- relevantSliceEntriesWithXB fineSimulation smvDataFine relSliceEntries targetXB
    -- -- print relMeshNums
    -- mapM_ print properRelSlices
    -- print $ length properRelSlices
    -- -- createSliceLocationTable smvDataFine sliceHeaders
    -- -- mapM_ print fineSliceHeaders
    -- -- print coarseSliceHeader
    -- print (seq fineSliceData "done")
-}
