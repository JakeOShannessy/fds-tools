{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
module FDSUtilities.Parsing.PLOT3D
    ( parsePLOT3DFile
    , parsePLOT3DFileAverage
    , parsePLOT3DAverage
    , parsePLOT3DFileSum
    , getQuantity
    , isPL3D
    , PL3DDataSet(..)
    , PL3DData(..)
    , PL3DSeries(..)
    , PL3DTimeFrame(..)
    , pl3dAverageAll
    -- , pl3dSum
    , pl3dSumAll
    , getPL3DData
    , pl3dAverageMesh
    ) where

import FDSUtilities.Parsing.SMVFile
import FDSUtilities.Types

import Control.DeepSeq

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.Binary
import Data.Char (isSpace)
import Data.List as L
import Debug.Trace
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB

import GHC.Float

import qualified Data.Array.Repa as R

import Data.Word
import Control.Monad
import Data.Binary.IEEE754

import qualified Data.Array.Repa as R

n = 5
intSize = 4 -- integer size is 32 bits or 4 bytes
floatSize = 4 -- float size is 32 bits or 4 bytes
-- TODO: implement fseek to skip unnecessary data.
parseInteger = fromIntegral <$> anyWord32le
parsePLOT3D :: Parser (VB.Vector (R.Array R.U R.DIM3 Float))
parsePLOT3D = do
    (i,j,k) <- parseRecordOfLength (intSize*3)
        ((,,) <$> parseInteger <*> parseInteger <*> parseInteger)
    parseRecordOfLength (intSize*4) (A.take (intSize*4))
    values <- parseRecordOfLength (floatSize*i*j*k*n)
        (VB.replicateM n ((parseBlock i j k (wordToFloat <$> anyWord32le))))
    endOfInput
    pure values

parsePLOT3DAverage :: Parser (VU.Vector Float)
parsePLOT3DAverage = do
    (i,j,k) <- parseRecordOfLength (intSize*3)
        ((,,) <$> parseInteger <*> parseInteger <*> parseInteger)
    parseRecordOfLength (intSize*4) (A.take (intSize*4))
    values <- parseRecordOfLength (floatSize*i*j*k*n)
        (VU.replicateM n (parseBlockAverage i j k (wordToFloat <$> anyWord32le)))
    endOfInput
    pure values

parsePLOT3DSum :: Parser (((Int,Int,Int), VU.Vector Float))
parsePLOT3DSum = do
    (i,j,k) <- parseRecordOfLength (intSize*3)
        ((,,) <$> parseInteger <*> parseInteger <*> parseInteger)
    parseRecordOfLength (intSize*4) (A.take (intSize*4))
    values <- parseRecordOfLength (floatSize*i*j*k*n)
        (VU.replicateM n (parseBlockSum i j k (wordToFloat <$> anyWord32le)))
    endOfInput
    pure ((i,j,k),values)

parseBlock :: Int -> Int -> Int -> Parser Float
    -> Parser (R.Array R.U R.DIM3 Float)
parseBlock i j k parser =
    R.fromUnboxed (R.Z R.:. i R.:. j R.:. k) <$> VU.replicateM (i*j*k) parser

parseBlockAverage :: Int -> Int -> Int -> Parser Float -> Parser Float
parseBlockAverage i j k parser =
    (/ (fromIntegral (i*j*k))) . sum <$> A.count (i*j*k) parser

parseBlockSum :: Int -> Int -> Int -> Parser Float -> Parser Float
parseBlockSum i j k parser =
    sum <$> A.count (i*j*k) parser


parseRecordLength :: Parser Int
parseRecordLength = fromIntegral <$> anyWord32le

-- |Parse a record length which must match the specified value.
parseSetRecordLength :: Int -> Parser Int
parseSetRecordLength l = fromIntegral <$> word32le (fromIntegral l)

parseRecordOfLength :: Int -> Parser a -> Parser a
parseRecordOfLength l parser = do
    parseSetRecordLength l
    res <- parser
    parseSetRecordLength l
    pure res

-- |Parse the data from a record, ensuring the record length tags at the start
-- and finish match.
parseRecord :: Parser a -> Parser a
parseRecord parser = do
    recLength <- parseRecordLength
    res <- parser
    parseSetRecordLength recLength
    pure res

parseRecordByteString :: Parser B.ByteString
parseRecordByteString = do
    recLength <- parseRecordLength
    bstring <- A.take recLength
    parseSetRecordLength recLength
    pure bstring

-- |Parse a PLOT3D file.
parsePLOT3DFile :: FilePath -> IO (Either String (VB.Vector (R.Array R.U R.DIM3 Float)))
parsePLOT3DFile filepath = do
    -- TODO: this is currently not interative and reads the whole thing, it is
    -- critical that this be iterative for large files.
    theFile <- B.readFile filepath
    pure $ eitherResult $ parse parsePLOT3D theFile `feed` B.empty

-- |Parse a PLOT3D file average.
parsePLOT3DFileAverage :: FilePath -> IO (Either String (VU.Vector Float))
parsePLOT3DFileAverage filepath = do
    -- TODO: this is currently not interative and reads the whole thing, it is
    -- critical that this be iterative for large files.
    theFile <- B.readFile filepath
    pure $ eitherResult $ parse parsePLOT3DAverage theFile `feed` B.empty

-- |Parse a PLOT3D file sum.
parsePLOT3DFileSum :: FilePath -> IO (Either String (((Int,Int,Int), VU.Vector Float)))
parsePLOT3DFileSum filepath = do
    -- TODO: this is currently not interative and reads the whole thing, it is
    -- critical that this be iterative for large files.
    theFile <- B.readFile filepath
    pure $ eitherResult $ parse parsePLOT3DSum theFile `feed` B.empty

-- |Parse the PLOT3D data from a ByteString.
parsePLOT3DData :: B.ByteString -> Either String (VB.Vector (R.Array R.U R.DIM3 Float))
parsePLOT3DData theFile = eitherResult $ parse parsePLOT3D theFile
    `feed` B.empty

getQuantity :: PL3DDataSet -> String -> PL3DData
getQuantity (PL3DDataSet pl3dData) longname = case VB.find (\x->pl3dEntryLongName (pl3dData_entry x) == longname) pl3dData of
    Just x -> x
    Nothing -> error ("Quantity " ++ show longname
        ++ " could not be found. Available quantities are:\n"
        ++ (unlines $ VB.toList (VB.map (pl3dEntryLongName . pl3dData_entry) pl3dData)))

isPL3D :: DataFileEntry -> Bool
isPL3D PL3DDataFile{} = True
isPL3D _ = False

--(time,value)
pl3dSumAll :: PL3DData -> VU.Vector (Float, Float)
pl3dSumAll (PL3DData entry series) =
    let
        -- the outer vector is a vector of meshes
        summedSeries :: VB.Vector (VB.Vector (Float, Float))
        summedSeries = VB.map (\(PL3DSeries entry values)->mkSum values) series
        resx = VB.foldl1' (VB.zipWith (\(t1,v1) (t2,v2)->(t1,v1+v2))) summedSeries
        -- (ts,vs) = VB.unzip res
    in VU.convert resx
    where
        mkSum values = VB.map (\(PLOT3DTimeFrame t vs)->(t, R.sumAllS vs)) values

-- pl3dAverageRegion :: PL3DData -> VU.Vector (Float, Float)
-- pl3dAverageRegion pl3dData@(PL3DData entry series) =
--     let
--         -- the outer vector is a vector of meshes
--         summedSeries :: VB.Vector (VB.Vector (Float, Int, Float))
--         summedSeries = VB.map (\(PL3DSeries entry values)->mkSum values) series
--         resx = VB.foldl1' (VB.zipWith (\(t1,c1,v1) (t2,c2,v2)->(t1,c1+c2,v1+v2))) summedSeries
--         -- (ts,vs) = VB.unzip res
--         vec = VU.convert $ VB.map (\(t,c,v)->(t,v/(fromIntegral c))) resx
--     in vec
--     where
--         -- this function can see the values and should accept or reject them
--         -- based on whether they are within the region. A filter need to be
--         -- applied before sumAllS
--         mkSum values = VB.map (\(PLOT3DTimeFrame t vs)->(t, (fromIntegral . R.size . R.extent) vs, R.sumAllS vs)) values

pl3dAverageAll :: PL3DData -> VU.Vector (Float, Float)
pl3dAverageAll pl3dData@(PL3DData entry series) =
    let
        -- the outer vector is a vector of meshes
        summedSeries :: VB.Vector (VB.Vector (Float, Int, Float))
        summedSeries = VB.map (\(PL3DSeries entry values)->mkSum values) series
        resx = VB.foldl1' (VB.zipWith (\(t1,c1,v1) (t2,c2,v2)->(t1,c1+c2,v1+v2))) summedSeries
        -- (ts,vs) = VB.unzip res
        vec = VU.convert $ VB.map (\(t,c,v)->(t,v/(fromIntegral c))) resx
    in vec
    where
        mkSum values = VB.map (\(PLOT3DTimeFrame t vs)->(t, (fromIntegral . R.size . R.extent) vs, R.sumAllS vs)) values

pl3dAverageMesh :: Int -> PL3DData -> VU.Vector (Float, Float)
pl3dAverageMesh i pl3dData@(PL3DData entry series) =
    let
        -- the outer vector is a vector of meshes
        summedSeries :: VB.Vector (VB.Vector (Float, Int, Float))
        summedSeries = VB.map (\(PL3DSeries entry values)->mkSum values) (VB.filter (\s->pl3dSeries_meshNum s == i) series)
        resx = VB.foldl1' (VB.zipWith (\(t1,c1,v1) (t2,c2,v2)->(t1,c1+c2,v1+v2))) summedSeries
        -- (ts,vs) = VB.unzip res
        vec = VU.convert $ VB.map (\(t,c,v)->(t,v/(fromIntegral c))) resx
    in vec
    where
        mkSum values = VB.map (\(PLOT3DTimeFrame t vs)->(t, (fromIntegral . R.size . R.extent) vs, R.sumAllS vs)) values

-- a vetor of quantities (multiple meshes, multiple times)
data PL3DDataSet = PL3DDataSet
    { pl3dDataSet :: VB.Vector PL3DData
    } deriving (Show)

-- a vector of meshes (single quantity, multiple times)
data PL3DData = PL3DData
    { pl3dData_entry :: PL3DEntry -- the quantity
    -- these two vectors must be of the same length
    , pl3dData_data :: VB.Vector PL3DSeries
    } deriving (Show)

-- as the meshes are not rectangles when combined they cannot be single repa
-- arrays
-- it would be more consistent to separate the meshes too, as with slices
--TODO: consider making this strict, why can't R.Array be unboxed?
-- a vector of times (single mesh, single quantity)
data PL3DSeries = PL3DSeries
    -- this is a vector over time over a certain quantity
    { pl3dSeries_meshNum :: Int -- the mesh
    , pl3dSeries_data :: VB.Vector PL3DTimeFrame
    } deriving (Show)

data PL3DTimeFrame = PLOT3DTimeFrame
    { pl3dTimeFrame_time :: Float -- the time
    , pl3dTimeFrame_data :: R.Array R.U R.DIM3 Float
    } deriving (Show)

-- |Transpose vectors.
vecTranspose :: VB.Vector (VB.Vector a) -> VB.Vector (VB.Vector a)
vecTranspose vecs = (\x->VB.fromList (map VB.fromList x))
    $ L.transpose $ VB.toList (VB.map VB.toList vecs)

-- it makes more sense for the outermost vector to be quantity, you are more
-- often going to want to combine meshes than quantities, and when you combine
-- quantities you will often want to combine meshes too
-- the vector is for the diffferent meshes
getPL3DData :: SMVFile -> IO PL3DDataSet
getPL3DData smv = do
    -- get all the PL3D files, making sure they are in order
    -- TODO: the pl3d files are not guaranteed to have the same time value
    -- for each mesh.
    -- in order for groupBy to work they must be appropriately ordered, so we
    -- need to order them by mesh firs
    let pl3dFiles = sortBy (\a b->compare (pl3dMeshNum a) (pl3dMeshNum b)) $ filter isPL3D $ smvDataFiles smv
    -- combine by mesh
        cfiles :: VB.Vector [DataFileEntry]
        cfiles = VB.fromList $ combineByMesh pl3dFiles
    -- TODO: use the masking (.xyz) data
    -- a vector of vectors
    -- outermost dimension is mesh, each of the inner dimensions is quantity (time is inside the series)
    -- maps over meshes
    vals <- (VB.mapM getPL3DTimeSeries cfiles) :: IO (VB.Vector (VB.Vector (PL3DEntry, PL3DSeries)))

    -- outermost dimension is quantity, innermost is mesh
    let vecs = vecTranspose vals :: VB.Vector (VB.Vector (PL3DEntry, PL3DSeries))
    --
    (pure $ PL3DDataSet $ VB.map func vecs)
    where
        func :: VB.Vector (PL3DEntry, PL3DSeries) -> PL3DData
        func theVec =
            let (entries, series) = VB.unzip theVec
            -- TODO: check that all entries are the same
            in PL3DData (VB.head entries) series

-- specVecTranspose :: VB.Vector (VB.Vector PL3DEntry, VB.Vector PL3DSeries) -> VB.Vector (VB.Vector PL3DData)

-- vector of quanties over meshes
-- k :: VB.Vector (VB.Vector PL3DSeries)
-- this is for a single mesh the vector is quantity (the time is inside the pl3dseries)
getPL3DTimeSeries :: [DataFileEntry] -> IO (VB.Vector (PL3DEntry, PL3DSeries))
getPL3DTimeSeries pl3dFiles = do
    let files = VB.fromList $
            sortBy (\a b-> compare (pl3dTime a) (pl3dTime b))  pl3dFiles
    -- outer dimension is time, inner dimension is quantity
    -- maps over times
    vals <- VB.mapM getPL3DFrameTime files :: IO (VB.Vector (VB.Vector PL3DTimeFrame))
    let
        -- TODO: check that all the entries are the same
        entries = VB.fromList $ head $ map pl3dEntries pl3dFiles
        -- TODO: check meshnums are the same
        meshNum = pl3dMeshNum $ head pl3dFiles
        -- outer dimensions is quantity, inner dimension is time
        vecs :: VB.Vector (VB.Vector PL3DTimeFrame)
        vecs = vecTranspose vals
        -- the dimension is quantity
        series :: VB.Vector PL3DSeries
        series = VB.map (PL3DSeries meshNum) vecs
    pure $ VB.zip entries series

-- returns a vector of the  different quantities at a single time
getPL3DFrameTime :: DataFileEntry -> IO (VB.Vector PL3DTimeFrame)
getPL3DFrameTime dfentry = do
    let time = pl3dTime dfentry
        filename = pl3dFilename dfentry
    -- this bang pattern forces us to know if the parse was successful
    Right !vals <- parsePLOT3DFile filename
    print filename
    VB.mapM_ (\(arr)->((R.deepSeqArray arr) `seq` (print $ R.extent arr))) vals
    (pure (VB.map (PLOT3DTimeFrame (realToFrac time)) vals))

-- same (l:ls) = all (== l) ls

combineTimeFrames :: [DataFileEntry] -> [[DataFileEntry]]
combineTimeFrames = L.groupBy (\a b->pl3dTime a == pl3dTime b)

combineByMesh :: [DataFileEntry] -> [[DataFileEntry]]
combineByMesh = L.groupBy (\a b->pl3dMeshNum a == pl3dMeshNum b)
