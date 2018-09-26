{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
module FDSUtilities.Parsing.SliceFile

    where

import FDSUtilities.Types

import Control.DeepSeq

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.Binary
import Data.Char (isSpace)
import Data.List as L
import Debug.Trace

import GHC.Float

import qualified Data.Array.Repa as R

import Data.Word
import Control.Monad
import Data.Binary.IEEE754

-- TODO: implement fseek to skip unnecessary data.

timelength = 12
headerlength = 4

-- |Parse a slice file.
parseSliceFile :: FilePath -> IO (Either String SliceDataSet)
parseSliceFile filepath = do
    -- TODO: this is currently not interative and reads the whole thing, it is
    -- critical that this be iterative for large files.
    theFile <- B.readFile filepath
    pure $ eitherResult $ parse parseSliceFileParser theFile `feed` B.empty

-- |Parse the slice data from a ByteString.
parseSliceData :: B.ByteString -> Either String SliceDataSet
parseSliceData theFile = eitherResult $ parse parseSliceFileParser theFile
    `feed` B.empty

-- |Parse only the header.
parseSliceHeader :: B.ByteString -> Either String SliceDataHeader
parseSliceHeader theFile = deepseq res res
    where
        res = eitherResult $ parse parseHeader theFile `feed` B.empty

parseSliceFileParser :: Parser SliceDataSet
parseSliceFileParser = do
    header@(SliceDataHeader quantity shortName units dimensions) <- parseHeader
    let (i1,i2,j1,j2,k1,k2) = dimensions
        i = i2 - i1 + 1
        j = j2 - j1 + 1
        k = k2 - k1 + 1
    parsedData <- parseDataSets i j k
    pure $ SliceDataSet header parsedData

parseHeader :: Parser SliceDataHeader
parseHeader = do
    quantity <- parseRecord
    shortName <- parseRecord
    units <- parseRecord
    dimensions <- parseDimensions
    pure $ SliceDataHeader
        (C8.unpack quantity)
        (C8.unpack shortName)
        (C8.unpack units)
        dimensions

parseDataSets :: Int -> Int -> Int -> Parser [Snapshot]
parseDataSets i j k = do
    parsedData <- many' $ try $ parseDataSet i j k
    endOfInput
    pure parsedData

parseDataSet :: Int -> Int -> Int -> Parser Snapshot
parseDataSet i j k = do
    time <- parseTime
    -- Bang pattern to ensure each repa array is fully evalated, this
    -- dramatically reduces memory usage.
    !parsedData <- parseData i j k
    pure $ Snapshot time parsedData

parseDataRun :: Int -> Parser [[Float]]
parseDataRun n = do
    (liftM (replicate n)) ((liftM (replicate n)) parseDatum)

parseRecordLength :: Parser Int
parseRecordLength = fromIntegral <$> anyWord32le

-- |Parse a record length which must match the specified value.
parseSetRecordLength :: Int -> Parser Int
parseSetRecordLength l = fromIntegral <$> word32le (fromIntegral l)

-- |Parse the data from a record, ensuring the record length tags at the start
-- and finish match.
parseRecord :: Parser B.ByteString
parseRecord = do
    recLength <- parseRecordLength
    bstring <- A.take recLength
    parseSetRecordLength recLength
    pure bstring

-- TODO: make sure the endianness is correct
parseDimensions :: Parser (Int, Int, Int, Int, Int, Int)
parseDimensions = do
    parseSetRecordLength recLength
    i1 <- fromIntegral <$> anyWord32le
    i2 <- fromIntegral <$> anyWord32le
    j1 <- fromIntegral <$> anyWord32le
    j2 <- fromIntegral <$> anyWord32le
    k1 <- fromIntegral <$> anyWord32le
    k2 <- fromIntegral <$> anyWord32le
    parseSetRecordLength recLength
    pure (i1, i2, j1, j2, k1, k2)
    where
        recLength = 24

parseTime :: Parser Float
parseTime =
    parseSetRecordLength recLength *>
        (wordToFloat <$> anyWord32le)
            <* parseSetRecordLength recLength
    where
        recLength = 4

parseDatum :: Parser Float
parseDatum = wordToFloat <$> anyWord32le

-- TODO: take a timeframe argument to extract only one slice
-- there should be an available function to skip to the correct location
-- directly in a file
parseData :: Int -> Int -> Int -> Parser (R.Array R.U R.DIM3 Float)
parseData i j k = do
-- TODO: the headers and tails are the length of the record. Take advantage of this.
-- TODO: consider using 4 dimensional repa arrays (with an associated time vector)
--       to account for time.
    recLength <- parseRecordLength
    theData <- replicateM k $ replicateM j $ replicateM i parseDatum
    -- TODO: this function currently parses into a list before converting to repa
    --       this is horribly inefficient, fix.
    let repaData :: R.Array R.U R.DIM3 Float
        repaData = R.fromListUnboxed (R.Z R.:. i R.:. j R.:. k) $ concat
            $ transpose $ concat theData
        -- the transpose is to ensure that the data is correctly oriented for
        -- the way repa reads lists
    parseSetRecordLength recLength
    -- ensure this is fully evaluated, it should be as the array is unboced
    pure repaData

-- TODO: find a better way to do this
parseDataSetsTime :: Float -> Int -> Int -> Int -> Parser [Snapshot]
parseDataSetsTime targettime i j k = do
    parsedData@(Snapshot time _) <- parseDataSet i j k
    if time >= targettime
        then pure [parsedData]
        else parseDataSetsTime targettime i j k
    pure [parsedData]

parseSliceFileFrame :: Int -> B.ByteString -> Either String SliceDataSet
parseSliceFileFrame frame theFile = eitherResult $ parse
    (parseSliceFileParserFrame frame) theFile `feed` B.empty
-- parseSliceFileTime timeframe theFile = eitherResult $ parse
--     (parseSliceFileParserTime timeframe) theFile `feed` B.empty

parseSliceFileParserFrame :: Int -> Parser SliceDataSet
parseSliceFileParserFrame frame = do
    header@(SliceDataHeader quantity shortName units dimensions) <- parseHeader
    let (i1,i2,j1,j2,k1,k2) = dimensions
        i = i2 - i1 + 1
        j = j2 - j1 + 1
        k = k2 - k1 + 1
    parsedData <- parseDataSetsFrame frame i j k
    pure $ SliceDataSet header parsedData

parseDataSetsFrame frame i j k = do
    let toTake = (timelength+headerlength+i*j*k*4+headerlength)*frame
    A.take toTake
    parsedData <- parseDataSet i j k
    -- endOfInput
    pure [parsedData]

-- parseSliceFileParserTime timeframe = do
--     header@(SliceDataHeader quantity shortName units dimensions) <- parseHeader
--     let (i1,i2,j1,j2,k1,k2) = dimensions
--         i = i2 - i1 + 1
--         j = j2 - j1 + 1
--         k = k2 - k1 + 1
--     parsedData <- parseDataSetsTime timeframe i j k
--     pure $ SliceDataSet header parsedData
