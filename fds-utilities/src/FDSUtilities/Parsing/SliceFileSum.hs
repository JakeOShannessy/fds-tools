module FDSUtilities.Parsing.SliceFileSum
    -- ( parseSliceFile
    -- ,
    -- )
    where

import FDSUtilities.Types
import FDSUtilities.Parsing.SliceFile

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
-- import Data.Either
import Control.Monad
import Data.Binary.IEEE754

-- TODO: Exclude cells that are covered by obstructions. This requires information from the smv file.
parseDatumSum :: Parser Float
parseDatumSum = do
    number <- anyWord32le
    let res = wordToFloat number
    -- return $ double2Float res
    return res
    -- return $ if res < 0.01 then 0 else (double2Float maxVis) - res
    -- TODO: take initial slice values, then see how much is lost at each
    -- timeframe after that.
    -- return res


parseDataSum :: Int -> Int -> Int -> Parser [[[Float]]]
parseDataSum i j k = do    -- TODO: the headers and tails are the length of the
-- record. Take advantage of this.
    recLength <- parseRecordLength
    theData <- replicateM k $ replicateM j $ replicateM i parseDatumSum
    parseSetRecordLength recLength
    return theData

parseDataSetSum i j k = do
    time <- parseTime
    parsedData <- parseDataSum i j k
    let res = (time, sum $ map sum (map (map sum) parsedData))
    -- let res = ((wordToFloat time), maximum $ map maximum (map (map maximum) parsedData))
    return $ deepseq res res

parseDataSetArea threshold i j k = do
    time <- parseTime
    parsedData <- parseData i j k
    let res = (time, filter threshold $ R.toList parsedData)
    return $ deepseq res res

parseDataSetsSum i j k = do
    parsedData <- many' $ try $ parseDataSetSum i j k
    endOfInput
    return parsedData

parseDataSetsArea threshold i j k = do
    parsedData <- many' $ try $ parseDataSetArea threshold i j k
    endOfInput
    return parsedData

parseSliceFileParserSum :: Parser (SliceDataHeader, [(Float, Float)])
parseSliceFileParserSum = do
    header@(SliceDataHeader quantity shortName units dimensions) <- parseHeader
    let (i1,i2,j1,j2,k1,k2) = dimensions
        i = i2 - i1 + 1
        j = j2 - j1 + 1
        k = k2 - k1 + 1
    parsedData <- parseDataSetsSum i j k
    return $ (header, parsedData)

parseSliceFileParserArea :: (Float -> Bool)
    -> Parser (SliceDataHeader, [(Float, [Float])])
parseSliceFileParserArea threshold = do
    header@(SliceDataHeader quantity shortName units dimensions) <- parseHeader
    let (i1, i2, j1, j2, k1, k2) = dimensions
        i = i2 - i1 + 1
        j = j2 - j1 + 1
        k = k2 - k1 + 1
    parsedData <- parseDataSetsArea threshold i j k
    return (header, parsedData)
