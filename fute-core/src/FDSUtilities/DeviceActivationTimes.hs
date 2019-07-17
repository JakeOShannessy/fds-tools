module FDSUtilities.DeviceActivationTimes where
    -- ( getDataList
    -- , DataVector(DataVector, dataVectorName, dataVectorUnits, dataVectorValues)
    -- , DataVectorPair(DataVectorPair, yVector, xVector)
    -- , FlexibleDataVector(..)
    -- , findDVectorPairByYName
    -- , findDVectorPairByYNameMaybe
    -- , getDataListAll
    -- , getOutLocProp
    -- , getOutProp
    -- ) where

-- import FDSUtilities.LockCheat
-- import Control.Concurrent
-- import Control.Concurrent.Thread.Delay
-- import Control.Exception
-- import Control.Lens
-- import Control.Monad

-- import Data.Char
-- import Data.Colour
-- import Data.Colour.Names
-- import Data.Colour.SRGB
-- import Data.Default
-- import Data.List
-- import Data.Maybe
-- import Data.Time

import FDSUtilities.Parsing.SimulationData
import FDSUtilities.Types
import FDSUtilities.Parsing.OutFile

-- import Graphics.Rendering.Chart
-- import Graphics.Rendering.Chart.Axis.LocalTime
-- import Graphics.Rendering.Chart.Backend.Cairo

-- import System.Console.GetOpt
-- import System.Directory
-- import System.Environment
-- import System.Exit
-- import System.FilePath
-- import System.IO
-- import System.Locale
-- import System.Process

-- import Text.CSV
-- import Text.ParserCombinators.Parsec
-- import Text.Printf

-- TODO: write test function to compare detector times found using this method with the detector times found in the .out file.

-- The integration is performed by FDS to obtain the %obs/m, this algorithm need only interpolate this.

-- PSEUDO CODE
-- get all devc vectors
-- optionally obtain ACTIVATION_OBSUCRATION from FDS File (TODO)
-- filter devc vectors to %obs/m devices
-- linearly interpolate based on obscuration threshold

-- getSD
getSDVectors sim = do
    vecs' <- getDataListAll sim
    let vecs = filter (\(DataVectorPair _ (DataVector _ units _))-> units == "%/m") vecs'
    return vecs

getDetectionTime :: Double -> DataVectorPair Double Double -> DevcActivation
getDetectionTime actObs (DataVectorPair timeVector obsVector) = case interpolateVectors actObs timeVector obsVector of
    Just x -> DevcActivationTime x
    Nothing -> NoActivation

getDetectionTimes :: Double -> [DataVectorPair Double Double] -> [DevcActivation]
getDetectionTimes actObs vecs = map (getDetectionTime actObs) vecs

getDetectionTimesSimulation sim actObs = do
    vecs <- getSDVectors sim
    let names = map (\(DataVectorPair _ (DataVector name _ _))->name) vecs
        times = getDetectionTimes actObs vecs
    return $ zip names times
