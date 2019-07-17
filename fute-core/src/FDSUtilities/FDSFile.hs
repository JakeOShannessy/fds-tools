{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.FDSFile
    ( module FDSUtilities.FDSFile.Types
    , module FDSUtilities.FDSFile.Utilities
    , module FDSUtilities.FDSFile.Decode
    , module FDSUtilities.FDSFile.NamelistFunctions
    , getOutData
    ) where

import FDSUtilities.FDSFile.Types
import FDSUtilities.FDSFile.Utilities
import FDSUtilities.FDSFile.Decode
import FDSUtilities.FDSFile.NamelistFunctions

import Control.Exception as E
import Control.Monad
import Data.Time

import FDSUtilities.Parsing
-- import FDSUtilities.Plot
-- import FDSUtilities.RunTimeCalc
import FDSUtilities.Simulation
-- import FDSUtilities.Parsing.SimulationData
import FDSUtilities.Types
-- import FDSUtilities.Types.Monitor
-- import FDSUtilities.Monitor

import System.Directory

import System.FilePath


getOutData :: FDSSimulation -> IO OutData
getOutData sim = do
    let handler :: SomeException -> IO (Either ParseError OutData)
        handler e = do
            print e
            getLine
            return $ Right $ OutData (Version 0 0 0) [] (MiscParameters 0 0) [] Nothing NotStarted
    outFileExists <- doesFileExist $ outFilePath sim
    outData' <- if outFileExists
                   then {-# SCC parseOutData #-} E.catch (parseSimulationOutFile sim) handler
                   else error $ "Outfile does not exist. > " ++ (outFilePath sim)
    let outData = case outData' of
            Left e -> error $ show e
            Right x -> x
    return outData
