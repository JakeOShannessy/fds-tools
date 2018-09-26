module FDSUtilities.Verification where

import FDSUtilities.Simulation
import FDSUtilities.Parsing
import FDSUtilities.FDSFileFunctions
import FDSUtilities.Verification.Tests
import FDSUtilities.Verification.Display
import FDSUtilities.Types.Assess
import FDSUtilities.Types
import Data.List
import System.Directory
import System.FilePath
import System.FilePath.Glob

import Control.Exception
import Control.Monad

import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

import Data.Tree

verifySimulationInputData :: FDSSimulation -> IO (Either String Assessment)
verifySimulationInputData fdsSim = do
    let inputPath = fdsFilePath fdsSim
    verifyInputFile inputPath

verifyInputFile :: FilePath -> IO (Either String Assessment)
verifyInputFile inputPath = do
    fdsDataRaw <- parseFDSFile inputPath
    case fdsDataRaw of
        (Right fdsData) -> do
            let (NamelistFile comments nmls) = fdsData
            r <- try $ do
                let x = verifyInputData fdsData
                seq x (putStr "")
                -- print x
                return x
            return $ case r of
                Right x -> Right x
                Left e -> Left $ show (e :: SomeException)
        (Left err)      -> return $ Left $ "Simulation Verification Assessment could not be completed due to a parsing error:\n" ++ show err

verifyInputData = verificationAssessment
