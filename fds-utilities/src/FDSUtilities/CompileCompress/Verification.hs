{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.CompileCompress.Verification where

import Data.Tree hiding (draw)
import Data.List
import Data.Monoid

import FDSUtilities.Types
import FDSUtilities.Types.Assess
import FDSUtilities.Verification.Display
import FDSUtilities.FDSFileFunctions
import FDSUtilities.Verification
import FDSUtilities.CompileCompress.Render
import FDSUtilities.Parsing

import System.Directory
import System.IO.Error
import System.FilePath

import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty


-- |Produce an input verification assessment (HTML).
produceVerificationAssessmentCompile :: CompilationActionFunction
produceVerificationAssessmentCompile destDir fdsSim = do
    createDirectoryIfMissing True destDir
    assessmentHtml <- produceVerificationAssessment fdsSim
    let page = toPage heading style script assessmentHtml
        path = joinPath [destDir, "Verification Assessment.html"]
    writeFile path $ renderHtml page
    return [path]
    where
      style = css
      script = jscript
      heading = simCHID fdsSim ++ " Input Verification"


-- |Produce an input verification assessment (HTML).
produceVerificationAssessment :: FDSSimulation -> IO H.Html
produceVerificationAssessment fdsSim = do
    assessment <- verifySimulationInputData fdsSim
    case assessment of
        Left err -> return $ H.pre $ H.string err
        Right ass -> return $ H.toMarkup ass


