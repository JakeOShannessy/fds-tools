{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.CompileCompress where

import Control.Exception as E
import Control.Lens
import Control.Monad

import Data.Default
import Data.List
import Data.Monoid
import Data.Time
import qualified Data.Text as T
import Data.Tree hiding (drawTree)
import qualified Data.Map as M

import FDSUtilities.CompileCompress.Screenshots
import FDSUtilities.CompileCompress.Compression
import FDSUtilities.CompileCompress.Vitals
import FDSUtilities.CompileCompress.Charts
import FDSUtilities.CompileCompress.Verification
import FDSUtilities.CompileCompress.Render
import FDSUtilities.Verification.Display
import FDSUtilities.Verification
import FDSUtilities.Parsing
import FDSUtilities.RunTimeCalc
import FDSUtilities.Types
import FDSUtilities.Types.Assess
import FDSUtilities.Types.Monitor
import FDSUtilities.Summary

import FDSUtilities.DeviceActivationTimes
import FDSUtilities.Parsing.SimulationData

import System.Directory
import System.IO.Error
import System.FilePath

import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

produceDeviceTimes :: [(String, Double)] -> CompilationActionFunction
produceDeviceTimes devSpecs path simulation = do
    vals <- getDevcTimes simulation devSpecs
    let outPath = (joinPath [path, "deviceTimes.txt"])
    writeFile outPath (show vals)
    return [outPath]

getDevcTimes simulation devSpecs = do
    allData <- getDataListAll simulation
    let vals = map (findDevcTime allData) devSpecs
    return vals

findDevcTime allData (name, threshold) =
    let vec = findDVectorPairByYName allData name
    in (name, getDetectionTime threshold vec)

-- |Runs a compilation, i.e. performs each listed CompilationAction and copies
-- the results to the destination directories.
performCompilation :: FilePath -> [FilePath] -> Compilation -> IO ()
performCompilation localPath destDirs compilation = do
    let simulation = compilation ^. compilationSimulation
    paths <- (liftM concat) $ mapM (performCompilationAction localPath simulation) (compilation ^. compilationActions)
    mapM_ (makeDuplicates localPath destDirs) (nub paths)
    where
        makeDuplicates localPath destDirs path = mapM_ (copyFileWithDirs $ joinPath [localPath, path]) destinationPaths
            where
                destinationPaths = map (\x-> joinPath [x, path]) destDirs

-- mapMSeq [] = []
-- mapMSeq f (x:xs) = f

-- |Runs and individual CompilationAction and returns the resultant paths.
performCompilationAction :: FilePath -> FDSSimulation -> CompilationAction -> IO [FilePath]
performCompilationAction localPath simulation compilationAction = do
    createDirectoryIfMissing True destDir
    rawPaths <- f destDir simulation
    rawPathsAbs <- mapM canonicalizePath rawPaths
    localPathAbs <- canonicalizePath localPath  -- Make absolute so that paths can be made relative properly.
    let relPaths = map (makeRelative localPathAbs) rawPathsAbs
    return relPaths
    where
        f = compilationAction ^. compilationActionFunction
        destDir = joinPath [localPath, simCHID simulation, compilationAction ^. compilationActionPath]

-- |Copy a file, creating the directory structure if necessary.
copyFileWithDirs oldPath newPath = do
    putStr "Duplicating "
    putStr oldPath
    putStrLn "..."
    putStrLn $ "To > " ++ newPath
    fileExists <- doesFileExist oldPath
    if fileExists
        then (do
                createDirectoryIfMissing True $ takeDirectory newPath
                toTry `E.catch` handler
                )
        else (do
            putStrLn "\t...file does not exist, skipping."
            return $ error "Unacceptable error"
            )
    where
        toTry = do
            copyFile oldPath newPath
            putStrLn "\t...completed."
            return newPath
        -- handler :: IOError -> IO (Maybe FilePath)
        handler e
            | isPermissionError e = do
                putStrLn "\t...permission denied, throwing error."
                ioError e
            | otherwise = ioError e


-- |Create a summary page that includes an input verification assessment
-- and output charts.
summaryHtml fdsSim = do
  outData <- parseSimulationOutFile fdsSim
  let monitorDict = MonitorDict $ M.empty
  case outData of
      (Left err) -> return $ H.toHtml $ show err
      (Right outData) -> do
          tZone <- getCurrentTimeZone
          run <- runInfo tZone outData fdsSim
          verif <- produceVerificationAssessment fdsSim
          chart <- charts Server tZone outData monitorDict fdsSim
          let
              html = do
                  run
                  H.br
                  verif
                  H.br
                  chart
          return html

summaryHtmlServe dir fdsSim monitorDict = do
  outData <- parseSimulationOutFile fdsSim
  case outData of
      (Left err) -> return $ H.pre $ H.string $ show err
      (Right outData) -> do
          tZone <- getCurrentTimeZone
          run <- runInfo tZone outData fdsSim
          chart <- chartsServe dir tZone outData monitorDict fdsSim
          let
              html = do
                  run
                  H.br
                  chart
          return html

-- |The current status of the model, generally for use while it is running.
runInfo tZone outData fdsSim = do
    return html
    where
      runData = getRunData outData
      mPars = miscellaneous outData
      simEndTime = simEnd mPars
      currentRunTime = case runData of
            [] -> error "No run data."
            x -> (fst $ last x) `diffUTCTime` (fst $ headErr "runInfo" x)
      caseStatus = CaseStatus
                        { caseCurrentProgress = getCurrentProgress outData
                        , casePredictedEndTime = predEndTime simEndTime runData
                        , caseOutStatus = outStatus outData
                        }
      html = do
          H.b $ H.toHtml ("Predicted completion: " :: String)
          H.toHtml predictedCompletion
          H.br
          H.b $ H.toHtml ("Current (real) running time: " :: String)
          H.toHtml $ show currentRunTime
          H.br
          H.b $ H.toHtml ("Current progress: " :: String)
          H.toHtml (caseCurrentProgress caseStatus)
          where
              predictedCompletion :: String
              predictedCompletion = case (caseOutStatus caseStatus) of
                  Incomplete -> show (utcToZonedTime tZone (casePredictedEndTime caseStatus))
                  NumericalInstability -> "---NUMERICAL-INSTABILITY---"
                  StoppedByUser -> "---STOPPED-BY-USER---"
                  Completed -> "---COMPLETED---"


charts location tZone outData monitorDict fdsSim = do
    chartPaths <- produceMonitorCharts monitorDict destDir fdsSim
    return $ mapM_ (\x->(do; genChartHtml x; H.br;)) chartPaths
    where
        destDir = case location of
            Local -> "."
            Server -> "media"
        genChartHtml :: String -> H.Html
        genChartHtml chartPath = H.img
            H.! A.src (H.toValue (takeFileName chartPath :: String))

chartsServe dir tZone outData monitorDict fdsSim = do
    chartPaths <- produceMonitorCharts monitorDict dir fdsSim
    -- only use the SVGs
    return $ mapM_ (\x->(do; genChartHtml x; H.br;)) $ filter (\fp->takeExtension fp == ".svg") chartPaths
    where
        genChartHtml :: String -> H.Html
        genChartHtml chartPath = H.img
            H.! A.src (H.toValue (takeFileName chartPath :: String))

data ServeType = Local | Server

createInputVerificationPage sim = do
  assess <- verifySimulationInputData sim
  return $ toPage "test" css jscript $ H.toHtml assess

produceInputPage
    :: FilePath -- ^The output directory
    -> FDSSimulation
    -> IO FilePath
produceInputPage outputDir simulation = do
    -- Create the directory for output
    createDirectoryIfMissing True outputDir
    -- Parse the input data
    fdsDataRaw <- parseSimulationFDSFile simulation :: IO (Either ParseError FDSFile)
    -- If the input data was successfully parsed, perform the verification.
    assess <- case fdsDataRaw of
        (Right fdsData) -> do
            r <- try $ do
                let x = verifyInputData fdsData
                seq (x) (print ())
                print x
                return x
            return $ case r of
                Right x -> Right x
                Left e -> Left $ show (e :: SomeException)
        -- TODO: render parse errors more beautifully
        (Left err) -> return $ Left $ "Simulation Verification Assessment could not be completed due to a parsing error:\n" ++ show err

    -- If the input data was successfully parsed, create the summary.
    summary <- case fdsDataRaw of
        (Right fdsData) -> do
            r <- try $ do
                let x = summariseInputData fdsData
                print x
                return x
            return $ case r of
                Right x -> Right x
                Left e -> Left $ "Simulation Input Summary could not be completed due to the following error: " ++ show (e :: SomeException)
        (Left err) -> return $ Left $ "Simulation Input Summary could not be completed due to a parsing error:\n" ++ show err

    let page = toPage heading style jscript $ do
            H.toHtml summary
            H.toHtml assess

    let path = joinPath [outputDir, "Verification Assessment.html"]
    writeFile path $ renderHtml page
    return path
    where
      script = jscript
      style = css
      heading = simCHID simulation ++ " Inputs & Verification"


-- (FilePath -> FDSSimulation -> IO [FilePath])
-- |Produce an input verification assessment (HTML).
produceInputPageCompile :: CompilationActionFunction
produceInputPageCompile destDir simulation = do
    createDirectoryIfMissing True destDir

    fdsDataRaw <- parseSimulationFDSFile simulation
    assess <- case fdsDataRaw of
        (Right fdsData) -> do
            r <- try $ do
                let x = verifyInputData fdsData
                seq (x) (print ())
                print x
                return x
            return $ case r of
                Right x -> Right x
                Left e -> Left $ show (e :: SomeException)
        (Left err) -> return $ Left $ "Simulation Verification Assessment could not be completed due to a parsing error:\n" ++ show err

    case fdsDataRaw of
        (Right fdsData) -> do
            r <- try $ do
                let x = summariseInputData fdsData
                print x
                return x
            return $ case r of
                Right x -> Right x
                Left e -> Left $ "Simulation Input Summary could not be completed due to the following error: " ++ show (e :: SomeException)
        (Left err) -> return $ Left $ "Simulation Input Summary could not be completed due to a parsing error:\n" ++ show err

    assess <- verifySimulationInputData simulation
    summary <- summariseSimulationInputData simulation
    let page = toPage heading style jscript $ do
            H.toHtml summary
            H.toHtml assess

    let path = joinPath [destDir, "Verification Assessment.html"]
    writeFile path $ renderHtml page
    return [path]
    where
      script = jscript
      style = css
      heading = simCHID simulation ++ " Inputs & Verification"

-- |Render an input verification assessment (HTML).
-- produceVerificationPage :: CompilationActionFunction
produceVerificationFragment assess = do
    let page = H.div $ do
            H.style $ H.toHtml style
            H.toHtml assess
    renderHtml page
    where
      script = jscript
      style = css

-- |Render an input verification assessment (console text).
renderVerificationConsoleText :: Assessment -> T.Text
renderVerificationConsoleText (Assessment tree) = T.pack $ drawTree $ fmap (T.unpack) $ testTreeToTextTree tree

-- renderTextTree
--     :: T.Text -- ^Indentation string
--     -> Tree T.Text -- ^Tree to be printed
--     -> T.Text
-- renderTextTree indentationString tree

-- renderTextTree'
--     :: T.Text -- ^Indentation string
--     -> T.Text -- ^Text accumulator
--     -> Tree T.Text
--     -> T.Text
-- renderTextTree' istring acc (Node node forest) =
--     renderTextTree' (istring <> istring) (acc <> istring*ilevel <> node <> "\n")

-- TODO: switch this to T.Text
-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree String -> String
drawTree  = unlines . draw

draw :: Tree String -> [String]
draw (Node x ts0) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

produceVerificationPage assess = do
    let page = toPage heading style jscript $ do
            H.toHtml assess
    renderHtml page
    where
        script = jscript
        style = css
        heading = "Input Verification"

produceFullPageCompile :: MonitorDict -> CompilationActionFunction
produceFullPageCompile monitorDict destDir simulation = do
    createDirectoryIfMissing True destDir
    assessRaw <- verifySimulationInputData simulation
    let assess = case assessRaw of
            Left e -> H.toHtml e
            Right x -> H.toHtml x
    summaryRaw <- summariseSimulationInputData simulation
    let summary = case summaryRaw of
            Left e -> H.toHtml e
            Right x -> H.toHtml x
    -- let chartsSection = stdChartsSection chartPaths
    chartsSection <- summaryHtmlServe destDir simulation monitorDict
    let page = toPage heading style jscript $ do
            H.h2 $ H.toHtml ("Inputs" :: String)
            summary
            assess
            H.h2 $ H.toHtml ("Outputs" :: String)
            chartsSection
    let path = joinPath [destDir, "Full.html"]
    writeFile path $ renderHtml page
    return [path]
    where
      script = jscript
      style = css
      heading = simCHID simulation ++ " Outputs"

produceFullPageCompileServe :: MonitorDict -> FilePath -> FDSSimulation
    -> IO H.Html
produceFullPageCompileServe monitorDict destDir simulation = do
    createDirectoryIfMissing True destDir
    assessRaw <- verifySimulationInputData simulation
    let assess = case assessRaw of
            Left e -> H.toHtml e
            Right x -> H.toHtml x
    summaryRaw <- summariseSimulationInputData simulation
    let summary = case summaryRaw of
            Left e -> H.toHtml e
            Right x -> H.toHtml x
    chartsSection <- summaryHtmlServe destDir simulation monitorDict
    let page = toPage heading style jscript $ do
            H.h2 $ H.toHtml ("Inputs" :: String)
            summary
            assess
            H.h2 $ H.toHtml ("Outputs" :: String)
            chartsSection
    -- let path = joinPath [destDir, "Full.html"]
    return page
    where
      script = jscript
      style = css
      heading = simCHID simulation ++ " Outputs"

produceStdOutputPageCompile :: CompilationActionFunction
produceStdOutputPageCompile destDir simulation = do
    createDirectoryIfMissing True destDir
    let monitorDict = def
        chartAction
            = compilationActionFunction .~ produceMonitorCharts monitorDict
                $ compilationActionPath .~ "charts"
                $ def
    chartPaths <- performCompilationAction destDir simulation chartAction
    let chartsSection = stdChartsSection chartPaths
    let page = toPage heading style jscript $ do
            H.h2 $ H.toHtml ("Outputs" :: String)
            chartsSection
    let path = joinPath [destDir, "Outputs.html"]
    writeFile path $ renderHtml page
    return [path]
    where
      script = jscript
      style = css
      heading = simCHID simulation ++ " Outputs"


stdChartsSection :: [FilePath] -> H.Html
stdChartsSection paths' = H.div $ do
    let paths = filter isSVG paths'
    mapM_ (\path -> H.img H.! A.src (H.toValue path)) paths
    where
        isSVG path = takeExtension path == ".png"
