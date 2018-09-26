module Main where

-- import Control.Exception
import Control.Lens

import Data.Default
import qualified Data.Map as M

import FDSUtilities.Monitor
import FDSUtilities.Parsing.OutFile
import FDSUtilities.Plot
import FDSUtilities.Simulation
import FDSUtilities.Parsing.SimulationData
import FDSUtilities.Types
import FDSUtilities.Types.Monitor
import FDSUtilities.WatchCase
import FDSUtilities.CompileCompress
import FDSUtilities.CompileCompress.Charts

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.Info

launch path = do
    let simulation = FDSSimulation
            { simDir = "."
            , simCHID = takeBaseName path
            }
        chartAction
            = compilationActionFunction .~ produceFullPageCompile def
                $ compilationActionPath .~ "charts"
                $ def
    chartPath' <- performCompilationAction "Charts" simulation chartAction
    let chartPath = case chartPath' of
			[x] -> x
			[] -> error "no chartPath"
    chartPathAbs <- canonicalizePath $ joinPath ["Charts", chartPath]
    putStrLn ("\"" ++ chartPathAbs ++ "\"")
    (exitCode, stdout, stderr) <- case os of
        "windows" -> readProcessWithExitCode "start" [chartPathAbs] []
        "mingw32" -> readProcessWithExitCode "start" [chartPathAbs] []
        "linux" -> readProcessWithExitCode "xdg-open" [chartPathAbs] []
    print exitCode
    if not (null stdout) then putStrLn stdout else return ()
    if not (null stderr) then putStrLn stderr else return ()


main = do
    args <- getArgs
    case args of
        ["--version"] -> print "version"
        [file] -> launch file
        []     -> error "No monitor file specified."
        (x:xs:xss) -> error "Too many arguments."
