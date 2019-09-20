module Main where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Lens
import Control.Monad

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

import Web.Browser (openBrowser)

launch path = do
    putStrLn "Rendering, please wait..."
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
    putStrLn ("Opening: \"" ++ chartPathAbs ++ "\" in browser")
    r <- openBrowser ("file://" ++ chartPathAbs)
    if r
        then do
            putStrLn "Browser window opened"
            threadDelay (3*1000*1000)
        else do
            putStrLn "Opening failed, press any key to continue"
            void getChar

guardedLaunch file = do
    let dir = takeDirectory file
    setCurrentDirectory dir
    res <- Control.Exception.try $ launch file
    case res of
        Right x -> pure x
        Left err -> do
            print (err :: SomeException)
            putStrLn ""
            putStrLn "Rendering failed, press any key to continue."
            void getChar
            pure ()

main = do
    args <- getArgs
    case args of
        ["--version"] -> print "version"
        [file] -> guardedLaunch file
        []     -> error "No monitor file specified."
        (x:xs:xss) -> error "Too many arguments."
