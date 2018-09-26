module FDSUtilities.CompileCompress.Compression
    ( performCompression
    )
    where

import Control.Exception as E
import Control.Lens
import Control.Monad

import Data.Default
import Data.List
import Data.Maybe

import FDSUtilities.CompileCompress.Screenshots
import FDSUtilities.CompileCompress.Vitals
import FDSUtilities.Monitor
import FDSUtilities.Paths
import FDSUtilities.Simulation
import FDSUtilities.Smokeview
import FDSUtilities.Types
import FDSUtilities.Types.Smokeview

import GHC.IO.Handle

import System.Directory
import System.IO.Error
import System.FilePath
import System.Process
--import System.Process.Pipe
import System.IO

import Text.Printf

performCompression :: FilePath -> FDSSimulation -> IO ()
performCompression destDir simulation = do
    cwd <- getCurrentDirectory
    destDirAbs <- canonicalizePath destDir
    setCurrentDirectory path
    putStrLn "FDS Results Compression Script"
    putLinebreak
    -- putStrLn "Enter case name: "
    -- casename <- getLine
    -- putLinebreak
    -- let casename = "HKOperaHouse2a_0J"
    let fdsFilename = casename ++ ".fds"
        destdir = (joinPath [destDirAbs, casename ++ "_CompressedResults"])
        colourbarFlip = False
        barLevel = 266
        iniFile =  "" --  makeIni $ iniConfigFDSFilename .~ (casename ++ ".fds") $ def
        skipframe = 20
    writeFile (casename ++ ".ini") iniFile
    createDirectoryIfMissing False destdir
    putStrLn $ "Compressed results outputted to " ++ destdir
    putLinebreak
    -- relDestDir <- makeRelativeToCurrentDirectory destdir
    let smokezipPath = "C:\\PROGRA~1\\FDS\\FDS5\\bin\\smokezip_win_64.exe"
    putStrLn $ "Running: " ++ smokezipPath ++ " -d " ++ "\"" ++ destdir ++ "\"" ++ " -skip "  ++ (show skipframe) ++ " -f " ++ casename
    putLinebreak

    (stdinHndl, stdoutHndl, stderrHndl, procHndl) <-
        runInteractiveCommand (smokezipPath ++ " -d " ++ "\"" ++ destdir ++ "\"" ++ " -skip "  ++ (show skipframe) ++ " -f " ++ casename)
    hSetBinaryMode stdoutHndl False
    hSetBuffering stdoutHndl LineBuffering
    hSetBuffering stdout LineBuffering
    err <- hGetContents stderrHndl
    res <- hGetContents stdoutHndl
    putStr res
    putStr err
    -- writeFile (joinPath [destdir, "compressLog.out"]) res
    putLinebreak

    copyFDSResultFileInt casename destdir ".fds"
    copyFDSResultFileInt casename destdir ".smv"
    copyFDSResultFileInt casename destdir ".out"
    copyFDSResultFileInt casename destdir "_evac.out"
    copyFDSResultFileInt casename destdir ".end"
    copyFDSResultFileInt casename destdir "_devc.csv"
    copyFDSResultFileInt casename destdir "_hrr.csv"
    copyFDSResultFileInt casename destdir "_mass.csv"
    copyFDSResultFileInt casename destdir "_evac.csv"
    setCurrentDirectory cwd
    where
        path = simDir simulation
        casename = simCHID simulation



copyFDSResultFileInt casename destdir suffix = do
    putStrLn $ "Copying " ++ suffix ++ " file..."
    copyFDSResultFile casename destdir suffix

copyFDSResultFile casename destdir suffix = do
    fileExists <- doesFileExist filename
    (toTry fileExists) `E.catch` handler
    where
        filename = casename ++ suffix
        toTry fileExists = if fileExists
            then copyFile filename (joinPath [destdir, filename])
            else putStrLn "File does not exist, skipping.."
        handler :: IOError -> IO ()
        handler e
            | isPermissionError e = putStrLn "Permission denied: skipping"
            | otherwise = ioError e

