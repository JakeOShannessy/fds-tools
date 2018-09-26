module FDSUtilities.CompileCompress.Vitals
    ( copyVitals
    , copyVital
    )
    where

import Control.Exception as E

import Data.Maybe

import FDSUtilities.Types

import System.Directory
import System.IO.Error
import System.FilePath

copyVitals :: FilePath -> FDSSimulation -> IO [FilePath]
copyVitals destdir fdsSim  = do
    files <- mapM (copyVital destdir fdsSim) suffixes
    return $ catMaybes files
    where
        suffixes =
            [ ".fds"
            , ".smv"
            , ".out"
            , "_evac.out"
            , ".end"
            , "_devc.csv"
            , "_hrr.csv"
            , "_mass.csv"
            , "_evac.csv"
            ]
copyVital :: FilePath -> FDSSimulation -> String -> IO (Maybe FilePath)
copyVital destDir simulation suffix = do
    putStr "Copying "
    putStr oldPath
    putStrLn "..."
    fileExists <- doesFileExist oldPath
    if fileExists
        then (do
            createDirectoryIfMissing True destDir
            toTry `E.catch` handler
            )
        else (do
            putStrLn "\t...file does not exist, skipping."
            return Nothing
            )
    where
        path = simDir simulation
        casename = simCHID simulation
        toTry = do
            copyFile oldPath newPath
            putStrLn "\t...completed."
            return $ Just newPath
        filename = casename ++ suffix
        oldPath = joinPath [path, filename]
        newPath = joinPath [destDir, filename]
        handler :: IOError -> IO (Maybe FilePath)
        handler e
            | isPermissionError e = do
                putStrLn "\t...permission denied, skipping."
                return Nothing
            | otherwise = ioError e
