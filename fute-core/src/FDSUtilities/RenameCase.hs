module FDSUtilities.RenameCase
    ( renameSimulation
    , renameFDSCase
    ) where

import Control.Lens
import Control.DeepSeq
import Control.Exception

import Data.String.Utils

import FDSUtilities.Types

import GHC.IO.Handle

import System.Directory
import System.Environment
import System.FilePath
import System.FilePath.Glob
import System.IO
import System.Process
--import System.Process.Pipe

import Text.Printf

-- TODO: change from simply finding and replacing strings to doing a proper parse and replace.
-- |Renames all files and replaces the the reference to the CHID in the SMV file.
--  This function does not test for internal consistency, nor it purely finds and replaces strings.
renameSimulation
    :: FDSSimulation    -- ^The simulation to rename
    -> String           -- ^The new CHID
    -> IO FDSSimulation -- ^Renames the simulation and returns the new simulation properties
renameSimulation simulation newCHID = do
    renameFDSCase oldSimDir oldCHID newCHID
    exist <- doesDirectoryExist newSimDir
    -- TODO: do we reverse the change.
    if exist then error "Directory already exists." else renameDirectory oldSimDir newSimDir
    -- TODO: catch some exceptions here
    return $ FDSSimulation {simCHID = newCHID, simDir = newSimDir}
    where
        oldCHID = simCHID simulation
        oldSimDir = simDir simulation
        newSimDir = joinPath [(takeDirectory oldSimDir), newCHID]

renameFDSCase path originalCasename newCasename = do
    reconfigureSMVFile path originalCasename newCasename
    reconfigureFDSFile path originalCasename newCasename
    filesToRename <- gatherFilenames path originalCasename
    mapM_ (renameDatFile originalCasename newCasename) (concat filesToRename)

renameDatFile originalCasename newCasename origFilePath  = do
    let (location, filename) = splitFileName origFilePath
        newFilename = replace originalCasename newCasename filename
        newFilePath = combine location newFilename
    renameFile origFilePath newFilePath

-- |Changes all of the references to the old CHID in the SMV file to the new CHID. Does not rename the SMV file.
reconfigureSMVFile casepath originalCasename newCasename = do
    let oldSMVPath = (joinPath [casepath, originalCasename ++ ".smv"])
    oldSMVFile <- openFile oldSMVPath ReadMode
    oldSMV <- hGetContents oldSMVFile
    rnf oldSMV `seq` hClose oldSMVFile
    let newSMV = replace originalCasename newCasename oldSMV
    writeFile oldSMVPath newSMV

-- |Changes all of the references to the old CHID in the FDS file to the new CHID. Does not rename the FDS file.
reconfigureFDSFile casepath originalCasename newCasename = do
    let oldFDSPath = (joinPath [casepath, originalCasename ++ ".fds"])
    oldFDSFile <- openFile oldFDSPath ReadMode
    oldFDS <- hGetContents oldFDSFile
    rnf oldFDS `seq` hClose oldFDSFile
    let newFDS = replace originalCasename newCasename oldFDS
    writeFile oldFDSPath newFDS

gatherFilenames :: FilePath -> String -> IO [[FilePath]]
gatherFilenames path originalCasename = do
    let datPattern = compile (originalCasename ++ "*")
    globDir [datPattern] path

