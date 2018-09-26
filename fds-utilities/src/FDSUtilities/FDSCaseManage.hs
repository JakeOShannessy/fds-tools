module FDSUtilities.FDSCaseManage where

import FDSUtilities
import System.Directory
import Control.Monad
import System.FilePath

-- |For convenience, called when starting the CaseTree
initCaseTree :: FilePath -> IO CaseTreeNode
initCaseTree projDir = buildCaseTree projDir

-- |Create the tree the holds all of the diffferent FDS cases
buildCaseTree :: FilePath -> IO CaseTreeNode
buildCaseTree path = do 
    isCase <- checkIfCase path
    if isCase
        then do
            theCase <- buildCase path
            return $ CaseNode theCase
        else do
            subDirs <- getSubdirectories path
            subNodes <- mapM buildCaseTree subDirs
            return $ DirectoryNode path NoParent subNodes

-- |Monad compatible if-then-else
condM condAction thenBranch elseBranch = do
    bool <- condAction
    if bool
        then return thenBranch
        else return elseBranch

-- |Checks if the particular directory should be treated as a case (i.e. contains the
--  "case" file
checkIfCase :: FilePath -> IO Bool
checkIfCase dirPath = (getDirectoryContents dirPath) >>= (return . (any ((==) "case")))

-- |Create a Case from a directory
buildCase :: FilePath -> IO Case
buildCase path = do
    subDirs <- getSubdirectories path
    revs <- mapM (buildRevision caseName) subDirs
    return $ Case path caseName revs
    where
        caseName = last $ splitDirectories path

-- |List all subdirectories (excluding "." and "..")
getSubdirectories :: FilePath -> IO [FilePath]
getSubdirectories dir = do
    conts1 <- getDirectoryContents dirName 
    conts <- filterM isnotSpecDirec conts1
    let con = map ((++) dirName) conts
        in  (filterM doesDirectoryExist con)
    where 
        dirName = dir ++ "\\"
        isnotSpecDirec :: FilePath -> IO Bool
        isnotSpecDirec x = do
            let a = ((x /= ".") && (x /= "..") && (x /= "pyrosim"))
                in return a

