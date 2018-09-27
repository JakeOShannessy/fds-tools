{-# LANGUAGE OverloadedStrings #-}
module FDS.CaseManage (createNewRev) where

import Data.String.Utils

import System.Directory
import System.Environment
import System.FilePath

import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

createNewRev path' = do
  let path = dropTrailingPathSeparator path'
  let dirName = takeBaseName path
      (projectNumber, mSpec, rSpec, name) = case parse parseDirName path dirName of
        Right x -> x
        Left e -> error $ show e
      newRevSpec = rSpec + 1
      newDirName = case name of
        Just nm -> projectNumber ++ "_M" ++ mSpec ++ "_R" ++ show newRevSpec ++ "_" ++ nm
        Nothing -> projectNumber ++ "_M" ++ mSpec ++ "_R" ++ show newRevSpec
  -- create a directory of the new revision in working directory
  exDir <- doesDirectoryExist newDirName
  if exDir then error "directory already exists" else return ()
  createDirectory newDirName
  let oldFDSPath = (joinPath [path, dirName ++ ".fds"])
  -- copy the fds file into the new directory with the new name
  fdsScript <- readFile oldFDSPath
  let newFDSScript = replace dirName newDirName fdsScript
      newFDSPath = (joinPath [".", newDirName, newDirName ++ ".fds"])
  writeFile (joinPath [".", newDirName, newDirName ++ ".fds"]) newFDSScript
  return ()

parseDirName :: Parser (String, String, Int, Maybe String)
parseDirName = do
  projectNumber <- many $ noneOf "_"
  char '_'
  char 'M'
  mSpec <- many $ noneOf "_"
  char '_'
  char 'R'
  rSpec' <- many digit
  name <- option Nothing $ do
    char '_'
    n  <- many $ noneOf "/"
    return $ Just n
  return (projectNumber, mSpec, read rSpec', name)
