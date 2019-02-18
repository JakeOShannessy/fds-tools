{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.Parsing.FDSFile
    ( parseFDSFile ) where

import Text.FDSInputParser
import Data.Text.IO as T

import Text.Parsec.Error (ParseError)
import Text.Namelist.Types

parseFDSFile :: FilePath -> IO (Either ParseError NamelistFile)
parseFDSFile filepath = parseFDSInputFile filepath
