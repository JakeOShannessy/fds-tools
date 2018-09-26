{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
module FDSUtilities.Types.Assess where



-- import Text.Namelist.Types
import Data.Tree
import FDSUtilities.Parsing

data Assessment = Assessment (Tree CompletedTest) deriving Show

type TestName = String

data Test
    = Test TestName (NamelistFile -> TestResult)
    | TestGroup TestName [Test]

data TestResult
    = Success String
    | Warning String
    | Failure String
    deriving (Ord, Eq)

data CompletedTest = CompletedTest TestName TestResult deriving (Show)

instance Show TestResult where
    show (Success str) = "OK -> " ++ str
    show (Warning str) = "Warning -> " ++ str
    show (Failure str) = "Failed -> " ++ str



isSuccess :: CompletedTest -> Bool
isSuccess (CompletedTest _ (Success _)) = True
isSuccess _ = False



