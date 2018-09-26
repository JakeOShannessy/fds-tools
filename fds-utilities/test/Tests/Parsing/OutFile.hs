module Main where

import qualified Data.Vector as V

import Control.Lens

import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Text.Blaze.Html.Renderer.Pretty

import Text.Parsec

import Data.Default
import Data.List
import Data.Either.Utils (forceEither)
import Data.Time

import FDSUtilities.Parsing
import FDSUtilities.Parsing.OutFile
import FDSUtilities.Types
import FDSUtilities.Types.Monitor
import FDSUtilities.CompileCompress
import FDSUtilities.CompileCompress.Charts
import FDSUtilities.CompileCompress.Verification
import FDSUtilities.CompileCompress.Render
import FDSUtilities.Verification
import FDSUtilities.Summary
import FDSUtilities.WatchCase

import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

assertRight :: Show a => Either a b -> IO b
assertRight (Left s) = assertFailure (show s) >> fail "assertion failed"
assertRight (Right a) = return a

tests = TestList
    [ test1
    , test2
    ]

test1 = TestLabel "fdsVersionParser" $ TestCase $ do
    let res = parse titleParser "testString" testString
    assertRight res
    where
        testString = "\n Fire Dynamics Simulator\n"

test2 = TestLabel "Current Date Parser" $ TestCase $ do
    let Right res = parse currentDateParser "testString" testString
    assertEqual "currentDate" "January 26, 2016  00:16:16" res
    where
        testString = " Current Date     : January 26, 2016  00:16:16\n"
