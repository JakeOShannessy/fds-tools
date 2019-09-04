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
import FDSUtilities.Verification
import FDSUtilities.Summary
import FDSUtilities.WatchCase

import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

import Tests.Verification
import qualified Tests.Parsing.SliceFile
import qualified Tests.Parsing.PLOT3D

main = do
    putStrLn "Running tests..."
    mainWithOpts

mainWithOpts = do
    -- Test options can also be specified in the code. The TestOptions
    -- type is an instance of the Monoid type class, so the easiest way
    -- to get an empty set of options is with `mempty`.
    let empty_test_opts = mempty :: TestOptions

    -- We update the empty TestOptions with our desired values.
    let my_test_opts = empty_test_opts
            { topt_maximum_generated_tests = Just 1000
            , topt_timeout = Just $ Just (40000000::Int)
            }

    -- Now we create an empty RunnerOptions in the same way, and add
    -- our TestOptions to it.
    let empty_runner_opts = mempty :: RunnerOptions
    let my_runner_opts = empty_runner_opts
            { ropt_test_options = Just my_test_opts
            }

    defaultMainWithOpts tests my_runner_opts

tests =
    [
     testGroup "Parsing Tests" $
        (hUnitTestToTests outParseTests)
        -- ++ (hUnitTestToTests smvParseTests)
        ++
        (hUnitTestToTests Tests.Parsing.SliceFile.tests)
        ++ (hUnitTestToTests Tests.Parsing.PLOT3D.tests)
    -- , testProperty "label" prop_propertyTest
    , testGroup "Verification Tests" $
        (hUnitTestToTests verificationTests)
    , testGroup "Full Verification Tests" $
        (hUnitTestToTests fullVerificationTests)
    ]

outParseTests = TestLabel "Out File Parsing Tests" $ TestList
    -- [ test1
    -- , inputSummary2
    -- , inputSummary3
    -- , regressionCase1
    -- ,
    [ regressionCase2
    ]

test1 = TestLabel "Some Test" $ TestCase $ do
    Right outData <- parseOutFile "test-data/BunCentreCoarse/BunCentreCoarse.out"
    let steps = timesteps outData
    if length steps == 29
        then return ()
        else error "wrong number of timesteps"
    return ()

fullVerificationTests = TestList
    [ verificationTest
    , verificationTest2
    ]

verificationTest = TestLabel "verificationTest" $ TestCase $ do
  Right res <- verifyInputFile "test-data/IlonaRose_MD_R57_IlonaRose.fds"
  writeFile "test-out/verif.html" $ renderHtml $ H.toHtml res

verificationTest2 = TestLabel "verificationTest2" $ TestCase $ do
  Right res <- verifyInputFile "test-data/BunCentreCoarse/BunCentreCoarse.fds"
  writeFile "test-out/verifCTCI.html" $ renderHtml $ H.toHtml res

inputSummary2 = TestLabel "inputSummary2" $ TestCase $ do
    res <- summariseInputFile "test-data/L140070_MZ1A_R10_CTCI.fds"
    writeFile "test-out/summaryCTCI.html" $ renderHtml $ H.toHtml res

inputSummary3 = TestLabel "inputSummary3" $ TestCase $ do
    res <- summariseInputFile "test-data/s131444_MWC_R13_ThePonds.fds"
    writeFile "test-out/summarys131444_MWC_R13_ThePonds.html" $ renderHtml $ H.toHtml res

regressionCase1 = TestLabel "regressionCase1" $ TestCase $ do
    Right outData <- parseOutFile "test-data/0017_MAltB_R6.out"
    let steps = timesteps outData
        expected = 546
    assertBool (show (length steps) ++ " steps found, " ++ show expected ++
        " steps expected.") (length steps == expected)

regressionCase2 = TestLabel "regressionCase2" $ TestCase $ do
    Right outData <- parseOutFile "test-data/room_fire.out"
    let steps = timesteps outData
        expected = 21
    assertBool (show (length steps) ++ " steps found, " ++ show expected ++
        " steps expected.") (length steps == expected)

parseTimeStepTest1 = TestLabel "Single Mesh Timestep" $ TestCase $ do
    testData <- readFile "test-data/fullSingleMeshStep.txt"
    tZone <- getCurrentTimeZone
    let parsedData = case parse (parseTimeStep tZone) "fullSingleMeshStep.txt" testData of
            Right x -> x
            Left e -> error $ show e
    assertBool "ss" (length (timeStepMeshes' parsedData) == 1)

parseTimeStepTest2 = TestLabel "Multiple Mesh Timestep" $ TestCase $ do
    testData <- readFile "test-data/fullMultipleMeshStep.txt"
    tZone <- getCurrentTimeZone
    let parsedData = case parse (parseTimeStep tZone) "fullMultipleMeshStep.txt" testData of
            Right x -> x
            Left e -> error $ show e
        expectedMeshes = 14
        foundMeshes = length (timeStepMeshes' parsedData)
    assertBool (show foundMeshes ++ " meshes found, " ++ show expectedMeshes ++
        " meshes expected.") (foundMeshes == expectedMeshes)
