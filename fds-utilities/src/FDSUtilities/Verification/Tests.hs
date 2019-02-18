{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.Verification.Tests where

import FDSUtilities.Simulation
import FDSUtilities.Parsing
import FDSUtilities.Types.Assess
import FDSUtilities.Types
import FDSUtilities.FDSFile
import Data.List
import System.Directory
import System.FilePath
import System.FilePath.Glob

import Debug.Trace
import qualified Data.Text as T
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Tree

import Text.Namelist
import Text.Parsec.Pos

default (T.Text)

-- CONSIDER: There are multiple ways to get the slice files: smvFile or
-- directory search
preSimulationSuite :: FDSSimulation -> IO Bool
preSimulationSuite simulation = do
    fdsFileData <- parseSimulationFDSFile simulation
    return False

-- This should produce a range of invalidity types rather than just a Bool.
validateSimulation :: FDSSimulation -> IO Bool
validateSimulation simulation = do
    relatedFiles <- gatherFilenames simulation
    smvListedFiles <- getSMVListedFiles simulation
    vp <- validateFilePresence relatedFiles smvListedFiles requiredFiles
    if vp
        then putStrLn "Validation suite passed."
        else putStrLn "Validation suite failed."
    return vp
    where
        requiredFiles = map ((flip basicSuffix) simulation) requiredSuffixes

validateFilePresence :: [FilePath] -> [FilePath] -> [FilePath] -> IO Bool
validateFilePresence relatedFiles' smvListedFiles' requiredFiles' = do
    relatedFiles <- mapM canonicalizePath relatedFiles'
    smvListedFiles <- mapM canonicalizePath smvListedFiles'
    requiredFiles <- mapM canonicalizePath requiredFiles'
    req <- mapM doesFileExist requiredFiles
    smv <- mapM doesFileExist smvListedFiles
    let excessFiles = relatedFiles \\ (smvListedFiles ++ requiredFiles)
        exc = not $ null excessFiles
    putStr "Required file presence: "
    if all id req then putStrLn "Pass" else putStrLn "Fail"
    putStr "SMV listed file presence: "
    if all id smv then putStrLn "Pass" else putStrLn "Fail"
    putStr "Presence of unlisted files: "
    if exc then putStrLn "Pass" else putStrLn "Fail"
    return $ all id [all id req, all id smv, exc]
    -- rel <- mapM doesFileExist smvListedFiles
-- data ValidationTest = ValidationTest String (FDSSimulation -> IO Bool)

getSMVListedFiles :: FDSSimulation -> IO [FilePath]
getSMVListedFiles simulation = do
    smvData' <- parseSimulationSMVFile simulation
    let smvData = case smvData' of
            Left e -> error $ show e
            Right x -> x
    let dataFiles = smvDataFiles smvData
        fileNames = map getDataFileName dataFiles
        filePaths = map (\x-> joinPath [simDir simulation, x]) fileNames
    return filePaths

-- |List all of the filenames in the simulation directory that match the CHID.
gatherFilenames :: FDSSimulation -> IO [FilePath]
gatherFilenames simulation = do
    let pattern = compile ((simCHID simulation) ++ "*")
    relatedFiles <- globDir [pattern] (simDir simulation)
    return (concat relatedFiles)

-- |Inverse of map.
pam :: [a -> b] -> a -> [b]
pam f x = map g f
    where
        g h = h x
-- |List of suffixes found in a valid simulation.
requiredSuffixes :: [String]
requiredSuffixes =
    [ "fds"
    , "smv"
    , "out"
    , "end"
    ]

--VALIDATION PROPERTIES
----REQUIREMENTS
-- .end, .fds, etc.. present
-- all references to other files in smvfile present

--VERIFICATION PROPERTIES

verificationAssessment = Assessment . verificationTests

verificationTests :: FDSFile -> Tree CompletedTest
verificationTests fdsData =
  let
    testName = "Verification Assessment"
    tests =
      [ parameterVerificationTests
      , outputDataCoverage
      , meshOverlapTests
      , flowCoverage
      , leakage
      , devicesTest
      , spkDetCeilingTest
      ]
    testResults = pam tests fdsData
    summaryResults = worstN testResults
  in Node (CompletedTest testName summaryResults) testResults

-- ensure that everage flow device is covered by a flow rate device
flowCoverage fdsData =
    let
        testName = "Flow Coverage Test"
        -- it is also possible that other objects (such as OBST have flow)
        vents = fdsFile_Vents fdsData
        obsts = fdsFile_Obsts fdsData
        surfs = fdsFile_Surfs fdsData
        -- vents which may have a flow
        ventsWithFlows = filter (ventHasFlow fdsData) vents
        -- obsts that have surfaces with flows
        obstWithFlows = filter (obstHasFlow fdsData) vents
        -- for each of the vents, ensure there is a flow device with the same
        -- dimensions find those which do not
        notCovered =  filter (not . (hasFlowDevc fdsData)) ventsWithFlows
    in if null notCovered
            then Node (CompletedTest testName $ Success
                $ "All flow devices have devcs.") []
            else Node (CompletedTest testName $ Failure $ unlines
                $ map formatRes notCovered) []
    where
        formatRes nml = "Flow object " <> getIdBound nml
            <> " does not have a flow tracking devices.\n    "
            -- ++ T.unpack (pprint nml)

leakage fdsData =
    let
        testName = "Leakage Implementation Test"
        parts = fdsFile_Parts fdsData
        screenParts = filter isScreenPart parts
        isScreenPart part = case part_DRAG_LAW part of
            "SCREEN" -> True
            _ -> False
        hasInertOrDefaultSurf nml = case part_SURF_ID nml of
            Nothing -> True
            Just "INERT" -> True
            _ -> False
    in if all (not . hasInertOrDefaultSurf) screenParts
            then Node (CompletedTest testName $ Success
                $ "No inert screens.")
                []
            else Node (CompletedTest testName $ Failure
                $ "PART uses the SCREEN drag law, but uses an INERT surface.")
                []

-- |Ensure that no devices are stuck in solids.
devicesTest :: FDSFile -> Tree CompletedTest
devicesTest fdsData =
    let
        testName = "Devices Stuck in Solids Test"
        stuckDevices = filter (fromMaybe False . stuckInSolid fdsData)
            $ fdsFile_Devcs fdsData
    in if null stuckDevices
        then Node (CompletedTest testName $ Success $ "No stuck devices.") []
        else Node (CompletedTest testName $ Failure $ unlines
            $ map formatRes stuckDevices) []
    where
        formatRes nml = "Device " <> getIdBound nml
            <> " is placed within a solid obstruction.\n    "
            -- <> T.unpack (pprint nml)

-- |Ensure that sprinklers and smoke detectors are beneath a ceiling.
spkDetCeilingTest :: FDSFile -> Tree CompletedTest
spkDetCeilingTest fdsData =
    let
        testName = "Sprinklers and detectors below ceiling"
        nonBeneathCeiling = filter
            (not . fromMaybe False . beneathCeiling fdsData)
            $ filter (\x-> isSprinkler fdsData x || isSmokeDetector fdsData x)
            $ fdsFile_Devcs fdsData
    in if null nonBeneathCeiling
        then Node (CompletedTest testName $ Success $ "No distant devices.") []
        else Node (CompletedTest testName $ Failure $ unlines
            $ map formatRes nonBeneathCeiling) []
    where
        formatRes nml = "Device " <> getIdBound nml
            <> " is not directly beneath the ceiling.\n    "
            -- <> T.unpack (pprint nml)

-- |Take the xb dimensions of a vent and see if there is a flow vent with the
-- matching dimensions, or a device that references it as a duct node.
hasFlowDevc :: FDSFile -> Vent -> Bool
hasFlowDevc fdsData namelist =
    let
        devcs = filter
            (\nml->devc_QUANTITY nml == (Just "VOLUME FLOW"))
            (fdsFile_Devcs fdsData)
        trackingFlowMatchingXB = any (matchXBs namelist) devcs
        -- get all the devices
        allDevcs = fdsFile_Devcs fdsData
        -- take only the devices which have a "DUCT_ID" parameter
        ductIDDevices = filter (isJust . devc_DUCT_ID) allDevcs
        -- take only the devices where the "DUCT_ID" matches the flowing
        -- namelist
        relevantDuctIDDevices = filter (\nml-> (Just True) == (do
            ductId <- devc_DUCT_ID nml
            flowId <- getId nml
            pure (ductId == flowId))) ductIDDevices
        -- take only the devices that measure "DUCT VOLUME FLOW", and check that
        -- the list is not null
        trackingFlowViaDuctID = not $ null $ filter
            (\nml->devc_QUANTITY nml == (Just "DUCT VOLUME FLOW")) allDevcs
    in trackingFlowMatchingXB || trackingFlowViaDuctID

-- |Check one obstruction and determine if it intersects any other namelists.
obstIntersectsWithOthers :: HasXB a => FDSFile -> a -> [Obst]
obstIntersectsWithOthers fdsData namelist =
    let
        obsts :: [Obst]
        obsts = fdsFile_Obsts fdsData
    in filter (nmlIntersect namelist) obsts

-- |Check if two objects have the same XB. If either does not have an XB, simply
-- return False.
matchXBs :: (MightHaveXB a, MightHaveXB b) => a -> b -> Bool
matchXBs nmlA nmlB = case (tryGetXB nmlA, tryGetXB nmlB) of
    (Just a, Just b) -> a == b
    _ -> False

meshOverlapTests fdsData =
    let
        testName = "Mesh Overlap Test"
        meshes = fdsFile_Meshes fdsData
        combinations :: [(Mesh, Mesh)]
        combinations = combinationPairs meshes
        testPair (m1,m2) = nmlIntersect m1 m2
        results :: [(Mesh, Mesh)]
        results = foldl' (\failed (m1,m2) ->
            if nmlIntersect m1 m2
                then (m1,m2):failed
                else failed) [] combinations
    in if null results
            then Node (CompletedTest testName $ Success
                $ "No meshes overlap.") []
            else Node (CompletedTest testName $ Failure $ unlines
                $ map formatRes results) []
    where
        formatRes (m1,m2) = "Meshes " <> (show $ getId m1) <> " and "
            <> (show $ getId m2) <> " overlap.\n    " <> show (getXB m1)
            <> "\n    " <> show (getXB m2)

combinationPairs :: [a]  -> [(a,a)]
combinationPairs [] = []
combinationPairs (l:ls) = (map (\y->(l, y)) ls) ++ (combinationPairs ls)

parameterVerificationTests fdsData =
  let
    testName = "Input Verification Tests"
    tests =
      [ reactionTests
      , miscTests
      , burnerTestsGroup
      , dumpTests
      ]
    testResults = pam tests fdsData
    summaryResults = worstN testResults
  in Node (CompletedTest testName summaryResults) testResults


-- |Test that the REAC properties are reasonable.
reactionTests fdsData =
  let
      testName = "REAC Properties"
      tests =
          [ sootYield
          , coYield
--           , chemicalFormula
          ]
      (summaryResults, testResults) = case (specified fdsData) of
        l@(Node (CompletedTest _ r@(Failure _)) _) -> (r, [])
        l@(Node (CompletedTest _ (Success _)) _) ->
          let
              [reac] = fdsFile_Reacs fdsData
              testResults :: [Tree CompletedTest]
              testResults = pam (pam tests reac) fdsData
              summaryResults :: TestResult
              summaryResults = worstN testResults
          in (summaryResults, testResults) :: (TestResult, [Tree CompletedTest])
  in Node (CompletedTest testName summaryResults) testResults
  where
      specified :: FDSFile -> Tree CompletedTest
      specified fdsData =
          let
              testName = "Specified"
          in case fdsFile_Reacs fdsData of
            []  -> Node (CompletedTest testName
                $ Failure "No REAC namelist specified.") []
            [x] -> Node (CompletedTest testName
                $ Success "REAC namelist specified.") []
            _   -> Node (CompletedTest testName
                $ Failure "Multiple REAC namelists specified.") []

      sootYield :: Reac -> FDSFile -> Tree CompletedTest
      sootYield reac fdsData =
          let
              propName = "Soot Yield"
              testName = "Soot Yield"
              possibleValues = [0.07, 0.1] :: [Double]
              nValue = reac_SOOT_YIELD reac
          in if nValue `elem` possibleValues
              then Node (CompletedTest testName $ Success
                $ propName <> " is " <> show nValue <> ".") []
              else Node (CompletedTest testName $ Failure
                $ propName <> " is " <> show nValue <> " not one of "
                <> show possibleValues <> ".") []

      coYield reac fdsData =
        let
            propName = "CO Yield"
            testName = "CO Yield"
            val = 0.05 :: Double
            nValue = reac_CO_YIELD reac
        in if nValue == val
            then Node (CompletedTest testName $ Success $ propName ++ " is "
                ++ show val ++ ".") []
            else Node (CompletedTest testName $ Failure $ propName ++ " is "
                ++ show nValue ++ " not " ++ show val ++ ".") []



-- |Test that the MISC properties are reasonable.
miscTests :: FDSFile -> Tree CompletedTest
miscTests fdsData =
    let
      testName = "MISC Properties"
      tests =
          [ visibilityFactor
          , maximumVisibility
          ]
      (summaryResults, testResults) = case (specified fdsData) of
        l@(Node (CompletedTest _ r@(Failure _)) _) -> (r, [])
        l@(Node (CompletedTest _ (Success _)) _) ->
          let
              Just misc = fdsFile_Misc fdsData
              testResults :: [Tree CompletedTest]
              testResults = pam (pam tests misc) fdsData
              summaryResults :: TestResult
              summaryResults = worstN testResults
          in (summaryResults, testResults) :: (TestResult, [Tree CompletedTest])
  in Node (CompletedTest testName summaryResults) testResults
  where
      specified :: FDSFile -> Tree CompletedTest
      specified fdsData =
          let
              testName = "Specified"
          in case fdsFile_Misc fdsData of
            Nothing -> Node (CompletedTest testName
                $ Failure "No MISC namelist specified.") []
            Just x  -> Node (CompletedTest testName
                $ Success "MISC namelist specified.") []

      visibilityFactor :: Misc -> FDSFile -> Tree CompletedTest
      visibilityFactor misc fdsData =
          let
              testName = "Visiblity Factor"
              nValue = misc_VISIBILITY_FACTOR misc
          in case nValue of
              3 -> Node (CompletedTest testName $ Success
                $ "Value: " ++ show nValue ++ ".") []
              8 -> Node (CompletedTest testName $ Success
                $ "Value: " ++ show nValue ++ ".") []
              _ -> Node (CompletedTest testName $ Failure
                $ "Value of " ++ show nValue ++ " not validated.") []

      maximumVisibility :: Misc -> FDSFile -> Tree CompletedTest
      maximumVisibility misc fdsData =
          let
              testName = "Maximum Visibility"
              nValue = misc_MAXIMUM_VISIBILITY misc
          in if nValue <= 100
              then Node (CompletedTest testName $ Success $ "Value: "
                ++ show nValue ++ ".") []
              else Node (CompletedTest testName $ Success $ "Value of "
                ++ show nValue ++ " may result in clipped output.") []

-- |Test that the DUMP properties are reasonable.
dumpTests :: FDSFile -> Tree CompletedTest
dumpTests fdsData =
    let
      testName = "DUMP Properties"
      tests =
          [ dt_restart
          , nframes
          ]
      (summaryResults, testResults) = case (specified fdsData) of
        l@(Node (CompletedTest _ r@(Failure _)) _) -> (r, [])
        l@(Node (CompletedTest _ (Success _)) _) ->
          let
              Just dump = fdsFile_Dump fdsData
              testResults :: [Tree CompletedTest]
              testResults = pam (pam tests dump) fdsData
              summaryResults :: TestResult
              summaryResults = worstN testResults
          in (summaryResults, testResults) :: (TestResult, [Tree CompletedTest])
  in Node (CompletedTest testName summaryResults) testResults
  where
      (tStart, tEnd) = getSimTimes fdsData
      simInterval = tEnd - tStart

      specified :: FDSFile -> Tree CompletedTest
      specified fdsData =
          let
              testName = "Specified"
          in case fdsFile_Dump fdsData of
            Nothing  -> Node (CompletedTest testName
                $ Failure "No DUMP namelist specified.") []
            Just x -> Node (CompletedTest testName
                $ Success "DUMP namelist specified.") []

      dt_restart :: Dump -> FDSFile -> Tree CompletedTest
      dt_restart dump fdsData =
          let
              testName = "Restart Interval"
              nValue = dump_DT_RESTART dump
          in Node (CompletedTest testName $ Success
            $ "Value: " ++ show nValue ++ ".") []

      nframes :: Dump -> FDSFile -> Tree CompletedTest
      nframes dump fdsData =
          let
              testName = "Number of Frames"
              nValue = dump_NFRAMES dump
          in if (mod (round simInterval :: Int) nValue)  == 0
                -- TODO: check that simTime is whole number
              then Node (CompletedTest testName $ Success
                $ "Value: " ++ show nValue ++ ".") []
              else Node (CompletedTest testName $ Success
                $ "Value of " ++ show nValue
                ++ " may result in clipped output.") []

worstN :: [Tree CompletedTest] -> TestResult
worstN ts = stripString $ worst
    $ map (\(Node (CompletedTest _ res) _) -> res) ts

worst :: [TestResult] -> TestResult
worst [] = error "No results to summarise."
worst results = last $ sort results

stripString res = case res of
            (Success _) -> Success ""
            (Warning _) -> Warning ""
            (Failure _) -> Failure ""


burnerTestsGroup :: FDSFile -> Tree CompletedTest
burnerTestsGroup = \fdsData ->
  let
    testName = "Burners"
    burners = getBurners fdsData
    completedTests = map (burnerTestsIndividual fdsData) burners
  in case completedTests of
    [] -> Node (CompletedTest testName (Warning "No burners present."))
        completedTests
    _  -> Node (CompletedTest testName (worstN completedTests)) completedTests

-- |Tests to apply to the various burners found in a model.
burnerTestsIndividual :: FDSFile -> Burner -> Tree CompletedTest
burnerTestsIndividual fdsData burner =
  let
      testName = "Burner Tests for " ++ burnerName
      tests = pam tests' burner
      testResults = pam tests fdsData
      summaryResults = worstN testResults
  in Node (CompletedTest testName summaryResults) testResults
  where
    tests' :: [(Burner -> FDSFile -> Tree CompletedTest)]
    tests' =
      [ sourceFroudeTest
      , nonDimTest
      , growthRateTest
      , intersectionTest
      ]
    burnerName = getIdBound burner

    sourceFroudeTest :: Burner -> FDSFile -> Tree CompletedTest
    sourceFroudeTest burner fdsData =
      let
        testName = "Source Froude Number"
        q = getBurnerMaxHRR fdsData burner
        sF = sourceFroude q fuelArea
      in if sF <= maxThreshold
        then Node (CompletedTest testName $ Success $ "Conforms. " ++ show sF
            ++ " <= " ++ show maxThreshold) []
        else Node (CompletedTest testName $ Failure $ "Does not conform.") []
      where
        fuelArea = burnerArea fdsData burner
        maxThreshold = 2.5

    -- |Test the non-dimensionalised ratio of the mesh surrounding a burner.
    nonDimTest :: Burner -> FDSFile -> Tree CompletedTest
    nonDimTest burner fdsData =
      let
          testName = "Non-Dimensionalised Ratio"
          q = getBurnerMaxHRR fdsData burner
          resolutions = getBurnerResolution fdsData burner
          nominalCellSizes = map getMaxCellSize resolutions
          nominalCellSize = case nominalCellSizes of
            [] -> error "no cell dimensions for burner"
            xs -> maximum xs
          getMaxCellSize (x,y,z) = maximum [x,y,z]
          charFireDiameter = (q / (ambientDensity*ambientSpecificHeat
            *ambientTemperature*(sqrt g)))**(2/5)
          ndr = charFireDiameter/nominalCellSize
          calcs =
              [ "Min. Mesh Resolution: " ++ show nominalCellSize ++ " m"
              , "Non-Dimensionalised Ratio: " ++ show ndr
              ]
          in if ndr >= 4
            then Node (CompletedTest testName $ Success $ unlines
                $ calcs ++ ["Conforms (4 <= NDR)."]) []
            else Node (CompletedTest testName $ Failure $ unlines
                $ calcs ++ ["Does not conform (4 <= NDR)."]) []
          where
              ambientDensity = 1.205
              ambientSpecificHeat = 1.005
              ambientTemperature = 293.15
              g = 9.81

    -- |Test the growth rate of a burner. A test result of Error TestResult is
    -- returned if it does not match a standard growth rate.
    growthRateTest :: Burner -> FDSFile -> Tree CompletedTest
    growthRateTest burner fdsData =
      let
          testName = "Growth Rate"
          surfs = getBurnerSurf fdsData burner
      in case surfs of
        [] -> Node (CompletedTest testName
            $ Failure "burner does not have burner surf") []
        [surf] ->
            let calcs =
                    [ "SURF: " ++ (getIdBound surf)
                    ]
            in case checkGrowthRate fdsData burner of
                  Right (growthRate, diff) ->
                      let error = diff/(growthRateToAlpha growthRate)
                          sign = if diff >=0 then "+" else ""
                      in Node (CompletedTest testName $ Success $ unlines
                        $ calcs ++
                          [ show growthRate
                          , "Difference: " ++ sign ++ show diff
                          , "Error: " ++ sign ++ show (error*100) ++ "%"
                          , "Conforms to standard fire."
                          ]) []
                  Left alpha -> Node (CompletedTest testName $ Failure
                    $ unlines $ calcs ++
                      [ "alpha: " ++ show alpha
                      , "Does not conform to standard fire."
                      ]) []
        _ -> Node (CompletedTest testName
            $ Failure "burner has multiple different burner surf types") []

    -- |Test that teh burner does not intersect with any other obstructions.
    intersectionTest :: Burner -> FDSFile -> Tree CompletedTest
    intersectionTest burner fdsData = case getBurnerId burner of
        -- TODO: we should be able to uniquely identify each OBST
        Nothing -> Node (CompletedTest testName $ Failure
            $ "Cannot test burner intersection as burner does not have a name.")
            []
        Just burnerId ->
            let
                isBurner nml = case getId nml of
                    Nothing -> False
                    Just x -> x == burnerId
                intersectsWith = filter (not . isBurner)
                    $ obstIntersectsWithOthers fdsData burner
            in if null intersectsWith
                then Node (CompletedTest testName $ Success
                    $ "Burner does not intersect with other obstructions.") []
                else Node (CompletedTest testName $ Failure
                    $ "Burner intersects wth the following obstructions: \n"
                        ++ unlines (map (\nml-> indent $ (fromMaybe "(unknown)"
                        $ getId nml)
                        {- ++ " at " ++ showSourcePose nml -}) intersectsWith))
                        []
        where
            testName = "Intersection"

showSourcePose nml = "Line " <> show (sourceLine pos) <> ", Column "
    <> show (sourceColumn pos) <> " of input file"
    where pos = nml_location nml

indent string = "--" ++ string
-- sprinklerTestsGroup :: NamelistFile -> Tree CompletedTest
-- sprinklerTestsGroup = \fdsData ->
--   let
--     testName = "Sprinklers"
--     sprinklers = getSprinklerDevcs fdsData
--     completedTests = map (sprinklerTestsIndividual fdsData) sprinklers
--   in case completedTests of
--     [] -> Node (CompletedTest testName (Warning "No burners present.")) completedTests
--     _  -> Node (CompletedTest testName (worstN completedTests)) completedTests

-- -- |Tests to apply to the various burners found in a model.
-- sprinklerTestsIndividual :: NamelistFile -> Namelist -> Tree CompletedTest
-- sprinklerTestsIndividual fdsData sprinkler =
--   let
--       testName = "Sprinklers Tests for " ++ sprinklerName
--       tests = pam tests' sprinkler
--       testResults = pam tests fdsData
--       summaryResults = worstN testResults
--   in Node (CompletedTest testName summaryResults) testResults
--   where
--     tests' :: [(Namelist -> NamelistFile -> Tree CompletedTest)]
--     tests' =
--       [ temperatureTest
--       ]
--     sprinklerName = getIDBound sprinkler

--     temperatureTest :: Namelist -> NamelistFile -> Tree CompletedTest
--     temperatureTest sprinkler fdsData =
--       let
--         testName = "Activation Temperature"
--         q = maxBurnerHRR fdsData burner
--         sF = sourceFroude q fuelArea
--       in if sF <= maxThreshold
--         then Node (CompletedTest testName $ Success $ "Conforms. " ++ show sF ++ " <= " ++ show maxThreshold) []
--         else Node (CompletedTest testName $ Failure $ "Does not conform.") []
--       where
--         fuelArea = burnerArea burner
--         maxThreshold = 2.5


----GUIDELINES
--
outputDataCoverage :: FDSFile -> Tree CompletedTest
outputDataCoverage fdsData =
    let
        testName = "Output Data Coverage"
        outputSlices = fdsFile_Slcfs fdsData
        tests =
            [ coDataCoverage
            , tempDataCoverage
            , visDataCoverage
            ]
        testResults = pam (pam tests outputSlices) fdsData
        summaryResults = worstN testResults
  in Node (CompletedTest testName summaryResults) testResults


-- genericDataCoverageTest slices =
    -- let relSlices = filter
            -- (\slice
            -- -> hasParameterValue "SPEC_ID" "carbon monoxide" slice
            -- && hasParameterValue "QUANTITY" "VOLUME FRACTION" slice
            -- ) slices
    -- in TestGroup (name ++ " Data Coverage")
        -- [ xAxisCoverage relSlices
        -- , yAxisCoverage relSlices
        -- , zAxisCoverage relSlices
        -- ]


-- |Carbon monoxide data coverage test.
coDataCoverage slices fdsData =
    let
        testName = "CO Data Coverage"
        coSlices = filter
            (\slice
            -> slcf_SPEC_ID slice == Just "CARBON MONOXIDE"
            && slcf_QUANTITY slice == Just "VOLUME FRACTION"
            ) slices
        tests =
            [ xAxisCoverage
            , yAxisCoverage
            , zAxisCoverage
            ]
        testResults = pam (pam tests coSlices) fdsData
        summaryResults = worstN testResults
    in Node (CompletedTest testName summaryResults) testResults

-- |Tempearature data coverage test.
tempDataCoverage slices fdsData =
    let
        testName = "Temperature Data Coverage"
        coSlices = filter
            (\slice
            -> slcf_QUANTITY slice == Just "TEMPERATURE"
            ) slices
        tests =
            [ xAxisCoverage
            , yAxisCoverage
            , zAxisCoverage
            ]
        testResults = pam (pam tests coSlices) fdsData
        summaryResults = worstN testResults
    in Node (CompletedTest testName summaryResults) testResults

-- |Soot Visibility data coverage test.
visDataCoverage slices fdsData =
    let
        testName = "Soot Visibiltity Data Coverage"
        coSlices = filter
            (\slice
            -> slcf_QUANTITY slice == Just ("VISIBILITY" :: String)
            ) slices
        tests =
            [ xAxisCoverage
            , yAxisCoverage
            , zAxisCoverage
            ]
        testResults = pam (pam tests coSlices) fdsData
        summaryResults = worstN testResults
    in Node (CompletedTest testName summaryResults) testResults


xAxisCoverage slices fdsData =
    let
        testName = "X Axis Coverage"
    in if not $ null $ filter (isJust . slcf_PBX) slices
      then Node (CompletedTest testName $ Success
        $ "Full X axis coverage of this value is present.") []
      else Node (CompletedTest testName $ Warning
        $ "Full X axis coverage of this value is not present.") []

yAxisCoverage slices fdsData =
    let
        testName = "Y Axis Coverage"
    in if not $ null $ filter (isJust . slcf_PBY) slices
      then Node (CompletedTest testName $ Success
        $ "Full Y axis coverage of this value is present.") []
      else Node (CompletedTest testName $ Warning
        $ "Full Y axis coverage of this value is not present.") []

zAxisCoverage slices fdsData =
    let
        testName = "Z Axis Coverage"
    in if not $ null $ filter (isJust . slcf_PBZ) slices
      then Node (CompletedTest testName $ Success
        $ "Full Z axis coverage of this value is present.") []
      else Node (CompletedTest testName $ Warning
        $ "Full Z axis coverage of this value is not present.") []
