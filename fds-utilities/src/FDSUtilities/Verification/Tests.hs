{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.Verification.Tests where

import FDSUtilities.Simulation
import FDSUtilities.Parsing
import FDSUtilities.FDSFileFunctions
import FDSUtilities.Types.Assess
import FDSUtilities.Types
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

-- CONSIDER: There are multiple ways to get the slice files: smvFile or directory search
-- sliceFileFilePaths :: FDSSimulation -> [String]
-- sliceFileFilePaths simulation =
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
    if vp then putStrLn "Validation suite passed." else putStrLn "Validation suite failed."
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
      ]
    testResults = pam tests fdsData
    summaryResults = worstN testResults
  in Node (CompletedTest testName summaryResults) testResults

-- ensure that everage flow device is covered by a flow rate device
flowCoverage fdsData =
    let
        testName = "Flow Coverage Test"
        -- it is also possible that other objects (such as OBST have flow)
        vents = findNamelists fdsData "VENT"
        obsts = findNamelists fdsData "OBST"
        surfs = findNamelists fdsData "SURF"
        -- vents which may have a flow
        ventsWithFlows = filter (ventHasFlow fdsData) vents
        -- obsts that have surfaces with flows
        obstWithFlows = filter (\flowable -> maybe False id (surfHasFlow <$> getSurf surfs flowable)) vents
        -- for each of the vents, ensure there is a flow device with the same dimensions
        -- find those which do not
        notCovered =  filter (not . (hasFlowDevc fdsData)) ventsWithFlows
    in if null notCovered
            then Node (CompletedTest testName $ Success $ "All flow devices have devcs.") []
            else Node (CompletedTest testName $ Failure $ unlines $ map formatRes notCovered) []
    where
        formatRes nml = "Flow object " ++ getIdString nml ++ " does not have a flow tracking devices.\n    " ++ T.unpack (pprint nml)

leakage fdsData =
    let
        testName = "Leakage Implementation Test"
        parts = findNamelists fdsData "PART"
        screenParts = filter isScreenPart parts
        isScreenPart part = case getParameterMaybe part "DRAG_LAW" of
            Nothing -> False
            Just (ParString "SCREEN") -> True
            Just _ -> False
        hasInertOrDefaultSurf nml = case getParameterMaybe nml "SURF_ID" of
            Nothing -> True
            Just (ParString "INERT") -> True
            _ -> False
    in if all (not . hasInertOrDefaultSurf) screenParts
            then Node (CompletedTest testName $ Success $ "No inert screens.") []
            else Node (CompletedTest testName $ Failure $ "PART uses the SCREEN drag law, but uses an INERT surface.") []

-- |Ensure that no devices are stuck in solids.
devicesTest :: NamelistFile -> Tree CompletedTest
devicesTest fdsData =
    let
        testName = "Devices Stuck in Solids Test"
        stuckDevices = filter (fromMaybe False . stuckInSolid fdsData) $ getDevices fdsData
    in if null stuckDevices
        then Node (CompletedTest testName $ Success $ "No stuck devices.") []
        else Node (CompletedTest testName $ Failure $ unlines $ map formatRes stuckDevices) []
    where
        formatRes nml = "Device " ++ getIdString nml ++ " is placed within a solid obstruction.\n    " ++ T.unpack (pprint nml)

ventHasFlow :: NamelistFile -> Namelist -> Bool
ventHasFlow fdsData vent =
    let
        hvacs = findNamelists fdsData "HVAC"
        linkedHVACs :: [Namelist]
        linkedHVACs = filter (\nml-> (maybe False id ((==) <$> getParameterMaybe nml "VENT_ID" <*> getParameterMaybe vent "ID")) || (maybe False id ((==) <$> getParameterMaybe nml "VENT2_ID" <*> getParameterMaybe vent "ID"))) hvacs
        isHVAC :: Bool
        isHVAC = (not . null) linkedHVACs
    in isHVAC || (maybe False id (surfHasFlow <$> getSurf (findNamelists fdsData "SURF") vent))

-- |Take the xb dimensions of a vent and see if there is a flow vent with the
-- matching dimensions, or a device that references it as a duct node.
hasFlowDevc :: NamelistFile -> Namelist ->  Bool
hasFlowDevc fdsData namelist =
    let
        devcs = filter (\nml->getParameterMaybe nml "QUANTITY" == (Just (ParString "VOLUME FLOW"))) (findNamelists fdsData "DEVC")
        trackingFlowMatchingXB = any (matchXBs namelist) devcs
        -- get all the devices
        allDevcs = findNamelists fdsData "DEVC"
        -- take only the devices which have a "DUCT_ID" parameter
        ductIDDevices = filter (\nml->isJust $ getParameterMaybe nml "DUCT_ID") allDevcs
        -- take only the devices where the "DUCT_ID" matches the flowing namelist
        relevantDuctIDDevices = filter (\nml-> (Just True) == (do
            ductId <- getParameterMaybe nml "DUCT_ID"
            flowId <- getParameterMaybe namelist "ID"
            pure (ductId == flowId))) ductIDDevices
        -- take only the devices that measure "DUCT VOLUME FLOW", and check that
        -- the list is not null
        trackingFlowViaDuctID = not $ null $ filter (\nml->getParameterMaybe nml "QUANTITY" == (Just (ParString "DUCT VOLUME FLOW"))) allDevcs
    in trackingFlowMatchingXB || trackingFlowViaDuctID
    -- where
    --     matchingDuctId namelist devc =
    --         let res = do
    --             ductId <- getParameterMaybe devc "DUCT_ID"
    --             flowId <- getParameterMaybe namelist "ID"
    --             pure (ductId == flowId)
    --         in res == (Just True)
    --     isDuctFlowTrackingDevc devc = all
    --         -- Check that the flow device has a "DUCT_ID" matching our flowing
    --         -- object.
    --         [ matchingDuctId namelist devc
    --         -- Check that the device is measuring "DUCT VOLUME FLOW"
    --         , getParameterMaybe nml "QUANTITY" == (Just (ParString "DUCT VOLUME FLOW"))
    --         ]

-- Check one obstruction and determine if it intersects any other namelists.
obstIntersectsWithOthers :: NamelistFile -> Namelist -> [Namelist]
obstIntersectsWithOthers fdsData namelist =
    let
        obsts = (findNamelists fdsData "OBST")
    in filter (nmlIntersect namelist) obsts

matchXBs :: Namelist -> Namelist -> Bool
matchXBs nmlA nmlB = maybe False id
    ((==)
        <$> getParameterMaybe nmlA "XB"
        <*> getParameterMaybe nmlB "XB")

getSurf :: [Namelist] -> Namelist -> Maybe Namelist
getSurf surfs nml = do
    surfId <- getParameterMaybe nml "SURF_ID"
    let ss = filter (\surf->(getParameterMaybe surf "ID") == (Just surfId)) surfs
    case ss of
        [s] -> pure s
        [] -> Nothing
        _ -> error ("Multiple matching surfs: " ++ show surfId)

surfHasFlow :: Namelist -> Bool
surfHasFlow surf = any isJust
    [ getParameterMaybe surf "MLRPUA"
    , getParameterMaybe surf "MASS_FLUX"
    , getParameterMaybe surf "MASS_FLUX_TOTAL"
    , getParameterMaybe surf "MASS_FLUX_VAR"
    , getParameterMaybe surf "HRRPUA"
    , getParameterMaybe surf "VEL"
    , getParameterMaybe surf "VEL_T"
    , getParameterMaybe surf "VOLUME_FLOW"
    ]

meshOverlapTests fdsData =
    let
        testName = "Mesh Overlap Test"
        meshes = getMeshes fdsData
        combinations :: [(Namelist, Namelist)]
        combinations = combinationPairs meshes
        testPair (m1,m2) = nmlIntersect m1 m2
        results :: [(Namelist, Namelist)]
        results = foldl' (\failed (m1,m2) -> if nmlIntersect m1 m2 then (m1,m2):failed else failed) [] combinations
    in if null results
            then Node (CompletedTest testName $ Success $ "No meshes overlap.") []
            else Node (CompletedTest testName $ Failure $ unlines $ map formatRes results) []
    where
        formatRes (m1,m2) = "Meshes " ++ getIdString m1 ++ " and " ++ getIdString m2 ++ " overlap.\n    " ++ show (getXB m1) ++ "\n    " ++ show (getXB m2)

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
              [reac] = findNamelists fdsData "REAC"
              testResults :: [Tree CompletedTest]
              testResults = pam (pam tests reac) fdsData
              summaryResults :: TestResult
              summaryResults = worstN testResults
          in (summaryResults, testResults) :: (TestResult, [Tree CompletedTest])
  in Node (CompletedTest testName summaryResults) testResults
  where
      specified :: NamelistFile -> Tree CompletedTest
      specified fdsData =
          let
              testName = "Specified"
          in case findNamelists fdsData "REAC" of
            []  -> Node (CompletedTest testName $ Failure "No REAC namelist specified.") []
            [x] -> Node (CompletedTest testName $ Success "REAC namelist specified.") []
            _   -> Node (CompletedTest testName $ Failure "Multiple REAC namelists specified.") []

      sootYield :: Namelist -> NamelistFile -> Tree CompletedTest
      sootYield reac fdsData =
          let
              propName = "Soot Yield"
              testName = "Soot Yield"
              possibleValues = [0.07, 0.1] :: [Double]
              nValueMaybe = getParameterMaybe reac "SOOT_YIELD"
          in case nValueMaybe of
            Nothing -> Node (CompletedTest testName $ Failure $ propName ++ " value not specified.") []
            Just (ParDouble nValue) -> if nValue `elem` possibleValues
              then Node (CompletedTest testName $ Success $ propName ++ " is " ++ show nValue ++ ".") []
              else Node (CompletedTest testName $ Failure $ propName ++ " is " ++ show nValue ++ " not one of " ++ show possibleValues ++ ".") []
            Just _ -> Node (CompletedTest testName $ Failure $ propName ++ " is not a number.") []

      coYield reac fdsData =
        let
            propName = "CO Yield"
            testName = "CO Yield"
            val = 0.05 :: Double
            nValueMaybe = getParameterMaybe reac "CO_YIELD"
        in case nValueMaybe of
          Nothing -> Node (CompletedTest testName $ Failure $ propName ++ " value not specified.") []
          Just (ParDouble nValue) -> if nValue == val
            then Node (CompletedTest testName $ Success $ propName ++ " is " ++ show val ++ ".") []
            else Node (CompletedTest testName $ Failure $ propName ++ " is " ++ show nValue ++ " not " ++ show val ++ ".") []
          Just _ -> Node (CompletedTest testName $ Failure $ propName ++ " is not a number.") []



-- |Test that the MISC properties are reasonable.
miscTests :: NamelistFile -> Tree CompletedTest
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
              [misc] = findNamelists fdsData "MISC"
              testResults :: [Tree CompletedTest]
              testResults = pam (pam tests misc) fdsData
              summaryResults :: TestResult
              summaryResults = worstN testResults
          in (summaryResults, testResults) :: (TestResult, [Tree CompletedTest])
  in Node (CompletedTest testName summaryResults) testResults
  where
      specified :: NamelistFile -> Tree CompletedTest
      specified fdsData =
          let
              testName = "Specified"
          in case findNamelists fdsData "MISC" of
            []  -> Node (CompletedTest testName $ Failure "No MISC namelist specified.") []
            [x] -> Node (CompletedTest testName $ Success "MISC namelist specified.") []
            _   -> Node (CompletedTest testName $ Failure "Multiple MISC namelists specified.") []

      visibilityFactor :: Namelist -> NamelistFile -> Tree CompletedTest
      visibilityFactor misc fdsData =
          let
              testName = "Visiblity Factor"
              nValueMaybe = getParameterMaybe misc "VISIBILITY_FACTOR"
          in case nValueMaybe of
            Nothing -> Node (CompletedTest testName $ Failure $ "Value not specified.") []
            Just nValue -> case getDouble nValue of
              3 -> Node (CompletedTest testName $ Success $ "Value: " ++ show nValue ++ ".") []
              8 -> Node (CompletedTest testName $ Success $ "Value: " ++ show nValue ++ ".") []
              _ -> Node (CompletedTest testName $ Failure $ "Value of " ++ show nValue ++ " not validated.") []
            -- TODO: consider not a number Just _ -> Node (CompletedTest testName $ Failure $ "Value is not a number.") []

      maximumVisibility :: Namelist -> NamelistFile -> Tree CompletedTest
      maximumVisibility misc fdsData =
          let
              testName = "Maximum Visibility"
              nValueMaybe = getParameterMaybe misc "MAXIMUM_VISIBILITY"
          in case nValueMaybe of
            Nothing -> Node (CompletedTest testName $ Failure $ "Value not specified.") []
            Just (ParDouble nValue) -> if nValue <= 100
              then Node (CompletedTest testName $ Success $ "Value: " ++ show nValue ++ ".") []
              else Node (CompletedTest testName $ Success $ "Value of " ++ show nValue ++ " may result in clipped output.") []
            Just _ -> Node (CompletedTest testName $ Failure $ "Value is not a number.") []


-- |Test that the DUMP properties are reasonable.
dumpTests :: NamelistFile -> Tree CompletedTest
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
              [dump] = findNamelists fdsData "DUMP"
              testResults :: [Tree CompletedTest]
              testResults = pam (pam tests dump) fdsData
              summaryResults :: TestResult
              summaryResults = worstN testResults
          in (summaryResults, testResults) :: (TestResult, [Tree CompletedTest])
  in Node (CompletedTest testName summaryResults) testResults
  where
--       simEndTime = getSimEndTime fdsData
      --TODO: actually get the simulation end time
      (tStart, tEnd) = getSimTimes fdsData
      simInterval = tEnd - tStart

      specified :: NamelistFile -> Tree CompletedTest
      specified fdsData =
          let
              testName = "Specified"
          in case findNamelists fdsData "DUMP" of
            []  -> Node (CompletedTest testName $ Failure "No DUMP namelist specified.") []
            [x] -> Node (CompletedTest testName $ Success "DUMP namelist specified.") []
            _   -> Node (CompletedTest testName $ Failure "Multiple DUMP namelists specified.") []

      dt_restart :: Namelist -> NamelistFile -> Tree CompletedTest
      dt_restart dump fdsData =
          let
              testName = "Restart Interval"
              nValueMaybe = getParameterMaybe dump "DT_RESTART"
          in case nValueMaybe of
            Nothing -> Node (CompletedTest testName $ Failure $ "Value not specified.") []
            Just (ParDouble nValue) ->  Node (CompletedTest testName $ Success $ "Value: " ++ show nValue ++ ".") []
            Just (ParInt nValue) ->  Node (CompletedTest testName $ Success $ "Value: " ++ show nValue ++ ".") []
            Just x -> Node (CompletedTest testName $ Failure $ "Value (" ++ show x ++ ") is not a number.") []

      nframes :: Namelist -> NamelistFile -> Tree CompletedTest
      nframes dump fdsData =
          let
              testName = "Number of Frames"
              nValueMaybe = getParameterMaybe dump "NFRAMES"
          in case nValueMaybe of
            Nothing -> Node (CompletedTest testName $ Failure $ "Value not specified.") []
            Just (ParInt nValue) -> if (mod (round simInterval :: Int) nValue)  == 0 -- TODO: check that simTime is whole number
              then Node (CompletedTest testName $ Success $ "Value: " ++ show nValue ++ ".") []
              else Node (CompletedTest testName $ Success $ "Value of " ++ show nValue ++ " may result in clipped output.") []
            Just _ -> Node (CompletedTest testName $ Failure $ "Value is not a number.") []

worstN :: [Tree CompletedTest] -> TestResult
worstN ts = stripString $ worst $ map (\(Node (CompletedTest _ res) _) -> res) ts
worst :: [TestResult] -> TestResult
worst [] = error "No results to summarise."
worst results = last $ sort results

stripString res = case res of
            (Success _) -> Success ""
            (Warning _) -> Warning ""
            (Failure _) -> Failure ""


burnerTestsGroup :: NamelistFile -> Tree CompletedTest
burnerTestsGroup = \fdsData ->
  let
    testName = "Burners"
    burners = getBurners fdsData
    completedTests = map (burnerTestsIndividual fdsData) burners
  in case completedTests of
    [] -> Node (CompletedTest testName (Warning "No burners present.")) completedTests
    _  -> Node (CompletedTest testName (worstN completedTests)) completedTests

-- |Tests to apply to the various burners found in a model.
burnerTestsIndividual :: NamelistFile -> Namelist -> Tree CompletedTest
burnerTestsIndividual fdsData burner =
  let
      testName = "Burner Tests for " ++ burnerName
      tests = pam tests' burner
      testResults = pam tests fdsData
      summaryResults = worstN testResults
  in Node (CompletedTest testName summaryResults) testResults
  where
    tests' :: [(Namelist -> NamelistFile -> Tree CompletedTest)]
    tests' =
      [ sourceFroudeTest
      , nonDimTest
      , growthRateTest
      , intersectionTest
      ]
    burnerName = getIDBound burner

    sourceFroudeTest :: Namelist -> NamelistFile -> Tree CompletedTest
    sourceFroudeTest burner fdsData =
      let
        testName = "Source Froude Number"
        q = maxBurnerHRR fdsData burner
        sF = sourceFroude q fuelArea
      in if sF <= maxThreshold
        then Node (CompletedTest testName $ Success $ "Conforms. " ++ show sF ++ " <= " ++ show maxThreshold) []
        else Node (CompletedTest testName $ Failure $ "Does not conform.") []
      where
        fuelArea = burnerArea fdsData burner
        maxThreshold = 2.5


    -- |Test the non-dimensionalised ratio of the mesh surrounding a burner.
    nonDimTest :: Namelist -> NamelistFile -> Tree CompletedTest
    nonDimTest burner fdsData =
      let
          testName = "Non-Dimensionalised Ratio"
          q = maxBurnerHRR fdsData burner
          resolutions = getObstResolutions fdsData burner
          nominalCellSizes = map getMaxCellSize resolutions
          nominalCellSize = case nominalCellSizes of
            [] -> error "no cell dimensions for burner"
            xs -> maximum xs
          getMaxCellSize (x,y,z) = maximum [x,y,z]
          charFireDiameter = (q / (ambientDensity*ambientSpecificHeat*ambientTemperature*(sqrt g)))**(2/5)
          ndr = charFireDiameter/nominalCellSize
          calcs =
              [ "Min. Mesh Resolution: " ++ show nominalCellSize ++ " m"
              , "Non-Dimensionalised Ratio: " ++ show ndr
              ]
          in if ndr >= 4
            then Node (CompletedTest testName $ Success $ unlines $ calcs ++ ["Conforms (4 <= NDR)."]) []
            else Node (CompletedTest testName $ Failure $ unlines $ calcs ++ ["Does not conform (4 <= NDR)."]) []
          where
              ambientDensity = 1.205
              ambientSpecificHeat = 1.005
              ambientTemperature = 293.15
              g = 9.81

    -- |Test the growth rate of a burner. A test result of Error TestResult is returned if it does not match a standard growth rate.
    growthRateTest :: Namelist -> NamelistFile -> Tree CompletedTest
    growthRateTest burner fdsData =
      let
          testName = "Growth Rate"
          surf = getBurnerSurf fdsData burner
          calcs =
              [ "SURF: " ++ (getIDBound surf)
              ]
      in  if hasParameter surf "TAU_Q"
              then case checkGrowthRate fdsData burner of
                  Right (growthRate, diff) ->
                      let error = diff/(growthRateToAlpha growthRate)
                          sign = if diff >=0 then "+" else ""
                      in Node (CompletedTest testName $ Success $ unlines $ calcs ++
                          [ show growthRate
                          , "Difference: " ++ sign ++ show diff
                          , "Error: " ++ sign ++ show (error*100) ++ "%"
                          , "Conforms to standard fire."
                          ]) []
                  Left alpha -> Node (CompletedTest testName $ Failure $ unlines $ calcs ++
                      [ "alpha: " ++ show alpha
                      , "Does not conform to standard fire."
                      ]) []
              else Node (CompletedTest testName $ Warning $ "Not defined using TAU_Q, max. HRR is " ++ show (maxBurnerHRR fdsData burner) ++ " kW" ) []

    -- |Test that teh burner does not intersect with any other obstructions.
    intersectionTest :: Namelist -> NamelistFile -> Tree CompletedTest
    intersectionTest burner fdsData = case getIdStringMaybe burner of
        -- TODO: we should be able to uniquely identify each OBST
        Nothing -> Node (CompletedTest testName $ Failure $ "Cannot test burner intersection as burner does not have a name.") []
        Just burnerId ->
            let
                isBurner nml = case getIdStringMaybe nml of
                    Nothing -> False
                    Just x -> x == burnerId
                intersectsWith = filter (not . isBurner) $ obstIntersectsWithOthers fdsData burner
            in if null intersectsWith
                then Node (CompletedTest testName $ Success $ "Burner does not intersect with other obstructions.") []
                else Node (CompletedTest testName $ Failure
                    $ "Burner intersects wth the following obstructions: \n"
                        ++ unlines (map (\nml-> indent $ (fromMaybe "(unknown)" $ getIdStringMaybe nml) ++ " at " ++ showSourcePose nml) intersectsWith)) []
        where
            testName = "Intersection"

showSourcePose nml = "Line " <> show (sourceLine pos) <> ", Column " <> show (sourceColumn pos) <> " of input file"
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
outputDataCoverage :: NamelistFile -> Tree CompletedTest
outputDataCoverage fdsData =
    let
        testName = "Output Data Coverage"
        outputSlices = findNamelists fdsData "SLCF"
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
            -> hasParameterValue "SPEC_ID" ("CARBON MONOXIDE" :: T.Text) slice
            && hasParameterValue "QUANTITY" ("VOLUME FRACTION" :: T.Text) slice
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
            -> hasParameterValue "QUANTITY" ("TEMPERATURE" :: T.Text) slice
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
            -> hasParameterValue "QUANTITY" ("VISIBILITY" :: T.Text) slice
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
    in if not $ null $ filter (\slice -> hasParameter slice ("PBX" :: T.Text)) slices
      then Node (CompletedTest testName $ Success $ "Full X axis coverage of this value is present.") []
      else Node (CompletedTest testName $ Warning $ "Full X axis coverage of this value is not present.") []

yAxisCoverage slices fdsData =
    let
        testName = "Y Axis Coverage"
    in if not $ null $ filter (\slice -> hasParameter slice ("PBY" :: T.Text)) slices
      then Node (CompletedTest testName $ Success $ "Full Y axis coverage of this value is present.") []
      else Node (CompletedTest testName $ Warning $ "Full Y axis coverage of this value is not present.") []

zAxisCoverage slices fdsData =
    let
        testName = "Z Axis Coverage"
    in if not $ null $ filter (\slice -> hasParameter slice ("PBZ" :: T.Text)) slices
      then Node (CompletedTest testName $ Success $ "Full Z axis coverage of this value is present.") []
      else Node (CompletedTest testName $ Warning $ "Full Z axis coverage of this value is not present.") []
