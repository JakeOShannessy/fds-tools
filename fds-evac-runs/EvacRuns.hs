module FDSUtilities.EvacRuns where

import Control.Concurrent
import Control.Concurrent.Thread.Delay
import Control.DeepSeq
import Control.Lens
import Control.Monad

-- import Data.Colour
-- import Data.Colour.Names
import Data.Default
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector.Unboxed as V

import FDSUtilities.CompileCompress
import FDSUtilities.Data
import FDSUtilities.Parsing.OutFile
import FDSUtilities.Parsing
import FDSUtilities.Paths
import FDSUtilities.RunTimeCalc
import FDSUtilities.Simulation
import FDSUtilities.SimulationData
import FDSUtilities.Types

-- import Graphics.Rendering.Chart
-- import Graphics.Rendering.Chart.Plot
-- import Graphics.Rendering.Chart.Backend.Diagrams

import Statistics.Sample

import System.Directory
import System.Process
import System.IO
import System.FilePath
import System.FilePath.Glob as G

-- import Text.CSV
-- import Text.ParserCombinators.Parsec
-- import Text.Printf

data StopCriterion = NRuns Int | CIThreshold Int Double
-- TODO: Paths are an issue throughout all FDSUtilities code
-- TODO: Consider expanding FDSSimulation type to account for multi-run simulations.
-- TODO: Evac distribution should be lognormal or similar to remove negative vals.
-- TODO: Copy eff file and use in new simulations
-- TODO: Maintain statistical data in memory so that re-reading all of the files is not necessary
-- TODO: Test for goodness of fit.
-- TODO: Convert to use the bos statistics package (and also use vectors instead of lists)
-- |
runEvacSims :: MultiSimulation -> StopCriterion -> IO ()
runEvacSims sim stopCriterion = do
    let fdsFileRel = joinPath [casepath, casename ++ ".fds"]
    fdsFileAbs <- canonicalizePath fdsFileRel
    -- setCurrentDirectory path
    case stopCriterion of
        -- NRuns n -> mapM_ (runEvacSim fdsFileAbs) [1..n]
        NRuns n -> runEvacSimGroup fdsFileAbs [1..n]
        CIThreshold minSamples t -> runEvacSimCIThreshold sim minSamples t 1 []
    where
        casepath = getSimDir sim
        casename = getSimCHID sim

runEvacSimGroup fdsFile nums = do
    let (thisRun, remRuns) = splitAt 7 nums
    mapM_ (runEvacSim fdsFile) thisRun

runEvacSimCIThreshold :: MultiSimulation -> Int -> Double -> Int -> [Int] -> IO ()
runEvacSimCIThreshold sim minSamples ciThreshold runNum exclusions = do
    let runName = "Run" ++ (formatN runNum)
        fdsFile = joinPath [casepath, casename ++ ".fds"]
        outFilePath = joinPath [casepath, runName, casename ++ ".out"]
    putStrLn runName
    compStatus <- completionStatus outFilePath
    let completed = isComplete compStatus
    putStrLn $ "    " ++ show compStatus
    if not completed
        then do
                runEvacSim fdsFile runNum

                ciWidthMaybe <- compileCIMaybe sim exclusions
                let multiple = case ciWidthMaybe of
                        Nothing -> False
                        Just ciWidth -> True
                    finished = case ciWidthMaybe of
                        Nothing -> False
                        Just ciWidth -> (ciWidth < ciThreshold) && (runNum >= minSamples)
                if finished
                    then putStrLn "Confidence Interval Threshold met, stopping..."
                    else do
                        if multiple
                            then do
                                evacResults <- compileEvacResults sim ciThreshold exclusions Nothing Nothing
                                putStrLn evacResults
                            else return ()
                        runEvacSimCIThreshold sim minSamples ciThreshold (runNum + 1) []
        else do
            cleanEvacRun sim runName
            runEvacSimCIThreshold sim minSamples ciThreshold (runNum + 1) []
    where
        casepath = getSimDir sim
        casename = getSimCHID sim

cleanEvacRun sim runName = do
    let partPattern = G.compile "*.prt5"
        p3dPattern = G.compile "*.q"
        slcfPattern = G.compile "*.sf"
        restartPattern = G.compile "*.restart"
        szPattern = G.compile "*.sz"
        dir = joinPath [casepath, runName]
    (filesToRemove, otherFiles) <- globDir
            [ p3dPattern
            -- , partPattern
            , restartPattern
            , slcfPattern
            , szPattern
            ] dir
    mapM_ removeFile (concat filesToRemove)
    where
        casepath = getSimDir sim



-- TODO: These run function could do with some improved abstractions.
    -- that is, upgraded to utilise the FDSSimulation and related types
runEvacSim :: FilePath -> Int -> IO ()
runEvacSim fdsFile runNum = do

    cwd <- getCurrentDirectory
    let overallSimDir = takeDirectory fdsFile
        directoryName = "Run" ++ (formatN runNum)
        dir = joinPath [overallSimDir, directoryName]
    createDirectoryIfMissing False dir -- better format this string
    let localFDSFile = (joinPath [dir, (takeBaseName fdsFile) ++ ".fds"])
    copyFile fdsFile localFDSFile
    setCurrentDirectory $ dir
    -- putStrLn fdsPath
    -- putStrLn fdsFile
    -- TODO: definitely need a more robust approach then below
    let command = fdsPath ++ " " ++ (takeBaseName fdsFile) ++ ".fds"
    putStrLn command
    -- pid <- runCommand command
    -- waitForProcess pid
    runCom command
    -- forkIO (runCom command)
    setCurrentDirectory cwd

runCom com = do
    pid <- runCommand com
    waitForProcess pid
    return ()

formatN = printf "%02.0d"



confidenceInterval mean stdDev n = (mean - var, mean + var)
    where   -- 1.96 is for 95% confidence
        var = 1.96*stdDev/(sqrt (fromIntegral n))

confidenceIntervalStdDev stdDev n = (stdDev**2 - var, stdDev**2 + var)
    where   -- 1.96 is for 95% confidence
        var = 1.96*stdDev**2*(sqrt 2)/(sqrt (fromIntegral n))


compileCIMaybe :: MultiSimulation -> [Int] -> IO (Maybe Double)
compileCIMaybe simulation exclusions = do
    let fdsFile = casename ++ ".fds"
    let datPattern = G.compile ("Run*")     -- change patter to only accept numbers
    ([direcs], otherFiles) <- globDir [datPattern] casepath
    let sims = sort $ rights $ map (\x->parse parseRunDir x (takeBaseName x)) direcs
    if length sims > 1
        then (liftM Just) $ compileCI simulation exclusions
        else return Nothing
    where
        casepath = getSimDir simulation
        baseDir = casepath
        casename = getSimCHID simulation


compileCI :: MultiSimulation -> [Int] -> IO Double
compileCI simulation exclusions = do
    let fdsFile = casename ++ ".fds"
    let datPattern = G.compile ("Run*")     -- change patter to only accept numbers
    ([direcs], otherFiles) <- globDir [datPattern] casepath
    let sims = sort $ map
            (\x-> FDSSimulation
                { simDir = joinPath [baseDir, x]
                , simCHID = casename
                , runNumber = Just ((\(Right x)->x) $ parse parseRunDir x (takeBaseName x))
                }) direcs
        includedSims = filter (\x-> not ((fromJust $ runNumber x) `elem` exclusions)) sims
        excludedSims = filter (\x-> (fromJust $ runNumber x) `elem` exclusions) sims
    completedSims <- filterM simulationCompleted sims
    completeEvacTimeProps <- mapM (getCompleteEvacTime exclusions) completedSims
    let results = foldl' (\acc (runNum, time, occ, included) -> if included then ((runNum, time, occ):acc) else acc) [] completeEvacTimeProps
    let var = getProp varianceL results  -- TODO: should abort if there is only one sample
        avg = getProp averageL results
        sde = sqrt var
        ninetyFifth = percentileVal avg sde 1.645
        (ciMeanLower, ciMeanUpper) = confidenceInterval avg sde (length results)
        ciWidth = ciMeanUpper - ciMeanLower
    return ciWidth
    where
        casepath = getSimDir simulation
        baseDir = casepath
        casename = getSimCHID simulation


natNum :: Parser Int
natNum = do
    digits <- many1 (oneOf "0123456789")
    return $ read digits

parseRunDir :: Parser Int
parseRunDir = do
    string "Run"
    natNum


normaliseHistogram :: Histogram -> HistogramNormalised
normaliseHistogram histogram@(Histogram range outOfBounds bins) = HistogramNormalised range outOfBounds newBins
    where
        nSamples = foldl' (\acc (Bin _ _ n) -> acc + n) 0 bins
        newBins = map (\(Bin limLo limHi n) -> (BinNormalised limLo limHi ((fromIntegral n)/(fromIntegral nSamples)))) bins

sumNormHistVals histogram@(HistogramNormalised range outOfBounds bins) = sumVals
    where
        sumVals = foldl' (\acc (BinNormalised _ _ n) -> acc + n) 0 bins

parseSim baseDir casename b = case parse parseRunDir b (takeBaseName b) of
    Left e -> Left e
    Right x -> Right FDSSimulation
                    { simDir = joinPath [baseDir, "Run" ++ formatN x]
                    , simCHID = casename
                    , runNumber = Just x
                    }

trianglePDF min peak max x
    | x > min && x < max = if x <= peak
                            then 2*(x-min)/((max-min)*(peak-min))
                            else 2*(max-x)/((max-min)*(max-peak))
    | otherwise = 0

compileEvacResults :: MultiSimulation -> Double -> [Int] -> Maybe Double -> Maybe Double -> IO String
compileEvacResults baseSim ciThreshold calledExclusions lo hi = do
    ([direcs], otherFiles) <- globDir [datPattern] baseDir
    -- TODO: remove filenames that are not directories
    readExclusions <- readExclusions $ joinPath [baseDir, "Exclusions.txt"]
    let sims = sort $ rights $ map (parseSim baseDir casename) direcs
        exclusions = calledExclusions ++ readExclusions
        includedSims = filter (\x-> not ((fromJust $ runNumber x) `elem` exclusions)) sims
        excludedSims = filter (\x-> (fromJust $ runNumber x) `elem` exclusions) sims
    completedSims <- filterM simulationCompleted sims
    -- let completedSims = sims
    completeEvacTimeProps <- (liftM V.fromList) $ mapM (getCompleteEvacTime exclusions) completedSims
    let completeEvacTimes = V.map (\((_, time, _, _))->time) $ V.filter (\((_, time, _, included))->included) completeEvacTimeProps
    -- deepseq completeEvacTimes (print completeEvacTimes)

    -- tPreTimes <- mapM (getTPreTimes exclusions) completedSims

    -- deepseq tPreTimes (print tPreTimes)
    -- createHistograms "Pre-Movement Times" baseDir casename (V.concat tPreTimes) Nothing 60 lo hi
    -- putStrLn "Pre-Movement Histograms Completed"
    -- allExitTimes <- (liftM catMaybes) $ mapM (getExitTimesMaybe exclusions simulation) sims
    let resultsChain = map showCompleteEvacTimeProps $ V.toList completeEvacTimeProps
        results = foldl' (\acc (runNum, time, occ, included) -> if included then ((runNum, time, occ):acc) else acc) [] $ V.toList completeEvacTimeProps
        bulkResults = map (\(runNum, time, occ) -> time) results
        logBulkResults = map log bulkResults
        logCompleteEvacTimes = V.map log completeEvacTimes
    let var = variance completeEvacTimes  -- TODO: should abort if there is only one sample
        avg = mean completeEvacTimes
        sde = sqrt var
        logVar = variance logCompleteEvacTimes
        logAvg = mean logCompleteEvacTimes
        mu = logAvg
        sigma = sqrt logVar
        evacCompletionTimes = getProp id results
        evacHistogram = if not (null evacCompletionTimes) then Just (histogram 60 lo hi (V.fromList evacCompletionTimes)) else Nothing
        evacHistogramNormalised = evacHistogram >>= (Just . normaliseHistogram) :: Maybe HistogramNormalised
        ninetyFifth = percentileVal avg sde 1.645
        (ciMeanLower, ciMeanUpper) = confidenceInterval avg sde (length results)
        (ciStdDevLower, ciStdDevUpper) = confidenceIntervalStdDev sde (length results)
        ciWidth = ciMeanUpper - ciMeanLower
        satisfactory = ciWidth <= ciThreshold
        pdfLogNormFunc = logNormDist mu sigma
        (Just (HistogramNormalised (lower, upper) _ _)) = evacHistogramNormalised
        pdfVals = map (\x->(x,pdfLogNormFunc x)) [lower,1..upper]
    let results = resultsChain ++
            [ if not (null evacCompletionTimes) then "Min: " ++ (show (minimum evacCompletionTimes)) else ""
            , if not (null evacCompletionTimes) then "Max: " ++ (show (maximum evacCompletionTimes)) else ""
            , "Mean: " ++ (show avg)
            , "Variance: " ++ (show var)
            , "Standard Deviation: " ++ (show sde)
            , "logMu: " ++ (show logAvg)
            , "logSigma: " ++ (show sigma)
            , "95th percentile value: " ++ (show ninetyFifth)
            , "95% mean confidence interval: " ++ show (ciMeanLower, ciMeanUpper)
            , "95% stdDev confidence interval: " ++ show (ciStdDevLower, ciStdDevUpper)
            , (if satisfactory then "Results have sufficiently converged (CI width is " ++ show ciWidth ++ ")." else "Results have not sufficiently converged (CI width is " ++ show ciWidth ++ ").")
            -- TODO: give prediction on how many more runs need to be completed.
            ]
    case evacHistogramNormalised of
            Just x -> do; putStr "NormSum: "; putStrLn $ show $ sumNormHistVals x; renderableToFile (FileOptions (800,400) SVG M.empty) (histChartNormalised True (casename ++ " - " ++ "Egress Time Normalised Distribution") x (Just pdfLogNormFunc)) (joinPath [baseDir, (casename ++ " - " ++ "Egress Time Normalised Distribution.svg")]); return ()
            Nothing -> error "no chart"
    case evacHistogram of
            Just x -> do; renderableToFile (FileOptions (800,400) SVG M.empty) (histChart True (casename ++ " - " ++ "Egress Time Distribution") x) (joinPath [baseDir, (casename ++ " - " ++ "Egress Time Distribution.svg")]); return ()
            Nothing -> return ()
    writeFile (joinPath [baseDir, "CompiledResults.txt"]) $ unlines results
    return $ unlines results
    where
        fdsFile = fdsFilePath baseSim
        datPattern = G.compile ("Run*")     -- change pattern to only accept numbers
        casename = chidMultiSim baseSim
        baseDir = baseDirMultiSim baseSim

-- createHistograms :: String -> FilePath -> String -> V.Vector Double -> (Double -> Double) -> Int -> Maybe Double -> Maybe Double -> IO ()
createHistograms title baseDir casename values pdfFunc bins lo hi = do
    let rawHistogram = if not (V.null values) then Just (histogram bins lo hi values) else Nothing
        normalisedHistogram = rawHistogram >>= (Just . normaliseHistogram) :: Maybe HistogramNormalised
        -- (Just (HistogramNormalised (lower, upper) _ _)) = normalisedHistogram
        -- pdfVals = map (\x->(x,pdfFunc x)) [lower,1..upper]
    case normalisedHistogram of
            Just x -> do; renderableToFile (FileOptions (800,400) SVG M.empty) (histChartNormalised True (casename ++ " - " ++ title ++ " Normalised Distribution") x pdfFunc) (joinPath [baseDir, (casename ++ " - " ++ title ++ " Normalised Distribution.svg")]); return ()
            Nothing -> error "no chart"
    case rawHistogram of
            Just x -> do; renderableToFile (FileOptions (800,400) SVG M.empty) (histChart True (casename ++ " - " ++ title ++ " Distribution") x) (joinPath [baseDir, (casename ++ " - " ++ title ++ " Distribution.svg")]); return ()
            Nothing -> return ()
    return ()

whereComplete :: (FDSSimulation -> a) -> FDSSimulation -> Maybe a
whereComplete f simulation = undefined

compileEvacRunResults :: FDSSimulation -> IO ()
compileEvacRunResults simulation = do
    let exclusionFilePath = joinPath [casepath, "Exclusions.txt"]
    exclusions <- readExclusions exclusionFilePath
    exitTimes <- getExitTimes' simulation exclusions :: IO (V.Vector Double)
    simDateTime <- getModificationTime outFile
    let evacHistogram = if not (V.null exitTimes) then Just (histogram 60 Nothing Nothing exitTimes) else Nothing
        evacHistogramNormalised = evacHistogram >>= (Just . normaliseHistogram) :: Maybe HistogramNormalised

        -- logBulkRes = V.map log exitTimes :: V.Vector Double
        logExitTimes = V.map log exitTimes :: V.Vector Double
        logVar = variance logExitTimes :: Double
        logAvg = average logExitTimes
        mu = logAvg
        sigma = sqrt logVar
        pdfLogNormFunc = logNormDist mu sigma
    case evacHistogramNormalised of
        Just x -> let title = "Individual Egress Times - Normalised"
                      path = (joinPath [casepath, "IndividualNormalised.svg"])
                      produceChartFile = do
                        renderableToFile (FileOptions (800,400) SVG M.empty) (histChartNormalised True title x (Just pdfLogNormFunc)) path
                        return ()
                  in do
                    existing <- doesFileExist path
                    resultsTimeMaybe <- if existing then ((liftM Just) $ getModificationTime path) else return Nothing
                    case resultsTimeMaybe of
                        Nothing -> produceChartFile
                        Just modTime -> if modTime < simDateTime
                            then produceChartFile
                            else return ()
                    return ()
        Nothing -> error "No chart"
    -- case evacHistogram of
            -- Just x -> do; renderableToFile (FileOptions (800,600) SVG) ; return ()
            -- Nothing -> error "No chart"
    case evacHistogram of
        Just x -> let title = "Individual Egress Times"
                      path = (joinPath [casepath, "Individual.svg"])
                      produceChartFile = do
                        renderableToFile (FileOptions (800,400) SVG M.empty) (histChart True title x) path
                        return ()
                  in do
                    existing <- doesFileExist path
                    resultsTimeMaybe <- if existing then ((liftM Just) $ getModificationTime path) else return Nothing
                    case resultsTimeMaybe of
                        Nothing -> produceChartFile
                        Just modTime -> if modTime < simDateTime
                            then produceChartFile
                            else return ()
                    return ()
        Nothing -> error "No chart"
    -- TODO: allow different distributions to be specified here
    tPreTimes <- getTPreTimes exclusions simulation
    let tPreHistogram = if not (V.null exitTimes) then Just (histogram 60 Nothing Nothing exitTimes) else Nothing
        tPreHistogramNormalised = tPreHistogram >>= (Just . normaliseHistogram) :: Maybe HistogramNormalised
    case tPreHistogramNormalised of
        Just x -> do; renderableToFile (FileOptions (800,400) SVG M.empty) (histChartNormalised True "Individual Pre-Movement Times - Normalised" x Nothing) (joinPath [casepath, "TPreIndividualNormalised.svg"]); return ()
        Nothing -> error "No chart"
    case tPreHistogram of
            Just x -> do; renderableToFile (FileOptions (800,400) SVG M.empty) (histChart True "Individual Pre-Movement Times" x) (joinPath [casepath, "TPreIndividual.svg"]); return ()
            Nothing -> error "No chart"

    return ()
    where
        fdsFile = fdsFilePath simulation
        outFile = outFilePath simulation
        casename = getSimCHID simulation
        casepath = getSimDir simulation

-- produceIndividualHists

-- chiSquared :: [(Double, Double)] ->
logNormDist mu sigma x = (1/(x*sigma*(sqrt (2*pi))))*(exp (-(((log x)-mu)**2)/(2*sigma**2)))

showCompleteEvacTimeProps :: (Int, Double, Int, Bool) -> String
showCompleteEvacTimeProps (runNum, time, value, included) = "Run" ++ formatN runNum ++ " Time: " ++ printf "%.1f" time ++ "\t Occ. Remaining: " ++ show value ++ (if not included then " #Excluded" else "")
simulationCompleted :: FDSSimulation -> IO Bool
simulationCompleted simulation = runCompleted outFile
    where
        outFile = outFilePath simulation

runCompleted :: FilePath -> IO Bool
runCompleted outFilePath = do
    status <- completionStatus outFilePath
    return $ isComplete status

completionStatus :: FilePath -> IO OutStatus
completionStatus outFilePath = do
    outFilePreExisting <- doesFileExist outFilePath
    if outFilePreExisting
        then do
            parsed <- parseOutFileWithError outFilePath
            case parsed of
                Left e -> return Incomplete -- marks parsing errors as an incomplete simulation (as this is the most common case)
                Right outData -> return $ outStatus outData
        else return NotStarted

isComplete :: OutStatus -> Bool
isComplete status = case status of
                        NotStarted -> False
                        Incomplete -> False
                        StoppedByUser -> True
                        Completed -> True

getCompleteEvacTime :: [Int] -> FDSSimulation -> IO (Int, Double, Int, Bool)
getCompleteEvacTime simExclusions simulation = do
    let exclusionsPath = joinPath [casepath, "Exclusions.txt"]
    localExclusions <- readExclusions exclusionsPath
    compileEvacRunResults simulation
    (totalOccupants, exitTimes) <- getExitTimes simulation localExclusions
    let res = (n, (V.maximum exitTimes), totalOccupants - (V.length exitTimes), included)
    deepseq res (putStrLn $ showCompleteEvacTimeProps res)
    return res
    where
        Just n = runNumber simulation
        included = not $ n `elem` simExclusions
        casename = simCHID simulation
        casepath = simDir simulation
        runName = "Run" ++ (formatN n)

getCompleteEvacTime' simExclusions simulation = do
    vals <- getCompleteEvacTime simExclusions simulation
    return $ (\(_, ts, _, _)->ts) vals

readExclusions exclusionFilePath = do
    exclusionsExist <- doesFileExist exclusionFilePath
    if exclusionsExist
        then do
            raw <- readFile exclusionFilePath
            let ls = lines raw
            return $ map read ls
        else return []



getProp f res = f times
    where
        (_, times, _) = unzip3 res

        -- 95th percentile = 1.645
percentileVal mean stdDeviation z = stdDeviation * z + mean

average = averageL . V.toList
averageL ls = (sum ls) / (fromIntegral (length ls))

-- varianceT = varianceL . V.toList
varianceL ls = averageL $ map (\x-> (x - mean)**2) ls
    where
        mean = averageL ls


getExitTimes :: FDSSimulation -> [Int] -> IO (Int, V.Vector Double )
getExitTimes simulation exclusions = do
    simDateTime <- getModificationTime evacOutFP
    resultsExist <- doesFileExist resultsFP
    resultsTimeMaybe <- if resultsExist then ((liftM Just) $ getModificationTime resultsFP) else return Nothing
    case resultsTimeMaybe of
        Nothing -> exitTimesFromSimulation evacOutFP resultsFP
        Just modTime -> if modTime < simDateTime
            then exitTimesFromSimulation evacOutFP resultsFP
            else exitTimesFromResultsFile resultsFP
    where
        evacOutFP = evacOutFilePath simulation
        resultsFP = joinPath [simDir, simCHID ++ "_UnfilteredExitProps.txt"]
        simDir = getSimDir simulation
        simCHID = getSimCHID simulation
        exitTimesFromSimulation evacOutFilePath resultsFilePath = do
            (EvacOutData _ _ _ initialAgentProps agentExitProps) <- parseFileFDS evacOutFileParser evacOutFilePath
            let totalOccupants = length initialAgentProps
                filteredExitProps = filter (\(AgentExitProps id _ _) -> not (id `elem` exclusions)) agentExitProps
                exitTimes = map (\(AgentExitProps _ exitTime _) -> exitTime) filteredExitProps
            if length exitTimes == 0 then error ("No exit times. >> " ++ resultsFP) else return ()
            writeFile resultsFilePath (unlines $ (show totalOccupants):(map show agentExitProps))
            return (totalOccupants, V.fromList exitTimes)
        exitTimesFromResultsFile resultsFilePath = do
            raw <- readFile resultsFilePath
            let res = lines raw
                totalOccupants = read $ head res
                agentExitProps = map read $ tail res
                filteredExitProps = filter (\(AgentExitProps id _ _) -> not (id `elem` exclusions)) agentExitProps
                exitTimes = map (\(AgentExitProps _ exitTime _) -> exitTime) filteredExitProps
            return (totalOccupants, V.fromList exitTimes)


getTPreTimes :: [Int] -> FDSSimulation -> IO (V.Vector Double)
getTPreTimes exclusions simulation = do
    simDateTime <- getModificationTime evacOutFP
    resultsExist <- doesFileExist resultsFP
    resultsTimeMaybe <- if resultsExist then ((liftM Just) $ getModificationTime resultsFP) else return Nothing
    case resultsTimeMaybe of
        Nothing -> tPreTimesFromSimulation evacOutFP resultsFP
        Just modTime -> if modTime < simDateTime
            then tPreTimesFromSimulation evacOutFP resultsFP
            else tPreTimesFromResultsFile resultsFP
    where
        evacOutFP = evacOutFilePath simulation
        resultsFP = joinPath [simDir, simCHID ++ "_UnfilteredInitialAgentProps.txt"]
        simDir = getSimDir simulation
        simCHID = getSimCHID simulation
        tPreTimesFromSimulation evacOutFilePath resultsFilePath = do
            (EvacOutData _ _ _ initialAgentProps agentExitProps) <- parseFileFDS evacOutFileParser evacOutFilePath
            let totalOccupants = length initialAgentProps
                filteredinitialAgentProps = filter (\(InitialAgentProps id _ _ _ _ _ _ _ _) -> not (id `elem` exclusions)) initialAgentProps
                tPreTimes = map (\(InitialAgentProps _ _ tPre _ _ _ _ _ _) -> tPre) filteredinitialAgentProps
            if length tPreTimes == 0 then error ("No pre-movement times. >> " ++ resultsFP) else return ()
            writeFile resultsFilePath (unlines $ map show initialAgentProps)
            return $ V.fromList tPreTimes
        tPreTimesFromResultsFile resultsFilePath = do
            raw <- readFile resultsFilePath
            let res = lines raw
                initialAgentProps = map read res
                totalOccupants = length initialAgentProps
                filteredinitialAgentProps = filter (\(InitialAgentProps id _ _ _ _ _ _ _ _) -> not (id `elem` exclusions)) initialAgentProps
                tPreTimes = map (\(InitialAgentProps _ _ tPre _ _ _ _ _ _) -> tPre) filteredinitialAgentProps
            return $ V.fromList tPreTimes


getExitTimes' :: FDSSimulation -> [Int] -> IO (V.Vector Double)
getExitTimes' simulation exclusions = do
    (totalOccupants, exitTimes) <- getExitTimes simulation exclusions
    return exitTimes




