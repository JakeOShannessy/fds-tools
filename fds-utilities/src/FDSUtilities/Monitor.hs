{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}
module FDSUtilities.Monitor where

import Control.Exception as E
import Control.Lens

import Data.Default
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V

import FDSUtilities.Plot
import FDSUtilities.Parsing.SimulationData
import FDSUtilities.Types
import FDSUtilities.Types.Monitor

import System.Directory
import System.FilePath

-- TODO: implement only updating when the file changes
-- TODO: send an email when the HRR deviates too much from
--       the standard curves, this would be really convenient
--       but would need a good statistical basis
-- TODO: catch errors and skip bad out files
-- TODO: add day name and more colloquial data to predicted end time
-- TODO: add history of prediction to track prediction reliability
-- TODO: scan geometry file and flag warnings of devices inside solids.
-- TODO: option to lock axes across multiple sims in a project
-- TODO: allow the outputting of javascript (d3) charts

-- |The default MonitorDict.
instance Default MonitorDict where
    def = MonitorDict $ M.fromList
        [ ("hrr", monVarName .~ "HRR"
            $ monVarSource .~ CSVOutput [("kW","HRR")]
            $ monVarMethod .~ TimeChart eurocodeHRRChartConfig
            $ def)
        , ("maxCFL", monVarName .~ "Maximum CFL Numbers"
            $ monVarSource .~ (RunTimeOutput $ getOutLocProp "Max CFL number")
            $ monVarMethod .~ TimeChart (chartYLabel .~ (Just "CFL Number") $ def)
            $ def)
        , ("maxDiv", monVarName .~ "Maximum Divergence"
            $ monVarSource .~ (RunTimeOutput $ getOutLocProp "Max divergence")
            $ monVarMethod .~ TimeChart (chartYLabel .~ (Just "Divergence") $ def)
            $ def)
        , ("minDiv", monVarName .~ "Minimum Divergence"
            $ monVarSource .~ (RunTimeOutput $ getOutLocProp "Min divergence")
            $ monVarMethod .~ TimeChart (chartYLabel .~ (Just "Divergence") $ def)
            $ def)
        , ("timestep", monVarName .~ "Timestep"
            $ monVarSource .~ (RunTimeOutput $ getOutPropTStep stepSize)
            $ monVarMethod .~ TimeChart (chartYLabel .~ (Just "Timestep (s)") $ def)
            $ def)
        ]


-- |Provides a message if the HRR deviates too much
checkHRR :: Double -> Double -> GrowthRate -> Double -> DataVectorPair Double Double -> IO ()
checkHRR ignore threshold growthRate capTime (DataVectorPair (DataVector _ _ times) (DataVector _ _ hrrs))
    | null violations = return ()
    | otherwise = putStrLn $ "HRR deviates by " ++ show (maxViolation*100) ++ "% at " ++ show maxViolationTime ++ " s"
    where
        violations = filter (\(t,h) -> (h > threshold) && (t > ignore) && (not $ isNaN h) && (not $ isInfinite t)) $ map checkHRRPoint dat
        (maxViolationTime, maxViolation) = maximumBy (\(t1,h1) (t2,h2) -> compare h1 h2) violations
        dat = zip (V.toList times) (V.toList hrrs)
        checkHRRPoint :: (Double, Double) -> (Double, Double)
        checkHRRPoint (t,h) = (t, hrrDeviation t h)
        hrrDeviation :: Double -> Double -> Double
        hrrDeviation time outputted = deviation outputted (hrrCapped growthRate capTime time)

-- |Test if a value deviates from a reference value by a certain percentage.
deviates
    :: Double -- ^Tested number
    -> Double -- ^Reference number
    -> Double -- ^Threshold (e.g. 0.1 for 10%)
    -> Bool
deviates test ref threshold = ((abs (test - ref))/ref) > threshold
deviation test ref = ((abs (test - ref))/ref)

-- |The info for vectors is the list of vector names associated with each set of units.
mergeInfo :: M.Map String [(String,String)] -> (String, String) -> M.Map String [(String,String)]
mergeInfo infoMap (name, units) = M.insertWith (++) units [(units, name)] infoMap

-- |A naive derivative.
basicDerivative :: (DataVectorPair Double Double) -> (DataVectorPair Double Double)
basicDerivative (DataVectorPair (DataVector xName xUnits xValues) (DataVector yName yUnits yValues))
    = (DataVectorPair (DataVector xName xUnits xValues) (DataVector (yName ++ " Derivative") (yUnits ++ "/s") newY))
    where
        newY = V.map deriv (V.fromList [0..(V.length xValues - 1)])
        deriv i
            | i == 0    = 0
            | otherwise = yDelta / xDelta
            where
                xDelta = (xValues V.! (i)) - (xValues V.! (i-1))
                yDelta = (yValues V.! (i)) - (yValues V.! (i-1))

-- TODO: choose different plot type based on units, e.g. "status" should result in activation bars.
-- |Convert an infoMap to a monitor list.
infoMapToMonitorList infoMap = filter (\x->(_monVarName x) /= "unnamed") $ map convKey $ M.toList infoMap
    where
        convKey (units,names) = monVarName .~ (unitNamer units)
                        $ monVarSource .~ CSVOutput names
                        $ monVarMethod .~ TimeChart def
                        $ def

-- |Produce a chart name based on the units used.
unitNamer :: String -> String
unitNamer units = case units of
    "%/m" -> "Obscuration Devices"
    "C" -> "Temperature Devices"
    "kW" -> "Power Devices"
    "kg/s" -> "Mass Flux Properties"
    "status" -> "Activation Status"
    "AgentsInside" -> "Agents Inside"
    "AgentsInsideMesh" -> "Agents Inside Mesh"
    "ExitCounter" -> "Exit Count"
    "TargetExitCounter" -> "Targeted Exit"
    _ -> "unnamed"

-- |Produce monitors that show each stage of the simulation. This is for the purposes of animating the charts.
produceProgressiveMonitor :: FilePath -> FDSSimulation -> [DataVectorPair Double Double] -> OutData -> Monitor -> IO [FilePath]
produceProgressiveMonitor destDir sim dList outData monitor = do
    let monitorName = monitor ^. monVarName
        method = monitor ^. monVarMethod
        transform = monitor ^. monVarTransform
        vecs = map transform $ getMonitorData dList outData $ monitor ^. monVarSource
        vecLength = V.length $ dataVectorValues $ xVector $ head vecs -- TODO: this assumes that all the vectors are the same length
        -- fullAction = monVarFullAction monitor
    -- fullAction vecs
    putStr "Charting: "
    putStrLn monitorName
    let newDestDir = joinPath [destDir, "Progressive"]
        vecProgression = map (\n->(n,map (\v->trimVec v n) vecs)) [0..vecLength - 1]
    case vecs of
        [] -> return [] -- error $ "No vectors, skipping chart " ++ show (map (dataVectorName.yVector) dList)-- TODO: How should this fail?
            -- print "No vecs"
            -- print (monitor ^. monVarSource)
            -- print (map (dataVectorName.yVector) dList)
            -- return [] --
        x  -> case method of
                TimeChart conf -> do
                    paths <- mapM (\(n,vs)->produceChart destDir conf vs (simCHID sim ++ " - " ++ monitorName) (monitorName ++ show n)) vecProgression
                    return $ concat paths

-- |Given a 'DataVectorPair', trim it to a certain length.
trimVec :: DataVectorPair Double Double -> Int -> DataVectorPair Double Double
trimVec (DataVectorPair (DataVector xName xUnits xValues) (DataVector yName yUnits yValues)) n
    = (DataVectorPair (DataVector xName xUnits (V.slice 0 n xValues)) (DataVector yName yUnits (V.slice 0 n yValues)))


produceMonitor :: FilePath -> FDSSimulation -> [DataVectorPair Double Double] -> OutData -> Monitor -> IO [FilePath]
produceMonitor destDir sim dList outData monitor = do
    let monitorName = monitor ^. monVarName
        method = monitor ^. monVarMethod
        transform = monitor ^. monVarTransform
        vecs = map transform $ getMonitorData dList outData $ monitor ^. monVarSource
        -- fullAction = monVarFullAction monitor
    -- fullAction vecs
    createDirectoryIfMissing True destDir
    putStr "Charting: "
    putStr monitorName
    putStrLn "..."
    paths <- case vecs of
        [] -> return [] -- error $ "No vectors, skipping chart " ++ show (map (dataVectorName.yVector) dList)-- TODO: How should this fail?
            -- print "No vecs"
            -- print (monitor ^. monVarSource)
            -- print (map (dataVectorName.yVector) dList)
            -- return [] --
        x  -> case method of
                TimeChart conf -> produceChart destDir conf vecs (simCHID sim ++ " - " ++ monitorName) monitorName
    putStrLn " complete."
    return paths


-- getMonitorData :: MonitorVal a => [DataVectorPair Double a] -> OutData -> MonitorSource -> [DataVectorPair Double a]
getMonitorData :: [DataVectorPair Double Double] -> OutData -> MonitorSource -> [DataVectorPair Double Double]
-- getMonitorData dList outData (CSVOutput names) = map (findDVectorPairByYName dList) names
getMonitorData dList outData (CSVOutput unitsAndNames) = mapMaybe (findDVectorPairByYNameMaybe dList) unitsAndNames -- TODO: this version wont crash but hides missing data
getMonitorData dList outData (RunTimeOutput f) = f outData
                                               -- -- | otherwise = error ((head names) ++ " not supported")
