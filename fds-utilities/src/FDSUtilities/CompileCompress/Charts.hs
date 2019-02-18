module FDSUtilities.CompileCompress.Charts where

import Control.Exception as E
import Control.Monad
import Data.Time

import FDSUtilities.Parsing
import FDSUtilities.Plot
import FDSUtilities.RunTimeCalc
import FDSUtilities.Simulation
import FDSUtilities.Parsing.SimulationData
import FDSUtilities.Types
import FDSUtilities.Types.Monitor
import FDSUtilities.Monitor

import System.Directory

import System.FilePath


produceMonitorCharts :: MonitorDict -> CompilationActionFunction
produceMonitorCharts monitorDict destDir simulation = do
    putStr (simCHID simulation)
    dList <- getDataListAll simulation
    mapM_ (print . (\(DataVectorPair (DataVector xName xUnits xValues) (DataVector yName yUnits yValues))->(yUnits,yName))) dList
    outData <- getOutData simulation
    let runData = getRunData outData
        status = outStatus outData
    -- case status of   -- TODO: It would be nice to include this information on the chart itself.
            -- NumericalInstability -> putStrLn " -> Warning: Numerical instability."
            -- Incomplete -> putStrLn " -> Running..."
            -- StoppedByUser -> putStrLn " -> Completed (user)"
            -- Completed -> putStrLn " -> Completed (time end)"
    let mPars = miscellaneous outData
        simEndTime = simEnd mPars
        peTime = predEndTime simEndTime runData
        caseStatus = CaseStatus
                        { caseCurrentProgress = getCurrentProgress outData
                        , casePredictedEndTime = peTime
                        , caseOutStatus = status
                        }
    tZone <- getCurrentTimeZone
    createDirectoryIfMissing True destDir
    normalMonitorPaths <- (liftM concat) $
        mapM (produceMonitor destDir simulation dList outData)
        $ monitorDictToList monitorDict
    runChartPath <- produceRunChart destDir tZone simEndTime runData
    -- TODO: remove special support for rundata chart and include in
    -- generalisation
    return (runChartPath ++ normalMonitorPaths)


getOutData sim = do
    let handler :: SomeException -> IO (Either ParseError OutData)
        handler e = do
            print e
            getLine
            return $ Right $ OutData (Version 0 0 0) [] (MiscParameters 0 0) [] Nothing NotStarted
    outFileExists <- doesFileExist $ outFilePath sim
    outData' <- if outFileExists
                   then {-# SCC parseOutData #-} E.catch (parseSimulationOutFile sim) handler
                   else error $ "Outfile does not exist. > " ++ (outFilePath sim)
    let outData = case outData' of
            Left e -> error $ show e
            Right x -> x
    return outData

-- |Produce charts that show each stage of the simulation. This is for the purposes of animating the charts.
produceProgressiveMonitorCharts :: [Monitor] -> CompilationActionFunction
produceProgressiveMonitorCharts monitors destDir simulation = do
    putStr (simCHID simulation)
    dList <- getDataListAll simulation
    mapM_ (print . (\(DataVectorPair (DataVector xName xUnits xValues) (DataVector yName yUnits yValues))->(yUnits,yName))) dList
    outData <- getOutData simulation
    let runData = getRunData outData
        status = outStatus outData
    -- case status of   -- TODO: It would be nice to include this information on the chart itself.
            -- NumericalInstability -> putStrLn " -> Warning: Numerical instability."
            -- Incomplete -> putStrLn " -> Running..."
            -- StoppedByUser -> putStrLn " -> Completed (user)"
            -- Completed -> putStrLn " -> Completed (time end)"
    let mPars = miscellaneous outData
        simEndTime = simEnd mPars
        peTime = predEndTime simEndTime runData
        caseStatus = CaseStatus
                        { caseCurrentProgress = getCurrentProgress outData
                        , casePredictedEndTime = peTime
                        , caseOutStatus = status
                        }
    tZone <- getCurrentTimeZone
    createDirectoryIfMissing True destDir
    normalMonitorPaths <- (liftM concat) $ mapM (produceProgressiveMonitor destDir simulation dList outData) monitors
    runChartPath <- produceRunChart destDir tZone simEndTime runData    -- TODO: remove special support for rundata chart and include in generalisation
    return (runChartPath ++ normalMonitorPaths)


-- |Get the current progress (i.e. number of seconds simulated).
getCurrentProgress :: OutData -> Double
getCurrentProgress outData = simTime $ last $ timesteps outData
