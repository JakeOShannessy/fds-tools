module FDSUtilities.RunTimeCalc
    ( getRunData
    , getRelativeRunData
    , getTimeLeft
    , predEndTime
    ) where

import Control.Concurrent

import Data.Either
import Data.List
import Data.Time
import qualified Data.Vector.Unboxed as V

import FDSUtilities.Types

import System.Directory
import System.FilePath
import System.IO
import System.Process


-- TODO: test for negative values
toHoursString :: Double -> String
toHoursString time = show hours ++ ":" ++ show minutes ++ ":" ++ show seconds
    where
        time' = floor time :: Int
        seconds = time' `rem` 60 :: Int
        minutes = (time' `quot` 60) `rem` 60 :: Int
        hours   = (time' `quot` 60) `quot` 60 :: Int

getTimeLeft :: OutData -> Double
getTimeLeft outFile = timeRemain (endTime-startTime) ultStep penUltStep
    where
        steps = timesteps outFile
        [penUltStep,ultStep] = drop (length steps - 2) steps
        mPars = miscellaneous outFile
        startTime = simStart mPars
        endTime = simEnd mPars

timeRemain :: Double -> TimeStep -> TimeStep -> Double
timeRemain = timeLeft

timeLeft :: Double -> TimeStep -> TimeStep -> Double
timeLeft simLength ultStep penUltStep =
    (dCalcTime / dSimTime) * (simLength - (simTime ultStep))
    where
        dCalcTime = realToFrac (diffUTCTime (time ultStep) (time penUltStep)) :: Double
        dSimTime = (simTime ultStep) - (simTime penUltStep)


getRunData :: OutData -> [(UTCTime, Double)]
getRunData outFile = dPoints --map convertToDVPair dPoints
    where
        mPars = miscellaneous outFile
        steps = timesteps outFile
        -- startTime = simStart mPars
        -- endTime = simEnd mPars
        -- simLength = simEnd - simStart   :: Double
        firstStep = case steps of
            [] -> error "getRunData: insufficient steps"
            (x:xs) -> x
        (startUTC, _) = getRunTimeDataPoint firstStep
        dPoints = map getRunTimeDataPoint steps

getRelativeRunData :: OutData -> DataVectorPair Double Double
getRelativeRunData outFile = convertToDVPair $ convertDLList ([],[]) $ map (subStartUTC startUTC) dPoints
    where
        mPars = miscellaneous outFile
        steps = timesteps outFile
        (penUltStep:ultStep:[]) = drop (length steps -2) steps
        startTime = simStart mPars
        endTime = simEnd mPars
        (startUTC, _) = getRunTimeDataPoint (headErr "getRelativeRunData" steps)
        dPoints = map getRunTimeDataPoint steps


subStartUTC :: UTCTime -> (UTCTime, Double) -> (Double, Double)
subStartUTC startUTC (utcTime, simTime) = ((fromRational . toRational) (diffUTCTime utcTime startUTC), simTime) :: (Double, Double)

convertDLList :: ([Double],[Double]) -> [(Double,Double)] -> ([Double],[Double])
convertDLList acc [] = acc
convertDLList acc@(accT, accSt) ((t,st):ps) = convertDLList ((t:accT), (st:accSt)) ps

convertToDVPair :: (V.Unbox a, V.Unbox b) =>
                     ([a], [b]) -> DataVectorPair a b
convertToDVPair (t, st) = DataVectorPair (DataVector "Clock Time" "s" (V.fromList t)) (DataVector "Simulation Time" "s" (V.fromList st))

-- runVec = do
    -- outFile <- lockCheat "\\\\raw-model01\\RAWFire FDS5 Projects\\Barangaroo\\Barangaroo DF1A - 3rd run\\Barangaroo_DF1A - Copy.out"
    -- -- outFile <- lockCheat "D:\\Data\\Ikea Local\\FDS\\IKEAMarsdenWF1\\IKEAMarsdenWF1.out"
    -- -- parseTest outFileParser outFile
    -- -- print outFile
    -- let testLines = lines outFile
        -- res1 = parse outFileParser "" outFile
        -- res = case res1 of
                    -- Right x -> x
                    -- Left err -> error (show err) -- (\(Right x) -> x) res1    -- if left skip iteration
        -- mPars = miscellaneous res
        -- steps = timesteps res
        -- -- mSteps =
        -- (penUltStep:ultStep:[]) = drop (length steps - 2) steps
        -- startTime = simStart mPars
        -- endTime = simEnd mPars
        -- dPoints = map getRunTimeDataPoint steps
    -- -- print $ (timeLeft (endTime-startTime) ultStep penUltStep)
    -- mapM_ print dPoints

getRunTimeDataPoint :: TimeStep -> (UTCTime,Double)
getRunTimeDataPoint tStep = (clockTime, curretSimTime)
    where
        clockTime = time tStep
        curretSimTime = simTime tStep

predEndTime :: Double -> [(UTCTime, Double)] -> UTCTime
predEndTime simEndTime dPoints = predTime simEndTime penUltStep ultStep
        where
            [penUltStep,ultStep] = if length dPoints >= 2
                then drop (length dPoints - 2) dPoints
                else error "Insufficient data points."

predTime :: Double -> (UTCTime, Double) -> (UTCTime, Double) -> UTCTime
predTime simEndTime (penUltStepClock,penUltStepSim) (ultStepClock, ultStepSim) = addUTCTime (fromIntegral timeLeft) ultStepClock
    where
        timeLeft :: Int
        timeLeft = floor $ (dCalcTime / dSimTime) * (simEndTime - ultStepSim)
            where
                dCalcTime = realToFrac (diffUTCTime ultStepClock penUltStepClock) :: Double
                dSimTime = ultStepSim - penUltStepSim
