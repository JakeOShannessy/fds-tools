import FDSUtilities.SimulationData

import Data.List
import Data.Maybe

vecPairMean (DataVectorPair (DataVector _ _ xValues) (DataVector _ _ yValues)) = ((sum xValues / (fromIntegral (length xValues))), (sum xValues / (fromIntegral (length xValues))))
vPairLength (DataVectorPair (DataVector _ _ v1) (DataVector _ _ v2)) = if length v1 == length v2 then length v1 else error "DataVector lengths do not match"
vecTake n   (DataVectorPair (DataVector a b v1) (DataVector c d v2)) = (DataVectorPair (DataVector a b (take n v1)) (DataVector c d (take n v2)))
vecDrop n   (DataVectorPair (DataVector a b v1) (DataVector c d v2)) = (DataVectorPair (DataVector a b (drop n v1)) (DataVector c d (drop n v2)))

getWindowMean
    :: DataVectorPair -- ^Data
    -> Int      -- ^Max Window Width
    -> Int      -- ^Index of mean
    -> (Double,Double)   -- ^Mean
getWindowMean values wWidth index = vecPairMean window
    where
        window = getWindow values wWidth index
getWindow :: DataVectorPair -> Int -> Int -> DataVectorPair
getWindow values wWidth index = if index >= (vPairLength values) then error "out of range" else window  --TODO, change the outputted DataVector properties to match gradient
    where
        del = minimum [index, wWidth, (vPairLength values) - index - 1]    -- takes window width as the number of points to either side
        window = vecTake (2*del + 1) $ vecDrop (index - del) values
        
availableWindows values wWidth = [minIndex..maxIndex]
    where
        minIndex = wWidth
        maxIndex = (vPairLength values) - wWidth
        
smoothedVals wWidth values@(DataVectorPair (DataVector xName xUnits v1) (DataVector yName yUnits v2)) = (DataVectorPair (DataVector xName xUnits xVals) (DataVector yName yUnits yVals))
    where
        windowIndexes = availableWindows values wWidth
        (xVals, yVals) = unzip $ map (getWindowMean values wWidth) windowIndexes
        
gradient :: DataVectorPair -> DataVectorPair
gradient (DataVectorPair (DataVector xName xUnits v1) (DataVector yName yUnits v2)) = (DataVectorPair (DataVector xName xUnits xVals) (DataVector ("Gradient of " ++ yName) ("(d/dx) " ++ yUnits) yVals))
    where
        (xVals,yVals) = unzip $ gradientWorker (zip v1 v2)
        
gradientWorker :: [(Double, Double)] -> [(Double, Double)]
gradientWorker (v1:v2:[]) = [grad v1 v2]
gradientWorker (v1:v2:values) = (grad v1 v2):(gradientWorker (v2:values))

-- the two findSteadyState functions are equivalent, just different methods
findSteadyState2 :: Double -> [(Double, Double)] -> (Double, Double)
findSteadyState2 threshold values = fromJust $ find (\(x,y)->y<= threshold) gradValues
    where
        gradValues = gradientWorker values

findSteadyState :: Double -> [(Double, Double)] -> (Double, Double)
findSteadyState threshold (v2:[]) = error "steady state not found"
findSteadyState threshold (v1:v2:values) | gradVal <= threshold = gradFull
                                         | otherwise = findSteadyState threshold (v2:values)
    where
        gradFull@(_,gradVal) = grad v1 v2
        
smoothGradient :: DataVectorPair -> DataVectorPair
smoothGradient = gradient . (smoothedVals 50) --TODO: remove hardcoded quantity

grad (ax, ay) (bx, by) = ((ax+bx)/2, (by-ay)/(bx-ax))

testDat = [(1,1),(2,2),(3,3),(4,4),(5,4),(6,4),(7,4)] :: [(Double, Double)]


        
