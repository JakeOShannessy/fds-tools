module FDSUtilities.Histogram where

import qualified Data.Vector as V

data Histogram = Histogram
    (Double, Double)
    (Int, Int)
    [Bin] -- ^Upper and lower bounds - No. of out of bound values (upper, lower) - Bins
    deriving (Show)
data Bin = Bin
    Double
    Double
    Int -- ^Lower limit inclusive - Upper limit exclusive - Number of samples
    deriving Show

data HistogramNormalised = HistogramNormalised
    (Double, Double)
    (Int, Int)
    [BinNormalised] -- ^Upper and lower bounds - No. of out of bound values (upper, lower) - Bins
    deriving (Show)
data BinNormalised = BinNormalised
    Double
    Double
    Double -- ^Lower limit inclusive - Upper limit exclusive - Number of samples
    deriving Show

updateHist :: Histogram -> Double -> Histogram
updateHist (Histogram (lowRange, highRange) (tooLow, tooHigh) bins) value
    | value < lowRange = Histogram (lowRange, highRange) (tooLow+1, tooHigh) bins
    | value >= highRange = Histogram (lowRange, highRange) (tooLow, tooHigh+1) bins
    | otherwise = Histogram (lowRange, highRange) (tooLow, tooHigh) (updateHist' [] bins value)
    where
        inBin :: Bin -> Double -> Bool
        inBin (Bin lo hi n) val = val >= lo && val < hi
        updateHist' :: [Bin] -> [Bin] -> Double -> [Bin]
        updateHist' checked [] x = error "Exceeds bounds of histogram, this should have been checked earlier"
        updateHist' checked (b:bins) x | inBin b x = checked ++ ((Bin lo hi (n+1)):bins)
                                       | otherwise = updateHist' (checked ++ [b]) bins x
            where
                (Bin lo hi n) = b

emptyHist :: Int -> (Double, Double) -> Histogram
emptyHist n (rangeLow, rangeHigh) = Histogram (minX, maxX) (0,0) bins
    where   -- TODO: Find a more thorough way of determining an appropriate range, removing outliers etc
        -- totalNo = fromIntegral (length sample) :: Double
        -- minX = case rangeLow of
            -- Just low -> low
            -- Nothing  -> minimum sample
        -- maxX = case rangeHigh of
            -- Just high -> high
            -- Nothing   -> maximum sample
        minX = rangeLow
        maxX = rangeHigh
        range = maxX - minX

        binSize = range/(fromIntegral n)   -- make sure all of this works well with floating point arithmetic
        -- bins = [minX,minX+binSize..maxX] -- it doesn't at the moment
        bins = mkBins [Bin (maxX-binSize) maxX 0]
            where
                mkBins [] = error "Cannot form bins from empty list"
                mkBins ((Bin lo hi n):ps)
                    | lo > minX = mkBins $ (Bin (lo-binSize) (hi-binSize) 0):(Bin lo hi n):ps
                    | otherwise = ((Bin lo hi 0):ps)

histogram :: Int -> Maybe Double -> Maybe Double -> V.Vector Double -> Histogram --[(Double,(Double,Double))]
histogram n rangeLow rangeHigh sample = V.foldl' updateHist initHist sample
    where
        minX = case rangeLow of
            Just low -> low
            Nothing  -> 0 --minimum sample
        maxX = case rangeHigh of
            Just high -> high
            Nothing   -> 1.5 * (V.maximum sample)
        range = maxX - minX
        initHist = emptyHist n (minX, maxX)
