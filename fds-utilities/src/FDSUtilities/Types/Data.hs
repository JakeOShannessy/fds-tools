{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
module FDSUtilities.Types.Data where

import Control.DeepSeq
import Control.Lens

import Data.Aeson as Aeson
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V


data FlexibleDataVector = DataPointVector String String [(Double,Double)] | DataFuncVector String String (Double -> Double)

instance Show FlexibleDataVector where
    show f = "FlexibleDataVector"
-- TODO: Create a function to output DataVectorPairs as CSV
data DataVectorPair a b = DataVectorPair
    { xVector   :: DataVector a
    , yVector   :: DataVector b
    }
    deriving (Show)

instance ToJSON (DataVectorPair Double Double) where
    toJSON (DataVectorPair
        (DataVector nameX unitsX valuesX)
        (DataVector nameY unitsY valuesY)) =
        object
            [ "NameX" Aeson..= nameX
            , "UnitsX" Aeson..= unitsX
            , "NameY" Aeson..= nameY
            , "UnitsY" Aeson..= unitsY
            , "Values" Aeson..= [object
                [ "x" Aeson..= x
                , "y" Aeson..= y
                ] | x <- V.toList valuesX, y <- V.toList valuesY]
            ]

instance Show SliceDataHeader where
    show (SliceDataHeader quantity shortName units dimensions)=
        show quantity ++ "\n"
        ++ show shortName ++ "\n"
        ++ show units ++ "\n"
        ++ show dimensions
        -- ++ concatMap (\x-> show x ++ "\n") parsedData

instance NFData SliceDataHeader where
    rnf (SliceDataHeader lName sName units location)
        = rnf lName `seq` rnf sName `seq` rnf units `seq` rnf location `seq` ()

mkDataVectorPairFromLists :: (String, String, [Double]) -> (String, String, [Double]) -> DataVectorPair Double Double
mkDataVectorPairFromLists (xName, xUnits, xs) (yName, yUnits, ys)
  = DataVectorPair
      (DataVector xName xUnits (V.fromList xs))
      (DataVector yName yUnits (V.fromList ys))

interpolateVectorPairX targX (DataVectorPair dv1 dv2) = interpolateVectors targX dv2 dv1
interpolateVectorPairY targY (DataVectorPair dv1 dv2) = interpolateVectors targY dv1 dv2

interpolateVectors :: Double -> DataVector Double -> DataVector Double -> Maybe Double
interpolateVectors target2 (DataVector name1 units1 values1) (DataVector name2 units2 values2) = interpolate target2 values1 values2
    where
        interpolate :: Double -> V.Vector Double -> V.Vector Double -> Maybe Double
        interpolate targ2 v1 v2 = do
            (i1, i2) <- findIndexes v2 targ2 0
            return $ interpIndex v1 v2 targ2 i1 i2
            where
                findIndexes vec target index
                    | index < ((V.length vec) - 2) && (not . V.null) vec
                        = if between target (vec V.! index) (vec V.! (index+1))
                            then Just (index, index+1)
                            else findIndexes vec target (index+1)
                    | otherwise = Nothing

-- |Test if x lies in the range [a,b].
between x a b | x <= b && x >= a = True
              | x <= a && x >= b = True
              | otherwise = False
        -- TODO: benchmark list vs vector for interpolation
        -- interpolate targ2 (v1:[]) _ = Nothing
        -- interpolate targ2 _ (v2:[]) = Nothing
        -- interpolate targ2 (v1a:v1b:v1s) (v2a:v2b:v2s) | targ2 < v2b && targ2 > v2a = Just $ interp targ2 v1a v1b v2a v2b
                                                      -- -- | targ2 < v2a && targ2 > v2b = Just $ interp targ2 v1a v1b v2a v2b
                                                      -- -- | otherwise = interpolate targ2 (v1b:v1s) (v2b:v2s)
interp targetY x1 x2 y1 y2 = (targetY-y1)/(y2-y1)*(x2-x1)+x1
interpIndex vecI vecJ targetY i1 i2 = interp targetY (vecI V.! i1) (vecI V.! i2) (vecJ V.! i1) (vecJ V.! i2)

vecLength (DataVectorPair (DataVector name1 units1 values1) (DataVector name2 units2 values2)) = if length1 == length2 then length1 else error "DataVectorPair of inconsistent lengths"
    where
        length1 = V.length values1
        length2 = V.length values2


data DataVector a = DataVector
    { dataVectorName      :: String
    , dataVectorUnits     :: String
    , dataVectorValues    :: V.Vector a
    }
    deriving (Show)


data SliceDataSet = SliceDataSet
    SliceDataHeader
    SliceData deriving (Show)

type SliceData = [Snapshot]

-- |The header information taken from a slice (.sf) file.
data SliceDataHeader = SliceDataHeader
    String
    String
    String
    (Int, Int, Int, Int, Int, Int) -- ^Long name - Short name - Units - IJK bounds

-- |A snapshot is a 3D block of FDS data (possibly reduced to 1 or 2 dimensions) at a certain time.
data Snapshot = Snapshot
    Float
    (R.Array R.U R.DIM3 Float)

instance Show Snapshot where
    show (Snapshot time theData) = let (R.Z R.:. i R.:. j R.:. k) = R.extent theData
                                   in "Snapshot " ++ show time ++ " " ++ show i ++ "x" ++ show j ++ "x" ++ show k
