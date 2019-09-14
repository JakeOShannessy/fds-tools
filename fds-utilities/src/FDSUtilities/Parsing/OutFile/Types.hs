{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.Parsing.OutFile.Types where

import qualified Control.Applicative as A
import Control.Monad (mzero)
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Time
import Data.Text

-- data CurrentProgress = CurrentProgress
--     { currentProgress_endTime :: Double
--     , currentProgress_lastSimTime :: Double
--     , currentProgress_lastWallTime :: UTCTime
--     , currentProgress_currentWallTime :: UTCTime
--     } deriving (Show)

-- instance ToJSON CurrentProgress where
--     toJSON currentProgress =
--         object
--             [ "EndTime" Aeson..= currentProgress_endTime currentProgress
--             , "LastSimTime" Aeson..= currentProgress_lastSimTime currentProgress
--             , "LastWallTime" Aeson..= currentProgress_lastWallTime currentProgress
--             , "CurrentWallTime" Aeson..= currentProgress_currentWallTime currentProgress
--             ]

-- instance FromJSON CurrentProgress where
--     parseJSON (Object v) = CurrentProgress A.<$>
--                                 v .: "EndTime" A.<*>
--                                 v .: "LastSimTime" A.<*>
--                                 v .: "LastWallTime" A.<*>
--                                 v .: "CurrentWallTime"
--     parseJSON _ = mzero

data MeshGridDimensions = MeshGridDimensions
    Int -- ^MeshNum
    Int -- ^x
    Int -- ^y
    Int -- ^z
    deriving (Show)

data MeshPhysicalDimensions = MeshPhysicalDimensions
    Int -- ^MeshNum
    Double -- ^x
    Double -- ^y
    Double -- ^z
    deriving (Show)

data RestartInfo = RestartInfo
    { restartInfo_compileDate :: String
    , restartInfo_version :: String
    , restartInfo_openMPStatus :: String
    , restartInfo_svnRevision :: Int
    , restartInfo_jobTitle :: String
    , restartInfo_chid :: String
    }
