module FDSUtilities.Parsing.OutFile.Types where

import Data.Time

data CurrentProgress = CurrentProgress
    { currentProgress_endTime :: Double
    , currentProgress_lastSimTime :: Double
    , currentProgress_lastWallTime :: UTCTime
    , currentProgress_currentWallTime :: UTCTime
    } deriving (Show)

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
