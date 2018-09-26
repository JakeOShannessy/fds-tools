{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
module FDSUtilities.Types.Monitor where

-- import Control.DeepSeq
import Control.Lens

-- import qualified Data.Array.Repa as R
-- import qualified Data.ByteString as B
import Data.Default
import qualified Data.Map as M
import Data.Time
-- import qualified Data.Vector.Unboxed as V

-- import Data.Maybe (fromMaybe)

-- import System.FilePath
-- import Text.Printf
-- import Text.Namelist.Types

import FDSUtilities.Types.Data
import FDSUtilities.Types.Simulation
import FDSUtilities.Types.OutFile
import FDSUtilities.Types.Smokeview

import qualified FDSUtilities.Class as FC

data ChartConfig = ChartConfig
    { _chartConfigMaxX :: Maybe Double
    , _chartConfigMaxY :: Maybe Double
    , _chartConfigMinX :: Maybe Double
    , _chartConfigMinY :: Maybe Double
    , _chartRefData :: [FlexibleDataVector]
    , _chartXLabel :: Maybe String
    , _chartYLabel :: Maybe String
    , _chartIncludeCHID :: Bool
    } deriving Show

instance Default ChartConfig where
    def = ChartConfig
            { _chartConfigMaxX = Nothing
            , _chartConfigMaxY = Nothing
            , _chartConfigMinX = Nothing
            , _chartConfigMinY = Nothing
            , _chartRefData = []
            , _chartXLabel = Nothing
            , _chartYLabel = Nothing
            , _chartIncludeCHID = False
            }

data MonitorFile = MonitorFile
        { _monitorFileProjectName :: String
        , _monitorFileDestDir :: FilePath
        , _monitorFileWatchCases :: [WatchCase]
        }

instance Default MonitorFile where
    def = MonitorFile
            { _monitorFileProjectName =  error "monitorFileProjectName not set"
            , _monitorFileDestDir =  error "monitorFileDestDir not set"
            , _monitorFileWatchCases =  error "monitorFileWatchCases not set"
            }

data WatchCase = WatchCase
        { _watchCaseSim :: FDSSimulation -- ^Simulation to monitor
        , _watchCaseMonitors :: [Monitor]
        }

instance Default WatchCase where
    def = WatchCase
            { _watchCaseSim =  error "watchCaseSim not set"
            , _watchCaseMonitors =  error "watchCaseMonitors not set"
            }

newtype MonitorDict = MonitorDict (M.Map String Monitor)

instance FC.Mapping String Monitor MonitorDict where
    (!) (MonitorDict map) = (M.!) map
    (\\) (MonitorDict map1) (MonitorDict map2) = MonitorDict ((M.\\) map1 map2)
    null (MonitorDict map) = M.null map
    size (MonitorDict map) = M.size map
    member x (MonitorDict map) = M.member x map
    notMember x (MonitorDict map) = M.notMember x map
    lookup x (MonitorDict map) = M.lookup x map
    empty = MonitorDict M.empty
    insert x y (MonitorDict map) = MonitorDict (M.insert x y map)
    delete x (MonitorDict map) = MonitorDict (M.delete x map)

monitorDictToList x = map snd (M.toList $ monitorDictToMap x)
monitorDictToMap (MonitorDict m) = m
modifyMonitorKey k transform' (MonitorDict m)
    = MonitorDict $ M.update transform k m
    where
        transform x = Just $ transform' x

addMonitorKey k val monitorDict
    = FC.insert k val monitorDict

-- |Reference data is taken to be a property of the chart itself (like a background) and so belongs under chart configuration, not here.
-- it is reasonable to assume that all data are time based, implicit in the term 'monitor'. This is not to show other relationships.
data Monitor = Monitor
        { _monVarName :: String -- ^The name of the monitor
        , _monVarSource :: MonitorSource -- ^A description of how to obtain the data -- TODO: consider allowing multiple data sources.
        , _monVarTransform :: (DataVectorPair Double Double) -> (DataVectorPair Double Double) -- ^A transformation to the entire vector
        -- , _monVarTransform :: (DataVectorPair Double Double) -> (DataVectorPair Double Double)
        -- , _monVarFullTransmform :: (MonitorVal a, MonitorVal b) => a -> b -- ^A transformation to each data point individually (i.e. the y-value).
        -- TODO: the above should be able to change between types of monitorval
        -- , _monVarAction :: MonitorVal a => Double -> a -> IO () -- ^A function to perform some task given the data
        -- , _monVarFullAction :: MonitorVal a => [DataVectorPair Double a] -> IO () -- ^A function to perform some task over all of given the data associated with this monitor
        -- , _monVarFullAction :: [DataVectorPair Double Double] -> IO () -- ^A function to perform some task over all of given the data associated with this monitor
        , _monVarMethod :: MonitorMethod -- ^The method with which the data will be displayed
        }

data MonitorSource
        = CSVOutput [(String,String)]
        -- -- | RunTimeOutput (MonitorVal a => (OutData -> [DataVectorPair Double a]))
        | RunTimeOutput (OutData -> [DataVectorPair Double Double])
        -- -- | None
instance Show MonitorSource where
    show (CSVOutput names) = "CSVOutput " ++ show names
    show (RunTimeOutput f) = "RunTimeOutput"

instance Default Monitor where
    def = Monitor
            { _monVarName =  error "monVarName not set"
            , _monVarSource =  error "monVarSource not set"
            , _monVarTransform = id
            , _monVarMethod =  error "monVarMethod not set"
            }

-- |The class of values that may be monitored.
class MonitorVal a

instance MonitorVal Double
instance MonitorVal Bool
instance MonitorVal UTCTime

-- |The type of ouput produced by a monitor.
data MonitorMethod = TimeChart ChartConfig | ActivationBar | ActivationDot deriving Show


makeLenses ''ChartConfig

makeLenses ''MonitorFile
makeLenses ''WatchCase
makeLenses ''Monitor

