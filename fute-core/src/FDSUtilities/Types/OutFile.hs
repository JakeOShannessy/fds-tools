{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
module FDSUtilities.Types.OutFile where

import Data.Time
import FDSUtilities.Types.Common

data CaseStatus = CaseStatus
        { caseCurrentProgress :: Double
        , casePredictedEndTime :: UTCTime
        , caseOutStatus :: OutStatus
        } deriving Show

data EvacVersionInformation = EvacVersionInformation deriving Show
data EvacMiscParams = EvacMiscParams deriving Show
data EvacMeshParams = EvacMeshParams deriving Show

data AgentExitProps = AgentExitProps Int Double String  deriving (Show, Read) -- AgentNumber ExitTime ExitName

data InitialAgentProps = InitialAgentProps Int (Double, Double, Double) Double Double Double Double Double Int Int  deriving (Show, Read) -- (xPos, yPos, zPos) tPre tDet dia v0 tau i_gr i_ff

data DevcActivation = DevcActivationTime Double | NoActivation deriving (Show, Read, Eq, Ord)

data MeshDimensions = MeshDimensions
    { meshCellsX :: Int
    , meshCellsY :: Int
    , meshCellsZ :: Int
    , meshDimX   :: Double
    , meshDimY   :: Double
    , meshDimZ   :: Double
    } deriving Show

data OutData = OutData
    { outFDSVersion :: Version
    , mDims :: [MeshDimensions]
    , miscellaneous :: MiscParameters
    , timesteps :: [TimeStep]
    , dAct  :: Maybe [(Int, String, DevcActivation)]
    , outStatus :: OutStatus
    } deriving (Show)
data EvacOutData = EvacOutData
    { evacVersionInformation :: EvacVersionInformation
    , evacMiscParams :: EvacMiscParams
    , evacMeshParams :: EvacMeshParams
    , evacInitialAgentProps :: [InitialAgentProps]
    , evacExitAgentProps :: [AgentExitProps]
    } deriving Show
data OutStatus = NotStarted | Incomplete | StoppedByUser | NumericalInstability | Completed deriving Show

data MiscParameters = MiscParameters
    { simStart  :: Double
    , simEnd    :: Double
    } deriving (Show)

data TimeStep = TimeStep
    { num :: Int
    , time :: UTCTime
    , stepSize :: Double
    , simTime :: Double
    , timeStepStepProps :: [StepProp]
    , timeStepMeshes' :: [MeshStep]
    } deriving (Show)

-- instance Show TimeStep where
--     show (TimeStep num time meshes) =
--         "Time Step " ++ show num ++ "   " ++ show time ++ "\n"
--         ++ "----------------------------------------------\n"
--         ++ concat (map show meshes)
--         ++ "\n"

data MeshStep = MeshStep
    { meshNum :: Int
    , meshStepProps :: [StepProp]
    } deriving Show

data StepProp = StepProp
    { stepPropKey :: String
    , stepPropValue :: Value
    , stepPropUnits :: Maybe String
    , stepPropLocation :: Maybe Location
    } deriving (Show, Eq)

data Location = Location
    { locationMesh :: Maybe Int
    , locationCoords :: (Int, Int, Int)
    } deriving (Show, Eq)

data Value = ValueInt Int | ValueDouble Double deriving (Show, Eq, Ord)

-- instance Show MeshStep where
--     show (MeshStep num cycle cpustep totalcpu timestep totaltime maxcfl maxdiv mindiv) =
--         "Mesh " ++ show num ++ ", Cycle " ++ show cycle ++ "\nCPU/step: "
--         ++ show cpustep ++ " s, Total CPU: " ++ show totalcpu ++ " s\n"
--         ++ "Time step: " ++ show timestep ++ " s, Total Time: " ++ show totaltime ++ " s\n"
--         ++ "Max CFL number: " ++ show maxcfl ++ "\n"
--         ++ "Max divergence: " ++ show maxdiv ++ "\n"
--         ++ "Min divergence: " ++ show mindiv ++ "\n"

data NumAndCoord = NumAndCoord Double (Int,Int,Int)

instance Show NumAndCoord where
    show (NumAndCoord num (x,y,z)) = show num ++ " at (" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"
