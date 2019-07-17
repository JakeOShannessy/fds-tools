{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings,
    FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
module FDSUtilities.Types.Simulation where

data FDSSimulation
    = FDSSimulation
        { simDir :: FilePath
        , simCHID :: String
        } deriving (Eq, Show)  -- TODO: CONSIDER: should this filepath always
                            -- be absolute? (in use, in input relative is fine)

-- for multiple evac runs. the input file should be included in basedDir
data MultiSimulation
    = MultiSimulation
        { baseDirMultiSim :: FilePath
        , chidMultiSim :: String
        } deriving (Eq, Show)

class Simulation a where
    getSimCHID :: a -> String
    getSimDir  :: a -> FilePath

instance Simulation FDSSimulation where
    getSimCHID = simCHID
    getSimDir = simDir

instance Simulation MultiSimulation where
    getSimCHID = chidMultiSim
    getSimDir = baseDirMultiSim
