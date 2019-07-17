module FDSUtilities.Simulation
    ( basicSim
    -- , simpleSim

    , fdsFilePath
    , smvFilePath
    , outFilePath
    , evacOutFilePath
    , iniFilePath
    , endFilePath

    , ctrlFilePath
    , devcFilePath
    , massFilePath
    , hrrFilePath
    , evacFilePath

    , basicSuffix
    ) where
import FDSUtilities.Types
import Data.List
import System.Directory
-- import System.Environment
import System.FilePath
import System.FilePath.Glob

basicSim projDir chid = FDSSimulation (joinPath [projDir, chid]) chid
-- simpleSim chid = FDSSimulation chid chid

basicSuffix :: (Simulation a) => String -> a -> FilePath
basicSuffix suffix simulation = joinPath [dir, chid ++ ('.':suffix)]
    where
        chid = getSimCHID simulation
        dir = getSimDir simulation


fdsFilePath :: (Simulation a) => a -> FilePath
fdsFilePath = basicSuffix "fds"

smvFilePath :: FDSSimulation -> FilePath
smvFilePath = basicSuffix "smv"

outFilePath :: FDSSimulation -> FilePath
outFilePath = basicSuffix "out"

iniFilePath :: FDSSimulation -> FilePath
iniFilePath = basicSuffix "ini"

endFilePath :: FDSSimulation -> FilePath
endFilePath = basicSuffix "end"

evacOutFilePath :: FDSSimulation -> FilePath
evacOutFilePath simulation = joinPath [dir, chid ++ "_evac.out"]
    where
        chid = getSimCHID simulation
        dir = getSimDir simulation


csvFile :: String -> FDSSimulation -> FilePath
csvFile nom simulation = joinPath [dir, chid ++ ('_':nom) ++ ".csv"]
    where
        chid = simCHID simulation
        dir = simDir simulation

ctrlFilePath  = csvFile "ctrl"
devcFilePath  = csvFile "devc"
massFilePath  = csvFile "mass"
hrrFilePath   = csvFile "hrr"
evacFilePath  = csvFile "evac"
