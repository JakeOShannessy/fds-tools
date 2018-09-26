{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative as A

--import Data.Aeson as Aeson
--import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad

import Data.Monoid ((<>))
import Data.Version

import FDSUtilities.Types
import FDSCaseManage (createNewRev)

import Options.Applicative

import System.Directory
import System.Environment
import System.FilePath
import System.Info


import Paths_fute (version)

import Commands

data Options = Options
    { optCommand :: Command
    } deriving Show

data Command
    = CountCells FilePath
    | Meshes FilePath
    | MeshCheck FilePath
    | PlotHRR FilePath
    | ShowHRR FilePath
    | PeakHRR FilePath
    | VerifyInput FilePath Bool
    | RenameSimulation FilePath String
    | CreateNewRev FilePath
    | CurrentProgress FilePath
    | PlotOut FilePath
    deriving Show

optionsParser :: Parser Options
optionsParser = Options
--    --<$> strOption
--    --    ( long "config"
--    --    <> short 'c'
--    --    <> metavar "PATH"
--    --    <> value "/home/ec2-user/run.config"
--    --    <> help "path for the config file")
    -- <$> switch
    --    ( long "version"
    --    <> help "Display version information")
    <$> commands

commandParser :: ParserInfo Command
commandParser = info (helper <*> commands) $ fullDesc
    <> progDesc "Run FDS simulations on AWS"
    <> header "Don't know what to put here"

commands :: Parser Command
commands = subparser $ mconcat
    [ command "count-cells" countCellsParser
    , command "meshes" meshesParser
    , command "mesh-check" meshCheckParser
    , command "plot-hrr" plotHRRParser
    , command "show-hrr" showHRRParser
    , command "peak-hrr" peakHRRParser
    , command "verify-input" verifyInputParser
    , command "rename" renameSimulationParser
    , command "new-rev" createNewRevParser
    , command "current-progress" currentProgressParser
    , command "plot-out" plotOutParser
    ]

-- TODO: add the machine readable option here
countCellsParser :: ParserInfo Command
countCellsParser = info (CountCells <$> parseFilePath)
                       (progDesc "Count the total number of cells")

meshesParser :: ParserInfo Command
meshesParser = info (Meshes <$> parseFilePath)
                       (progDesc "Display information on each mesh")

meshCheckParser :: ParserInfo Command
meshCheckParser = info (MeshCheck <$> parseFilePath)
                       (progDesc "Check that the meshes are well behaved")

plotHRRParser :: ParserInfo Command
plotHRRParser = info (PlotHRR <$> parseFilePath)
                       (progDesc "Plot the HRR")

showHRRParser :: ParserInfo Command
showHRRParser = info (ShowHRR <$> parseFilePath)
                       (progDesc "Plot and show the HRR")

peakHRRParser :: ParserInfo Command
peakHRRParser = info (PeakHRR <$> parseFilePath)
                       (progDesc "Print the highest HRR value from available data")

verifyInputParser :: ParserInfo Command
verifyInputParser = info (VerifyInput <$> parseFilePath <*> switch
                            ( long "show"
                            <> help "Display the verification info after creation"))
                       (progDesc "Verify an FDS input file")

renameSimulationParser :: ParserInfo Command
renameSimulationParser = info (RenameSimulation <$> parseFilePath <*> parseCHID)
                       (progDesc "Rename a simulation")

createNewRevParser :: ParserInfo Command
createNewRevParser = info (CreateNewRev <$> parseFilePath)
                        (progDesc "Create a new revision of a simulation")

currentProgressParser :: ParserInfo Command
currentProgressParser = info (CurrentProgress <$> parseFilePath)
                        (progDesc "Output the current progress of the simulation")

plotOutParser :: ParserInfo Command
plotOutParser = info (PlotOut <$> parseFilePath)
                        (progDesc "Plot information from the .out file")

parseFilePath :: Parser FilePath
parseFilePath = argument str (metavar "INPUT-FILE")

parseCHID :: Parser String
parseCHID = argument str (metavar "CHID")

-- simulationParser :: Parser FDSSimulation
-- simulationParser = argument str (metavar "SIMULATION")

-- |A string might be any number of things that describe a simulation. This
-- function translates those strings into FDSSimulations depending on what
-- it finds in the filesystemm
stringToFDSSimulation :: String -> IO FDSSimulation
stringToFDSSimulation string = undefined

-- simParser :: Parser

main :: IO ()
main = execParser opts >>= enact
  where
    opts = info (helper <*> versionP <*> optionsParser)
      ( fullDesc
      <> progDesc "Enter a command"
      <> header "fute - a collection of tools for FDS-SMV" )

versionP :: Parser (a -> a)
versionP = infoOption
            versionString
            (long "version" <>
             help "Display version information")

-- Regardless of whether the command needs an input file, results, or both,
-- they can all be represented with the FDSSimulation type. The availability
-- of the required information will need to be checked anyway.

enact :: Options -> IO ()
enact options = do
    -- here, fp usually represents a string that indentifies a simulation
    -- the lack of a string will default to ".", which will search the current
    -- directory
    case optCommand options of
        -- can use only an input file or with results
        CountCells fp -> countCells fp
        -- can use only an input file or with results
        Meshes fp -> meshDetails fp
        -- can use only an input file or with results
        MeshCheck fp -> meshCheck fp
        -- needs results
        PlotHRR fp -> plotHRR fp
        -- needs results
        ShowHRR fp -> showHRR fp
        -- needs results
        PeakHRR fp -> peakHRR fp
        -- PredictHRR fp -> predictHRR fp
        -- needs only an input file, switch determines whether to show in a
        -- browser
        VerifyInput fp False -> verifyInput fp
        VerifyInput fp True -> showInputVerification fp
        -- needs an input file
        RenameSimulation fp newName -> renameSimulationInPath fp newName >> return ()
        -- needs an input file
        CreateNewRev fp -> createNewRev fp >> return ()
        CurrentProgress fp -> currentProgress fp >> return ()
        PlotOut fp -> plotOut fp >> return ()

versionString :: String
versionString = "FDS-SMV utilities collection - fute " <> showVersion version
    <> "\n" <> "Copyright (C) 2016 Jake O'Shannessy"
    <> "\n" <> "Compiled for: " <> os <> " " <> arch
    <> "\n" <> "Compiled using: " <> compilerName <> " "
        <> showVersion compilerVersion
