module FDSUtilities.Parsing
    ( module Text.Namelist.Types
    , module FDSUtilities.Parsing.FDSFile
    , module FDSUtilities.Parsing
    , module FDSUtilities.Parsing.OutFile
    , module FDSUtilities.Parsing.PLOT3D
    , module FDSUtilities.Parsing.SliceFile
    , module FDSUtilities.Parsing.SliceFileSum
    , module FDSUtilities.Parsing.SMVFile
    , ParseError
    ) where
import qualified Data.ByteString as B
import Data.Time.LocalTime
import FDSUtilities.Parsing.FDSFile
import FDSUtilities.Parsing.OutFile
import FDSUtilities.Parsing.PLOT3D
import FDSUtilities.Parsing.SliceFile
import FDSUtilities.Parsing.SliceFileSum
import FDSUtilities.Parsing.SMVFile

import FDSUtilities.Simulation
import FDSUtilities.Types
import Text.ParserCombinators.Parsec
import Text.Namelist
import Text.Namelist.Types

import System.IO
import System.FilePath

parseSimulationSMVFile :: FDSSimulation -> IO (Either ParseError SMVFile)
parseSimulationSMVFile simulation = parseSMVFile (smvFilePath simulation)


parseSimulationOutFile :: FDSSimulation -> IO (Either ParseError OutData)
parseSimulationOutFile simulation = parseOutFile (outFilePath simulation)

type FDSFile = NamelistFile
parseSimulationFDSFile :: FDSSimulation -> IO (Either ParseError FDSFile)
parseSimulationFDSFile simulation = parseFDSFile (fdsFilePath simulation)

{-# DEPRECATED getSMVData "Use parseSimulationSMVFile instead" #-}
getSMVData = parseSimulationSMVFile

sliceFilenames :: SMVFile -> [String]
sliceFilenames smvData = map slcfFilename sliceFileEntries
    where
        sliceFileEntries = filter isSliceEntry $ smvDataFiles smvData

isSliceEntry x = case x of
        SLCFDataFile {} -> True
        _ -> False

-- getSliceHeadersSpecific :: FDSSimulation
getSliceHeadersSpecific simulation sliceEntries = do
    let fileNames = map slcfFilename sliceEntries
        filePaths = map (\x-> joinPath [simDir simulation, x]) fileNames
    headers <- getHeaders filePaths
    return headers


getHeaders filePaths = do
    let getHeaderBytes filepath = do
            fileHndl <- openFile filepath ReadMode
            B.hGet fileHndl 400    -- TODO: find out the max length of header and fix this.
            -- hClose fileHndl  -- TODO: using strict bytestrings so handle closing should be fine
    fileData <- mapM getHeaderBytes filePaths
    let parseSliceHeader' x = case parseSliceHeader x of
                Right x -> x
                Left err -> error $ show err -- TODO: this is a parse error, handle properly
    let headers' = map parseSliceHeader' fileData
    return headers'

getSliceHeadersSimulation :: FDSSimulation -> Maybe SMVFile -> IO [SliceDataHeader]
getSliceHeadersSimulation simulation smvData' = do
    smvData'' <- case smvData' of
                    Just x -> return $ Right x
                    Nothing -> parseFromFile smvParser (smvFilePath simulation)
    let smvData = case smvData'' of
            Left e -> error $ show e
            Right x -> x
    let filenames = sliceFilenames smvData
        filePaths = map (\x-> joinPath [simDir simulation, x]) filenames
    headers <- getHeaders filePaths
    return headers
