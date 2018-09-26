{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module FDSUtilities.Summary where

import Control.Exception

import FDSUtilities.Simulation
import FDSUtilities.Parsing
import FDSUtilities.Simulation
import FDSUtilities.FDSFileFunctions
import FDSUtilities.Types.Assess
import FDSUtilities.Types
import Data.List
import System.Directory
import System.FilePath
import System.FilePath.Glob

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String.Utils
import Data.Tree
import qualified Data.Map as M

import Text.Blaze.Html
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import FDSUtilities.Types.Assess

import Text.Printf

import Debug.Trace

summariseSimulationInputData :: FDSSimulation -> IO (Either String InputSummary)
summariseSimulationInputData fdsSim = do
    let inputPath = fdsFilePath fdsSim
    summariseInputFile inputPath

summariseInputFile :: FilePath -> IO (Either String InputSummary)
summariseInputFile inputPath = do
    fdsDataRaw <- parseFDSFile inputPath
    case fdsDataRaw of
        (Right fdsData) -> do
            r <- try $ do
                let x = summariseInputData fdsData
                print x
                return x
            return $ case r of
                Right x -> Right x
                Left e -> Left $ "Simulation Input Summary could not be completed due to the following error: " ++ show (e :: SomeException)
        (Left err) -> return $ Left $ "Simulation Input Summary could not be completed due to a parsing error:\n" ++ show err

data InputSummary = InputSummary [(String, [(String, InputSummaryMarkup)])] deriving (Show)

instance ToMarkup InputSummary where
    toMarkup (InputSummary dict) = H.div H.! A.class_ "summary-div" $ do
      H.table H.! A.class_ "summary-table" $ do
        mapM_ (\(sectionName,entries)-> do
            H.thead $ do
              H.tr $ do
                H.th $ H.toHtml sectionName
            H.tbody $ do
              mapM_ (\(key,val)->H.tr $ do
                H.td $ H.toHtml (key <> ": ")
                H.td $ H.toHtml val) entries) dict

instance ToMarkup InputSummaryMarkup where
    toMarkup (InputSummaryMarkup string) = H.preEscapedToHtml $ replace "\n" "<br/>" string

instance ToMarkup (Either String InputSummary) where
    toMarkup (Left err) = do
        H.toHtml $ ("Simulation Summary could not be completed due to:" :: String)
        H.br
        H.pre $ H.toHtml $ show err
    toMarkup (Right ass) = H.toHtml ass

class ToString a where
    toString :: a -> String
    --toString a = show a

data InputSummaryMarkup = InputSummaryMarkup String deriving (Show)

instance ToString (Either String InputSummary) where
    toString (Left err) = err
    toString (Right summary) = toString summary

instance ToString InputSummary where
    toString (InputSummary dict) =
        concatMap (\(secHead,entries)->
            secHead <> (concat
                        $ intersperse "\n  " 
                        $ map (\(t,s)-> t <> ": " <> toString s) entries)) dict

instance ToString InputSummaryMarkup where
    toString (InputSummaryMarkup s) = s

-- summariseInputData :: NamelistFile -> InputSummary
summariseInputData fdsData = InputSummary
    [ ("General",
      [ ("CHID", InputSummaryMarkup $ chid)
      , ("Simulation Time", InputSummaryMarkup $ format (show simulationInterval) <> " s")
      ])
    , ("Fire",
      [ ("# Burners", InputSummaryMarkup $ show nBurners)
      , ("Total Max. HRR", InputSummaryMarkup $  (format $ printf "%.2f" totalMaxHRR) <> " kW")
      , ("Heat of Combustion", InputSummaryMarkup $ (format $ printf "%.2f" hoC) <> (" kJ/kg" :: String))
      , ("Total Max. Soot Production Rate", InputSummaryMarkup $ printf "%.2e" sootRate <> (" kg/s" ::String))
      ])
    , ("Sprinklers",
      [ ("# Sprinklers", InputSummaryMarkup $ show nSprinklers)
      , ("Sprinkler Activation Temperatures", InputSummaryMarkup $ if nSprinklers == 0 then "N/A" else intercalate "," $ map (\x->show x <> " C") sprinklerActivationTemperatures) -- TODO: sort in order of proximity to fire.
      ])
    , ("Detection",
      [ ("# Smoke Detectors", InputSummaryMarkup $ show nSmokeDetectors)
      , ("Smoke Detector Obscurations", InputSummaryMarkup $ if nSmokeDetectors == 0 then "N/A" else  intercalate "," $ map (\x->show x <> " %Obs/m") smokeDetectorObscurations)
      ])
    , ("Ventilation",
      [ ("# Exhaust Vents", InputSummaryMarkup $ show nExhausts)
      , ("Total Exhaust Rate", InputSummaryMarkup $ (show exhaustRate) <> (" m^3/s" :: String))
      , ("# Supply Vents", InputSummaryMarkup $ show nSupplies)
      , ("Total Supply Rate", InputSummaryMarkup $ (show supplyRate) <> (" m^3/s" :: String))
      ])
    , ("Domain",
      [ ("# Meshes", InputSummaryMarkup $ show nMeshes)
      , ("# Cells", InputSummaryMarkup $ format $ show nCells)
      , ("Mesh Resolutions", InputSummaryMarkup $ mconcat $ intersperse "\n" $ map htmlResolution resolutions)
      , ("Non-Dimensionalised Ratio", InputSummaryMarkup $ mconcat $ intersperse "\n" $ map (printf "%.2f") ndrs)
      ])
    ]
    where
      chid = getCHID fdsData
      (tStart,tEnd) = getSimTimes fdsData
      simulationInterval = tEnd - tStart
      totalMaxHRR = getTotalMaxHRR fdsData
      nBurners = length $ getBurners fdsData
        
      ndrs = getNDRs fdsData
      nExhausts = length $ getExhausts fdsData
      exhaustRate = sum $ map abs $ fmap (getFlowRate fdsData) $ getExhausts fdsData


      nSupplies = length $ getSupplies fdsData
      supplyRate = sum $ map abs $ fmap (getFlowRate fdsData) $ getSupplies fdsData

      meshes = findNamelists fdsData "MESH"
      nMeshes = length meshes
      nCells = sum $ fmap getNCells meshes
      (sX, sY, sZ) = getSmallestResolution fdsData
      
      resolutions = nub $ getOrderedResolutions fdsData
      htmlResolution (x, y, z) =
            (show x) <> ("m x " :: String)
            <> (show y) <> ("m x " :: String)
            <> (show z) <> ("m" :: String)

      nSprinklers = length $ getSprinklers fdsData
      sprinklerActivationTemperatures = nub $ mapMaybe (sprinklerActivationTemperature fdsData) $ getSprinklers fdsData
      nSmokeDetectors= length $ getSmokeDetectors fdsData
      smokeDetectorObscurations =  nub $ mapMaybe (smokeDetectorObscuration fdsData) $ getSmokeDetectors fdsData

      nThermalDetectors= length $ getThermalDetectors fdsData

      hoC = getHoC fdsData

      sootRate = getSootProductionRate fdsData


-- format :: (Num a, Show a) => a -> String
format x = h++t
    where
        sp = break (== '.') $ x
        h = reverse (intercalate "," $ chunksOf 3 $ reverse $ fst sp)
        t = snd sp
