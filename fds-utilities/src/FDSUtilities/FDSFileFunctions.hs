{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.FDSFileFunctions where

import qualified Data.Array as A
import           Data.List
import qualified Data.Map as M
import qualified Data.Vector as V

import           FDSUtilities.Parsing
import           FDSUtilities.Types
import           FDSUtilities.Types.Assess
import Text.Namelist

import           Debug.Trace
import qualified Data.Text as T
import Data.Monoid

import Text.Regex

import qualified System.IO.Strict as StrictIO
import System.Directory


addRestartToFile path = do
    exists <- doesFileExist path
    if exists
        then do
            inputScript <- StrictIO.readFile path
            let newInputScript = addRestart inputScript
            writeFile path newInputScript
        else error $ "addRestartToFile: " ++ path ++ " does not exist."

-- TODO: this belongs in FDSUtilities
addRestart inputScript =
    let
        restartExists = matchRegex
            (mkRegex "(RESTART=)(\\.FALSE\\.|\\.TRUE\\.)")
             inputScript
        miscExists = matchRegex (mkRegex "&MISC ") inputScript
    in case restartExists of
        Just _ -> subRegex
            (mkRegex "(RESTART=)(\\.FALSE\\.|\\.TRUE\\.)")
            inputScript
            "\\1.TRUE."
        Nothing -> case miscExists of
            Just _ -> subRegex
                (mkRegex "&MISC ")
                inputScript
                "&MISC RESTART=.TRUE. "
            Nothing ->
                -- add misc before first mesh
                subRegex
                    (mkRegex "&MESH ")
                    inputScript
                    "&MISC RESTART=.TRUE. /\n\n&MESH "

addRestartDTToFile dt path = do
    exists <- doesFileExist path
    if exists
        then do
            inputScript <- StrictIO.readFile path
            let newInputScript = addRestartDT dt inputScript
            writeFile path newInputScript
        else error $ "addRestartDTToFile: " ++ path ++ " does not exist."

-- TODO: this belongs in FDSUtilities
addRestartDT dt inputScript =
    let
        restartDTExists = matchRegex
            (mkRegex "(DT_RESTART[\\ ]*=[\\ ]*)([+-]?[0-9]*[.]?[0-9]+)")
             inputScript
        dumpExists = matchRegex (mkRegex "&DUMP ") inputScript
    in case restartDTExists of
        Just _ -> subRegex
            (mkRegex "(DT_RESTART[\\ ]*=[\\ ]*)([+-]?[0-9]*[.]?[0-9]+)")
            inputScript
            ("\\2 " ++ show dt)
        Nothing -> case dumpExists of
            Just _ -> subRegex
                (mkRegex "&DUMP ")
                inputScript
                ("&DUMP DT_RESTART=" ++ show dt ++" ")
            Nothing ->
                -- add dump before first mesh
                subRegex
                    (mkRegex "&MESH ")
                    inputScript
                    ("&DUMP DT_RESTART=" ++ show dt ++ " /\n\n&MESH ")

getMeshes fdsData = findNamelists fdsData "MESH"

getNCells nml@(Namelist "MESH" _ parameters _) = i*j*k
    where
        (i,j,k) = getMeshIJK nml

getMeshIJK nml@(Namelist "MESH" _ parameters _) = (toInt i, toInt j,toInt k)
    where
        [i, j, k] = case getArrayToList "IJK" nml of
            [i,j,k] -> [i,j,k]
            xs -> error $ "wrong number of IJK elements, expected 3, got " ++ (show (length xs))

-- TODO: check that any float is a whole number first
toInt :: ParameterValue -> Int
toInt (ParInt i) = i
toInt (ParDouble d) = round d

getMeshSkew nml@(Namelist "MESH" _ parameters _) =
    let (dx,dy,dz) = getMeshResolution nml
    in (maximum [dx,dy,dz] / minimum [dx,dy,dz])
    where
        (i,j,k) = getMeshIJK nml

getMeshResolution nml@(Namelist "MESH" _ parameters _) =
    (x/(fromIntegral i),y/(fromIntegral j),z/(fromIntegral k))
    where
        (i,j,k) = getMeshIJK nml
        (x,y,z) = getDimensions nml

getMeshResolution _ = error "Incorrect namelist supplied."
getObstDimensions = getDimensions
getDimensions nml@(Namelist _ _ parameters _) = (x,y,z)
    where
        (x1,x2,y1,y2,z1,z2) = getXB nml
        x = x2 - x1
        y = y2 - y1
        z = z2 - z1

getXB nml@(Namelist _ _ parameters _) = (x1,x2,y1,y2,z1,z2)
    where
        [ x1, x2, y1, y2, z1, z2] =
            case getArrayToList "XB" nml of
                [a,b,c,d,e,f] -> map parToDouble [a,b,c,d,e,f]
                e -> error $ T.unpack $ "Failed to parse XB array: " <> T.unlines (map pprint e)

getObstResolutions fdsData obst =
    map getMeshResolution $ findObstMesh fdsData obst

getSmallestResolution fdsData = smallest
    where
        resolutions = map getMeshResolution $ getMeshes fdsData
        smallest = minimumBy compareResolutions resolutions

getOrderedResolutions fdsData = smallest
    where
        resolutions = map getMeshResolution $ getMeshes fdsData
        smallest = sortBy compareResolutions resolutions

compareResolutions (x1,y1,z1) (x2,y2,z2) = compare (x1*y1*z1) (x2*y2*z2)

findObstMesh fdsData obst = filter (isOverlap obst) meshes
    where
        meshes = getMeshes fdsData

-- |Test two Namelists with XBs and determine if they intersect in 3d.
nmlIntersect :: Namelist -> Namelist -> Bool
nmlIntersect nmlA nmlB = xbIntersect xbA xbB
    where
        xbA = getXB nmlA
        xbB = getXB nmlB

type XB = (Double, Double, Double, Double, Double, Double)
-- |Test if two XBs intersect (i.e. their bounding boxes). Two bounding boxes
-- intersect of all 3 dimensions have overlap. EQ is considered overlap.
xbIntersect :: XB -> XB -> Bool
xbIntersect xbA xbB = intersectX && intersectY && intersectZ
    where
        intersectX = x2A > x1B && x2B > x1A
        intersectY = y2A > y1B && y2B > y1A
        intersectZ = z2A > z1B && z2B > z1A

        (x1A,x2A,y1A,y2A,z1A,z2A) = sortXB xbA
        (x1B,x2B,y1B,y2B,z1B,z2B) = sortXB xbB

-- | Sort an XB such that x2>x1 y2>y1 and z2>z1.
sortXB :: XB -> XB
sortXB (x1',x2',y1',y2',z1',z2') = (x1,x2,y1,y2,z1,z2)
    where
        x1 = min x1' x2'
        x2 = max x1' x2'
        y1 = min y1' y2'
        y2 = max y1' y2'
        z1 = min z1' z2'
        z2 = max z1' z2'

-- |Test if the XBs of the two namelists overlap
isOverlap nmlA nmlB = not $ any id [cond1, cond2, cond3, cond4, cond5, cond6]
    where
        cond1 = x2A < x1B
        cond2 = x1A > x2B
        cond3 = y2A < y1B
        cond4 = y1A > y2B
        cond5 = z2A < z1B
        cond6 = z1A > z2B
        (x1A,x2A,y1A,y2A,z1A,z2A) = getXB nmlA --peform min/max
        (x1B,x2B,y1B,y2B,z1B,z2B) = getXB nmlB


getSprinklers fdsData = filter (isSprinkler fdsData)
    $ findNamelists fdsData "DEVC"

isSprinkler fdsData nml@(Namelist _ _ parameters _) =
    case getParameterMaybe nml "PROP_ID" of
         Nothing -> False
         Just propName -> propName `elem` (getSprinklerPropIds fdsData)

getSprinklerPropIds fdsData =
    fmap (flip getParameter "ID")
    $ filter (isSprinklerProp fdsData)
    $ findNamelists fdsData "PROP"

isSprinklerProp fdsData nml@(Namelist _ _ parameters _) =
    case getParameterMaybe nml "QUANTITY" of
        Just (ParString "SPRINKLER LINK TEMPERATURE") -> True
        _ -> False

isObst nml = (nml_name nml) == "OBST"

sprinklerActivationTemperature fdsData nml = do
    propId <- getParameterMaybe nml "PROP_ID"
    let prop = case filter (\p-> (getParameter p "ID") == propId)
                    $ findNamelists fdsData "PROP" of
            [] -> error "no matching prop"
            [x] -> x
            _ -> error "too many matching props"
    (ParDouble temp) <- getParameterMaybe prop "ACTIVATION_TEMPERATURE"
    return temp

getNDRs fdsData = map (ndr fdsData) burners
    where
        burners = getBurners fdsData

ndr fdsData burner = ndr
    where
        q = maxBurnerHRR fdsData burner
        resolutions = getObstResolutions fdsData burner
        nominalCellSizes = map getMaxCellSize resolutions
        nominalCellSize = maximum nominalCellSizes
        getMaxCellSize (x,y,z) = maximum [x,y,z]
        charFireDiameter = (q / (ambientDensity*ambientSpecificHeat*ambientTemperature*(sqrt g)))**(2/5)
        ndr = charFireDiameter/nominalCellSize
        calcs =
          [ "Min. Mesh Resolution: " ++ show nominalCellSize ++ " m"
          , "Non-Dimensionalised Ratio: " ++ show ndr
          ]
        ambientDensity = 1.205
        ambientSpecificHeat = 1.005
        ambientTemperature = 293.15
        g = 9.81


smokeDetectorObscuration fdsData nml = do
    propId <- getParameterMaybe nml "PROP_ID"
    let isProp p = getParameter p "ID" == propId
    let prop = case filter isProp $ findNamelists fdsData "PROP" of
            [] -> error "no matching prop"
            [x] -> x
            _ -> error "too many matching props"
    return $  case getParameterMaybe prop "ACTIVATION_OBSCURATION" of
        Nothing -> 3.24 -- FDS Default
        Just (ParDouble x) -> x

getSootProductionRate fdsData = y_s/hoc*hrr
    where
        [reac] = findNamelists fdsData "REAC"
        y_s = case getParameterMaybe reac "SOOT_YIELD" of
            Just (ParDouble x) -> x
        hoc = getHoC fdsData
        hrr = getTotalMaxHRR fdsData

-- To calculate the heat of combustion, deteremine the oxygen consumption
-- then multiply by EPUMO2
-- TODO: include the other methods of specifying HoC.
-- Investigate the minor difference between this and the .out file value
getHoC fdsData = v_o2*w_o2*epumo2/(v_F*w_F)
    where
        [reac] = findNamelists fdsData "REAC"

        y_s = case getParameterMaybe reac "SOOT_YIELD" of
            Just (ParDouble x) -> x
        y_co = case getParameterMaybe reac "CO_YIELD" of
            Just (ParDouble x) -> x
        soot_h_fraction = case getParameterMaybe reac "SOOT_H_FRACTION" of
            Just (ParDouble x) -> x
            Nothing -> 0 -- TODO: abstract defaults to a separate table
        epumo2 = case getParameterMaybe reac "EMPUMO2" of
            Just (ParDouble x) -> x
            Nothing -> 13100


        -- for fuel molecule CxHyOzNv
        (ParDouble x) = getParameter reac "C"
        (ParDouble y) = getParameter reac "H"
        (ParDouble z) = getParameter reac "O"
        (ParDouble v) = getParameter reac "N"

        w_c = 12.01 -- ^Molar mass of atomic carbon.
        w_h = 1.008 -- ^Molar mass of atomic hydrogen.
        w_o = 15.999 -- ^Molar mass of atomic oxygen.
        w_n = 14.007 -- ^Molar mass of atomic nitrogen.

        w_o2 = w_o*2
        w_co = 1*w_c + 1*w_o

        v_F = 1

        -- |Molar mass of fuel.
        w_F = x*w_c + y*w_h + z*w_o + v*w_n

        -- 'v_' represents molar fraction

        v_o2 = v_co2 + v_co/2 + v_h2o/2 - z/2

        v_co2 = x - v_co - (1-soot_h_fraction) * v_s

        v_h2o = y/2 - soot_h_fraction/2 * v_s

        v_co = w_F/w_co * y_co

        v_s = w_F/w_S * y_s

        v_n2 = v/2

        w_S = soot_h_fraction*w_h + (1-soot_h_fraction)*w_c

getSmokeDetectors fdsData =
    filter (isSmokeDetector fdsData) $ findNamelists fdsData "DEVC"

isSmokeDetector fdsData nml@(Namelist _ _ parameters _) =
    case getParameterMaybe nml "PROP_ID" of
        Nothing -> False
        Just propName -> propName `elem` (getSmokeDetectorPropIds fdsData)

getSmokeDetectorPropIds fdsData =
    fmap (flip getParameter "ID")
    $ filter (isSmokeDetectorProp fdsData)
    $ findNamelists fdsData "PROP"

isSmokeDetectorProp fdsData nml@(Namelist _ _ parameters _) =
    case getParameterMaybe nml "QUANTITY" of
       Just (ParString "CHAMBER OBSCURATION") -> True
       _ -> False

getThermalDetectors fdsData =
    filter (isThermalDetector fdsData)
    $ findNamelists fdsData "DEVC"

isThermalDetector fdsData nml@(Namelist _ _ parameters _) =
    case getParameterMaybe nml "PROP_ID" of
        Nothing -> False
        Just propName -> propName `elem` (getThermalDetectorPropIds fdsData)

getThermalDetectorPropIds fdsData =
    fmap (flip getParameter "ID")
    $ filter (isThermalDetectorProp fdsData)
    $ findNamelists fdsData "PROP"

isThermalDetectorProp fdsData nml@(Namelist _ _ parameters _) =
    case getParameterMaybe nml "QUANTITY" of
        Just (ParString "LINK TEMPERATURE") -> True
        _ -> False

getBurnerSurfaces fdsData
    = filter (\n-> (hasParameter n "HRRPUA")) $ findNamelists fdsData "SURF"

getExhaustSurfaces fdsData
    = filter isExhaustSurface $ findNamelists fdsData "SURF"

getSupplySurfaces fdsData
    = filter isSupplySurface $ findNamelists fdsData "SURF"

isExhaustSurface nml
  | hasParameter nml "VEL" =
      let flow = parToDouble $ getParameter nml "VEL"
      in flow >= 0
  | hasParameter nml "VOLUME_FLUX" =
      let flow = parToDouble $ getParameter nml "VOLUME_FLUX"
      in flow >= 0
  | hasParameter nml "VOLUME_FLOW" =
      let flow = getDouble $ getParameter nml "VOLUME_FLOW"
      in flow >= 0
  | otherwise = False

isSupplySurface nml
  | hasParameter nml "VEL" =
      let flow = parToDouble $ getParameter nml "VEL"
      in flow < 0
  | hasParameter nml "VOLUME_FLUX" =
      let flow = parToDouble $ getParameter nml "VOLUME_FLUX"
      in flow < 0
  | hasParameter nml "VOLUME_FLOW" =
      let flow = parToDouble $ getParameter nml "VOLUME_FLOW"
      in flow < 0
  | otherwise = False

getFlowRate fdsData nml = case flowRate of
  Just x -> x
  Nothing -> error $ T.unpack $ "mech vent does not have a flow rate:\n" <> pprint nml
  where
    flowRate = do
      surfIds <- case getParameterMaybe nml "SURF_ID" of
                      Just x -> Just [x]
                      Nothing -> case getParameterMaybe nml "SURF_IDS" of
                          Just x -> Just $ getArrayToList "SURF_IDS" nml
                          Nothing -> case getParameterMaybe nml "SURF_ID6" of
                              Just x -> Just $ getArrayToList "SURF_ID6" nml
                              Nothing -> Nothing
      let surfs =
              case (filter (\p-> (getParameter p "ID") `elem` surfIds)
                           $ findNamelists fdsData "SURF") of
              [] -> error "no matching surf"
              x -> x
          exSurfs = filter (\nml
                           -> hasParameter nml "VOLUME_FLOW"
                           || hasParameter nml "VOLUME_FLUX"
                           || hasParameter nml "VEL") surfs
          surf = case exSurfs of
              [x] -> x
              x -> error $ T.unpack $ "23423:\nsurfs: "
                         <> pprint surfIds <> "\nsurfs: " <> pprint x
      flow <- case getParameterMaybe surf "VOLUME_FLOW" of
                      Just x -> Just x
                      Nothing -> case getParameterMaybe surf "VOLUME_FLUX" of
                          Just x -> Just x
                          Nothing -> Nothing
                          -- TODO: cover other types of flow spec
      return $ parToDouble flow



getTotalMaxHRR fdsData = sum $ map (getMaxHRR fdsData) $ getBurners fdsData


getBurnerObsts :: NamelistFile -> [Namelist]
getBurnerObsts fdsData
    = filter (isBurner fdsData) $ findNamelists fdsData "OBST"

getBurners fdsData = (filter (isBurner fdsData) $ findNamelists fdsData "OBST")
      ++ (filter (isBurner fdsData) $ findNamelists fdsData "VENT")

getExhausts fdsData =
    (filter (isExhaust fdsData) $ findNamelists fdsData "OBST")
        ++ (filter (isExhaust fdsData) $ findNamelists fdsData "VENT")

getSupplies fdsData = (filter (isSupply fdsData) $ findNamelists fdsData "OBST")
      ++ (filter (isSupply fdsData) $ findNamelists fdsData "VENT")

-- |Determines which sides of an obstruction are burner surfaces
-- returns Bools in the form (xMin,xMax,yMin,yMax,zMin,zMax)
getBurnerSideStatus :: NamelistFile -> Namelist -> (Bool, Bool, Bool, Bool, Bool, Bool)
getBurnerSideStatus fdsData nml = (xMinStatus, xMaxStatus,yMinStatus,yMaxStatus,zMinStatus,zMaxStatus)
  where
    burnerSurfNames = map (getIdString) $ getBurnerSurfaces fdsData
    (xMinSurfName,xMaxSurfName,yMinSurfName,yMaxSurfName,zMinSurfName,zMaxSurfName) = getObstSurfNames nml
    xMinStatus = xMinSurfName `elem` burnerSurfNames
    xMaxStatus = xMaxSurfName `elem` burnerSurfNames
    yMinStatus = yMinSurfName `elem` burnerSurfNames
    yMaxStatus = yMaxSurfName `elem` burnerSurfNames
    zMinStatus = zMinSurfName `elem` burnerSurfNames
    zMaxStatus = zMaxSurfName `elem` burnerSurfNames

getObstSurfNames :: Namelist -> (String, String, String, String, String, String)
getObstSurfNames obst = if hasParameter obst "SURF_ID"
      then let (ParString name) = getParameter obst "SURF_ID" in (T.unpack name, T.unpack name, T.unpack name, T.unpack name, T.unpack name, T.unpack name)
      else if hasParameter obst "SURF_IDS"
              then let [topName, sidesName, bottomName] = map parToString $ getArrayToList "SURF_IDS" obst in (sidesName, sidesName, sidesName, sidesName, bottomName, topName)
              else if hasParameter obst "SURF_ID6"
                      then let [xMinName,xMaxName,yMinName,yMaxName,zMinName,zMaxName] = map parToString $ getArrayToList "SURF_ID6" obst in (xMinName,xMaxName,yMinName,yMaxName,zMinName,zMaxName)
                                                                                                                          else error "Surfaces not parsed correctly. 7687"


getBurnerSurfNames :: NamelistFile -> Namelist -> [String]
getBurnerSurfNames fdsData nml@(Namelist _ _  parameters _)
    | hasParameter nml "SURF_IDS"
        = (intersect burnerSurfNames
        $ map getString
        $ getArrayToList "SURF_IDS" nml)
    | hasParameter nml "SURF_ID6"
        = (intersect burnerSurfNames
        $ map getString
        $ getArrayToList "SURF_ID6" nml)
    | hasParameter nml "SURF_ID" =
        let currentSurf = getString
                $ getParameter nml "SURF_ID"
        in if currentSurf `elem` burnerSurfNames then [currentSurf] else []
    | otherwise = []
    where
        burnerSurfNames = map (getIdString) $ getBurnerSurfaces fdsData

isBurner fdsData nml@(Namelist _ _ parameters _)
    | hasParameter nml "SURF_IDS"
        = 0 < (length
        $ intersect burnerSurfNames
        $ map getString
        $ getArrayToList "SURF_IDS" nml)
    | hasParameter nml "SURF_ID6"
        = 0 < (length
        $ intersect burnerSurfNames
        $ map getString
        $ getArrayToList "SURF_ID6" nml)
    | hasParameter nml "SURF_ID" =
        let currentSurf = getString
                $ getParameter nml "SURF_ID"
        in currentSurf `elem` burnerSurfNames
    | otherwise = False
    where
        burnerSurfNames = map (getIdString) $ getBurnerSurfaces fdsData

isExhaust fdsData nml@(Namelist _ _ parameters _)
    | hasParameter nml "SURF_IDS"
        = 0 < (length
        $ intersect exhaustSurfNames
        $ map getString
        $ getArrayToList "SURF_IDS" nml)
     | hasParameter nml "SURF_ID6"
        = 0 < (length
        $ intersect exhaustSurfNames
        $ map getString
        $ getArrayToList "SURF_ID6" nml)
    | hasParameter nml "SURF_ID" =
        let currentSurf = getString
                $ getParameter nml "SURF_ID"
        in currentSurf `elem` exhaustSurfNames
    | otherwise = False
    where
        exhaustSurfNames = map (getIdString) $ getExhaustSurfaces fdsData

isSupply fdsData nml@(Namelist _ _ parameters _)
    | hasParameter nml "SURF_IDS"
        = 0 < (length
        $ intersect supplySurfNames
        $ map getString
        $ getArrayToList "SURF_IDS" nml)
    | hasParameter nml "SURF_ID" =
        let currentSurf = getString
                $ getParameter nml "SURF_ID"
        in currentSurf `elem` supplySurfNames
    | otherwise = False
    where
        supplySurfNames = map (getIdString) $ getSupplySurfaces fdsData

parToDouble :: ParameterValue -> Double
parToDouble (ParInt x) = fromIntegral x
parToDouble (ParDouble x) = x

parToString :: ParameterValue -> String
parToString (ParString x) = T.unpack x

getIDBound nml = case getID nml of
    Just x -> x
    Nothing -> "Unnamed " ++ getNMLName nml


getID :: Namelist -> Maybe String
getID nml = do
    idParam <- getParameterMaybe nml "ID"
    return $ getString idParam

getNMLName (Namelist name _ _ _) = T.unpack name

sourceFroude q fuelArea =
   q/(ambientDensity*ambientSpecificHeat*
        ambientTemperature*(fuelDiameter**2)*(sqrt (g*fuelDiameter)))
    where
        ambientDensity = 1.205
        ambientSpecificHeat = 1.005
        ambientTemperature = 293.15
        g = 9.81
        fuelDiameter = sqrt ((4*fuelArea)/pi)

checkGrowthRate fdsData burner = evalGrowthRate hrrAlpha
    where
        (hrrCap, hrrAlpha) = burnerGrowthRate fdsData burner
maxBurnerHRR = getMaxHRR

burnerGrowthRate fdsData burner@(Namelist _ _ params _)
    = (hrrCap, hrrAlpha)
    where
        hrrCap = hrrpua * (burnerArea fdsData burner)
        hrrAlpha = hrrCap / ((abs tau_q)**2)
        surf = if isBurner fdsData burner
                    then getBurnerSurf fdsData burner
                    else error "this is not a burner"
        hrrpua = getDouble $ getParameter surf "HRRPUA"
        tau_q  = getDouble $ getParameter surf "TAU_Q"


getMaxHRR fdsData burner = maxHRR $ getHRR fdsData burner

getHRR fdsData burner =
   let surf = getBurnerSurf fdsData burner
       area = burnerArea fdsData burner
       nominalHRRPUA = getDouble $ getParameter surf "HRRPUA"
       tauq = getParameterMaybe surf "TAU_Q"
       rampq = getParameterMaybe surf "RAMP_Q"
   in case (tauq,rampq) of
      (Nothing, Nothing) -> HRRStatic (nominalHRRPUA*area)
      (Just t, Nothing) -> HRRTauQ (nominalHRRPUA*area) (getDouble t)
      (Nothing, Just r) -> HRRRamp
          $ map (\(t,f)->(t,nominalHRRPUA*area*f))
          $ getRamp fdsData (getString r)
      (Just _,  Just _) ->
          error "Both TAU_Q and RAMP_Q set for burner surface"
data HRR
    = HRRTauQ Double Double
    | HRRRamp [(Double, Double)]
    | HRRStatic Double

maxHRR :: HRR -> Double
maxHRR (HRRTauQ m t) = m
maxHRR (HRRRamp es) = maximum $ snd $ unzip es
maxHRR (HRRStatic hrr) = hrr

getRamp fdsData rampId =
    let rampEntries = findNamelists fdsData "RAMP"
        matchingRampEntries = filter
            (\nml-> (getString $ getParameter nml "ID") == rampId)
            rampEntries
        mkVal nml =
            let t = getDouble $ getParameter nml "T"
                f = getDouble $ getParameter nml "F"
            in (t,f)
        vals = map mkVal matchingRampEntries
    in vals

getBurnerSurf :: NamelistFile -> Namelist -> Namelist
getBurnerSurf fdsData nml@(Namelist _ _ parameters _)
    | hasParameter nml "SURF_IDS"
        = let currentSurfName = case matchingSurfNames of
                [] -> error "No surfaces"
                [h] -> h
                _ -> error "Surface defined multiple times"
              matchingSurfNames
                = intersect burnerSurfNames
                $ map getString
                $ getArrayToList "SURF_IDS" nml
              matches = filter
                    (\n-> currentSurfName == (getIdString n))
                    surfs
          in case matches of
                [] -> error "No burner surf matches"
                (x:xs) -> x
    | hasParameter nml "SURF_ID6"
        = let currentSurfName = case matchingSurfNames of
                [] -> error "No surfaces"
                [h] -> h
                _ -> error "Surface defined multiple times"
              matchingSurfNames
                = intersect burnerSurfNames
                $ map getString
                $ getArrayToList "SURF_ID6" nml
              matches = filter
                    (\n-> currentSurfName == (getIdString n))
                    surfs
          in case matches of
                [] -> error "No burner surf matches"
                (x:xs) -> x
    | hasParameter nml "SURF_ID" =
        let currentSurfName = getString
                $ getParameter nml "SURF_ID"
        in headErr "getBurnerSurf" $ filter
                  (\n-> currentSurfName == (getIdString n))
                  surfs
    | otherwise = error "Burner has no SURF parameter"
    where
        burnerSurfNames = map (\x->getString $ getParameter x "ID")
            $ getBurnerSurfaces fdsData
        surfs = findNamelists fdsData "SURF"

getIdString :: Namelist -> String
getIdString nml = getString $ getParameter nml "ID"

getIdStringMaybe :: Namelist -> Maybe String
getIdStringMaybe nml = getString <$> getParameterMaybe nml "ID"

getString :: ParameterValue -> String
getString = (\(ParString string) -> T.unpack string)
getDouble par = case par of
    (ParDouble d) -> d
    (ParInt i)    -> fromIntegral i

getSimTimes :: NamelistFile -> (Double, Double)
getSimTimes fdsData = (timeStart, timeEnd)
  where
    time = case findNamelists fdsData "TIME" of
        [] -> error $ "TIME not specified."
        [x] -> x
        _ -> error $ "Multiple TIMEs specified."
    timeStart = case getParameterMaybe time "T_BEGIN" of
        Nothing -> 0.0
        Just ((ParDouble x)) -> x
        Just ((ParInt x)) -> fromIntegral x
    timeEnd   = case getParameterMaybe time "T_END" of
        Nothing -> 1.0
        Just ((ParDouble x)) -> x
        Just ((ParInt x)) -> fromIntegral x

getCHID :: NamelistFile -> String
getCHID fdsData = chid
  where
    head = case findNamelists fdsData "HEAD" of
        [] -> error $ "HEAD not specified."
        [x] -> x
        _ -> error $ "Multiple HEADs specified."
    chid = case getParameterMaybe head "CHID" of
        Nothing -> error $ "No CHID specified."
        Just ((ParString x)) -> T.unpack x
        Just x -> error $ T.unpack $ "Invalid type for CHID: " <> pprint x

-- TODO: this function does not account for multiple burner
-- surface types on the one obstruction, deprecate this function
burnerArea :: NamelistFile -> Namelist -> Double
burnerArea fdsData nml = sum
                                  [ xMinSurfArea
                                  , xMaxSurfArea
                                  , yMinSurfArea
                                  , yMaxSurfArea
                                  , zMinSurfArea
                                  , zMaxSurfArea
                                  ]
    where
        parameters = nml_params nml
        (delX, delY, delZ) = getObstDimensions nml
        xArea = delY*delZ
        yArea = delX*delZ
        zArea = delX*delY
        xMinSurfArea =  if xMinStatus then xArea else 0
        xMaxSurfArea =  if xMaxStatus then xArea else 0
        yMinSurfArea =  if yMinStatus then yArea else 0
        yMaxSurfArea =  if yMaxStatus then yArea else 0
        zMinSurfArea =  if zMinStatus then zArea else 0
        zMaxSurfArea =  if zMaxStatus then zArea else 0
        (xMinStatus, xMaxStatus,yMinStatus,yMaxStatus,zMinStatus,zMaxStatus) = getBurnerSideStatus fdsData nml

-- |Given an obstruction and a particular surf name, find the
-- area of that type of surface contributed by the obstruction.
determineSurfAreaObst :: Namelist -> String -> Double
determineSurfAreaObst obst surfName = sum
                                  [ xMinSurfArea
                                  , xMaxSurfArea
                                  , yMinSurfArea
                                  , yMaxSurfArea
                                  , zMinSurfArea
                                  , zMaxSurfArea
                                  ]
  where
    (x1,x2,y1,y2,z1,z2)  = getXB obst
    delX = abs (x2-x1)
    delY = abs (y2-y1)
    delZ = abs (z2-z1)
    xArea = delY*delZ
    yArea = delX*delZ
    zArea = delX*delY
    xMinSurfArea =  if xMinSurfName == surfName then xArea else 0
    xMaxSurfArea =  if xMaxSurfName == surfName then xArea else 0
    yMinSurfArea =  if yMinSurfName == surfName then yArea else 0
    yMaxSurfArea =  if yMaxSurfName == surfName then yArea else 0
    zMinSurfArea =  if zMinSurfName == surfName then zArea else 0
    zMaxSurfArea =  if zMaxSurfName == surfName then zArea else 0
    (xMinSurfName,xMaxSurfName,yMinSurfName,yMaxSurfName,zMinSurfName,zMaxSurfName) = getObstSurfNames obst

-- |Determine the axis of orientation of a plane in space.
-- planeOrientation (x1,x2,y1,y2,x1,x2) =

hasParameter nml parameterName = M.member parameterName (nml_params nml)
getParameter nml parameterName =
    case M.lookup parameterName (nml_params nml) of
       Just x -> x
       Nothing -> error $ T.unpack $ "no such parameter: " <> parameterName <> " for: " <> pprint nml
getParameterMaybe :: Namelist -> T.Text -> Maybe ParameterValue
getParameterMaybe nml parameterName =
    M.lookup parameterName (nml_params nml)

hasParameterValue :: ToParameterValue a => T.Text -> a -> Namelist -> Bool
hasParameterValue parName parValue nml =
    let theValue = M.lookup parName (nml_params nml)
    in case theValue of
        Nothing -> False
        Just x -> x == (toParameterValue parValue)


getArrayToList parName nml = ls
    where
        Just (ParArray arr) = M.lookup parName (nml_params nml)
        ls = M.elems arr

-- TODO: make sure to take the closest one
-- the time of measurement is at the cap tiem
evalGrowthRate alpha
    | absDiff < 0.001 =
        Right (growthRate, (growthRateToAlpha growthRate) - alpha)
    | otherwise    = Left alpha
    where
        (absDiff, growthRate) = minimum $ map (growthRateDiff alpha) growthRates
        eps = 0.001
        eq = aEq eps

growthRateDiff alpha growthRate =
    (abs $ (growthRateToAlpha growthRate) - alpha, growthRate)

growthRates =
    [ NFPASlow
    , NFPAFast
    , NFPAMedium
    , NFPAUltrafast
    , EurocodeSlow
    , EurocodeMedium
    , EurocodeFast
    , EurocodeUltrafast
    ]

-- convertToProper :: ParameterValue
aEq eps a b = a < (b+eps) && a > (b-eps)

findNamelists (NamelistFile comments namelists) nameTarget =
    filter (\(Namelist name _ _ _) -> name == nameTarget) namelists
