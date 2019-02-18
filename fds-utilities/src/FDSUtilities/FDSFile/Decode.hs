{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.FDSFile.Decode where

import qualified Data.Array as A
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Vector as V

import           FDSUtilities.FDSFile.Types
import           FDSUtilities.FDSFile.Utilities
import           FDSUtilities.FDSFile.NamelistFunctions
import           FDSUtilities.Parsing
import           FDSUtilities.Types
import           FDSUtilities.Types.Assess
import           Text.Namelist

import           Debug.Trace
import qualified Data.Text as T
import           Data.Monoid

import           Text.Regex
import           Text.Parsec.Pos (initialPos)

import qualified System.IO.Strict as StrictIO
import           System.Directory

simpleSurf surfId = Namelist "SURF" "" (M.fromList
    [("ID", ParString surfId)
    ])
    (initialPos "Test Input")

-- |Convert a @NamelistFile@ to an @FDSFile@.
decodeNamelistFile :: NamelistFile -> FDSFile
decodeNamelistFile nmlFile =
    foldl' decodeNamelist initFDSFile $ nmlFile_namelists nmlFile
    where
        inertSurf = decodeSurf (simpleSurf "INERT")
        openSurf = decodeSurf (simpleSurf "OPEN")
        hvacSurf = decodeSurf (simpleSurf "HVAC")
        initFDSFile = FDSFile
            { fdsFile_Head = Nothing
            , fdsFile_Time = Nothing
            , fdsFile_Dump = Nothing
            , fdsFile_Misc = Nothing
            , fdsFile_Meshes = []
            , fdsFile_Reacs = []
            , fdsFile_Devcs = []
            , fdsFile_Matls = []
            , fdsFile_Surfs = [inertSurf, openSurf, hvacSurf]
            , fdsFile_Obsts = []
            , fdsFile_Holes = []
            , fdsFile_Hvacs = []
            , fdsFile_Vents = []
            , fdsFile_Bndfs = []
            , fdsFile_Isofs = []
            , fdsFile_Slcfs = []
            , fdsFile_Ramps = []
            , fdsFile_Props = []
            , fdsFile_Parts = []
            , fdsFile_Trnxs = []
            , fdsFile_Trnys = []
            , fdsFile_Trnzs = []
            , fdsFile_unknownNamelists = []
            }

decodeNamelist :: FDSFile -> Namelist -> FDSFile
decodeNamelist fdsData nml = case nml_name nml of
    "OBST" -> decodeObst fdsData nml
    "VENT" -> decodeVent fdsData nml
    "DEVC" -> decodeDevc fdsData nml
    "PART" -> decodePart fdsData nml
    "TIME" -> decodeTime fdsData nml
    "PROP" -> decodeProp fdsData nml
    "SURF" -> decodeSurfInto fdsData nml
    "MESH" -> decodeMesh fdsData nml
    "SLCF" -> decodeSlcf fdsData nml
    "REAC" -> decodeReac fdsData nml
    "HVAC" -> decodeHvac fdsData nml
    "DUMP" -> decodeDump fdsData nml
    "MISC" -> decodeMisc fdsData nml
    "HEAD" -> decodeHead fdsData nml
    _ -> decodeUnknown fdsData nml

decodeUnknown fdsData nml =
    fdsData { fdsFile_unknownNamelists = nml:(fdsFile_unknownNamelists fdsData)}

decodeObst fdsData nml =
    let obst = Obst
            { obst_ALLOW_VENT = fromMaybe True $ parToBool <$> getParameterMaybe nml "ALLOW_VENT"
            , obst_BNDF_FACE = fromMaybe (False, False, False, False, False, False)
               $ parTo6 parToBool <$> getParameterMaybe nml "BNDF_FACE"
            , obst_BNDF_OBST = fromMaybe True $ parToBool <$> getParameterMaybe nml "BNDF_OBST"
            , obst_BULK_DENSITY = parToDouble <$> getParameterMaybe nml "BULK_DENSITY"
            , obst_COLOR = parToString <$> getParameterMaybe nml "COLOR"
            , obst_CTRL_ID = parToString <$> getParameterMaybe nml "CTRL_ID"
            , obst_DEVC_ID = parToString <$> getParameterMaybe nml "DEVC_ID"
            , obst_EVACUATION = fromMaybe False $ parToBool <$> getParameterMaybe nml "EVACUATION"
            , obst_FYI = parToString <$> getParameterMaybe nml "FYI"
            , obst_HT3D = fromMaybe False $ parToBool <$> getParameterMaybe nml "HT3D"
            , obst_ID = parToString <$> getParameterMaybe nml "ID"
            , obst_MATL_ID = parToString <$> getParameterMaybe nml "MATL_ID"
            , obst_MESH_ID = parToString <$> getParameterMaybe nml "MESH_ID"
            , obst_MULT_ID = parToString <$> getParameterMaybe nml "MULT_ID"
            , obst_OUTLINE = fromMaybe False $ parToBool <$> getParameterMaybe nml "OUTLINE"
            , obst_OVERLAY = fromMaybe True $ parToBool <$> getParameterMaybe nml "OVERLAY"
            , obst_PERMIT_HOLE = fromMaybe True $ parToBool <$> getParameterMaybe nml "PERMIT_HOLE"
            , obst_PROP_ID = parToString <$> getParameterMaybe nml "PROP_ID"
            , obst_REMOVABLE = fromMaybe True $ parToBool <$> getParameterMaybe nml "REMOVABLE"
            , obst_RGB = parToRGB <$> getParameterMaybe nml "RGB"
            , obst_SURF_ID = parToString <$> getParameterMaybe nml "SURF_ID"
            , obst_SURF_ID6 = parTo6String <$> getParameterMaybe nml "SURF_ID6"
            , obst_SURF_IDS = parTo3String <$> getParameterMaybe nml "SURF_IDS"
            , obst_TEXTURE_ORIGIN = XYZ 0 0 0
            , obst_THICKEN = fromMaybe False $ parToBool <$> getParameterMaybe nml "THICKEN"
            , obst_TRANSPARENCY = 1
            , obst_XB = fromMaybe (error "No XB value") $ getXBMaybe nml
            }
    in fdsData { fdsFile_Obsts = obst:(fdsFile_Obsts fdsData)}




decodeVent fdsData nml =
    let vent = Vent
            { vent_COLOR = parToString <$> getParameterMaybe nml "COLOR"
            -- , vent_CTRL_ID :: String
            -- , vent_DEVC_ID :: String
            -- , vent_DYNAMIC_PRESSURE :: Double
            -- , vent_EVACUATION :: Bool
            -- , vent_FYI :: String
            , vent_ID = parToString <$> getParameterMaybe nml "ID"
            -- , vent_IOR :: Int
            -- , vent_L_EDDY :: Double
            -- , vent_L_EDDY_IJ :: [Int]
            -- , vent_MB :: String
            -- , vent_MESH_ID :: String
            -- , vent_MULT_ID :: String
            -- , vent_N_EDDY :: Int
            -- , vent_OUTLINE :: Bool
            -- , vent_PBX :: Double
            -- , vent_PBY :: Double
            -- , vent_PBZ :: Double
            -- , vent_PRESSURE_RAMP :: String
            -- , vent_RADIUS :: Double
            -- , vent_REYNOLDS_STRESS :: [Double]
            -- , vent_RGB :: RGB
            -- , vent_SPREAD_RATE :: Double
            , vent_SURF_ID = parToString <$> getParameterMaybe nml "SURF_ID"
            -- , vent_TEXTURE_ORIGIN :: [Double]
            -- , vent_TMP_EXTERIOR :: Double
            -- , vent_TMP_EXTERIOR_RAMP :: String
            -- , vent_TRANSPARENCY :: Double
            -- , vent_UVW :: [Double]
            -- , vent_VEL_RMS :: Double
            -- -- , vent_WIND :: String
            , vent_XB = parToXB <$> getParameterMaybe nml "XB"
            -- , vent_XYZ :: XYZ
            }
    in fdsData { fdsFile_Vents = vent:(fdsFile_Vents fdsData)}

parToList tranform (ParArray arr) = fmap tranform $ M.elems arr

parToRGB (ParArray arr) =
    let [a,b,c] = M.elems arr
    in RGB (parToInt a) (parToInt b) (parToInt c)

parToXYZ par =
    let (x,y,z) = parTo3 parToDouble par
    in XYZ x y z

parToIJK par =
    let (i,j,k) = parTo3 parToInt par
    in IJK i j k

parToXB par =
    let (x1,x2,y1,y2,z1,z2) = parTo6 parToDouble par
    in XB x1 x2 y1 y2 z1 z2

parTo3String (ParArray arr) =
    let [a,b,c] = M.elems arr
    in (parToString a, parToString b, parToString c)

parTo2 tranform (ParArray arr) =
    let [a, b] = fmap tranform $ M.elems arr
    in (a, b)

parTo3 tranform (ParArray arr) =
    let [a, b, c] = fmap tranform $ M.elems arr
    in (a, b, c)

parTo6 tranform (ParArray arr) =
    let [a,b,c,d,e,f] = fmap tranform $ M.elems arr
    in (a, b, c, d, e, f)

parTo6String = parTo6 parToString


decodeDevc :: FDSFile -> Namelist -> FDSFile
decodeDevc fdsData nml =
    let
        devcId = parToString <$> getParameterMaybe nml "ID"
        devc = Devc
            { devc_BYPASS_FLOWRATE = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "BYPASS_FLOWRATE"
            , devc_CONVERSION_ADDEND = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "CONVERSION_ADDEND"
            , devc_CONVERSION_FACTOR = fromMaybe 1 $ parToDouble <$> getParameterMaybe nml "CONVERSION_FACTOR"
            , devc_COORD_FACTOR = fromMaybe 1 $ parToDouble <$> getParameterMaybe nml "COORD_FACTOR"
            , devc_CTRL_ID = Nothing
            , devc_DELAY = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "DELAY"
            , devc_DEPTH = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "DEPTH"
            , devc_DEVC_ID = parToString <$> getParameterMaybe nml "DEVC_ID"
            , devc_DRY = fromMaybe False $ parToBool <$> getParameterMaybe nml "DRY"
            , devc_DUCT_ID = parToString <$> getParameterMaybe nml "DUCT_ID"
            , devc_EVACUATION = fromMaybe False $ parToBool <$> getParameterMaybe nml "EVACUATION"
            , devc_FLOWRATE = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "FLOWRATE"
            , devc_FYI = parToString <$> getParameterMaybe nml "FYI"
            , devc_HIDE_COORDINATES = fromMaybe False $ parToBool <$> getParameterMaybe nml "HIDE_COORDINATES"
            , devc_ID = parToString <$> getParameterMaybe nml "ID"
            , devc_INITIAL_STATE = fromMaybe False $ parToBool <$> getParameterMaybe nml "INITIAL_STATE"
            , devc_INIT_ID = Nothing
            , devc_IOR = Nothing
            , devc_LATCH = True
            , devc_MATL_ID = Nothing
            , devc_NODE_ID = []
            , devc_NO_UPDATE_DEVC_ID = Nothing
            , devc_NO_UPDATE_CTRL_ID = Nothing
            , devc_ORIENTATION = fromMaybe (XYZ 0 0 1) $ parToXYZ <$> getParameterMaybe nml "ORIENTATION"
            , devc_ORIENTATION_NUMBER = fromMaybe 1 $ parToInt <$> getParameterMaybe nml "ORIENTATION_NUMBER"
            , devc_OUTPUT = fromMaybe True $ parToBool <$> getParameterMaybe nml "OUTPUT"
            , devc_PART_ID = parToString <$> getParameterMaybe nml "PART_ID"
            , devc_PIPE_INDEX = fromMaybe 1 $ parToInt <$> getParameterMaybe nml "PIPE_INDEX"
            , devc_POINTS = fromMaybe 1 $ parToInt <$> getParameterMaybe nml "POINTS"
            , devc_PROP_ID = parToString <$> getParameterMaybe nml "PROP_ID"
            , devc_QUANTITY = parToString <$> getParameterMaybe nml "QUANTITY"
            , devc_QUANTITY2 = parToString <$> getParameterMaybe nml "QUANTITY2"
            , devc_QUANTITY_RANGE = fromMaybe (-1e50,1e50) $ parTo2 parToDouble <$> getParameterMaybe nml "QUANTITY_RANGE"
            , devc_R_ID = parToString <$> getParameterMaybe nml "R_ID"
            , devc_REAC_ID = parToString <$> getParameterMaybe nml "REAC_ID"
            , devc_RELATIVE = fromMaybe False $ parToBool <$> getParameterMaybe nml "RELATIVE"
            , devc_ROTATION = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "ROTATION"
            , devc_SETPOINT = parToDouble <$> getParameterMaybe nml "SETPOINT"
            , devc_SMOOTHING_FACTOR = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "SMOOTHING_FACTOR"
            , devc_SPEC_ID = parToString <$> getParameterMaybe nml "SPEC_ID"
            , devc_STATISTICS = parToString <$> getParameterMaybe nml "STATISTICS"
            , devc_STATISTICS_START = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "STATISTICS_START" -- TODO: T_BEGIN is the default, how do we get that?
            , devc_SURF_ID = parToString <$> getParameterMaybe nml "SURF_ID"
            , devc_TIME_AVERAGED = fromMaybe True $ parToBool <$> getParameterMaybe nml "TIME_AVERAGED"
            , devc_TIME_HISTORY = fromMaybe False $ parToBool <$> getParameterMaybe nml "TIME_HISTORY"
            , devc_TRIP_DIRECTION = fromMaybe 1 $ parToInt <$> getParameterMaybe nml "TRIP_DIRECTION"
            , devc_UNITS = parToString <$> getParameterMaybe nml "UNITS"
            , devc_VELO_INDEX = fromMaybe 0 $ parToInt <$> getParameterMaybe nml "VELO_INDEX"
            , devc_XB = parToXB <$> getParameterMaybe nml "XB"
            , devc_XYZ = parToXYZ <$> getParameterMaybe nml "XYZ"
            , devc_X_ID = (\n->n<>"-x") <$> devcId
            , devc_Y_ID = (\n->n<>"-y") <$> devcId
            , devc_Z_ID = (\n->n<>"-z") <$> devcId
            , devc_XYZ_UNITS = "m"
            }
    in fdsData { fdsFile_Devcs = devc:(fdsFile_Devcs fdsData)}

decodeMesh :: FDSFile -> Namelist -> FDSFile
decodeMesh fdsData nml =
    let
        -- devcId = parToString <$> getParameterMaybe nml "ID"
        mesh = Mesh
            { mesh_ID = parToString <$> getParameterMaybe nml "ID"
            , mesh_XB = fromMaybe (XB 0 1 0 1 0 1) $ parToXB <$> getParameterMaybe nml "XB"
            , mesh_IJK = fromMaybe (IJK 10 10 10) $ parToIJK <$> getParameterMaybe nml "IJK"
            , mesh_COLOR = fromMaybe "BLACK" $ parToString <$> getParameterMaybe nml "COLOR"
            , mesh_CYLINDRICAL = fromMaybe False $ parToBool <$> getParameterMaybe nml "CYLINDRICAL"
            , mesh_EVACUATION = fromMaybe False $ parToBool <$> getParameterMaybe nml "EVACUATION"
            , mesh_EVAC_HUMANS = fromMaybe False $ parToBool <$> getParameterMaybe nml "EVAC_HUMANS"
            , mesh_EVAC_Z_OFFSET = fromMaybe 1 $ parToDouble <$> getParameterMaybe nml "EVAC_Z_OFFSET"
            , mesh_FYI = parToString <$> getParameterMaybe nml "FYI"
            , mesh_LEVEL = fromMaybe 1 $ parToInt <$> getParameterMaybe nml "LEVEL"
            , mesh_MPI_PROCESS = parToInt <$> getParameterMaybe nml "LEVEL"
            , mesh_MULT_ID = parToString <$> getParameterMaybe nml "FYI"
            , mesh_RGB = fromMaybe (RGB 0 0 0) $ parToRGB <$> getParameterMaybe nml "RGB"
            , mesh_N_THREADS = parToInt <$> getParameterMaybe nml "N_THREADS"
            -- , mesh_PERIODIC_MESH_IDS :: [Text]
            }
    in fdsData { fdsFile_Meshes = mesh:(fdsFile_Meshes fdsData)}

decodePart :: FDSFile -> Namelist -> FDSFile
decodePart fdsData nml =
    let
        part = Part
            { part_AGE = fromMaybe 1e5 $ parToDouble <$> getParameterMaybe nml "AGE"
            , part_BREAKUP = fromMaybe False $ parToBool <$> getParameterMaybe nml "BREAKUP"
            , part_BREAKUP_CNF_RAMP_ID = parToString <$> getParameterMaybe nml "BREAKUP_CNF_RAMP_ID"
            , part_BREAKUP_DISTRIBUTION = fromMaybe "ROSIN.." $ parToString <$> getParameterMaybe nml "BREAKUP_CNF_RAMP_ID"
            , part_BREAKUP_GAMMA_D = fromMaybe 2.4 $ parToDouble <$> getParameterMaybe nml "BREAKUP_GAMMA_D"
            , part_BREAKUP_RATIO = fromMaybe (3/7) $ parToDouble <$> getParameterMaybe nml "BREAKUP_RATIO"
            , part_BREAKUP_SIGMA_D = parToDouble <$> getParameterMaybe nml "BREAKUP_SIGMA_D"
            , part_CHECK_DISTRIBUTION = fromMaybe False $ parToBool <$> getParameterMaybe nml "CHECK_DISTRIBUTION"
            , part_CNF_RAMP_ID = parToString <$> getParameterMaybe nml "CNF_RAMP_ID"
            , part_COLOR = fromMaybe "BLACK" $ parToString <$> getParameterMaybe nml "COLOR"
            , part_COMPLEX_REFRACTIVE_INDEX = fromMaybe 0.01 $ parToDouble <$> getParameterMaybe nml "COMPLEX_REFRACTIVE_INDEX"
            , part_CTRL_ID = parToString <$> getParameterMaybe nml "CTRL_ID"
            , part_DENSE_VOLUME_FRACTION = fromMaybe 1e-5 $ parToDouble <$> getParameterMaybe nml "DENSE_VOLUME_FRACCTION"
            , part_DEVC_ID = parToString <$> getParameterMaybe nml "DEVC_ID"
            , part_DIAMETER = parToDouble <$> getParameterMaybe nml "DIAMETER"
            , part_DISTRIBUTION = fromMaybe "ROSIN..." $ parToString <$> getParameterMaybe nml "DISTRIBUTION"
            , part_DRAG_COEFFICIENT = fromMaybe [] $ parToList parToDouble <$> getParameterMaybe nml "DRAG_COEFFICIENT"
            , part_DRAG_LAW = fromMaybe "SPHERE" $ parToString <$> getParameterMaybe nml "DRAG_LAW"
            , part_FREE_AREA_FRACTION = parToDouble <$> getParameterMaybe nml "FREE_AREA_FRACTION"
            , part_FYI = parToString <$> getParameterMaybe nml "FYI"
            , part_GAMMA_D  = fromMaybe 2.4 $ parToDouble <$> getParameterMaybe nml "GAMMA_D"
            , part_HEAT_OF_COMBUSTION = parToDouble <$> getParameterMaybe nml "HEAT_OF_COMBUSTION"
            , part_HORIZONTAL_VELOCITY  = fromMaybe 0.2 $ parToDouble <$> getParameterMaybe nml "HORIZONTAL_VELOCITY"
            , part_ID = parToString <$> getParameterMaybe nml "ID"
            -- , part_INITIAL_TEMPERATURE :: Double -- TMPA
            , part_MASSLESS = fromMaybe False $ parToBool <$> getParameterMaybe nml "MASSLESS"
            , part_MAXIMUM_DIAMETER = fromMaybe (1/0 {- Infinity -}) $ parToDouble <$> getParameterMaybe nml "MAXIMUM_DIAMETER"
            , part_MINIMUM_DIAMETER = fromMaybe 20 $ parToDouble <$> getParameterMaybe nml "MINIMUM_DIAMETER"
            , part_MONODISPERSE = fromMaybe False $ parToBool <$> getParameterMaybe nml "MONODISPERSE"
            , part_N_STRATA = fromMaybe 6 $ parToInt <$> getParameterMaybe nml "N_STRATA"
            -- , part_ORIENTATION :: [Double]
            -- , part_PERMEABILITY :: [Double]
            , part_PERIODIC_X = fromMaybe False $ parToBool <$> getParameterMaybe nml "PERIODIC_X"
            , part_PERIODIC_Y = fromMaybe False $ parToBool <$> getParameterMaybe nml "PERIODIC_Y"
            , part_PERIODIC_Z = fromMaybe False $ parToBool <$> getParameterMaybe nml "PERIODIC_Z"
            , part_POROUS_VOLUME_FRACTION = parToDouble <$> getParameterMaybe nml "POROUS_VOLUME_FRACTION"
            , part_PROP_ID = parToString <$> getParameterMaybe nml "PROP_ID"
            -- , part_QUANTITIES :: [String]
            -- , part_QUANTITIES_SPEC_ID :: [String]
            , part_RADIATIVE_PROPERTY_TABLE = parToDouble <$> getParameterMaybe nml "RADIATIVE_PROPERTY_TABLE"
            , part_REAL_REFRACTIVE_INDEX = fromMaybe 1.33 $ parToDouble <$> getParameterMaybe nml "REAL_REFRACTIVE_INDEX"
            , part_RGB = parToRGB <$> getParameterMaybe nml "RGB"
            , part_RUNNING_AVERAGE_FACTOR = fromMaybe 0.5 $ parToDouble <$> getParameterMaybe nml "RUNNING_AVERAGE_FACTOR"
            , part_SAMPLING_FACTOR = fromMaybe 1 $ parToInt <$> getParameterMaybe nml "SAMPLING_FACTOR"
            , part_SECOND_ORDER_PARTICLE_TRANSPORT = fromMaybe False $ parToBool <$> getParameterMaybe nml "SECOND_ORDER_PARTICLE_TRANSPORT"
            , part_SIGMA_D = parToDouble <$> getParameterMaybe nml "SIGMA_D"
            , part_SPEC_ID = parToString <$> getParameterMaybe nml "SPEC_ID"
            , part_STATIC = fromMaybe False $ parToBool <$> getParameterMaybe nml "STATIC"
            , part_SURFACE_TENSION = fromMaybe 7.28e-2 $ parToDouble <$> getParameterMaybe nml "SURFACE_TENSION"
            , part_SURF_ID = parToString <$> getParameterMaybe nml "SURF_ID"
            -- , part_TARGET_ONLY :: Bool
            , part_TURBULENT_DISPERSION = fromMaybe False $ parToBool <$> getParameterMaybe nml "AGE"
            , part_VERTICAL_VELOCITY = fromMaybe 0.5 $ parToDouble <$> getParameterMaybe nml "VERTICAL_VELOCITY"
            }
    in fdsData { fdsFile_Parts = part:(fdsFile_Parts fdsData)}

decodeSurfInto fdsData nml =
    let surf = decodeSurf nml
    in fdsData { fdsFile_Surfs = surf:(fdsFile_Surfs fdsData)}

decodeSurf nml = Surf
            { surf_ADIABATIC = fromMaybe False $ parToBool <$> getParameterMaybe nml "ADIABATIC"
            , surf_AUTO_IGNITION_TEMPERATURE = fromMaybe (-273) $ parToDouble <$> getParameterMaybe nml "AUTO_IGNITION_TEMPERATURE"
            , surf_BACKING = fromMaybe "EXPOSED" $ parToString <$> getParameterMaybe nml "BACKING"
            , surf_BURN_AWAY = fromMaybe False $ parToBool <$> getParameterMaybe nml "BACKING"
            , surf_CELL_SIZE_FACTOR = fromMaybe 1.0 $ parToDouble <$> getParameterMaybe nml "CELL_SIZE_FACTOR"
            , surf_C_FORCED_CONSTANT = fromMaybe 0.0 $ parToDouble <$> getParameterMaybe nml "C_FORCED_CONSTANT"
            , surf_C_FORCED_PR_EXP = fromMaybe 0.0 $ parToDouble <$> getParameterMaybe nml "C_FORCED_PR_EXP"
            , surf_C_FORCED_RE = fromMaybe 0.0 $ parToDouble <$> getParameterMaybe nml "C_FORCED_RE"
            , surf_C_FORCED_RE_EXP = fromMaybe 0.0 $ parToDouble <$> getParameterMaybe nml "C_FORCED_RE_EXP"
            , surf_C_HORIZONTAL = fromMaybe 1.52 $ parToDouble <$> getParameterMaybe nml "C_HORIZONTAL"
            , surf_C_VERTICAL = fromMaybe 1.31 $ parToDouble <$> getParameterMaybe nml "C_VERTICAL"
            , surf_COLOR = parToString <$> getParameterMaybe nml "COLOR"
            -- , surf_CONVECTION_LENGTH_SCALE :: Double
            -- , surf_CONVECTIVE_HEAT_FLUX :: Double
            -- , surf_CONVERT_VOLUME_TO_MASS :: Bool
            -- , surf_DEFAULT :: Bool
            -- , surf_DT_INSERT :: Double
            -- , surf_EMISSIVITY :: Double
            -- , surf_EMISSIVITY_BACK :: Double
            -- , surf_EVAC_DEFAULT :: Bool
            -- , surf_EXTERNAL_FLUX :: Double
            -- , surf_E_COEFFICIENT :: Double
            -- , surf_FIRELINE_MLR_MAX :: Double
            -- , surf_FREE_SLIP :: Bool
            -- , surf_FYI :: String
            -- , surf_GEOMETRY :: String
            -- , surf_HEAT_OF_VAPORIZATION :: Double
            -- , surf_HEAT_TRANSFER_COEFFICIENT :: Double
            -- , surf_HEAT_TRANSFER_COEFFICIENT_BACK :: Double
            -- , surf_HEAT_TRANSFER_MODEL :: String
            , surf_HRRPUA = parToDouble <$> getParameterMaybe nml "HRRPUA"
            -- , surf_HT3D :: Bool
            , surf_ID = parToString <$> getParameterMaybe nml "ID"
            -- , surf_IGNITION_TEMPERATURE :: Double
            -- , surf_INNER_RADIUS :: Double
            -- , surf_INTERNAL_HEAT_SOURCE :: [Double]
            -- , surf_LAYER_DIVIDE :: Double
            -- , surf_LEAK_PATH :: [Int]
            -- , surf_LENGTH :: Double
            , surf_MASS_FLUX = Nothing -- parToDouble <$> getParameterMaybe nml "MASS_FLUX"
            , surf_MASS_FLUX_TOTAL = parToDouble <$> getParameterMaybe nml "MASS_FLUX_TOTAL"
            , surf_MASS_FLUX_VAR = parToDouble <$> getParameterMaybe nml "MASS_FLUX_VAR"
            -- , surf_MASS_FRACTION :: [Double]
            -- , surf_MASS_TRANSFER_COEFFICIENT :: Double
            -- , surf_MATL_ID :: [String]
            -- , surf_MATL_MASS_FRACTION :: [Double]
            -- , surf_MINIMUM_LAYER_THICKNESS :: Double
            , surf_MLRPUA = parToDouble <$> getParameterMaybe nml "MLRPUA"
            -- , surf_N_LAYER_CELLS_MAX :: [Int]
            -- , surf_NET_HEAT_FLUX :: Double
            -- , surf_NO_SLIP :: Bool
            -- , surf_NPPC :: Int
            -- , surf_PARTICLE_MASS_FLUX :: Double
            -- , surf_PART_ID :: String
            -- , surf_PLE :: Double
            -- , surf_PROFILE :: String
            -- , surf_RADIUS :: Double
            -- , surf_RAMP_EF :: String
            -- , surf_RAMP_MF :: [String]
            -- , surf_RAMP_PART :: String
            , surf_RAMP_Q = parToString <$> getParameterMaybe nml "RAMP_Q"
            , surf_RAMP_T = parToString <$> getParameterMaybe nml "RAMP_T"
            -- , surf_RAMP_T_I :: Maybe String
            -- , surf_RAMP_V :: Maybe String
            -- , surf_RAMP_V_X :: Maybe String
            -- , surf_RAMP_V_Y :: Maybe String
            -- , surf_RAMP_V_Z :: Maybe String
            -- , surf_RGB :: RGB
            -- , surf_ROUGHNESS :: Double
            -- , surf_SPEC_ID :: String
            -- , surf_SPREAD_RATE :: Double
            -- , surf_STRETCH_FACTOR :: [Double]
            -- , surf_TAU_EF :: Double
            -- , surf_TAU_MF :: Double
            -- , surf_TAU_PART :: Double
            , surf_TAU_Q = fromMaybe 1 $ parToDouble <$> getParameterMaybe nml "TAU_Q"
            -- , surf_TAU_T :: Double
            -- , surf_TAU_V :: Double
            -- , surf_TEXTURE_HEIGHT :: Double
            -- , surf_TEXTURE_MAP :: String
            -- , surf_TEXTURE_WIDTH :: Double
            -- , surf_TGA_ANALYSIS :: Bool
            -- , surf_TGA_FINAL_TEMPERATURE :: Double
            -- , surf_TGA_HEATING_RATE :: Double
            -- , surf_THICKNESS :: [Double]
            -- , surf_TMP_BACK :: Double
            -- , surf_TMP_FRONT :: Double
            -- , surf_TMP_INNER :: [Double]
            -- , surf_TRANSPARENCY :: Double
            -- , surf_VEGETATION :: Bool
            -- , surf_VEGETATION_ARRHENIUS_DEGRAD :: Bool
            -- , surf_VEGETATION_CDRAG :: Double
            -- , surf_VEGETATION_CHAR_FRACTION :: Double
            -- , surf_VEGETATION_ELEMENT_DENSITY :: Int
            -- , surf_VEGETATION_GROUND_TEMP :: Double
            -- , surf_VEGETATION_HEIGHT :: Double
            -- , surf_VEGETATION_INITIAL_TEMP :: Double
            -- , surf_VEGETATION_LAYERS :: Int
            -- , surf_VEGETATION_LINEAR_DEGRAD :: Bool
            -- , surf_VEGETATION_LOAD :: Double
            -- , surf_VEGETATION_LSET_IGNITE_TIME :: Double
            -- , surf_VEG_LSET_QCON :: Double
            -- , surf_VEGETATION_MOISTURE :: Double
            -- , surf_VEGETATION_NO_BURN :: Bool
            -- , surf_VEGETATION_SVRATIO :: Int
            -- , surf_VEG_LEVEL_SET_SPREAD :: Bool
            -- , surf_VEG_LSET_ROS_BACK :: Double
            -- , surf_VEG_LSET_ROS_FLANK :: Double
            -- , surf_VEG_LSET_ROS_HEAD :: Double
            -- , surf_VEG_LSET_WIND_EXP :: Double
            -- , surf_VEG_LSET_SIGMA :: Double
            -- , surf_VEG_LSET_HT :: Double
            -- , surf_VEG_LSET_BETA :: Double
            -- , surf_VEG_LSET_ELLIPSE :: Double
            -- , surf_VEG_LSET_TAN2 :: Bool
            -- , surf_VEG_LSET_ELLIPSE_HEAD :: Double
            , surf_VEL = parToDouble <$> getParameterMaybe nml "VEL"
            -- , surf_VEL_BULK :: Double
            -- , surf_VEL_GRAD :: Double
            , surf_VEL_T = parTo2 parToDouble <$> getParameterMaybe nml "VEL_T"
            , surf_VOLUME_FLOW = parToDouble <$> getParameterMaybe nml "VOLUME_FLOW"
            -- , surf_WIDTH :: Double
            -- , surf_XYZ :: XYZ
            -- , surf_Z0 :: Double
            }

decodeProp fdsData nml =
    let
        prop = Prop
            { prop_ACTIVATION_OBSCURATION = fromMaybe 3.24 $ parToDouble <$> getParameterMaybe nml "ACTIVATION_OBSCURATION"
            , prop_ACTIVATION_TEMPERATURE = fromMaybe 74 $ parToDouble <$> getParameterMaybe nml "ACTIVATION_TEMPERATURE"
            -- , prop_ALPHA_C :: Double
            -- , prop_ALPHA_E :: Double
            -- , prop_BEAD_DENSITY :: Double
            -- , prop_BEAD_DIAMETER :: Double
            -- , prop_BEAD_EMISSIVITY :: Double
            -- , prop_BEAD_HEAT_TRANSFER_COEFFICIENT :: Double
            -- , prop_BEAD_SPECIFIC_HEAT :: Double
            -- , prop_BETA_C :: Double
            -- , prop_BETA_E :: Double
            -- -- , prop_FED_ACTIVITY :: String
            -- , prop_CHARACTERISTIC_VELOCITY :: Double
            -- , prop_C_FACTOR :: Double
            -- , prop_DENSITY :: Double
            -- , prop_DIAMETER :: Double
            -- , prop_DROPLET_VELOCITY :: Double
            -- , prop_EMISSIVITY :: Double
            -- , prop_FLOW_RAMP :: String
            -- , prop_FLOW_RATE :: Double
            -- , prop_FLOW_TAU :: Double
            -- , prop_FYI :: String
            -- , prop_GAUGE_EMISSIVITY :: Double
            -- , prop_GAUGE_TEMPERATURE :: Double
            -- , prop_HEAT_TRANSFER_COEFFICIENT :: Double
            , prop_ID = parToString <$> getParameterMaybe nml "ID"
            -- , prop_INITIAL_TEMPERATURE :: Double
            -- , prop_K_FACTOR :: Double
            -- , prop_LENGTH :: Double
            -- , prop_MASS_FLOW_RATE :: Double
            -- , prop_OFFSET :: Double
            -- , prop_OPERATING_PRESSURE :: Double
            -- , prop_ORIFICE_DIAMETER :: Double
            -- , prop_P0 :: String
            -- , prop_PARTICLES_PER_SECOND :: Int
            -- , prop_PARTICLE_VELOCITY :: Double
            -- , prop_PART_ID :: String
            -- , prop_PDPA_END :: Double
            -- , prop_PDPA_HISTOGRAM :: Bool
            -- , prop_PDPA_HISTOGRAM_LIMITS :: [Double]
            -- , prop_PDPA_HISTOGRAM_NBINS :: Int
            -- , prop_PDPA_HISTOGRAM_CUMULATIVE :: Bool
            -- , prop_PDPA_INTEGRATE :: Bool
            -- , prop_PDPA_M :: Int
            -- , prop_PDPA_N :: Int
            -- , prop_PDPA_NORMALIZE :: Bool
            -- , prop_PDPA_RADIUS :: Double
            -- , prop_PDPA_START :: Double
            -- , prop_PRESSURE_RAMP :: String
            -- -- , prop_PX :: String
            -- -- , prop_PXX :: String
            , prop_QUANTITY = parToString <$> getParameterMaybe nml "QUANTITY"
            -- , prop_RTI :: Double
            -- , prop_SMOKEVIEW_ID :: [String]
            -- , prop_SMOKEVIEW_PARAMETERS :: [String]
            -- , prop_SPEC_ID :: String
            -- , prop_SPRAY_ANGLE :: [Double]
            -- , prop_SPRAY_PATTERN_BETA :: Double
            -- , prop_SPRAY_PATTERN_MU :: Double
            -- , prop_SPRAY_PATTERN_SHAPE :: String
            -- , prop_SPRAY_PATTERN_TABLE :: String
            -- , prop_VELOCITY_COMPONENT :: Int
            -- -- , prop_DROPLET_VELOCITY :: String
            }
    in fdsData { fdsFile_Props = prop:(fdsFile_Props fdsData)}

decodeSlcf fdsData nml =
    let
        slcf = Slcf
            { slcf_CELL_CENTERED = fromMaybe False $ parToBool <$> getParameterMaybe nml "CELL_CENTERED"
            , slcf_EVACUATION = fromMaybe False $ parToBool <$> getParameterMaybe nml "EVACUATION"
            -- -- , slcf_FACE_CENTERED :: String
            -- -- , slcf_FIRE_LINE :: String
            -- , slcf_FYI :: String
            , slcf_ID = parToString <$> getParameterMaybe nml "ID"
            -- , slcf_IOR :: Int
            -- , slcf_LEVEL_SET_FIRE_LINE :: String
            -- , slcf_MAXIMUM_VALUE :: Double
            -- , slcf_MESH_NUMBER :: Int
            -- , slcf_MINIMUM_VALUE :: Double
            -- , slcf_PART_ID :: String
            , slcf_PBX = parToDouble <$> getParameterMaybe nml "PBX"
            , slcf_PBY = parToDouble <$> getParameterMaybe nml "PBY"
            , slcf_PBZ = parToDouble <$> getParameterMaybe nml "PBZ"
            -- -- , slcf_PROP_ID :: String
            , slcf_QUANTITY = parToString <$> getParameterMaybe nml "QUANTITY"
            , slcf_QUANTITY2 = parToString <$> getParameterMaybe nml "QUANTITY2"
            -- , slcf_REAC_ID :: String
            -- -- , slcf_SLICETYPE :: String
            , slcf_SPEC_ID = parToString <$> getParameterMaybe nml "SPEC_ID"
            -- , slcf_VECTOR :: Bool
            -- , slcf_VELO_INDEX :: Int
            -- , slcf_XB :: XB
            }
    in fdsData { fdsFile_Slcfs = slcf:(fdsFile_Slcfs fdsData)}

decodeHvac fdsData nml =
    let
        hvac = Hvac
            { hvac_AIRCOIL_ID = parToString <$> getParameterMaybe nml "AIRCOIL_ID"
            -- , hvac_AMBIENT :: Bool
            -- , hvac_AREA :: Double
            -- , hvac_CLEAN_LOSS :: Double
            -- , hvac_COOLANT_SPECIFIC_HEAT :: Double
            -- , hvac_COOLANT_MASS_FLOW :: Double
            -- , hvac_COOLANT_TEMPERATURE :: Double
            -- , hvac_CTRL_ID :: String
            -- , hvac_DAMPER :: Bool
            -- , hvac_DEVC_ID :: String
            -- , hvac_DIAMETER :: Double
            -- , hvac_DUCT_ID :: [String]
            -- , hvac_DUCT_INTERP_TYPE :: String
            -- , hvac_EFFICIENCY :: [Double]
            -- , hvac_FAN_ID :: String
            -- , hvac_FILTER_ID :: String
            -- , hvac_FIXED_Q :: [Double]
            -- , hvac_ID :: String
            -- , hvac_LEAK_ENTHALPY :: Bool
            -- , hvac_LENGTH :: Double
            -- , hvac_LOADING :: [Double]
            -- , hvac_LOADING_MULTIPLIER :: [Double]
            -- , hvac_LOSS :: [Double]
            -- , hvac_MASS_FLOW :: Double
            -- , hvac_MAX_FLOW :: Double
            -- , hvac_MAX_PRESSURE :: Double
            -- , hvac_N_CELLS :: Int
            -- , hvac_NODE_ID :: [String]
            -- , hvac_PERIMETER :: Double
            -- , hvac_RAMP_ID :: String
            -- , hvac_RAMP_LOSS :: String
            -- , hvac_REVERSE :: Bool
            -- , hvac_ROUGHNESS :: Double
            -- , hvac_SPEC_ID :: String
            -- , hvac_TAU_AC :: Double
            -- , hvac_TAU_FAN :: Double
            -- , hvac_TAU_VF :: Double
            -- , hvac_TYPE_ID :: String
            , hvac_VENT_ID = parToString <$> getParameterMaybe nml "VENT_ID"
            , hvac_VENT2_ID = parToString <$> getParameterMaybe nml "VENT2_ID"
            -- , hvac_VOLUME_FLOW :: Double
            -- , hvac_XYZ :: XYZ
            }
    in fdsData { fdsFile_Hvacs = hvac:(fdsFile_Hvacs fdsData)}

decodeReac fdsData nml =
    let
        reac = Reac
            { reac_A = parToDouble <$> getParameterMaybe nml "A"
            -- -- , reac_ALT_REAC_ID :: String
            -- , reac_AUTO_IGNITION_TEMPERATURE :: Double
            , reac_C = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "C"
            -- , reac_CHECK_ATOM_BALANCE :: Bool
            , reac_CO_YIELD = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "CO_YIELD"
            -- , reac_CRITICAL_FLAME_TEMPERATURE :: Double
            -- , reac_E :: Double
            , reac_EPUMO2 = fromMaybe 13100 $ parToDouble <$> getParameterMaybe nml "EPUMO2"
            -- -- , reac_K :: String
            -- , reac_EQUATION :: String
            -- , reac_FIXED_MIX_TIME :: Double
            -- -- , reac_FLAME_SPEED :: String
            -- -- , reac_FLAME_SPEED_EXPONENT :: String
            -- -- , reac_FLAME_SPEED_TEMPERATURE :: String
            -- , reac_FORMULA :: String
            -- , reac_FUEL :: String
            -- , reac_FUEL_RADCAL_ID :: String
            -- -- , reac_FWD_ID :: String
            , reac_FYI = parToString <$> getParameterMaybe nml "FYI"
            , reac_H = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "H"
            -- , reac_HEAT_OF_COMBUSTION :: Double
            , reac_ID = parToString <$> getParameterMaybe nml "ID"
            -- , reac_IDEAL :: Bool
            , reac_N = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "N"
            -- , reac_NU :: [Double]
            -- , reac_N_S :: [Double]
            -- , reac_N_T :: Double
            , reac_O = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "O"
            -- -- , reac_ODE_SOLVER :: String
            -- , reac_RADIATIVE_FRACTION :: Double
            -- , reac_RAMP_CHI_R :: String
            -- -- , reac_RAMP_FS :: String
            -- , reac_REAC_ATOM_ERROR :: Double
            -- , reac_REAC_MASS_ERROR :: Double
            -- -- , reac_REVERSE :: String
            , reac_SOOT_H_FRACTION = fromMaybe 0.1 $ parToDouble <$> getParameterMaybe nml "SOOT_H_FRACTION"
            , reac_SOOT_YIELD = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "SOOT_YIELD"
            -- , reac_SPEC_ID_N_S :: [String]
            -- , reac_SPEC_ID_NU :: [String]
            -- -- , reac_TABLE_FS :: String
            -- -- , reac_TAU_CHEM :: String
            -- -- , reac_TAU_FLAME :: String
            -- , reac_THIRD_BODY :: Bool
            -- -- , reac_TURBULENT_FLAME_SPEED_ALPHA :: String
            -- -- , reac_TURBULENT_FLAME_SPEED_EXPONENT :: String
            -- -- , reac_Y_P_MIN_EDC :: String
            }
    in fdsData { fdsFile_Reacs = reac:(fdsFile_Reacs fdsData)}

decodeMisc fdsData nml =
    let
        misc = Misc
            { misc_ALLOW_SURFACE_PARTICLES = fromMaybe False $ parToBool <$> getParameterMaybe nml "ALLOW_SURFACE_PARTICLES"
            -- , misc_ALLOW_UNDERSIDE_PARTICLES :: Bool
            -- , misc_ASSUMED_GAS_TEMPERATURE :: Double
            -- , misc_ASSUMED_GAS_TEMPERATURE_RAMP :: String
            -- , misc_BAROCLINIC :: Bool
            -- , misc_BNDF_DEFAULT :: Bool
            -- , misc_CC_IBM :: Bool
            -- , misc_CNF_CUTOFF :: Double
            -- , misc_CFL_MAX :: Double
            -- , misc_CFL_MIN :: Double
            -- , misc_CFL_VELOCITY_NORM :: Int
            -- , misc_CHECK_HT :: Bool
            -- , misc_CHECK_REALIZABILITY :: Bool
            -- , misc_CHECK_VN :: Bool
            -- , misc_CLIP_MASS_FRACTION :: Bool
            -- , misc_COMPUTE_VISCOSITY_TWICE :: Bool
            -- , misc_COMPUTE_ZETA_SOURCE_TERM :: Bool
            -- , misc_CONSTANT_H_SOLID :: Bool
            -- , misc_CONSTANT_SPECIFIC_HEAT_RATIO :: Bool
            -- , misc_CORIOLIS_VECTOR :: [Double]
            -- , misc_CORRECT_SUBGRID_TEMPERATURE :: Bool
            -- , misc_COUPLED_1D3D_HEAT_TRANSFER :: Bool
            -- , misc_C_DEARDORFF :: Double
            -- , misc_C_RNG :: Double
            -- , misc_C_RNG_CUTOFF :: Double
            -- , misc_C_SMAGORINSKY :: Double
            -- , misc_C_VREMAN :: Double
            -- , misc_DNS :: Bool
            -- , misc_DRAG_CFL_MAX :: Double
            -- , misc_DT_MEAN_FORCING :: Double
            -- , misc_ENTHALPY_TRANSPORT :: Bool
            -- , misc_EVACUATION_DRILL :: Bool
            -- , misc_EVACUATION_MC_MODE :: Bool
            -- , misc_EVAC_PRESSURE_ITERATIONS :: Int
            -- , misc_EVAC_SURF_DEFAULT :: String
            -- , misc_EVAC_TIME_ITERATIONS :: Int
            -- , misc_EVAPORATION :: Bool
            -- -- , misc_EXCHANGE_EDGES :: String
            -- , misc_EXTERNAL_BOUNDARY_CORRECTION :: Bool
            -- , misc_EXTINCTION_MODEL :: String
            -- , misc_HVAC_PRES_RELAX :: Double
            -- , misc_HT3D_TEST :: Int
            -- , misc_FDS5_OPTIONS :: Bool
            -- , misc_FLUX_LIMITER :: Int
            -- , misc_FORCE_VECTOR :: [Double]
            -- , misc_FREEZE_VELOCITY :: Bool
            -- , misc_FYI :: String
            -- , misc_GAMMA :: Double
            -- , misc_GRAVITATIONAL_DEPOSITION :: Bool
            -- , misc_GRAVITATIONAL_SETTLING :: Bool
            -- , misc_GROUND_LEVEL :: Double
            -- , misc_GVEC :: [Double]
            -- , misc_DT_HVAC :: Double
            -- , misc_H_F_REFERENCE_TEMPERATURE :: Double
            -- , misc_HRRPUV_MAX_SMV :: Double
            -- , misc_HUMIDITY :: Double
            -- , misc_HVAC_MASS_TRANSPORT :: Bool
            -- , misc_IBLANK_SMV :: Bool
            -- , misc_IMMERSED_BOUNDARY_METHOD :: Int
            -- , misc_INITIAL_UNMIXED_FRACTION :: Double
            -- -- , misc_KINETIC_ENERGY_SOURCE :: String
            -- , misc_LAPSE_RATE :: Double
            -- , misc_LES_FILTER_WIDTH :: String
            -- , misc_MAX_CHEMISTRY_ITERATIONS :: Int
            -- , misc_MAX_LEAK_PATHS :: Int
            , misc_MAXIMUM_VISIBILITY = fromMaybe 30 $ parToDouble <$> getParameterMaybe nml "MAXIMUM_VISIBILITY"
            -- , misc_MEAN_FORCING :: [Bool]
            -- , misc_MPI_TIMEOUT :: Double
            -- , misc_N_FIXED_CHEMISTRY_SUBSTEPS :: Int
            -- , misc_NEAR_WALL_TURBULENCE_MODEL :: String
            -- -- , misc_NEW_MOMENTUM_NUDGING :: String
            -- -- , misc_NEW_OPEN_BOUNDARY :: String
            -- , misc_NOISE :: Bool
            -- , misc_NOISE_VELOCITY :: Double
            -- , misc_NO_EVACUATION :: Bool
            -- , misc_NO_RAMPS :: Bool
            -- -- , misc_NORTHANGLE :: String
            -- , misc_OVERWRITE :: Bool
            -- , misc_PARTICLE_CFL_MAX :: Double
            -- , misc_PARTICLE_CFL_MIN :: Double
            -- , misc_PARTICLE_CFL :: Bool
            -- , misc_PERIODIC_TEST :: Int
            -- -- , misc_PROFILING :: String
            -- , misc_POROUS_FLOOR :: Bool
            -- -- , misc_POTENTIAL_TEMPERATURE_CORRECTION :: String
            -- , misc_PR :: Double
            -- , misc_PROCESS_ALL_MESHES :: Bool
            -- , misc_PROJECTION :: Bool
            -- , misc_P_INF :: Double
            -- -- , misc_RAMP_FVX_T :: String
            -- -- , misc_RAMP_FVY_T :: String
            -- -- , misc_RAMP_FVZ_T :: String
            -- , misc_RAMP_GX :: String
            -- , misc_RAMP_GY :: String
            -- , misc_RAMP_GZ :: String
            -- , misc_RAMP_U0 :: String
            -- , misc_RAMP_U0_T :: String
            -- , misc_RAMP_V0 :: String
            -- , misc_RAMP_V0_T :: String
            -- , misc_RAMP_W0 :: String
            -- , misc_RAMP_W0_T :: String
            -- , misc_RAMP_U0_Z :: String
            -- , misc_RAMP_V0_Z :: String
            -- , misc_RAMP_W0_Z :: String
            -- -- , misc_RADIATION :: String
            -- , misc_RESEARCH_MODE :: Bool
            -- , misc_RESTART :: Bool
            -- , misc_RESTART_CHID :: String
            -- , misc_RICHARDSON_ERROR_TOLERANCE :: Double
            -- , misc_RUN_AVG_FAC :: Double
            -- , misc_SC :: Double
            -- , misc_SECOND_ORDER_INTERPOLATED_BOUNDARY :: Bool
            -- , misc_SECOND_ORDER_PARTICLE_TRANSPORT :: Bool
            -- , misc_SHARED_FILE_SYSTEM :: Bool
            -- -- , misc_SLIP_CONDITION :: String
            -- , misc_SMOKE_ALBEDO :: Double
            -- , misc_SOLID_PHASE_ONLY :: Bool
            -- -- , misc_SOOT_OXIDATION :: String
            -- -- , misc_SPONGE_LAYER_DISTANCE :: String
            -- , misc_STRATIFICATION :: Bool
            -- , misc_SUPPRESSION :: Bool
            -- -- , misc_SURF_DEFAULT :: String
            -- -- , misc_TEMPERATURE_DEPENDENT_REACTION :: String
            -- -- , misc_TENSOR_DIFFUSIVITY :: String
            -- , misc_TERRAIN_CASE :: Bool
            -- , misc_TERRAIN_IMAGE :: String
            -- -- , misc_TEST_FILTER_QUADRATURE :: String
            -- , misc_TEXTURE_ORIGIN :: [Double]
            -- , misc_THERMOPHORETIC_DEPOSITION :: Bool
            -- , misc_THICKEN_OBSTRUCTIONS :: Bool
            -- -- , misc_TRANSPORT_UNMIXED_FRACTION :: String
            -- -- , misc_TRANSPORT_ZETA_SCHEME :: String
            -- , misc_TMPA :: Double
            -- , misc_TURBULENCE_MODEL :: String
            -- , misc_TURBULENT_DEPOSITION :: Bool
            -- -- , misc_TURB_INIT_CLOCK :: String
            -- , misc_U0 :: Double
            -- , misc_UVW_FILE :: String
            -- , misc_V0 :: Double
            -- , misc_VEG_LEVEL_SET_COUPLED :: Bool
            -- , misc_VEG_LEVEL_SET_UNCOUPLED :: Bool
            -- , misc_VERBOSE :: Double
            , misc_VISIBILITY_FACTOR = fromMaybe 3 $ parToDouble <$> getParameterMaybe nml "VISIBILITY_FACTOR"
            -- , misc_VN_MAX :: Double
            -- , misc_VN_MIN :: Double
            -- , misc_Y_CO2_INFTY :: Double
            -- , misc_Y_O2_INFTY :: Double
            -- , misc_W0 :: Double
            -- -- , misc_WD_PROPS :: String
            -- -- , misc_WIND_BOUNDARY :: String
            -- -- , misc_WIND_ONLY :: String
            }
    in fdsData { fdsFile_Misc = (Just misc)}

decodeTime fdsData nml =
    let
        time = Time
            { time_DT = parToDouble <$> getParameterMaybe nml "DT"
            -- , time_EVAC_DT_FLOWFIELD :: Double
            -- , time_EVAC_DT_STEADY_STATE :: Double
            , time_FYI = parToString <$> getParameterMaybe nml "FYI"
            -- , time_LIMITING_DT_RATIO :: Double
            -- , time_LOCK_TIME_STEP :: Bool
            -- , time_RESTRICT_TIME_STEP :: Bool
            , time_T_BEGIN = fromMaybe 0 $ parToDouble <$> getParameterMaybe nml "T_BEGIN"
            , time_T_END = fromMaybe 1 $ parToDouble <$> getParameterMaybe nml "T_END"
            -- , time_T_END_GEOM :: Double
            -- , time_TIME_SHRINK_FACTOR :: Double
            -- , time_WALL_INCREMENT :: Int
            -- , time_WALL_INCREMENT_HT3D :: Int
            -- , time_TWFIN :: Double
            }
    in fdsData { fdsFile_Time = (Just time)}

decodeHead fdsData nml =
    let
        head = Head
            { head_CHID = parToString <$> getParameterMaybe nml "CHID"
            , head_FYI = parToString <$> getParameterMaybe nml "FYI"
            , head_TITLE = parToString <$> getParameterMaybe nml "TITLE"
            }
    in fdsData { fdsFile_Head = (Just head)}

decodeDump fdsData nml =
    let
        dump = Dump
            { dump_CLIP_RESTART_FILES = fromMaybe True $ parToBool <$> getParameterMaybe nml "CLIP_RESTART_FILES"
            -- , dump_COLUMN_DUMP_LIMIT :: Bool
            -- , dump_CTRL_COLUMN_LIMIT :: Int
            -- , dump_DEVC_COLUMN_LIMIT :: Int
            -- , dump_DT_BNDE :: Double
            -- , dump_DT_BNDF :: Double
            -- , dump_DT_CPU :: Double
            -- , dump_DT_CTRL :: Double
            -- , dump_DT_DEVC :: Double
            -- , dump_DT_DEVC_LINE :: Double
            -- , dump_DT_FLUSH :: Double
            -- , dump_DT_GEOM :: Double
            -- , dump_DT_HRR :: Double
            -- , dump_DT_ISOF :: Double
            -- , dump_DT_MASS :: Double
            -- , dump_DT_PART :: Double
            -- , dump_DT_PL3D :: Double
            -- , dump_DT_PROF :: Double
            , dump_DT_RESTART = fromMaybe 1000000 $ parToDouble <$> getParameterMaybe nml "DT_RESTART"
            -- , dump_DT_SL3D :: Double
            -- , dump_DT_SLCF :: Double
            -- , dump_EB_PART_FILE :: Bool
            -- , dump_FLUSH_FILE_BUFFERS :: Bool
            -- , dump_GEOM_DIAG :: Bool
            -- , dump_MASS_FILE :: Bool
            -- , dump_MAXIMUM_PARTICLES :: Int
            -- , dump_MMS_TIMER :: Double
            , dump_NFRAMES = fromMaybe 1000 $ parToInt <$> getParameterMaybe nml "NFRAMES"
            -- , dump_PLOT3D_PART_ID :: [String]
            -- , dump_PLOT3D_QUANTITY :: [String]
            -- , dump_PLOT3D_SPEC_ID :: [String]
            -- , dump_PLOT3D_VELO_INDEX :: [Int]
            -- , dump_RENDER_FILE :: String
            -- , dump_SIG_FIGS :: Int
            -- , dump_SIG_FIGS_EXP :: Int
            -- , dump_SMOKE3D :: Bool
            -- , dump_SMOKE3D_QUANTITY :: String
            -- , dump_SMOKE3D_SPEC_ID :: String
            -- , dump_STATUS_FILES :: Bool
            -- , dump_SUPPRESS_DIAGNOSTICS :: Bool
            -- , dump_UVW_TIMER :: [Double]
            -- , dump_VELOCITY_ERROR_FILE :: Bool
            -- , dump_WRITE_XYZ :: Bool
            }
    in fdsData { fdsFile_Dump = (Just dump)}

addRestartToFile path = do
    exists <- doesFileExist path
    if exists
        then do
            inputScript <- StrictIO.readFile path
            let newInputScript = addRestart inputScript
            writeFile path newInputScript
        else error $ "addRestartToFile: " ++ path ++ " does not exist."

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
