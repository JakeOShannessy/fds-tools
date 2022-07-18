-- |This module defines data structures for FDS input data, and the functions
-- for converting them to and from Fortran Namelists. This is inherently less
-- flexible than the Namelist data, which can hold any data, but allows us to
-- operate more flexibly on FDS input data.
--
-- TODO: How do we deal with defaults? Should we leave a Maybe value? or should
-- we insert the defaults when we convert from the Namelists. Let's go with the
-- defaults, it allows us to ignore whether something is specified or not,
-- although it does mean we need to be aware of the version of FDS we are
-- considering. This decision is based on the fact that the primary purpose is
-- analysis of the values, not how it is specified. If we care about the
-- structure of the input itself, we can always fall back to the Namelist data.
{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.FDSFile.Types where

import Text.Namelist
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

-- class FromPS a where
--     fromPS :: a ->

-- |The Haskell data type representation of an FDS input script. The first items
-- (such as head and time) are single occurrence items. As any of these items
-- may or may not occur they are given maybe types. The other items may occur
-- zero, one or many times, and are therefore given a list type. There is
-- provision for storing namelists that are not understood for the purposes of
-- forward compatibility.
data FDSFile = FDSFile
    { fdsFile_Head :: (Maybe Head)
    , fdsFile_Time :: (Maybe Time)
    , fdsFile_Dump :: (Maybe Dump)
    , fdsFile_Misc :: (Maybe Misc)
    , fdsFile_Meshes :: [Mesh]
    , fdsFile_Reacs :: [Reac]
    , fdsFile_Devcs :: [Devc]
    , fdsFile_Matls :: [Matl]
    , fdsFile_Surfs :: [Surf]
    , fdsFile_Obsts :: [Obst]
    , fdsFile_Holes :: [Hole]
    , fdsFile_Hvacs :: [Hvac]
    , fdsFile_Vents :: [Vent]
    , fdsFile_Bndfs :: [Bndf]
    , fdsFile_Isofs :: [Isof]
    , fdsFile_Slcfs :: [Slcf]
    , fdsFile_Ramps :: [Ramp]
    , fdsFile_Props :: [Prop]
    , fdsFile_Parts :: [Part]
    , fdsFile_Trnxs :: [Trnx]
    , fdsFile_Trnys :: [Trny]
    , fdsFile_Trnzs :: [Trnz]
    , fdsFile_unknownNamelists :: [Namelist]
    } deriving (Show, Eq)

instance Default FDSFile where
    def = FDSFile
        { fdsFile_Head = Nothing
        , fdsFile_Time = Nothing
        , fdsFile_Dump = Nothing
        , fdsFile_Misc = Nothing
        , fdsFile_Meshes = []
        , fdsFile_Reacs = []
        , fdsFile_Devcs = []
        , fdsFile_Matls = []
        , fdsFile_Surfs = []
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

data Head = Head
    { head_CHID :: Maybe String -- ^CHID
    , head_FYI :: Maybe String -- ^FYI
    , head_TITLE :: Maybe String -- ^TITLE
    } deriving (Show, Eq)

instance Default Head where
    def =  Head
        { head_CHID = Nothing
        , head_FYI = Nothing
        , head_TITLE = Nothing
        }

data Bndf = Bndf
    { bndf_CELL_CENTRED :: Bool -- ^CELL_CENTERED --default: False
    , bndf_FYI :: Maybe String       -- ^FYI
    , bndf_PART_ID :: Maybe Text    -- ^PART_ID
    , bndf_PROP_ID :: Maybe Text    -- ^PROP_ID
    , bndf_RECOUNT_DRIP :: Bool -- ^RECOUNT_DRIP --default: False
    , bndf_QUANTITY :: Maybe Text   -- ^QUANTITY
    , bndf_SPEC_ID :: Maybe Text    -- ^SPEC_ID
    , bndf_STATISTICS :: Maybe Text    -- ^STATISTICS
    } deriving (Show, Eq)

instance Default Bndf where
    def = Bndf
        { bndf_CELL_CENTRED = False
        , bndf_FYI = Nothing
        , bndf_PART_ID = Nothing
        , bndf_PROP_ID = Nothing
        , bndf_RECOUNT_DRIP = False
        , bndf_QUANTITY = Nothing
        , bndf_SPEC_ID = Nothing
        , bndf_STATISTICS = Nothing
        }

data Bnde = Bnde
    { bnde_CELL_CENTERED :: Bool
    , bnde_FYI :: Maybe String
    , bnde_PART_ID :: Maybe String
    , bnde_PROP_ID :: Maybe String
    , bnde_QUANTITY :: Maybe String
    , bnde_SPEC_ID :: Maybe String
    } deriving (Show, Eq)

instance Default Bnde where
    def = Bnde
        { bnde_CELL_CENTERED = False
        , bnde_FYI = Nothing
        , bnde_PART_ID = Nothing
        , bnde_PROP_ID = Nothing
        , bnde_QUANTITY = Nothing
        , bnde_SPEC_ID = Nothing
        }

data Clip = Clip
    { clip_FYI :: Maybe String                        -- ^FYI
    , clip_MAXIMUM_DENSITY :: Double            -- ^MAXIMUM_DENSITY
    , clip_MAXIMUM_MASS_FRACTION :: [[Double]]  -- ^MAXIMUM_MASS_FRACTION
    , clip_MAXIMUM_TEMPERATURE :: Double        -- ^MAXIMUM_TEMPERATURE
    , clip_MINIMUM_DENSITY :: Double            -- ^MINIMUM_DENSITY
    , clip_MINIMUM_MASS_FRACTION :: [[Double]]  -- ^MINIMUM_MASS_FRACTION
    , clip_MINIMUM_TEMPERATURE :: Double        -- ^MINIMUM_TEMPERATURE
    }
    deriving (Show, Eq)

-- instance Default Clip where
--     def = Clip
--         { clip_FYI = ""
--         , clip_MAXIMUM_DENSITY :: Double            -- ^MAXIMUM_DENSITY
--         , clip_MAXIMUM_MASS_FRACTION :: [[Double]]  -- ^MAXIMUM_MASS_FRACTION
--         , clip_MAXIMUM_TEMPERATURE :: Double        -- ^MAXIMUM_TEMPERATURE
--         , clip_MINIMUM_DENSITY :: Double            -- ^MINIMUM_DENSITY
--         , clip_MINIMUM_MASS_FRACTION :: [[Double]]  -- ^MINIMUM_MASS_FRACTION
--         , clip_MINIMUM_TEMPERATURE :: Double        -- ^MINIMUM_TEMPERATURE
--         }

data Ctrl = Ctrl
    { ctrl_CONSTANT :: Double
    -- , ctrl_CYCLES :: String
    -- , ctrl_CYCLE_TIME :: String
    , ctrl_DELAY :: Double
    , ctrl_DIFFERENTIAL_GAIN :: Double
    , ctrl_EVACUATION :: Bool
    , ctrl_FUNCTION_TYPE :: String
    , ctrl_ID :: String
    , ctrl_INITIAL_STATE :: Bool
    , ctrl_INTEGRAL_GAIN :: Double
    , ctrl_INPUT_ID :: [String]
    , ctrl_LATCH :: Bool
    , ctrl_N :: Int
    , ctrl_ON_BOUND :: String
    , ctrl_PROPORTIONAL_GAIN :: Double
    , ctrl_RAMP_ID :: String
    , ctrl_SETPOINT :: Double
    , ctrl_TARGET_VALUE :: Double
    , ctrl_TRIP_DIRECTION :: Int
    }
    deriving (Show, Eq)

data Csvf = Csvf
    { csvf_CSVFILE :: Text
    , csvf_UVWFILE :: Text
    }
    deriving (Show, Eq)

data Devc = Devc
    { devc_BYPASS_FLOWRATE :: Double
    , devc_CONVERSION_ADDEND :: Double
    , devc_CONVERSION_FACTOR :: Double
    , devc_COORD_FACTOR :: Double
    , devc_CTRL_ID :: Maybe String
    , devc_DELAY :: Double
    , devc_DEPTH :: Double
    , devc_DEVC_ID :: Maybe String
    , devc_DRY :: Bool
    , devc_DUCT_ID :: Maybe String
    , devc_EVACUATION :: Bool
    , devc_FLOWRATE :: Double
    , devc_FYI :: Maybe String
    , devc_HIDE_COORDINATES :: Bool
    , devc_ID :: Maybe String
    , devc_INITIAL_STATE :: Bool
    , devc_INIT_ID :: Maybe String
    , devc_IOR :: Maybe Int
    , devc_LATCH :: Bool
    , devc_MATL_ID :: Maybe String
    , devc_NODE_ID :: [String]
    , devc_NO_UPDATE_DEVC_ID :: Maybe String
    , devc_NO_UPDATE_CTRL_ID :: Maybe String
    , devc_ORIENTATION :: XYZ
    , devc_ORIENTATION_NUMBER :: Int
    , devc_OUTPUT :: Bool
    , devc_PART_ID :: Maybe String
    , devc_PIPE_INDEX :: Int
    , devc_POINTS :: Int
    , devc_PROP_ID :: Maybe String
    , devc_QUANTITY :: Maybe String
    , devc_QUANTITY2 :: Maybe String
    , devc_QUANTITY_RANGE :: (Double, Double)
    , devc_R_ID :: Maybe String
    , devc_REAC_ID :: Maybe String
    , devc_RELATIVE :: Bool
    , devc_ROTATION :: Double
    , devc_SETPOINT :: Maybe Double
    , devc_SMOOTHING_FACTOR :: Double
    , devc_SPEC_ID :: Maybe String
    , devc_STATISTICS :: Maybe String
    , devc_STATISTICS_START :: Double
    , devc_SURF_ID :: Maybe String
    , devc_TIME_AVERAGED :: Bool
    , devc_TIME_HISTORY :: Bool
    , devc_TRIP_DIRECTION :: Int
    , devc_UNITS :: Maybe String
    , devc_VELO_INDEX :: Int
    , devc_XB :: Maybe XB
    , devc_XYZ :: Maybe XYZ
    , devc_X_ID :: Maybe String
    , devc_Y_ID :: Maybe String
    , devc_Z_ID :: Maybe String
    , devc_XYZ_UNITS :: String
    }
    deriving (Show, Eq)

data Dump = Dump
    { dump_CLIP_RESTART_FILES :: Bool
    , dump_COLUMN_DUMP_LIMIT :: Bool
    , dump_CTRL_COLUMN_LIMIT :: Int
    , dump_DEVC_COLUMN_LIMIT :: Int
    , dump_DT_BNDE :: Double
    , dump_DT_BNDF :: Double
    , dump_DT_CPU :: Double
    , dump_DT_CTRL :: Double
    , dump_DT_DEVC :: Double
    , dump_DT_DEVC_LINE :: Double
    , dump_DT_FLUSH :: Double
    , dump_DT_GEOM :: Double
    , dump_DT_HRR :: Double
    , dump_DT_ISOF :: Double
    , dump_DT_MASS :: Double
    , dump_DT_PART :: Double
    , dump_DT_PL3D :: Double
    , dump_DT_PROF :: Double
    , dump_DT_RESTART :: Double
    , dump_DT_SL3D :: Double
    , dump_DT_SLCF :: Double
    , dump_EB_PART_FILE :: Bool
    , dump_FLUSH_FILE_BUFFERS :: Bool
    , dump_GEOM_DIAG :: Bool
    , dump_MASS_FILE :: Bool
    , dump_MAXIMUM_PARTICLES :: Int
    , dump_MMS_TIMER :: Double
    , dump_NFRAMES :: Int
    , dump_PLOT3D_PART_ID :: [String]
    , dump_PLOT3D_QUANTITY :: [String]
    , dump_PLOT3D_SPEC_ID :: [String]
    , dump_PLOT3D_VELO_INDEX :: [Int]
    , dump_RENDER_FILE :: String
    , dump_SIG_FIGS :: Int
    , dump_SIG_FIGS_EXP :: Int
    , dump_SMOKE3D :: Bool
    , dump_SMOKE3D_QUANTITY :: String
    , dump_SMOKE3D_SPEC_ID :: String
    , dump_STATUS_FILES :: Bool
    , dump_SUPPRESS_DIAGNOSTICS :: Bool
    , dump_UVW_TIMER :: [Double]
    , dump_VELOCITY_ERROR_FILE :: Bool
    , dump_WRITE_XYZ :: Bool
    }
    deriving (Show, Eq)

data Hole = Hole
    { hole_COLOR :: String
    , hole_CTRL_ID :: String
    , hole_DEVC_ID :: String
    , hole_EVACUATION :: Bool
    , hole_FYI :: Maybe String
    , hole_ID :: String
    , hole_MESH_ID :: String
    , hole_MULT_ID :: String
    , hole_RGB :: RGB
    , hole_TRANSPARENCY :: Double
    , hole_XB :: XB
    }
    deriving (Show, Eq)

data Hvac = Hvac
    { hvac_AIRCOIL_ID :: Maybe String
    , hvac_AMBIENT :: Bool
    , hvac_AREA :: Double
    , hvac_CLEAN_LOSS :: Double
    , hvac_COOLANT_SPECIFIC_HEAT :: Double
    , hvac_COOLANT_MASS_FLOW :: Double
    , hvac_COOLANT_TEMPERATURE :: Double
    , hvac_CTRL_ID :: String
    , hvac_DAMPER :: Bool
    , hvac_DEVC_ID :: String
    , hvac_DIAMETER :: Double
    , hvac_DUCT_ID :: [String]
    , hvac_DUCT_INTERP_TYPE :: String
    , hvac_EFFICIENCY :: [Double]
    , hvac_FAN_ID :: String
    , hvac_FILTER_ID :: String
    , hvac_FIXED_Q :: [Double]
    , hvac_ID :: String
    , hvac_LEAK_ENTHALPY :: Bool
    , hvac_LENGTH :: Double
    , hvac_LOADING :: [Double]
    , hvac_LOADING_MULTIPLIER :: [Double]
    , hvac_LOSS :: [Double]
    , hvac_MASS_FLOW :: Double
    , hvac_MAX_FLOW :: Double
    , hvac_MAX_PRESSURE :: Double
    , hvac_N_CELLS :: Int
    , hvac_NODE_ID :: [String]
    , hvac_PERIMETER :: Double
    , hvac_RAMP_ID :: String
    , hvac_RAMP_LOSS :: String
    , hvac_REVERSE :: Bool
    , hvac_ROUGHNESS :: Double
    , hvac_SPEC_ID :: String
    , hvac_TAU_AC :: Double
    , hvac_TAU_FAN :: Double
    , hvac_TAU_VF :: Double
    , hvac_TYPE_ID :: String
    , hvac_VENT_ID :: Maybe String
    , hvac_VENT2_ID :: Maybe String
    , hvac_VOLUME_FLOW :: Double
    , hvac_XYZ :: XYZ
    }
    deriving (Show, Eq)

data Init = Init
    { init_AUTO_IGNITION_TEMPERATURE :: Double
    , init_CELL_CENTERED :: Bool
    , init_CTRL_ID :: String
    , init_DENSITY :: Double
    , init_DEVC_ID :: String
    , init_DIAMETER :: Double
    , init_DT_INSERT :: Double
    , init_DX :: Double
    , init_DY :: Double
    , init_DZ :: Double
    , init_HEIGHT :: Double
    , init_HRRPUV :: Double
    , init_ID :: String
    , init_MASS_FRACTION :: [Double]
    , init_MASS_PER_TIME :: Double
    , init_MASS_PER_VOLUME :: Double
    , init_MULT_ID :: String
    , init_N_PARTICLES :: Int
    , init_N_PARTICLES_PER_CELL :: Int
    , init_PART_ID :: String
    , init_RADIUS :: Double
    , init_SHAPE :: String
    , init_SPEC_ID :: [String]
    , init_TEMPERATURE :: Double
    , init_UVW :: [Double]
    , init_XB :: XB
    , init_XYZ :: XYZ
    , init_PARTICLE_WEIGHT_FACTOR :: Double
    , init_NUMBER_INITIAL_PARTICLES :: Int
    }
    deriving (Show, Eq)

data Isof = Isof
    { isof_FYI :: Maybe String
    , isof_QUANTITY :: String
    , isof_SPEC_ID :: String
    , isof_VALUE :: Double
    , isof_VELO_INDEX :: Int
    }
    deriving (Show, Eq)

----------------------------------------
data Matl = Matl
    { matl_A :: [Double]
    , matl_ABSORPTION_COEFFICIENT :: Double
    , matl_BOILING_TEMPERATURE :: Double
    , matl_COLOR :: String
    , matl_CONDUCTIVITY :: Double
    , matl_CONDUCTIVITY_RAMP :: String
    , matl_DENSITY :: Double
    , matl_E :: [Double]
    , matl_EMISSIVITY :: Double
    , matl_FYI :: Maybe String
    , matl_HEATING_RATE :: [Double]
    , matl_HEAT_OF_COMBUSTION :: [Double]
    , matl_HEAT_OF_REACTION :: [Double]
    , matl_ID :: String
    , matl_MATL_ID :: [String]
    , matl_NU_MATL :: [Double]
    , matl_NU_SPEC :: [Double]
    , matl_N_REACTIONS :: Int
    , matl_N_S :: [Double]
    , matl_N_T :: [Double]
    , matl_N_O2 :: [Double]
    , matl_PCR :: [Bool]
    , matl_PYROLYSIS_RANGE :: [Double]
    , matl_REFERENCE_RATE :: [Double]
    , matl_REFERENCE_TEMPERATURE :: [Double]
    , matl_RGB :: RGB
    , matl_SPECIFIC_HEAT :: Double
    , matl_SPECIFIC_HEAT_RAMP :: String
    , matl_SPEC_ID :: [String]
    , matl_THRESHOLD_SIGN :: [Double]
    , matl_THRESHOLD_TEMPERATURE :: [Double]
    -- , matl_POROSITY :: String
    , matl_ALLOW_SHRINKING :: Bool
    , matl_ALLOW_SWELLING :: Bool
    , matl_GAS_DIFFUSION_DEPTH :: [Double]
    }
    deriving (Show, Eq)

data PyrolysisReac
    = PyrolysisReacAE
        { pyrolysisReacAE_A :: Double -- ^A
        , pyrolysisReacAE_E :: Double -- ^E
        , pyrolysisReacAE_HEAT_OF_REACTION :: Double -- ^HEAT_OF_REACTION
        }
    | PyrolysisReacTGM
        { pyrolysisReacTGM_REFERENCE_TEMPERATURE :: Double -- ^REFERENCE_TEMPERATURE
        , pyrolysisReacTGM_HEATING_RATE :: Double -- ^HEATING_RATE
        , pyrolysisReacTGM_PYROLYSIS_RANGE :: Double -- ^PYROLYSIS_RANGE
        , pyrolysisReacTGM_HEAT_OF_REACTION :: Double -- ^HEAT_OF_REACTION
        }
    | NoPyrolysis
    deriving (Show, Eq)


----------------------------------------
data Mesh = Mesh
    { mesh_ID :: Maybe String -- ^ID
    , mesh_XB :: XB -- ^XB
    , mesh_IJK :: IJK -- ^IJK
    , mesh_COLOR :: String -- ^COLOR
    , mesh_CYLINDRICAL :: Bool -- ^CYLINDRICAL
    , mesh_EVACUATION :: Bool -- ^EVACUATION
    , mesh_EVAC_HUMANS :: Bool
    , mesh_EVAC_Z_OFFSET :: Double
    , mesh_FYI :: Maybe String
    , mesh_LEVEL :: Int
    , mesh_MPI_PROCESS :: Maybe Int
    , mesh_MULT_ID :: Maybe String
    , mesh_RGB :: RGB
    , mesh_N_THREADS :: Maybe Int
    -- , mesh_PERIODIC_MESH_IDS :: [Text]
    }
    deriving (Show, Eq)

data Misc = Misc
    { misc_AGGLOMERATION :: Bool
    , misc_AEROSOL_AL2O3 :: Bool
    , misc_ALLOW_SURFACE_PARTICLES :: Bool
    , misc_ALLOW_UNDERSIDE_PARTICLES :: Bool
    , misc_ASSUMED_GAS_TEMPERATURE :: Double
    , misc_ASSUMED_GAS_TEMPERATURE_RAMP :: String
    , misc_BAROCLINIC :: Bool
    , misc_BNDF_DEFAULT :: Bool
    , misc_CC_IBM :: Bool
    , misc_CNF_CUTOFF :: Double
    , misc_CFL_MAX :: Double
    , misc_CFL_MIN :: Double
    , misc_CFL_VELOCITY_NORM :: Int
    , misc_CHECK_HT :: Bool
    , misc_CHECK_REALIZABILITY :: Bool
    , misc_CHECK_VN :: Bool
    , misc_CLIP_MASS_FRACTION :: Bool
    , misc_COMPUTE_VISCOSITY_TWICE :: Bool
    , misc_COMPUTE_ZETA_SOURCE_TERM :: Bool
    , misc_CONSTANT_H_SOLID :: Bool
    , misc_CONSTANT_SPECIFIC_HEAT_RATIO :: Bool
    , misc_CORIOLIS_VECTOR :: [Double]
    , misc_CORRECT_SUBGRID_TEMPERATURE :: Bool
    , misc_COUPLED_1D3D_HEAT_TRANSFER :: Bool
    , misc_C_DEARDORFF :: Double
    , misc_C_RNG :: Double
    , misc_C_RNG_CUTOFF :: Double
    , misc_C_SMAGORINSKY :: Double
    , misc_C_VREMAN :: Double
    , misc_DNS :: Bool
    , misc_DRAG_CFL_MAX :: Double
    , misc_DT_MEAN_FORCING :: Double
    , misc_ENTHALPY_TRANSPORT :: Bool
    , misc_EVACUATION_DRILL :: Bool
    , misc_EVACUATION_MC_MODE :: Bool
    , misc_EVAC_PRESSURE_ITERATIONS :: Int
    , misc_EVAC_SURF_DEFAULT :: String
    , misc_EVAC_TIME_ITERATIONS :: Int
    , misc_EVAPORATION :: Bool
    -- , misc_EXCHANGE_EDGES :: String
    , misc_EXTERNAL_BOUNDARY_CORRECTION :: Bool
    , misc_EXTINCTION_MODEL :: String
    , misc_HVAC_PRES_RELAX :: Double
    , misc_HT3D_TEST :: Int
    , misc_FDS5_OPTIONS :: Bool
    , misc_FLUX_LIMITER :: Int
    , misc_FORCE_VECTOR :: [Double]
    , misc_FREEZE_VELOCITY :: Bool
    , misc_FYI :: Maybe String
    , misc_GAMMA :: Double
    , misc_GRAVITATIONAL_DEPOSITION :: Bool
    , misc_GRAVITATIONAL_SETTLING :: Bool
    , misc_GROUND_LEVEL :: Double
    , misc_GVEC :: [Double]
    , misc_DT_HVAC :: Double
    , misc_H_F_REFERENCE_TEMPERATURE :: Double
    , misc_HRRPUV_MAX_SMV :: Double
    , misc_HUMIDITY :: Double
    , misc_HVAC_MASS_TRANSPORT :: Bool
    , misc_IBLANK_SMV :: Bool
    , misc_IMMERSED_BOUNDARY_METHOD :: Int
    , misc_INITIAL_UNMIXED_FRACTION :: Double
    -- , misc_KINETIC_ENERGY_SOURCE :: String
    , misc_LAPSE_RATE :: Double
    , misc_LES_FILTER_WIDTH :: String
    , misc_MAX_CHEMISTRY_ITERATIONS :: Int
    , misc_MAX_LEAK_PATHS :: Int
    , misc_MAXIMUM_VISIBILITY :: Double
    , misc_MEAN_FORCING :: [Bool]
    , misc_MPI_TIMEOUT :: Double
    , misc_N_FIXED_CHEMISTRY_SUBSTEPS :: Int
    , misc_NEAR_WALL_TURBULENCE_MODEL :: String
    -- , misc_NEW_MOMENTUM_NUDGING :: String
    -- , misc_NEW_OPEN_BOUNDARY :: String
    , misc_NOISE :: Bool
    , misc_NOISE_VELOCITY :: Double
    , misc_NO_EVACUATION :: Bool
    , misc_NO_RAMPS :: Bool
    -- , misc_NORTHANGLE :: String
    , misc_OVERWRITE :: Bool
    , misc_PARTICLE_CFL_MAX :: Double
    , misc_PARTICLE_CFL_MIN :: Double
    , misc_PARTICLE_CFL :: Bool
    , misc_PERIODIC_TEST :: Int
    -- , misc_PROFILING :: String
    , misc_POROUS_FLOOR :: Bool
    -- , misc_POTENTIAL_TEMPERATURE_CORRECTION :: String
    , misc_PR :: Double
    , misc_PROCESS_ALL_MESHES :: Bool
    , misc_PROJECTION :: Bool
    , misc_P_INF :: Double
    -- , misc_RAMP_FVX_T :: String
    -- , misc_RAMP_FVY_T :: String
    -- , misc_RAMP_FVZ_T :: String
    , misc_RAMP_GX :: String
    , misc_RAMP_GY :: String
    , misc_RAMP_GZ :: String
    , misc_RAMP_U0 :: String
    , misc_RAMP_U0_T :: String
    , misc_RAMP_V0 :: String
    , misc_RAMP_V0_T :: String
    , misc_RAMP_W0 :: String
    , misc_RAMP_W0_T :: String
    , misc_RAMP_U0_Z :: String
    , misc_RAMP_V0_Z :: String
    , misc_RAMP_W0_Z :: String
    -- , misc_RADIATION :: String
    , misc_RESEARCH_MODE :: Bool
    , misc_RESTART :: Bool
    , misc_RESTART_CHID :: String
    , misc_RICHARDSON_ERROR_TOLERANCE :: Double
    , misc_RUN_AVG_FAC :: Double
    , misc_SC :: Double
    , misc_SECOND_ORDER_INTERPOLATED_BOUNDARY :: Bool
    , misc_SECOND_ORDER_PARTICLE_TRANSPORT :: Bool
    , misc_SHARED_FILE_SYSTEM :: Bool
    -- , misc_SLIP_CONDITION :: String
    , misc_SMOKE_ALBEDO :: Double
    , misc_SOLID_PHASE_ONLY :: Bool
    -- , misc_SOOT_OXIDATION :: String
    -- , misc_SPONGE_LAYER_DISTANCE :: String
    , misc_STRATIFICATION :: Bool
    , misc_SUPPRESSION :: Bool
    -- , misc_SURF_DEFAULT :: String
    -- , misc_TEMPERATURE_DEPENDENT_REACTION :: String
    -- , misc_TENSOR_DIFFUSIVITY :: String
    , misc_TERRAIN_CASE :: Bool
    , misc_TERRAIN_IMAGE :: String
    -- , misc_TEST_FILTER_QUADRATURE :: String
    , misc_TEXTURE_ORIGIN :: [Double]
    , misc_THERMOPHORETIC_DEPOSITION :: Bool
    , misc_THICKEN_OBSTRUCTIONS :: Bool
    -- , misc_TRANSPORT_UNMIXED_FRACTION :: String
    -- , misc_TRANSPORT_ZETA_SCHEME :: String
    , misc_TMPA :: Double
    , misc_TURBULENCE_MODEL :: String
    , misc_TURBULENT_DEPOSITION :: Bool
    -- , misc_TURB_INIT_CLOCK :: String
    , misc_U0 :: Double
    , misc_UVW_FILE :: String
    , misc_V0 :: Double
    , misc_VEG_LEVEL_SET_COUPLED :: Bool
    , misc_VEG_LEVEL_SET_UNCOUPLED :: Bool
    , misc_VERBOSE :: Double
    , misc_VISIBILITY_FACTOR :: Double
    , misc_VN_MAX :: Double
    , misc_VN_MIN :: Double
    , misc_Y_CO2_INFTY :: Double
    , misc_Y_O2_INFTY :: Double
    , misc_W0 :: Double
    -- , misc_WD_PROPS :: String
    -- , misc_WIND_BOUNDARY :: String
    -- , misc_WIND_ONLY :: String
    }
    deriving (Show, Eq)

data Mult = Mult
    { mult_DX :: Double
    , mult_DXB :: [Double]
    , mult_DX0 :: Double
    , mult_DY :: Double
    , mult_DYB :: [Double]
    , mult_DY0 :: Double
    , mult_DZ :: Double
    , mult_DZB :: [Double]
    , mult_DZ0 :: Double
    , mult_ID :: Text
    , mult_I_LOWER :: Int
    , mult_I_UPPER :: Int
    , mult_J_LOWER :: Int
    , mult_J_UPPER :: Int
    , mult_K_LOWER :: Int
    , mult_K_UPPER :: Int
    , mult_N_LOWER :: Int
    , mult_N_UPPER :: Int
    }
    deriving (Show, Eq)

data Obst = Obst
    { obst_ALLOW_VENT :: Bool
    , obst_BNDF_FACE :: (Bool, Bool, Bool, Bool, Bool, Bool)
    , obst_BNDF_OBST :: Bool
    , obst_BULK_DENSITY :: Maybe Double
    , obst_COLOR :: Maybe String
    , obst_CTRL_ID :: Maybe String
    , obst_DEVC_ID :: Maybe String
    , obst_EVACUATION :: Bool
    , obst_FYI :: Maybe String
    , obst_HT3D :: Bool
    , obst_ID :: Maybe String
    , obst_MATL_ID :: Maybe String
    , obst_MESH_ID :: Maybe String
    , obst_MULT_ID :: Maybe String
    -- , obst_NOTERRAIN :: Bool
    , obst_OUTLINE :: Bool
    , obst_OVERLAY :: Bool
    , obst_PERMIT_HOLE :: Bool
    , obst_PROP_ID :: Maybe String
    , obst_REMOVABLE :: Bool
    , obst_RGB :: Maybe RGB
    , obst_SURF_ID :: Maybe String
    , obst_SURF_ID6 :: Maybe (String, String, String, String, String, String)
    , obst_SURF_IDS :: Maybe (String, String, String)
    , obst_TEXTURE_ORIGIN :: XYZ
    , obst_THICKEN :: Bool
    , obst_TRANSPARENCY :: Double
    , obst_XB :: XB
    }
    deriving (Show, Eq)

data Part = Part
    { part_AGE :: Double
    , part_BREAKUP :: Bool
    , part_BREAKUP_CNF_RAMP_ID :: Maybe String
    , part_BREAKUP_DISTRIBUTION :: String
    , part_BREAKUP_GAMMA_D :: Double
    , part_BREAKUP_RATIO :: Double
    , part_BREAKUP_SIGMA_D :: Maybe Double
    , part_CHECK_DISTRIBUTION :: Bool
    , part_CNF_RAMP_ID :: Maybe String
    , part_COLOR :: String
    , part_COMPLEX_REFRACTIVE_INDEX :: Double
    , part_CTRL_ID :: Maybe String
    , part_DENSE_VOLUME_FRACTION :: Double
    , part_DEVC_ID :: Maybe String
    , part_DIAMETER :: Maybe Double
    , part_DISTRIBUTION :: String
    , part_DRAG_COEFFICIENT :: [Double]
    , part_DRAG_LAW :: String
    , part_FREE_AREA_FRACTION :: Maybe Double
    , part_FYI :: Maybe String
    , part_GAMMA_D :: Double
    , part_HEAT_OF_COMBUSTION :: Maybe Double
    , part_HORIZONTAL_VELOCITY :: Double
    , part_ID :: Maybe String
    , part_INITIAL_TEMPERATURE :: Double
    , part_MASSLESS :: Bool
    , part_MAXIMUM_DIAMETER :: Double
    , part_MINIMUM_DIAMETER :: Double
    , part_MONODISPERSE :: Bool
    , part_N_STRATA :: Int
    , part_ORIENTATION :: [Double]
    , part_PERMEABILITY :: [Double]
    , part_PERIODIC_X :: Bool
    , part_PERIODIC_Y :: Bool
    , part_PERIODIC_Z :: Bool
    , part_POROUS_VOLUME_FRACTION :: Maybe Double
    , part_PROP_ID :: Maybe String
    , part_QUANTITIES :: [String]
    , part_QUANTITIES_SPEC_ID :: [String]
    , part_RADIATIVE_PROPERTY_TABLE :: Maybe Double
    , part_REAL_REFRACTIVE_INDEX :: Double
    , part_RGB :: Maybe RGB
    , part_RUNNING_AVERAGE_FACTOR :: Double
    , part_SAMPLING_FACTOR :: Int
    , part_SECOND_ORDER_PARTICLE_TRANSPORT :: Bool
    , part_SIGMA_D :: Maybe Double
    , part_SPEC_ID :: Maybe String
    , part_STATIC :: Bool
    , part_SURFACE_TENSION :: Double
    , part_SURF_ID :: Maybe String
    , part_TARGET_ONLY :: Bool
    , part_TURBULENT_DISPERSION :: Bool
    , part_VERTICAL_VELOCITY :: Double
    }
    deriving (Show, Eq)

data Pres = Pres
    { pres_CHECK_POISSON :: Bool
    , pres_FISHPAK_BC :: [Int]
    -- , pres_GLMAT_SOLVER :: String
    , pres_ITERATION_SUSPEND_FACTOR :: Double
    -- , pres_LAPLACE_PRESSURE_CORRECTION :: String
    , pres_MAX_PRESSURE_ITERATIONS :: Int
    , pres_PRESSURE_RELAX_TIME :: Double
    , pres_PRESSURE_TOLERANCE :: Double
    , pres_RELAXATION_FACTOR :: Double
    , pres_SCARC_METHOD :: String
    , pres_SCARC_KRYLOV :: String
    , pres_SCARC_MULTIGRID :: String
    , pres_SCARC_SMOOTH :: String
    , pres_SCARC_PRECON :: String
    , pres_SCARC_COARSE :: String
    , pres_SCARC_INITIAL :: String
    , pres_SCARC_ACCURACY :: Double
    , pres_SCARC_DEBUG :: String
    , pres_SCARC_MULTIGRID_CYCLE :: String
    , pres_SCARC_MULTIGRID_LEVEL :: String
    , pres_SCARC_MULTIGRID_COARSENING :: String
    , pres_SCARC_MULTIGRID_ITERATIONS :: Int
    , pres_SCARC_MULTIGRID_ACCURACY :: Double
    , pres_SCARC_MULTIGRID_INTERPOL :: String
    , pres_SCARC_KRYLOV_ITERATIONS :: Int
    , pres_SCARC_KRYLOV_ACCURACY :: Double
    , pres_SCARC_SMOOTH_ITERATIONS :: Int
    , pres_SCARC_SMOOTH_ACCURACY :: Double
    , pres_SCARC_SMOOTH_OMEGA :: String
    , pres_SCARC_PRECON_ITERATIONS :: Int
    , pres_SCARC_PRECON_ACCURACY :: Double
    , pres_SCARC_PRECON_OMEGA :: String
    , pres_SCARC_COARSE_ITERATIONS :: Int
    , pres_SCARC_COARSE_ACCURACY :: Double
    , pres_SOLVER :: String
    , pres_SUSPEND_PRESSURE_ITERATIONS :: Int
    , pres_VELOCITY_TOLERANCE :: Double
    }
    deriving (Show, Eq)

data Prof = Prof
    { prof_FORMAT_INDEX :: Int
    , prof_FYI :: String
    , prof_ID :: String
    , prof_IOR :: Int
    , prof_QUANTITY :: String
    , prof_XYZ :: XYZ
    }
    deriving (Show, Eq)

data Prop = Prop
    { prop_ACTIVATION_OBSCURATION :: Double
    , prop_ACTIVATION_TEMPERATURE :: Double
    , prop_ALPHA_C :: Double
    , prop_ALPHA_E :: Double
    , prop_BEAD_DENSITY :: Double
    , prop_BEAD_DIAMETER :: Double
    , prop_BEAD_EMISSIVITY :: Double
    , prop_BEAD_HEAT_TRANSFER_COEFFICIENT :: Double
    , prop_BEAD_SPECIFIC_HEAT :: Double
    , prop_BETA_C :: Double
    , prop_BETA_E :: Double
    -- , prop_FED_ACTIVITY :: String
    , prop_CHARACTERISTIC_VELOCITY :: Double
    , prop_C_FACTOR :: Double
    , prop_DENSITY :: Double
    , prop_DIAMETER :: Double
    , prop_DROPLET_VELOCITY :: Double
    , prop_EMISSIVITY :: Double
    , prop_FLOW_RAMP :: String
    , prop_FLOW_RATE :: Double
    , prop_FLOW_TAU :: Double
    , prop_FYI :: Maybe String
    , prop_GAUGE_EMISSIVITY :: Double
    , prop_GAUGE_TEMPERATURE :: Double
    , prop_HEAT_TRANSFER_COEFFICIENT :: Double
    , prop_ID :: Maybe String
    , prop_INITIAL_TEMPERATURE :: Double
    , prop_K_FACTOR :: Double
    , prop_LENGTH :: Double
    , prop_MASS_FLOW_RATE :: Double
    , prop_OFFSET :: Double
    , prop_OPERATING_PRESSURE :: Double
    , prop_ORIFICE_DIAMETER :: Double
    , prop_P0 :: String
    , prop_PARTICLES_PER_SECOND :: Int
    , prop_PARTICLE_VELOCITY :: Double
    , prop_PART_ID :: String
    , prop_PDPA_END :: Double
    , prop_PDPA_HISTOGRAM :: Bool
    , prop_PDPA_HISTOGRAM_LIMITS :: [Double]
    , prop_PDPA_HISTOGRAM_NBINS :: Int
    , prop_PDPA_HISTOGRAM_CUMULATIVE :: Bool
    , prop_PDPA_INTEGRATE :: Bool
    , prop_PDPA_M :: Int
    , prop_PDPA_N :: Int
    , prop_PDPA_NORMALIZE :: Bool
    , prop_PDPA_RADIUS :: Double
    , prop_PDPA_START :: Double
    , prop_PRESSURE_RAMP :: String
    -- , prop_PX :: String
    -- , prop_PXX :: String
    , prop_QUANTITY :: Maybe String
    , prop_RTI :: Double
    , prop_SMOKEVIEW_ID :: [String]
    , prop_SMOKEVIEW_PARAMETERS :: [String]
    , prop_SPEC_ID :: String
    , prop_SPRAY_ANGLE :: [Double]
    , prop_SPRAY_PATTERN_BETA :: Double
    , prop_SPRAY_PATTERN_MU :: Double
    , prop_SPRAY_PATTERN_SHAPE :: String
    , prop_SPRAY_PATTERN_TABLE :: String
    , prop_VELOCITY_COMPONENT :: Int
    -- , prop_DROPLET_VELOCITY :: String
    }
    deriving (Show, Eq)
data Radi = Radi
    { radi_ANGLE_INCREMENT :: Int
    , radi_BAND_LIMITS :: [Double]
    , radi_C_MAX :: Double
    , radi_C_MIN :: Double
    , radi_INITIAL_RADIATION_ITERATIONS :: Int
    , radi_KAPPA0 :: Double
    , radi_NMIEANG :: Int
    , radi_NUMBER_RADIATION_ANGLES :: Int
    , radi_PATH_LENGTH :: Double
    , radi_RADIATION :: Bool
    , radi_RADIATION_ITERATIONS :: Int
    -- , radi_RADIATIVE_FRACTION :: String
    , radi_RADTMP :: Double
    -- , radi_RTE_SOURCE_CORRECTION :: String
    , radi_TIME_STEP_INCREMENT :: Int
    , radi_WIDE_BAND_MODEL :: Bool
    , radi_MIE_MINIMUM_DIAMETER :: Double
    , radi_MIE_MAXIMUM_DIAMETER :: Double
    , radi_MIE_NDG :: Int
    , radi_NUMBER_INITIAL_ITERATIONS :: Int
    , radi_QR_CLIP :: Double
    }
    deriving (Show, Eq)

data Ramp = Ramp
    { ramp_ID :: String
    , ramp_entries :: [RampEntry]
    } deriving (Show, Eq)

data RampEntry = RampEntry
    { rampEntry_CTRL_ID :: String
    , rampEntry_DEVC_ID :: String
    , rampEntry_F :: Double
    , rampEntry_FYI :: Maybe String
    , rampEntry_NUMBER_INTERPOLATION_POINTS :: Int
    , rampEntry_T :: Double
    , rampEntry_X :: Double
    , rampEntry_Z :: Double
    } deriving (Show, Eq)

data Reac = Reac
    { reac_A :: Maybe Double
    -- , reac_ALT_REAC_ID :: String
    , reac_AUTO_IGNITION_TEMPERATURE :: Double
    , reac_C :: Double
    , reac_CHECK_ATOM_BALANCE :: Bool
    , reac_CO_YIELD :: Double
    , reac_CRITICAL_FLAME_TEMPERATURE :: Double
    , reac_E :: Double
    , reac_EPUMO2 :: Double
    -- , reac_K :: String
    , reac_EQUATION :: String
    , reac_FIXED_MIX_TIME :: Double
    -- , reac_FLAME_SPEED :: String
    -- , reac_FLAME_SPEED_EXPONENT :: String
    -- , reac_FLAME_SPEED_TEMPERATURE :: String
    , reac_FORMULA :: String
    , reac_FUEL :: String
    , reac_FUEL_RADCAL_ID :: String
    -- , reac_FWD_ID :: String
    , reac_FYI :: Maybe String
    , reac_H :: Double
    , reac_HEAT_OF_COMBUSTION :: Double
    , reac_ID :: Maybe String
    , reac_IDEAL :: Bool
    , reac_N :: Double
    , reac_NU :: [Double]
    , reac_N_S :: [Double]
    , reac_N_T :: Double
    , reac_O :: Double
    -- , reac_ODE_SOLVER :: String
    , reac_RADIATIVE_FRACTION :: Double
    , reac_RAMP_CHI_R :: String
    -- , reac_RAMP_FS :: String
    , reac_REAC_ATOM_ERROR :: Double
    , reac_REAC_MASS_ERROR :: Double
    -- , reac_REVERSE :: String
    , reac_SOOT_H_FRACTION :: Double
    , reac_SOOT_YIELD :: Double
    , reac_SPEC_ID_N_S :: [String]
    , reac_SPEC_ID_NU :: [String]
    -- , reac_TABLE_FS :: String
    -- , reac_TAU_CHEM :: String
    -- , reac_TAU_FLAME :: String
    , reac_THIRD_BODY :: Bool
    -- , reac_TURBULENT_FLAME_SPEED_ALPHA :: String
    -- , reac_TURBULENT_FLAME_SPEED_EXPONENT :: String
    -- , reac_Y_P_MIN_EDC :: String
    }
    deriving (Show, Eq)

data Slcf = Slcf
    { slcf_AGL_SLICE :: String
    , slcf_CELL_CENTERED :: Bool
    , slcf_EVACUATION :: Bool
    -- , slcf_FACE_CENTERED :: String
    -- , slcf_FIRE_LINE :: String
    , slcf_FYI :: Maybe String
    , slcf_ID :: Maybe String
    , slcf_IOR :: Int
    , slcf_LEVEL_SET_FIRE_LINE :: String
    , slcf_MAXIMUM_VALUE :: Double
    , slcf_MESH_NUMBER :: Int
    , slcf_MINIMUM_VALUE :: Double
    , slcf_PART_ID :: String
    , slcf_PBX :: Maybe Double
    , slcf_PBY :: Maybe Double
    , slcf_PBZ :: Maybe Double
    -- , slcf_PROP_ID :: String
    , slcf_QUANTITY :: Maybe String
    , slcf_QUANTITY2 :: Maybe String
    , slcf_REAC_ID :: String
    -- , slcf_SLICETYPE :: String
    , slcf_SPEC_ID :: Maybe String
    , slcf_VECTOR :: Bool
    , slcf_VELO_INDEX :: Int
    , slcf_XB :: XB
    }
    deriving (Show, Eq)

data Spec = Spec
    { spec_AEROSOL :: Bool
    , spec_ALIAS :: String
    , spec_BACKGROUND :: Bool
    -- , spec_COPY_LUMPED :: String
    , spec_CONDUCTIVITY :: Double
    , spec_CONDUCTIVITY_SOLID :: Double
    , spec_DENSITY_LIQUID :: Double
    , spec_DENSITY_SOLID :: Double
    , spec_DIFFUSIVITY :: Double
    , spec_ENTHALPY_OF_FORMATION :: Double
    , spec_EPSILONKLJ :: Double
    , spec_FIC_CONCENTRATION :: Double
    , spec_FLD_LETHAL_DOSE :: Double
    , spec_FORMULA :: String
    , spec_FYI :: Maybe String
    , spec_HEAT_OF_VAPORIZATION :: Double
    , spec_H_V_REFERENCE_TEMPERATURE :: Double
    , spec_ID :: String
    , spec_LUMPED_COMPONENT_ONLY :: Bool
    , spec_MASS_EXTINCTION_COEFFICIENT :: Double
    , spec_MASS_FRACTION :: [Double]
    , spec_MASS_FRACTION_0 :: Double
    -- , spec_MAX_DIAMETER :: String
    , spec_MEAN_DIAMETER :: Double
    , spec_MELTING_TEMPERATURE :: Double
    -- , spec_MIN_DIAMETER :: String
    , spec_MW :: Double
    -- , spec_N_BINS :: String
    , spec_PR_GAS :: Double
    , spec_PRIMITIVE :: Bool
    , spec_RADCAL_ID :: String
    , spec_RAMP_CP :: String
    , spec_RAMP_CP_L :: String
    , spec_RAMP_D :: String
    , spec_RAMP_G_F :: String
    , spec_RAMP_K :: String
    , spec_RAMP_MU :: String
    , spec_REFERENCE_ENTHALPY :: Double
    , spec_REFERENCE_TEMPERATURE :: Double
    , spec_SIGMALJ :: Double
    , spec_SPEC_ID :: [String]
    , spec_SPECIFIC_HEAT :: Double
    , spec_SPECIFIC_HEAT_LIQUID :: Double
    , spec_VAPORIZATION_TEMPERATURE :: Double
    , spec_VISCOSITY :: Double
    , spec_VOLUME_FRACTION :: [Double]
    }
    deriving (Show, Eq)

data Surf = Surf
    { surf_ADIABATIC :: Bool
    , surf_AUTO_IGNITION_TEMPERATURE :: Double
    , surf_BACKING :: String
    , surf_BURN_AWAY :: Bool
    , surf_CELL_SIZE_FACTOR :: Double
    , surf_C_FORCED_CONSTANT :: Double
    , surf_C_FORCED_PR_EXP :: Double
    , surf_C_FORCED_RE :: Double
    , surf_C_FORCED_RE_EXP :: Double
    , surf_C_HORIZONTAL :: Double
    , surf_C_VERTICAL :: Double
    , surf_COLOR :: Maybe String
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
    , surf_FYI :: Maybe String
    -- , surf_GEOMETRY :: String
    -- , surf_HEAT_OF_VAPORIZATION :: Double
    -- , surf_HEAT_TRANSFER_COEFFICIENT :: Double
    -- , surf_HEAT_TRANSFER_COEFFICIENT_BACK :: Double
    -- , surf_HEAT_TRANSFER_MODEL :: String
    , surf_HRRPUA :: Maybe Double
    , surf_HT3D :: Bool
    , surf_ID :: Maybe String
    , surf_IGNITION_TEMPERATURE :: Double
    , surf_INNER_RADIUS :: Double
    , surf_INTERNAL_HEAT_SOURCE :: [Double]
    , surf_LAYER_DIVIDE :: Double
    , surf_LEAK_PATH :: [Int]
    , surf_LENGTH :: Double
    , surf_MASS_FLUX :: Maybe [Double]
    , surf_MASS_FLUX_TOTAL :: Maybe Double
    , surf_MASS_FLUX_VAR :: Maybe Double
    , surf_MASS_FRACTION :: [Double]
    , surf_MASS_TRANSFER_COEFFICIENT :: Double
    , surf_MATL_ID :: [String]
    , surf_MATL_MASS_FRACTION :: [Double]
    , surf_MINIMUM_LAYER_THICKNESS :: Double
    , surf_MLRPUA :: Maybe Double
    -- , surf_N_CELLS_MAX :: String
    , surf_N_LAYER_CELLS_MAX :: [Int]
    , surf_NET_HEAT_FLUX :: Double
    , surf_NO_SLIP :: Bool
    , surf_NPPC :: Int
    , surf_PARTICLE_MASS_FLUX :: Double
    , surf_PART_ID :: String
    , surf_PLE :: Double
    , surf_PROFILE :: String
    , surf_RADIUS :: Double
    , surf_RAMP_EF :: String
    , surf_RAMP_MF :: [String]
    , surf_RAMP_PART :: String
    , surf_RAMP_Q :: Maybe String
    , surf_RAMP_T :: Maybe String
    , surf_RAMP_T_I :: Maybe String
    , surf_RAMP_V :: Maybe String
    , surf_RAMP_V_X :: Maybe String
    , surf_RAMP_V_Y :: Maybe String
    , surf_RAMP_V_Z :: Maybe String
    , surf_RGB :: RGB
    , surf_ROUGHNESS :: Double
    , surf_SPEC_ID :: String
    , surf_SPREAD_RATE :: Double
    , surf_STRETCH_FACTOR :: [Double]
    , surf_TAU_EF :: Double
    , surf_TAU_MF :: Double
    , surf_TAU_PART :: Double
    , surf_TAU_Q :: Double
    , surf_TAU_T :: Double
    , surf_TAU_V :: Double
    , surf_TEXTURE_HEIGHT :: Double
    , surf_TEXTURE_MAP :: String
    , surf_TEXTURE_WIDTH :: Double
    , surf_TGA_ANALYSIS :: Bool
    , surf_TGA_FINAL_TEMPERATURE :: Double
    , surf_TGA_HEATING_RATE :: Double
    , surf_THICKNESS :: [Double]
    , surf_TMP_BACK :: Double
    , surf_TMP_FRONT :: Double
    , surf_TMP_INNER :: [Double]
    , surf_TRANSPARENCY :: Double
    , surf_VEGETATION :: Bool
    , surf_VEGETATION_ARRHENIUS_DEGRAD :: Bool
    , surf_VEGETATION_CDRAG :: Double
    , surf_VEGETATION_CHAR_FRACTION :: Double
    , surf_VEGETATION_ELEMENT_DENSITY :: Int
    , surf_VEGETATION_GROUND_TEMP :: Double
    , surf_VEGETATION_HEIGHT :: Double
    , surf_VEGETATION_INITIAL_TEMP :: Double
    , surf_VEGETATION_LAYERS :: Int
    , surf_VEGETATION_LINEAR_DEGRAD :: Bool
    , surf_VEGETATION_LOAD :: Double
    , surf_VEGETATION_LSET_IGNITE_TIME :: Double
    , surf_VEG_LSET_QCON :: Double
    , surf_VEGETATION_MOISTURE :: Double
    , surf_VEGETATION_NO_BURN :: Bool
    , surf_VEGETATION_SVRATIO :: Int
    , surf_VEG_LEVEL_SET_SPREAD :: Bool
    , surf_VEG_LSET_ROS_BACK :: Double
    , surf_VEG_LSET_ROS_FLANK :: Double
    , surf_VEG_LSET_ROS_HEAD :: Double
    , surf_VEG_LSET_WIND_EXP :: Double
    , surf_VEG_LSET_SIGMA :: Double
    , surf_VEG_LSET_HT :: Double
    , surf_VEG_LSET_BETA :: Double
    , surf_VEG_LSET_ELLIPSE :: Double
    , surf_VEG_LSET_TAN2 :: Bool
    , surf_VEG_LSET_ELLIPSE_HEAD :: Double
    , surf_VEL :: Maybe Double
    , surf_VEL_BULK :: Double
    , surf_VEL_GRAD :: Double
    , surf_VEL_T :: Maybe (Double, Double)
    , surf_VOLUME_FLOW :: Maybe Double
    , surf_WIDTH :: Double
    , surf_XYZ :: XYZ
    , surf_Z0 :: Double
    -- , surf_ZETA_FRONT :: String
    -- , surf_EXTERNAL_FLUX_RAMP :: String
    -- , surf_TAU_EXTERNAL_FLUX :: String
    -- , surf_VOLUME_FLUX :: String
    }
    deriving (Show, Eq)

data SurfLayer = SurfLayer
--    { surfLayer_Position :: Int     -- ^Position
--    { surfLayer_Surf :: Surf    -- ^Surface
    { surfLayer_THICKNESS :: Double
    , surfLayer_Components :: [SurfLayerComponent]
    }
    deriving (Show, Eq)

data SurfLayerComponent = SurfLayerComponent
--    { surfLayerComponent_Pos :: Int
--    , surfLayerComponent_ParentLayer :: SurfLayer
    { surfLayerComponent_MATL :: Matl
--    { surfLayerComponent_MATL :: Text
    , surfLayerComponent_MATL_MASS_FRACTION :: Double
    }
    deriving (Show, Eq)
--surfLayerComponent_Pos = surfLayerComponent_Pos
--surfLayerComponent_ParentLayer = surfLayerComponent_ParentLayer
-- surfLayerComponent_MATL = surfLayerComponent_MATL
-- surfLayerComponent_MATL_MASS_FRACTION = (\a -> a) . surfLayerComponent_MATL_MASS_FRACTION
data SurfBurner = SurfBurner
--    { surfLayerComponent_Pos :: Int
--    , surfLayerComponent_ParentLayer :: SurfLayer
--    { surfLayerComponent_MATL :: Matl
    { surfBurner_HRRPUA :: Double
    , surfBURNER_TAU_Q :: Double
    }
    | NoBurner
    deriving (Show, Eq)

data Tabl = Tabl
    { tabl_FYI :: Maybe String
    , tabl_ID :: String
    , tabl_TABLE_DATA :: [Double]
    }
    deriving (Show, Eq)

data Time = Time
    { time_DT :: Maybe Double
    , time_EVAC_DT_FLOWFIELD :: Double
    , time_EVAC_DT_STEADY_STATE :: Double
    , time_FYI :: Maybe String
    , time_LIMITING_DT_RATIO :: Double
    , time_LOCK_TIME_STEP :: Bool
    , time_RESTRICT_TIME_STEP :: Bool
    , time_T_BEGIN :: Double
    , time_T_END :: Double
    , time_T_END_GEOM :: Double
    , time_TIME_SHRINK_FACTOR :: Double
    , time_WALL_INCREMENT :: Int
    , time_WALL_INCREMENT_HT3D :: Int
    , time_TWFIN :: Double
    }
    deriving (Show, Eq)

data Trnx = Trnx
    { trnx_CC :: Double
    , trnx_FYI :: Maybe String
    , trnx_IDERIV :: Int
    , trnx_MESH_NUMBER :: Int
    , trnx_PC :: Double
    }
    deriving (Show, Eq)

data Trny = Trny
    { trny_CC :: Double
    , trny_FYI :: Maybe String
    , trny_IDERIV :: Int
    , trny_MESH_NUMBER :: Int
    , trny_PC :: Double
    }
    deriving (Show, Eq)

data Trnz = Trnz
    { trnz_CC :: Double
    , trnz_FYI :: Maybe String
    , trnz_IDERIV :: Int
    , trnz_MESH_NUMBER :: Int
    , trnz_PC :: Double
    }
    deriving (Show, Eq)

data Vent = Vent
    { vent_COLOR :: Maybe String
    , vent_CTRL_ID :: String
    , vent_DEVC_ID :: String
    , vent_DYNAMIC_PRESSURE :: Double
    , vent_EVACUATION :: Bool
    , vent_FYI :: Maybe String
    , vent_ID :: Maybe String
    , vent_IOR :: Int
    , vent_L_EDDY :: Double
    , vent_L_EDDY_IJ :: [Int]
    , vent_MB :: String
    , vent_MESH_ID :: String
    , vent_MULT_ID :: String
    , vent_N_EDDY :: Int
    , vent_OUTLINE :: Bool
    , vent_PBX :: Double
    , vent_PBY :: Double
    , vent_PBZ :: Double
    , vent_PRESSURE_RAMP :: String
    , vent_RADIUS :: Double
    , vent_REYNOLDS_STRESS :: [Double]
    , vent_RGB :: RGB
    , vent_SPREAD_RATE :: Double
    , vent_SURF_ID :: Maybe String
    , vent_TEXTURE_ORIGIN :: [Double]
    , vent_TMP_EXTERIOR :: Double
    , vent_TMP_EXTERIOR_RAMP :: String
    , vent_TRANSPARENCY :: Double
    , vent_UVW :: [Double]
    , vent_VEL_RMS :: Double
    -- , vent_WIND :: String
    , vent_XB :: Maybe XB
    , vent_XYZ :: XYZ
    }
    deriving (Show, Eq)

data Zone = Zone
    { zone_ID :: String
    , zone_LEAK_AREA :: Double
    , zone_LEAK_PRESSURE_EXPONENT :: Double
    , zone_LEAK_REFERENCE_PRESSURE :: Double
    , zone_XB :: XB
    , zone_PERIODIC :: Bool
    }
    deriving (Show, Eq)

data Plane = X | Y | Z
    deriving (Show, Eq)

data Direction = NegX | PosX | NegY | PosY | NegZ | PosZ deriving (Show, Eq)

data IJK = IJK
    { ijk_i :: Int -- ^I
    , ijk_j :: Int -- ^J
    , ijk_k :: Int -- ^K
    }
    deriving (Show, Eq)

-- TODO: switch to word8 as it should be 0-255
data RGB = RGB
    { rgb_r :: Int -- ^R
    , rgb_g :: Int -- ^G
    , rgb_b :: Int -- ^B
    }
    deriving (Show, Eq)

data XB = XB
    Coord
    Coord
    Coord
    Coord
    Coord
    Coord
    deriving (Show, Eq)
-- ^x1 x2 y1 y2 z1 z2
data XYZ = XYZ
    { xyz_x :: Coord -- ^X
    , xyz_y :: Coord -- ^Y
    , xyz_z :: Coord -- ^Z
    }
    deriving (Show, Eq)

type Coord = Double

type GridCoord = Int
