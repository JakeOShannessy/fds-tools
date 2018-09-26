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
module FDSUtilities.FDSFile where

import Text.Namelist
-- import Data.Array.IArray
import Data.Text (Text)
import qualified Data.Text as T
-- import Data.List

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
    , fdsFile_Vents :: [Vent]
    , fdsFile_Bndfs :: [Bndf]
    , fdsFile_Isofs :: [Isof]
    , fdsFile_Slcfs :: [Slcf]
    , fdsFile_unknownNamelists :: [Namelist]
    } deriving (Show)

data Head = Head
    { head_CHID :: Text
    , head_FYI :: Text
    , head_TITLE :: Text
    } deriving (Show)
-- ^CHID FYI TITLE

data Bndf = Bndf
    { bndf_CELL_CENTRED :: Bool -- ^CELL_CENTERED --default: False
    , bndf_FYI :: Text        -- ^FYI
    , bndf_PART_ID :: Text    -- ^PART_ID
    , bndf_PROP_ID :: Text    -- ^PROP_ID
    , bndf_RECOUNT_DRIP :: Bool -- ^RECOUNT_DRIP --default: False
    , bndf_QUANTITY :: Text   -- ^QUANTITY
    , bndf_SPEC_ID :: Text    -- ^SPEC_ID
    } deriving (Show)

data Clip = Clip
    { clip_FYI :: Text                        -- ^FYI
    , clip_MAXIMUM_DENSITY :: Double            -- ^MAXIMUM_DENSITY
    , clip_MAXIMUM_MASS_FRACTION :: [[Double]]  -- ^MAXIMUM_MASS_FRACTION
    , clip_MAXIMUM_TEMPERATURE :: Double        -- ^MAXIMUM_TEMPERATURE
    , clip_MINIMUM_DENSITY :: Double            -- ^MINIMUM_DENSITY
    , clip_MINIMUM_MASS_FRACTION :: [[Double]]  -- ^MINIMUM_MASS_FRACTION
    , clip_MINIMUM_TEMPERATURE :: Double        -- ^MINIMUM_TEMPERATURE
    }
    deriving (Show)

data Ctrl = Ctrl
    { ctrl_DELAY :: Double        -- ^DELAY --default: 0
    , ctrl_FUNCTION_TYPE :: Text  -- ^FUNCTION_TYPE
    , ctrl_ID :: Text             -- ^ID
    , ctrl_INITIAL_DTATE :: Bool  -- ^INITIAL_STATE --default: False
    , ctrl_INPUT_ID :: Text       -- ^INPUT_ID
    , ctrl_LATCH :: Bool          -- ^LATCH --default: True
    , ctrl_N :: Int               -- ^N --default: 1
    , ctrl_ON_BOUND :: Text       -- ^ON_BOUND --default: "LOWER"
    , ctrl_RAMP_ID :: Text        -- ^RAMP_ID
    , ctrl_SETPOINT :: Double     -- ^SETPOINT
    }
    deriving (Show)

data Devc = Devc
--    { devc_BYPASS_FLOWRATE :: Double -- ^BYPASS_FLOWRATE
    { devc_CTRL_ID :: Text -- ^CTRL_ID
--    , devc_DELAY :: Double -- ^DELAY
--    , devc_DEVC_ID :: Text -- ^DEVC_ID
--    , devc_DUCT_ID :: Text -- ^DUCT_ID
--    , devc_DEPTH :: Double -- ^DEPTH
--    , devc_FLOWRATE :: Double -- ^FLOWRATE
    , devc_FYI :: Text -- ^FYI
--    , devc_IOR :: Int -- ^IOR
    , devc_ID :: Text -- ^ID
--    , devc_INITIAL_STATE :: Bool -- ^INITIAL_STATE
--    , devc_LATCH :: Bool -- ^LATCH
--    , devc_MATL_ID :: Text -- ^MATL_ID
--    , devc_NODE_ID :: Text -- ^NODE_ID
    , devc_ORIENTATION :: SemiXB -- ^ORIENTATION
--    , devc_PART_ID :: Text -- ^PART_ID
--    , devc_POINTS :: Int -- ^POINTS
    , devc_PROP_ID :: Text -- ^PROP_ID
    , devc_QUANTITY :: Text -- ^QUANTITY
    , devc_ROTATION :: SemiXB -- ^ROTATION
--    , devc_SETPOINT :: Double -- ^SETPOINT
--    , devc_SPEC_ID :: Text -- ^SPEC_ID
--    , devc_STATISTICS :: Text -- ^STATISTICS
--    , devc_SURF_ID :: Text -- ^SURF_ID
--    , devc_TIME_AVERAGED :: Bool -- ^TIME_AVERAGED
--    , devc_TRIP_DIRECTION :: Int -- ^TRIP_DIRECTION
--    , devc_VELO_INDEX :: Int -- ^VELO_INDEX
    , devc_XB :: XB -- ^XB
    , devc_XYZ :: SemiXB -- ^XYZ
    }
    deriving (Show)

data Dump = Dump
    Double --dt_restart
    [Text] --Plot3d_quantity
    Bool --writexyz
    Text --render_file
    deriving (Show)
--    -- ^COLUMN_DUMP_LIMIT CTRL_COLUMN_LIMIT DEVC_COLUMN_LIMIT DT_BNDF
--    --  DT_CTRL DT_DEVC DT_DEVC_LINE DT_FLUSH DT_HRR DT_ISOF DT_MASS
--    --  DT_PART DT_PL3D DT_PROF DT_RESTART DT_SLCF FLUSH_FILE_BUFFERS
--    --  MASS_FILE MAXIMUM_DROPLETS NFRAMES PLOT3D_QUANTITY PLOT3D_SPEC_ID
--    --  PLOT3D_VELO_INDEX SMOKE3D SMOKE3D_QUANTITY SMOKE3D_SPEC_ID
--    --  STATE_FILE STATUS_FILES WRITE_XYZ RENDER_FILE


data Hole = Hole
    XB
    deriving (Show)
-- ^XB
data Hvac = Hvac
data Init = Init
data Isof = Isof
    Text
    Double
    deriving (Show)
-- ^QUANTITY VALUE
----------------------------------------
data Matl = Matl
    { matl_ID :: Text -- ^ID
    , matl_SPECIFIC_HEAT :: Double -- ^SPECIFIC_HEAT
    , matl_CONDUCTIVITY :: Double -- ^CONDUCTIVITY
    , matl_DENSITY :: Double -- ^DENSITY
    , matl_HEAT_OF_COMBUSTION :: Double -- ^HEAT_OF_COMBUSTION
    , matl_N_REACTIONS :: Int -- ^N_REACTIONS
    , matl_NU_FUEL :: Double -- ^NU_FUEL
    , matl_NS :: Double -- ^NS
    , matl_Pyrolysis_Properties :: PyrolysisReac -- ^Pyrolysis_Properties
    }
    deriving (Show)

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
    deriving (Show)


----------------------------------------
data Mesh = Mesh
    { mesh_ID :: Text -- ^ID
    , mesh_XB :: XB -- ^XB
    , mesh_IJK :: IJK -- ^IJK
    }
    deriving (Show)

data Misc = Misc
    { misc_SURF_DEFAULT :: Text -- ^SURF_DEFAULT
    , misc_HUMIDITY :: Double -- ^HUMIDITY
    , misc_TMPA :: Double -- ^TMPA
    }
    deriving (Show)

data Mult = Mult
data Obst = Obst
    { obst_XB :: XB -- ^XB
    , obst_SURF_ID :: Surf -- ^Surf ID
    }
    deriving (Show)

data Part = Part
data Pres = Pres
data Prof = Prof
data Prop = Prop
data Radi = Radi
data Ramp = Ramp
data Reac = Reac
    Text
    Text
    Double
    Double
    Double
    Double
    Double
    Double
    Double
    Double
    Double
    Double
    Double
    Double
    deriving (Show)
-- ^ID FYI SOOT_YIELD N C H O OTHER MW_OTHER Y_CO Y_H2 HEAT_OF_COMBUSTION EPUMO2 HFRAC

data Slcf = Slcf
    Text  -- QUANTITY
    Bool    -- VECTOR
    Plane   -- Plane
    Double  -- Offset
    deriving (Show)

data Spec = Spec
data Surf = Surf
    { surf_ID :: Text -- ^ID
    , surf_RGB :: RGB -- ^RGB
    , surf_COLOR :: Text -- ^COLOR
    , surf_BURN_AWAY :: Bool -- ^BURN_AWAY
    , surf_Layers :: [SurfLayer]
    , surf_Burner :: SurfBurner
    }
    deriving (Show)

data SurfLayer = SurfLayer
--    { surfLayer_Position :: Int     -- ^Position
--    { surfLayer_Surf :: Surf    -- ^Surface
    { surfLayer_THICKNESS :: Double
    , surfLayer_Components :: [SurfLayerComponent]
    }
    deriving (Show)

data SurfLayerComponent = SurfLayerComponent
--    { surfLayerComponent_Pos :: Int
--    , surfLayerComponent_ParentLayer :: SurfLayer
    { surfLayerComponent_MATL :: Matl
--    { surfLayerComponent_MATL :: Text
    , surfLayerComponent_MATL_MASS_FRACTION :: Double
    }
    deriving (Show)
--surfLayerComponent_Pos = surfLayerComponent_Pos
--surfLayerComponent_ParentLayer = surfLayerComponent_ParentLayer
surfLayerComponent_MATL = surfLayerComponent_MATL
surfLayerComponent_MATL_MASS_FRACTION = (\a -> a) . surfLayerComponent_MATL_MASS_FRACTION
data SurfBurner = SurfBurner
--    { surfLayerComponent_Pos :: Int
--    , surfLayerComponent_ParentLayer :: SurfLayer
--    { surfLayerComponent_MATL :: Matl
    { surfBurner_HRRPUA :: Double
    , surfBURNER_TAU_Q :: Double
    }
    | NoBurner
    deriving (Show)

data Tabl = Tabl
data Time = Time
    Double
    Double
    Double
    Bool
    deriving (Show)
-- ^DT T_BEGIN T_END SYNCHRONIZE

data Trnx = Trnx
data Trny = Trny
data Trnz = Trnz
data Vent = Vent
    Text
    XB
    Text
    deriving (Show)
-- ^SURF_ID XB COLOR
data Zone = Zone

data Plane = X | Y | Z
    deriving (Show)
data IJK = IJK
    Int
    Int
    Int
    deriving (Show)
-- ^I J K
data RGB = RGB
    Int
    Int
    Int
    deriving (Show)
-- ^R G B
data XB = XB
    Coord
    Coord
    Coord
    Coord
    Coord
    Coord
    deriving (Show)
-- ^x1 x2 y1 y2 z1 z2
data SemiXB = SemiXB
    Coord
    Coord
    Coord
    deriving (Show)

type Coord = Double

type GridCoord = Int

-- --data (FDSType a) => FDSValue  a = FDSValue a | FDSNone
-- data FDSValue  a = FDSValue a | FDSNone

-- class FDSType a where
    -- inFDS :: ParameterValue -> FDSValue a
    -- inFDS ArrayEmpty = FDSNone

    -- exFDS :: FDSValue a -> ParameterValue

-- instance FDSType Text where
    -- inFDS (ParString value) = FDSValue value
    -- inFDS ArrayEmpty = FDSNone
    -- exFDS value = ParString value
-- instance FDSType Int where
    -- inFDS (ParInt value) = FDSValue value
    -- inFDS ArrayEmpty = FDSNone
    -- exFDS value = ParInt value
-- instance FDSType Double where
    -- inFDS (ParDouble value) = FDSValue value
    -- inFDS ArrayEmpty = FDSNone
    -- exFDS value = ParDouble value
-- instance FDSType Bool where
    -- inFDS (ParBool value) = FDSValue value
    -- inFDS ArrayEmpty = FDSNone
    -- exFDS value = ParBool value
-- instance FDSType Plane where
    -- inFDS (ParString value) = case value of
        -- "X" -> FDSValue X
        -- "Y" -> FDSValue Y
        -- "Z" -> FDSValue Z
    -- inFDS ArrayEmpty = FDSNone
    -- exFDS value = case value of
        -- X -> ParString "X"
        -- Y -> ParString "Y"
        -- Z -> ParString "Z"
-- instance FDSType XB where
    -- inFDS (ParArry value) = FDSValue ((arr2XB . array2List) value)
    -- exFDS value = ParArry ((list2Array . xb2Arr) value)
-- instance FDSType SemiXB where
    -- inFDS (ParArry value) = FDSValue ((arr2SemiXB . array2List) value)
    -- exFDS value = ParArry ((list2Array . semiXB2Arr) value)
-- instance FDSType IJK where
    -- inFDS (ParArry value) = FDSValue ((arr2IJK . array2List) value)
    -- exFDS value = ParArry ((list2Array . ijk2Arr) value)
-- instance FDSType RGB where
    -- inFDS (ParArry value) = FDSValue ((arr2RGB . array2List) value)
    -- exFDS value = ParArry ((list2Array . rgb2Arr) value)
-- instance (FDSType b) => FDSType [[FDSValue b]] where
    -- inFDS (ParArry value) = FDSValue (array2dList value)
    -- exFDS value = ParArry (list2dArray value)
-- instance (FDSType b) => FDSType [FDSValue b] where
    -- inFDS (ParArry value) = FDSValue (extractLong $ array2dList value)
    -- exFDS value = ParArry (list2dArray [value])
-- --instance (FDSType b) => FDSType [b] where
-- --    inFDS (ParArry value) = (head $ array2dList value)
-- --    exFDS value = ParArry (list2dArray [value])

-- extractLong :: [[a]] -> [a]
-- extractLong [x] = x
-- extractLong xs | length ys == 1 = head ys
    -- where ys = transpose xs

-- arr2XB :: [FDSValue Coord] -> XB
-- arr2XB [x1,x2,x3,x4,x5,x6] =
    -- XB x1 x2 x3 x4 x5 x6
-- xb2Arr :: XB -> [FDSValue Coord]
-- xb2Arr (XB x1 x2 x3 x4 x5 x6) = [x1,x2,x3,x4,x5,x6]
-- arr2SemiXB :: [FDSValue Coord] -> SemiXB
-- arr2SemiXB [x1,x2,x3] =
    -- SemiXB x1 x2 x3
-- semiXB2Arr :: SemiXB -> [FDSValue Coord]
-- semiXB2Arr (SemiXB x1 x2 x3) = [x1,x2,x3]
-- arr2IJK :: [FDSValue Int] -> IJK
-- arr2IJK [x1,x2,x3] =
    -- IJK x1 x2 x3
-- ijk2Arr :: IJK -> [FDSValue Int]
-- ijk2Arr (IJK x1 x2 x3) = [x1,x2,x3]
-- arr2RGB :: [FDSValue Int] -> RGB
-- arr2RGB [x1,x2,x3] =
    -- RGB x1 x2 x3
-- rgb2Arr :: RGB -> [FDSValue Int]
-- rgb2Arr (RGB x1 x2 x3) = [x1,x2,x3]
-- array2List :: (FDSType a) => (Array (Int,Int) ParameterValue) -> [FDSValue a]
-- array2List array = map inFDS (elems array)
-- list2Array :: (FDSType a) => [FDSValue a] -> (Array (Int,Int) ParameterValue)
-- list2Array paramValues = mkParArray $ zip (range ((1,1),((length paramValues),1))) (map exFDS paramValues)

-- array2dList :: (FDSType a) => (Array (Int,Int) ParameterValue) -> [[FDSValue a]]
-- array2dList array = map (filter ifArrayNotEmpty) $ divArr cols elms
    -- where bnds = snd (bounds array)
-- --          rows = fst bounds
          -- cols = snd bnds
          -- elms = map inFDS (elems array)
-- divArr :: (FDSType a) => Int -> [FDSValue a] -> [[FDSValue a]]
-- divArr _ [] = []
-- divArr cols elms = take cols elms : divArr cols (drop cols elms)
-- ifArrayNotEmpty :: (FDSType a) => FDSValue a -> Bool
-- ifArrayNotEmpty x = case x of
    -- FDSNone -> False
    -- _       -> True
-- ifArrayNotEmptyP :: ParameterValue -> Bool
-- ifArrayNotEmptyP x = case x of
    -- ArrayEmpty -> False
    -- _       -> True

-- list2dArray :: (FDSType a) => [[FDSValue a]] -> (Array (Int,Int) ParameterValue)
-- list2dArray lists = array bnds (zip assc (map exFDS elms))
    -- where rows = length lists
          -- cols = maximum (map length lists)
          -- bnds = ((1,1),(rows,cols))
          -- elms = concat lists
          -- assc = range bnds

-- instance (Show a,FDSType a) => Show a where
    -- show val = "FDSValue " ++ (show val)
    -- show FDSNone = "FDSNone"

-- class Render a where
    -- render :: a -> Text
-- instance (Show a,FDSType a) => Render a where
    -- render val = show val
    -- render FDSNone = "None"

