{-# LANGUAGE FlexibleInstances #-}
module FDSUtilities.Data where

import qualified Data.Array.Repa as R
import FDSUtilities.Types
import FDSUtilities.Parsing.SMVFile

ijkToXYZ :: MeshTRNs -> (Int, Int, Int) -> Maybe (Double, Double, Double)
ijkToXYZ (MeshTRNs xs ys zs) (i, j, k) = do
    x <- lookup i xs
    y <- lookup j ys
    z <- lookup k zs
    return (x, y, z)

-- TODO: this needs to be different as it involves rounding
-- xyzToIJK (MeshTRNs is js ks) (x, y, z) = do
--     i <- lookup x is
--     j <- lookup y js
--     k <- lookup z ks
--     return (x, y, z)

-- 1: find all slices that covere this cell with this data type. If there is
--    only one, continue else throw an error.
-- 2: apply probeSlice to the snapshots
-- |Create a one dimensional time series data vector from a slice file.
probeSlice
    :: SliceDataSet -- ^The slice data from which we will attempt to retrieve
                    -- the data.
    -> (Int, Int, Int) -- ^The (i,j,k) location of the data.
    -> [(Float, Float)] -- ^[(time, value)]
    -- -> DataVectorPair Float Float
probeSlice (SliceDataSet (SliceDataHeader a b c range@(i1,i2,j1,j2,k1,k2)) snapshots)
    globalPoint@(i,j,k)
    | pointInRange range globalPoint =
        fmap (takePoint localPoint) snapshots
    | otherwise = error "point not in range"
    where
        localPoint = (i-i1,j-j1,k-k1)

takePoint :: (Int, Int, Int) -> Snapshot -> (Float, Float)
takePoint (i, j, k) (Snapshot time array) = (time, value)
    where
        value = array R.! (R.Z R.:. i R.:. j R.:. k)

pointInRange
    :: (Ord a)
    => (a, a, a, a, a, a)
    -> (a, a, a)
    -> Bool
pointInRange (x1, x2, y1, y2, z1, z2) (x, y, z) = and
    [ x >= x1
    , x <= x2
    , y >= y1
    , y <= y2
    , z >= z1
    , z <= z2
    ]
-- import Text.Namelist
-- import Data.Array.IArray
-- import Data.List

-- data FDSCase = FDSCase
    -- { fdsCaseHead :: (Maybe Head)
    -- , fdsCaseTime :: (Maybe Time)
    -- , fdsCaseDump :: (Maybe Dump)
    -- , fdsCaseMisc :: (Maybe Misc)
    -- , fdsCaseMeshes :: [Mesh]
    -- , fdsCaseReacs :: [Reac]
    -- , fdsCaseDevcs :: [Devc]
    -- , fdsCaseMatls :: [Matl]
    -- , fdsCaseSurfs :: [Surf]
    -- , fdsCaseObsts :: [Obst]
    -- , fdsCaseHoles :: [Hole]
    -- , fdsCaseVents :: [Vent]
    -- , fdsCaseBndfs :: [Bndf]
    -- , fdsCaseIsofs :: [Isof]
    -- , fdsCaseSlcfs :: [Slcf]
    -- } deriving (Show)

-- data Bndf = Bndf
    -- { bndf_CELL_CENTRED_field :: (FDSValue Bool) -- CELL_CENTERED --default: False
    -- , bndf_FYI_field :: (FDSValue String)        -- FYI
    -- , bndf_PART_ID_field :: (FDSValue String)    -- PART_ID
    -- , bndf_PROP_ID_field :: (FDSValue String)    -- PROP_ID
    -- , bndf_RECOUNT_DRIP_field :: (FDSValue Bool) -- RECOUNT_DRIP --default: False
    -- , bndf_QUANTITY_field :: (FDSValue String)   -- QUANTITY
    -- , bndf_SPEC_ID_field :: (FDSValue String)    -- SPEC_ID
    -- }
    -- deriving (Show)
-- bndf_CELL_CENTRED = bndf_CELL_CENTRED_field
-- bndf_FYI = bndf_FYI_field
-- bndf_PART_ID = bndf_PART_ID_field
-- bndf_PROP_ID = bndf_PROP_ID_field
-- bndf_RECOUNT_DRIP = bndf_RECOUNT_DRIP_field
-- bndf_QUANTITY = bndf_QUANTITY_field
-- bndf_SPEC_ID = bndf_SPEC_ID_field

-- -- defBndf :: Maybe -> Maybe -> Maybe -> Maybe -> Maybe -> Maybe -> Maybe -> Bndf
-- -- defBndf cellCentered fyi partID propID recountDrip quantity specID =

-- data Clip = Clip
    -- { clip_FYI :: String                        -- FYI
    -- , clip_MAXIMUM_DENSITY :: Double            -- MAXIMUM_DENSITY
    -- , clip_MAXIMUM_MASS_FRACTION :: [[Double]]  -- MAXIMUM_MASS_FRACTION
    -- , clip_MAXIMUM_TEMPERATURE :: Double        -- MAXIMUM_TEMPERATURE
    -- , clip_MINIMUM_DENSITY :: Double            -- MINIMUM_DENSITY
    -- , clip_MINIMUM_MASS_FRACTION :: [[Double]]  -- MINIMUM_MASS_FRACTION
    -- , clip_MINIMUM_TEMPERATURE :: Double        -- MINIMUM_TEMPERATURE
    -- }
    -- deriving (Show)

-- data Ctrl = Ctrl
    -- { ctrl_DELAY :: Double          -- DELAY --default: 0
    -- , ctrl_FUNCTION_TYPE :: String  -- FUNCTION_TYPE
    -- , ctrl_ID :: String             -- ID
    -- , ctrl_INITIAL_DTATE :: Bool    -- INITIAL_STATE --default: False
    -- , ctrl_INPUT_ID :: String       -- INPUT_ID
    -- , ctrl_LATCH :: Bool            -- LATCH --default: True
    -- , ctrl_N :: Int                 -- N --default: 1
    -- , ctrl_ON_BOUND :: String       -- ON_BOUND --default: "LOWER"
    -- , ctrl_RAMP_ID :: String        -- RAMP_ID
    -- , ctrl_SETPOINT :: Double       -- SETPOINT
    -- }
    -- deriving (Show)

-- data Devc = Devc
-- --    { devc_BYPASS_FLOWRATE_field :: (FDSValue Double)
    -- { devc_CTRL_ID_field :: (FDSValue String)
-- --    , devc_DELAY_field :: (FDSValue Double)
-- --    , devc_DEVC_ID_field :: (FDSValue String)
-- --    , devc_DUCT_ID_field :: (FDSValue String)
-- --    , devc_DEPTH_field :: (FDSValue Double)
-- --    , devc_FLOWRATE_field :: (FDSValue Double)
    -- , devc_FYI_field :: (FDSValue String)
-- --    , devc_IOR_field :: (FDSValue Int)
    -- , devc_ID_field :: (FDSValue String)
-- --    , devc_INITIAL_STATE_field :: (FDSValue Bool)
-- --    , devc_LATCH_field :: (FDSValue Bool)
-- --    , devc_MATL_ID_field :: (FDSValue String)
-- --    , devc_NODE_ID_field :: (FDSValue String)
    -- , devc_ORIENTATION_field :: (FDSValue SemiXB)
-- --    , devc_PART_ID_field :: (FDSValue String)
-- --    , devc_POINTS_field :: (FDSValue Int)
    -- , devc_PROP_ID_field :: (FDSValue String)
    -- , devc_QUANTITY_field :: (FDSValue String)
    -- , devc_ROTATION_field :: (FDSValue SemiXB)
-- --    , devc_SETPOINT_field :: (FDSValue Double)
-- --    , devc_SPEC_ID_field :: (FDSValue String)
-- --    , devc_STATISTICS_field :: (FDSValue String)
-- --    , devc_SURF_ID_field :: (FDSValue String)
-- --    , devc_TIME_AVERAGED_field :: (FDSValue Bool)
-- --    , devc_TRIP_DIRECTION_field :: (FDSValue Int)
-- --    , devc_VELO_INDEX_field :: (FDSValue Int)
    -- , devc_XB_field :: (FDSValue XB)
    -- , devc_XYZ_field :: (FDSValue SemiXB)
    -- }
    -- deriving (Show)
-- --    -- ^1:BYPASS_FLOWRATE 2:CTRL_ID 3:DELAY 4:DEVC_ID 5:DUCT_ID 6:DEPTH 7:FLOWRATE
-- --    --  8:FYI 9:IOR 10:ID 11:INITIAL_STATE 12:LATCH 13:MATL_ID 14:NODE_ID 15:ORIENTATION
-- --    --  16:PART_ID 17:POINTS 18:PROP_ID 19:QUANTITY 20:ROTATION 21:SETPOINT 22:SPEC_ID
-- --    --  23:STATISTICS 24:SURF_ID 25:TIME_AVERAGED 26:TRIP_DIRECTION 27:VELO_INDEX
-- --    --  28:XB 29:XYZ

-- --devc_BYPASS_FLOWRATE  = (\(FDSValue a) -> a) . devc_BYPASS_FLOWRATE_field
-- devc_CTRL_ID  = (\(FDSValue a) -> a) . devc_CTRL_ID_field
-- --devc_DELAY  = (\(FDSValue a) -> a) . devc_DELAY_field
-- --devc_DEVC_ID  = (\(FDSValue a) -> a) . devc_DEVC_ID_field
-- --devc_DUCT_ID  = (\(FDSValue a) -> a) . devc_DUCT_ID_field
-- --devc_DEPTH  = (\(FDSValue a) -> a) . devc_DEPTH_field
-- --devc_FLOWRATE  = (\(FDSValue a) -> a) . devc_FLOWRATE_field
-- devc_FYI  = (\(FDSValue a) -> a) . devc_FYI_field
-- --devc_IOR  = (\(FDSValue a) -> a) . devc_IOR_field
-- devc_ID  = (\(FDSValue a) -> a) . devc_ID_field
-- --devc_INITIAL_STATE  = (\(FDSValue a) -> a) . devc_INITIAL_STATE_field
-- --devc_LATCH = (\(FDSValue a) -> a) . devc_LATCH_field
-- --devc_MATL_ID = (\(FDSValue a) -> a) . devc_MATL_ID_field
-- --devc_NODE_ID = (\(FDSValue a) -> a) . devc_NODE_ID_field
-- devc_ORIENTATION = (\(FDSValue a) -> a) . devc_ORIENTATION_field
-- --devc_PART_ID = (\(FDSValue a) -> a) . devc_PART_ID_field
-- --devc_POINTS  = (\(FDSValue a) -> a) . devc_POINTS_field
-- devc_PROP_ID  = (\(FDSValue a) -> a) . devc_PROP_ID_field
-- devc_QUANTITY  = (\(FDSValue a) -> a) . devc_QUANTITY_field
-- devc_ROTATION  = (\(FDSValue a) -> a) . devc_ROTATION_field
-- --devc_SETPOINT  = (\(FDSValue a) -> a) . devc_SETPOINT_field
-- --devc_SPEC_ID  = (\(FDSValue a) -> a) . devc_SPEC_ID_field
-- --devc_STATISTICS  = (\(FDSValue a) -> a) . devc_STATISTICS_field
-- --devc_SURF_ID  = (\(FDSValue a) -> a) . devc_SURF_ID_field
-- --devc_TIME_AVERAGED  = (\(FDSValue a) -> a) . devc_TIME_AVERAGED_field
-- --devc_TRIP_DIRECTION  = (\(FDSValue a) -> a) . devc_TRIP_DIRECTION_field
-- --devc_VELO_INDEX  = (\(FDSValue a) -> a) . devc_VELO_INDEX_field
-- devc_XB  = (\(FDSValue a) -> a) . devc_XB_field
-- devc_XYZ  = (\(FDSValue a) -> a) . devc_XYZ_field

-- data Dump = Dump
    -- (FDSValue Double) --dt_restart
    -- (FDSValue [FDSValue String]) --Plot3d_quantity
    -- (FDSValue Bool) --writexyz
    -- (FDSValue String) --render_file
    -- deriving (Show)
-- --    -- ^COLUMN_DUMP_LIMIT CTRL_COLUMN_LIMIT DEVC_COLUMN_LIMIT DT_BNDF
-- --    --  DT_CTRL DT_DEVC DT_DEVC_LINE DT_FLUSH DT_HRR DT_ISOF DT_MASS
-- --    --  DT_PART DT_PL3D DT_PROF DT_RESTART DT_SLCF FLUSH_FILE_BUFFERS
-- --    --  MASS_FILE MAXIMUM_DROPLETS NFRAMES PLOT3D_QUANTITY PLOT3D_SPEC_ID
-- --    --  PLOT3D_VELO_INDEX SMOKE3D SMOKE3D_QUANTITY SMOKE3D_SPEC_ID
-- --    --  STATE_FILE STATUS_FILES WRITE_XYZ RENDER_FILE

-- data Head = Head
    -- (FDSValue String)
    -- (FDSValue String)
    -- (FDSValue String)
    -- deriving (Show)
-- -- ^CHID FYI TITLE

-- data Hole = Hole
    -- (FDSValue XB)
    -- deriving (Show)
-- -- ^XB
-- data Hvac = Hvac
-- data Init = Init
-- data Isof = Isof
    -- (FDSValue String)
    -- (FDSValue Double)
    -- deriving (Show)
-- -- ^QUANTITY VALUE
-- ----------------------------------------
-- data Matl = Matl
    -- (FDSValue String)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Int)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- PyrolysisReac
    -- deriving (Show)
-- -- ^ID SPECIFIC_HEAT CONDUCTIVITY DENSITY HEAT_OF_COMBUSTION N_REACTIONS NU_FUEL NS Pyrolysis_Properties

-- malt_ID (Matl id _ _ _ _ _ _ _ _) = (\(FDSValue a) -> a) id
-- matl_SPECIFIC_HEAT (Matl _ specific_heat _ _ _ _ _ _ _) = (\(FDSValue a) -> a) specific_heat
-- matl_CONDUCTIVITY  (Matl _ _ conductivity _ _ _ _ _ _) = (\(FDSValue a) -> a) conductivity
-- matl_DENSITY  (Matl _ _ _ density _ _ _ _ _) = (\(FDSValue a) -> a) density
-- matl_HEAT_OF_COMBUSTION  (Matl _ _ _ _ heat_of_combustion _ _ _ _) = (\(FDSValue a) -> a) heat_of_combustion
-- matl_N_REACTIONS  (Matl _ _ _ _ _ n_reactions _ _ _) = (\(FDSValue a) -> a) n_reactions
-- matl_NU_FUEL  (Matl _ _ _ _ _ _ nu_fuel _ _) = (\(FDSValue a) -> a) nu_fuel
-- matl_NS  (Matl _ _ _ _ _ _ _ ns _) = (\(FDSValue a) -> a) ns
-- malt_PyrReac  (Matl _ _ _ _ _ _ _ _ pyrolysis_reaction) = pyrolysis_reaction
-- ----------------------------------------
-- data Mesh = Mesh
    -- (FDSValue String)
    -- (FDSValue XB)
    -- (FDSValue IJK)
    -- deriving (Show)
-- -- ^ID XB IJK
-- mesh_ID (Mesh id _ _) = (\(FDSValue a) -> a) id
-- mesh_XB (Mesh _ xb _) = (\(FDSValue a) -> a) xb
-- mesh_IJK (Mesh _ _ ijk) = (\(FDSValue a) -> a) ijk

-- data Misc = Misc
    -- (FDSValue String)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- deriving (Show)
-- -- ^SURF_DEFAULT HUMIDITY TMPA

-- data Mult = Mult
-- data Obst = Obst
    -- { obst_XB_field :: (FDSValue XB)      -- ^XB
    -- , obst_SURF_ID_field :: Surf    -- ^Surf ID
    -- }
    -- deriving (Show)
-- obst_XB = (\(FDSValue a) -> a) . obst_XB_field
-- obst_SURF_ID = obst_SURF_ID_field

-- data Part = Part
-- data Pres = Pres
-- data Prof = Prof
-- data Prop = Prop
-- data Radi = Radi
-- data Ramp = Ramp
-- data Reac = Reac
    -- (FDSValue String)
    -- (FDSValue String)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- deriving (Show)
-- -- ^ID FYI SOOT_YIELD N C H O OTHER MW_OTHER Y_CO Y_H2 HEAT_OF_COMBUSTION EPUMO2 HFRAC

-- data PyrolysisReac = PyrolysisReacAE
        -- (FDSValue Double)
        -- (FDSValue Double)
        -- (FDSValue Double)   -- ^A E HEAT_OF_REACTION
    -- | PyrolysisReacTGM
        -- (FDSValue Double)
        -- (FDSValue Double)
        -- (FDSValue Double)
        -- (FDSValue Double)   -- ^REFERENCE_TEMPERATURE HEATING_RATE PYROLYSIS_RANGE HEAT_OF_REACTION
    -- | NoPyrolysis
    -- deriving (Show)
-- data Slcf = Slcf
    -- (FDSValue String)  -- QUANTITY
    -- (FDSValue Bool)    -- VECTOR
    -- (FDSValue Plane)   -- Plane
    -- (FDSValue Double)  -- Offset
    -- deriving (Show)

-- data Spec = Spec
-- data Surf = Surf
    -- { surf_ID_field :: (FDSValue String)
    -- , surf_RGB_field :: (FDSValue RGB)
    -- , surf_COLOR_field :: (FDSValue String)
    -- , surf_BURN_AWAY_field :: (FDSValue Bool)
    -- , surf_Layers_field :: [SurfLayer]
    -- , surf_Burner_field :: SurfBurner
    -- }
    -- deriving (Show)
-- -- ^ID THICKNESS RGB BURN_AWAY
-- surf_ID = (\(FDSValue a) -> a) . surf_ID_field
-- surf_RGB = surf_RGB_field
-- surf_BURN_AWAY = (\(FDSValue a) -> a) . surf_BURN_AWAY_field
-- surf_Layers = surf_Layers_field

-- data SurfLayer = SurfLayer
-- --    { surfLayer_Position_field :: Int     -- ^Position
-- --    { surfLayer_Surf_field :: Surf    -- ^Surface
    -- { surfLayer_THICKNESS_field :: (FDSValue Double)
    -- , surfLayer_Components_field :: [SurfLayerComponent]
    -- }
    -- deriving (Show)
-- --surfLayer_Position = surfLayer_Position_field
-- --surfLayer_Surf = surfLayer_Surf_field
-- surfLayer_THICKNESS = (\(FDSValue a) -> a) . surfLayer_THICKNESS_field
-- surfLayer_Components = surfLayer_Components_field
-- data SurfLayerComponent = SurfLayerComponent
-- --    { surfLayerComponent_Pos_field :: Int
-- --    , surfLayerComponent_ParentLayer_field :: SurfLayer
    -- { surfLayerComponent_MATL_field :: Matl
-- --    { surfLayerComponent_MATL_field :: (FDSValue String)
    -- , surfLayerComponent_MATL_MASS_FRACTION_field :: (FDSValue Double)
    -- }
    -- deriving (Show)
-- --surfLayerComponent_Pos = surfLayerComponent_Pos_field
-- --surfLayerComponent_ParentLayer = surfLayerComponent_ParentLayer_field
-- surfLayerComponent_MATL = surfLayerComponent_MATL_field
-- surfLayerComponent_MATL_MASS_FRACTION = (\(FDSValue a) -> a) . surfLayerComponent_MATL_MASS_FRACTION_field
-- data SurfBurner = SurfBurner
-- --    { surfLayerComponent_Pos_field :: Int
-- --    , surfLayerComponent_ParentLayer_field :: SurfLayer
-- --    { surfLayerComponent_MATL_field :: Matl
    -- { surfBurner_HRRPUA_field :: (FDSValue Double)
    -- , surfBURNER_TAU_Q_field :: (FDSValue Double)
    -- }
    -- | NoBurner
    -- deriving (Show)

-- data Tabl = Tabl
-- data Time = Time
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Double)
    -- (FDSValue Bool)
    -- deriving (Show)
-- -- ^DT T_BEGIN T_END SYNCHRONIZE

-- data Trnx = Trnx
-- data Trny = Trny
-- data Trnz = Trnz
-- data Vent = Vent
    -- (FDSValue String)
    -- (FDSValue XB)
    -- (FDSValue String)
    -- deriving (Show)
-- -- ^SURF_ID XB COLOR
-- data Zone = Zone

-- data Plane = X | Y | Z
    -- deriving (Show)
-- data IJK = IJK
    -- Int
    -- Int
    -- Int
    -- deriving (Show)
-- -- ^I J K
-- data RGB = RGB
    -- Int
    -- Int
    -- Int
    -- deriving (Show)
-- -- ^R G B
-- data XB = XB
    -- Coord
    -- Coord
    -- Coord
    -- Coord
    -- Coord
    -- Coord
    -- deriving (Show)
-- -- ^x1 x2 y1 y2 z1 z2
-- data SemiXB = SemiXB
    -- Coord
    -- Coord
    -- Coord
    -- deriving (Show)

-- type Coord = Double

-- type GridCoord = Int

-- --data (FDSType a) => FDSValue  a = FDSValue a | FDSNone
-- data FDSValue  a = FDSValue a | FDSNone

-- class FDSType a where
    -- inFDS :: ParameterValue -> FDSValue a
    -- inFDS ArrayEmpty = FDSNone

    -- exFDS :: FDSValue a -> ParameterValue

-- instance FDSType String where
    -- inFDS (ParString value) = FDSValue value
    -- inFDS ArrayEmpty = FDSNone
    -- exFDS (FDSValue value) = ParString value
-- instance FDSType Int where
    -- inFDS (ParInt value) = FDSValue value
    -- inFDS ArrayEmpty = FDSNone
    -- exFDS (FDSValue value) = ParInt value
-- instance FDSType Double where
    -- inFDS (ParDouble value) = FDSValue value
    -- inFDS ArrayEmpty = FDSNone
    -- exFDS (FDSValue value) = ParDouble value
-- instance FDSType Bool where
    -- inFDS (ParBool value) = FDSValue value
    -- inFDS ArrayEmpty = FDSNone
    -- exFDS (FDSValue value) = ParBool value
-- instance FDSType Plane where
    -- inFDS (ParString value) = case value of
        -- "X" -> FDSValue X
        -- "Y" -> FDSValue Y
        -- "Z" -> FDSValue Z
    -- inFDS ArrayEmpty = FDSNone
    -- exFDS (FDSValue value) = case value of
        -- X -> ParString "X"
        -- Y -> ParString "Y"
        -- Z -> ParString "Z"
-- instance FDSType XB where
    -- inFDS (ParArry value) = FDSValue ((arr2XB . array2List) value)
    -- exFDS (FDSValue value) = ParArry ((list2Array . xb2Arr) value)
-- instance FDSType SemiXB where
    -- inFDS (ParArry value) = FDSValue ((arr2SemiXB . array2List) value)
    -- exFDS (FDSValue value) = ParArry ((list2Array . semiXB2Arr) value)
-- instance FDSType IJK where
    -- inFDS (ParArry value) = FDSValue ((arr2IJK . array2List) value)
    -- exFDS (FDSValue value) = ParArry ((list2Array . ijk2Arr) value)
-- instance FDSType RGB where
    -- inFDS (ParArry value) = FDSValue ((arr2RGB . array2List) value)
    -- exFDS (FDSValue value) = ParArry ((list2Array . rgb2Arr) value)
-- instance (FDSType b) => FDSType [[FDSValue b]] where
    -- inFDS (ParArry value) = FDSValue (array2dList value)
    -- exFDS (FDSValue value) = ParArry (list2dArray value)
-- instance (FDSType b) => FDSType [FDSValue b] where
    -- inFDS (ParArry value) = FDSValue (extractLong $ array2dList value)
    -- exFDS (FDSValue value) = ParArry (list2dArray [value])
-- --instance (FDSType b) => FDSType [b] where
-- --    inFDS (ParArry value) = (head $ array2dList value)
-- --    exFDS value = ParArry (list2dArray [value])

-- extractLong :: [[a]] -> [a]
-- extractLong [x] = x
-- extractLong xs | length ys == 1 = head ys
    -- where ys = transpose xs

-- arr2XB :: [FDSValue Coord] -> XB
-- arr2XB [(FDSValue x1),(FDSValue x2),(FDSValue x3),(FDSValue x4),(FDSValue x5),(FDSValue x6)] =
    -- XB x1 x2 x3 x4 x5 x6
-- xb2Arr :: XB -> [FDSValue Coord]
-- xb2Arr (XB x1 x2 x3 x4 x5 x6) = [(FDSValue x1),(FDSValue x2),(FDSValue x3),(FDSValue x4),(FDSValue x5),(FDSValue x6)]
-- arr2SemiXB :: [FDSValue Coord] -> SemiXB
-- arr2SemiXB [(FDSValue x1),(FDSValue x2),(FDSValue x3)] =
    -- SemiXB x1 x2 x3
-- semiXB2Arr :: SemiXB -> [FDSValue Coord]
-- semiXB2Arr (SemiXB x1 x2 x3) = [(FDSValue x1),(FDSValue x2),(FDSValue x3)]
-- arr2IJK :: [FDSValue Int] -> IJK
-- arr2IJK [(FDSValue x1),(FDSValue x2),(FDSValue x3)] =
    -- IJK x1 x2 x3
-- ijk2Arr :: IJK -> [FDSValue Int]
-- ijk2Arr (IJK x1 x2 x3) = [(FDSValue x1),(FDSValue x2),(FDSValue x3)]
-- arr2RGB :: [FDSValue Int] -> RGB
-- arr2RGB [(FDSValue x1),(FDSValue x2),(FDSValue x3)] =
    -- RGB x1 x2 x3
-- rgb2Arr :: RGB -> [FDSValue Int]
-- rgb2Arr (RGB x1 x2 x3) = [(FDSValue x1),(FDSValue x2),(FDSValue x3)]
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

-- instance (Show a,FDSType a) => Show (FDSValue a) where
    -- show (FDSValue val) = "FDSValue " ++ (show val)
    -- show FDSNone = "FDSNone"

-- class Render a where
    -- render :: a -> String
-- instance (Show a,FDSType a) => Render (FDSValue a) where
    -- render (FDSValue val) = show val
    -- render FDSNone = "None"
