{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.FDSFile.Utilities where

import           FDSUtilities.Simulation
import           FDSUtilities.Types.Assess
import           FDSUtilities.Types
import           FDSUtilities.FDSFile.Types
import           Data.List
import           System.Directory
import           System.FilePath
import           System.FilePath.Glob

import           Debug.Trace
import qualified Data.Text as T
import           Data.Maybe (isJust, fromMaybe)
import           Data.Monoid ((<>))
import           Data.Tree

import qualified Data.Array as A
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Vector as V

import           Text.Namelist
import           Text.Parsec.Pos

import           GHC.Stack

default (T.Text)

data SurfaceIdSpec
    = SingleSurfId String
    | TripleSurfId String String String
    | SixSurfId String String String String String String
    deriving (Show, Eq)

data SurfaceSpec
    = SingleSurf Surf
    | TripleSurf Surf Surf Surf
    | SixSurf Surf Surf Surf Surf Surf Surf
    deriving (Show, Eq)

-- |This is complicated as an element can have 0-6 different surfs
class FDSElement a => HasSurf a where
    getSurfIds :: HasCallStack => a -> SurfaceIdSpec

instance HasSurf Burner where
    getSurfIds (BurnerObst obst) = getSurfIds obst
    getSurfIds (BurnerVent vent) = getSurfIds vent

instance HasSurf Mech where
    getSurfIds (MechObst obst) = getSurfIds obst
    getSurfIds (MechVent vent) = getSurfIds vent

instance HasSurf Obst where
    getSurfIds obst = case obst_SURF_ID obst of
        Just x -> SingleSurfId x
        Nothing -> case obst_SURF_IDS obst of
            Just (s1, s2, s3) -> TripleSurfId s1 s2 s3
            Nothing -> case obst_SURF_ID6 obst of
                Just (s1, s2, s3, s4, s5, s6) -> SixSurfId s1 s2 s3 s4 s5 s6
                Nothing -> SingleSurfId "INERT"

instance HasSurf Vent where
    getSurfIds vent = case vent_SURF_ID vent of
        Just x -> SingleSurfId x
        Nothing -> SingleSurfId "INERT"

class MightHaveXB a where
    tryGetXB :: a -> Maybe XB

instance MightHaveXB Obst where
    tryGetXB = pure . getXB

instance MightHaveXB Vent where
    tryGetXB = pure . getXB

instance MightHaveXB Burner where
    tryGetXB = pure . getXB

instance MightHaveXB Mech where
    tryGetXB = pure . getXB

instance MightHaveXB Mesh where
    tryGetXB = pure . getXB

instance MightHaveXB Devc where
    tryGetXB = devc_XB

class (MightHaveXB a) => HasXB a where
    getXB :: a -> XB

instance HasXB Obst where
    getXB = obst_XB

instance HasXB Vent where
    getXB = (fromMaybe (error "VENT does not have XB")) . vent_XB

instance HasXB Burner where
    getXB (BurnerObst obst) = getXB obst
    getXB (BurnerVent vent) = getXB vent

instance HasXB Mech where
    getXB (MechObst obst) = getXB obst
    getXB (MechVent vent) = getXB vent

instance HasXB Mesh where
    getXB = mesh_XB

class FDSElement a => HasXYZ a where
    getXYZ :: a -> XYZ

class MightHaveXYZ a where
    tryGetXYZ :: a -> Maybe XYZ

instance MightHaveXYZ Devc where
    tryGetXYZ = devc_XYZ

data Burner
    = BurnerObst Obst
    | BurnerVent Vent
    deriving (Show, Eq)

getBurnerId :: Burner -> Maybe String
getBurnerId (BurnerObst obst) = getId obst
getBurnerId (BurnerVent vent) = getId vent

data BurnerFace = BurnerFace
    { bf_area :: Double
    , bf_surf :: Surf
    } deriving (Show, Eq)

data Mech
    = MechObst Obst
    | MechVent Vent
    deriving (Show, Eq)

getMechId :: Mech -> Maybe String
getMechId (MechObst obst) = getId obst
getMechId (MechVent vent) = getId vent

getIdBound :: FDSElement a => a -> String
getIdBound nml = case getId nml of
    Just x -> x
    Nothing -> "Unnamed " ++ getNMLName nml

getSurfs :: HasSurf a => FDSFile -> a -> SurfaceSpec
getSurfs fdsData = (getSurfsFromIds fdsData) . getSurfIds

getSurfsFromIds :: FDSFile -> SurfaceIdSpec -> SurfaceSpec
getSurfsFromIds fdsData idSpec = case idSpec of
        SingleSurfId s1 -> SingleSurf (find s1)
        TripleSurfId s1 s2 s3 -> TripleSurf (find s1) (find s2) (find s3)
        SixSurfId s1 s2 s3 s4 s5 s6 ->
            SixSurf (find s1) (find s2) (find s3) (find s4) (find s5) (find s6)
    where
        surfs = fdsFile_Surfs fdsData
        find surfId =
            let matches = filter (\surf->(getId surf) == (Just surfId)) surfs
            in case matches of
                [s] -> s
                [] -> error $ "No matching surface for: " <> show surfId
                x -> error ("Multiple matching surfs: " <> show x)

getSurfList :: HasSurf a => FDSFile -> a -> [Surf]
getSurfList fdsData nml = case getSurfs fdsData nml of
    SingleSurf surf -> [surf]
    TripleSurf surf1 surf2 surf3 -> [surf1, surf2, surf3]
    SixSurf surf1 surf2 surf3 surf4 surf5 surf6 ->
        [surf1, surf2, surf3, surf4, surf5, surf6]

class FDSElement a where
    getId :: a -> Maybe String
    getFYI :: a -> Maybe String
    getNMLName :: a -> String

instance FDSElement Mesh where
    getId = mesh_ID
    getFYI = mesh_FYI
    getNMLName = const "MESH"

instance FDSElement Surf where
    getId = surf_ID
    getFYI = surf_FYI
    getNMLName = const "SURF"

instance FDSElement Obst where
    getId = obst_ID
    getFYI = obst_FYI
    getNMLName = const "OBST"

instance FDSElement Vent where
    getId = vent_ID
    getFYI = vent_FYI
    getNMLName = const "VENT"

instance FDSElement Ramp where
    getId = pure . ramp_ID
    getFYI ramp = rampEntry_FYI (head (ramp_entries ramp))
    getNMLName = const "RAMP"

instance FDSElement Prop where
    getId = prop_ID
    getFYI = prop_FYI
    getNMLName = const "PROP"

instance FDSElement Devc where
    getId = devc_ID
    getFYI = devc_FYI
    getNMLName = const "DEVC"

instance FDSElement Burner where
    getId (BurnerObst obst) = getId obst
    getId (BurnerVent vent) = getId vent
    getFYI (BurnerObst obst) = getFYI obst
    getFYI (BurnerVent vent) = getFYI vent
    getNMLName (BurnerObst obst) = getNMLName obst
    getNMLName (BurnerVent vent) = getNMLName vent

instance FDSElement Mech where
    getId (MechObst obst) = getId obst
    getId (MechVent vent) = getId vent
    getFYI (MechObst obst) = getFYI obst
    getFYI (MechVent vent) = getFYI vent
    getNMLName (MechObst obst) = getNMLName obst
    getNMLName (MechVent vent) = getNMLName vent

getNCells :: Mesh -> Int
getNCells mesh =
    let
        ijk = mesh_IJK mesh
        i = ijk_i ijk
        j = ijk_j ijk
        k = ijk_k ijk
    in i*j*k

-- |Check if a device is stuck in a solid. Returns Nothing if it's not a
-- sensible question (e.g. it is not a point device).
stuckInSolid :: FDSFile -> Devc -> Maybe Bool
stuckInSolid fdsData devc = do
    xyz <- tryGetXYZ devc
    cell <- determineCell fdsData xyz
    pure $ isCellSolid fdsData cell

-- |Check if the cell directly above a device is solid. This is useful to make
-- sure that sprinklers and smoke detectors are directly beneath the a ceiling.
--
-- TODO: This is more complicated as it may not be a solid cell, but a solid
-- surface. This is exacerbated by being on a mesh boundary.
beneathCeiling :: FDSFile -> Devc -> Maybe Bool
beneathCeiling fdsData devc = do
    xyz <- tryGetXYZ devc
    cell <- determineCell fdsData xyz
    pure $ isFaceSolid fdsData cell PosZ

-- |((MeshNum, IJK), FACEDIR)
type Face = ((Int, (Int, Int, Int)), Direction)

isSolidFace :: FDSFile-> Face -> Bool
isSolidFace fdsData face@(cell, dir) =
    let (a,b,c,d,e,f) = getCellFacesSolidness fdsData cell
    in case dir of
        NegX -> a
        PosX -> b
        NegY -> c
        PosY -> d
        NegZ -> e
        PosZ -> f

getCellFacesSolidness :: FDSFile -> (Int, (Int, Int, Int))
    -> (Bool, Bool, Bool, Bool, Bool, Bool)
getCellFacesSolidness fdsData cell = if thisCell
    -- If this cell is sold then all its face must be solid. This is the
    -- simplest case.
    then (True, True, True, True, True, True)
    -- Otherwise we need to test the cells around it, however, we need to be
    -- careful as we might need to step out of the particulary mesh.
    --
    -- We also need to account for thin obstructions. This means making a data
    -- structure that shows the solidness of each face.
    --
    -- We also need to consider when the cell above is only partially covered.
    else undefined
    where
        thisCell = isCellSolid fdsData cell

-- |Determine the cell in which a point lies. The output is (MeshNum, (I,J,K)).
determineCell :: FDSFile -> XYZ
    -> Maybe (Int,(Int,Int,Int))
determineCell fdsData point = do
    meshIndex <- determineMeshIndex fdsData point
    let mesh = (fdsFile_Meshes fdsData) !! (meshIndex - 1)
    cell <- determineCellInMesh fdsData point mesh
    pure (meshIndex, cell)

-- |Determine in which cell within a mesh a point lies. Return Nothing if the
-- point does not lie within the mesh.
determineCellInMesh :: FDSFile -> XYZ -> Mesh
    -> Maybe (Int,Int,Int)
determineCellInMesh fdsData point@(XYZ x y z) mesh = do
    iCell <- findPointInLine xs x
    jCell <- findPointInLine ys y
    kCell <- findPointInLine zs z
    pure (iCell, jCell, kCell)
    where
        (xs, ys, zs) = getMeshLines fdsData mesh

moveInMesh :: FDSFile -> (Int,(Int,Int,Int)) -> Direction
    -> Maybe [(Int,(Int,Int,Int))]
moveInMesh fdsData currentCell dir = undefined

findPointInLine :: [Double] -> Double -> Maybe Int
findPointInLine [] _ = Nothing
findPointInLine (x:xs) p = if (p < x) then Nothing
    else findPointInLine' 0 xs p

findPointInLine' :: Int -> [Double] -> Double -> Maybe Int
findPointInLine' _ [] _ = Nothing
findPointInLine' index (x:xs) p = if p < x
    then Just index
    else findPointInLine' (index + 1) xs p

-- |Determine which mesh the point is in. Output is MeshNum. This assumes that
-- there are no overlapping meshes.
determineMesh :: FDSFile -> XYZ -> Maybe Mesh
determineMesh fdsData point = find (isInMesh point) meshes
        where
            meshes = fdsFile_Meshes fdsData :: [Mesh]

-- |Same as determineMesh but returns the index of the mesh rather than the mesh
-- itself.
determineMeshIndex :: FDSFile -> XYZ -> Maybe Int
determineMeshIndex fdsData point = fmap (+1) $ findIndex (isInMesh point) meshes
        where
            meshes = fdsFile_Meshes fdsData :: [Mesh]

-- |Determine if a face is an 'OPEN' vent at cell @cell@ and direction @dir@.
-- NB: This does not consider MB style mesh boundary specs
isFaceOpenVent :: FDSFile -> (Int, (Int, Int, Int)) -> Direction -> Bool
isFaceOpenVent fdsData cell dir =
    case find
        (\xb-> faceOccupy cellSize xb faceXB)
        ((fmap getXB openObsts) <> (fmap getXB openVents)) of
            Nothing -> False
            Just _ -> True
    where
        cellSize = getMinDim fdsData cell
        faceXB = getFaceXB fdsData cell dir
        openObsts = filter (hasOpenSurf fdsData) $ fdsFile_Obsts fdsData
        openVents = filter (hasOpenSurf fdsData) $ fdsFile_Vents fdsData

hasOpenSurf :: HasSurf a => FDSFile -> a -> Bool
hasOpenSurf fdsData nml = any isOpenSurf (getSurfList fdsData nml)

isOpenSurf :: Surf -> Bool
isOpenSurf surf =
    let id = surf_ID surf
    in case id of
        Just x -> x == "OPEN"
        Nothing -> False

-- |Get the solidness of a single face at cell @cell@ and direction @dir@. NB:
-- This does not consider neighbouring cells.
isFaceSolid :: FDSFile -> (Int, (Int, Int, Int)) -> Direction -> Bool
isFaceSolid fdsData cell dir =
    case find (\xb->faceOccupy cellSize xb faceXB) obstsAndVentsXBs of
        Nothing ->
            -- Face is an external mesh boundary
            (isFaceExternalMeshBoundary fdsData cell PosZ)
                -- Which is not covered by an 'OPEN' vent
                && (not (isFaceOpenVent fdsData cell PosZ))
        Just _ -> True
    where
        cellSize = getMinDim fdsData cell
        faceXB = getFaceXB fdsData cell dir
        -- Exclude 'OPEN' vents and obsts, as they are not solid
        obstsAndVentsXBs = ((fmap getXB solidObsts) <> (fmap getXB solidVents))
        solidObsts =
            filter (not . (hasOpenSurf fdsData)) $ fdsFile_Obsts fdsData
        solidVents =
            filter (not . (hasOpenSurf fdsData)) $ fdsFile_Vents fdsData

-- |Determine if a face is an external mesh boundary. I.e., it could be 'OPEN'.
isFaceExternalMeshBoundary :: FDSFile -> (Int, (Int, Int, Int)) -> Direction
    -> Bool
isFaceExternalMeshBoundary fdsData cell@(meshNum, (i,j,k)) dir =
    let
        mesh = (fdsFile_Meshes fdsData) !! (meshNum - 1)
        -- First we need to determine if the cell is on the edge of the mesh (in
        -- the chosen direction) | @cellN@ is the cell number in the chosen
        -- direction
        cellN = case dir of
            PosX -> i
            NegX -> i
            PosY -> j
            NegY -> j
            PosZ -> k
            NegZ -> k
        (meshMaxI, meshMaxJ, meshMaxK) =
            -- These are lines not cells
            let (xs, ys, zs) = getMeshLines fdsData mesh
                nCellsI = length xs - 1
                nCellsJ = length ys - 1
                nCellsK = length zs - 1
            -- We need to subtract 1 to go from the quantity to the max index
            in (nCellsI - 1, nCellsJ - 1, nCellsK - 1)
        -- | @maxCellN@ is the boundary cell number of the mesh in the chosen
        -- direction
        maxCellN = case dir of
            PosX -> meshMaxI
            NegX -> 0
            PosY -> meshMaxJ
            NegY -> 0
            PosZ -> meshMaxK
            NegZ -> 0
        -- This determines if the cell is at the edge of the mesh in the chosen
        -- direction.
        cellIsMeshBoundary = cellN == maxCellN
        -- TODO: how do we determine if the cell is external?
        --
        -- Next we need to determine if there is another mesh on the other side
        -- of this boundary. I.e., determine whether it is external.
        --
        -- To do this we will take the midpoint of the face, then go "up" a
        -- small amount in the direction of the normal axis. We will then check
        -- if this point lies within another mesh. This is not a great way to do
        -- this, but it will suffice for now, until we have better data
        -- structures in place.
        --
        -- TODO: improve this.
        faceMidPoint =
            let (XB x1 x2 y1 y2 z1 z2) = getFaceXB fdsData cell dir
            in case dir of
                PosX -> midpointXB (XB x2 x2 y1 y2 z1 z2)
                NegX -> midpointXB (XB x1 x1 y1 y2 z1 z2)
                PosY -> midpointXB (XB x1 x2 y2 y2 z1 z2)
                NegY -> midpointXB (XB x1 x2 y1 y1 z1 z2)
                PosZ -> midpointXB (XB x1 x2 y1 y2 z2 z2)
                NegZ -> midpointXB (XB x1 x2 y1 y2 z1 z1)
        eps = 0.000001
        faceMidPointPlus = case dir of
            PosX -> faceMidPoint `addXYZ` (XYZ    eps      0      0)
            NegX -> faceMidPoint `addXYZ` (XYZ (-eps)      0      0)
            PosY -> faceMidPoint `addXYZ` (XYZ      0    eps      0)
            NegY -> faceMidPoint `addXYZ` (XYZ      0 (-eps)      0)
            PosZ -> faceMidPoint `addXYZ` (XYZ      0      0    eps)
            NegZ -> faceMidPoint `addXYZ` (XYZ      0      0 (-eps))
    in and
        [ cellIsMeshBoundary
        -- check if the point just over the bounday is within any mesh
        , not $ any
            (\mesh->isInMesh faceMidPointPlus mesh)
            (fdsFile_Meshes fdsData)
        ]

addXYZ :: XYZ -> XYZ -> XYZ
addXYZ (XYZ x1 y1 z1) (XYZ x2 y2 z2) = (XYZ (x1+x2) (y1+y2) (z1+z2))

midpointXB :: XB -> XYZ
midpointXB (XB x1 x2 y1 y2 z1 z2) = XYZ ((x1+x2)/2) ((y1+y2)/2) ((z1+z2)/2)

getFaceXB :: FDSFile -> (Int, (Int, Int, Int)) -> Direction -> XB
getFaceXB fdsData cell dir =
    let cellXB = getCellXB fdsData cell
        XB x1A x2A y1A y2A z1A z2A = sortXB cellXB
    in case dir of
        NegX -> XB x1A x1A y1A y2A z1A z2A
        PosX -> XB x2A x2A y1A y2A z1A z2A
        NegY -> XB x1A x2A y1A y1A z1A z2A
        PosY -> XB x1A x2A y2A y2A z1A z2A
        NegZ -> XB x1A x2A y1A y2A z1A z1A
        PosZ -> XB x1A x2A y1A y2A z2A z2A

-- |Use the OBST namelists to determine if a particular cell is solid or not.
-- TODO: only considers basic OBSTs and not MULT or the like.
isCellSolid :: FDSFile -> (Int, (Int, Int, Int)) -> Bool
isCellSolid fdsData cell@(meshNum,(i,j,k)) =
    case find (\obst->xbOccupy (getXB obst) cellXB) obsts of
        Nothing -> False
        Just _ -> True
    where
        cellXB = getCellXB fdsData cell
        -- If any obst overlaps with this cell, then it's solid
        obsts = fdsFile_Obsts fdsData

-- |Determine if if the first XB occupies more than or equal to 50% of the
-- second XB in all dimensions. This is used to determine if an obstruction
-- (first XB) causes a cell (second XB) to be solid or not.
xbOccupy :: XB -> XB -> Bool
xbOccupy xbA xbB = occupyX && occupyY && occupyZ
    where
        occupyX = occupyFatly (x1A,x2A) (x1B,x2B)
        occupyY = occupyFatly (y1A,y2A) (y1B,y2B)
        occupyZ = occupyFatly (z1A,z2A) (z1B,z2B)

        XB x1A x2A y1A y2A z1A z2A = sortXB xbA
        XB x1B x2B y1B y2B z1B z2B = sortXB xbB

-- |This is a lower requirement than xbOccupy. All xbOccupy satisfies this as
-- well.
faceOccupy :: Double -> XB -> XB -> Bool
faceOccupy cellSize xbA xbB@(XB xMin xMax yMin yMax zMin zMax) =
    case (xSame, ySame, zSame) of
        (True, False, False) -> faceOccupyX cellSize xbA xbB
        (False, True, False) -> faceOccupyY cellSize xbA xbB
        (False, False, True) -> faceOccupyZ cellSize xbA xbB
        _ -> error "Not a face"
    where
        xSame = xMin == xMax
        ySame = yMin == yMax
        zSame = zMin == zMax

faceOccupyX :: Double -> XB -> XB -> Bool
faceOccupyX cellSize xbA xbB = occupyX && occupyY && occupyZ
    where
        occupyX = occupyThinly (x1A,x2A) (x1B-(cellSize/2),x2B+(cellSize/2))
        occupyY = occupyFatly (y1A,y2A) (y1B,y2B)
        occupyZ = occupyFatly (z1A,z2A) (z1B,z2B)

        XB x1A x2A y1A y2A z1A z2A = sortXB xbA
        XB x1B x2B y1B y2B z1B z2B = sortXB xbB

faceOccupyY :: Double -> XB -> XB -> Bool
faceOccupyY cellSize xbA xbB = occupyX && occupyY && occupyZ
    where
        occupyX = occupyFatly (x1A,x2A) (x1B,x2B)
        occupyY = occupyThinly (y1A,y2A) (y1B-(cellSize/2),y2B+(cellSize/2))
        occupyZ = occupyFatly (z1A,z2A) (z1B,z2B)

        XB x1A x2A y1A y2A z1A z2A = sortXB xbA
        XB x1B x2B y1B y2B z1B z2B = sortXB xbB

faceOccupyZ :: Double -> XB -> XB -> Bool
faceOccupyZ cellSize xbA xbB = occupyX && occupyY && occupyZ
    where
        occupyX = occupyFatly (x1A,x2A) (x1B,x2B)
        occupyY = occupyFatly (y1A,y2A) (y1B,y2B)
        occupyZ = occupyThinly (z1A,z2A) (z1B-(cellSize/2),z2B+(cellSize/2))

        XB x1A x2A y1A y2A z1A z2A = sortXB xbA
        XB x1B x2B y1B y2B z1B z2B = sortXB xbB


-- |xbOccupy but along one dimension. Testing if the first occupies the second.
occupyFatly :: (Double, Double) -> (Double, Double) -> Bool
occupyFatly (xMin,xMax) (xMin',xMax') = (xMin < xMid') && (xMax >= xMid')
    where
        xMid' = (xMin'+xMax')/2

-- |xbOccupy but along one dimension. Testing if the first occupies the second.
occupyThinly :: (Double, Double) -> (Double, Double) -> Bool
occupyThinly (xMin,xMax) (xMin',xMax') = ((xMin >= xMin') && (xMin <= xMax'))
    || ((xMax >= xMin') && (xMax <= xMax'))

getCellXB :: FDSFile -> (Int, (Int, Int, Int)) -> XB
getCellXB fdsData cell@(meshNum,(i,j,k)) = (XB x1 x2 y1 y2 z1 z2)
    where
        meshes = fdsFile_Meshes fdsData
        mesh = meshes !! (meshNum - 1)
        (xs, ys, zs) = getMeshLines fdsData mesh
        x1 = xs !! i
        x2 = xs !! (i+1)
        y1 = ys !! j
        y2 = ys !! (j+1)
        z1 = zs !! k
        z2 = zs !! (k+1)

getMinDim :: FDSFile -> (Int, (Int, Int, Int)) -> Double
getMinDim fdsData cell@(meshNum,(i,j,k)) = minimum [delX, delY, delZ]
    where
        delX = x2-x1
        delY = y2-y1
        delZ = z2-z1
        meshes = fdsFile_Meshes fdsData
        mesh = meshes !! (meshNum - 1)
        (xs, ys, zs) = getMeshLines fdsData mesh
        x1 = xs !! i
        x2 = xs !! (i+1)
        y1 = ys !! j
        y2 = ys !! (j+1)
        z1 = zs !! k
        z2 = zs !! (k+1)

isInMesh :: XYZ -> Mesh -> Bool
isInMesh point@(XYZ x y z) mesh = and
    [ (x >= xmin) && (x <= xmax)
    , (y >= ymin) && (y <= ymax)
    , (z >= zmin) && (z <= zmax)
    ]
    where
        XB x1 x2 y1 y2 z1 z2 = getXB mesh

        xmin = min x1 x2
        ymin = min y1 y2
        zmin = min z1 z2

        xmax = max x1 x2
        ymax = max y1 y2
        zmax = max z1 z2

getMeshIJK :: Mesh -> (Int, Int, Int)
getMeshIJK mesh =
    let
        ijk = mesh_IJK mesh
        i = ijk_i ijk
        j = ijk_j ijk
        k = ijk_k ijk
    in (i, j, k)

-- |Uniform meshes can be determined using simply the MESH namelist. For
-- non-uniform meshes we need to use the TRN entries.
getMeshLines :: FDSFile -> Mesh -> ([Double], [Double], [Double])
getMeshLines fdsData mesh = (xs, ys, zs)
    where
        (i,j,k) = getMeshIJK mesh
        XB x1 x2 y1 y2 z1 z2 = getXB mesh

        xmin = min x1 x2
        ymin = min y1 y2
        zmin = min z1 z2

        xmax = max x1 x2
        ymax = max y1 y2
        zmax = max z1 z2

        delX = (xmax - xmin)/(fromIntegral i)
        delY = (ymax - ymin)/(fromIntegral j)
        delZ = (zmax - zmin)/(fromIntegral k)

        xs = case trnx of
            Nothing -> map (\n->xmin + (fromIntegral n)*(delX)) [0..i]
            Just x -> trnBased
        ys = case trny of
            Nothing -> map (\n->ymin + (fromIntegral n)*(delY)) [0..j]
            Just x -> trnBased
        zs = case trnz of
            Nothing -> map (\n->zmin + (fromIntegral n)*(delZ)) [0..k]
            Just x -> trnBased

        trnBased = undefined

        Just meshIndex = findIndex (== mesh) (fdsFile_Meshes fdsData)

        -- Get the relevant TRNs
        trnx = case filter
                (\p-> (trnx_MESH_NUMBER p) == (meshIndex+1))
                (fdsFile_Trnxs fdsData) of
            [] -> Nothing
            [x] -> Just x
            _ -> error "multiple TRNs found"
        trny = case filter
                (\p-> (trny_MESH_NUMBER p) == (meshIndex+1))
                (fdsFile_Trnys fdsData) of
            [] -> Nothing
            [x] -> Just x
            _ -> error "multiple TRNs found"
        trnz = case filter
            (\p-> (trnz_MESH_NUMBER p) == (meshIndex+1))
                (fdsFile_Trnzs fdsData) of
            [] -> Nothing
            [x] -> Just x
            _ -> error "multiple TRNs found"

getMeshSkew :: Mesh -> Double
getMeshSkew nml =
    let (dx,dy,dz) = getMeshResolution nml
    in (maximum [dx,dy,dz] / minimum [dx,dy,dz])
    where
        (i,j,k) = getMeshIJK nml

getMeshResolution :: Mesh -> (Double, Double, Double)
getMeshResolution nml =
    (x/(fromIntegral i),y/(fromIntegral j),z/(fromIntegral k))
    where
        (i,j,k) = getMeshIJK nml
        (x,y,z) = getDimensions nml

getDimensions :: HasXB a => a -> (Double, Double, Double)
getDimensions nml = (x,y,z)
    where
        XB x1 x2 y1 y2 z1 z2 = getXB nml
        x = x2 - x1
        y = y2 - y1
        z = z2 - z1

getBurnerResolution :: FDSFile -> Burner -> [(Double, Double, Double)]
getBurnerResolution fdsData (BurnerObst obst) = getResolutions fdsData obst
getBurnerResolution fdsData (BurnerVent vent) = getResolutions fdsData vent

getResolutions :: HasXB a => FDSFile -> a -> [(Double, Double, Double)]
getResolutions fdsData obst =
    map getMeshResolution $ findInMesh fdsData obst

getSmallestResolution :: FDSFile -> (Double, Double, Double)
getSmallestResolution fdsData = smallest
    where
        resolutions = map getMeshResolution (fdsFile_Meshes fdsData)
        smallest = minimumBy compareResolutions resolutions

getOrderedResolutions :: FDSFile -> [(Double, Double, Double)]
getOrderedResolutions fdsData = smallest
    where
        resolutions = map getMeshResolution (fdsFile_Meshes fdsData)
        smallest = sortBy compareResolutions resolutions

compareResolutions
    :: (Double, Double, Double)
    -> (Double, Double, Double)
    -> Ordering
compareResolutions (x1,y1,z1) (x2,y2,z2) = compare (x1*y1*z1) (x2*y2*z2)

findInMesh :: HasXB a => FDSFile -> a -> [Mesh]
findInMesh fdsData obst = filter (isOverlap obst) (fdsFile_Meshes fdsData)

-- |Test two Namelists with XBs and determine if they intersect in 3d.
nmlIntersect :: (HasXB a, HasXB b) => a -> b -> Bool
nmlIntersect nmlA nmlB = xbIntersect xbA xbB
    where
        xbA = getXB nmlA
        xbB = getXB nmlB

-- |Test if two XBs intersect (i.e. their bounding boxes). Two bounding boxes
-- intersect of all 3 dimensions have overlap. EQ is considered overlap.
xbIntersect :: XB -> XB -> Bool
xbIntersect xbA xbB = intersectX && intersectY && intersectZ
    where
        intersectX = x2A > x1B && x2B > x1A
        intersectY = y2A > y1B && y2B > y1A
        intersectZ = z2A > z1B && z2B > z1A

        XB x1A x2A y1A y2A z1A z2A = sortXB xbA
        XB x1B x2B y1B y2B z1B z2B  = sortXB xbB

-- | Sort an XB such that x2>x1 y2>y1 and z2>z1.
sortXB :: XB -> XB
sortXB (XB x1' x2' y1' y2' z1' z2') = (XB x1 x2 y1 y2 z1 z2)
    where
        x1 = min x1' x2'
        x2 = max x1' x2'
        y1 = min y1' y2'
        y2 = max y1' y2'
        z1 = min z1' z2'
        z2 = max z1' z2'

-- |Test if the XBs of the two namelists overlap
isOverlap :: (HasXB a1, HasXB a2) => a2 -> a1 -> Bool
isOverlap nmlA nmlB = not $ any id [cond1, cond2, cond3, cond4, cond5, cond6]
    where
        cond1 = x2A < x1B
        cond2 = x1A > x2B
        cond3 = y2A < y1B
        cond4 = y1A > y2B
        cond5 = z2A < z1B
        cond6 = z1A > z2B
        XB x1A x2A y1A y2A z1A z2A = getXB nmlA --peform min/max
        XB x1B x2B y1B y2B z1B z2B = getXB nmlB

getSprinklers :: FDSFile -> [Devc]
getSprinklers fdsData = filter (isSprinkler fdsData)
    $ fdsFile_Devcs fdsData

isSprinkler :: FDSFile -> Devc -> Bool
isSprinkler fdsData nml =
    case devc_PROP_ID nml of
        Nothing -> False
        Just propId -> propId `elem` (getThermalDetectorPropIds fdsData)

getSprinklerPropIds :: FDSFile -> [String]
getSprinklerPropIds fdsData =
    catMaybes $ fmap getId (getSprinklerProps fdsData)

getSprinklerProps :: FDSFile -> [Prop]
getSprinklerProps fdsData =
        filter isSprinklerProp
        $ fdsFile_Props fdsData

isSprinklerProp :: Prop -> Bool
isSprinklerProp nml = prop_QUANTITY nml == Just "SPRINKLER LINK TEMPERATURE"

sprinklerActivationTemperature :: FDSFile -> Devc -> Maybe Double
sprinklerActivationTemperature fdsData nml = do
    propId <- devc_PROP_ID nml
    let prop = getProp fdsData propId
    pure $ prop_ACTIVATION_TEMPERATURE prop

getProp :: FDSFile -> String -> Prop
getProp fdsData propId =
    let [prop] = filter (\p-> (getId p) == Just propId) (fdsFile_Props fdsData)
    in prop

getNDRs :: FDSFile -> [Double]
getNDRs fdsData = map (ndr fdsData) burners
    where
        burners = getBurners fdsData

ndr :: FDSFile -> Burner -> Double
ndr fdsData burner = ndr
    where
        q = getBurnerMaxHRR fdsData burner
        resolutions = getBurnerResolution fdsData burner
        nominalCellSizes = map getMaxCellSize resolutions
        nominalCellSize = maximum nominalCellSizes
        getMaxCellSize (x,y,z) = maximum [x,y,z]
        charFireDiameter = (q / (ambientDensity*ambientSpecificHeat
            *ambientTemperature*(sqrt g)))**(2/5)
        ndr = charFireDiameter/nominalCellSize
        calcs =
            [ "Min. Mesh Resolution: " ++ show nominalCellSize ++ " m"
            , "Non-Dimensionalised Ratio: " ++ show ndr
            ]
        ambientDensity = 1.205
        ambientSpecificHeat = 1.005
        ambientTemperature = 293.15
        g = 9.81

smokeDetectorObscuration :: FDSFile -> Devc -> Maybe Double
smokeDetectorObscuration fdsData nml = do
    propId <- devc_PROP_ID nml
    let prop = getProp fdsData propId
        obs = prop_ACTIVATION_OBSCURATION prop
    pure obs
    -- return $  case getParameterMaybe prop "ACTIVATION_OBSCURATION" of
    --     Nothing -> 3.24 -- FDS Default
    --     Just (ParDouble x) -> x

getSootProductionRate :: FDSFile -> Double
getSootProductionRate fdsData = y_s/hoc*hrr
    where
        [reac] = fdsFile_Reacs fdsData
        y_s = reac_SOOT_YIELD reac
        -- y_s = case getParameterMaybe reac "SOOT_YIELD" of
        --     Just (ParDouble x) -> x
        hoc = getHoC fdsData
        hrr = getTotalMaxHRR fdsData

-- To calculate the heat of combustion, deteremine the oxygen consumption
-- then multiply by EPUMO2
-- TODO: include the other methods of specifying HoC.
-- Investigate the minor difference between this and the .out file value
getHoC :: FDSFile -> Double
getHoC fdsData = v_o2*w_o2*epumo2/(v_F*w_F)
    where
        [reac] = fdsFile_Reacs fdsData

        y_s = reac_SOOT_YIELD reac
        -- y_s = case getParameterMaybe reac "SOOT_YIELD" of
        --     Just (ParDouble x) -> x
        y_co = reac_CO_YIELD reac
        -- y_co = case getParameterMaybe reac "CO_YIELD" of
        --     Just (ParDouble x) -> x
        soot_h_fraction = reac_SOOT_H_FRACTION reac
        -- soot_h_fraction = case getParameterMaybe reac "SOOT_H_FRACTION" of
        --     Just (ParDouble x) -> x
        --     Nothing -> 0 -- TODO: abstract defaults to a separate table
        epumo2 = reac_EPUMO2 reac
        -- epumo2 = case reac "EMPUMO2" of
        --     Just (ParDouble x) -> x
        --     Nothing -> 13100

        -- for fuel molecule CxHyOzNv
        x = reac_C reac
        y = reac_H reac
        z = reac_O reac
        v = reac_N reac

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

getSmokeDetectors :: FDSFile -> [Devc]
getSmokeDetectors fdsData =
    filter (isSmokeDetector fdsData) $ fdsFile_Devcs fdsData

isSmokeDetector :: FDSFile -> Devc -> Bool
isSmokeDetector fdsData nml =
    case devc_PROP_ID nml of
        Nothing -> False
        Just propId -> propId `elem` (getSmokeDetectorPropIds fdsData)

getSmokeDetectorPropIds :: FDSFile -> [String]
getSmokeDetectorPropIds fdsData = catMaybes $ fmap getId $
    filter (isSmokeDetectorProp fdsData)
    $ fdsFile_Props fdsData

isSmokeDetectorProp :: FDSFile -> Prop -> Bool
isSmokeDetectorProp fdsData nml = prop_QUANTITY nml == Just "CHAMBER OBSCURATION"

getThermalDetectors :: FDSFile -> [Devc]
getThermalDetectors fdsData =
    filter (isThermalDetector fdsData)
    $ fdsFile_Devcs fdsData

isThermalDetector :: FDSFile -> Devc -> Bool
isThermalDetector fdsData nml =
    case devc_PROP_ID nml of
        Nothing -> False
        Just propId -> propId `elem` (getThermalDetectorPropIds fdsData)

getThermalDetectorPropIds :: FDSFile -> [String]
getThermalDetectorPropIds fdsData =
    catMaybes $ fmap getId (getThermalDetectorProps fdsData)

getThermalDetectorProps :: FDSFile -> [Prop]
getThermalDetectorProps fdsData =
        filter (isThermalDetectorProp fdsData)
        $ fdsFile_Props fdsData

isThermalDetectorProp :: FDSFile -> Prop -> Bool
isThermalDetectorProp fdsData nml = prop_QUANTITY nml == Just "LINK TEMPERATURE"

ventHasFlow :: FDSFile -> Vent -> Bool
ventHasFlow fdsData vent =
    let
        hvacs = fdsFile_Hvacs fdsData
        linkedHVACs :: [Hvac]
        linkedHVACs = filter (isLinkedToVent vent) hvacs
        isHVAC :: Bool
        isHVAC = (not . null) linkedHVACs
    in isHVAC || any surfHasFlow (getSurfList fdsData vent)
    where
        isLinkedToVent vent hvac =
            (maybe False id ((==) <$> hvac_VENT_ID hvac <*> vent_ID vent))
            || (maybe False id ((==) <$> hvac_VENT2_ID hvac <*> vent_ID vent))

obstHasFlow :: FDSFile -> Vent -> Bool
obstHasFlow fdsData flowable =
    (not . null) (surfHasFlow <$> getSurfList fdsData flowable)

surfHasFlow :: Surf -> Bool
surfHasFlow surf = or
    [ isJust $ surf_MLRPUA surf
    , isJust $ surf_MASS_FLUX surf
    , isJust $ surf_MASS_FLUX_TOTAL surf
    , isJust $ surf_MASS_FLUX_VAR surf
    , isJust $ surf_HRRPUA surf
    , isJust $ surf_VEL surf
    , isJust $ surf_VEL_T surf
    , isJust $ surf_VOLUME_FLOW surf
    ]

getFlowRate :: (HasXB a, HasSurf a) => FDSFile -> a -> Double
getFlowRate fdsData burner =
    let (xMin, xMax, yMin, yMax, zMin, zMax) =
            getFlowRates fdsData burner
    in sum [xMin, xMax, yMin, yMax, zMin, zMax]

getFlowRates :: (HasXB a, HasSurf a) => FDSFile -> a
    -> (Double, Double, Double, Double, Double, Double)
getFlowRates fdsData nml =
    -- TODO: this doesn't handle VENT namelists properly
    let (xMinSurf, xMaxSurf, yMinSurf, yMaxSurf, zMinSurf, zMaxSurf) =
            getSixSurfs fdsData nml
        (xMinArea, xMaxArea, yMinArea, yMaxArea, zMinArea, zMaxArea) =
            getSurfAreas fdsData nml
    in
        ( getFaceFlowRate fdsData xMinSurf xMinArea
        , getFaceFlowRate fdsData xMaxSurf xMaxArea
        , getFaceFlowRate fdsData yMinSurf yMinArea
        , getFaceFlowRate fdsData yMaxSurf yMaxArea
        , getFaceFlowRate fdsData zMinSurf zMinArea
        , getFaceFlowRate fdsData zMaxSurf zMaxArea
        )

-- getFaceHRR :: HasSurf a => FDSFile a -> HRR
getFaceFlowRate :: FDSFile -> Surf -> Double -> Double
getFaceFlowRate fdsData surf area
    | not (isSupplySurf surf || isExhaustSurf surf) = 0
    | otherwise = case surf_VEL surf of
        Just vel -> vel*area
        Nothing -> case surf_VOLUME_FLOW surf of
            Nothing -> 0
            Just volFlow -> volFlow

getTotalMaxHRR :: FDSFile -> Double
getTotalMaxHRR fdsData =
    sum $ map (getBurnerMaxHRR fdsData) (getBurners fdsData)

getBurners :: FDSFile -> [Burner]
getBurners fdsData =
    (fmap BurnerObst $ filter (isBurner fdsData) $ fdsFile_Obsts fdsData)
    <> (fmap BurnerVent $ filter (isBurner fdsData) $ fdsFile_Vents fdsData)

getExhausts :: FDSFile -> [Mech]
getExhausts fdsData =
    (fmap MechObst $ filter (isExhaust fdsData) $ fdsFile_Obsts fdsData)
    <> (fmap MechVent $ filter (isExhaust fdsData) $ fdsFile_Vents fdsData)

getSupplies :: FDSFile -> [Mech]
getSupplies fdsData =
    (fmap MechObst $ filter (isSupply fdsData) $ fdsFile_Obsts fdsData)
    <> (fmap MechVent $ filter (isSupply fdsData) $ fdsFile_Vents fdsData)

-- |Determines which sides of an obstruction are burner surfaces returns Bools
-- in the form (xMin,xMax,yMin,yMax,zMin,zMax)
getBurnerSideStatus :: HasSurf a => FDSFile -> a
    -> (Bool, Bool, Bool, Bool, Bool, Bool)
getBurnerSideStatus fdsData nml =
    (xMinStatus, xMaxStatus,yMinStatus,yMaxStatus,zMinStatus,zMaxStatus)
    where
    (xMinSurf,xMaxSurf,yMinSurf,yMaxSurf,zMinSurf,zMaxSurf) =
        getSixSurfs fdsData nml
    xMinStatus = isBurnerSurf xMinSurf
    xMaxStatus = isBurnerSurf xMaxSurf
    yMinStatus = isBurnerSurf yMinSurf
    yMaxStatus = isBurnerSurf yMaxSurf
    zMinStatus = isBurnerSurf zMinSurf
    zMaxStatus = isBurnerSurf zMaxSurf

getSixSurfs :: HasSurf a => FDSFile -> a
    -> (Surf, Surf, Surf, Surf, Surf, Surf)
getSixSurfs fdsData nml
    -- TODO: this approach to vents are not good
    | getNMLName nml == "VENT" =
        let inertSurf = find "INERT"
        in case getSurfs fdsData nml of
            SingleSurf surf ->
                (inertSurf, inertSurf, inertSurf, inertSurf, inertSurf, surf)
            TripleSurf topSurf sideSurf bottomSurf ->
                (sideSurf, sideSurf, sideSurf, sideSurf, bottomSurf, topSurf)
            SixSurf surf1 surf2 surf3 surf4 surf5 surf6 ->
                (surf1, surf2, surf3, surf4, surf5, surf6)
    | getNMLName nml == "OBST" = case getSurfs fdsData nml of
        SingleSurf surf -> (surf, surf, surf, surf, surf, surf)
        TripleSurf topSurf sideSurf bottomSurf ->
            (sideSurf, sideSurf, sideSurf, sideSurf, bottomSurf, topSurf)
        SixSurf surf1 surf2 surf3 surf4 surf5 surf6 ->
            (surf1, surf2, surf3, surf4, surf5, surf6)
    | otherwise = error "this object does not have surfaces"
    where
        surfs = fdsFile_Surfs fdsData
        find surfId =
            let matches = filter (\surf->(getId surf) == (Just surfId)) surfs
            in case matches of
                [s] -> s
                [] -> error $ "No matching surface for: " <> show surfId
                x -> error ("Multiple matching surfs: " <> show x)

isBurner :: HasSurf a => FDSFile -> a -> Bool
isBurner fdsData nml =
    let surfs = getSurfList fdsData nml
    in any isBurnerSurf surfs

isExhaust :: HasSurf a => FDSFile -> a -> Bool
isExhaust fdsData nml = any isExhaustSurf (getSurfList fdsData nml)

isSupply :: HasSurf a => FDSFile -> a -> Bool
isSupply fdsData nml = any isSupplySurf (getSurfList fdsData nml)

sourceFroude :: Double -> Double -> Double
sourceFroude q fuelArea =
    q/(ambientDensity*ambientSpecificHeat*
        ambientTemperature*(fuelDiameter**2)*(sqrt (g*fuelDiameter)))
    where
        ambientDensity = 1.205
        ambientSpecificHeat = 1.005
        ambientTemperature = 293.15
        g = 9.81
        fuelDiameter = sqrt ((4*fuelArea)/pi)

checkGrowthRate :: FDSFile -> Burner
    -> Either Double (GrowthRate, Double)
checkGrowthRate fdsData burner = evalGrowthRate hrrAlpha
    where
        (hrrCap, hrrAlpha) = burnerGrowthRate fdsData burner

burnerGrowthRate :: FDSFile -> Burner -> (Double, Double)
burnerGrowthRate fdsData burner = case hrr of
    HRRTauQ hrrpua tau_q ->
        let
            hrrCap = hrrpua * (burnerArea fdsData burner)
            hrrAlpha = hrrCap / ((abs tau_q)**2)
        in (hrrCap, hrrAlpha)
    HRRRamp _ -> error "not tauq"
    HRRStatic _ -> error "not tauq"
    HRRCombined _ -> error "not tauq"
    NoHRR -> error "not tauq"
    where
        hrr = getHRR fdsData burner

getBurnerMaxHRR :: FDSFile -> Burner -> Double
getBurnerMaxHRR fdsData (BurnerObst obst) = getMaxHRR fdsData obst
getBurnerMaxHRR fdsData (BurnerVent vent) = getMaxHRR fdsData vent

getMaxHRR :: (HasXB a, HasSurf a) => FDSFile -> a -> Double
getMaxHRR fdsData burner = maxHRR $ getHRR fdsData burner

getHRR :: (HasXB a, HasSurf a) => FDSFile -> a -> HRR
getHRR fdsData burner =
    let (xMinHRR, xMaxHRR, yMinHRR, yMaxHRR, zMinHRR, zMaxHRR) =
            getHRRs fdsData burner
    in mconcat [xMinHRR, xMaxHRR, yMinHRR, yMaxHRR, zMinHRR, zMaxHRR]

getHRRs :: (HasXB a, HasSurf a) => FDSFile -> a
    -> (HRR, HRR, HRR, HRR, HRR, HRR)
getHRRs fdsData nml =
    let (xMinSurf, xMaxSurf, yMinSurf, yMaxSurf, zMinSurf, zMaxSurf) =
            getSixSurfs fdsData nml
        (xMinArea, xMaxArea, yMinArea, yMaxArea, zMinArea, zMaxArea) =
            getSurfAreas fdsData nml
    in
        ( getFaceHRR fdsData xMinSurf xMinArea
        , getFaceHRR fdsData xMaxSurf xMaxArea
        , getFaceHRR fdsData yMinSurf yMinArea
        , getFaceHRR fdsData yMaxSurf yMaxArea
        , getFaceHRR fdsData zMinSurf zMinArea
        , getFaceHRR fdsData zMaxSurf zMaxArea
        )

getFaceHRR :: FDSFile -> Surf -> Double -> HRR
getFaceHRR fdsData surf area
    | not (isBurnerSurf surf) = NoHRR
    | otherwise =
        let Just nominalHRRPUA = surf_HRRPUA surf
            tauq = surf_TAU_Q surf
            rampq = surf_RAMP_Q surf
        in case rampq of
            -- (Nothing, Nothing) -> HRRStatic (nominalHRRPUA*area)
            Just r -> HRRRamp
                $ map (\(t,f)->(t,nominalHRRPUA*area*f))
                $ fromMaybe (error "No ramp data")
                $ getRampData <$> getRamp fdsData r
            Nothing -> HRRTauQ (nominalHRRPUA*area) tauq

data HRR
    = HRRTauQ Double Double
    | HRRRamp [(Double, Double)]
    | HRRStatic Double
    | HRRCombined [HRR]
    | NoHRR
    deriving (Show, Eq)

instance Semigroup HRR where
    (HRRCombined as) <> (HRRCombined bs) = HRRCombined (as <> bs)
    (HRRCombined as) <> b = HRRCombined (as <> [b])
    a <> (HRRCombined bs) = HRRCombined ([a] <> bs)
    NoHRR <> b = b
    a <> NoHRR = a
    a <> b = HRRCombined [a,b]

instance Monoid HRR where
    mempty = NoHRR

maxHRR :: HRR -> Double
maxHRR (HRRTauQ m t) = m
maxHRR (HRRRamp es) = maximum $ snd $ unzip es
maxHRR (HRRStatic hrr) = hrr
maxHRR (HRRCombined []) = 0
maxHRR (HRRCombined [x]) = maxHRR x
maxHRR (HRRCombined xs) = maxHRR (mconcat xs)
maxHRR NoHRR = 0

getRampData :: Ramp -> [(Double, Double)]
getRampData ramp =
    fmap (\re->(rampEntry_T re, rampEntry_F re)) (ramp_entries ramp)

getRamp :: FDSFile -> String -> Maybe Ramp
getRamp fdsData rampId =
    let ramps = fdsFile_Ramps fdsData
        matches = filter (\r->getId r == Just rampId) ramps
        -- mkVal nml =
        --     let t = getDouble $ getParameter nml "T"
        --         f = getDouble $ getParameter nml "F"
        --     in (t,f)
        -- vals = map mkVal matchingRampEntries
    in case matches of
        [] -> Nothing
        [x] -> Just x
        _ -> error "RAMP: too many matches"

isBurnerSurf :: Surf -> Bool
isBurnerSurf = isJust . surf_HRRPUA

isExhaustSurf :: Surf -> Bool
isExhaustSurf surf =
    let
        vel = surf_VEL surf
        volFlux = Nothing -- surf_VOLUME_FLUX surf
        volFlow = surf_VOLUME_FLOW surf
        flow = catMaybes [vel, volFlux, volFlow]
    in case flow of
        [] -> False
        [f] -> f > 0
        _ -> error "Error multiple flow parameters"

isSupplySurf :: Surf -> Bool
isSupplySurf surf =
    let
        vel = surf_VEL surf
        volFlux = Nothing -- surf_VOLUME_FLUX surf
        volFlow = surf_VOLUME_FLOW surf
        flow = catMaybes [vel, volFlux, volFlow]
    in case flow of
        [] -> False
        [f] -> f < 0
        _ -> error "Error multiple flow parameters"

-- |A burner can be any element that can have a surf.
getBurnerSurf :: HasSurf a => FDSFile -> a -> [Surf]
getBurnerSurf fdsData nml =
    let surfs = getSurfList fdsData nml
    in nub $ filter isBurnerSurf surfs

getSimTimes :: FDSFile -> (Double, Double)
getSimTimes fdsData = case fdsFile_Time fdsData of
    Nothing -> (0,1) -- Defaults
    Just time -> (time_T_BEGIN time, time_T_END time)

getCHID :: FDSFile -> Maybe String
getCHID fdsData = head_CHID =<< (fdsFile_Head fdsData)

-- TODO: this function does not account for multiple burner
-- surface types on the one obstruction, deprecate this function
burnerArea :: FDSFile -> Burner -> Double
burnerArea fdsData burner = sum
    [ xMinSurfArea
    , xMaxSurfArea
    , yMinSurfArea
    , yMaxSurfArea
    , zMinSurfArea
    , zMaxSurfArea
    ]
    where
        (delX, delY, delZ) = getDimensions burner
        xArea = delY*delZ
        yArea = delX*delZ
        zArea = delX*delY
        xMinSurfArea = if xMinStatus then xArea else 0
        xMaxSurfArea = if xMaxStatus then xArea else 0
        yMinSurfArea = if yMinStatus then yArea else 0
        yMaxSurfArea = if yMaxStatus then yArea else 0
        zMinSurfArea = if zMinStatus then zArea else 0
        zMaxSurfArea = if zMaxStatus then zArea else 0
        (xMinStatus, xMaxStatus,yMinStatus,yMaxStatus,zMinStatus,zMaxStatus) =
            getBurnerSideStatus fdsData burner

getSurfAreas :: (HasXB a, HasSurf a) => FDSFile -> a
    -> (Double, Double, Double, Double, Double, Double)
getSurfAreas fdsData nml =
    ( xMinSurfArea
    , xMaxSurfArea
    , yMinSurfArea
    , yMaxSurfArea
    , zMinSurfArea
    , zMaxSurfArea
    )
    where
        (delX, delY, delZ) = getDimensions nml
        xArea = delY*delZ
        yArea = delX*delZ
        zArea = delX*delY
        xMinSurfArea = xArea
        xMaxSurfArea = xArea
        yMinSurfArea = yArea
        yMaxSurfArea = yArea
        zMinSurfArea = zArea
        zMaxSurfArea = zArea

-- |Determine the axis of orientation of a plane in space.
-- planeOrientation (x1,x2,y1,y2,x1,x2) =

-- TODO: make sure to take the closest one
-- the time of measurement is at the cap tiem
evalGrowthRate :: Double -> Either Double (GrowthRate, Double)
evalGrowthRate alpha
    | absDiff < (0.001 :: Double) =
        Right (growthRate, (growthRateToAlpha growthRate) - alpha)
    | otherwise    = Left alpha
    where
        (absDiff, growthRate) = minimum $ map (growthRateDiff alpha) growthRates
        eps = 0.001 :: Double
        eq = aEq eps

growthRateDiff :: Double -> GrowthRate -> (Double, GrowthRate)
growthRateDiff alpha growthRate =
    (abs $ (growthRateToAlpha growthRate) - alpha, growthRate)

growthRates :: [GrowthRate]
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
aEq :: (Ord a, Num a) => a -> a -> a -> Bool
aEq eps a b = a < (b+eps) && a > (b-eps)

-- findNamelists (FDSFile comments namelists) nameTarget =
--     filter (\(Namelist name _ _ _) -> name == nameTarget) namelists
