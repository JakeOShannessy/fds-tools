{-# LANGUAGE OverloadedStrings #-}
module FDSUtilities.FDSFile.NamelistFunctions where

import qualified Data.Array as A
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Vector as V

import           FDSUtilities.FDSFile.Types
import           FDSUtilities.FDSFile.Utilities
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


-- getMeshes :: NamelistFile -> [Namelist]
-- getMeshes fdsData = findNamelists fdsData "MESH"

-- getDevices :: NamelistFile -> [Namelist]
-- getDevices fdsData = findNamelists fdsData "DEVC"

-- getTRNs :: NamelistFile -> [Namelist]
-- getTRNs fdsData = getTRNXs fdsData ++ getTRNYs fdsData ++ getTRNZs fdsData

-- getTRNXs :: NamelistFile -> [Namelist]
-- getTRNXs fdsData = (findNamelists fdsData "TRNX")

-- getTRNYs :: NamelistFile -> [Namelist]
-- getTRNYs fdsData = (findNamelists fdsData "TRNY")

-- getTRNZs :: NamelistFile -> [Namelist]
-- getTRNZs fdsData = (findNamelists fdsData "TRNZ")

-- getNCells nml@(Namelist "MESH" _ parameters _) = i*j*k
--     where
--         (i,j,k) = getMeshIJK nml

-- -- |Check if a device is stuck in a solid. Returns Nothing if it's not a
-- -- sensible question (e.g. it is not a point device).
-- stuckInSolid :: NamelistFile -> Namelist -> Maybe Bool
-- stuckInSolid fdsData devc = isCellSolid fdsData
--     <$> ((determineCell fdsData)
--     =<< (getXYZ devc))

-- -- |Check if the cell directly above a device is solid. This is useful to make
-- -- sure that sprinklers and smoke detectors are directly beneath the a ceiling.
-- --
-- -- TODO: This is more complicated as it may not be a solid cell, but a solid
-- -- surface. This is exacerbated by being on a mesh boundary.
-- beneathCeiling :: NamelistFile -> Namelist -> Maybe Bool
-- beneathCeiling fdsData devc = do
--     xyz <- getXYZ devc
--     cell <- determineCell fdsData xyz
--     pure $ isFaceSolid fdsData cell PosZ

-- -- |((MeshNum, IJK), FACEDIR)
-- type Face = ((Int, (Int, Int, Int)), Direction)

-- isSolidFace :: NamelistFile -> Face -> Bool
-- isSolidFace fdsData face@(cell, dir) =
--     let (a,b,c,d,e,f) = getCellFacesSolidness fdsData cell
--     in case dir of
--         NegX -> a
--         PosX -> b
--         NegY -> c
--         PosY -> d
--         NegZ -> e
--         PosZ -> f

-- getCellFacesSolidness :: NamelistFile -> (Int, (Int, Int, Int)) -> (Bool, Bool, Bool, Bool, Bool, Bool)
-- getCellFacesSolidness fdsData cell = if thisCell
--     -- If this cell is sold then all its face must be solid. This is the
--     -- simplest case.
--     then (True, True, True, True, True, True)
--     -- Otherwise we need to test the cells around it, however, we need to be
--     -- careful as we might need to step out of the particulary mesh.
--     --
--     -- We also need to account for thin obstructions. This means making a data
--     -- structure that shows the solidness of each face.
--     --
--     -- We also need to consider when the cell above is only partially covered.
--     else undefined
--     where
--         thisCell = isCellSolid fdsData cell

-- -- |Determine the cell in which a point lies. The output is (MeshNum, (I,J,K)).
-- determineCell :: NamelistFile -> (Double, Double, Double) -> Maybe (Int,(Int,Int,Int))
-- determineCell fdsData point = do
--     meshIndex <- determineMeshIndex fdsData point
--     let mesh = getMeshes fdsData !! (meshIndex - 1)
--     cell <- determineCellInMesh fdsData point mesh
--     pure (meshIndex, cell)

-- -- |Determine in which cell within a mesh a point lies. Return Nothing if the
-- -- point does not lie within the mesh.
-- determineCellInMesh :: NamelistFile -> (Double, Double, Double) -> Namelist -> Maybe (Int,Int,Int)
-- determineCellInMesh fdsData point@(x,y,z) mesh = do
--     iCell <- findPointInLine xs x
--     jCell <- findPointInLine ys y
--     kCell <- findPointInLine zs z
--     pure (iCell, jCell, kCell)
--     where
--         (xs, ys, zs) = getMeshLines fdsData mesh

-- moveInMesh :: NamelistFile -> (Int,(Int,Int,Int)) -> Direction -> Maybe [(Int,(Int,Int,Int))]
-- moveInMesh fdsData currentCell dir = undefined

-- findPointInLine :: [Double] -> Double -> Maybe Int
-- findPointInLine (x:xs) p = if (p < x) then Nothing
--     else findPointInLine' 0 xs p

-- findPointInLine' :: Int -> [Double] -> Double -> Maybe Int
-- findPointInLine' _ [] _ = Nothing
-- findPointInLine' index (x:xs) p = if p < x
--     then Just index
--     else findPointInLine' (index + 1) xs p

-- -- |Determine which mesh the point is in. Output is MeshNum. This assumes that
-- -- there are no overlapping meshes.
-- determineMesh :: NamelistFile -> (Double, Double, Double) -> Maybe Namelist
-- determineMesh fdsData point = find (isInMesh point) meshes
--         where
--             meshes = getMeshes fdsData :: [Namelist]

-- -- |Same as determineMesh but returns the index of the mesh rather than the mesh
-- -- itself.
-- determineMeshIndex :: NamelistFile -> (Double, Double, Double) -> Maybe Int
-- determineMeshIndex fdsData point = fmap (+1) $ findIndex (isInMesh point) meshes
--         where
--             meshes = getMeshes fdsData :: [Namelist]

-- -- |Determine if a face is an 'OPEN' vent at cell @cell@ and direction @dir@. NB:
-- -- This does not consider MB style mesh boundary specs
-- isFaceOpenVent :: NamelistFile -> (Int, (Int, Int, Int)) -> Direction -> Bool
-- isFaceOpenVent fdsData cell dir = case find (\obst->faceOccupy cellSize (getXB obst) faceXB) (filter (hasOpenSurf) $ obsts ++ vents) of
--     Nothing -> False
--     Just _ -> True
--     where
--         cellSize = getMinDim fdsData cell
--         faceXB = getFaceXB fdsData cell dir
--         obsts = findNamelists fdsData "OBST"
--         vents = findNamelists fdsData "VENT"

-- hasOpenSurf :: Namelist -> Bool
-- hasOpenSurf nml = case getSurfId nml of
--     Just "OPEN" -> True
--     _ -> False

-- -- |Get the solidness of a single face at cell @cell@ and direction @dir@. NB:
-- -- This does not consider neighbouring cells.
-- isFaceSolid :: NamelistFile -> (Int, (Int, Int, Int)) -> Direction -> Bool
-- isFaceSolid fdsData cell dir = case find (\obst->faceOccupy cellSize (getXB obst) faceXB) obstsAndVents of
--     Nothing ->
--         -- Face is an external mesh boundary
--         (isFaceExternalMeshBoundary fdsData cell PosZ)
--             -- Which is not covered by an 'OPEN' vent
--             && (not (isFaceOpenVent fdsData cell PosZ))
--     Just _ -> True
--     where
--         cellSize = getMinDim fdsData cell
--         faceXB = getFaceXB fdsData cell dir
--         -- Exclude 'OPEN' vents and obsts, as they are not solid
--         obstsAndVents = filter (not . hasOpenSurf) $ obsts ++ vents
--         obsts = findNamelists fdsData "OBST"
--         vents = findNamelists fdsData "VENT"

-- -- |Determine if a face is an external mesh boundary. I.e., it could be 'OPEN'.
-- isFaceExternalMeshBoundary :: NamelistFile -> (Int, (Int, Int, Int)) -> Direction -> Bool
-- isFaceExternalMeshBoundary fdsData cell@(meshNum, (i,j,k)) dir =
--     let
--         mesh = getMeshes fdsData !! (meshNum - 1)
--         -- First we need to determine if the cell is on the edge of the mesh (in the chosen direction)
--         -- | @cellN@ is the cell number in the chosen direction
--         cellN = case dir of
--             PosX -> i
--             NegX -> i
--             PosY -> j
--             NegY -> j
--             PosZ -> k
--             NegZ -> k
--         (meshMaxI, meshMaxJ, meshMaxK) =
--             -- These are lines not cells
--             let (xs, ys, zs) = getMeshLines fdsData mesh
--                 nCellsI = length xs - 1
--                 nCellsJ = length ys - 1
--                 nCellsK = length zs - 1
--             -- We need to subtract 1 to go from the quantity to the max index
--             in (nCellsI - 1, nCellsJ - 1, nCellsK - 1)
--         -- | @maxCellN@ is the boundary cell number of the mesh in the chosen direction
--         maxCellN = case dir of
--             PosX -> meshMaxI
--             NegX -> 0
--             PosY -> meshMaxJ
--             NegY -> 0
--             PosZ -> meshMaxK
--             NegZ -> 0
--         -- This determines if the cell is at the edge of the mesh in the chosen direction.
--         cellIsMeshBoundary = cellN == maxCellN
--         -- TODO: how do we determine if the cell is external?
--         --
--         -- Next we need to determine if there is another mesh on the other side of this boundary.
--         -- I.e., determine whether it is external.
--         -- To do this we will take the midpoint of the face, then go "up" a small amount in the
--         -- direction of the normal axis. We will then check if this point lies within another mesh.
--         -- This is not a great way to do this, but it will suffice for now, until we have better
--         -- data structures in place.
--         -- TODO: improve this.
--         faceMidPoint =
--             let (XB x1 x2 y1 y2 z1 z2) = getFaceXB fdsData cell dir
--             in case dir of
--                 PosX -> midpointXB (XB x2 x2 y1 y2 z1 z2)
--                 NegX -> midpointXB (XB x1 x1 y1 y2 z1 z2)
--                 PosY -> midpointXB (XB x1 x2 y2 y2 z1 z2)
--                 NegY -> midpointXB (XB x1 x2 y1 y1 z1 z2)
--                 PosZ -> midpointXB (XB x1 x2 y1 y2 z2 z2)
--                 NegZ -> midpointXB (XB x1 x2 y1 y2 z1 z1)
--         faceMidPointPlus = case dir of
--             PosX -> faceMidPoint `addXYZ` (XYZ   0.000001    0           0        )
--             NegX -> faceMidPoint `addXYZ` (XYZ (-0.000001)   0           0        )
--             PosY -> faceMidPoint `addXYZ` (XYZ   0           0.000001    0        )
--             NegY -> faceMidPoint `addXYZ` (XYZ   0         (-0.000001)   0        )
--             PosZ -> faceMidPoint `addXYZ` (XYZ   0           0           0.000001 )
--             NegZ -> faceMidPoint `addXYZ` (XYZ   0           0         (-0.000001))

--     in and
--         [ cellIsMeshBoundary
--         -- check if the point just over the bounday is within any mesh
--         , not $ any (\mesh->isInMeshXYZ mesh faceMidPointPlus) (getMeshes fdsData)
--         ]

-- addXYZ :: XYZ -> XYZ -> XYZ
-- addXYZ (XYZ x1 y1 z1) (XYZ x2 y2 z2) = (XYZ (x1+x2) (y1+y2) (z1+z2))

-- midpointXB :: XB -> XYZ
-- midpointXB (XB x1 x2 y1 y2 z1 z2) = XYZ ((x1+x2)/2) ((y1+y2)/2) ((z1+z2)/2)

-- getFaceXB :: NamelistFile -> (Int, (Int, Int, Int)) -> Direction -> XB
-- getFaceXB fdsData cell dir =
--     let cellXB = FDSUtilities.FDSFileFunctions.getCellXB fdsData cell
--         XB x1A x2A y1A y2A z1A z2A = sortXB cellXB
--     in case dir of
--         NegX -> XB x1A x1A y1A y2A z1A z2A
--         PosX -> XB x2A x2A y1A y2A z1A z2A
--         NegY -> XB x1A x2A y1A y1A z1A z2A
--         PosY -> XB x1A x2A y2A y2A z1A z2A
--         NegZ -> XB x1A x2A y1A y2A z1A z1A
--         PosZ -> XB x1A x2A y1A y2A z2A z2A

-- -- |Use the OBST namelists to determine if a particular cell is solid or not.
-- -- TODO: only considers basic OBSTs and not MULT or the like.
-- isCellSolid :: NamelistFile -> (Int, (Int, Int, Int)) -> Bool
-- isCellSolid fdsData cell@(meshNum,(i,j,k)) = case find (\obst->xbOccupy (getXB obst) cellXB) obsts of
--     Nothing -> False
--     Just _ -> True
--     where
--         cellXB = FDSUtilities.FDSFileFunctions.getCellXB fdsData cell
--         -- If any obst overlaps with this cell, then it's solid
--         obsts = findNamelists fdsData "OBST"

-- -- |Determine if if the first XB occupies more than or equal to 50% of the
-- -- second XB in all dimensions. This is used to determine if an obstruction
-- -- (first XB) causes a cell (second XB) to be solid or not.
-- xbOccupy :: XB -> XB -> Bool
-- xbOccupy xbA xbB = occupyX && occupyY && occupyZ
--     where
--         occupyX = occupyFatly (x1A,x2A) (x1B,x2B)
--         occupyY = occupyFatly (y1A,y2A) (y1B,y2B)
--         occupyZ = occupyFatly (z1A,z2A) (z1B,z2B)

--         XB x1A x2A y1A y2A z1A z2A = sortXB xbA
--         XB x1B x2B y1B y2B z1B z2B = sortXB xbB

-- -- |This is a lower requirement than xbOccupy. All xbOccupy satisfies this as
-- -- well.
-- faceOccupy :: Double -> XB -> XB -> Bool
-- faceOccupy cellSize xbA xbB@(XB xMin xMax yMin yMax zMin zMax) = case (xSame, ySame, zSame) of
--     (True, False, False) -> faceOccupyX cellSize xbA xbB
--     (False, True, False) -> faceOccupyY cellSize xbA xbB
--     (False, False, True) -> faceOccupyZ cellSize xbA xbB
--     _ -> error "Not a face"
--     where
--         xSame = xMin == xMax
--         ySame = yMin == yMax
--         zSame = zMin == zMax

-- faceOccupyX :: Double -> XB -> XB -> Bool
-- faceOccupyX cellSize xbA xbB = occupyX && occupyY && occupyZ
--     where
--         occupyX = occupyThinly (x1A,x2A) (x1B-(cellSize/2),x2B+(cellSize/2))
--         occupyY = occupyFatly (y1A,y2A) (y1B,y2B)
--         occupyZ = occupyFatly (z1A,z2A) (z1B,z2B)

--         XB x1A x2A y1A y2A z1A z2A = sortXB xbA
--         XB x1B x2B y1B y2B z1B z2B = sortXB xbB

-- faceOccupyY :: Double -> XB -> XB -> Bool
-- faceOccupyY cellSize xbA xbB = occupyX && occupyY && occupyZ
--     where
--         occupyX = occupyFatly (x1A,x2A) (x1B,x2B)
--         occupyY = occupyThinly (y1A,y2A) (y1B-(cellSize/2),y2B+(cellSize/2))
--         occupyZ = occupyFatly (z1A,z2A) (z1B,z2B)

--         XB x1A x2A y1A y2A z1A z2A = sortXB xbA
--         XB x1B x2B y1B y2B z1B z2B = sortXB xbB

-- faceOccupyZ :: Double -> XB -> XB -> Bool
-- faceOccupyZ cellSize xbA xbB = occupyX && occupyY && occupyZ
--     where
--         occupyX = occupyFatly (x1A,x2A) (x1B,x2B)
--         occupyY = occupyFatly (y1A,y2A) (y1B,y2B)
--         occupyZ = occupyThinly (z1A,z2A) (z1B-(cellSize/2),z2B+(cellSize/2))

--         XB x1A x2A y1A y2A z1A z2A = sortXB xbA
--         XB x1B x2B y1B y2B z1B z2B = sortXB xbB


-- -- |xbOccupy but along one dimension. Testing if the first occupies the second.
-- occupyFatly :: (Double, Double) -> (Double, Double) -> Bool
-- occupyFatly (xMin,xMax) (xMin',xMax') = (xMin < xMid') && (xMax >= xMid')
--     where
--         xMid' = (xMin'+xMax')/2

-- -- |xbOccupy but along one dimension. Testing if the first occupies the second.
-- occupyThinly :: (Double, Double) -> (Double, Double) -> Bool
-- occupyThinly (xMin,xMax) (xMin',xMax') = ((xMin >= xMin') && (xMin <= xMax'))
--     || ((xMax >= xMin') && (xMax <= xMax'))

-- getCellXB :: NamelistFile -> (Int, (Int, Int, Int)) -> XB
-- getCellXB fdsData cell@(meshNum,(i,j,k)) = (XB x1 x2 y1 y2 z1 z2)
--     where
--         meshes = getMeshes fdsData
--         mesh = meshes !! (meshNum - 1)
--         (xs, ys, zs) = getMeshLines fdsData mesh
--         x1 = xs !! i
--         x2 = xs !! (i+1)
--         y1 = ys !! j
--         y2 = ys !! (j+1)
--         z1 = zs !! k
--         z2 = zs !! (k+1)

-- getMinDim :: NamelistFile -> (Int, (Int, Int, Int)) -> Double
-- getMinDim fdsData cell@(meshNum,(i,j,k)) = minimum [delX, delY, delZ]
--     where
--         delX = x2-x1
--         delY = y2-y1
--         delZ = z2-z1
--         meshes = getMeshes fdsData
--         mesh = meshes !! (meshNum - 1)
--         (xs, ys, zs) = getMeshLines fdsData mesh
--         x1 = xs !! i
--         x2 = xs !! (i+1)
--         y1 = ys !! j
--         y2 = ys !! (j+1)
--         z1 = zs !! k
--         z2 = zs !! (k+1)

-- isInMeshXYZ :: Namelist -> XYZ -> Bool
-- isInMeshXYZ mesh (XYZ x y z) = isInMesh (x, y, z) mesh

-- isInMesh :: (Double, Double, Double) -> Namelist -> Bool
-- isInMesh point@(x,y,z) mesh = and
--     [ (x >= xmin) && (x <= xmax)
--     , (y >= ymin) && (y <= ymax)
--     , (z >= zmin) && (z <= zmax)
--     ]
--     where
--         XB x1 x2 y1 y2 z1 z2 = getXB mesh

--         xmin = min x1 x2
--         ymin = min y1 y2
--         zmin = min z1 z2

--         xmax = max x1 x2
--         ymax = max y1 y2
--         zmax = max z1 z2

-- getMeshIJK nml@(Namelist "MESH" _ parameters _) = (toInt i, toInt j,toInt k)
--     where
--         [i, j, k] = case getArrayToList "IJK" nml of
--             Just [i,j,k] -> [i,j,k]
--             Just xs -> error $ "wrong number of IJK elements, expected 3, got " ++ (show (length xs))
--             Nothing -> error $ "IJK not found"

-- -- |Uniform meshes can be determined using simply the MESH namelist. For
-- -- non-uniform meshes we need to use the TRN entries.
-- getMeshLines :: NamelistFile -> Namelist -> ([Double], [Double], [Double])
-- getMeshLines fdsData mesh = (xs, ys, zs)
--     where
--         (i,j,k) = getMeshIJK mesh
--         XB x1 x2 y1 y2 z1 z2 = getXB mesh

--         xmin = min x1 x2
--         ymin = min y1 y2
--         zmin = min z1 z2

--         xmax = max x1 x2
--         ymax = max y1 y2
--         zmax = max z1 z2

--         delX = (xmax - xmin)/(fromIntegral i)
--         delY = (ymax - ymin)/(fromIntegral j)
--         delZ = (zmax - zmin)/(fromIntegral k)

--         xs = case trnx of
--             Nothing -> map (\n->xmin + (fromIntegral n)*(delX)) [0..i]
--             Just x -> trnBased
--         ys = case trny of
--             Nothing -> map (\n->ymin + (fromIntegral n)*(delY)) [0..j]
--             Just x -> trnBased
--         zs = case trnz of
--             Nothing -> map (\n->zmin + (fromIntegral n)*(delZ)) [0..k]
--             Just x -> trnBased

--         trnBased = undefined

--         Just meshIndex = findIndex (== mesh) $ getMeshes fdsData

--         trnx = case filter
--                 (\p-> (getParameterMaybe p "MESH_NUMBER") == (Just (ParInt (meshIndex+1))))
--                 $ getTRNXs fdsData of
--             [] -> Nothing
--             [x] -> Just x
--         trny = case filter
--                 (\p-> (getParameterMaybe p "MESH_NUMBER") == (Just (ParInt (meshIndex+1))))
--                 $ getTRNYs fdsData of
--             [] -> Nothing
--             [x] -> Just x
--         trnz = case filter
--                 (\p-> (getParameterMaybe p "MESH_NUMBER") == (Just (ParInt (meshIndex+1))))
--                 $ getTRNZs fdsData of
--             [] -> Nothing
--             [x] -> Just x

-- -- TODO: check that any float is a whole number first
-- toInt :: ParameterValue -> Int
-- toInt (ParInt i) = i
-- toInt (ParDouble d) = round d

-- getMeshSkew nml@(Namelist "MESH" _ parameters _) =
--     let (dx,dy,dz) = getMeshResolution nml
--     in (maximum [dx,dy,dz] / minimum [dx,dy,dz])
--     where
--         (i,j,k) = getMeshIJK nml

-- getMeshResolution nml@(Namelist "MESH" _ parameters _) =
--     (x/(fromIntegral i),y/(fromIntegral j),z/(fromIntegral k))
--     where
--         (i,j,k) = getMeshIJK nml
--         (x,y,z) = getDimensions nml

-- getMeshResolution _ = error "Incorrect namelist supplied."
-- getObstDimensions = getDimensions
-- getDimensions nml@(Namelist _ _ parameters _) = (x,y,z)
--     where
--         XB x1 x2 y1 y2 z1 z2 = getXB nml
--         x = x2 - x1
--         y = y2 - y1
--         z = z2 - z1

-- getXB nml = fromMaybe (error "No XB") $ getXBMaybe nml

getXBMaybe :: Namelist -> Maybe XB
getXBMaybe nml@(Namelist _ _ parameters _) = case getArrayToList "XB" nml of
    Just [a,b,c,d,e,f] ->
        let [x1, x2, y1, y2, z1, z2] = map parToDouble [a,b,c,d,e,f]
        in Just $ XB x1 x2 y1 y2 z1 z2
    Just e -> error $ T.unpack $ "Failed to parse XB array: " <> T.unlines (map pprint e)
    Nothing -> Nothing

-- getXYZ nml@(Namelist _ _ parameters _) = case r of
--     Just [x, y, z] -> Just (x, y, z)
--     _ -> Nothing
--     where
--         r = case getArrayToList "XYZ" nml of
--                 Just [a,b,c] -> Just $ map parToDouble [a,b,c]
--                 Just e -> error $ T.unpack $ "Failed to parse XYZ array: " <> T.unlines (map pprint e)
--                 Nothing -> Nothing

-- getObstResolutions fdsData obst =
--     map getMeshResolution $ findObstMesh fdsData obst

-- getSmallestResolution fdsData = smallest
--     where
--         resolutions = map getMeshResolution $ getMeshes fdsData
--         smallest = minimumBy compareResolutions resolutions

-- getOrderedResolutions fdsData = smallest
--     where
--         resolutions = map getMeshResolution $ getMeshes fdsData
--         smallest = sortBy compareResolutions resolutions

-- compareResolutions (x1,y1,z1) (x2,y2,z2) = compare (x1*y1*z1) (x2*y2*z2)

-- findObstMesh fdsData obst = filter (isOverlap obst) meshes
--     where
--         meshes = getMeshes fdsData

-- -- |Test two Namelists with XBs and determine if they intersect in 3d.
-- nmlIntersect :: Namelist -> Namelist -> Bool
-- nmlIntersect nmlA nmlB = xbIntersect xbA xbB
--     where
--         xbA = getXB nmlA
--         xbB = getXB nmlB

-- -- |Test if two XBs intersect (i.e. their bounding boxes). Two bounding boxes
-- -- intersect of all 3 dimensions have overlap. EQ is considered overlap.
-- xbIntersect :: XB -> XB -> Bool
-- xbIntersect xbA xbB = intersectX && intersectY && intersectZ
--     where
--         intersectX = x2A > x1B && x2B > x1A
--         intersectY = y2A > y1B && y2B > y1A
--         intersectZ = z2A > z1B && z2B > z1A

--         XB x1A x2A y1A y2A z1A z2A = sortXB xbA
--         XB x1B x2B y1B y2B z1B z2B  = sortXB xbB

-- -- | Sort an XB such that x2>x1 y2>y1 and z2>z1.
-- sortXB :: XB -> XB
-- sortXB (XB x1' x2' y1' y2' z1' z2') = (XB x1 x2 y1 y2 z1 z2)
--     where
--         x1 = min x1' x2'
--         x2 = max x1' x2'
--         y1 = min y1' y2'
--         y2 = max y1' y2'
--         z1 = min z1' z2'
--         z2 = max z1' z2'

-- -- |Test if the XBs of the two namelists overlap
-- isOverlap nmlA nmlB = not $ any id [cond1, cond2, cond3, cond4, cond5, cond6]
--     where
--         cond1 = x2A < x1B
--         cond2 = x1A > x2B
--         cond3 = y2A < y1B
--         cond4 = y1A > y2B
--         cond5 = z2A < z1B
--         cond6 = z1A > z2B
--         XB x1A x2A y1A y2A z1A z2A = getXB nmlA --peform min/max
--         XB x1B x2B y1B y2B z1B z2B = getXB nmlB


-- getSprinklers fdsData = filter (isSprinkler fdsData)
--     $ findNamelists fdsData "DEVC"

-- isSprinkler :: NamelistFile -> Namelist -> Bool
-- isSprinkler fdsData nml@(Namelist _ _ parameters _) =
--     case getParameterMaybe nml "PROP_ID" of
--          Nothing -> False
--          Just propName -> propName `elem` (getSprinklerPropIds fdsData)

-- getSprinklerPropIds :: NamelistFile -> [ParameterValue]
-- getSprinklerPropIds fdsData =
--     fmap (flip getParameter "ID")
--     $ filter (isSprinklerProp fdsData)
--     $ findNamelists fdsData "PROP"

-- isSprinklerProp fdsData nml@(Namelist _ _ parameters _) =
--     case getParameterMaybe nml "QUANTITY" of
--         Just (ParString "SPRINKLER LINK TEMPERATURE") -> True
--         _ -> False

-- isObst nml = (nml_name nml) == "OBST"

-- sprinklerActivationTemperature fdsData nml = do
--     propId <- getParameterMaybe nml "PROP_ID"
--     let prop = case filter (\p-> (getParameter p "ID") == propId)
--                     $ findNamelists fdsData "PROP" of
--             [] -> error "no matching prop"
--             [x] -> x
--             _ -> error "too many matching props"
--     (ParDouble temp) <- getParameterMaybe prop "ACTIVATION_TEMPERATURE"
--     return temp

-- getNDRs fdsData = map (ndr fdsData) burners
--     where
--         burners = getBurners fdsData

-- ndr fdsData burner = ndr
--     where
--         q = maxBurnerHRR fdsData burner
--         resolutions = getObstResolutions fdsData burner
--         nominalCellSizes = map getMaxCellSize resolutions
--         nominalCellSize = maximum nominalCellSizes
--         getMaxCellSize (x,y,z) = maximum [x,y,z]
--         charFireDiameter = (q / (ambientDensity*ambientSpecificHeat*ambientTemperature*(sqrt g)))**(2/5)
--         ndr = charFireDiameter/nominalCellSize
--         calcs =
--           [ "Min. Mesh Resolution: " ++ show nominalCellSize ++ " m"
--           , "Non-Dimensionalised Ratio: " ++ show ndr
--           ]
--         ambientDensity = 1.205
--         ambientSpecificHeat = 1.005
--         ambientTemperature = 293.15
--         g = 9.81


-- smokeDetectorObscuration fdsData nml = do
--     propId <- getParameterMaybe nml "PROP_ID"
--     let isProp p = getParameter p "ID" == propId
--     let prop = case filter isProp $ findNamelists fdsData "PROP" of
--             [] -> error "no matching prop"
--             [x] -> x
--             _ -> error "too many matching props"
--     return $  case getParameterMaybe prop "ACTIVATION_OBSCURATION" of
--         Nothing -> 3.24 -- FDS Default
--         Just (ParDouble x) -> x

-- getSootProductionRate fdsData = y_s/hoc*hrr
--     where
--         [reac] = findNamelists fdsData "REAC"
--         y_s = case getParameterMaybe reac "SOOT_YIELD" of
--             Just (ParDouble x) -> x
--         hoc = getHoC fdsData
--         hrr = getTotalMaxHRR fdsData

-- -- To calculate the heat of combustion, deteremine the oxygen consumption
-- -- then multiply by EPUMO2
-- -- TODO: include the other methods of specifying HoC.
-- -- Investigate the minor difference between this and the .out file value
-- getHoC fdsData = v_o2*w_o2*epumo2/(v_F*w_F)
--     where
--         [reac] = findNamelists fdsData "REAC"

--         y_s = case getParameterMaybe reac "SOOT_YIELD" of
--             Just (ParDouble x) -> x
--         y_co = case getParameterMaybe reac "CO_YIELD" of
--             Just (ParDouble x) -> x
--         soot_h_fraction = case getParameterMaybe reac "SOOT_H_FRACTION" of
--             Just (ParDouble x) -> x
--             Nothing -> 0 -- TODO: abstract defaults to a separate table
--         epumo2 = case getParameterMaybe reac "EMPUMO2" of
--             Just (ParDouble x) -> x
--             Nothing -> 13100


--         -- for fuel molecule CxHyOzNv
--         (ParDouble x) = getParameter reac "C"
--         (ParDouble y) = getParameter reac "H"
--         (ParDouble z) = getParameter reac "O"
--         (ParDouble v) = getParameter reac "N"

--         w_c = 12.01 -- ^Molar mass of atomic carbon.
--         w_h = 1.008 -- ^Molar mass of atomic hydrogen.
--         w_o = 15.999 -- ^Molar mass of atomic oxygen.
--         w_n = 14.007 -- ^Molar mass of atomic nitrogen.

--         w_o2 = w_o*2
--         w_co = 1*w_c + 1*w_o

--         v_F = 1

--         -- |Molar mass of fuel.
--         w_F = x*w_c + y*w_h + z*w_o + v*w_n

--         -- 'v_' represents molar fraction

--         v_o2 = v_co2 + v_co/2 + v_h2o/2 - z/2

--         v_co2 = x - v_co - (1-soot_h_fraction) * v_s

--         v_h2o = y/2 - soot_h_fraction/2 * v_s

--         v_co = w_F/w_co * y_co

--         v_s = w_F/w_S * y_s

--         v_n2 = v/2

--         w_S = soot_h_fraction*w_h + (1-soot_h_fraction)*w_c

-- getSmokeDetectors fdsData =
--     filter (isSmokeDetector fdsData) $ findNamelists fdsData "DEVC"

-- isSmokeDetector fdsData nml@(Namelist _ _ parameters _) =
--     case getParameterMaybe nml "PROP_ID" of
--         Nothing -> False
--         Just propName -> propName `elem` (getSmokeDetectorPropIds fdsData)

-- getSmokeDetectorPropIds fdsData =
--     fmap (flip getParameter "ID")
--     $ filter (isSmokeDetectorProp fdsData)
--     $ findNamelists fdsData "PROP"

-- isSmokeDetectorProp fdsData nml@(Namelist _ _ parameters _) =
--     case getParameterMaybe nml "QUANTITY" of
--        Just (ParString "CHAMBER OBSCURATION") -> True
--        _ -> False

-- getThermalDetectors fdsData =
--     filter (isThermalDetector fdsData)
--     $ findNamelists fdsData "DEVC"

-- isThermalDetector fdsData nml@(Namelist _ _ parameters _) =
--     case getParameterMaybe nml "PROP_ID" of
--         Nothing -> False
--         Just propName -> propName `elem` (getThermalDetectorPropIds fdsData)

-- getThermalDetectorPropIds fdsData =
--     fmap (flip getParameter "ID")
--     $ filter (isThermalDetectorProp fdsData)
--     $ findNamelists fdsData "PROP"

-- isThermalDetectorProp fdsData nml@(Namelist _ _ parameters _) =
--     case getParameterMaybe nml "QUANTITY" of
--         Just (ParString "LINK TEMPERATURE") -> True
--         _ -> False

-- getBurnerSurfaces fdsData
--     = filter (\n-> (hasParameter n "HRRPUA")) $ findNamelists fdsData "SURF"

-- getExhaustSurfaces fdsData
--     = filter isExhaustSurface $ findNamelists fdsData "SURF"

-- getSupplySurfaces fdsData
--     = filter isSupplySurface $ findNamelists fdsData "SURF"

-- isExhaustSurface nml
--   | hasParameter nml "VEL" =
--       let flow = parToDouble $ getParameter nml "VEL"
--       in flow >= 0
--   | hasParameter nml "VOLUME_FLUX" =
--       let flow = parToDouble $ getParameter nml "VOLUME_FLUX"
--       in flow >= 0
--   | hasParameter nml "VOLUME_FLOW" =
--       let flow = getDouble $ getParameter nml "VOLUME_FLOW"
--       in flow >= 0
--   | otherwise = False

-- isSupplySurface nml
--   | hasParameter nml "VEL" =
--       let flow = parToDouble $ getParameter nml "VEL"
--       in flow < 0
--   | hasParameter nml "VOLUME_FLUX" =
--       let flow = parToDouble $ getParameter nml "VOLUME_FLUX"
--       in flow < 0
--   | hasParameter nml "VOLUME_FLOW" =
--       let flow = parToDouble $ getParameter nml "VOLUME_FLOW"
--       in flow < 0
--   | otherwise = False

-- getFlowRate fdsData nml = case flowRate of
--   Just x -> x
--   Nothing -> error $ T.unpack $ "mech vent does not have a flow rate:\n" <> pprint nml
--   where
--     flowRate = do
--       surfIds <- case getParameterMaybe nml "SURF_ID" of
--                       Just x -> Just [x]
--                       Nothing -> case getParameterMaybe nml "SURF_IDS" of
--                           Just x -> getArrayToList "SURF_IDS" nml
--                           Nothing -> case getParameterMaybe nml "SURF_ID6" of
--                               Just x -> getArrayToList "SURF_ID6" nml
--                               Nothing -> Nothing
--       let surfs =
--               case (filter (\p-> (getParameter p "ID") `elem` surfIds)
--                            $ findNamelists fdsData "SURF") of
--               [] -> error "no matching surf"
--               x -> x
--           exSurfs = filter (\nml
--                            -> hasParameter nml "VOLUME_FLOW"
--                            || hasParameter nml "VOLUME_FLUX"
--                            || hasParameter nml "VEL") surfs
--           surf = case exSurfs of
--               [x] -> x
--               x -> error $ T.unpack $ "23423:\nsurfs: "
--                          <> pprint surfIds <> "\nsurfs: " <> pprint x
--       flow <- case getParameterMaybe surf "VOLUME_FLOW" of
--                       Just x -> Just x
--                       Nothing -> case getParameterMaybe surf "VOLUME_FLUX" of
--                           Just x -> Just x
--                           Nothing -> Nothing
--                           -- TODO: cover other types of flow spec
--       return $ parToDouble flow



-- getTotalMaxHRR fdsData = sum $ map (getMaxHRR fdsData) $ getBurners fdsData


-- getBurnerObsts :: NamelistFile -> [Namelist]
-- getBurnerObsts fdsData
--     = filter (isBurner fdsData) $ findNamelists fdsData "OBST"

-- getBurners fdsData = (filter (isBurner fdsData) $ findNamelists fdsData "OBST")
--       ++ (filter (isBurner fdsData) $ findNamelists fdsData "VENT")

-- getExhausts fdsData =
--     (filter (isExhaust fdsData) $ findNamelists fdsData "OBST")
--         ++ (filter (isExhaust fdsData) $ findNamelists fdsData "VENT")

-- getSupplies fdsData = (filter (isSupply fdsData) $ findNamelists fdsData "OBST")
--       ++ (filter (isSupply fdsData) $ findNamelists fdsData "VENT")

-- -- |Determines which sides of an obstruction are burner surfaces
-- -- returns Bools in the form (xMin,xMax,yMin,yMax,zMin,zMax)
-- getBurnerSideStatus :: NamelistFile -> Namelist -> (Bool, Bool, Bool, Bool, Bool, Bool)
-- getBurnerSideStatus fdsData nml = (xMinStatus, xMaxStatus,yMinStatus,yMaxStatus,zMinStatus,zMaxStatus)
--   where
--     burnerSurfNames = map (getIdString) $ getBurnerSurfaces fdsData
--     (xMinSurfName,xMaxSurfName,yMinSurfName,yMaxSurfName,zMinSurfName,zMaxSurfName) = getObstSurfNames nml
--     xMinStatus = xMinSurfName `elem` burnerSurfNames
--     xMaxStatus = xMaxSurfName `elem` burnerSurfNames
--     yMinStatus = yMinSurfName `elem` burnerSurfNames
--     yMaxStatus = yMaxSurfName `elem` burnerSurfNames
--     zMinStatus = zMinSurfName `elem` burnerSurfNames
--     zMaxStatus = zMaxSurfName `elem` burnerSurfNames

-- getObstSurfNames :: Namelist -> (String, String, String, String, String, String)
-- getObstSurfNames obst = if hasParameter obst "SURF_ID"
--       then let (ParString name) = getParameter obst "SURF_ID" in (T.unpack name, T.unpack name, T.unpack name, T.unpack name, T.unpack name, T.unpack name)
--       else if hasParameter obst "SURF_IDS"
--               then let [topName, sidesName, bottomName] = map parToString $ (fromMaybe (error "No surf_id6") $ getArrayToList "SURF_IDS" obst) in (sidesName, sidesName, sidesName, sidesName, bottomName, topName)
--               else if hasParameter obst "SURF_ID6"
--                       then let [xMinName,xMaxName,yMinName,yMaxName,zMinName,zMaxName] = map parToString $ (fromMaybe (error "No surf_ids") $ getArrayToList "SURF_ID6" obst) in (xMinName,xMaxName,yMinName,yMaxName,zMinName,zMaxName)
--                                                                                                                           else error "Surfaces not parsed correctly. 7687"


-- getBurnerSurfNames :: NamelistFile -> Namelist -> [String]
-- getBurnerSurfNames fdsData nml@(Namelist _ _  parameters _)
--     | hasParameter nml "SURF_IDS"
--         = (intersect burnerSurfNames
--         $ map getString
--         $ fromMaybe (error "No surf_ids") $ getArrayToList "SURF_IDS" nml)
--     | hasParameter nml "SURF_ID6"
--         = (intersect burnerSurfNames
--         $ map getString
--         $ fromMaybe (error "No surf_id6") $ getArrayToList "SURF_ID6" nml)
--     | hasParameter nml "SURF_ID" =
--         let currentSurf = getString
--                 $ getParameter nml "SURF_ID"
--         in if currentSurf `elem` burnerSurfNames then [currentSurf] else []
--     | otherwise = []
--     where
--         burnerSurfNames = map (getIdString) $ getBurnerSurfaces fdsData

-- isBurner fdsData nml@(Namelist _ _ parameters _)
--     | hasParameter nml "SURF_IDS"
--         = 0 < (length
--         $ intersect burnerSurfNames
--         $ map getString
--         $ fromMaybe (error "No surf_ids") $ getArrayToList "SURF_IDS" nml)
--     | hasParameter nml "SURF_ID6"
--         = 0 < (length
--         $ intersect burnerSurfNames
--         $ map getString
--         $ fromMaybe (error "No surf_id6") $ getArrayToList "SURF_ID6" nml)
--     | hasParameter nml "SURF_ID" =
--         let currentSurf = getString
--                 $ getParameter nml "SURF_ID"
--         in currentSurf `elem` burnerSurfNames
--     | otherwise = False
--     where
--         burnerSurfNames = map (getIdString) $ getBurnerSurfaces fdsData

-- isExhaust fdsData nml@(Namelist _ _ parameters _)
--     | hasParameter nml "SURF_IDS"
--         = 0 < (length
--         $ intersect exhaustSurfNames
--         $ map getString
--         $ fromMaybe (error "No surf_ids") $getArrayToList "SURF_IDS" nml)
--      | hasParameter nml "SURF_ID6"
--         = 0 < (length
--         $ intersect exhaustSurfNames
--         $ map getString
--         $ fromMaybe (error "No surf_id6") $getArrayToList "SURF_ID6" nml)
--     | hasParameter nml "SURF_ID" =
--         let currentSurf = getString
--                 $ getParameter nml "SURF_ID"
--         in currentSurf `elem` exhaustSurfNames
--     | otherwise = False
--     where
--         exhaustSurfNames = map (getIdString) $ getExhaustSurfaces fdsData

-- isSupply fdsData nml@(Namelist _ _ parameters _)
--     | hasParameter nml "SURF_IDS"
--         = 0 < (length
--         $ intersect supplySurfNames
--         $ map getString
--         $ fromMaybe (error "No surf_id6") $ getArrayToList "SURF_IDS" nml)
--     | hasParameter nml "SURF_ID" =
--         let currentSurf = getString
--                 $ getParameter nml "SURF_ID"
--         in currentSurf `elem` supplySurfNames
--     | otherwise = False
--     where
--         supplySurfNames = map (getIdString) $ getSupplySurfaces fdsData

parToDouble :: ParameterValue -> Double
parToDouble (ParInt x) = fromIntegral x
parToDouble (ParDouble x) = x
parToDouble x = error $ show x <> " is not a number"

parToInt :: ParameterValue -> Int
parToInt (ParInt x) = x
parToInt x = error $ show x <> " is not an int"

parToString :: ParameterValue -> String
parToString (ParString x) = T.unpack x
parToString x = error $ show x <> " is not a string"

parToBool :: ParameterValue -> Bool
parToBool (ParBool x) = x
parToBool x = error $ show x <> " is not a bool"

-- checkGrowthRate fdsData burner = evalGrowthRate hrrAlpha
--     where
--         (hrrCap, hrrAlpha) = burnerGrowthRate fdsData burner
-- maxBurnerHRR = getMaxHRR

-- burnerGrowthRate fdsData burner@(Namelist _ _ params _)
--     = (hrrCap, hrrAlpha)
--     where
--         hrrCap = hrrpua * (burnerArea fdsData burner)
--         hrrAlpha = hrrCap / ((abs tau_q)**2)
--         surf = if isBurner fdsData burner
--                     then getBurnerSurf fdsData burner
--                     else error "this is not a burner"
--         hrrpua = getDouble $ getParameter surf "HRRPUA"
--         tau_q  = getDouble $ getParameter surf "TAU_Q"


-- getMaxHRR fdsData burner = maxHRR $ getHRR fdsData burner

-- getHRR fdsData burner =
--    let surf = getBurnerSurf fdsData burner
--        area = burnerArea fdsData burner
--        nominalHRRPUA = getDouble $ getParameter surf "HRRPUA"
--        tauq = getParameterMaybe surf "TAU_Q"
--        rampq = getParameterMaybe surf "RAMP_Q"
--    in case (tauq,rampq) of
--       (Nothing, Nothing) -> HRRStatic (nominalHRRPUA*area)
--       (Just t, Nothing) -> HRRTauQ (nominalHRRPUA*area) (getDouble t)
--       (Nothing, Just r) -> HRRRamp
--           $ map (\(t,f)->(t,nominalHRRPUA*area*f))
--           $ getRamp fdsData (getString r)
--       (Just _,  Just _) ->
--           error "Both TAU_Q and RAMP_Q set for burner surface"
-- data HRR
--     = HRRTauQ Double Double
--     | HRRRamp [(Double, Double)]
--     | HRRStatic Double

-- maxHRR :: HRR -> Double
-- maxHRR (HRRTauQ m t) = m
-- maxHRR (HRRRamp es) = maximum $ snd $ unzip es
-- maxHRR (HRRStatic hrr) = hrr

-- getRamp fdsData rampId =
--     let rampEntries = findNamelists fdsData "RAMP"
--         matchingRampEntries = filter
--             (\nml-> (getString $ getParameter nml "ID") == rampId)
--             rampEntries
--         mkVal nml =
--             let t = getDouble $ getParameter nml "T"
--                 f = getDouble $ getParameter nml "F"
--             in (t,f)
--         vals = map mkVal matchingRampEntries
--     in vals

-- getBurnerSurf :: NamelistFile -> Namelist -> Namelist
-- getBurnerSurf fdsData nml@(Namelist _ _ parameters _)
--     | hasParameter nml "SURF_IDS"
--         = let currentSurfName = case matchingSurfNames of
--                 [] -> error "No surfaces"
--                 [h] -> h
--                 _ -> error "Surface defined multiple times"
--               matchingSurfNames
--                 = intersect burnerSurfNames
--                 $ map getString
--                 $ fromMaybe (error "No surf_id6") $ getArrayToList "SURF_IDS" nml
--               matches = filter
--                     (\n-> currentSurfName == (getIdString n))
--                     surfs
--           in case matches of
--                 [] -> error "No burner surf matches"
--                 (x:xs) -> x
--     | hasParameter nml "SURF_ID6"
--         = let currentSurfName = case matchingSurfNames of
--                 [] -> error "No surfaces"
--                 [h] -> h
--                 _ -> error "Surface defined multiple times"
--               matchingSurfNames
--                 = intersect burnerSurfNames
--                 $ map getString
--                 $ fromMaybe (error "No SURF_ID6") $ getArrayToList "SURF_ID6" nml
--               matches = filter
--                     (\n-> currentSurfName == (getIdString n))
--                     surfs
--           in case matches of
--                 [] -> error "No burner surf matches"
--                 (x:xs) -> x
--     | hasParameter nml "SURF_ID" =
--         let currentSurfName = getString
--                 $ getParameter nml "SURF_ID"
--         in headErr "getBurnerSurf" $ filter
--                   (\n-> currentSurfName == (getIdString n))
--                   surfs
--     | otherwise = error "Burner has no SURF parameter"
--     where
--         burnerSurfNames = map (\x->getString $ getParameter x "ID")
--             $ getBurnerSurfaces fdsData
--         surfs = findNamelists fdsData "SURF"

-- getIdString :: Namelist -> String
-- getIdString nml = getString $ getParameter nml "ID"

-- getIdStringMaybe :: Namelist -> Maybe String
-- getIdStringMaybe nml = getString <$> getParameterMaybe nml "ID"

-- getString :: ParameterValue -> String
-- getString = (\(ParString string) -> T.unpack string)


-- getDouble par = case par of
--     (ParDouble d) -> d
--     (ParInt i)    -> fromIntegral i

-- getSimTimes :: NamelistFile -> (Double, Double)
-- getSimTimes fdsData = (timeStart, timeEnd)
--   where
--     time = case findNamelists fdsData "TIME" of
--         [] -> error $ "TIME not specified."
--         [x] -> x
--         _ -> error $ "Multiple TIMEs specified."
--     timeStart = case getParameterMaybe time "T_BEGIN" of
--         Nothing -> 0.0
--         Just ((ParDouble x)) -> x
--         Just ((ParInt x)) -> fromIntegral x
--     timeEnd   = case getParameterMaybe time "T_END" of
--         Nothing -> 1.0
--         Just ((ParDouble x)) -> x
--         Just ((ParInt x)) -> fromIntegral x

-- getCHID :: NamelistFile -> String
-- getCHID fdsData = chid
--   where
--     head = case findNamelists fdsData "HEAD" of
--         [] -> error $ "HEAD not specified."
--         [x] -> x
--         _ -> error $ "Multiple HEADs specified."
--     chid = case getParameterMaybe head "CHID" of
--         Nothing -> error $ "No CHID specified."
--         Just ((ParString x)) -> T.unpack x
--         Just x -> error $ T.unpack $ "Invalid type for CHID: " <> pprint x

-- -- TODO: this function does not account for multiple burner
-- -- surface types on the one obstruction, deprecate this function
-- burnerArea :: NamelistFile -> Namelist -> Double
-- burnerArea fdsData nml = sum
--                                   [ xMinSurfArea
--                                   , xMaxSurfArea
--                                   , yMinSurfArea
--                                   , yMaxSurfArea
--                                   , zMinSurfArea
--                                   , zMaxSurfArea
--                                   ]
--     where
--         parameters = nml_params nml
--         (delX, delY, delZ) = getObstDimensions nml
--         xArea = delY*delZ
--         yArea = delX*delZ
--         zArea = delX*delY
--         xMinSurfArea =  if xMinStatus then xArea else 0
--         xMaxSurfArea =  if xMaxStatus then xArea else 0
--         yMinSurfArea =  if yMinStatus then yArea else 0
--         yMaxSurfArea =  if yMaxStatus then yArea else 0
--         zMinSurfArea =  if zMinStatus then zArea else 0
--         zMaxSurfArea =  if zMaxStatus then zArea else 0
--         (xMinStatus, xMaxStatus,yMinStatus,yMaxStatus,zMinStatus,zMaxStatus) = getBurnerSideStatus fdsData nml

-- -- |Given an obstruction and a particular surf name, find the
-- -- area of that type of surface contributed by the obstruction.
-- determineSurfAreaObst :: Namelist -> String -> Double
-- determineSurfAreaObst obst surfName = sum
--                                   [ xMinSurfArea
--                                   , xMaxSurfArea
--                                   , yMinSurfArea
--                                   , yMaxSurfArea
--                                   , zMinSurfArea
--                                   , zMaxSurfArea
--                                   ]
--   where
--     XB x1 x2 y1 y2 z1 z2  = getXB obst
--     delX = abs (x2-x1)
--     delY = abs (y2-y1)
--     delZ = abs (z2-z1)
--     xArea = delY*delZ
--     yArea = delX*delZ
--     zArea = delX*delY
--     xMinSurfArea =  if xMinSurfName == surfName then xArea else 0
--     xMaxSurfArea =  if xMaxSurfName == surfName then xArea else 0
--     yMinSurfArea =  if yMinSurfName == surfName then yArea else 0
--     yMaxSurfArea =  if yMaxSurfName == surfName then yArea else 0
--     zMinSurfArea =  if zMinSurfName == surfName then zArea else 0
--     zMaxSurfArea =  if zMaxSurfName == surfName then zArea else 0
--     (xMinSurfName,xMaxSurfName,yMinSurfName,yMaxSurfName,zMinSurfName,zMaxSurfName) = getObstSurfNames obst

-- -- |Determine the axis of orientation of a plane in space.
-- -- planeOrientation (x1,x2,y1,y2,x1,x2) =

-- hasParameter nml parameterName = M.member parameterName (nml_params nml)
-- getParameter nml parameterName =
--     case M.lookup parameterName (nml_params nml) of
--        Just x -> x
--        Nothing -> error $ T.unpack $ "no such parameter: " <> parameterName <> " for: " <> pprint nml
getParameterMaybe :: Namelist -> T.Text -> Maybe ParameterValue
getParameterMaybe nml parameterName =
    M.lookup parameterName (nml_params nml)

-- getSurfId nml = getString <$> getParameterMaybe nml "SURF_ID"

-- hasParameterValue :: ToParameterValue a => T.Text -> a -> Namelist -> Bool
-- hasParameterValue parName parValue nml =
--     let theValue = M.lookup parName (nml_params nml)
--     in case theValue of
--         Nothing -> False
--         Just x -> x == (toParameterValue parValue)


getArrayToList :: T.Text -> Namelist -> Maybe [ParameterValue]
getArrayToList parName nml = case M.lookup parName (nml_params nml) of
    Just (ParArray arr) -> Just $ M.elems arr
    _ -> Nothing

-- -- TODO: make sure to take the closest one
-- -- the time of measurement is at the cap tiem
-- evalGrowthRate alpha
--     | absDiff < 0.001 =
--         Right (growthRate, (growthRateToAlpha growthRate) - alpha)
--     | otherwise    = Left alpha
--     where
--         (absDiff, growthRate) = minimum $ map (growthRateDiff alpha) growthRates
--         eps = 0.001
--         eq = aEq eps

-- growthRateDiff alpha growthRate =
--     (abs $ (growthRateToAlpha growthRate) - alpha, growthRate)

-- growthRates =
--     [ NFPASlow
--     , NFPAFast
--     , NFPAMedium
--     , NFPAUltrafast
--     , EurocodeSlow
--     , EurocodeMedium
--     , EurocodeFast
--     , EurocodeUltrafast
--     ]

-- convertToProper :: ParameterValue
-- aEq eps a b = a < (b+eps) && a > (b-eps)

findNamelists :: NamelistFile -> T.Text -> [Namelist]
findNamelists (NamelistFile comments namelists) nameTarget =
    filter (\(Namelist name _ _ _) -> name == nameTarget) namelists
