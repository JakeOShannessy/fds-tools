module FDSUtilities.Parsing.SMVFile where

import Control.Monad
import Control.Exception hiding (try)

import Data.Either
import Data.List
import Data.Time

import FDSUtilities.Types
import FDSUtilities.Parsing.Common
-- import FDSUtilities.Simulation

import System.FilePath

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

-- TODO: High priority, handle spaces in device names
-- TODO: Compile a library of example .out files and build full-scale tests
-- TODO: Implement unit-testing as well.


-- TODO: move the below (which require F.Types and F.Simulation to a F.Parsing module at a higher level)
-- parseSimulationSMVFile :: FDSSimulation -> IO (Either ParseError SMVFile)
-- parseSimulationSMVFile :: FDSSimulation -> IO SMVFile
-- parseSimulationSMVFile simulation = do
    -- parsedData <- parseFromFile smvParser smvFilePath
    -- return $ case parsedData of
                -- Right x -> x
                -- Left err -> error $ show err -- TODO: this is a parse error, handle properly
    -- where
        -- smvFilePath = joinPath [simDir simulation, (simCHID simulation) ++ ".smv"]

parseSMVFile :: FilePath -> IO (Either ParseError SMVFile)
parseSMVFile filePath = parseFromFile smvParser filePath
-- parseMajorVersion

supportedVersions :: [Version]
supportedVersions =
    [ Version 5 5 3
    , Version 5 5 0
    , Version 6 0 0
    , Version 6 0 1
    , Version 6 1 0
    , Version 6 1 1
    , Version 6 1 2
    ]

smvParser :: Parser SMVFile
smvParser = do
    string "TITLE" *> eol *> char ' '
    title <- optionMaybe titleString
    eol

    (string "VERSION" <|> string "FDSVERSION") *> eol
    fdsVersion <- string "FDS" *> char ' ' *> parseVersion <* eol
    fdsGitVersion <- basicString <* eol
    eol

    string "ENDF" *> eol
    endf <- char ' ' *> fullString <* eol
    eol

    string "INPF" *> eol
    inpf <- char ' ' *> fullString <* eol
    eol

    string "REVISION" *> eol
    revision <- onlySpaces *> basicString
    onlySpaces <* eol
    eol

    string "CHID" *> eol
    chid <- char ' ' *> fullString <* eol
    eol

    csvEntries <- if fdsVersion >= (Version 6 0 0)
        then many (do
            string "CSVF"
            eol
            csvFile <- parseCSVEntry
            eol
            return csvFile)
        else return []

    string "NMESHES" *> eol
    nMeshes <- onlySpaces *> intNum <*eol
    eol

    string "VIEWTIMES" *> eol
    a <- onlySpaces *> floatNum
    b <- onlySpaces *> floatNum
    c <- onlySpaces *> intNum <* eol
    eol

    geomModel <- optionMaybe (do
        string "CADGEOM" <* eol

        geomModel <- onlySpaces *> basicStringExtra <* eol
        eol
        return geomModel
        )

    string "ALBEDO" *> eol
    d <- onlySpaces *> floatNum <* eol
    eol

    iblank <- optionMaybe (do
        string "IBLANK" *> eol
        iBlank <- onlySpaces *> intNum <* eol
        eol
        return iBlank
        )

    string "GVEC" *> eol
    onlySpaces *> floatNum
    onlySpaces *> floatNum
    onlySpaces *> floatNum <* eol
    eol

    string "SURFDEF" *> eol
    surfDef <- onlySpaces *> basicString
    onlySpaces <* eol
    eol

    surfs <- many parseSurface
    matls <- many parseMaterial
    geom <- optionMaybe parseGeom
    pClass <- many (try parseClassOfParticles)
    hClass <- pure [] -- many (try parseClassOfHumans)
    outline <- parseOutline

    string "TOFFSET" *> eol
    e <- onlySpaces *> floatNum
    f <- onlySpaces *> floatNum
    g <- onlySpaces *> floatNum <* eol
    eol

    string "HRRPUVCUT" *> eol
    h <- onlySpaces *> intNum <* eol
    j <- many (do; onlySpaces; i <- floatNum; eol; return i;)
    eol

    string "RAMP" *> eol
    k <- onlySpaces *> intNum <* eol
    ramps <- count k parseRamp
    eol

    props <- many parseProp
    devices <- many parseDevice

    if fdsVersion >= (Version 6 0 0)
        then do
            string "VERT" *> eol

            nVerts <- onlySpaces *> intNum <* eol
            -- verts <- count nVerts parseVerts
            eol

            string "FACE" *> eol
            nFaces <- onlySpaces *> intNum <* eol
            -- faces <- count nFaces parseFace
            eol

            pure ()
        else pure ()



    meshes <- many (parseMesh fdsVersion)
    optional $ try $ string "SMOKEDIFF" *> eol
    tailEntries <- many1 parseTailEntry
    let (dataFileEntries, devActEntries, obstVisEntries) = sortTailEntries tailEntries
    eof
    return $ SMVFile
        { title         = title
        , fdsVersion    = fdsVersion
        , nMeshes       = nMeshes
        , surfs         = surfs
        , pClass        = pClass
        , outline       = outline
        , props         = props
        , devices       = devices
        , smvMeshes     = meshes
        , smvDataFiles  = dataFileEntries
        , devcActs      = devActEntries
        , obstVis       = obstVisEntries
        }

parseCSVEntry :: Parser (String, String)
parseCSVEntry = (,)
    <$> (onlySpaces *> basicString <* eol)
    <*> (onlySpaces *> basicString <* eol)

data SMVFile = SMVFile
    { title         :: Maybe String
    , fdsVersion    :: Version
    , nMeshes       :: Int
    , surfs         :: [SMVSurf]
    , pClass        :: [SMVPClass]
    , outline       :: [SMVOutlineEntry]
    , props         :: [SMVProp]
    , devices       :: [SMVDevice]
    , smvMeshes     :: [SMVMesh]
    , smvDataFiles  :: [DataFileEntry]
    , devcActs      :: [DevcActEntry]
    , obstVis       :: [ObstVisEntry]
    } deriving Show

parseGeom = do
    string "GEOM"
    onlySpaces
    n <- intNum
    eol
    onlySpaces
    geom <- manyTill anyChar eol
    eol

parseRamp :: Parser [(Double, Double)]
parseRamp = do
    string " RAMP: "
    name <- basicStringSpaces <* eol
    nPoints <- onlySpaces *> intNum <* eol
    points <- count nPoints parsePoint
    return points
    where
        -- parse t and f
        parsePoint = (,)
            <$> (onlySpaces *> floatNum)
            <*> (onlySpaces *> floatNum <* onlySpaces <* eol)

parseSurface :: Parser SMVSurf
parseSurface = do
    string "SURFACE" *> eol
    surfName <- onlySpaces *> basicStringSpaces -- TODO: does not allow names with spaces
    onlySpaces
    eol

    a <- onlySpaces *> floatNum
    b <- onlySpaces *> floatNum
    eol
    c <- onlySpaces *> intNum
    d <- onlySpaces *> floatNum
    e <- onlySpaces *> floatNum
    f <- onlySpaces *> floatNum
    g <- onlySpaces *> floatNum
    h <- onlySpaces *> floatNum
    i <- onlySpaces *> floatNum
    eol
    j <- onlySpaces *> basicStringSpacesExtra <* onlySpaces <* eol
    eol

    return $ SMVSurf surfName a b c d e f g h i j

data SMVSurf = SMVSurf String Double Double Int Double Double Double Double Double Double String deriving Show

parseMaterial = do
    string "MATERIAL" *> eol
    matlName <- onlySpaces *> basicStringSpaces <* onlySpaces <* eol -- TODO: does not allow names with spaces
    a <- onlySpaces *> floatNum
    b <- onlySpaces *> floatNum
    c <- onlySpaces *> floatNum <* eol

    eol

    return $ SMVMatl matlName a b c

data SMVMatl = SMVMatl String Double Double Double deriving Show

parseClassOfParticles = do
    string "CLASS_OF_PARTICLES" *> eol
    c <- onlySpaces *> basicStringSpaces
    onlySpaces
    eol
    d <- onlySpaces *> floatNum
    e <- onlySpaces *> floatNum
    f <- onlySpaces *> floatNum
    eol
    g <- onlySpaces *> intNum
    eol
    optionMaybe parseDropletDiameter
    eol
    return $ SMVPClass c d e f g

-- TODO: not yet implemented
-- TODO: currently overloads the GHC simplifier
-- parseClassOfHumans = do
--     string "CLASS_OF_HUMANS" *> eol
--     name <- onlySpaces *> basicStringSpaces <* onlySpaces <* eol
--     d <- onlySpaces *> floatNum
--     e <- onlySpaces *> floatNum
--     f <- onlySpaces *> floatNum
--     eol
--     g <- onlySpaces *> intNum
--     eol
--     onlySpaces
--     string "HUMAN_COLOR" *> eol
--     colour <- onlySpaces *> basicStringSpaces
--     eol
--     onlySpaces
--     eol
--     onlySpaces
--     string "HUMAN_FED_DOSE" *> eol
--     onlySpaces
--     string "FED" *> eol
--     onlySpaces
--     eol
--     onlySpaces
--     string "HUMAN_SPEED" *> eol
--     onlySpaces
--     string "speed" *> eol
--     onlySpaces
--     string "m/s" *> eol
--     -- TODO: unsure of how the below fits, are there one or many
--     string "AVATAR_COLOR" *> eol
--     onlySpaces
--     acN <- intNum
--     eol
--     let parseLine = do
--             c1 <- onlySpaces *> intNum
--             c2 <- onlySpaces *> intNum
--             c3 <- onlySpaces *> intNum
--             eol
--     cs <- count acN parseLine
--     eol
--     return () -- (SMVPClass name d e f g)


parseDropletDiameter = do
    onlySpaces
    string "DROPLET DIAMETER"
    onlySpaces
    eol
    onlySpaces
    string "diam"
    onlySpaces
    eol
    onlySpaces
    string "mu-m"
    onlySpaces
    eol


data SMVPClass = SMVPClass String Double Double Double Int deriving Show

parseOutline = do
    string "OUTLINE"
    eol
    onlySpaces
    a <- intNum
    eol
    entries <- many parseOutlineEntry
    eol
    return entries

-- parse
parseOutlineEntry = do
    a <- onlySpaces *> floatNum
    b <- onlySpaces *> floatNum
    c <- onlySpaces *> floatNum
    d <- onlySpaces *> floatNum
    e <- onlySpaces *> floatNum
    f <- onlySpaces *> floatNum
    eol
    return $ SMVOutlineEntry a b c d e f

data SMVOutlineEntry = SMVOutlineEntry Double Double Double Double Double Double deriving Show

parseProp = do
    string "PROP" *> eol
    name <- onlySpaces *> fullString <* eol
    a <- onlySpaces *> intNum <*eol
    typ <- onlySpaces *> basicString <* eol
    b <- onlySpaces *> intNum <* eol
    eol

    return $ SMVProp name a typ b

data SMVProp = SMVProp String Int String Int deriving Show

parseDevice = do
    string "DEVICE"
    eol
    onlySpaces
    name <- fullString
    -- TODO: this part skips unkown info
    manyTill anyChar eol
    -- eol
    a <- onlySpaces *> floatNum
    b <- onlySpaces *> floatNum
    c <- onlySpaces *> floatNum
    d <- onlySpaces *> floatNum
    e <- onlySpaces *> floatNum
    f <- onlySpaces *> floatNum
    g <- onlySpaces *> intNum
    h <- onlySpaces *> intNum
    clearLine
    eol

    return $ SMVDevice name a b c d e f g h

data SMVDevice = SMVDevice String Double Double Double Double Double Double Int Int deriving Show

parseMesh version = do
    string "OFFSET"
    eol
    offset1 <- onlySpaces *> floatNum
    offset2 <- onlySpaces *> floatNum
    offset3 <- onlySpaces *> floatNum
    eol
    eol

    string "GRID"
    onlySpaces
    name <- basicStringSpaces
    eol
    nXCells <- onlySpaces *> intNum
    nYCells <- onlySpaces *> intNum
    nZCells <- onlySpaces *> intNum
    grid4Unknown <- onlySpaces *> intNum
    eol
    eol

    (bbox, meshColour) <- parsePDims

    trns <- parseTRNs

    obsts <- parseObsts
    vents <- parseVents

    if version >= (Version 6 0 0)
        then do
            string "CVENT"
            eol
            onlySpaces
            nCVents <- intNum
            eol
            -- cvents <- count nCVents parseCVent
            eol
            spaces  -- TODO: remove
        else return ()

    return $ SMVMesh
        { smvMeshName = name
        , smvMeshOffset = (offset1, offset2, offset3)
        , smvMeshGrid = ((nXCells, nYCells, nZCells), grid4Unknown)
        , smvMeshBBox = bbox
        , smvMeshColour = meshColour
        , smvMeshTRNs = trns
        , smvMeshObsts = obsts
        , smvMeshVents = vents
        }

data SMVMesh = SMVMesh
    { smvMeshName   :: String
    , smvMeshOffset :: (Double, Double, Double)
    , smvMeshGrid   :: ((Int, Int, Int), Int)
    , smvMeshBBox   :: (Double, Double, Double, Double, Double, Double)
    , smvMeshColour :: (Double, Double, Double)
    , smvMeshTRNs   :: MeshTRNs
    , smvMeshObsts  :: [SMVObst]
    , smvMeshVents  :: [SMVVent]
    } deriving Show

parsePDims = do
    string "PDIM" *> eol
    xMin <- onlySpaces *> floatNum
    xMax <- onlySpaces *> floatNum
    yMin <- onlySpaces *> floatNum
    yMax <- onlySpaces *> floatNum
    zMin <- onlySpaces *> floatNum
    zMax <- onlySpaces *> floatNum
    r <- onlySpaces *> floatNum
    g <- onlySpaces *> floatNum
    b <- onlySpaces *> floatNum
    eol
    eol
    return $ ((xMin, xMax, yMin, yMax, zMin, zMax), (r,g,b))
data MeshTRNs = MeshTRNs [(Int,Double)] [(Int,Double)] [(Int,Double)] deriving Show

getMeshBounds :: MeshTRNs -> (Double, Double, Double, Double, Double, Double)
getMeshBounds meshTRNs@(MeshTRNs trnx trny trnz) = (xMin, xMax, yMin, yMax, zMin, zMax)
    where
        (_, xMin) = headErr "trnx" trnx
        (_, xMax) = last trnx
        (_, yMin) = headErr "trny" trny
        (_, yMax) = last trny
        (_, zMin) = headErr "trnx" trnz
        (_, zMax) = last trnz
-- TODO: this uses ijk at the min corner, not cell centre. Check for appropriateness.
getCellXB
    :: MeshTRNs -- ^TRNs for the particular mesh
    -> (Int, Int, Int)  -- ^The ijk value for the cell (from zero)
    -> Either String (Double, Double, Double, Double, Double, Double)
getCellXB trns@(MeshTRNs trnx trny trnz) cell@(i, j, k) = do
    xs <- trnLookup trnx i
    ys <- trnLookup trny j
    zs <- trnLookup trnz k
    return $ rearrange xs ys zs
    where   -- TODO: fix inconsistent error handling.
        rearrange (x1, x2) (y1, y2) (z1, z2) = (x1, x2, y1, y2, z1, z2)
        trnLookup :: [(Int,Double)] -> Int -> Either String (Double, Double)
        trnLookup (t:[]) n =
            Left $ "getCellXB: TRN lookup failed for "
                ++ show n ++ ". Max TRN " ++ show t ++ "."
        trnLookup (t1@(t1N,t1V):t2@(t2N,t2V):trn) n
            = if t1N == n
                then Right (t1V,t2V)
                else trnLookup (t2:trn) n

getCornerXYZ :: MeshTRNs -> (Int, Int, Int) -> Either String (Double, Double, Double)
getCornerXYZ trns@(MeshTRNs trnx trny trnz) corner@(i, j, k) = do
    x <- case lookup i trnx of
                Just x -> Right x
                Nothing -> Left $ "getCornerXYZ: TRNx lookup failed for " ++ show i
    y <- case lookup j trny of
                Just y -> Right y
                Nothing -> Left $ "getCornerXYZ: TRNy lookup failed for " ++ show j
    z <- case lookup k trnz of
                Just z -> Right z
                Nothing -> Left $ "getCornerXYZ: TRNz lookup failed for " ++ show k
    return (x, y, z)

-- |Get the minimum corner IJK value of the cell which contains a point
xyzToIJKMinCorner :: MeshTRNs -> (Double, Double, Double) -> Either String (Int, Int, Int)
xyzToIJKMinCorner trns@(MeshTRNs trnx trny trnz) xyz@(x,y,z) = do
    i <- case ijkLookupSingle x trnx of
            Left e -> Left $ e ++ show trnx
            Right x -> Right x
    j <- case ijkLookupSingle y trny of
            Left e -> Left $ e ++ show trny
            Right y -> Right y
    k <- case ijkLookupSingle z trnz of
            Left e -> Left $ e ++ show trnz
            Right z -> Right z
    return (i, j, k)
    where
        epsilon = 0.001  -- TODO: investigate the necessity of this error allowance.
        ijkLookupSingle :: Double -> [(Int,Double)] -> Either String Int
        ijkLookupSingle x ((tN,tV):trns) =
            if (x+epsilon) < tV  -- TODO: investigate this error allowance
                then error $ show (x+epsilon) ++ " is below the minimum trn value of " ++ show tV
                else ijkLookupSingleWorker x ((tN,tV):trns)
        ijkLookupSingleWorker :: Double -> [(Int,Double)] -> Either String Int
        ijkLookupSingleWorker x ((tN,tV):(tN2,tV2):[]) =
            if (x-epsilon) <= tV2 -- TODO: investigate this error allowance
                then Right tN
                else Left $ "xyzToIJK: TRN lookup failed: " ++ show (x-epsilon) ++ " is not <= to " ++ show tV2 ++ "."
        ijkLookupSingleWorker x ((tN,tV):(tN2,tV2):trn)
            = if x <= tV2 then Right tN else ijkLookupSingle x ((tN2,tV2):trn)


getCellCentre :: MeshTRNs -> (Int, Int, Int) -> Either String (Double, Double, Double)
getCellCentre trns cellIJK = case getCellXB trns cellIJK of
                Right (x1, x2, y1, y2, z1, z2) -> Right ((x1+x2)/2, (y1+y2)/2, (z1+z2)/2)
                Left x -> Left x

getCellLocation :: MeshTRNs -> (Int, Int, Int) -> Either String (Double, Double, Double)
getCellLocation trns cellIJK = case getCellXB trns cellIJK of
                Right (x1, x2, y1, y2, z1, z2) -> Right (x1, y1, z1)
                Left x -> Left x

-- getCellAxisLocation trns@(MeshTRNs trnx trny trnz) cell@(i, j, k) = (x, y, z)

parseTRNs = do
    string "TRNX" *> eol
    trnx <- parseTRN
    string "TRNY" *> eol
    trny <- parseTRN
    string "TRNZ" *> eol
    trnz <- parseTRN
    return $ MeshTRNs trnx trny trnz

parseTRN = do
    n <- onlySpaces *> intNum <* eol
    entries <- many (try parseTRNEntry)
    eol
    return entries
    where
        parseTRNEntry :: Parser (Int, Double)
        parseTRNEntry = (,)
            <$> (onlySpaces *> intNum) -- i
            <*> (onlySpaces *> floatNum <* eol) -- v

parseObsts = do
    string "OBST" *> eol
    n <- onlySpaces *> intNum <* eol
    line1s <- count n parseObstLine1
    line2s <- count n parseObstLine2
    eol
    let obsts = zipWith stitchObst line1s line2s -- TODO: actually make this into a useful datatype
    return obsts
    where
        stitchObst ((x1, x2, y1, y2, z1, z2), id, (s1, s2, s3, s4, s5, s6)) ((i1, i2, j1, j2, k1, k2), colorindex, blocktype) = SMVObst
            { smvObstXB = (x1, x2, y1, y2, z1, z2)
            , smvObstId = id
            , smvObstSurfaces = (s1, s2, s3, s4, s5, s6)
            , smvObstIJK = (i1, i2, j1, j2, k1, k2)
            , smvObstColourIndex = colorindex
            , smvObstBlockType = blocktype
            }

data SMVObst = SMVObst
    { smvObstXB :: (Double, Double, Double, Double, Double, Double)
    , smvObstId :: Int  -- This value is not unique
    , smvObstSurfaces :: (Int, Int, Int, Int, Int, Int)
    , smvObstIJK :: (Int, Int, Int, Int, Int, Int)
    , smvObstColourIndex:: Int
    , smvObstBlockType :: Int
    } deriving Show

parseVents = do
    string "VENT" *> eol
    n <- onlySpaces *> intNum
    i <- onlySpaces *> intNum <* eol -- number of dummy vents
    line1s <- count n parseVentLine1
    line2s <- count n parseVentLine2
    optional eol
    let vents = zipWith stitchVent line1s line2s -- TODO: actually make this into a useful datatype
    return vents
    where
        stitchVent ((x1, x2, y1, y2, z1, z2), g, h) (i, j, k, l, m, n, o, p) = SMVVent (x1, x2, y1, y2, z1, z2) g h i j k l m n o p

data SMVVent = SMVVent
    { smvVentXB :: (Double, Double, Double, Double, Double, Double)
    , smvVentUnkown1 :: Int
    , smvVentUnkown2 :: Int
    , smvVentUnkown3 :: Int
    , smvVentUnkown4 :: Int
    , smvVentUnkown5 :: Int
    , smvVentUnkown6 :: Int
    , smvVentUnkown7 :: Int
    , smvVentUnkown8 :: Int
    , smvVentUnkown9 :: Int
    , smvVentUnkown10 :: Int
    } deriving Show


parseObstLine1 = do
    x1 <- onlySpaces *> floatNum
    x2 <- onlySpaces *> floatNum
    y1 <- onlySpaces *> floatNum
    y2 <- onlySpaces *> floatNum
    z1 <- onlySpaces *> floatNum
    z2 <- onlySpaces *> floatNum
    id <- onlySpaces *> intNum
    s1 <- onlySpaces *> intNum
    s2 <- onlySpaces *> intNum
    s3 <- onlySpaces *> intNum
    s4 <- onlySpaces *> intNum
    s5 <- onlySpaces *> intNum
    s6 <- onlySpaces *> intNum
    onlySpaces
    ops <- optionMaybe (do
                opA <- onlySpaces *> floatNum
                opB <- onlySpaces *> floatNum
                opC <- onlySpaces *> floatNum
                onlySpaces
                return (opA, opB, opC)
            )
    eol
    return ((x1, x2, y1, y2, z1, z2), id, (s1, s2, s3, s4, s5, s6))

parseObstLine2 = do
    i1 <- onlySpaces *> intNum
    i2 <- onlySpaces *> intNum
    j1 <- onlySpaces *> intNum
    j2 <- onlySpaces *> intNum
    k1 <- onlySpaces *> intNum
    k2 <- onlySpaces *> intNum
    colorindex <- onlySpaces *> intNum
    blocktype <- onlySpaces *> intNum
    ops <- optionMaybe (do
                opA <- onlySpaces *> floatNum
                opB <- onlySpaces *> floatNum
                opC <- onlySpaces *> floatNum
                opD <- onlySpaces *> floatNum
                return (opA, opB, opC, opD)
            )
    eol

    return ((i1, i2, j1, j2, k1, k2), colorindex, blocktype)


parseVentLine1 = do
    x1 <- onlySpaces *> floatNum
    x2 <- onlySpaces *> floatNum
    y1 <- onlySpaces *> floatNum
    y2 <- onlySpaces *> floatNum
    z1 <- onlySpaces *> floatNum
    z2 <- onlySpaces *> floatNum
    g <- onlySpaces *> intNum
    h <- onlySpaces *> intNum
    optionMaybe (onlySpaces *> floatNum *> onlySpaces *> floatNum *> onlySpaces *> floatNum *> pure ())
    eol

    return ((x1, x2, y1, y2, z1, z2), g, h)



parseVentLine2 = do
    i <- onlySpaces *> intNum
    j <- onlySpaces *> intNum
    k <- onlySpaces *> intNum
    l <- onlySpaces *> intNum
    m <- onlySpaces *> intNum
    n <- onlySpaces *> intNum
    o <- onlySpaces *> intNum
    p <- onlySpaces *> intNum
    ops <- optionMaybe (do
                opA <- onlySpaces *> floatNum
                opB <- onlySpaces *> floatNum
                opC <- onlySpaces *> floatNum
                opD <- onlySpaces *> floatNum
                return (opA, opB, opC, opD)
            )
    eol

    return (i, j, k, l, m, n, o, p)

parseAlias :: Parser String
parseAlias =  char '%' *> manyTill anyChar (try (string " &"))

parseTailEntry :: Parser TailEntry
parseTailEntry = do
    name <- basicString
    case name of
        "SLCF" -> do
                    meshNum <- onlySpaces *> intNum
                    alias <- onlySpaces *> optionMaybe parseAlias
                    clearLine   -- TODO: change
                    -- eol
                    filename <- onlySpaces *> fullString <* eol
                    longName <- onlySpaces *> basicStringSpacesExtra <* eol
                    shortName <- onlySpaces *> basicStringSpacesExtra <* eol
                    units <- onlySpaces *> basicStringExtra
                    -- units' <- optionMaybe basicStringExtra
                    -- let units = case units' of
                            -- Just x -> x
                            -- Nothing -> "unknown"
                    eol
                    return $ DataFileTailEntry $ SLCFDataFile meshNum filename longName shortName units alias
        "SLCC" -> do
                    meshNum <- onlySpaces *> intNum
                    alias <- onlySpaces *> optionMaybe parseAlias
                    clearLine   -- TODO: change
                    -- eol
                    filename <- onlySpaces *> fullString <* eol
                    longName <- onlySpaces *> basicStringSpacesExtra <* eol
                    shortName <- onlySpaces *> basicStringSpacesExtra <* eol
                    units <- onlySpaces *> basicStringExtra
                    -- units' <- optionMaybe basicStringExtra
                    -- let units = case units' of
                            -- Just x -> x
                            -- Nothing -> "unknown"
                    eol
                    return $ DataFileTailEntry $ SLCFDataFile
                      { slcfMeshNum = meshNum
                      , slcfFilename = filename
                      , slcfLongName = longName
                      , slcfShortName = shortName
                      , slcfUnits = units
                      , slcfAlias = alias
                      }
        "BNDF" -> do
                    meshNum <- onlySpaces *> intNum
                    anotherNum <- onlySpaces *> intNum <* eol   -- TODO: find out what this number is and include in datatype
                    filename <- onlySpaces *> fullString <* eol
                    longName <- onlySpaces *> basicStringSpacesExtra <* eol
                    shortName <- onlySpaces *> basicStringSpacesExtra <* eol
                    units <- onlySpaces *> basicStringExtra
                    -- units' <- optionMaybe basicStringExtra
                    -- let units = case units' of
                            -- Just x -> x
                            -- Nothing -> "unknown"
                    eol
                    return $ DataFileTailEntry $ BNDFDataFile meshNum filename longName shortName units
        "BNDC" -> do    -- TODO: BNDF and BNDC are the same
                    meshNum <- onlySpaces *> intNum
                    anotherNum <- onlySpaces *> intNum <* eol   -- TODO: find out what this number is and include in datatype
                    filename <- onlySpaces *> fullString <* eol
                    longName <- onlySpaces *> basicStringSpacesExtra <* eol
                    shortName <- onlySpaces *> basicStringSpacesExtra <* eol
                    units <- onlySpaces *> basicStringExtra
                    -- units' <- optionMaybe basicStringExtra
                    -- let units = case units' of
                            -- Just x -> x
                            -- Nothing -> "unknown"
                    eol
                    return $ DataFileTailEntry $ BNDFDataFile meshNum filename longName shortName units
        "PRT5" -> do
                    meshNum <- onlySpaces *> intNum <* eol
                    filename <- onlySpaces *> fullString <* eol
                    (aNum:bNum:_) <- many1 $ do
                        onlySpaces
                        aNum <- intNum
                        eol
                        return aNum
                    return $ DataFileTailEntry $ PRT5DataFile meshNum filename aNum bNum
        "SMOKE3D" -> do
                    meshNum <- onlySpaces *> intNum <* eol
                    filename <- onlySpaces *> fullString <* eol
                    longName <- onlySpaces *> basicStringSpacesExtra <* eol
                    shortName <- onlySpaces *> basicStringExtra <* eol
                    units <- onlySpaces *> basicStringExtra <* eol
                    return $ DataFileTailEntry $ SMOKE3DDataFile meshNum filename longName shortName units
        "SMOKF3D" -> do
                    meshNum <- onlySpaces *> intNum <* eol
                    filename <- onlySpaces *> fullString <* eol
                    longName <- onlySpaces *> basicStringSpacesExtra <* eol
                    shortName <- onlySpaces *> basicStringExtra <* eol
                    units <- onlySpaces *> basicStringExtra <* eol
                    return $ DataFileTailEntry $ SMOKE3DDataFile meshNum filename longName shortName units
        "PL3D" -> do
                    time <- onlySpaces *> floatNum
                    meshNum <- onlySpaces *> intNum <* eol
                    filename <- onlySpaces *> fullString <* eol
                    pl3dEntries <- many parsePL3DEntry
                    return $ DataFileTailEntry $ PL3DDataFile time meshNum filename pl3dEntries
        "XYZ" -> do
                    eol
                    filename <- onlySpaces *> fullString <* eol
                    return $ DataFileTailEntry $ XYZDataFile filename
        "ISOG" -> do
                    meshNum <- onlySpaces *> intNum <* eol
                    filename <- onlySpaces *> fullString <* eol
                    longName <- onlySpaces *> basicStringSpacesExtra <* eol
                    shortName <- onlySpaces *> basicStringExtra <* eol
                    units <- onlySpaces *> basicStringExtra <* eol
                    return $ DataFileTailEntry $ SMOKE3DDataFile meshNum filename longName shortName units
        "DEVICE_ACT" -> do
                    devcName <- onlySpaces *> deviceNameParser <* eol <* onlySpaces
                    d1 <- intNum <* onlySpaces
                    d2 <- floatNum <* onlySpaces
                    d3 <- intNum <* eol
                    return $ DeviceActivationTailEntry $ DevcActEntry devcName d1 d2 d3
        "SHOW_OBST" -> do
                    k1 <- onlySpaces *> intNum <* eol
                    k2 <- onlySpaces *> intNum
                    k3 <- onlySpaces *> floatNum <* eol
                    return $ ObstVisTailEntry $ ShowObst k1 k2 k3
        "HIDE_OBST" -> do
                    j1 <- onlySpaces *> intNum <* eol
                    j2 <- onlySpaces *> intNum
                    j3 <- onlySpaces *> floatNum <* eol
                    return $ ObstVisTailEntry $ HideObst j1 j2 j3
        "CLOSE_VENT" -> do
                    j1 <- onlySpaces *> intNum <* eol
                    j2 <- onlySpaces *> intNum
                    j3 <- onlySpaces *> floatNum <* eol
                    return $ ObstVisTailEntry $ HideObst j1 j2 j3   --TODO: incorrect entry
        "OPEN_VENT" -> do
                    j1 <- onlySpaces *> intNum <* eol
                    j2 <- onlySpaces *> intNum
                    j3 <- onlySpaces *> floatNum <* eol
                    return $ ObstVisTailEntry $ HideObst j1 j2 j3 --TODO: incorrect entry
        _ -> error $ "The following was parsed by \"parseDataFile\" >" ++ name ++ "<"


sortTailEntries :: [TailEntry] -> ([DataFileEntry], [DevcActEntry], [ObstVisEntry])
sortTailEntries tailEntries = foldl' sortEntry ([], [], []) tailEntries
    where
        sortEntry (dataFileEntries, devActEntries, obstVisEntries) entry =
            case entry of
                DataFileTailEntry x -> (x:dataFileEntries, devActEntries, obstVisEntries)
                DeviceActivationTailEntry x -> (dataFileEntries, x:devActEntries, obstVisEntries)
                ObstVisTailEntry x -> (dataFileEntries, devActEntries, x:obstVisEntries)

data TailEntry
    = DataFileTailEntry DataFileEntry
    | DeviceActivationTailEntry DevcActEntry
    | ObstVisTailEntry ObstVisEntry
    deriving Show

data DataFileEntry   -- TODO: Considere moving each datafile type to its own datatype and making DataFileEntry a Typeclass
    = SLCFDataFile
        { slcfMeshNum   :: Int
        , slcfFilename  :: String
        , slcfLongName  :: String
        , slcfShortName :: String
        , slcfUnits     :: String
        , slcfAlias     :: Maybe String
        }-- n filename longName shortName units
    | BNDFDataFile
        { bndfMeshNum   :: Int
        , bndfFilename  :: String
        , bndfLongName  :: String
        , bndfShortName :: String
        , bndfUnits     :: String
        }-- n filename longName shortName units
    | PRT5DataFile
        { prt5MeshNum   :: Int
        , prt5Filename  :: String
        , prt5aNum      :: Int
        , prt5bNum     :: Int
        }-- n filename longName shortName units
    | SMOKE3DDataFile
        { smoke3dMeshNum   :: Int
        , smoke3dFilename  :: String
        , smoke3dLongName  :: String
        , smoke3dShortName :: String
        , smoke3dUnits     :: String
        }-- n filename longName shortName units Int String String String String -- n filename longName shortName units
    | PL3DDataFile
        { pl3dTime      :: Double
        , pl3dMeshNum   :: Int
        , pl3dFilename  :: String
        , pl3dEntries     :: [PL3DEntry]
        }-- Double Int String [(String, String, String)] -- time n filename [(longName, shortName, units)]
    | XYZDataFile
        { xyzFilename  :: String
        }
    deriving Show

getDataFileName dataFile = case dataFile of
    SLCFDataFile {slcfFilename = filename} -> filename
    BNDFDataFile {bndfFilename = filename} -> filename
    PRT5DataFile {prt5Filename = filename} -> filename
    SMOKE3DDataFile {smoke3dFilename = filename} -> filename
    PL3DDataFile {pl3dFilename = filename} -> filename
    XYZDataFile {xyzFilename = filename} -> filename

data PL3DEntry = PL3DEntry
    { pl3dEntryLongName  :: String
    , pl3dEntryShortName :: String
    , pl3dEntryUnits     :: String
    } deriving Show

-- dataFileMeshNum dataFileEntry = case dataFileEntry of
    -- SLCFDataFile meshNum _ String String String ->
    -- PRT5DataFile meshNum _ Int Int -- n filename aNum bNum
    -- SMOKE3DDataFile meshNum _ String String String -- n filename longName shortName units
    -- PL3DDataFile time meshNum String [(String, String, String)] -- time n filename [(longName, shortName, units)]

data DevcActEntry = DevcActEntry String Int Double Int deriving Show    -- name n time? unknown
data ObstVisEntry
    = ShowObst Int Int Double
    | HideObst Int Int Double
    deriving Show
takeJust (Just x) = x

parsePL3DEntry = do
    -- onlySpaces
    char ' '
    longName <- basicStringSpacesExtra
    eol
    onlySpaces
    shortName <- basicStringExtra
    eol
    onlySpaces
    units <- basicStringExtra
    eol
    return $ PL3DEntry longName shortName units

clearLine = do
    manyTill anyChar eol

basicString :: Parser String
basicString =
    many1 (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._QWERTYUIOPASDFGHJKLZXCVBNM-*")
    <?> "basicString"

basicStringExtra :: Parser String
basicStringExtra =
    many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._QWERTYUIOPASDFGHJKLZXCVBNM/-*")

basicStringSpaces :: Parser String
basicStringSpaces =
    many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._ QWERTYUIOPASDFGHJKLZXCVBNM-*")

basicStringSpacesExtra :: Parser String
basicStringSpacesExtra =
    many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._ QWERTYUIOPASDFGHJKLZXCVBNM/-*")
    <?> "basicStringSpacesExtra"

fullString :: Parser String
fullString =
    many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._, ()QWERTYUIOPASDFGHJKLZXCVBNM/-*")
    <?> "basicStringSpacesExtra"

deviceNameParser :: Parser String
deviceNameParser =
    many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\._, ()QWERTYUIOPASDFGHJKLZXCVBNM/-*+")
    <?> "Device Name"

titleString :: Parser String
titleString =
    manyTill anyChar eol
    <?> "Title String"

basicStringComma :: Parser String
basicStringComma =
    many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm:\\.,_QWERTYUIOPASDFGHJKLZXCVBNM*")

coords :: Parser (Int,Int,Int)
coords = (,,)
    <$> (char '(' *> onlySpaces *> intNum <* sep)
    <*> (intNum <* sep)
    <*> (intNum <* onlySpaces <* char ')')
    <?> "Coordinates (i,j,k)"
    where
        sep :: Parser ()
        sep = onlySpaces *> char ',' *> onlySpaces *> pure ()
