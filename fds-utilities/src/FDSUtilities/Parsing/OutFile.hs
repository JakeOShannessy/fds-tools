-- TODO: this module currently uses v6

module FDSUtilities.Parsing.OutFile
    ( parseOutFile
    -- module FDSUtilities.Parsing.OutFile
    -- , outFileParser
    , parseTimeStep
    , parseValueInt
    , parseValueDouble
    , parseValue
    , evacOutFileParser
    , getCurrentProgressOut
    , CurrentProgress(..)
    , titleParser
    , currentDateParser
    )
    where

import Data.Either
import Data.Time

import FDSUtilities.Types
import FDSUtilities.Parsing.OutFile.Types

import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Token
import FDSUtilities.Parsing.Common
import FDSUtilities.Parsing.Indent

import Data.Char
import Data.Maybe

import Data.Tree
import Data.Either.Utils (forceEither)
-- TODO: High priority, handle spaces in device names
-- TODO: Compile a library of example .out files and build full-scale tests
-- TODO: Implement unit-testing as well.

parseOutFile :: FilePath -> IO (Either ParseError OutData)
parseOutFile filePath = do
    tZone <- getCurrentTimeZone
    input <- readFile filePath
    -- Filter out lines that start from the beginning
    let theLines = filter (\s-> case s of; [] -> True; (s:ss) -> s == ' ';) $ lines input
    let (Node _ parsedForest) = parseIndent $ unlines theLines
    -- TODO: correctly carry through ParseErrors.
    let parsedObjects = mapMaybe (parseObject tZone) parsedForest
    return $ Right $ parsedObjectsToOutData parsedObjects

headParser :: Parser ()
headParser = do
    onlySpaces >> eol
    onlySpaces >> string "Fire Dynamics Simulator" >> eol
    onlySpaces >> eol

    _ <- currentDateParser
    _ <- versionParser
    _ <- revisionParser
    _ <- revisionDateParser
    _ <- compilationDateParser

    emptyLines

    -- mpiStatusParser
    -- openMPStatusParser
    --
    -- emptyLines
    --
    -- mpiVersionParser
    -- mpiLibraryVersionParser
    --
    -- emptyLines
    --
    -- jobTitleParser
    -- jobIdParser

emptyLines :: Parser ()
emptyLines = many eol >> pure ()

currentDateParser :: Parser String
currentDateParser = do
    _ <- onlySpaces
    _ <- string "Current Date"
    _ <- onlySpaces
    _ <- char ':'
    _ <- onlySpaces
    dateString <- many1 (noneOf "\r\n")
    eol
    return dateString
    <?> "Current Date"

versionParser :: Parser Version
versionParser = do
    _ <- onlySpaces
    _ <- string "Version"
    _ <- onlySpaces
    _ <- string ":"
    _ <- onlySpaces
    _ <- optional $ string "FDS"
    _ <- onlySpaces
    version <- parseVersion
    _ <- onlySpaces
    serialOrPar <- many anyChar
    return version
    <?> "FDS Version"

revisionParser :: Parser String
revisionParser = do
    _ <- onlySpaces
    _ <- string "Revision"
    _ <- onlySpaces
    _ <- string ":"
    _ <- onlySpaces
    revisionString <- manyTill anyChar eol
    return revisionString
    <?> "FDS Revision"

revisionDateParser :: Parser String
revisionDateParser = do
    _ <- onlySpaces
    _ <- string "Revision Date"
    _ <- onlySpaces
    _ <- char ':'
    _ <- onlySpaces
    dateString <- manyTill anyChar eol
    return dateString
    <?> "Revision Date"

compilationDateParser :: Parser String
compilationDateParser = do
    _ <- onlySpaces
    _ <- string "Compilation Date"
    _ <- onlySpaces
    _ <- char ':'
    _ <- onlySpaces
    dateString <- manyTill anyChar eol
    return dateString
    <?> "Compilation Date"

parsedObjectsToOutData :: [ParsedObject] -> OutData
parsedObjectsToOutData parsedObjects =
    let
        ParsedVersion version = headErr "parsedObjectsToOutData" $ filter isParsedVersion parsedObjects
        gridDim
            = map (\(ParsedMeshGridDim x) -> x)
            $ filter isParsedMeshGridDim parsedObjects
        physDim
            = map (\(ParsedMeshPhysicalDim x) -> x)
            $ filter isParsedMeshPhysicalDim parsedObjects
        [ParsedMiscParameters mParams] = filter isParsedMiscParameters parsedObjects
        timestepsD = concatMap (\(ParsedTimesteps x) -> x)
                     $ filter isParsedTimesteps parsedObjects

        deviceActivation = case filter isParsedDeviceActivation parsedObjects of
            [] -> Nothing
            [ParsedDeviceActivation d] -> Just d
            _ -> error "too many activations"
        outStatusRes = case filter isParsedOutStatus parsedObjects of
            [] -> Incomplete
            [ParsedOutStatus d] -> d
            _ -> error "too many outres"

        meshDims = combineDims gridDim physDim

    in  OutData
            { outFDSVersion = version
            , mDims = meshDims
            , miscellaneous = mParams
            , timesteps = timestepsD
            , dAct = deviceActivation
            , outStatus = outStatusRes
            }

getCurrentProgressOut :: OutData -> IO CurrentProgress
getCurrentProgressOut outData = do
    let ts = case timesteps outData of
            [] -> error "getCurrentProgressOut: no timesteps"
            xs -> last xs
        lastWall = time ts
        lastSim = simTime ts
        endTime = simEnd $ miscellaneous outData
    currentTime <- getCurrentTime
    return $  CurrentProgress
        { currentProgress_endTime = endTime
        , currentProgress_lastSimTime = lastSim
        , currentProgress_lastWallTime = lastWall
        , currentProgress_currentWallTime = currentTime
        }


data CurrentProgress = CurrentProgress
    { currentProgress_endTime :: Double
    , currentProgress_lastSimTime :: Double
    , currentProgress_lastWallTime :: UTCTime
    , currentProgress_currentWallTime :: UTCTime
    } deriving (Show)

combineDims :: [MeshGridDimensions] -> [MeshPhysicalDimensions] -> [MeshDimensions]
combineDims gridDim physDim  = map (buildMatch gridDim physDim) [1..(length gridDim)]
    where
        buildMatch gridDim physDim i =
            let
                [aGridDim] = filter (\(MeshGridDimensions n _ _ _)-> n == i) gridDim
                (MeshGridDimensions meshNumGrid xCells yCells zCells) = aGridDim
                [aPhysDim] = filter (\(MeshPhysicalDimensions n _ _ _)-> n == i) physDim
                (MeshPhysicalDimensions meshNumPhys xDim yDim zDim) = aPhysDim
            in if meshNumGrid == meshNumPhys
                then MeshDimensions xCells yCells zCells xDim yDim zDim
                else error "Mesh numbers do not match"


meshGridDimensions' :: Tree String -> Either ParseError MeshGridDimensions
meshGridDimensions' (Node val [(Node entries [])]) = do
    meshNum <- parse headerParser "" val
    (xCells, yCells, zCells) <- parse entriesParser "" entries
    return $ MeshGridDimensions meshNum xCells yCells zCells
    where
        headerParser = do
            _ <- string "Grid Dimensions, Mesh"
            _ <- spaces
            meshNum <- intNum
            return meshNum
        entriesParser = do
            _ <- string "Cells in the X Direction"
            spaces
            xCells <- intNum
            eol
            _ <- string "Cells in the Y Direction"
            spaces
            yCells <- intNum
            eol
            _ <- string "Cells in the Z Direction"
            spaces
            zCells <- intNum
            eol
            return (xCells, yCells, zCells)
meshGridDimensions' x = error $ drawTree x

meshPhysicalDimensions' :: Tree String -> Either ParseError MeshPhysicalDimensions
meshPhysicalDimensions' (Node val [(Node entries [])]) = do
    meshNum <- parse headerParser "" val
    (xDim, yDim, zDim) <- parse entriesParser "" entries
    return $ MeshPhysicalDimensions meshNum xDim yDim zDim
    where
        headerParser = do
            _ <- string "Physical Dimensions, Mesh"
            spaces
            meshNum <- intNum
            return meshNum
        entriesParser = do
            _ <- string "Length (m)"
            spaces
            xDim <- floatNum
            eol
            _ <- string "Width  (m)"
            spaces
            yDim <- floatNum
            eol
            _ <- string "Height (m)"
            spaces
            zDim <- floatNum
            eol
            _ <- string "Initial Time Step (s)"
            spaces
            initTimeStep <- floatNum
            eol
            return (xDim,yDim,zDim)
meshPhysicalDimensions' (Node val _) = error "invalid number of node entries"

runTimeParser' :: TimeZone -> Tree String -> Either ParseError [TimeStep]
runTimeParser' tZone (Node val [(Node entries [])]) = parse entriesParser "" entries
    where
        entriesParser = many $ parseTimeStep tZone
runTimeParser' _ (Node val _) = error "invalid number of node entries"

runTimeParserRestart' :: TimeZone -> Tree String -> Maybe ParsedObject
runTimeParserRestart' tZone (Node val []) = Nothing
runTimeParserRestart' tZone (Node val [(Node entries [])])
    = Just
    $ ParsedTimesteps
    $ forceEither
    $ parse entriesParser "" entries
    where
        entriesParser = many $ parseTimeStep tZone
runTimeParserRestart' _ (Node val _) = error "invalid number of node entries"

titleParser :: Parser ()
titleParser = do
    _ <- onlySpaces
    eol
    _ <- onlySpaces
    _ <- string "Fire Dynamics Simulator"
    eol


fdsVersionParser' :: Tree String -> Either ParseError Version
fdsVersionParser' (Node val []) = parse versionParser "" val
    where
        versionParser = do
            _ <- string "Version"
            _ <- onlySpaces
            _ <- string ":"
            _ <- onlySpaces
            _ <- optional $ string "FDS"
            _ <- onlySpaces
            version <- parseVersion
            _ <- onlySpaces
            serialOrPar <- many anyChar
            return version
            <?> "FDS version"
fdsVersionParser' (Node val _) = error "invalid number of node entries"

outStatusParser' :: Tree [Char] -> Either ParseError OutStatus
outStatusParser' (Node val []) = parse theParser "" val
    where
        theParser = choice [parseStoppedByUser, parseCompleted, parseNumericalInstability]
            where
                parseStoppedByUser = try (string "STOP: FDS stopped by user") >> return StoppedByUser
                parseCompleted = try (string "STOP: FDS completed successfully") >> (return Completed)
                parseNumericalInstability = try (string "STOP: Numerical Instability") >> return NumericalInstability
                -- parseNumericalInstability = try (string "STOP: FDS was improperly set-up") >> return NumericalInstability
                -- parseNumericalInstability = try (string "STOP: Set-up only") >> return NumericalInstability
                -- parseNumericalInstability = try (string "STOP: FDS was stopped by KILL control function") >> return NumericalInstability
outStatusParser' (Node val _) = error "invalid number of node entries"

parseObject :: TimeZone -> Tree [Char] -> Maybe ParsedObject
parseObject tZone node@(Node val subNodes)
    | (take 5 val) == "Versi" = Just
                $ ParsedVersion
                $ forceEither $ fdsVersionParser' node
    | (take 5 val) == "Grid " = Just
                $ ParsedMeshGridDim
                $ forceEither $ meshGridDimensions' node
    | (take 5 val) == "Physi" = Just
                $ ParsedMeshPhysicalDim
                $ forceEither $ meshPhysicalDimensions' node
    | (take 5 val) == "Misce" = Just
                $ ParsedMiscParameters
                $ forceEither $ miscParameters' node
    | (take 5 val) == "Run T" = Just
                $ ParsedTimesteps
                $ forceEither $ runTimeParser' tZone node
    | (take 5 val) == "Job I" = runTimeParserRestart' tZone node
    | (take 5 val) == "DEVIC" = Just
                $ ParsedDeviceActivation
                $ forceEither $ devcActTimes' node
    | (take 5 val) == "STOP:" = Just
                $ ParsedOutStatus
                $ forceEither $ outStatusParser' node

    | otherwise = Nothing

-- devcActTimes' :: Parser [(Int, String, DevcActivation)]
devcActTimes' :: Tree [Char]
    -> Either ParseError [(Int, String, DevcActivation)]
devcActTimes' (Node val [(Node entries [])]) = parse entriesParser "" entries
    where
        entriesParser = do
            acts <- many (try devActTime)
            return acts
devcActTimes' (Node val _) = error "invalid number of node entries"

devcActTimes :: Parser [(Int, String, DevcActivation)]
devcActTimes = do
    _ <- string " DEVICE Activation Times"
    eol
    eol
    acts <- many (try devActTime)
    eol
    eol
    return acts

devActTime :: Parser (Int, String, DevcActivation)
devActTime = do
    _ <- onlySpaces
    number <- intNum
    _ <- onlySpaces
    -- name <- basicString
    -- onlySpaces
    name <- manyTill (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm_. -QWERTYUIOPASDFGHJKLZXCVBNM") (lookAhead devcActivationValue)
    activation <- devcActivationValue
    eol
    return (number, name, activation)
    -- where
        -- parseActTime = try $

data ParsedObject
    = ParsedVersion Version
    | ParsedMeshGridDim MeshGridDimensions
    | ParsedMeshPhysicalDim MeshPhysicalDimensions
    | ParsedMiscParameters MiscParameters
    | ParsedTimesteps [TimeStep]
    | ParsedDeviceActivation [(Int, String, DevcActivation)]
    | ParsedOutStatus OutStatus
    deriving (Show)

isParsedVersion :: ParsedObject -> Bool
isParsedVersion (ParsedVersion _) = True
isParsedVersion _ = False

isParsedMeshGridDim :: ParsedObject -> Bool
isParsedMeshGridDim (ParsedMeshGridDim _) = True
isParsedMeshGridDim _ = False

isParsedMeshPhysicalDim :: ParsedObject -> Bool
isParsedMeshPhysicalDim (ParsedMeshPhysicalDim _) = True
isParsedMeshPhysicalDim _ = False

isParsedMiscParameters :: ParsedObject -> Bool
isParsedMiscParameters (ParsedMiscParameters _) = True
isParsedMiscParameters _ = False

isParsedTimesteps :: ParsedObject -> Bool
isParsedTimesteps (ParsedTimesteps _) = True
isParsedTimesteps _ = False

isParsedDeviceActivation :: ParsedObject -> Bool
isParsedDeviceActivation (ParsedDeviceActivation _) = True
isParsedDeviceActivation _ = False

isParsedOutStatus :: ParsedObject -> Bool
isParsedOutStatus (ParsedOutStatus _) = True
isParsedOutStatus _ = False

outFileParserSetUp :: Parser OutData
outFileParserSetUp = do
    eol
    _ <- string "Stop FDS, Set-up only"
    eol
    fail "Set-up only"

miscParameters' :: Tree String -> Either ParseError MiscParameters
miscParameters' (Node val [(Node entries [])]) = parse theParser "" entries
    where
        theParser = do
            _ <- string "Simulation Start Time (s)"
            _ <- spaces
            simStartTime <- floatNum
            eol
            _ <- spaces
            _ <- string "Simulation End Time (s)"
            _ <- spaces
            simEndTime <- floatNum
            eol
            _ <- spaces
            _ <- string "LES Calculation"
            eol
            _ <- choice [try smagorinskyConstant, try deardorffModel
                , (try $ do
                    dConst <- string "Eddy Viscosity:" *> onlySpaces *> string "Deardorff Model (C_DEARDORFF = " *> floatNum <* string ")" <* eol
                    sConst <- string "Near-wall Eddy Viscosity:" *> onlySpaces *> string "Smagorinsky with Van Driest damping (C_SMAGORINSKY = " *> floatNum <* string ")" <* eol
                    let (a,b) = (dConst, sConst) :: (Double, Double )
                    pure ())
                , (try $ do
                    string "Eddy Viscosity:           Deardorff Model (C_DEARDORFF)                          0.10" >> eol
                    string "Near-wall Eddy Viscosity: Smagorinsky with Van Driest damping (C_SMAGORINSKY)    0.20" >> eol
                    pure ())
                ]

            _ <- spaces
            _ <- string "Turbulent Prandtl Number"
            _ <- spaces
            turbPrandtl <- floatNum
            eol
            _ <- spaces
            _ <- optionMaybe $ (do
                _ <- string "Turbulent Schmidt Number"
                _ <- spaces
                _ <- floatNum
                _ <- spaces
                pure ()
                )
            _ <- string "Ambient Temperature (C)"
            _ <- spaces
            ambTemp <- floatNum
            eol

            return $ MiscParameters simStartTime simEndTime
miscParameters' (Node val _) = error "invalid number of node entries"

supportedVersions :: [Version]
supportedVersions =
    [ Version 5 5 3
    , Version 5 5 0
    , Version 6 0 0
    , Version 6 0 1
    , Version 6 1 0
    , Version 6 1 1
    , Version 6 3 2
    ]

-- TODO: when uncommented overloads the GHC simplifier
evacOutFileParser :: Parser EvacOutData
evacOutFileParser = do
    -- eol
    -- string " FDS+Evac Evacuation Module"
    -- eol
    -- eol
    -- compilationDateString <- compilationDateParserEvac
    -- fdsVersionString <- fdsVersionParserEvac
    -- svnRevisionNoString <- svnRevisionNoParserEvac

    -- -- eol
    -- -- clearLine -- color method
    -- -- clearLine -- vis_door-Crit
    -- -- clearLine -- EFF file
    -- -- eol
    -- -- clearLine -- these are per mesh (Evac)
    -- -- eol
    -- -- many initLineParser -- init
    -- -- eol
    -- -- clearLine
    -- -- clearLine
    -- -- eol
    -- manyTill clearLine (try (do string " EVAC: Initial positions of the agents"; eol;))
    -- initialAgentProps <- evacInitialAgentsParser
    -- eol
    -- eol
    -- agentExitProps <- evacAgentExitParser
    -- eof
    -- -- return initialAgentProps
    -- return $ EvacOutData EvacVersionInformation EvacMiscParams EvacMeshParams initialAgentProps agentExitProps
    return $ EvacOutData
        { evacVersionInformation = EvacVersionInformation
        , evacMiscParams = EvacMiscParams
        , evacMeshParams = EvacMeshParams
        , evacInitialAgentProps = []
        , evacExitAgentProps = []
        }

initLineParser :: Parser String
initLineParser = do
    _ <- string " INIT:"
    clearLine

completionStatusParser :: Parser OutStatus
completionStatusParser = do --this is shoddy, redo completely
    -- val <- try (string "STOP: FDS stopped by user") <|> try (string "STOP: FDS completed successfully") <|> try (string "STOP: FDS completed successfully")
    -- return $ case var of
                -- "STOP: FDS stopped by user" -> StoppedByUser
                -- "STOP: FDS completed successfully" -> Completed
                -- "STOP: Numerical Instability" -> NumericalInstability
    -- return $ if val == "STOP: FDS stopped by user" then StoppedByUser else Completed
    choice [parseStoppedByUser, parseCompleted, parseNumericalInstability]
    where
        parseStoppedByUser = try (string "STOP: FDS stopped by user") >> return StoppedByUser
        parseCompleted = try (string "STOP: FDS completed successfully") >> (return Completed)
        parseNumericalInstability = try (string "STOP: Numerical Instability") >> return NumericalInstability
        -- parseNumericalInstability = try (string "STOP: FDS was improperly set-up") >> return NumericalInstability
        -- parseNumericalInstability = try (string "STOP: Set-up only") >> return NumericalInstability
        -- parseNumericalInstability = try (string "STOP: FDS was stopped by KILL control function") >> return NumericalInstability

evacInitialAgentsParser :: Parser [InitialAgentProps]
evacInitialAgentsParser = do
    -- string " EVAC: Initial positions of the agents"
    -- eol
    _ <- onlySpaces
    _ <- string "Agent"
    _ <- onlySpaces
    _ <- string "X"
    _ <- onlySpaces
    _ <- string "Y"
    _ <- onlySpaces
    _ <- string "Z"
    _ <- onlySpaces
    _ <- string "Tpre"
    _ <- onlySpaces
    _ <- string "Tdet"
    _ <- onlySpaces
    _ <- string "Dia"
    _ <- onlySpaces
    _ <- string "V0"
    _ <- onlySpaces
    _ <- string "Tau"
    _ <- onlySpaces
    _ <- string "I_gr"
    _ <- onlySpaces
    _ <- string "I_ff"
    eol
    initialAgentProps <- many (try evacInitialSingleAgentPropParser)
    return initialAgentProps

evacAgentExitParser :: Parser [AgentExitProps]
evacAgentExitParser = many evacAgentExitSingleParser

evacAgentExitSingleParser :: Parser AgentExitProps
evacAgentExitSingleParser = do
    _ <- onlySpaces
    _ <- string "Agent"
    _ <- onlySpaces
    _ <- string "n:o"
    _ <- onlySpaces
    agentNumber <- intNum
    _ <- onlySpaces
    action <- (try (string "counted at")) <|> (try (string "out at"))   <?> "agent action"
    _ <- onlySpaces
    exitTime <- floatNum
    _ <- onlySpaces
    _ <- string "s, exit "
    exitName <- basicString
    _ <- string ", FED="
    _ <- onlySpaces
    fed <- floatNum
    _ <- string ","
    _ <- onlySpaces
    _ <- string "Color_i="
    _ <- onlySpaces
    colorA <- intNum    -- TODO: work out what these are
    _ <- onlySpaces
    colorB <- intNum
    _ <- onlySpaces
    colorC <- intNum
    eol
    return $ AgentExitProps agentNumber exitTime exitName

evacInitialSingleAgentPropParser :: Parser InitialAgentProps
evacInitialSingleAgentPropParser = do
    _ <- onlySpaces
    agentNumber <- intNum
    _ <- onlySpaces
    xPos <- floatNum
    _ <- onlySpaces
    yPos <- floatNum
    _ <- onlySpaces
    zPos <- floatNum
    _ <- onlySpaces
    tPre <- floatNum
    _ <- onlySpaces
    tDet <- floatNum
    _ <- onlySpaces
    dia <- floatNum
    _ <- onlySpaces
    v0 <- floatNum
    _ <- onlySpaces
    tau <- floatNum
    _ <- onlySpaces
    i_gr <- intNum
    _ <- onlySpaces
    i_ff <- intNum
    _ <- onlySpaces
    _ <- intNum
    eol
    return $ InitialAgentProps agentNumber (xPos, yPos, zPos) tPre tDet dia v0 tau i_gr i_ff

cpuUsageParser :: Parser ()
cpuUsageParser = do
    _ <- string " CPU Time Usage, Mesh"
    _ <- onlySpaces
    meshNum <- intNum
    eol
    eol
    _ <- onlySpaces
    _ <- string "CPU "
    _ <- string "(s)" <|> string "s"
    _ <- onlySpaces
    _ <- string "%"
    _ <- onlySpaces
    eol
    _ <- string "       ------------------------"
    optional $ string "----"
    eol
    _ <- routineParser "MAIN"
    _ <- routineParser "DIVG"
    _ <- routineParser "MASS"
    _ <- routineParser "VELO"
    _ <- routineParser "PRES"
    _ <- routineParser "WALL"
    _ <- routineParser "DUMP"
    _ <- routineParser "PART"
    _ <- routineParser "RADI"
    _ <- routineParser "FIRE"
    _ <- routineParser "COMM"
    _ <- optionMaybe (try (do
        _ <- routineParser "EVAC"
        _ <- routineParser "FOR"
        _ <- routineParser "P2P"
        _ <- routineParser "MOV"
        pure ()
        ))
    _ <- optionMaybe $ try (do
        _ <- onlySpaces
        _ <- string "SubTot"
        _ <- onlySpaces
        cpuSeconds <- floatNum
        _ <- onlySpaces
        cpuPercentage <- floatNum
        eol
        )

    eol
    eol
    where
        routineParser routine = do
            _ <- onlySpaces
            _ <- string routine
            _ <- onlySpaces
            cpuSeconds <- floatNum
            _ <- onlySpaces
            cpuPercentage <- floatNum
            eol
            return (routine, cpuSeconds, cpuPercentage)

compilationDateParserEvac :: Parser String
compilationDateParserEvac = do
    _ <- string " FDS+Evac Compilation Date"
    _ <- spaces
    _ <- string ":"
    _ <- spaces
    compilationDateString <- manyTill anyChar eol
    return compilationDateString

fdsVersionParser :: Parser Version
fdsVersionParser = do
    _ <- string " Version"
    _ <- onlySpaces
    _ <- string ":"
    _ <- onlySpaces
    _ <- optional $ string "FDS"
    _ <- onlySpaces
    version <- parseVersion
    _ <- onlySpaces
    serialOrPar <- manyTill anyChar eol
    return version
    <?> "FDS version"

fdsVersionParserEvac :: Parser String
fdsVersionParserEvac = do
    _ <- string " FDS+Evac Version"
    _ <- spaces
    _ <- string ":"
    _ <- spaces
    fdsVersionString <- manyTill anyChar eol
    return fdsVersionString

openMPStatusParser :: Parser String
openMPStatusParser = do
    _ <- string " OpenMP"
    _ <- spaces
    openMPStatusString <- manyTill anyChar eol
    return openMPStatusString

openThreadNParser :: Parser Int
openThreadNParser = do
    _ <- string " Number of OpenMP threads:"
    _ <- onlySpaces
    n <- intNum
    eol
    return n

svnRevisionNoParser :: Parser String
svnRevisionNoParser = do
    _ <- string " SVN Revision No."
    _ <- spaces
    _ <- string ":"
    _ <- spaces
    svnRevisionNoString <- manyTill anyChar eol
    return svnRevisionNoString

svnRevisionNoParserEvac :: Parser String
svnRevisionNoParserEvac = do
    _ <- string " FDS+Evac SVN Revision No."
    _ <- spaces
    _ <- string ":"
    _ <- spaces
    svnRevisionNoString <- manyTill anyChar eol
    return svnRevisionNoString

jobTitleParser :: Parser String
jobTitleParser = do
    _ <- string " Job TITLE"
    _ <- spaces
    _ <- string ":"
    _ <- onlySpaces
    jobTitle <- manyTill anyChar eol
    return jobTitle

jobCHIDParser :: Parser String
jobCHIDParser = do
    _ <- string " Job ID string"
    _ <- spaces
    _ <- string ":"
    _ <- spaces
    jobCHID <- manyTill anyChar eol
    return jobCHID






devcActivationValue :: Parser DevcActivation
devcActivationValue = do
    try (do {_ <- string "No Activation"; pure NoActivation;})
    <|> try (do {d <- floatNum; _ <- space; _ <- string "s";pure (DevcActivationTime d)})

miscParameters :: Parser MiscParameters
miscParameters = do
    _ <- string " Miscellaneous Parameters"
    eol
    spaces
    _ <- string "Simulation Start Time (s)"
    spaces
    simStartTime <- floatNum
    eol
    spaces
    _ <- string "Simulation End Time (s)"
    spaces
    simEndTime <- floatNum
    eol
    spaces
    _ <- string "LES Calculation"
    eol
    choice [try smagorinskyConstant, try deardorffModel]


    spaces
    _ <- string "Turbulent Prandtl Number"
    spaces
    turbPrandtl <- floatNum
    eol
    spaces
    _ <- optionMaybe $ (do
        _ <- string "Turbulent Schmidt Number"
        _ <- spaces
        _ <- floatNum
        _ <- spaces
        pure ()
        )
    _ <- string "Ambient Temperature (C)"
    spaces
    ambTemp <- floatNum
    eol

    return $ MiscParameters simStartTime simEndTime

smagorinskyConstant :: Parser ()
smagorinskyConstant = do
    spaces
    _ <- string "Smagorinsky Constant"
    spaces
    smagConst <- floatNum
    eol

deardorffModel :: Parser ()
deardorffModel = do
    _ <- onlySpaces
    _ <- string "Deardorff Model"
    _ <- optionMaybe (do
        _ <- string " (C_DEARDORFF)"
        _ <- onlySpaces
        v <- floatNum
        return v)
    eol

meshDimensions :: Parser MeshDimensions
meshDimensions = do
    _ <- string " Grid Dimensions, Mesh"
    spaces
    meshNum <- intNum
    eol
    eol
    _ <- string "   Cells in the X Direction"
    spaces
    xCells <- intNum
    eol
    _ <- string "   Cells in the Y Direction"
    spaces
    yCells <- intNum
    eol
    _ <- string "   Cells in the Z Direction"
    spaces
    zCells <- intNum
    eol
    eol
    eol
    _ <- string " Physical Dimensions, Mesh"
    spaces
    meshNumAgain <- intNum
    eol
    eol
    _ <- string "   Length (m)"
    spaces
    xDim <- floatNum
    eol
    _ <- string "   Width  (m)"
    spaces
    yDim <- floatNum
    eol
    _ <- string "   Height (m)"
    spaces
    zDim <- floatNum
    eol
    _ <- string "   Initial Time Step (s)"
    spaces
    initTimeStep <- floatNum
    eol
    eol
    return $ MeshDimensions xCells yCells zCells xDim yDim zDim

clearLine :: Parser String
clearLine = do
    manyTill anyChar eol

-- parseTimeStepV5 :: TimeZone -> Parser TimeStep
-- parseTimeStepV5 tZone = do
--     onlySpaces
--     string "Time Step"
--     onlySpaces
--     stepNumber <- many digit
--     onlySpaces
--     timeString <- manyTill anyChar eol
--     onlySpaces
--     string "----------------------------------------------"
--     eol
--     meshes <- many (try parseMeshStep)
--     onlySpaces
--     eol
--     let stepTime = localTimeToUTC tZone (parseTimeOrError True defaultTimeLocale "%B %e, %Y  %T" timeString)
--     return $ TimeStep (read stepNumber) stepTime meshes

meshLine :: Parser (Int, Int)
meshLine = do
    _ <- try (do { spaces; string "Mesh";})
    spaces
    number <- intNum
    cycleNumMaybe <- optionMaybe $ do
        _ <- char ','
        spaces
        _ <- string "Cycle"
        spaces
        intNum
    eol
    return (number, fromMaybe 0 cycleNumMaybe)

data RestartInfo = RestartInfo
    { restartInfo_compileDate :: String
    , restartInfo_version :: String
    , restartInfo_openMPStatus :: String
    , restartInfo_svnRevision :: Int
    , restartInfo_jobTitle :: String
    , restartInfo_chid :: String
    }

-- parseTimeStepMulti :: TimeZone -> Parser (Either RestartInfo TimeStep)
-- parseTimeStepMulti tZone = do
    -- try (do
        -- tStep <- parseTimeStep tZone
        -- return $ Right tStep)
    -- <|> (do
        -- restartInfo <- parseRestartInfo
        -- return $ Left restartInfo)

-- parseRestartInfo = do
    -- headInfo <- parseHead
    -- return $ RestartInfo
        -- { restartInfo_compileDate = "undefined"
        -- , restartInfo_version = "undefined"
        -- , restartInfo_openMPStatus = "undefined"
        -- , restartInfo_svnRevision = 1234
        -- , restartInfo_jobTitle = "undefined"
        -- , restartInfo_chid = "undefined"
        -- }

parseStepProp :: Parser [StepProp]
parseStepProp = do
    _ <- onlySpaces
    a <- optionMaybe $ lookAhead $ try (string "Time Step")
    case a of
        Just x -> unexpected "not step prop"
        _ -> do
            firstProp <- parseSingleProp
            secondPropMaybe <- optionMaybe $ try $ do
                _ <- onlySpaces
                _ <- char ','
                _ <- onlySpaces
                parseSingleProp
            _ <- onlySpaces
            eol
            pure $ case secondPropMaybe of
                Nothing ->  [firstProp]
                Just secondProp -> [firstProp, secondProp]

parseSingleProp :: Parser StepProp
parseSingleProp = do
    key <- many1 (noneOf "-\r\n:,")
    _ <- char ':'
    _ <- onlySpaces
    value <- parseValue
    _ <- onlySpaces
    units <- optionMaybe $ try $ do
        a <- optionMaybe $ lookAhead $ string "at"
        b <- optionMaybe $ lookAhead $ string "on"
        case (a,b) of
            (Just "at", _) -> unexpected "not units"
            (_, Just "on") -> unexpected "not units"
            _ -> parseUnits
    _ <- onlySpaces
    location <- optionMaybe $ try $ do
        meshNum <- optionMaybe $ try (do
            _ <- string "on"
            _ <- onlySpaces
            _ <- string "Mesh"
            _ <- onlySpaces
            n <- intNum
            _ <- onlySpaces
            return n
            <?> "Mesh Number")
        _ <- string "at"
        _ <- onlySpaces
        coords <- intCoords
        return $ Location
            { locationMesh = meshNum
            , locationCoords = coords
            }
    pure $ StepProp
        { stepPropKey = key
        , stepPropValue = value
        , stepPropUnits = units
        , stepPropLocation = location
        }

-- TODO: currently, parsing an Int will never succeed
parseValue :: Parser Value
parseValue = choice [try parseValueDouble, try parseValueInt]

parseValueInt :: Parser Value
parseValueInt = do
    int <- intNum
    return $ ValueInt int
    <?> "ValueInt"

parseValueDouble :: Parser Value
parseValueDouble = do
    double <- floatNum
    return $ ValueDouble double
    <?> "ValueDouble"

parseUnits :: Parser String
parseUnits = many1 (satisfy (\c->
    isAscii c
    && not (isControl c)
    && not (isSpace c)
    && not (c == ',')))

intCoords :: Parser (Int, Int, Int)
intCoords = P.between (char '(')  (char ')') (do
    _ <- onlySpaces
    i <- intNum
    _ <- onlySpaces >> optional (char ',') >> onlySpaces
    j <- intNum
    _ <- onlySpaces >> optional (char ',') >> onlySpaces
    k <- intNum
    _ <- onlySpaces
    return (i, j, k)
    <?> "Location")

parseTimeStep :: TimeZone -> Parser TimeStep
parseTimeStep tZone = do
    _ <- onlySpaces
    _ <- string "Time Step"
    _ <- onlySpaces
    stepNumber <- intNum
    _ <- onlySpaces
    timeString <- manyTill anyChar eol

    timeInfo <- optionMaybe $ try $ parseTimeInfo

    props <- concat <$> many1 parseStepProp

    many1 (char '-') >> eol

    meshes <- many1 (try parseMeshStep)

    let (stepSize, totalTime) = case timeInfo of
            Just x -> x
            Nothing ->
                let (mesh:_) = meshes
                    props = meshStepProps mesh
                    totalTime =
                        let [prop] = filter (\x-> stepPropKey x == "Total time") props
                            ValueDouble dVal = stepPropValue prop
                        in dVal
                    stepSize =
                        let [prop] = filter (\x-> stepPropKey x == "Total step") props
                            ValueDouble dVal = stepPropValue prop
                        in dVal
                in (stepSize, totalTime)

    -- onlySpaces
    -- eol -- the splitting is now done by an earlier parse pass
    let stepTime = localTimeToUTC tZone (parseTimeOrError True defaultTimeLocale "%B %e, %Y  %T" timeString)
    return $ TimeStep
        { num = stepNumber
        , time = stepTime
        , stepSize = stepSize
        , simTime = totalTime
        , timeStepStepProps = props
        , timeStepMeshes' = meshes
        }

parseTimeInfo :: Parser (Double, Double)
parseTimeInfo =  do
    _ <- onlySpaces
    _ <- string "Step Size:"
    _ <- onlySpaces
    stepSize <- floatNum
    _ <- onlySpaces
    _ <- char 's'
    _ <- char ','
    _ <- onlySpaces
    _ <- string "Total Time:"
    _ <- onlySpaces
    totalTime <- floatNum
    _ <- onlySpaces
    _ <- char 's'
    eol
    pure (stepSize, totalTime)

parseMeshStep :: Parser MeshStep
parseMeshStep = do
    meshL <- optionMaybe $ try $ do
        _ <- string "Mesh"
        _ <- onlySpaces
        n <- intNum
        _ <- onlySpaces
        _ <- optionMaybe $ try $ do
            _ <- char ','
            _ <- onlySpaces
            _ <- string "Cycle"
            _ <- onlySpaces
            cycle <- intNum
            _ <- onlySpaces
            pure cycle
        eol
        pure n
    let meshNumber = case meshL of
            Nothing -> 1
            Just n -> n
    properties <- concat <$> many1 (try parseStepProp)
    return $ MeshStep meshNumber properties

totalHRR :: Parser Double
totalHRR = do
    _ <- try (do { spaces; string "Total Heat Release Rate:";})
    _ <- spaces
    hrr <- floatNum
    _ <- string " kW"
    eol
    return hrr

poisPert :: Parser Double
poisPert = do
    _ <- try (do { spaces; string "Poisson Pert. :";})
    _ <- onlySpaces
    pp <- floatNum
    eol
    return pp

maxVNNumber :: Parser ()
maxVNNumber = do
    _ <- try (do { spaces; _ <- string "Max VN"; _ <- onlySpaces; string "number:";})
    _ <- onlySpaces
    minDivVal <- floatNum
    _ <- string " at "
    minDivCoord <- coords
    eol

maxDivError :: Parser Double
maxDivError = do
    _ <- try (do { spaces; string "Max div. error:";})
    _ <- spaces
    minDivVal <- floatNum
    _ <- string " at "
    minDivCoord <- coords
    eol
    return minDivVal

numberOfParticles :: Parser Double
numberOfParticles = do
    _ <- try (do { spaces; string "No. of Lagrangian Particles:";})
    _ <- spaces
    nParts <- floatNum
    eol
    return nParts

maxHRRPUV :: Parser Double
maxHRRPUV = do
    _ <- try (do { spaces; string "Max HRRPUV:";})
    _ <- onlySpaces
    nParts <- floatNum
    _ <- onlySpaces
    _ <- string "kW/m^3"
    eol
    return nParts

parseRLTB :: Parser Double
parseRLTB = do -- radiation loss to boundaries
    _ <- try (do { spaces; string "Radiation Loss to Boundaries:";})
    spaces
    rltb <- floatNum
    _ <- string " kW"
    eol
    return rltb



basicString :: Parser String
basicString = do
    str <- many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm._QWERTYUIOPASDFGHJKLZXCVBNM")
    return str

basicStringSpaces :: Parser String
basicStringSpaces = do
    str <- many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm. _QWERTYUIOPASDFGHJKLZXCVBNM()")
    return str

basicStringComma :: Parser String
basicStringComma = do
    str <- many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm.,_QWERTYUIOPASDFGHJKLZXCVBNM()")
    return str

basicStringCommaSpaces :: Parser String
basicStringCommaSpaces = do
    str <- many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm., _QWERTYUIOPASDFGHJKLZXCVBNM()")
    return str

coords :: Parser (Int,Int,Int)
coords = do
    _ <- char '('
    spaces
    x <- intNum
    _ <- char ','
    spaces
    y <- intNum
    _ <- char ','
    spaces
    z <- intNum
    _ <- char ')'
    return (x, y, x)

eolString :: Parser String
eolString = do
    res <- oneOf "\n\r"
    return $ show res


caseCHID :: Parser String
caseCHID = do
    _ <- string " Job ID String       : "
    many anyChar

caseTitle :: Parser String
caseTitle = do
    _ <- string " Job TITLE        : "
    many anyChar

parseSpeciesInformation :: Parser ()
parseSpeciesInformation = do
    _ <- onlySpaces
    _ <- string "Primitive Species Information"
    eol
    eol
    specs <- many (try parseSpecies)
    eol
    where
        parseSpecies = do
            _ <- onlySpaces
            name <- basicStringSpaces
            _ <- onlySpaces
            eol
            species <- try parseGasSpecies <|> try parseMixtureFractionVariable

            _ <- onlySpaces
            _ <- string "Initial Mass Fraction"
            _ <- onlySpaces
            initMassFrac <- floatNum
            eol
            eol
            return (species, initMassFrac)

        parseGasSpecies = do
            _ <- onlySpaces
            _ <- string "Gas Species"
            eol
            optional $ try (do; _ <- onlySpaces; _ <- string "Background Species"; eol)
            _ <- onlySpaces
            _ <- string "Molecular Weight (g/mol)"
            _ <- onlySpaces
            molWeight <- floatNum
            eol
        parseMixtureFractionVariable = do
            _ <- onlySpaces
            _ <- string "Mixture Fraction Variable"
            eol



parseGasPhaseReactionInformation :: Parser [()]
parseGasPhaseReactionInformation = do
    _ <- onlySpaces
    _ <- string "Gas Phase Reaction Information"
    eol
    eol
    gPhaseReacs <- many (try parseGasPhaseReaction)
    eol
    return gPhaseReacs
    where
        parseGasPhaseReaction = do
            _ <- onlySpaces
            reacName <- basicStringSpaces
            eol
            _ <- onlySpaces
            _ <- string "Mixture Fraction Reaction"  -- TODO: there are other reaction models to be parsed
            eol

            -- onlySpaces
            -- fyi <- basicStringCommaSpaces
            -- eol
            let parseMolWeight = do
                    _ <- onlySpaces
                    _ <- string "Molecular Weight, Fuel (g/mol)"
                    _ <- onlySpaces
                    molWeight <- floatNum
                    eol

            try parseMolWeight <|> (do; _ <- clearLine; parseMolWeight)

            _ <- onlySpaces
            _ <- string "Heat of Combustion (kJ/kg)"
            _ <- onlySpaces
            hoc <- floatNum
            eol

            _ <- onlySpaces
            _ <- string "Stoich. Coeff., O_2"
            _ <- onlySpaces
            vO2 <- floatNum
            eol

            _ <- onlySpaces
            _ <- string "Stoich. Coeff., CO_2"
            _ <- onlySpaces
            vCO2 <- floatNum
            eol

            _ <- onlySpaces
            _ <- string "Stoich. Coeff., H2O"
            _ <- onlySpaces
            vH2O <- floatNum
            eol

            _ <- onlySpaces
            _ <- string "Stoich. Coeff., Soot"
            _ <- onlySpaces
            vSoot <- floatNum
            eol

            vCO <- optionMaybe (try (do
                        _ <- onlySpaces
                        _ <- string "Stoich. Coeff., CO"
                        _ <- onlySpaces
                        vCO <- floatNum
                        eol
                        return vCO
                        ))

            _ <- onlySpaces
            _ <- string "Stoich. Coeff., N_2"
            _ <- onlySpaces
            vN2 <- floatNum
            eol

            vOther <- optionMaybe (try (do
                        _ <- onlySpaces
                        _ <- string "Stoich. Coeff., Other"
                        _ <- onlySpaces
                        vCO <- floatNum
                        eol
                        return vCO
                        ))

            _ <- onlySpaces
            _ <- string "Stoichiometric Value of Z"
            _ <- onlySpaces
            vZ <- floatNum
            eol
            eol
