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
    let (Node _ parsedForest) = parseIndent input
    -- TODO: correctly carry through ParseErrors.
    let parsedObjects = mapMaybe (parseObject tZone) parsedForest
    return $ Right $ parsedObjectsToOutData parsedObjects

headParser = do
    onlySpaces >> eol
    onlySpaces >> string "Fire Dynamics Simulator" >> eol
    onlySpaces >> eol

    currentDateParser
    versionParser
    revisionParser
    revisionDateParser
    compilationDateParser

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

emptyLines = many eol

currentDateParser :: Parser String
currentDateParser = do
    onlySpaces
    string "Current Date"
    onlySpaces
    char ':'
    onlySpaces
    dateString <- many1 (noneOf "\r\n")
    eol
    return dateString
    <?> "Current Date"

versionParser = do
    onlySpaces
    string "Version"
    onlySpaces
    string ":"
    onlySpaces
    optional $ string "FDS"
    onlySpaces
    version <- parseVersion
    onlySpaces
    serialOrPar <- many anyChar
    return version
    <?> "FDS Version"

revisionParser :: Parser String
revisionParser = do
    onlySpaces
    string "Revision"
    onlySpaces
    string ":"
    onlySpaces
    revisionString <- manyTill anyChar eol
    return revisionString
    <?> "FDS Revision"

revisionDateParser :: Parser String
revisionDateParser = do
    onlySpaces
    string "Revision Date"
    onlySpaces
    char ':'
    onlySpaces
    dateString <- manyTill anyChar eol
    return dateString
    <?> "Revision Date"

compilationDateParser :: Parser String
compilationDateParser = do
    onlySpaces
    string "Compilation Date"
    onlySpaces
    char ':'
    onlySpaces
    dateString <- manyTill anyChar eol
    return dateString
    <?> "Compilation Date"

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
            [ParsedDeviceActivation d] -> Just d
            [] -> Nothing
        outStatusRes = case filter isParsedOutStatus parsedObjects of
            [ParsedOutStatus d] -> d
            [] -> Incomplete

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


-- meshDimensions' :: Parser MeshDimensions
meshGridDimensions' (Node val [(Node entries [])]) = do
    meshNum <- parse headerParser "" val
    (xCells, yCells, zCells) <- parse entriesParser "" entries
    return $ MeshGridDimensions meshNum xCells yCells zCells
    where
        headerParser = do
            string "Grid Dimensions, Mesh"
            spaces
            meshNum <- intNum
            return meshNum
        entriesParser = do
            string "Cells in the X Direction"
            spaces
            xCells <- intNum
            eol
            string "Cells in the Y Direction"
            spaces
            yCells <- intNum
            eol
            string "Cells in the Z Direction"
            spaces
            zCells <- intNum
            eol
            return (xCells, yCells, zCells)
meshGridDimensions' x = error $ drawTree x

meshPhysicalDimensions' (Node val [(Node entries [])]) = do
    meshNum <- parse headerParser "" val
    (xDim, yDim, zDim) <- parse entriesParser "" entries
    return $ MeshPhysicalDimensions meshNum xDim yDim zDim
    where
        headerParser = do
            string "Physical Dimensions, Mesh"
            spaces
            meshNum <- intNum
            return meshNum
        entriesParser = do
            string "Length (m)"
            spaces
            xDim <- floatNum
            eol
            string "Width  (m)"
            spaces
            yDim <- floatNum
            eol
            string "Height (m)"
            spaces
            zDim <- floatNum
            eol
            string "Initial Time Step (s)"
            spaces
            initTimeStep <- floatNum
            eol
            return (xDim,yDim,zDim)

runTimeParser' tZone (Node val [(Node entries [])]) = parse entriesParser "" entries
    where
        entriesParser = many $ parseTimeStep tZone

runTimeParserRestart' tZone (Node val []) = Nothing
runTimeParserRestart' tZone (Node val [(Node entries [])])
    = Just
    $ ParsedTimesteps
    $ forceEither
    $ parse entriesParser "" entries
    where
        entriesParser = many $ parseTimeStep tZone

titleParser = do
    onlySpaces
    eol
    onlySpaces
    string "Fire Dynamics Simulator"
    eol


fdsVersionParser' (Node val []) = parse versionParser "" val
    where
        versionParser = do
            string "Version"
            onlySpaces
            string ":"
            onlySpaces
            optional $ string "FDS"
            onlySpaces
            version <- parseVersion
            onlySpaces
            serialOrPar <- many anyChar
            return version
            <?> "FDS version"

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
devcActTimes' (Node val [(Node entries [])]) = parse entriesParser "" entries
    where
        entriesParser = do
            acts <- many (try devActTime)
            return acts


devcActTimes :: Parser [(Int, String, DevcActivation)]
devcActTimes = do
    string " DEVICE Activation Times"
    eol
    eol
    acts <- many (try devActTime)
    eol
    eol
    return acts

devActTime :: Parser (Int, String, DevcActivation)
devActTime = do
    onlySpaces
    number <- intNum
    onlySpaces
    -- name <- basicString
    -- onlySpaces
    name <- manyTill (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm_. -QWERTYUIOPASDFGHJKLZXCVBNM") (lookAhead devcActivationValue)
    activation <- devcActivationValue
    eol
    return (number, name, activation)
    -- where
        -- parseActTime = try $

data MeshGridDimensions = MeshGridDimensions
    Int -- ^MeshNum
    Int -- ^x
    Int -- ^y
    Int -- ^z
    deriving (Show)

data MeshPhysicalDimensions = MeshPhysicalDimensions
    Int -- ^MeshNum
    Double -- ^x
    Double -- ^y
    Double -- ^z
    deriving (Show)

data ParsedObject
    = ParsedVersion Version
    | ParsedMeshGridDim MeshGridDimensions
    | ParsedMeshPhysicalDim MeshPhysicalDimensions
    | ParsedMiscParameters MiscParameters
    | ParsedTimesteps [TimeStep]
    | ParsedDeviceActivation [(Int, String, DevcActivation)]
    | ParsedOutStatus OutStatus
    deriving (Show)

isParsedVersion (ParsedVersion _) = True
isParsedVersion _ = False

isParsedMeshGridDim (ParsedMeshGridDim _) = True
isParsedMeshGridDim _ = False

isParsedMeshPhysicalDim (ParsedMeshPhysicalDim _) = True
isParsedMeshPhysicalDim _ = False

isParsedMiscParameters (ParsedMiscParameters _) = True
isParsedMiscParameters _ = False

isParsedTimesteps (ParsedTimesteps _) = True
isParsedTimesteps _ = False

isParsedDeviceActivation (ParsedDeviceActivation _) = True
isParsedDeviceActivation _ = False

isParsedOutStatus (ParsedOutStatus _) = True
isParsedOutStatus _ = False



outFileParserSetUp :: Parser OutData
outFileParserSetUp = do
    eol
    string "Stop FDS, Set-up only"
    eol
    fail "Set-up only"

-- miscParameters' :: Parser MiscParameters
miscParameters' (Node val [(Node entries [])]) = parse theParser "" entries
    where
        theParser = do
            string "Simulation Start Time (s)"
            spaces
            simStartTime <- floatNum
            eol
            spaces
            string "Simulation End Time (s)"
            spaces
            simEndTime <- floatNum
            eol
            spaces
            string "LES Calculation"
            eol
            choice [try smagorinskyConstant, try deardorffModel]

            spaces
            string "Turbulent Prandtl Number"
            spaces
            turbPrandtl <- floatNum
            eol
            spaces
            optionMaybe $ (do
                string "Turbulent Schmidt Number"
                spaces
                floatNum
                spaces
                )
            string "Ambient Temperature (C)"
            spaces
            ambTemp <- floatNum
            eol

            return $ MiscParameters simStartTime simEndTime

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

initLineParser = do
    string " INIT:"
    clearLine

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

evacInitialAgentsParser = do
    -- string " EVAC: Initial positions of the agents"
    -- eol
    onlySpaces
    string "Agent"
    onlySpaces
    string "X"
    onlySpaces
    string "Y"
    onlySpaces
    string "Z"
    onlySpaces
    string "Tpre"
    onlySpaces
    string "Tdet"
    onlySpaces
    string "Dia"
    onlySpaces
    string "V0"
    onlySpaces
    string "Tau"
    onlySpaces
    string "I_gr"
    onlySpaces
    string "I_ff"
    eol
    initialAgentProps <- many (try evacInitialSingleAgentPropParser)
    return initialAgentProps

evacAgentExitParser = do
    many evacAgentExitSingleParser

evacAgentExitSingleParser = do
    onlySpaces
    string "Agent"
    onlySpaces
    string "n:o"
    onlySpaces
    agentNumber <- intNum
    onlySpaces
    action <- (try (string "counted at")) <|> (try (string "out at"))   <?> "agent action"
    onlySpaces
    exitTime <- floatNum
    onlySpaces
    string "s, exit "
    exitName <- basicString
    string ", FED="
    onlySpaces
    fed <- floatNum
    string ","
    onlySpaces
    string "Color_i="
    onlySpaces
    colorA <- intNum    -- TODO: work out what these are
    onlySpaces
    colorB <- intNum
    onlySpaces
    colorC <- intNum
    eol
    return $ AgentExitProps agentNumber exitTime exitName

evacInitialSingleAgentPropParser = do
    onlySpaces
    agentNumber <- intNum
    onlySpaces
    xPos <- floatNum
    onlySpaces
    yPos <- floatNum
    onlySpaces
    zPos <- floatNum
    onlySpaces
    tPre <- floatNum
    onlySpaces
    tDet <- floatNum
    onlySpaces
    dia <- floatNum
    onlySpaces
    v0 <- floatNum
    onlySpaces
    tau <- floatNum
    onlySpaces
    i_gr <- intNum
    onlySpaces
    i_ff <- intNum
    onlySpaces
    intNum
    eol
    return $ InitialAgentProps agentNumber (xPos, yPos, zPos) tPre tDet dia v0 tau i_gr i_ff



cpuUsageParser = do
    string " CPU Time Usage, Mesh"
    onlySpaces
    meshNum <- intNum
    eol
    eol
    onlySpaces
    string "CPU "
    string "(s)" <|> string "s"
    onlySpaces
    string "%"
    onlySpaces
    eol
    string "       ------------------------"
    optional $ string "----"
    eol
    routineParser "MAIN"
    routineParser "DIVG"
    routineParser "MASS"
    routineParser "VELO"
    routineParser "PRES"
    routineParser "WALL"
    routineParser "DUMP"
    routineParser "PART"
    routineParser "RADI"
    routineParser "FIRE"
    routineParser "COMM"
    optionMaybe (try (do
        routineParser "EVAC"
        routineParser "FOR"
        routineParser "P2P"
        routineParser "MOV"
        ))
    optionMaybe $ try (do
        onlySpaces
        string "SubTot"
        onlySpaces
        cpuSeconds <- floatNum
        onlySpaces
        cpuPercentage <- floatNum
        eol
        )

    eol
    eol
    where
        routineParser routine = do
            onlySpaces
            string routine
            onlySpaces
            cpuSeconds <- floatNum
            onlySpaces
            cpuPercentage <- floatNum
            eol
            return (routine, cpuSeconds, cpuPercentage)

compilationDateParserEvac = do
    string " FDS+Evac Compilation Date"
    spaces
    string ":"
    spaces
    compilationDateString <- manyTill anyChar eol
    return compilationDateString

fdsVersionParser = do
    string " Version"
    onlySpaces
    string ":"
    onlySpaces
    optional $ string "FDS"
    onlySpaces
    version <- parseVersion
    onlySpaces
    serialOrPar <- manyTill anyChar eol
    return version
    <?> "FDS version"

fdsVersionParserEvac = do
    string " FDS+Evac Version"
    spaces
    string ":"
    spaces
    fdsVersionString <- manyTill anyChar eol
    return fdsVersionString

openMPStatusParser = do
    string " OpenMP"
    spaces
    openMPStatusString <- manyTill anyChar eol
    return openMPStatusString

openThreadNParser = do
    string " Number of OpenMP threads:"
    onlySpaces
    n <- intNum
    eol
    return n

svnRevisionNoParser = do
    string " SVN Revision No."
    spaces
    string ":"
    spaces
    svnRevisionNoString <- manyTill anyChar eol
    return svnRevisionNoString

svnRevisionNoParserEvac = do
    string " FDS+Evac SVN Revision No."
    spaces
    string ":"
    spaces
    svnRevisionNoString <- manyTill anyChar eol
    return svnRevisionNoString

jobTitleParser = do
    string " Job TITLE"
    spaces
    string ":"
    onlySpaces
    jobTitle <- manyTill anyChar eol
    return jobTitle

jobCHIDParser = do
    string " Job ID string"
    spaces
    string ":"
    spaces
    jobCHID <- manyTill anyChar eol
    return jobCHID






devcActivationValue :: Parser DevcActivation
devcActivationValue = do
    try (do {string "No Activation"; return NoActivation;})
    <|> try (do {d <- floatNum; space; string "s"; return (DevcActivationTime d)})



miscParameters :: Parser MiscParameters
miscParameters = do
    string " Miscellaneous Parameters"
    eol
    spaces
    string "Simulation Start Time (s)"
    spaces
    simStartTime <- floatNum
    eol
    spaces
    string "Simulation End Time (s)"
    spaces
    simEndTime <- floatNum
    eol
    spaces
    string "LES Calculation"
    eol
    choice [try smagorinskyConstant, try deardorffModel]


    spaces
    string "Turbulent Prandtl Number"
    spaces
    turbPrandtl <- floatNum
    eol
    spaces
    optionMaybe $ (do
        string "Turbulent Schmidt Number"
        spaces
        floatNum
        spaces
        )
    string "Ambient Temperature (C)"
    spaces
    ambTemp <- floatNum
    eol

    return $ MiscParameters simStartTime simEndTime

smagorinskyConstant = do
    spaces
    string "Smagorinsky Constant"
    spaces
    smagConst <- floatNum
    eol
deardorffModel = do
    onlySpaces
    string "Deardorff Model"
    _ <- optionMaybe (do
        string " (C_DEARDORFF)"
        onlySpaces
        v <- floatNum
        return v)
    eol

meshDimensions :: Parser MeshDimensions
meshDimensions = do
    string " Grid Dimensions, Mesh"
    spaces
    meshNum <- intNum
    eol
    eol
    string "   Cells in the X Direction"
    spaces
    xCells <- intNum
    eol
    string "   Cells in the Y Direction"
    spaces
    yCells <- intNum
    eol
    string "   Cells in the Z Direction"
    spaces
    zCells <- intNum
    eol
    eol
    eol
    string " Physical Dimensions, Mesh"
    spaces
    meshNumAgain <- intNum
    eol
    eol
    string "   Length (m)"
    spaces
    xDim <- floatNum
    eol
    string "   Width  (m)"
    spaces
    yDim <- floatNum
    eol
    string "   Height (m)"
    spaces
    zDim <- floatNum
    eol
    string "   Initial Time Step (s)"
    spaces
    initTimeStep <- floatNum
    eol
    eol
    return $ MeshDimensions xCells yCells zCells xDim yDim zDim

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

meshLine = do
    try (do { spaces; string "Mesh";})
    spaces
    number <- intNum
    cycleNumMaybe <- optionMaybe $ do
        char ','
        spaces
        string "Cycle"
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
    onlySpaces
    a <- optionMaybe $ lookAhead $ try (string "Time Step")
    case a of
        Just x -> unexpected "not step prop"
        _ -> do
            firstProp <- parseSingleProp
            secondPropMaybe <- optionMaybe $ try $ do
                onlySpaces
                char ','
                onlySpaces
                parseSingleProp
            onlySpaces
            eol
            pure $ case secondPropMaybe of
                Nothing ->  [firstProp]
                Just secondProp -> [firstProp, secondProp]

parseSingleProp = do
    key <- many1 (noneOf "-\r\n:,")
    char ':'
    onlySpaces
    value <- parseValue
    onlySpaces
    units <- optionMaybe $ try $ do
        a <- optionMaybe $ lookAhead $ string "at"
        b <- optionMaybe $ lookAhead $ string "on"
        case (a,b) of
            (Just "at", _) -> unexpected "not units"
            (_, Just "on") -> unexpected "not units"
            _ -> parseUnits
    onlySpaces
    location <- optionMaybe $ try $ do
        meshNum <- optionMaybe $ try (do
            string "on"
            onlySpaces
            string "Mesh"
            onlySpaces
            n <- intNum
            onlySpaces
            return n
            <?> "Mesh Number")
        string "at"
        onlySpaces
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
parseValue = choice [try parseValueDouble, try parseValueInt]

parseValueInt = do
    int <- intNum
    return $ ValueInt int
    <?> "ValueInt"

parseValueDouble = do
    double <- floatNum
    return $ ValueDouble double
    <?> "ValueDouble"

parseUnits = many1 (satisfy (\c->
    isAscii c
    && not (isControl c)
    && not (isSpace c)
    && not (c == ',')))

intCoords = P.between (char '(')  (char ')') (do
    onlySpaces
    i <- intNum
    onlySpaces >> optional (char ',') >> onlySpaces
    j <- intNum
    onlySpaces >> optional (char ',') >> onlySpaces
    k <- intNum
    onlySpaces
    return (i, j, k)
    <?> "Location")

parseTimeStep :: TimeZone -> Parser TimeStep
parseTimeStep tZone = do
    onlySpaces
    string "Time Step"
    onlySpaces
    stepNumber <- intNum
    onlySpaces
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

parseTimeInfo =  do
    onlySpaces
    string "Step Size:"
    onlySpaces
    stepSize <- floatNum
    onlySpaces
    char 's'
    char ','
    onlySpaces
    string "Total Time:"
    onlySpaces
    totalTime <- floatNum
    onlySpaces
    char 's'
    eol
    pure (stepSize, totalTime)

parseMeshStep :: Parser MeshStep
parseMeshStep = do
    meshL <- optionMaybe $ try $ do
        string "Mesh"
        onlySpaces
        n <- intNum
        onlySpaces
        _ <- optionMaybe $ try $ do
            char ','
            onlySpaces
            string "Cycle"
            onlySpaces
            cycle <- intNum
            onlySpaces
            pure cycle
        eol
        pure n
    let meshNumber = case meshL of
            Nothing -> 1
            Just n -> n
    properties <- concat <$> many1 (try parseStepProp)
    return $ MeshStep meshNumber properties

totalHRR = do
    try (do { spaces; string "Total Heat Release Rate:";})
    spaces
    hrr <- floatNum
    string " kW"
    eol
    return hrr

poisPert = do
    try (do { spaces; string "Poisson Pert. :";})
    onlySpaces
    pp <- floatNum
    eol
    return pp

maxVNNumber = do
    try (do { spaces; string "Max VN"; onlySpaces; string "number:";})
    onlySpaces
    minDivVal <- floatNum
    string " at "
    minDivCoord <- coords
    eol

maxDivError = do
    try (do { spaces; string "Max div. error:";})
    spaces
    minDivVal <- floatNum
    string " at "
    minDivCoord <- coords
    eol
    return minDivVal

numberOfParticles = do
    try (do { spaces; string "No. of Lagrangian Particles:";})
    spaces
    nParts <- floatNum
    eol
    return nParts

maxHRRPUV = do
    try (do { spaces; string "Max HRRPUV:";})
    onlySpaces
    nParts <- floatNum
    onlySpaces
    string "kW/m^3"
    eol
    return nParts

parseRLTB = do -- radiation loss to boundaries
    try (do { spaces; string "Radiation Loss to Boundaries:";})
    spaces
    rltb <- floatNum
    string " kW"
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
    char '('
    spaces
    x <- intNum
    char ','
    spaces
    y <- intNum
    char ','
    spaces
    z <- intNum
    char ')'
    return (x, y, x)



eolString :: Parser String
eolString = do
    res <- oneOf "\n\r"
    return $ show res


caseCHID :: Parser String
caseCHID = do
    string " Job ID String       : "
    many anyChar

caseTitle :: Parser String
caseTitle = do
    string " Job TITLE        : "
    many anyChar


parseSpeciesInformation = do
    onlySpaces
    string "Primitive Species Information"
    eol
    eol
    specs <- many (try parseSpecies)
    eol
    where
        parseSpecies = do
            onlySpaces
            name <- basicStringSpaces
            onlySpaces
            eol
            species <- try parseGasSpecies <|> try parseMixtureFractionVariable

            onlySpaces
            string "Initial Mass Fraction"
            onlySpaces
            initMassFrac <- floatNum
            eol
            eol
            return (species, initMassFrac)

        parseGasSpecies = do
            onlySpaces
            string "Gas Species"
            eol
            optional $ try (do; onlySpaces; string "Background Species"; eol)
            onlySpaces
            string "Molecular Weight (g/mol)"
            onlySpaces
            molWeight <- floatNum
            eol
        parseMixtureFractionVariable = do
            onlySpaces
            string "Mixture Fraction Variable"
            eol



parseGasPhaseReactionInformation = do
    onlySpaces
    string "Gas Phase Reaction Information"
    eol
    eol
    gPhaseReacs <- many (try parseGasPhaseReaction)
    eol
    return gPhaseReacs
    where
        parseGasPhaseReaction = do
            onlySpaces
            reacName <- basicStringSpaces
            eol
            onlySpaces
            string "Mixture Fraction Reaction"  -- TODO: there are other reaction models to be parsed
            eol

            -- onlySpaces
            -- fyi <- basicStringCommaSpaces
            -- eol
            let parseMolWeight = do
                    onlySpaces
                    string "Molecular Weight, Fuel (g/mol)"
                    onlySpaces
                    molWeight <- floatNum
                    eol

            try parseMolWeight <|> (do; clearLine; parseMolWeight)

            onlySpaces
            string "Heat of Combustion (kJ/kg)"
            onlySpaces
            hoc <- floatNum
            eol

            onlySpaces
            string "Stoich. Coeff., O_2"
            onlySpaces
            vO2 <- floatNum
            eol

            onlySpaces
            string "Stoich. Coeff., CO_2"
            onlySpaces
            vCO2 <- floatNum
            eol

            onlySpaces
            string "Stoich. Coeff., H2O"
            onlySpaces
            vH2O <- floatNum
            eol

            onlySpaces
            string "Stoich. Coeff., Soot"
            onlySpaces
            vSoot <- floatNum
            eol

            vCO <- optionMaybe (try (do
                        onlySpaces
                        string "Stoich. Coeff., CO"
                        onlySpaces
                        vCO <- floatNum
                        eol
                        return vCO
                        ))

            onlySpaces
            string "Stoich. Coeff., N_2"
            onlySpaces
            vN2 <- floatNum
            eol

            vOther <- optionMaybe (try (do
                        onlySpaces
                        string "Stoich. Coeff., Other"
                        onlySpaces
                        vCO <- floatNum
                        eol
                        return vCO
                        ))

            onlySpaces
            string "Stoichiometric Value of Z"
            onlySpaces
            vZ <- floatNum
            eol

            eol
