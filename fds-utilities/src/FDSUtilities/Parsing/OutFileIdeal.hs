{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo    #-}
module FDSUtilities.Parsing.OutFileIdeal
    ( parseOutFile
    , unknownBlockParser
    , colonPairValueString
    , Block(..)
    )
    where

import Data.Either
import Data.Time

import FDSUtilities.Types

import FDSUtilities.Parsing.CommonIdeal
import Text.Parsec
import Text.Parsec.Indent hiding (withBlock, block)
import FDSUtilities.Parsing.OutFile.Types

import Data.Char
import Data.Maybe

-- TODO: High priority, handle spaces in device names
-- TODO: Compile a library of example .out files and build full-scale tests
-- TODO: Implement unit-testing as well.

parseOutFile :: FilePath -> IO (Either ParseError ([(String,String)],[Block]))
parseOutFile filePath = do
    tZone <- getCurrentTimeZone
    input <- readFile filePath
    return $ runIndentParser (outFileParser tZone) () filePath input

-- | @ 'withBlock' f a p @ parses @ a @
--   followed by an indented block of @ p @
--   combining them with @ f @
withBlock
    :: (Monad m, Stream s (IndentT m) z)
    => (a -> [b] -> c)
    -> IndentParserT s u m a
    -> IndentParserT s u m b
    -> IndentParserT s u m c
withBlock f a p = withPos $ do
    r1 <- a
    r2 <- option [] (indented >> block p)
    return (f r1 r2)

-- withBlockSingle
--     :: (Monad m, Stream s (IndentT m) z)
--     => (a -> b -> c)
--     -> IndentParserT s u m a
--     -> IndentParserT s u m b
--     -> IndentParserT s u m c
-- withBlockSingle f a p = withPos $ do
--     r1 <- a
--     r2 <- (indented >> blockSingle p)
--     return (f r1 r2)

-- -- | Parses until column is less than reference
-- blockSingle
--     :: (Monad m, Stream s (IndentT m) z)
--     => IndentParserT s u m a
--     -> IndentParserT s u m a
-- blockSingle p = unlines <$> many1 (indented >> p)


-- | Parses a block of lines at a higher indentation level
block
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m a
    -> IndentParserT s u m [a]
block p = many1 (indented >> p)
-- TODO: preserve the indentation level

outFileParser :: TimeZone -> IndentParser String () ([(String,String)],[Block])
outFileParser tZone = do
    titleParser
    colonPairs <- many (try (colonPairParser colonPairValueString <* emptyLines))
    spaces
    blocks <- many1 (choice (blockParsers tZone) <|> unknownBlockParser)
    -- colonPairs <- many (try (colonPairParser colonPairValueString <* emptyLines))
    eof
    return (colonPairs, blocks)


emptyLines :: (Monad m) => ParsecT String u m ()
emptyLines = skipMany eol

blockParsers tZone =
    [ (runTimeBlockParser tZone)
    ]

titleParser =
    onlySpaces *> eol
    *> onlySpaces *> string "Fire Dynamics Simulator" *> eol
    *> onlySpaces *> eol

-- each top level element in an out file is either a key:value pair or a block

-- colonPairParser :: Parser a -> Parser (String, a)
colonPairParser valueParser = try ((,)
    <$> (onlySpaces *> colonPairKeyString)
    <* (onlySpaces *> char ':' <* onlySpaces)
    <*> (valueParser <* onlySpaces <* eol))
    <?> "<Key>:<Value>"

data Block
    = UnknownBlock String [String]
    | RunTimeBlock [TimeStep]
    deriving (Show)

unknownBlockParser :: IndentParser String () Block
unknownBlockParser =
    withBlock
        UnknownBlock
        (colonPairValueString <* spaces)
        (colonPairValueString <* spaces)

runTimeBlockParser :: TimeZone -> IndentParser String () Block
runTimeBlockParser tZone =
    withBlock
        (const RunTimeBlock)
        (try (string "Run Time Diagnostics" <* spaces))
        (parseTimeStep tZone <* spaces)

colonPairKeyString =
    many1 (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm@.+%-;, _QWERTYUIOPASDFGHJKLZXCVBNM/^()")
    <?> "Key"

-- colonPairValueString :: Parser String
colonPairValueString =
    many1 (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm@.:+%-;, _QWERTYUIOPASDFGHJKLZXCVBNM/^()")
    <?> "Value"

-- fdsVersionParser = do
--     string " Version"
--     onlySpaces
--     string ":"
--     onlySpaces
--     (optional (string "FDS"))
--     onlySpaces
--     version <- parseVersion <* onlySpaces <* eol
--     return version
--     <?> "FDS version"

-- currentDateParser :: Parser String
-- currentDateParser = do
--     onlySpaces
--     string "Current Date"
--     onlySpaces
--     char ':'
--     onlySpaces
--     dateString <- many1 (noneOf "\r\n")
--     eol
--     return dateString
--     <?> "Current Date"

-- revisionParser :: Parser String
-- revisionParser = do
--     onlySpaces
--     string "Revision"
--     onlySpaces
--     string ":"
--     onlySpaces
--     revisionString <- manyTill anyChar eol
--     return revisionString
--     <?> "FDS Revision"

-- revisionDateParser :: Parser String
-- revisionDateParser = do
--     onlySpaces
--     string "Revision Date"
--     onlySpaces
--     char ':'
--     onlySpaces
--     dateString <- manyTill anyChar eol
--     return dateString
--     <?> "Revision Date"

-- compilationDateParser :: Parser String
-- compilationDateParser = do
--     onlySpaces
--     string "Compilation Date"
--     onlySpaces
--     char ':'
--     onlySpaces
--     dateString <- manyTill anyChar eol
--     return dateString
--     <?> "Compilation Date"

-- emptyLines = many eol

-- meshDimensions :: Parser MeshDimensions
-- meshDimensions = do
--     gridDims <- meshGridDimensions
--     eol
--     eol
--     physDims <- meshPhysicalDimensions
--     eol
--     return $
--         let
--             -- TODO: use the mesh num as an index
--             MeshGridDimensions meshNumGrid xCells yCells zCells =  gridDims
--             MeshPhysicalDimensions meshNumPhys xDim yDim zDim = physDims
--         in if meshNumGrid == meshNumPhys
--             then MeshDimensions xCells yCells zCells xDim yDim zDim
--             else error "Mesh numbers do not match"
--     <?> "Mesh Dimensions"

-- meshGridDimensions :: Parser MeshGridDimensions
-- meshGridDimensions = do
--     meshNum <- headerParser
--     eol
--     (xCells, yCells, zCells) <- entriesParser
--     return $ MeshGridDimensions meshNum xCells yCells zCells
--     where
--         headerParser = do
--             onlySpaces
--             string "Grid Dimensions, Mesh"
--             spaces
--             meshNum <- intNum
--             eol
--             return meshNum
--         entriesParser = do
--             onlySpaces
--             string "Cells in the X Direction"
--             spaces
--             xCells <- intNum
--             eol
--             onlySpaces
--             string "Cells in the Y Direction"
--             spaces
--             yCells <- intNum
--             eol
--             onlySpaces
--             string "Cells in the Z Direction"
--             spaces
--             zCells <- intNum
--             eol
--             return (xCells, yCells, zCells)

-- meshPhysicalDimensions :: Parser MeshPhysicalDimensions
-- meshPhysicalDimensions = do
--     meshNum <- headerParser
--     eol
--     (xDim, yDim, zDim) <- entriesParser
--     return $ MeshPhysicalDimensions meshNum xDim yDim zDim
--     where
--         headerParser = do
--             onlySpaces
--             string "Physical Dimensions, Mesh"
--             spaces
--             meshNum <- intNum
--             eol
--             return meshNum
--         entriesParser = do
--             onlySpaces
--             string "Length (m)"
--             spaces
--             xDim <- floatNum
--             eol
--             onlySpaces
--             string "Width  (m)"
--             spaces
--             yDim <- floatNum
--             eol
--             onlySpaces
--             string "Height (m)"
--             spaces
--             zDim <- floatNum
--             eol
--             onlySpaces
--             string "Initial Time Step (s)"
--             spaces
--             initTimeStep <- floatNum
--             eol
--             return (xDim,yDim,zDim)

-- miscParameters :: Parser MiscParameters
-- miscParameters = do
--     onlySpaces >> string "Miscellaneous Parameters" >> eol
--     eol

--     onlySpaces
--     string "Simulation Start Time (s)"
--     onlySpaces
--     simStartTime <- floatNum
--     eol

--     onlySpaces
--     string "Simulation End Time (s)"
--     onlySpaces
--     simEndTime <- floatNum
--     eol
--     onlySpaces
--     string "LES Calculation"
--     eol
--     choice [try smagorinskyConstant, try deardorffModel]

--     onlySpaces
--     string "Turbulent Prandtl Number"
--     onlySpaces
--     turbPrandtl <- floatNum
--     eol
--     onlySpaces
--     optionMaybe $ (do
--         string "Turbulent Schmidt Number"
--         spaces
--         floatNum
--         spaces
--         )
--     string "Ambient Temperature (C)"
--     spaces
--     ambTemp <- floatNum
--     eol

--     return $ MiscParameters simStartTime simEndTime


-- massFractionsTransMatrixParser = do
--     onlySpaces
--     string "Mass Fraction Transformation Matrix to Convert Species Mixtures (Columns) to Primitive Species (Rows)" >> eol
--     eol
--     mixtureSpecies <- onlySpaces >> speciesMixturesParser
--     primitiveRows <- many1 (primitiveRowParser (length mixtureSpecies))
--     eol
--     eol

-- speciesMixturesParser :: Parser [String]
-- speciesMixturesParser = many1 (basicString <* onlySpaces)

-- primitiveRowParser :: Int -> Parser (String, [Double])
-- primitiveRowParser n = try $ do
--     onlySpaces
--     species <- manyTill (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm_. -QWERTYUIOPASDFGHJKLZXCVBNM") (lookAhead floatNum)
--     fractions <- count n floatNum
--     eol
--     return (species, fractions)
--     -- TODO: this
--     -- (basicString <* onlySpaces)

-- primSpeciesParser = do
--     -- TODO: this now has much more info
--     onlySpaces
--     string "Primitive Species Information"
--     eol
--     eol
--     specs <- many (try parseSpecies)
--     eol
--     where
--         parseSpecies = do
--             onlySpaces
--             name <- basicStringSpaces
--             onlySpaces
--             eol
--             species <- try parseGasSpecies <|> try parseMixtureFractionVariable

--             onlySpaces
--             string "Initial Mass Fraction"
--             onlySpaces
--             initMassFrac <- floatNum
--             eol
--             eol
--             return (species, initMassFrac)

--         parseGasSpecies = do
--             onlySpaces
--             string "Gas Species"
--             eol
--             optional $ try (do; onlySpaces; string "Background Species"; eol)
--             onlySpaces
--             string "Molecular Weight (g/mol)"
--             onlySpaces
--             molWeight <- floatNum
--             eol
--         parseMixtureFractionVariable = do
--             onlySpaces
--             string "Mixture Fraction Variable"
--             eol

-- trackedSpeciesParser = undefined

-- gasReactionParser = do
--     onlySpaces
--     string "Gas Phase Reaction Information"
--     eol
--     eol
--     gPhaseReacs <- many (try parseGasPhaseReaction)
--     eol
--     return gPhaseReacs
--     where
--         parseGasPhaseReaction = do
--             onlySpaces
--             reacName <- basicStringSpaces
--             eol
--             onlySpaces
--             string "Mixture Fraction Reaction"  -- TODO: there are other reaction models to be parsed
--             eol

--             -- onlySpaces
--             -- fyi <- basicStringCommaSpaces
--             -- eol
--             let parseMolWeight = do
--                     onlySpaces
--                     string "Molecular Weight, Fuel (g/mol)"
--                     onlySpaces
--                     molWeight <- floatNum
--                     eol

--             try parseMolWeight <|> (do; clearLine; parseMolWeight)

--             onlySpaces
--             string "Heat of Combustion (kJ/kg)"
--             onlySpaces
--             hoc <- floatNum
--             eol

--             onlySpaces
--             string "Stoich. Coeff., O_2"
--             onlySpaces
--             vO2 <- floatNum
--             eol

--             onlySpaces
--             string "Stoich. Coeff., CO_2"
--             onlySpaces
--             vCO2 <- floatNum
--             eol

--             onlySpaces
--             string "Stoich. Coeff., H2O"
--             onlySpaces
--             vH2O <- floatNum
--             eol

--             onlySpaces
--             string "Stoich. Coeff., Soot"
--             onlySpaces
--             vSoot <- floatNum
--             eol

--             vCO <- optionMaybe (try (do
--                         onlySpaces
--                         string "Stoich. Coeff., CO"
--                         onlySpaces
--                         vCO <- floatNum
--                         eol
--                         return vCO
--                         ))

--             onlySpaces
--             string "Stoich. Coeff., N_2"
--             onlySpaces
--             vN2 <- floatNum
--             eol

--             vOther <- optionMaybe (try (do
--                         onlySpaces
--                         string "Stoich. Coeff., Other"
--                         onlySpaces
--                         vCO <- floatNum
--                         eol
--                         return vCO
--                         ))

--             onlySpaces
--             string "Stoichiometric Value of Z"
--             onlySpaces
--             vZ <- floatNum
--             eol

--             eol

-- materialParser = undefined
-- surfaceParser = undefined
-- devicePropsParser = undefined
-- deviceCoordsParser = undefined
-- sliceInfoParser = undefined
-- radiationModelParser = undefined


runTimeParser :: (Monad m, Stream s m Char) => TimeZone -> ParsecT s u m [TimeStep]
runTimeParser tZone = many $ parseTimeStep tZone

-- runTimeParserRestart' tZone (Node val []) = Nothing
-- runTimeParserRestart' tZone (Node val [(Node entries [])])
--     = Just
--     $ ParsedTimesteps
--     $ forceEither
--     $ parse entriesParser "" entries
--     where
--         entriesParser = many $ parseTimeStep tZone

-- fdsVersionParser' (Node val []) = parse versionParser "" val
--     where
--         versionParser = do
--             string "Version"
--             onlySpaces
--             string ":"
--             onlySpaces
--             optional $ string "FDS"
--             onlySpaces
--             version <- parseVersion
--             onlySpaces
--             serialOrPar <- many anyChar
--             return version
--             <?> "FDS version"

-- deviceActivationParser :: Parser [(Int, String, DevcActivation)]
-- deviceActivationParser = do
--     string " DEVICE Activation Times"
--     eol
--     eol
--     acts <- many (try devActTime)
--     eol
--     eol
--     return acts

-- devActTime :: Parser (Int, String, DevcActivation)
-- devActTime = do
--     onlySpaces
--     number <- intNum
--     onlySpaces
--     -- name <- basicString
--     -- onlySpaces
--     name <- manyTill (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm_. -QWERTYUIOPASDFGHJKLZXCVBNM") (lookAhead devcActivationValue)
--     activation <- devcActivationValue
--     eol
--     return (number, name, activation)
--     -- where
--         -- parseActTime = try $

-- devcActivationValue :: Parser DevcActivation
-- devcActivationValue = do
--     try (do {string "No Activation"; return NoActivation;})
--     <|> try (do {d <- floatNum; space; string "s"; return (DevcActivationTime d)})

-- clocksParser = undefined

--  -- Time Stepping Wall Clock Time (s):     5782.880
--  -- Total Elapsed Wall Clock Time (s):     5795.196

-- stopParser = choice [parseStoppedByUser, parseCompleted, parseNumericalInstability]
--     where
--         parseStoppedByUser = try (string "STOP: FDS stopped by user") >> return StoppedByUser
--         parseCompleted = try (string "STOP: FDS completed successfully") >> (return Completed)
--         parseNumericalInstability = try (string "STOP: Numerical Instability") >> return NumericalInstability
--         -- parseNumericalInstability = try (string "STOP: FDS was improperly set-up") >> return NumericalInstability
--         -- parseNumericalInstability = try (string "STOP: Set-up only") >> return NumericalInstability
--         -- parseNumericalInstability = try (string "STOP: FDS was stopped by KILL control function") >> return NumericalInstability



-- outFileParserSetUp :: Parser OutData
-- outFileParserSetUp = do
--     eol
--     string "Stop FDS, Set-up only"
--     eol
--     fail "Set-up only"

-- supportedVersions =
--     [ Version 5 5 3
--     , Version 5 5 0
--     , Version 6 0 0
--     , Version 6 0 1
--     , Version 6 1 0
--     , Version 6 1 1
--     , Version 6 3 2
--     ]

-- -- miscParameters :: Parser MiscParameters
-- -- miscParameters = do
-- --     string " Miscellaneous Parameters"
-- --     eol
-- --     spaces
-- --     string "Simulation Start Time (s)"
-- --     spaces
-- --     simStartTime <- floatNum
-- --     eol
-- --     spaces
-- --     string "Simulation End Time (s)"
-- --     spaces
-- --     simEndTime <- floatNum
-- --     eol
-- --     spaces
-- --     string "LES Calculation"
-- --     eol
-- --     choice [try smagorinskyConstant, try deardorffModel]


-- --     spaces
-- --     string "Turbulent Prandtl Number"
-- --     spaces
-- --     turbPrandtl <- floatNum
-- --     eol
-- --     spaces
-- --     optionMaybe $ (do
-- --         string "Turbulent Schmidt Number"
-- --         spaces
-- --         floatNum
-- --         spaces
-- --         )
-- --     string "Ambient Temperature (C)"
-- --     spaces
-- --     ambTemp <- floatNum
-- --     eol

-- --     return $ MiscParameters simStartTime simEndTime

-- smagorinskyConstant = do
--     spaces
--     string "Smagorinsky Constant"
--     spaces
--     smagConst <- floatNum
--     eol
-- deardorffModel = do
--     onlySpaces
--     string "Deardorff Model"
--     _ <- optionMaybe (do
--         string " (C_DEARDORFF)"
--         onlySpaces
--         v <- floatNum
--         return v)
--     eol


-- clearLine = do
--     manyTill anyChar eol


-- meshLine = do
--     try (do { spaces; string "Mesh";})
--     spaces
--     number <- intNum
--     cycleNumMaybe <- optionMaybe $ do
--         char ','
--         spaces
--         string "Cycle"
--         spaces
--         intNum
--     eol
--     return (number, fromMaybe 0 cycleNumMaybe)


-- -- parseTimeStepMulti :: TimeZone -> Parser (Either RestartInfo TimeStep)
-- -- parseTimeStepMulti tZone = do
--     -- try (do
--         -- tStep <- parseTimeStep tZone
--         -- return $ Right tStep)
--     -- <|> (do
--         -- restartInfo <- parseRestartInfo
--         -- return $ Left restartInfo)

-- -- parseRestartInfo = do
--     -- headInfo <- parseHead
--     -- return $ RestartInfo
--         -- { restartInfo_compileDate = "undefined"
--         -- , restartInfo_version = "undefined"
--         -- , restartInfo_openMPStatus = "undefined"
--         -- , restartInfo_svnRevision = 1234
--         -- , restartInfo_jobTitle = "undefined"
--         -- , restartInfo_chid = "undefined"
--         -- }

parseStepProp :: (Monad m, Stream s m Char) => ParsecT s u m StepProp
parseStepProp = do
    onlySpaces
    key <- many1 (noneOf "-\r\n:")
    char ':' *> onlySpaces
    value <- parseValue <* onlySpaces
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
    eol
    return $ StepProp
        { stepPropKey = key
        , stepPropValue = value
        , stepPropUnits = units
        , stepPropLocation = location
        }

-- TODO: currently, parsing an Int will never succeed
parseValue :: (Monad m, Stream s m Char) => ParsecT s u m Value
parseValue = choice [try parseValueDouble, try parseValueInt]

parseValueInt :: (Monad m, Stream s m Char) => ParsecT s u m Value
parseValueInt = do
    int <- intNum
    return $ ValueInt int
    <?> "ValueInt"

parseValueDouble :: (Monad m, Stream s m Char) => ParsecT s u m Value
parseValueDouble = do
    double <- floatNum
    return $ ValueDouble double
    <?> "ValueDouble"

parseUnits :: (Monad m, Stream s m Char) => ParsecT s u m String
parseUnits = many1 (satisfy (\c->
    isAscii c
    && not (isControl c)
    && not (isSpace c)))

intCoords :: (Monad m, Stream s m Char) => ParsecT s u m (Int, Int, Int)
intCoords = Text.Parsec.between (char '(')  (char ')') (do
    onlySpaces
    i <- intNum
    onlySpaces >> optional (char ',') >> onlySpaces
    j <- intNum
    onlySpaces >> optional (char ',') >> onlySpaces
    k <- intNum
    onlySpaces
    return (i, j, k)
    <?> "Location")

parseTimeStep :: (Monad m, Stream s m Char) => TimeZone -> ParsecT s u m TimeStep
parseTimeStep tZone = do
    onlySpaces *> string "Time Step" *> onlySpaces
    stepNumber <- intNum <* onlySpaces
    timeString <- manyTill anyChar eol

    onlySpaces *> string "Step Size:" *> onlySpaces
    stepSize <- floatNum <* onlySpaces <* char 's' <* char ','
    onlySpaces *> string "Total Time:" *> onlySpaces
    totalTime <- floatNum <* onlySpaces <* char 's'
    eol

    props <- many1 (try parseStepProp)

    onlySpaces *> many1 (char '-') *> eol

    meshes <- many1 (try parseMeshStep)
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

parseMeshStep :: (Monad m, Stream s m Char) => ParsecT s u m MeshStep
parseMeshStep = do
    meshL <- optionMaybe $ try $
        onlySpaces *> string "Mesh" *> onlySpaces
            *> intNum <* onlySpaces <*  eol
    let meshNumber = case meshL of
            Nothing -> 1
            Just n -> n
    properties <- many1 (try parseStepProp)
    return $ MeshStep meshNumber properties
    <?> "Mesh Step"

-- totalHRR = do
--     try (do { spaces; string "Total Heat Release Rate:";})
--     spaces
--     hrr <- floatNum
--     string " kW"
--     eol
--     return hrr

-- poisPert = do
--     try (do { spaces; string "Poisson Pert. :";})
--     onlySpaces
--     pp <- floatNum
--     eol
--     return pp

-- maxVNNumber = do
--     try (do { spaces; string "Max VN"; onlySpaces; string "number:";})
--     onlySpaces
--     minDivVal <- floatNum
--     string " at "
--     minDivCoord <- coords
--     eol

-- maxDivError = do
--     try (do { spaces; string "Max div. error:";})
--     spaces
--     minDivVal <- floatNum
--     string " at "
--     minDivCoord <- coords
--     eol
--     return minDivVal

-- numberOfParticles = do
--     try (do { spaces; string "No. of Lagrangian Particles:";})
--     spaces
--     nParts <- floatNum
--     eol
--     return nParts

-- maxHRRPUV = do
--     try (do { spaces; string "Max HRRPUV:";})
--     onlySpaces
--     nParts <- floatNum
--     onlySpaces
--     string "kW/m^3"
--     eol
--     return nParts

-- parseRLTB = do -- radiation loss to boundaries
--     try (do { spaces; string "Radiation Loss to Boundaries:";})
--     spaces
--     rltb <- floatNum
--     string " kW"
--     eol
--     return rltb



-- basicString :: Parser String
-- basicString = do
--     str <- many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm._QWERTYUIOPASDFGHJKLZXCVBNM")
--     return str

-- basicStringSpaces :: Parser String
-- basicStringSpaces = do
--     str <- many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm. _QWERTYUIOPASDFGHJKLZXCVBNM()")
--     return str

-- basicStringComma :: Parser String
-- basicStringComma = do
--     str <- many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm.,_QWERTYUIOPASDFGHJKLZXCVBNM()")
--     return str

-- basicStringCommaSpaces :: Parser String
-- basicStringCommaSpaces = do
--     str <- many (oneOf "0123456789qwertyuiopasdfghjklzxcvbnm., _QWERTYUIOPASDFGHJKLZXCVBNM()")
--     return str

coords :: (Monad m, Stream s m Char) => ParsecT s u m (Int,Int,Int)
coords = (,,)
    <$> (char '(' *> onlySpaces *> intNum <* sep)
    <*> (intNum <* sep)
    <*> (intNum <* onlySpaces <* char ')')
    <?> "Coordinates (i,j,k)"
    where
        sep :: (Monad m, Stream s m Char) => ParsecT s u m ()
        sep = onlySpaces *> char ',' *> onlySpaces *> pure ()

-- eolString :: Parser String
-- eolString = do
--     res <- oneOf "\n\r"
--     return $ show res


-- caseCHID :: Parser String
-- caseCHID = do
--     string " Job ID String       : "
--     many anyChar

-- caseTitle :: Parser String
-- caseTitle = do
--     string " Job TITLE        : "
--     many anyChar


-- getCurrentProgressOut :: OutData -> IO CurrentProgress
-- getCurrentProgressOut outData = do
--     let ts = case timesteps outData of
--             [] -> error "getCurrentProgressOut: no timesteps"
--             xs -> last xs
--         lastWall = time ts
--         lastSim = simTime ts
--         endTime = simEnd $ miscellaneous outData
--     currentTime <- getCurrentTime
--     return $  CurrentProgress
--         { currentProgress_endTime = endTime
--         , currentProgress_lastSimTime = lastSim
--         , currentProgress_lastWallTime = lastWall
--         , currentProgress_currentWallTime = currentTime
--         }
