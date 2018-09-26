fdsVersionParserEvac = do
    string " FDS+Evac Version"
    spaces
    string ":"
    spaces
    fdsVersionString <- manyTill anyChar eol
    return fdsVersionString

svnRevisionNoParserEvac = do
    string " FDS+Evac SVN Revision No."
    spaces
    string ":"
    spaces
    svnRevisionNoString <- manyTill anyChar eol
    return svnRevisionNoString


-- evacOutFileParser :: Parser EvacOutData
evacOutFileParser = do
    eol
    string " FDS+Evac Evacuation Module"
    eol
    eol
    compilationDateString <- compilationDateParserEvac
    fdsVersionString <- fdsVersionParserEvac
    svnRevisionNoString <- svnRevisionNoParserEvac

    -- eol
    -- clearLine -- color method
    -- clearLine -- vis_door-Crit
    -- clearLine -- EFF file
    -- eol
    -- clearLine -- these are per mesh (Evac)
    -- eol
    -- many initLineParser -- init
    -- eol
    -- clearLine
    -- clearLine
    -- eol
    manyTill clearLine (try (do string " EVAC: Initial positions of the agents"; eol;))
    initialAgentProps <- evacInitialAgentsParser
    eol
    eol
    agentExitProps <- evacAgentExitParser
    eof
    -- return initialAgentProps
    return $ EvacOutData EvacVersionInformation EvacMiscParams EvacMeshParams initialAgentProps agentExitProps

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

compilationDateParserEvac = do
    string " FDS+Evac Compilation Date"
    spaces
    string ":"
    spaces
    compilationDateString <- manyTill anyChar eol
    return compilationDateString

