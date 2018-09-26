module FDSUtilities.CompileCompress.Screenshots where

import Control.Exception as E
import Control.Lens
import Control.Monad

import Data.List
-- import Data.Maybe
import qualified Data.Map as M

-- import FDSUtilities.Monitor

import FDSUtilities.Parsing
import FDSUtilities.Parsing.SliceFile
import FDSUtilities.Parsing.SMVFile
import qualified Data.ByteString as B

-- import FDSUtilities.Simulation
import FDSUtilities.Smokeview
import FDSUtilities.Smokeview.Script
import FDSUtilities.Types
import FDSUtilities.Paths
import FDSUtilities.Types.Monitor
import FDSUtilities.Types.Smokeview

-- import GHC.IO.Handle

import System.Directory
import System.IO.Error
import System.FilePath
import System.Process
-- --import System.Process.Pipe
import System.IO
import System.IO.Temp

-- import Text.Printf



-- TODO: provide an easy interface to calculate the viewAngle
-- TODO: obtain times from SMV file and slice file rather than relying on smokeview, which does no checking.
-- TODO: allow for slightly incorrect specification of slice locations. Output warning of correction but continue.
-- TODO: throw warning if the request values could not be found.
-- TODO: compile all the warnings through the compliation and present them at the end.


-- addRender :: Render -> ScreenCompilationSet -> ScreenCompilationSet
-- addRender render compSet = compRenders .~ (render:renders) $ compSet
    -- where
        -- renders = compSet^.compRenders

-- produceScreenShots :: [ScreenCompilationSet] -> CompilationActionFunction
-- produceScreenShots screenCompilationSets destDir simulation = (liftM concat) $ mapM (produceScreenSet destDir simulation) screenCompilationSets

-- produceScreenSet :: FilePath -> FDSSimulation -> ScreenCompilationSet -> IO [FilePath]
-- produceScreenSet resultsDir sim compilationSet = do
    -- print compilationSet
    -- produceAllScreenshots config resultsDir sim renders
    -- where
        -- config = compilationSet ^. compIniConfig
        -- renders = compilationSet ^. compRenders

-- produceAllScreenshots :: IniConfig -> FilePath -> FDSSimulation -> [Render] -> IO [FilePath]
-- produceAllScreenshots baseIniConfig destDir sim@(FDSSimulation {chid = simCHID, simDir = path}) renders = withSystemTempDirectory "screenshotting" theThing
    -- where
        -- theThing tempDir = do
            -- resultsDirAbs <- canonicalizePath destDir
            -- absProjPath <- canonicalizePath (simDir sim)
            -- let sim' = sim {simDir = absProjPath}
            -- print "before getCurrentDirectory"
            -- cwd <- getCurrentDirectory
            -- print "after getCurrentDirectory"
            -- setCurrentDirectory tempDir
            -- paths <- (liftM concat) $ mapM (\render -> compileScreenshots baseIniConfig resultsDirAbs sim' (render ^. renderData) (render ^. renderTimes)) renders
            -- setCurrentDirectory cwd
            -- return paths

produceScreenshots :: ScreenCompilationSet -> FilePath -> FDSSimulation -> IO [FilePath]
produceScreenshots compilationSet resultsDir sim =
  withSystemTempDirectory "screenshotting" $ \tempDir -> do
    resultsDirAbs <- canonicalizePath resultsDir
    absProjPath <- canonicalizePath (simDir sim)
    let sim' = sim {simDir = absProjPath}
    -- TODO: this should not be necessay due to setting renderDir
    cwd <- getCurrentDirectory
    setCurrentDirectory tempDir
    paths <- compileScreenSet resultsDirAbs sim' compilationSet
    setCurrentDirectory cwd
    return paths

-- mkResultPath resultsDirAbs (l:ls) = case l of
    -- (SLCFLoad slicename axis location) ->
            -- joinPath [resultsDirAbs, "SLCF", slicename, show axis, show location]
    -- (SLCFLoadVec slicename axis location) ->
            -- joinPath [resultsDirAbs, "SLCFVec", slicename, show axis, show location]
    -- (SMOKE3DLoad name) ->
            -- joinPath [resultsDirAbs, "SMOKE3D", name]
    -- (BNDFLoad name axis) ->
            -- joinPath [resultsDirAbs, "BNDF", name, show axis]


compileScreenSet :: FilePath -> FDSSimulation -> ScreenCompilationSet -> IO [FilePath]
compileScreenSet resultsDirAbs sim compilationSet@(ScreenCompilationSet name baseConfig sets) = do
  let screenSetDir = joinPath [resultsDirAbs, name]
  putStrLn $ "Compiling FDS results for " ++ show (simCHID sim)
  absProjPath <- canonicalizePath (simDir sim)    -- TODO: this canonicalisation should be made inherent to the datatype
  print absProjPath
  print"about to compile lua screenshot set"
  let sim' = (sim {simDir = absProjPath}) -- updated simulation information with absolute path
  -- at this  point we now produce the lua file
  -- TODO: write baseConfig out to ini file for default loading (make it in a temp maybe)
  let baseIniPath = joinPath [simDir sim, simCHID sim ++ ".ini"]

  let lualines = Script $ (concatMap (produceScreenScript baseIniPath screenSetDir) sets)
      luaScript = scriptToLuaString lualines ++ "exit()\n"
      -- filenames = map (mkRenderName casename {-loadData-}) times
      -- smvFilename = chid sim ++ ".smv"
      -- smvFilePath = joinPath [simDir sim, smvFilename]
      luaFilePath = joinPath [simDir sim, simCHID sim ++ ".lua"]

  let script = Script $ (concatMap (produceScreenScript baseIniPath screenSetDir) sets) -- ++ ["exit()"]
      -- filenames = map (mkRenderName casename {-loadData-}) times
      ssfScript = scriptToSSFString script
      smvFilename = simCHID sim ++ ".smv"
      smvFilePath = joinPath [simDir sim, smvFilename]
      ssfFilePath = joinPath [simDir sim, simCHID sim ++ ".ssf"]
      iniString = fdsPrint baseConfig
  writeFile baseIniPath iniString
  -- writeFile ssfFilePath ssfScript
  writeFile luaFilePath luaScript
  createDirectoryIfMissing True screenSetDir
  -- let smokeviewPathLua = "/home/jake/Projects/fds-smv/SMV/Build/gcc_linux/smokeview_gcc_64"
  let smokeviewPath = FDSUtilities.Paths.smokeviewPath
  res <- readProcess smokeviewPath ["-runluascript", smvFilePath]  ""
  -- res <- readProcess smokeviewPath ["-runscript", smvFilePath]  ""
  putStr res
  return []

-- TODO: The below needs a better abstraction (after the lens abstraction is completed).
-- TODO: look at slice times, either in the SMV file or slices files themselves to ensure
--       that the closest timeframe is chosen and the output filename matches.
-- compileScreenshots :: IniConfig -> FilePath -> FDSSimulation -> [LoadData] -> [Double] -> IO [FilePath]
-- compileScreenshots baseIniConfig resultsDirAbs sim loadData {-slice@(Slice slicename axis location)-} times' = do
    -- let times = nub times'

    -- let destDir = mkResultPath resultsDirAbs loadData

    -- putStrLn $ "Compiling FDS results for " ++ show (chid sim)
    -- putLinebreak
    -- absProjPath <- canonicalizePath (simDir sim)    -- TODO: this canonicalisation should be made inherent to the datatype
    -- print absProjPath
    -- print"about to compile screenshot"
    -- let sim' = (sim {simDir = absProjPath}) -- updated simulation information with absolute path
    -- sliceFiles <- filesToLoad sim' loadData
    -- print "after filesToLoad"
    -- let
        -- -- fdsFilename = fdsFilePath sim
        -- casename = chid sim
        -- fdsFilename = casename ++ ".fds"
        -- sliceProperties = getSliceProps (baseIniConfig ^. iniConfigSliceProperties) (head loadData) -- TODO: is based of first slice
        -- colourBarFlip = case sliceProperties of
                            -- Just x -> x ^. sliceColourBarFlip
                            -- Nothing -> baseIniConfig ^. iniConfigColourBarFlip
        -- colourBounds = case sliceProperties of
                            -- Just x -> x ^. sliceColourBounds
                            -- Nothing -> noBounds
        -- contourVal = case sliceProperties of
                            -- Just x -> x ^. sliceContourValue
                            -- Nothing -> Nothing

        -- barLevel = calcBarLevelMaybe colourBounds contourVal
        -- iniConfig
            -- = iniConfigBarLevel .~ barLevel
            -- $ iniConfigColourBarFlip .~ colourBarFlip
            -- $ baseIniConfig
        -- iniString = makeIni iniConfig
        -- -- TODO: convert times to closest frame numbers and use those instead
        -- ssfString = makeSSFInd {-(head loadData)-} sliceFiles times casename -- TODO: is based of first slice
        -- iniFilename = casename ++ ".ini"
        -- iniFilePath = joinPath [simDir sim, iniFilename]
        -- ssfFilename = casename ++ ".ssf"
        -- ssfFilePath = joinPath [simDir sim, ssfFilename]
        -- filenames = map (mkRenderName casename {-loadData-}) times
        -- smvFilename = casename ++ ".smv"
        -- smvFilePath = joinPath [simDir sim, smvFilename]
    -- print "about to write config files"
    -- writeFile iniFilePath iniString
    -- writeFile ssfFilePath ssfString
    -- -- let command = (smokeviewPath ++ " -script " ++ "\"" ++ ssfFilePath ++ "\"" ++ " " ++ "\"" ++ smvFilePath ++ "\"")
    -- print "before create directory"
    -- createDirectoryIfMissing True destDir
    -- print "after create directory"
    -- res <- readProcess smokeviewPath ["-script", ssfFilePath, smvFilePath] ""
    -- putStr res
    -- paths <- mapM (moveScreenshot sim' destDir) (nub filenames)
    -- print paths
    -- return paths

-- getSliceProps :: SlicePropertiesDict -> LoadData -> Maybe SliceProperties
-- getSliceProps slicePropertiesDict (SLCFLoad slicename axis location)
    -- -- = find (\sliceProps -> (sliceProps ^. sliceAbbrev) == slicename) sliceProperties
    -- = M.lookup slicename sliceProperties
    -- where
        -- sliceProperties = dictToMap slicePropertiesDict
-- getSliceProps _ _ = Nothing

calcBarLevelMaybe :: DataBounds -> Maybe Double -> Maybe Int --TODO: ideally the maybe logic would be taken out of this function
calcBarLevelMaybe colourBounds (Just cont) = Just $ round (((cont - min)/ (max - min)) * 255)
    where
        Just min = colourBounds^.dBoundMin
        Just max = colourBounds^.dBoundMax
calcBarLevelMaybe _ _ = Nothing

moveScreenshot :: FDSSimulation -> FilePath -> String -> IO FilePath
moveScreenshot sim destDir rendername = do
    putStr "Moving "
    putStr oldPath
    putStr " to "
    putStr newPath
    putStrLn "..."
    fileExists <- doesFileExist oldPath
    if fileExists
        then (do
                createDirectoryIfMissing True destDir
                toTry `E.catch` handler
                )
        else (do
            putStrLn "\t...file does not exist, skipping."
            return $ error "Screenshot missing."
            )
    where
        toTry = do
            -- renameFile oldPath newPath
            copyFile oldPath newPath
            putStrLn "\t...completed."
            return newPath
        filename = rendername ++ ".png"
        oldPath = joinPath [simDir sim, filename]
        newPath = joinPath [destDir, filename]
        -- handler :: IOError -> IO (Maybe FilePath)
        handler e
            | isPermissionError e = do
                putStrLn "\t...permission denied, throwing error."
                ioError e
            | otherwise = ioError e







-- mkRenderName casename slice@(Slice slicename axis location) time = casename ++ "_" ++ slicename ++ "_" ++ show axis ++ "_" ++ show location ++ "_" ++ (printf "%04.0d" ((round time) :: Int)) ++ "s"


-- filesToLoad :: FDSSimulation -> [LoadData] -> IO [(FilePath, Bool)] -- the bool is true if load vector
-- filesToLoad sim loadData = do
    -- paths <- mapM (getData sim) loadData -- the bool is true if load vectorloadData
    -- return $ concat paths

putLinebreak = putStrLn "\n"




--------------------------------- The below code needs a serious refactor

dataFileEntryFilterSMOKE3D shortNameRule dataFileEntries = filter theFilter dataFileEntries
            where
                theFilter dataFileEntry = shortNameRule (smoke3dShortName dataFileEntry)

dataFileEntryFilterBNDF shortNameRule dataFileEntries = filter theFilter dataFileEntries
            where
                theFilter dataFileEntry = shortNameRule (bndfShortName dataFileEntry)

isSMOKE3DEntry (SMOKE3DDataFile _ _ _ _ _) = True
isSMOKE3DEntry _ = False

isBNDFEntry (BNDFDataFile _ _ _ _ _) = True
isBNDFEntry _ = False


-- getData :: FDSSimulation -> LoadData -> IO [(FilePath,Bool)]
-- getData simulation (SMOKE3DLoad name) = do
    -- let dir = simDir simulation
        -- projCHID = chid simulation
        -- smvFilename = projCHID ++ ".smv"
        -- smvFilePath = joinPath [dir, smvFilename]
    -- putStr "about to process smv file: "
    -- putStrLn smvFilePath
    -- parsedSMV <- parseSMVFile smvFilePath
    -- let smvData = case parsedSMV of
            -- Left e -> error $ show e
            -- Right x -> x
    -- let
        -- dataFileEntries = smvDataFiles smvData
        -- smoke3dFileEntries = filter isSMOKE3DEntry dataFileEntries
        -- selectedDataFileEntries' = dataFileEntryFilterSMOKE3D
                                -- (\x -> x == name)
                                -- smoke3dFileEntries
    -- selectedDataFileEntries <- mapM (mkPathsAbsolute dir) selectedDataFileEntries'

    -- return $ (flip zip) (repeat False) $ map smoke3dFilename selectedDataFileEntries
    -- where
        -- mkPathsAbsolute simDir entry@(SMOKE3DDataFile {smoke3dFilename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute' simDir entry filename = do
            -- absPath <- canonicalizePath $ joinPath [simDir, filename]
            -- return $ entry {smoke3dFilename = absPath}

-- getData simulation (BNDFLoad name _) = do
    -- let dir = simDir simulation
        -- projCHID = chid simulation
        -- smvFilename = projCHID ++ ".smv"
        -- smvFilePath = joinPath [dir, smvFilename]
    -- parsedSMV <- parseSMVFile smvFilePath
    -- let smvData = case parsedSMV of
            -- Left e -> error $ show e
            -- Right x -> x
    -- let
        -- dataFileEntries = smvDataFiles smvData
        -- bndfFileEntries = filter isBNDFEntry dataFileEntries
        -- selectedDataFileEntries' = dataFileEntryFilterBNDF
                                -- (\x -> x == name)
                                -- bndfFileEntries
    -- selectedDataFileEntries <- mapM (mkPathsAbsolute dir) selectedDataFileEntries'
    -- return $ (flip zip) (repeat False) $ map bndfFilename selectedDataFileEntries
    -- where
        -- mkPathsAbsolute simDir entry@(BNDFDataFile {bndfFilename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute' simDir entry filename = do
            -- absPath <- canonicalizePath $ joinPath [simDir, filename]
            -- return $ entry {bndfFilename = absPath}

-- getData simulation (SLCFLoad name axis location) = do
    -- let dir = simDir simulation
        -- projCHID = chid simulation
        -- smvFilename = projCHID ++ ".smv"
        -- smvFilePath = joinPath [dir, smvFilename]
    -- putStr "about to process smv file: "
    -- putStrLn smvFilePath
    -- let catchAny :: IO a -> (SomeException -> IO a) -> IO a
        -- catchAny = E.catch
    -- parsedSMV <- catchAny (parseSMVFile smvFilePath) $ \e -> do
      -- putStrLn $ "Got an exception: " ++ show e
      -- error "an exception"
    -- putStrLn "before parse"
    -- catchAny (return () {- print parsedSMV -}) $ \e -> do
      -- putStrLn $ "Got an exception while printing: " ++ show e
      -- error "a printing exception"
    -- print "after print"
    -- let smvData = case parsedSMV of
            -- Left e -> error $ show e
            -- Right x -> x
    -- print $ smvDataFiles smvData
    -- print "after parse"
    -- let
        -- dataFileEntries = smvDataFiles smvData
        -- sliceFileEntries = filter isSLCFEntry dataFileEntries
        -- selectedDataFileEntries' = dataFileEntryFilterSLCF
                                -- (\_ -> True)
                                -- (\_ -> True)
                                -- (\x -> x == name)
                                -- (\_ -> True)
                                -- dataFileEntries

    -- print "before selectedDataFileEntries"
    -- selectedDataFileEntries <- mapM (mkPathsAbsolute dir) selectedDataFileEntries'
    -- print "selectedDataFileEntries"
    -- withLocs <- mapM (addLocationData "." smvData) selectedDataFileEntries
    -- print "withLocs"
    -- print withLocs
    -- let filterLoc item@(meshNum, dataFileEntry, sliceHeader, (x1, x2, y1, y2, z1, z2), filepath) = case axis of
            -- X -> x1 `approxEqual` location && x2 `approxEqual` location
            -- Y -> y1 `approxEqual` location && y2 `approxEqual` location
            -- Z -> z1 `approxEqual` location && z2 `approxEqual` location
    -- let toRender = filter filterLoc withLocs

    -- return $ (flip zip) (repeat False) $ map (\((_, _, _, locs, filepath))->filepath) toRender

    -- where
        -- eps=0.05
        -- approxEqual x y = (x-eps) < y && (x+eps) > y
        -- addLocationData :: FilePath -> SMVFile -> DataFileEntry -> IO (Int, DataFileEntry, SliceDataHeader, (Double,Double,Double,Double,Double,Double), FilePath)
        -- addLocationData dir smvFile dataFileEntry@(SLCFDataFile
                -- { slcfMeshNum = meshNum
                -- , slcfFilename = filename
                -- , slcfLongName = longName
                -- , slcfShortName = shortName
                -- , slcfUnits = units
                -- })
                -- = do
                    -- Right (sliceHeader@(SliceDataHeader lName sName units (i1, i2, j1, j2, k1, k2)), filepath) <- parseSingleSliceHeaderForTable (joinPath [dir, filename])
                    -- let (x1, y1, z1) = case getCornerXYZ trns (i1, j1, k1) of
                                        -- Left x -> error x
                                        -- Right (x1, y1, z1) -> (x1, y1, z1)
                        -- (x2, y2, z2) = case getCornerXYZ trns (i2, j2, k2) of
                                        -- Left x -> error x
                                        -- Right (x2, y2, z2) -> (x2, y2, z2)
                        -- res = (meshNum, dataFileEntry, sliceHeader, (x1, x2, y1, y2, z1, z2), filepath)
                    -- return $ res
                    -- where
                        -- meshes = smvMeshes smvFile
                        -- mesh = meshes !! (meshNum - 1)
                        -- trns = smvMeshTRNs mesh
        -- isSLCFEntry dataFileEntry = case dataFileEntry of
                                        -- (SLCFDataFile _ _ _ _ _ _) -> True
                                        -- _ -> False
        -- mkPathsAbsolute simDir entry@(SLCFDataFile {slcfFilename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute simDir entry@(BNDFDataFile {bndfFilename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute simDir entry@(PRT5DataFile {prt5Filename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute simDir entry@(SMOKE3DDataFile {smoke3dFilename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute simDir entry@(PL3DDataFile {pl3dFilename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute' simDir entry filename = do
            -- putStrLn $ "about to make " ++ show simDir ++ show entry ++ show filename ++ " absolute"
            -- absPath <- canonicalizePath $ joinPath [simDir, filename]
            -- return $ entry {slcfFilename = absPath}

        -- parseSingleSliceHeaderForTable :: FilePath -> IO (Either String (SliceDataHeader, FilePath))
        -- parseSingleSliceHeaderForTable filepath = withFile filepath ReadMode $ \fHndl -> do
            -- rawData <- B.hGet fHndl 200 -- TODO: check that this is the right length for the header, change to parse as the file is read and only read the necessary amount
            -- let header = parseSliceHeader rawData
            -- return $ case header of
                        -- Left x -> Left "Parse error."
                        -- Right x -> Right (x, filepath)


        -- dataFileEntryFilterSLCF meshRule longNameRule shortNameRule unitsRule dataFileEntries = filter theFilter dataFileEntries
            -- where
                -- theFilter dataFileEntry = case dataFileEntry of -- TODO: this is unsatisfactory as it relies on the layout of SLCFDataFile
                    -- SLCFDataFile meshNum filename longName shortName units alias ->
                                            -- let conditions =
                                                    -- [ meshRule meshNum
                                                    -- , longNameRule longName
                                                    -- , shortNameRule shortName
                                                    -- , unitsRule units
                                                    -- ]
                                            -- in all id conditions
                    -- _ -> False

-- getData simulation (SLCFLoadVec name axis location) = do
    -- let dir = simDir simulation
        -- projCHID = chid simulation
        -- smvFilename = projCHID ++ ".smv"
        -- smvFilePath = joinPath [dir, smvFilename]
    -- putStr "about to process smv file: "
    -- putStrLn smvFilePath
    -- let catchAny :: IO a -> (SomeException -> IO a) -> IO a
        -- catchAny = E.catch
    -- parsedSMV <- catchAny (parseSMVFile smvFilePath) $ \e -> do
      -- putStrLn $ "Got an exception: " ++ show e
      -- error "an exception"
    -- putStrLn "before parse"
    -- catchAny (return () {- print parsedSMV -}) $ \e -> do
      -- putStrLn $ "Got an exception while printing: " ++ show e
      -- error "a printing exception"
    -- print "after print"
    -- let smvData = case parsedSMV of
            -- Left e -> error $ show e
            -- Right x -> x
    -- print $ smvDataFiles smvData
    -- print "after parse"
    -- let
        -- dataFileEntries = smvDataFiles smvData
        -- sliceFileEntries = filter isSLCFEntry dataFileEntries
        -- selectedDataFileEntries' = dataFileEntryFilterSLCF
                                -- (\_ -> True)
                                -- (\_ -> True)
                                -- (\x -> x == name)
                                -- (\_ -> True)
                                -- dataFileEntries

    -- print "before selectedDataFileEntries"
    -- selectedDataFileEntries <- mapM (mkPathsAbsolute dir) selectedDataFileEntries'
    -- print "selectedDataFileEntries"
    -- withLocs <- mapM (addLocationData "." smvData) selectedDataFileEntries
    -- print "withLocs"
    -- print withLocs
    -- let filterLoc item@(meshNum, dataFileEntry, sliceHeader, (x1, x2, y1, y2, z1, z2), filepath) = case axis of
            -- X -> x1 `approxEqual` location && x2 `approxEqual` location
            -- Y -> y1 `approxEqual` location && y2 `approxEqual` location
            -- Z -> z1 `approxEqual` location && z2 `approxEqual` location
    -- let toRender = filter filterLoc withLocs

    -- return $ (flip zip) (repeat True) $ map (\((_, _, _, locs, filepath))->filepath) toRender

    -- where
        -- eps=0.05
        -- approxEqual x y = (x-eps) < y && (x+eps) > y
        -- addLocationData :: FilePath -> SMVFile -> DataFileEntry -> IO (Int, DataFileEntry, SliceDataHeader, (Double,Double,Double,Double,Double,Double), FilePath)
        -- addLocationData dir smvFile dataFileEntry@(SLCFDataFile
                -- { slcfMeshNum = meshNum
                -- , slcfFilename = filename
                -- , slcfLongName = longName
                -- , slcfShortName = shortName
                -- , slcfUnits = units
                -- })
                -- = do
                    -- Right (sliceHeader@(SliceDataHeader lName sName units (i1, i2, j1, j2, k1, k2)), filepath) <- parseSingleSliceHeaderForTable (joinPath [dir, filename])
                    -- let (x1, y1, z1) = case getCornerXYZ trns (i1, j1, k1) of
                                        -- Left x -> error x
                                        -- Right (x1, y1, z1) -> (x1, y1, z1)
                        -- (x2, y2, z2) = case getCornerXYZ trns (i2, j2, k2) of
                                        -- Left x -> error x
                                        -- Right (x2, y2, z2) -> (x2, y2, z2)
                        -- res = (meshNum, dataFileEntry, sliceHeader, (x1, x2, y1, y2, z1, z2), filepath)
                    -- return $ res
                    -- where
                        -- meshes = smvMeshes smvFile
                        -- mesh = meshes !! (meshNum - 1)
                        -- trns = smvMeshTRNs mesh
        -- isSLCFEntry dataFileEntry = case dataFileEntry of
                                        -- (SLCFDataFile _ _ _ _ _ _) -> True
                                        -- _ -> False
        -- mkPathsAbsolute simDir entry@(SLCFDataFile {slcfFilename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute simDir entry@(BNDFDataFile {bndfFilename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute simDir entry@(PRT5DataFile {prt5Filename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute simDir entry@(SMOKE3DDataFile {smoke3dFilename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute simDir entry@(PL3DDataFile {pl3dFilename = filename}) = mkPathsAbsolute' simDir entry filename
        -- mkPathsAbsolute' simDir entry filename = do
            -- putStrLn $ "about to make " ++ show simDir ++ show entry ++ show filename ++ " absolute"
            -- absPath <- canonicalizePath $ joinPath [simDir, filename]
            -- return $ entry {slcfFilename = absPath}

        -- parseSingleSliceHeaderForTable :: FilePath -> IO (Either String (SliceDataHeader, FilePath))
        -- parseSingleSliceHeaderForTable filepath = withFile filepath ReadMode $ \fHndl -> do
            -- rawData <- B.hGet fHndl 200 -- TODO: check that this is the right length for the header, change to parse as the file is read and only read the necessary amount
            -- let header = parseSliceHeader rawData
            -- return $ case header of
                        -- Left x -> Left "Parse error."
                        -- Right x -> Right (x, filepath)


        -- dataFileEntryFilterSLCF meshRule longNameRule shortNameRule unitsRule dataFileEntries = filter theFilter dataFileEntries
            -- where
                -- theFilter dataFileEntry = case dataFileEntry of -- TODO: this is unsatisfactory as it relies on the layout of SLCFDataFile
                    -- SLCFDataFile meshNum filename longName shortName units alias ->
                                            -- let conditions =
                                                    -- [ meshRule meshNum
                                                    -- , longNameRule longName
                                                    -- , shortNameRule shortName
                                                    -- , unitsRule units
                                                    -- ]
                                            -- in all id conditions
                    -- _ -> False






