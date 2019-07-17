{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}
module FDSUtilities.WatchCase where

-- TODO: Deprecate this entire file and move the functionality elsewhere

-- -- |Creates a watch case based on what information is available.
-- createAutoWatchCase :: FDSSimulation -> IO WatchCase
-- createAutoWatchCase sim = do
--     dList <- getDataListAll sim
--     let vecInfo' = map (\dvp->((dataVectorName.yVector) dvp, (dataVectorUnits.yVector) dvp)) dList
--         vecInfo = foldl' mergeInfo M.empty vecInfo'
--     print vecInfo
--     return $ WatchCase sim $ infoMapToMonitorList vecInfo

-- -- TODO: remove dependency on the ordering of data type.
-- concatenateWatchCases :: WatchCase -> WatchCase -> WatchCase
-- concatenateWatchCases
--     (WatchCase sim1 monitors1)
--     (WatchCase sim2 monitors2)
--     = if sim1 /= sim2 then error "sims do not match"
--         else WatchCase sim1 $ monitors1 ++ monitors2

-- mergeWatchCases :: WatchCase -> WatchCase -> WatchCase
-- mergeWatchCases watchCase1 watchCases2 = undefined


-- data ServeType = Local | Server
-- -- TODO: Consider storing all data in one large simple data structure
-- --       that is then scanned on the plot command.
-- chartWatchCaseBase :: ServeType -> FilePath -> WatchCase -> IO (Either FilePath H.Html)
-- chartWatchCaseBase serveType statusDir watchCase = do
--     let sim = watchCase ^. watchCaseSim
--         monitors = watchCase ^. watchCaseMonitors
--     putStr (simCHID sim)
--     dList <- {-# SCC getDataListAll #-} getDataListAll sim
--     -- let outFilePath = joinPath [simDir sim, simCHID sim ++ ".out"]
--     outFileExists <- doesFileExist $ outFilePath sim
--     -- print outFileExists


--     outData' <- if outFileExists
--                    then {-# SCC parseOutData #-} E.catch (parseSimulationOutFile sim) handler
--                    else error $ "Outfile does not exist. > " ++ (outFilePath sim)
--     let outData = case outData' of
--             Left e -> error $ show e
--             Right x -> x
--     let runData = getRunData outData
--         status = outStatus outData
--         version = outFDSVersion outData
--     verifAss <- {-# SCC verifAss #-} E.catch (assess version sim) assessFailHandler
--     -- print verifAss
--     case status of
--             NumericalInstability -> putStrLn " -> Warning: Numerical instability."
--             Incomplete -> putStrLn " -> Running..."
--             StoppedByUser -> putStrLn " -> Completed (user)"
--             Completed -> putStrLn " -> Completed (time end)"
--     -- currentTime <- getCurrentTime
--     let destinationPath = case serveType of
--             Local -> joinPath [statusDir, chid sim]
--             Server -> joinPath ["media", chid sim, "charts"]
--         mPars = miscellaneous outData
--         simEndTime = simEnd mPars
--         peTime = predEndTime simEndTime runData
--         currentRunTime = case runData of
--             [] -> error "No run data."
--             x -> (fst $ last x) `diffUTCTime` (fst $ head x)
--         -- peTime = currentTime
--         caseStatus = CaseStatus
--                         { caseCurrentProgress = getCurrentProgress outData
--                         , casePredictedEndTime = peTime
--                         , caseOutStatus = status
--                         }
--     tZone <- getCurrentTimeZone
--     statusLocation <- case serveType of
--         Local -> (liftM Left) $ createStatusFile destinationPath tZone (chid sim) ("RunTime":(map (view monVarName) monitors)) caseStatus verifAss currentRunTime
--         Server -> return $ Right $ createStatusHtmlServe destinationPath tZone (chid sim) ("RunTime":(map (view monVarName) monitors)) caseStatus verifAss currentRunTime
--     (mapM_ (produceMonitor destinationPath sim dList outData) monitors)
--     (produceRunChart destinationPath tZone simEndTime runData)    -- TODO: remove special support for rundata chart and include in generalisation
--     return statusLocation






-- -- TODO: Consider storing all data in one large simple data structure
-- --       that is then scanned on the plot command.
-- chartWatchCase :: FilePath -> WatchCase -> IO FilePath
-- chartWatchCase statusDir watchCase = do
--     ret <- chartWatchCaseBase Local statusDir watchCase
--     return $ case ret of
--         Left path -> path


-- chartWatchCaseServe :: FilePath -> WatchCase -> IO H.Html
-- chartWatchCaseServe statusDir watchCase = do
--     ret <- chartWatchCaseBase Server statusDir watchCase
--     return $ case ret of
--         Right path -> path



-- -- |Get the current progress (i.e. number of seconds simulated).
-- getCurrentProgress :: OutData -> Double
-- getCurrentProgress outData = minimum $ map totalTime $ timeStepMeshes' $ lastStep
--     where
--         lastStep = case timesteps outData of
--             [] -> error "No progress data available."
--             x -> last x









