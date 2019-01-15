import Prelude hiding ((*>))
import Development.Shake
import Development.Shake.FilePath
import Data.List (stripPrefix, isPrefixOf, takeWhile, nub, (\\), concat)
import Text.XML.HXT.Core

main = shakeArgs shakeOptions
    {shakeLint = Just LintBasic
    -- , shakeAssume = Just AssumeDirty
    , shakeFiles = "_build/shake"
    } $ do
        phony "clean" $ do
            removeFilesAfter "_build" ["//*"]
            removeFilesAfter "dist" ["//*"]
            command_ [] "stack" ["clean"]
        let buildDir = "_build"
            inBuildDir x = joinPath [buildDir, x]
            installer = "FDSToolsInstaller.msi"
            monitorProg = "dist/FDSQuickMon.exe"
            futeProg = "dist/fute.exe"
        want [installer]

        -- wxs is the installer specification file
        let wxs = "FDSToolsInstaller.wxs"
        
        -- wixobj is the wix object file
        let wixobj = "_build" </> "FDSToolsInstaller.wixobj"

        installer *> \out -> do
            need [wxs]
            installerProvidedLibs <- getInstallerProvidedLibs wxs
            need [monitorProg, futeProg]
            libs <- getRequiredLibs installerProvidedLibs [monitorProg, futeProg]
            need ([wixobj, wxs, futeProg, monitorProg] ++ libs)
            cmd "light" [wixobj, "-o", out]

        wixobj *> \out -> do
            need [wxs]
            installerProvidedLibs <- getInstallerProvidedLibs wxs
            need [monitorProg, futeProg]
            libs <- getRequiredLibs installerProvidedLibs [monitorProg, futeProg]
            need ([wxs, futeProg, monitorProg] ++ libs)
            cmd "candle" [wxs, "-o", out]

        monitorProg *> \out -> do
            -- we will always run "stack install" to make sure the binaries are up to date,
            -- but we will only copy them to further processing if they have changed, to
            -- prevent unnecessary remakes.
            alwaysRerun
            command_ [] "stack" ["install", "fds-quick-monitor-program", "--local-bin-path", "_build/bins"]
            copyFileChanged ("_build/bins/FDSQuickMon.exe") out
        futeProg *> \out -> do
            alwaysRerun
            command_ [] "stack" ["install", "fute", "--local-bin-path", "_build/bins"]
            copyFileChanged ("_build/bins/fute.exe") out

        "lib64/*.dll" *> \out -> do
            let filename = dropDirectory1 out
            Stdout pathMSYS <- cmd ["stack", "exec", "--", "which", filename] 
            -- We use "init" to ditch the the newline returned from these commmands
            let trimmedMSYSPath = init pathMSYS :: String 
            Stdout realPath <- cmd ["cygpath", "-w", trimmedMSYSPath]
            copyFile' (init realPath) out

-- buildWixObj
imConvert = "C:/Program Files (x86)/ImageMagick-6.8.9-Q16/convert.exe"
-- windres = "C:/Program Files/Haskell/ghc-7.8.2/mingw/bin/windres.exe"
windres = "windres"

-- |Given a list of executables, determine the shared libraries they require to
-- run.
getRequiredLibs installerProvidedLibs progs = do
    libString <- concat <$> mapM runDependenciesExe progs
    let libs = nub $ map ("lib64/" ++) $ map init $ map (takeWhile (\c->c /= ':')) $ map (\(Just x) -> x) $ map (stripPrefix "[Environment] ") $ filter (isPrefixOf "[Environment] ") $ lines libString
    liftIO $ mapM_ print libs
    if length (libs \\ installerProvidedLibs) > 0
        then error $ "The following libraries are required, but not provided by the installer:\n" ++ (unlines (libs \\ installerProvidedLibs))
        else pure ()
    pure libs

-- |Run the dependency finding program on an executable to determine the shared
-- libraries it depends on.
runDependenciesExe prog = do
    Stdout libs <- cmd
        [ "C:\\Users\\josha\\Downloads\\Dependencies\\Dependencies.exe"
        , "-modules"
        , prog
        ]
    pure (libs :: String)

getInstallerProvidedLibs installerPath = do
    liftIO $ runX (readDocument [ withValidate no] installerPath
        >>> deep (isElem >>> hasName "Component" >>> hasAttrValue "Id" (== "CommonRender"))
        >>> isElem >>> getChildren >>> hasName "File" >>> getAttrValue "Source")

