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
            -- monitorProg = "dist/FDSQuickMon.exe"
            -- futeProg = "dist/fute.exe"
            futeRsProg = "dist/fute-rs.exe"
        want [installer]

        -- wxs is the installer specification file
        let wxs = "FDSToolsInstaller.wxs"

        -- wixobj is the wix object file
        let wixobj = "_build" </> "FDSToolsInstaller.wixobj"

        installer *> \out -> do
            need [wxs]
            -- installerProvidedLibs <- getInstallerProvidedLibs wxs
            need [futeRsProg]
            -- libs <- getRequiredLibs installerProvidedLibs [monitorProg, futeProg]
            -- need ([wixobj, wxs, futeProg, monitorProg] ++ libs)
            need ([wixobj, wxs, futeRsProg])
            cmd "light" [wixobj, "-o", out]

        wixobj *> \out -> do
            need [wxs]
            -- installerProvidedLibs <- getInstallerProvidedLibs wxs
            -- need [monitorProg, futeProg]
            -- libs <- getRequiredLibs installerProvidedLibs [monitorProg, futeProg]
            need ([wxs, futeRsProg])
            cmd "candle" [wxs, "-o", out]
        futeRsProg *> \out -> do
            alwaysRerun
            command_ [] "cargo" ["build", "--release"]
            copyFileChanged ("target/release/fute-rs.exe") out

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
