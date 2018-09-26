import Prelude hiding ((*>))
import Development.Shake
import Development.Shake.FilePath

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
            verifProg = "dist/fds-verification.exe"
            monitorProg = "dist/FDSQuickMon.exe"
            futeProg = "dist/fute.exe"

        want [installer]
        installer *> \out -> do
            let wixobj = "_build/FDSToolsInstaller.wixobj"
            need [wixobj, futeProg, verifProg, monitorProg]
            cmd "light" [wixobj, "-o", out]
        "_build/FDSToolsInstaller.wixobj" *> \out -> do
            let wxs = "FDSToolsInstaller.wxs"
            need [wxs]
            cmd "candle" [wxs, "-o", "_build/FDSToolsInstaller.wixobj"]
        verifProg *> \out -> do
            alwaysRerun
            command_ [] "stack" ["install", "--local-bin-path", "dist"]
        monitorProg *> \out -> do
            alwaysRerun
            command_ [] "stack" ["install", "--local-bin-path", "dist"]
        futeProg *> \out -> do
            alwaysRerun
            command_ [] "stack" ["install", "--local-bin-path", "dist"]

-- buildWixObj
imConvert = "C:/Program Files (x86)/ImageMagick-6.8.9-Q16/convert.exe"
-- windres = "C:/Program Files/Haskell/ghc-7.8.2/mingw/bin/windres.exe"
windres = "windres"
