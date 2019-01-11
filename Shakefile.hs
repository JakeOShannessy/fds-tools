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
            monitorProg = "dist/FDSQuickMon.exe"
            futeProg = "dist/fute.exe"

        want [installer]
        -- wxs is the installer specification file
        let wxs = "FDSToolsInstaller.wxs"
        -- wixobj is the wix object file
        let wixobj = "_build" </> "FDSToolsInstaller.wixobj"
        installer *> \out -> do
            need ([wixobj, wxs, futeProg, monitorProg] ++ libs)
            cmd "light" [wixobj, "-o", out]
        wixobj *> \out -> do
            need ([wxs, futeProg, monitorProg] ++ libs)
            cmd "candle" [wxs, "-o", out]
        monitorProg *> \out -> do
            alwaysRerun
            command_ [] "stack" ["install", "fds-quick-monitor-program", "--local-bin-path", "dist"]
        futeProg *> \out -> do
            alwaysRerun
            command_ [] "stack" ["install", "fute", "--local-bin-path", "dist"]

-- buildWixObj
imConvert = "C:/Program Files (x86)/ImageMagick-6.8.9-Q16/convert.exe"
-- windres = "C:/Program Files/Haskell/ghc-7.8.2/mingw/bin/windres.exe"
windres = "windres"

-- Windows DLLs required for distribution
libs = 
    [ "lib64/libcairo-2.dll"
    , "lib64/libfontconfig-1.dll"
    , "lib64/libfreetype-6.dll"
    , "lib64/libiconv-2.dll"
    , "lib64/liblzma-5.dll"
    , "lib64/libpixman-1-0.dll"
    , "lib64/libpng15-15.dll"
    , "lib64/libxml2-2.dll"
    , "lib64/zlib1.dll"
    ]
