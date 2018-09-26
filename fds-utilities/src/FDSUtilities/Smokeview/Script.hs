module FDSUtilities.Smokeview.Script where

import FDSUtilities.Types
import System.FilePath
import FDSUtilities.Types.Smokeview
import FDSUtilities.Types.Screenshots

import Control.Lens




    

siToLuaString :: ScriptInstruction -> String -- this string may include new lines, but does not end in one
siToLuaString (SILoadFile path) = "loaddatafile(" ++ show path ++ ")"
siToLuaString (SILoadVFile path) = "loaddatavfile(" ++ show path ++ ")"
siToLuaString (SIRender name) = "render(\"" ++ name ++ "\")"
siToLuaString (SISetTime time) = "settime(" ++ show time ++ ")"
siToLuaString (SIUnloadAll) = "unloadall()"
siToLuaString (SISetRenderDir path) = "setrenderdir(" ++ show path ++ ")"
siToLuaString (SILoadIniFile path) = "loadinifile(" ++ show path ++ ")"
siToLuaString (SISetViewpoint viewpoint) = "setviewpoint(" ++ show (viewpoint ^. viewpoint5Name) ++ ")" -- relies on the viewpoint being preloaded in ini
siToLuaString (SISetSceneClip axis clipMin min clipMax max) = (case axis of
    X -> "set_sceneclip_x("
    Y -> "set_sceneclip_y("
    Z -> "set_sceneclip_z("
    ) ++ (if clipMin then "1" else "0") ++ "," ++ show min ++ "," ++ (if clipMax then "1" else "0") ++ "," ++ show max ++ ")"

scriptToSSFString :: Script -> String
scriptToSSFString (Script instructions) =  unlines $ map siToSSFString instructions

scriptToLuaString :: Script -> String
scriptToLuaString (Script instructions) =  unlines $ map siToLuaString instructions

siToSSFString :: ScriptInstruction -> String -- this string may include new lines, but does not end in one
siToSSFString (SILoadFile path) = "LOADFILE\n " ++ path
siToSSFString (SILoadVFile path) = "LOADVFILE\n " ++ path
siToSSFString (SIRender name) = "RENDERONCE\n " ++ name
siToSSFString (SISetTime time) = "SETTIMEVAL\n " ++ show time
siToSSFString (SIUnloadAll) = "UNLOADALL\n "
siToSSFString (SISetRenderDir path) = "RENDERDIR\n " ++ path
siToSSFString (SILoadIniFile path) = "LOADINIFILE\n " ++ path
siToSSFString (SISetViewpoint viewpoint) = "SETVIEWPOINT\n " ++ (viewpoint ^. viewpoint5Name) -- relies on the viewpoint being preloaded in ini


produceScreenScript :: FilePath -> FilePath -> LoadedSet -> [ScriptInstruction]
produceScreenScript baseIniPath screenSetPath set@(LoadedSet name dataLoads setups) =
    [ SIUnloadAll
    ] ++ produceDataLoads dataLoads ++ (concatMap (produceScreenSetupCode baseIniPath loadedSetDir) setups)
  where
    loadedSetDir = joinPath [screenSetPath, name]

produceDataLoads :: [LoadData] -> [ScriptInstruction]
produceDataLoads dataLoads = map produceDataLoad dataLoads

produceDataLoad :: LoadData -> ScriptInstruction
produceDataLoad dataLoad = case dataLoad of
    LoadFilePath path -> SILoadFile path
    LoadVecFilePath path -> SILoadVFile path
  -- LoadFilePath path -> "loaddatafile(\"" ++ path ++ "\")"
  -- LoadVecFilePath path -> "loaddatavfile(\"" ++ path ++ "\")"

produceScreenSetupCode :: FilePath -> FilePath -> SetupSet -> [ScriptInstruction]
produceScreenSetupCode baseIniPath loadedSetPath setup@(SetupSet name conf times) =
  [ SISetRenderDir setupPath
  , SILoadIniFile baseIniPath
  ] ++ produceSetupChanges conf ++ (concatMap produceScreenTimeCode times)
  where
    setupPath = joinPath [loadedSetPath, name]
  -- [ "setrenderdir(\"" ++ setupPath ++ "\")"
  -- , "loadinifile(\"" ++ baseIniPath ++ "\")"
  -- ] ++ produceSetupChanges conf ++ (concatMap produceScreenTimeCode times)
  -- where
    -- setupPath = joinPath [loadedSetPath, name]

produceSetupChanges :: RenderSetup -> [ScriptInstruction]
produceSetupChanges (RenderSetup settings viewpoint) = [SISetViewpoint viewpoint] ++ concatMap produceSettingChange settings -- TODO: not yet implemented

produceSettingChange :: ConfigSetting -> [ScriptInstruction]
produceSettingChange (DoScriptInstruction x) = [x]

produceScreenTimeCode :: Double -> [ScriptInstruction]
produceScreenTimeCode time = [SISetTime time, SIRender (show time ++ "s")]
-- ["settime(" ++ show time ++ ")", "render()"]
