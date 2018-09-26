module FDSUtilities.Smokeview.SSF
    ( makeSSFInd
    , mkRenderName -- TODO: this is the incorrect location for this function.
    ) where

import FDSUtilities.Types.Smokeview
import System.FilePath
import Text.Printf
    
-- makeSSF :: LoadData -> [Double] -> String -> String
-- makeSSF loadData times casename =  "RENDERDIR\n"
    -- ++ " " ++ "\n"
    -- ++ load loadData
    -- ++ concat (map (rendering casename loadData) times)
    
-- |Make an SSF that load each slice individually rather than relyin on multi-slices
makeSSFInd :: {-LoadData ->-} [(FilePath, Bool)] -> [Double] -> String -> String
makeSSFInd {-loadData-} filenames times casename =  "RENDERDIR\n"
    ++ " " ++ "\n"
    ++ concatMap (loadFile) filenames
    ++ concat (map (rendering casename {-loadData-}) times)
    
-- |Make a lua script.
-- makeLuaScript :: -> String

-- removed the space padding to match the latest smokeview version.
mkRenderName casename {-loadData-} time = (printf "%d" ((round time) :: Int)) ++ "s"
-- mkRenderName casename {-loadData-} time = (printf "%04.0d" ((round time) :: Int)) ++ "s"


rendering casename {-loadData-} time = setTime time ++ "RENDERONCE\n" ++ " " ++ mkRenderName casename {-loadData-} time ++ "\n"

-- load (Slice name axis location) = "LOADSLICE\n " ++ name ++ "\n " ++ show (axisToNum axis) ++ " " ++ show location ++ "\n"

loadFile (filename, vec)
  | vec       = "LOADVFILE\n " ++ takeFileName filename ++ "\n"
  | otherwise = "LOADFILE\n " ++ takeFileName filename ++ "\n"

setTime time = "SETTIMEVAL\n " ++ show time ++ "\n"

axisToNum axis = case axis of
                    X -> 1
                    Y -> 2
                    Z -> 3
