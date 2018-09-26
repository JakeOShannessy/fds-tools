{-# LANGUAGE TemplateHaskell #-}
module FDSUtilities.Types.Screenshots where

import Data.Default
import Control.Lens
import FDSUtilities.Types.Smokeview
import FDSUtilities.Types.Simulation

-- data ScreenCompilationSet = ScreenCompilationSet
    -- { _compIniConfig :: IniConfig
    -- , _compRenders :: [Render]
    -- }


    
-- toScript :: ScreenCompilationSetLua -> Script
-- toScript (ScreenCompilationSetLua name )
    
-- the whole complation set includes a default iniconfig so that we can be
-- confident as to what the defaults are. This config is loaded at the start
-- of the run.
data ScreenCompilationSet = ScreenCompilationSet String IniConfig [LoadedSet] deriving Show
data LoadedSet = LoadedSet String [LoadData] [SetupSet] deriving Show
data SetupSet = SetupSet String RenderSetup [Double] deriving Show

-- instance Default ScreenCompilationSet where
    -- def = ScreenCompilationSet
            -- { _compIniConfig = error "compIniConfig not set"
            -- , _compRenders = error "compRenders not set"
            -- }

-- data Render = Render
    -- { _renderData :: [LoadData]
    -- , _renderTimes  :: [Double]
    -- } deriving Show

data Render = Render
  { _renderData :: [LoadData]
  , _renderSetup :: RenderSetup
  , _renderTimes :: [Double]
  } deriving Show

instance Default Render where
  def = Render
        { _renderData = error "renderLuaData not set"
        , _renderSetup = error "renderLuaSetup not set"
        , _renderTimes = error "renderLuaTimes not set"
        }

-- instance Default Render where
    -- def = Render
            -- { _renderData = error "renderData not set"
            -- , _renderTimes  = error "renderTimes not set"
            -- }

-- |Sets what data is loaded in Smokeview. This must be capable of representing
-- all possible data configurations.
-- TODO: need generic way of specifying data to be loaded.
-- loading via filenames is the least abstracted, and all configurations
-- should be possible with these two types. All data can be loaded using
-- LoadFilePath, with the exception that to show vector entries LoadVe
-- loading via filenames is the least abstracted, and all configurations
-- should be possible with these two types. All data can be loaded using
-- information from the SMV file. Although using this information
-- currently has to be done manually by the compilation script.
data LoadData
  = LoadFilePath FilePath -- load using the filename (relative to the smv file)
  | LoadVecFilePath FilePath -- load vectors using the filename, smokeview
                             -- retrieving the additional files
  deriving Show
-- |This includes all config and view data. As they can both be changed on the
-- fly with no difference in computation load they belong together.
data RenderSetup = RenderSetup
  { _renderSetupConfig :: [ConfigSetting] -- simply a list of changes to make
                                             -- these will be converted directly
                                             -- to lua script.
  , _renderSetupView :: Viewpoint5 -- is it necessary for this to be a separate entry?
  } deriving Show

-- |This should represent every possible config setting that can be set in lua.
data ConfigSetting =
    DoScriptInstruction ScriptInstruction
    deriving Show

-- |Sets what data is loaded in Smokeview.
-- data LoadData
    -- = SLCFLoad String Axis Double
    -- | SLCFLoadVec String Axis Double
    
    -- | BNDFLoad String Axis-- Double
    -- -- | PRT5Load String Double
    
    -- | SMOKE3DLoad String
    
    -- TODO: configure the "show" settings for PL3D
    -- | PL3DLoad String Double
    
    -- deriving Show

makeLenses ''ScreenCompilationSet
makeLenses ''Render



-- instance Show ScreenCompilationSet where
    -- show s = show $ s^.compRenders
