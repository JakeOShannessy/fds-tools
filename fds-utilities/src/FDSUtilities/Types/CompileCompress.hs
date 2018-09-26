{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module FDSUtilities.Types.CompileCompress where

import Control.Lens
import Data.Default
import Data.Monoid
import FDSUtilities.Types.Simulation
import Text.Blaze.Html
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

-- |A Compilation is an 'FDSSimulation' with a number of 'CompilationAction's associated with it.
data Compilation = Compilation
        { _compilationSimulation :: FDSSimulation
        , _compilationActions :: [CompilationAction]
        }
instance Default Compilation where
    def = Compilation
            { _compilationSimulation = error "compilationSimulation not set"
            , _compilationActions = error "compilationActions not set"
            }

-- |A 'CompilationAction' is an action which takes an simulation and produces compiled
--  files. The function returns the local location of these files.
data CompilationAction = CompilationAction
        { _compilationActionFunction :: CompilationActionFunction
            -- ^ This function takes a path to place results locally and the simulation.
            -- Returns file produced relative to localpath (not localpath/chid).
        , _compilationActionPath     :: FilePath
            -- ^This directory path is relative to the localPath/chid set in the Compilation
        }
instance Default CompilationAction where
    def = CompilationAction
            { _compilationActionFunction = error "compilationActionFunction  not set"
            , _compilationActionPath = error "compilationActionPath not set"
            }

-- |A CompilationActionFunction is a function that takes a results directory
-- and a simulation, and returns the locations of it outputs.
type CompilationActionFunction = (FilePath -> FDSSimulation -> IO [FilePath])

data Page = Page
    { pageTitle :: String
    , pageCSS :: String
    , pageScript :: String
    , pageContent :: H.Html
    }

-- instance Monoid Page where

instance ToMarkup Page where
    toMarkup page = H.docTypeHtml $ do
        H.head $ do
            H.title $ H.toHtml $ pageTitle page
            H.style $ H.toHtml $ pageCSS page
            H.script H.! A.type_ "text/javascript" $ H.preEscapedToHtml $ pageScript page
        H.body $ do
            H.h1 $ H.toHtml $ pageTitle page
            H.br
            pageContent page



makeLenses ''Compilation
makeLenses ''CompilationAction


