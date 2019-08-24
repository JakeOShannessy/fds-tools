{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
module FDSUtilities.Types
    ( module FDSUtilities.Types
    , module FDSUtilities.Types.Common
    , module FDSUtilities.Types.Data
    , module FDSUtilities.Types.Fire
    , module FDSUtilities.Types.OutFile
    , module FDSUtilities.Types.Simulation
    , module FDSUtilities.Types.Screenshots
    , module FDSUtilities.Types.Smokeview
    , module FDSUtilities.Parsing.OutFile.Types
    ) where

import qualified FDSUtilities.Class as FC

import FDSUtilities.Types.Common
import FDSUtilities.Types.Data
import FDSUtilities.Types.Fire
import FDSUtilities.Types.OutFile
import FDSUtilities.Types.Simulation
import FDSUtilities.Types.Screenshots
import FDSUtilities.Types.Smokeview
import FDSUtilities.Parsing.OutFile.Types

headErr :: String -> [p] -> p
headErr err var = case var of
    [] -> error err
    (x:_) -> x
