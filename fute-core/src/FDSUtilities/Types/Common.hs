{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
module FDSUtilities.Types.Common where

data Version = Version
    Int
    Int
    Int
    deriving (Eq, Ord) -- Major version  Minor version  Maintenance version

instance Show Version where
    show (Version major minor maint) = show major ++ "." ++ show minor ++ "." ++ show maint
    