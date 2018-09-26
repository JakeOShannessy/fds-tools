{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
module FDSUtilities.Class where

import qualified Data.Map as M

class Mapping k v m | m -> k, m -> v where
    empty :: m
    insert :: k -> v -> m -> m
    delete :: k -> m -> m
    (!) :: m -> k -> v
    (\\) :: m -> m -> m
    null :: m -> Bool
    size :: m -> Int
    member :: k -> m -> Bool
    notMember :: k -> m -> Bool
    lookup :: k -> m -> Maybe v

instance Ord k => Mapping k v (M.Map k v) where
    (!) = (M.!)
    (\\) = (M.\\)
    null = M.null
    size = M.size
    member = M.member
    notMember = M.notMember
    lookup = M.lookup
    empty = M.empty
    insert = M.insert
    delete = M.delete
    
