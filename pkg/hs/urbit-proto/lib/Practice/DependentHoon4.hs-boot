{-# LANGUAGE RoleAnnotations #-}

module Practice.DependentHoon4 where

import ClassyPrelude

class Peg a

type Var a = (Eq a, Ord a, Show a, Peg a)

data Soft
data Code a
data Semi a
data Stub
data Pelt
data Fish
data Con a
data Line a
data Cube a
data Warp
data Weft a

data Fail

type role Semi representational
type role Con  representational
type role Line representational
type role Cube representational
type role Weft nominal
