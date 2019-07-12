module Types where

import ClassyPrelude

import Data.Void

import qualified Data.Map as M


-- This entire prototype is only to show off the process model to reason about
-- whether things just deadlock. So our "vase" is just a list of text
-- fragments. Thats all you get on your typed channels.
data Vase = Vase [Text]

-- A list of path elements
data Path = Path [Text]
  deriving (Show, Eq)


-------------------------------------------------------------------------------

-- | A minimally simulated execution environment for Nuevo instances
--
-- This is not part of Nuevo proper, but is meant to be the Vere equivalent
-- which sends messages to each 
data VereEnv = VereEnv
  { instances :: M.Map Connection NuevoState

  }


-------------------------------------------------------------------------------

-- | 
data Connection
  = TopConnection
  | ProcessConnection Path Int
  deriving (Show, Eq)

data NuevoEvent
  = NEvInit Int
  deriving (Show, Eq)

data NuevoEffect
  = NEfFork
  | NEfTerminate
  deriving (Show, Eq)

-- | Each instance of 
data NuevoState = NuevoState
  { nsProgram :: NuevoProgram
  }
  deriving (Show, Eq)


--  Processes a single Nuevo event 
type NuevoFunction = (NuevoState,NuevoEvent) -> (NuevoState,[NuevoEffect])


-- The type of a program that nuevo runs.
type NuevoProgram = Void
