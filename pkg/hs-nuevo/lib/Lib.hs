module Lib where

import ClassyPrelude

import Types

import qualified Data.Bimap as B
import qualified Data.Map as M


-- spawn :: VereEnv -> NuevoState -> NuevoEvent -> 
-- spawn v oldNuevo init@NEvInit{..} = 

--   -- We are spawning a new copy of the kernel off an old copy of the kernel,
--         oldNuevo
--         { nsParent  = nevInitConnection
--         , nsName    = nevInitName
--         , nsProgram = nevInitProgram
--         }

runNuevoFunction :: NuevoFunction

-- The NEvInit function just changes the program identity and then 
runNuevoFunction (oldState, NEvInit{..}) =
  runNuevoFunction (newState, NEvRecv nevInitSentOver nevInitMessage)
  where
    newState = oldState
      { nsParent  = nevInitConnection
      , nsName    = nevInitName
      , nsProgram = nevInitProgram
      , nsProgramState = M.empty
      -- TODO: Use random numbers for unguessability in the real implementation
      , nsNextBone = 2
      , nsSocketToBone = B.singleton nevInitSentOver 1
      }

-- A recv takes a message and gives it to the program to make a list of effects
runNuevoFunction (oldNuevoState@NuevoState{..}, NEvRecv{..}) =
  (newNuevoState, nuevoEffects)
  where
    bone = nsSocketToBone B.! nevRecvSentOver

    (newProgramState, programEffects) =
      (nsProgram (nsProgramState, PEvRecv bone nevRecvMessage))

    -- TODO: this depends on the effects; we may need to allocate new bones for
    -- new sockets.
    newNuevoState = oldNuevoState
      { nsProgramState = newProgramState
      }

    nuevoEffects = map (programEffectsToNuevoEffect newNuevoState) programEffects


-- TODO: This is probably stateful to NuevoState?
programEffectsToNuevoEffect :: NuevoState -> ProgramEffect -> NuevoEffect
programEffectsToNuevoEffect NuevoState{..} = \case
  PEfSend bone msg -> NEfSend (nsSocketToBone B.!> bone) msg
