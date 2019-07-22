module Nuevo where

import ClassyPrelude

import Types

import qualified Data.Bimap as B
import qualified Data.Map   as M


-- The next step here is figuring out flow control. So we can apply one
-- manually created event to a NuevoState. This doesn't yet take the output
-- NuevoEffects and apply them to the Vere structure, which then doesn't
-- perform action based on them.
--
-- Vere should take the output NuevoEffects and then actually respond to
-- them. We have the translation from internal programs to nuevo effects, but
-- nothing then responds to those effects.

runNuevoFunction :: NuevoFunction

-- The NEvInit function just changes the program identity and then
runNuevoFunction oldState (OvEvent a b NEvInit{..}) =
  runNuevoFunction newState (OvEvent a b (NEvRecv nevInitSentOver nevInitMessage))
  where
    newState = oldState
      { nsParent  = nevInitParent
      , nsName    = nevInitName
      , nsInstanceNum = nevInitInstance
      , nsChildren = M.empty
      , nsProgram = nevInitProgram
      , nsProgramState = M.empty
      -- TODO: Use random numbers for unguessability in the real implementation
      , nsNextBone = 2
      , nsSocketToBone = B.singleton nevInitSentOver 1
      , nsSocketToType = M.singleton nevInitSentOver nevInitSocketType
      }

-- A recv takes a message and gives it to the program to make a list of effects
runNuevoFunction oldNuevoState@NuevoState{..} (OvEvent _ _ NEvRecv{..}) =
  (newNuevoState, nuevoEffects)
  where
    -- TODO: Check for invalid incoming sockets.
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
-- TODO: Yes, PEfFork needs to increment next socket.
programEffectsToNuevoEffect :: NuevoState -> ProgramEffect -> OvEffect
programEffectsToNuevoEffect ns@NuevoState{..} = \case
  PEfSend bone msg -> OvEffect (socketCounterparty target) (NEvRecv target msg)
    where target = (nsSocketToBone B.!> bone)

  PEfFork{..} ->
    OvEffect
    TopNodeId
    NEvInit
    { nevInitParent = (nuevoToId ns)
    , nevInitName = pathAppend nsName peForkName
    -- TODO: Calculate the right instance number when we get to crashing.
    , nevInitInstance = 0
    , nevInitProgram = peForkProgram
    -- TODO: Allocate a real socket number here.
    , nevInitSentOver = Socket 0 (nuevoToId ns) (nuevoToId ns)
    , nevInitSocketType = peForkHandle
    , nevInitMessage = peForkMessage
    }


--
nuevoToId :: NuevoState -> NodeId
nuevoToId ns = ProcessNodeId (nsName ns) (nsInstanceNum ns)

pathAppend :: Path -> String -> Path
pathAppend (Path a) b = Path (a ++ [b])
