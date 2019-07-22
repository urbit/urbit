module Vere where

import ClassyPrelude

import Data.Time.Clock.System

import qualified Data.Map as M
import           Nuevo
import           Program
import           Types

initialVereEnv :: NuevoProgram -> VereEnv
initialVereEnv np =
  VereEnv
  { instances = M.singleton firstProcessId firstInstance
  , drivers   = M.singleton "base" terminalDriver
  }
  where
    firstProcessId = (ProcessNodeId (Path []) 0)
    firstInstance = InstanceThread
      { itEventQueue =
        [ NEvInit
          TopNodeId
          (Path [])
          0
          np
          (Socket 0 TopNodeId TopNodeId)
          SocketType
          "init"
        ]
      , itNuevoState = emptyNuevoState
      , itEventLog = []
      }

-- TODO: This isn't the way to handle this long term, but for now, we run over
-- each instance
vereStep :: VereEnv -> IO VereEnv
vereStep ve = do
  now <- getSystemTime

  let (effects, newInstances) =
        M.mapAccumWithKey (accumExec now) [] (instances ve)

  print ("instances: " ++ (show newInstances))
  print ("effects: " ++ (show effects))

  foldlM
    handleOvEffect
    ve{instances=newInstances}
    effects


-- | For every instance of nuevo, run one event and accumulate the effects.
accumExec :: SystemTime
          -> [OvEffect]
          -> NodeId
          -> InstanceThread
          -> ([OvEffect], InstanceThread)
accumExec now previousEffects id it@(InstanceThread [] _ _)
  = (previousEffects, it)

accumExec now previousEffects id InstanceThread{..}
  = (previousEffects ++ newEffects, newInst)
  where
    (event : restEvents) = itEventQueue
    (newState, newEffects) =
      runNuevoFunction itNuevoState (OvEvent now id event)
    newInst = InstanceThread
      { itEventQueue = restEvents
      , itNuevoState = newState
      , itEventLog = (0, event):itEventLog
      }

-- | Handles an OvEffect. In IO because of the handling of IoDriver and
-- TopNodeId.
handleOvEffect :: VereEnv -> OvEffect -> IO VereEnv

-- TODO: This should be distinct from IoDriver, but until I have passing
-- sockets over sockets working, do this.
handleOvEffect env (OvEffect TopNodeId msg)                 =  do
  let (Just fun) = M.lookup "base" (drivers env)
  effects <- (fun msg)
  pure (env)

-- TODO: Must handle the Init specially, so that it creates an entry
handleOvEffect env (OvEffect (ProcessNodeId path id) NEvInit{..})
  = undefined

-- TODO: Must figure out how to integrate the terminated here.

handleOvEffect env (OvEffect (ProcessNodeId path id) msg) = do
  -- There are two things that need special handling: fork and
  -- terminate. Everything else is just enqueued to the right process, erroring
  -- if it doesn't exist.
  print ("msg: " ++ (show msg))
  pure (env)

handleOvEffect env (OvEffect (IoDriver driver) msg)       = do
  let (Just fun) = M.lookup driver (drivers env)
  effects <- (fun msg)
  -- TODO: Take the effects from the driver and then run them immediately,
  -- since they're almost assuredly the things we send back to the program.
  pure (env)


-- -- Nuevo has given us an effect and we must react to it.
-- handleNuevoEffect :: NodeId -> VereEnv -> NuevoEffect -> IO VereEnv
-- handleNuevoEffect self@(ProcessNodeId (Path path) seq) env = \case
--   NEfSend p@(PipeSocket _ _ target) msg ->
--     -- This puts a corresponding recv in the mailbox of the counterparty
--     pure $ enqueueEvent env target (NEvRecv (flipSocket self p) msg)

--   -- Sending to an io driver is an IO event
--   s@(NEfSend (IoSocket id driver) msg) -> do
--     print ("driver: " ++ driver)
--     let (Just fun) = M.lookup driver (drivers env)
--     -- TODO: Do things that the driver says
--     effects <- (fun s)
--     pure (env)

--   -- Forking a new process
--   NEfFork{..} -> pure $ env
--     { instances = M.insert newNodeId newInstance (instances env)
--     }
--     where
--       -- TODO: Allocate the socket representation here and return to the caller
--       -- in case %init passes.
--       newNodeId = ProcessNodeId newName 0
--       -- TODO: Actually copy the old state
--       newInstance = InstanceThread
--         { itEventQueue =
--           [NEvInit newNodeId newName neForkProgram newProcessSocket neForkMessage]
--         , itNuevoState = emptyNuevoState
--         , itEventLog = []
--         }
--       newName = Path (path ++ [neForkName])
--       -- TODO: Make a valid socket instead of a dummy value
--       newProcessSocket = Socket 5 self self


-- enqueueEvent :: VereEnv -> NodeId -> NuevoEvent -> VereEnv
-- enqueueEvent env con event =
--   env{instances=newInstances}
--   where
--     newInstances = M.adjust changeInstance con (instances env)
--     changeInstance i@InstanceThread{..} =
--       i{itEventQueue = itEventQueue ++ [event]}


terminalDriver :: IoDriver
terminalDriver (NEvRecv _ msg) = do
  print ("term: " ++ msg)
  pure ([])

-- flipSocket :: NodeId -> Socket -> Socket
-- flipSocket self p@PipeSocket{..} = p{pipeCounterparty = self}
-- flipSocket _    i@IoSocket{..}   = i
