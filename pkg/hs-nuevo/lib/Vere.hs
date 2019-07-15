module Vere where

import ClassyPrelude

import qualified Data.Map as M
import           Nuevo
import           Types


-- TODO: This isn't the way to handle this long term, but for now, we run over
-- each instance
-- vereStep :: VereEnv -> IO VereEnv
-- vereStep ve = do
--   M.mapAccum runThread (instances ve)
--   pure ()



runInstanceStep :: NuevoState -> NuevoEvent -> IO ()
runInstanceStep ns ne = do
  let (newState, effects) = runNuevoFunction ns ne
  pure ()

  -- We now have a list of NuevoEffects, which are IO requests.



-- Nuevo has given us an effect and we must react to it.
handleNuevoEffect :: Connection -> VereEnv -> NuevoEffect -> VereEnv
handleNuevoEffect self@(ProcessConnection (Path path) seq) env = \case
  NEfSend p@(PipeSocket _ _ target) msg ->
    -- This puts a corresponding recv in the mailbox of the counterparty
    enqueueEvent env target (NEvRecv (flipSocket self p) msg)

  -- Sending to an io driver is an IO event; we need to handle this.
  NEfSend (IoSocket id driver) msg -> undefined

  -- Forking a new process
  NEfFork{..} -> env
    { instances = M.insert newConnection newInstance (instances env)
    }
    where
      -- TODO: Allocate the socket representation here and return to the caller
      -- in case %init passes.
      newConnection = ProcessConnection newName 0
      newInstance =
        InstanceThread
        [NEvInit newConnection newName neForkProgram newProcessSocket neForkMessage]
        (itNuevoState oldState)
        []
      newName = Path (path ++ [neForkName])
      -- TODO: Make a valid socket instead of a dummy value
      newProcessSocket = PipeSocket 5 self self
      (Just oldState) = M.lookup self (instances env)



    -- we add a new entry o



enqueueEvent :: VereEnv -> Connection -> NuevoEvent -> VereEnv
enqueueEvent env con event =
  env{instances=newInstances}
  where
    newInstances = M.adjust changeInstance con (instances env)
    changeInstance i@InstanceThread{..} =
      i{itEventQueue = itEventQueue ++ [event]}


flipSocket :: Connection -> Socket -> Socket
flipSocket self p@PipeSocket{..} = p{pipeCounterparty = self}
flipSocket _    i@IoSocket{..}   = i
