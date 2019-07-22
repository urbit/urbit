module Types where

import ClassyPrelude

import qualified Data.Bimap as B
import qualified Data.Map   as M

-- This entire prototype is only to show off the process model to reason about
-- whether things just deadlock. So our "vase" is just a list of text
-- fragments. Thats all you get on your typed channels.
data Vase = Vase [String]

-- A list of path elements
data Path = Path [String]
  deriving (Show, Eq, Ord)

-- The typed message data that goes on a handle. Currently unimplemented
data HandleType = HandleType
  deriving (Show)

type Message = String

-------------------------------------------------------------------------------

-- | A minimally simulated execution environment for Nuevo instances
--
-- This is not part of Nuevo proper, but is meant to be the Vere equivalent
-- which sends messages to each
data VereEnv = VereEnv
  { instances :: M.Map NodeId InstanceThread

  }
  deriving (Show)


data InstanceThread = InstanceThread
  -- TODO: eventually, event queue needs to be some sort of STMed thing between
  -- threads. Right now, it is a list queue.
  { itEventQueue :: [NuevoEvent]
  , itNuevoState :: NuevoState
  -- TODO: keep track of latest event number
  , itEventLog   :: [(Int,NuevoEvent)]
  }
  deriving (Show)

-------------------------------------------------------------------------------

-- | Represents a node in the Nuevo DAG
--
-- Nuevo instances and Vere itself form a DAG.
--
data NodeId
  = TopNodeId
  | ProcessNodeId Path Int
  deriving (Show, Eq, Ord)

-- | Was: "Handle"
data Socket
  = PipeSocket
  { pipeId           :: Int
  , pipeCreator      :: NodeId
  , pipeCounterparty :: NodeId
  }

  | IoSocket
  { ioId     :: Int
  , ioDriver :: String
  }

  deriving (Show, Eq, Ord)



data NuevoEvent
  = NEvInit
  { nevInitId       :: NodeId
  , nevInitName     :: Path
  , nevInitProgram  :: NuevoProgram
  , nevInitSentOver :: Socket
  , nevInitMessage  :: Message
  }

  | NEvRecv
  { nevRecvSentOver :: Socket
  , nevRecvMessage  :: Message
  }
  deriving (Show)

data NuevoEffect
  = NEfFork
  { neForkName    :: String
  , neForkLogged  :: Bool
  , neForkProgram :: NuevoProgram
  , neForkHandle  :: HandleType
  , neForkMessage :: Message
  }
--  | NEfTerminate
  | NEfSend Socket Message
  deriving (Show)

-- | Each instance of a Nuevo kernel.
data NuevoState = NuevoState
  { nsParent       :: NodeId
  , nsName         :: Path
--  , nsChildren :: M.Map String
  , nsProgram      :: NuevoProgram
  , nsProgramState :: ProgramState
  , nsNextBone     :: Int
  , nsSocketToBone :: B.Bimap Socket Int
  }
  deriving (Show)

emptyNuevoState :: NuevoState
emptyNuevoState = NuevoState
  { nsParent = TopNodeId
  , nsName = Path []
  , nsProgram = emptyProgram
  , nsProgramState = M.empty
  , nsNextBone = 1
  , nsSocketToBone = B.empty
  }


--  Processes a single Nuevo event
type NuevoFunction = NuevoState -> NuevoEvent -> (NuevoState, [NuevoEffect])


-------------------------------------------------------------------------------

-- Types for the program running under Nuevo

data ProgramEvent
  = PEvRecv
  { peRecvBone    :: Int
  , peRecvMessage :: Message
  }
  deriving (Show, Eq)

data ProgramEffect
  = PEfSend
  { peSendBone    :: Int
  , peSendMessage :: Message
  }
  | PEfFork
  { peForkName    :: String
  , peForkLogged  :: Bool
  , peForkProgram :: NuevoProgram
  , peForkHandle  :: HandleType
  , peForkMessage :: Message
  }
  deriving (Show)

-- TODO: A realer state.
type ProgramState = M.Map String String

-- | The type of a program that nuevo runs.
type NuevoProgram = (ProgramState, ProgramEvent) -> (ProgramState, [ProgramEffect])

instance Show NuevoProgram where
  show _ = "<nuevo program>"

emptyProgram :: NuevoProgram
emptyProgram (a, _) = (a, [])

