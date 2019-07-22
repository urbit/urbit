module Types where

import ClassyPrelude

import Data.Time.Clock.System

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
data SocketType = SocketType
  deriving (Show)

type Message = String

-------------------------------------------------------------------------------

-- | A minimally simulated execution environment for Nuevo instances
--
-- This is not part of Nuevo proper, but is meant to be the Vere equivalent
-- which sends messages to each
data VereEnv = VereEnv
  { instances :: M.Map NodeId InstanceThread
  , drivers   :: M.Map String IoDriver

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

-- | Represents all things which can send or receive messages
--
data NodeId
  -- | TopNode represents the part of Vere that owns the other side of the root
  -- pipe.
  = TopNodeId
  -- | ProcessNodeId represents a unique instance of nuevo. Instances of Nuevo
  -- form a DAG by Path and have an instance number which increments on process
  -- termination.
  | ProcessNodeId Path Int
  -- | IoDrivers are modules of Vere which actually perform IO effects.
  | IoDriver String
  deriving (Show, Eq, Ord)

-- | A typed socket which data is sent over.
--
-- Was: "Handle"
data Socket
  = Socket
  { socketId           :: Int
  , socketCreator      :: NodeId
  , socketCounterparty :: NodeId
  }
  deriving (Show, Eq, Ord)


data NuevoEvent
  -- | Initializes a new process
  = NEvInit
  { nevInitParent     :: NodeId
  , nevInitName       :: Path  -- ^ The path name of this process
  , nevInitInstance   :: Int   -- ^ Unique sequence number ; life of the process
  -- todo: logged
  , nevInitProgram    :: NuevoProgram
  , nevInitSentOver   :: Socket
  , nevInitSocketType :: SocketType
  , nevInitMessage    :: Message
  }

  | NEvRecv
  { nevRecvSentOver :: Socket
  , nevRecvMessage  :: Message
  }
  deriving (Show)

-- | Each instance of a Nuevo kernel.
data NuevoState = NuevoState
  { nsParent       :: NodeId
  , nsName         :: Path
  , nsInstanceNum  :: Int
  , nsChildren     :: M.Map String Socket
  , nsProgram      :: NuevoProgram
  , nsProgramState :: ProgramState
  , nsNextBone     :: Int
  , nsSocketToBone :: B.Bimap Socket Int
  , nsSocketToType :: M.Map Socket SocketType
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
type NuevoFunction =  NuevoState
                   -> OvEvent
                   -> (NuevoState, [OvEffect])

type IoDriver = NuevoEvent -> IO [OvEffect]

instance Show IoDriver where
  show _ = "<io driver>"

--  TODO: Unify NuevoEffect/Event
data OvEvent  = OvEvent
  { eventNow    :: SystemTime
  , eventTarget :: NodeId
  , eventEvent  :: NuevoEvent }

data OvEffect = OvEffect
  { effectTarget :: NodeId,
    outputEvent  :: NuevoEvent }
              deriving (Show)

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
  , peForkHandle  :: SocketType
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

