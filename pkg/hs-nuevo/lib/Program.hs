module Program where

import ClassyPrelude

import Types

unsafeMessagePrintingProgram :: NuevoProgram
unsafeMessagePrintingProgram (state, event) =
  trace ("Prog trace: bone=" ++ (show (peRecvBone event)) ++ ", msg=" ++ (peRecvMessage event))
  (state, [])


spawnsAnUnsafeMessagePrintingProgram :: NuevoProgram
spawnsAnUnsafeMessagePrintingProgram (state, event) =
  trace ("Spawn prog trace: bone=" ++ (show (peRecvBone event)) ++ ", msg=" ++ (peRecvMessage event))
  (state, [PEfFork "child" False unsafeMessagePrintingProgram HandleType "forking"])
