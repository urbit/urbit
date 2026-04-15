::  %eval-command mark
::
::    Payload for an %eval-command poke on any %sole/%shoe agent.
::
::    ses: the @ta session name used when subscribing to /sole/[ship]/[ses]
::    src: raw input tape (same syntax as the agent's interactive REPL)
::
::    The caller must already hold a subscription to /sole/[ship]/[ses]
::    (which creates and anchors the session).  Results arrive as
::    %sole-effect facts on that subscription; %sole-effect %pro
::    signals that the command has finished executing.
::
|_  [ses=@ta src=tape]
++  grad  %noun
++  grow
  |%
  ++  noun  [ses src]
  --
++  grab
  |%
  ++  noun  ,[ses=@ta src=tape]
  ++  json
    |=  jon=^json
    ^-  [ses=@ta src=tape]
    =,  dejs:format
    =/  res  ((ot ses+so src+so ~) jon)
    [`@ta`-.res (trip +.res)]
  --
--
