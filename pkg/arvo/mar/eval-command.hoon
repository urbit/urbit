::  %eval-command mark
::
::    Payload for the %dojo %eval-command poke.
::
::    ses: the @ta session name used when subscribing to /sole/[ship]/[ses]
::    src: full Dojo command tape (same syntax as the terminal REPL)
::
::    The caller must already hold a subscription to /sole/[ship]/[ses]
::    (which creates and anchors the Dojo session).  Results arrive as
::    %sole-effect %tan facts on that subscription; %sole-effect %pro
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
