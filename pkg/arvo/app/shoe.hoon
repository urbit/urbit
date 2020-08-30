::  shoe: example usage of /lib/shoe
::
::    the app supports one command: "demo".
::    running this command renders some text on all sole clients.
::
/+  shoe, verb, dbug, default-agent
|%
+$  state-0  [%0 ~]
+$  command  ~
::
+$  card  card:shoe
--
=|  state-0
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
%-  (agent:shoe command)
^-  (shoe:shoe command)
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    des   ~(. (default:shoe this command) bowl)
::
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this]
::
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
::
++  command-parser
  |=  sole-id=@ta
  ^+  |~(nail *(like [? command]))
  (cold [& ~] (jest 'demo'))
::
++  tab-list
  |=  sole-id=@ta
  ^-  (list [@t tank])
  :~  ['demo' leaf+"run example command"]
  ==
::
++  on-command
  |=  [sole-id=@ta =command]
  ^-  (quip card _this)
  =-  [[%shoe ~ %sole -]~ this]
  =/  =tape  "{(scow %p src.bowl)} ran the command"
  ?.  =(src our):bowl
    [%txt tape]
  [%klr [[`%br ~ `%g] [(crip tape)]~]~]
::
++  can-connect
  |=  sole-id=@ta
  ^-  ?
  ?|  =(~zod src.bowl)
      (team:title [our src]:bowl)
  ==
::
++  on-connect      on-connect:des
++  on-disconnect   on-disconnect:des
--