::  shoe: example usage of /lib/shoe
::
::    the app supports one command: "demo".
::    running this command renders some text on all sole clients.
::
/+  shoe, verb, dbug, default-agent
|%
+$  state-0  [%0 ~]
+$  command
  $?  %demo
      %row
      %table
  ==
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
  %+  stag  &
  (perk %demo %row %table ~)
::
++  tab-list
  |=  sole-id=@ta
  ^-  (list [@t tank])
  :~  ['demo' leaf+"run example command"]
      ['row' leaf+"print a row"]
      ['table' leaf+"display a table"]
  ==
::
++  on-command
  |=  [sole-id=@ta =command]
  ^-  (quip card _this)
  =;  [to=(list _sole-id) fec=shoe-effect:shoe]
    [[%shoe to fec]~ this]
  ?-  command
      %demo
    :-  ~
    :-  %sole
    =/  =tape  "{(scow %p ship.src.bowl)} ran the command"
    ?.  =(ship.src our):bowl
      [%txt tape]
    [%klr [[`%br ~ `%g] [(crip tape)]~]~]
  ::
      %row
    :-  [sole-id]~
    :+  %row
      ~[8 27 35 5]
    ~[p+ship.src.bowl da+now.bowl t+'plenty room here!' t+'less here!']
  ::
      %table
    :-  [sole-id]~
    :^  %table
        ~[t+'ship' t+'date' t+'long text' t+'tldr']
      ~[8 27 35 5]
    :~  ~[p+ship.src.bowl da+now.bowl t+'plenty room here!' t+'less here!']
        ~[p+~marzod t+'yesterday' t+'sometimes:\0anewlines' t+'newlines']
    ==
  ==
::
++  can-connect
  |=  sole-id=@ta
  ^-  ?
  ?|  =(~zod ship.src.bowl)
      (team:title [our ship.src]:bowl)
  ==
::
++  on-connect      on-connect:des
++  on-disconnect   on-disconnect:des
--
