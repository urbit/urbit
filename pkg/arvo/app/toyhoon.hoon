::  shoe: example usage of /lib/shoe
::
::    the app supports one command: "demo".
::    running this command renders some text on all sole clients.
::
/+  shoe, verb, dbug, default-agent,
    th=toyhoon, tp=toyhoon-parse
|%
+$  state-0  [%0 subject=vase:th]
+$  command
  $%  [%eval =cord]
      [%read =cord]
      [%toke =cord]
      [%tags =cord]
      [%save face=term =cord]
      [%nuke ~]
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
  [~ this(state !<(state-0 old))]
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
  |=  =sole-id:shoe
  ^+  |~(nail *(like [? command]))
  =/  hun  (cook crip (star next))
  %+  stag  %.n
  ;~  pose
    ;~(pfix tis (stag %save ;~(plug sym ;~(pfix ace hun))))
    ;~(pfix wut wut ace (stag %toke hun))
    ;~(pfix wut wut wut ace (stag %tags hun))
    ;~(pfix wut ace (stag %read hun))
    (cold [%nuke ~] (jest '|wipe'))
    (stag %eval hun)
  ==
  :: [[1 +((lent q))] ~ [| %eval (crip q)] [1 +((lent q))] q]
  :: ~&  parse=+<
  :: =+  res=~(tall parser:tp & (init-cord-cursor:tp (crip q)) p ~)
  :: ~&  res=res
  :: ?~  res  [p ~]
  :: =-  ~&  prod=-  -
  :: [[1 1] ~ [| %eval u.res] [1 i.cur.s.res] ""]  ::TODO  cursor
::
++  tab-list
  |=  =sole-id:shoe
  ^-  (list [@t tank])
  ~
::
++  on-command
  |=  [=sole-id:shoe =command]
  ^-  (quip card _this)
  =;  [fec=shoe-effect:shoe s=_state]
    [[%shoe [sole-id]~ fec]~ this(state s)]
  ?-  -.command
      %nuke  [[%sole %txt ">>"] state(subject [%noun ~])]
  ::
      ?(%eval %save %read %toke %tags)
    =/  hon=cord
      ?-(-.command ?(%eval %read %toke %tags) cord.command, %save cord.command)
    ?:  ?=(%tags -.command)
      =|  taz=(list tag:tp)
      =;  =_taz
        :_  state
        [%sole %mor [%txt '> ??? ' (trip hon)] [%tan >taz< ~] ~]
      =+  lex=gate:tp
      =+  cur=(init-cord-cursor:tp hon)
      |-
      =+  res=(lex cur)
      ?~  res  (flop taz)
      $(taz [tag.res taz], cur cur.res)
    ?:  ?=(%toke -.command)
      =|  toz=(list toke:tp)
      =;  =_toz
        :_  state
        [%sole %mor [%txt '> ?? ' (trip hon)] [%tan >toz< ~] ~]
      =+  parser=parser:tp
      =/  pst  ^+  +<:parser
        [& (init-cord-cursor:tp hon) [1 1] ~]
      |-  ^+  toz
      =+  res=~(gulp parser pst)
      ?~  res  (flop toz)
      $(toz [u.res toz], +.pst s.res)
    =+  parser=parser:tp
    =+  res=~(tall parser & (init-cord-cursor:tp hon) [1 1] ~)
    ?~  res
      :_  state
      [%sole %mor [%txt '? ' (trip hon)] [%bel ~] ~]
    ?.  finished:parser(st s.res)
      :_  state
      =+  pon=(cat 3 (fil 3 i.cur.s.res '-') '--^')
      [%sole %mor [%txt '? ' (trip hon)] [%txt (trip pon)] [%bel ~] ~]
      ::TODO  mb still print parse result?
    ?:  ?=(%read -.command)
      :_  state
      [%sole %mor [%txt '> ? ' (trip hon)] [%tan >u.res< ~] ~]
    =/  out=(each vase:th tang)
      (mule |.((slap:th subject %noun (open:th u.res))))
    ?:  ?=(%| -.out)
      :_  state
      [%sole %mor [%txt '! ' (trip hon)] [%tan p.out] ~]
    ?-  -.command
        %eval
      :_  state
      [%sole %mor [%txt '> ' (trip hon)] [%tan >p.out< ~] ~]
    ::
        %save
      :-  [%sole %txt '> =' face.command ' ' (trip hon)]
      state(subject (slop:th p.out(p [%face face.command p.p.out]) subject))
    ==
  ==
::
++  can-connect
  |=  =sole-id:shoe
  ^-  ?
  &
::
++  on-connect      on-connect:des
++  on-disconnect   on-disconnect:des
--