/+  default-agent, verb, dbug, shoe
::
|%
+$  scrip  [=ship dap=term =path]
+$  state
  $:  %0
      subs=(jar scrip [wen=@da =cage])
  ==
+$  command
  $%  [%show-subs ~]
      [%show-facts ~]
      [%sub scrip]
  ==
++  sub-wide   ~[15 10 20]
++  fact-wide  (welp sub-wide 10 100 30 ~)
--
|%
++  scrap
  |=  scrip
  ^-  ^path
  [(scot %p ship) dap path]
::
++  scarp
  |=  =path
  ^-  scrip
  ?>  ?=([@ @ *] path)
  [(slav %p i.path) i.t.path t.t.path]
::
++  summarize
  |=  =cage
  ^-  @t
  (end [3 100] (crip ~(ram re (sell q.cage))))
--
::
%+  verb  |
=|  =state
%-  agent:dbug
^-  agent:gall
%-  (agent:shoe command)
^-  (shoe:shoe command)
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
    des  ~(. (default:shoe this command) bowl)
++  on-init   on-init:def
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card:shoe _this)
  ?.  ?=([%sub *] wire)  (on-agent:def wire sign)
  ?+    -.sign  (on-agent:def wire sign)
      %fact
    ?.  ?=([@ @ @ *] t.wire)  (on-agent:def wire sign)
    =*  sole-id  i.t.wire
    =/  =scrip  (scarp t.t.wire)
    =.  subs.state  (~(add ja subs.state) scrip [now.bowl cage.sign])
    :_  this  :_  ~
    :*  %shoe  sole-id^~  %row  fact-wide
        p+ship.scrip
        tas+dap.scrip
        t+(spat path.scrip)
        tas+p.cage.sign
        t+(summarize cage.sign)
        da+now.bowl
        ~
    ==
  ==
::
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
::
++  command-parser
  |=  sole-id=@ta
  %+  stag  %|
  ;~  pose
    ;~(plug (cold %show-subs (jest %show-subs)) (easy ~))
    ;~(plug (cold %show-facts (jest %show-facts)) (easy ~))
    ;~  (glue ace)
      (cold %sub (jest %sub))
      ;~(pfix sig fed:ag)
      ;~(pfix cen sym)
      stap
    ==
  ==
::
++  tab-list
  |=  sole-id=@ta
  ^-  (list [@t tank])
  :~  ['show-subs' 'show-subs']
      ['show-facts' 'show-facts']
      ['sub' 'sub ~ship %agent /path']
  ==
::
++  on-command
  |=  [sole-id=@ta com=command]
  ^-  (quip card:shoe _this)
  ?-    -.com
      %show-subs
    :_  this  :_  ~
    ^-  card:shoe
    :*  %shoe  sole-id^~  %table
        ~[t+'ship' t+'agent' t+'path']
        sub-wide
        %+  turn  ~(tap by subs.state)
        |=  [=scrip *]
        ~[p+ship.scrip tas+dap.scrip t+(spat path.scrip)]
    ==
  ::
      %show-facts
    :_  this  :_  ~
    ^-  card:shoe
    :*  %shoe  sole-id^~  %table
        ~[t+'ship' t+'agent' t+'path' t+'mark' t+'fact' t+'date']
        fact-wide
        %-  zing
        %+  turn  ~(tap by subs.state)
        |=  [=scrip facts=(list [wen=@da =cage])]
        %+  turn  facts
        |=  [wen=@da =cage]
        :~  p+ship.scrip
            tas+dap.scrip
            t+(spat path.scrip)
            tas+p.cage
            t+(summarize cage)
            da+wen
        ==
    ==
  ::
      %sub
    =.  subs.state  (~(put by subs.state) +.com ~)
    :_  this  :_  ~
    ^-  card:shoe
    [%pass [%sub sole-id (scrap +.com)] %agent [ship dap]:com %watch path.com]
  ==
::
++  on-connect      on-connect:des
++  can-connect     can-connect:des
++  on-disconnect   on-disconnect:des
--
