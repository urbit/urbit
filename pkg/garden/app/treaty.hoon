/-  docket, *treaty
/+  default-agent, agentio, verb, dbug
|%
+$  card  card:agent:gall
+$  state-0
  $:  treaties=(map [=ship =desk] treaty)
      sovereign=(map desk treaty)
      entente=alliance
      =allies:ally
  ==
--
^-  agent:gall
%+  verb  &
%-  agent:dbug
=|  state-0
=*  state  -
=<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    io    ~(. agentio bowl)
    pass  pass:io
    cc    ~(. +> bowl)
++  on-init  
  =/  sponsor=ship  (sein:title [our now our]:bowl)
  ?:  =(our.bowl sponsor)  `this
  (on-poke %ally-update-0 !>([%add sponsor]))
++  on-save  !>(state)
++  on-load
  |=  =vase
  =+  !<(old=state-0 vase)
  `this(state old)
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  (team:title [our src]:bowl)
  |^  
  ?+  mark  (on-poke:def mark vase)
    %ally-update-0      (ally-diff !<(diff:ally vase))
    %alliance-update-0  (alliance-diff !<(diff:alliance vase)) 
  ::
      %noun 
    =+  ;;([%add =desk] q.vase)
    =/  =docket:docket  ~(get-docket so:cc desk)
    =/  =treaty  (treaty-from-docket:cc desk docket)
    =.  sovereign  (~(put by sovereign) desk treaty)
    `this
  ==
  ::
  ++  ally-diff
    |=  =diff:ally
    ^-  (quip card _this)
    =/  upd=card  (ally-update:ca:cc diff)
    =*  ship  ship.diff
    ?<  =(ship our.bowl)
    =*  al   ~(. al:cc ship.diff)
    ?-  -.diff
      %add  [~[watch:al upd] this(allies (~(put by allies) ship *alliance))]
      %del  [~[leave:al upd] this(allies (~(del by allies) ship))]
    ==
  ::
  ++  alliance-diff
    |=  =diff:alliance
    ^-  (quip card _this)
    =-  [[(alliance-update:ca:cc diff) -.-] +.-]
    ^-  (quip card _this)
    =,  diff
    ?-  -.diff
    ::
        %add  
      =.  entente  (~(put in entente) [ship desk])
      ?.  =(our.bowl ship)  `this
      =*  so  ~(. so:cc desk)
      =/  =docket:docket  get-docket:so
      =/  =treaty  (treaty-from-docket:cc desk docket)
      =.  sovereign  (~(put by sovereign) desk treaty)
      :_  this
      ~[publish warp give]:so
    ::
        %del
      =.  entente  (~(del in entente) [ship desk])
     ?.  =(our.bowl ship)  `this
     =.  sovereign  (~(del by sovereign) desk)
     :_(this ~(kick so:cc desk)^~)
   ==
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+  path  (on-watch:def path)
    ::  syncing 
      [%treaty @ @ ~]  
    =/  =ship  (slav %p i.t.path)
    =*  desk   i.t.t.path
    =/  =treaty  
      ?:  =(our.bowl ship)  (~(got by sovereign) desk)
      (~(got by treaties) [ship desk])
    :_  this
    (fact-init:io treaty+!>(treaty))^~
    ::
      [%alliance ~]
    :_  this
    (fact-init:io (alliance-update:cg:cc %ini entente))^~

    ::  local
    ::
      [%allies ~]
    :_  this
    (fact-init:io (ally-update:cg:cc %ini allies))^~
  ==
::
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
    [%x %alliance ~]  ``(alliance-update:cg:ca %ini entente)
    [%x %allies ~]    ``(ally-update:cg:ca %ini allies)
  ::
     [%x %treaties @ ~]
    =/  =ship  (slav %p i.t.t.path)
    =/  allied  
      %-  ~(gas by *(map [^ship desk] treaty))
      %+  murn  ~(tap by treaties)
      |=  [[s=^ship =desk] =treaty]
      ?.  =(s ship)  ~ 
      `[[s desk] treaty]
    ``(treaty-update:cg:ca %ini allied)
  ::
      [%x %treaty @ @ ~]
    =/  =ship  (slav %p i.t.t.path)
    =*  desk   i.t.t.t.path
    =/  =treaty  (~(got by treaties) [ship desk])
    ``treaty+!>(treaty)
  ==
::
++  on-agent 
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  =*  ship  src.bowl
  |^  
  ?+  wire  (on-agent:def wire sign)
    [%ally @ ~]  ?>(=(src.bowl (slav %p i.t.wire)) take-ally)
  ::
      [%treaty @ @ ~]  
    =*  desk  i.t.t.wire
    ?>  =(ship (slav %p i.t.wire))
    (take-treaty desk)
  ==
  ::
  ++  take-ally
    ?+  -.sign  (on-agent:def wire sign)
    ::
        %kick
      ?.  (~(has by allies) ship)  `this
      :_(this ~[~(watch al:cc ship)])
    ::
        %watch-ack
      ?~  p.sign  (on-agent:def wire sign)
      =.  allies  (~(del by allies) ship)
      %-  (slog leaf+"Broke alliance with {<ship>}" u.p.sign)
      `this
    ::
        %fact
      ?.  =(%alliance-update-0 p.cage.sign)  `this
      =+  !<(=update:alliance q.cage.sign)
      =^  cards  allies  
        ?-  -.update 
        ::
            %ini
          :_   (~(put by allies) src.bowl init.update)
          %+  murn  ~(tap in init.update)
          |=  [s=^ship =desk]
          ~(safe-watch tr:cc ship desk)
        ::
            %add  
          :_  (~(put ju allies) src.bowl [ship desk]:update)
          (drop ~(safe-watch tr:cc [ship desk]:update))

          %del
          :_  (~(del ju allies) src.bowl [ship desk]:update)
          ~[~(leave tr:cc [ship desk]:update)]
        ==
      :_  this
      :_  cards
      (fact:io (ally-update:cg:cc %new ship (~(get ju allies) src.bowl)) /allies ~)
    ==
  ::
  ++  take-treaty
    |=  =desk
    ?+  -.sign  (on-agent:def wire sign)
      %kick   :_(this ~[~(watch tr:cc ship desk)])
    ::
        %watch-ack
      ?~  p.sign  `this
      =.  treaties  (~(del by treaties) ship desk)
      %-  (slog leaf+"Withdrew from treaty {<ship>}/{<desk>}" u.p.sign)
      `this
    ::
        %fact
      ?.  =(%treaty p.cage.sign)  `this
      =+  !<(=treaty q.cage.sign)
      ?>  =([ship desk] [ship desk]:treaty)
      =.  treaties  (~(put by treaties) [ship desk]:treaty treaty)
      [~(give tr ship desk)^~ this]
    ==
  --
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  |^
  ?+  wire  (on-arvo:def wire sign)
    [%init ~]  !! :: setup sponsor ally
  ::
      [%sovereign @ ~]
    =*  desk  i.t.wire
    (take-sovereign desk)

  ==
  ::
  ++  take-sovereign
    |=  =desk
    =/  so  ~(. so:cc desk)
    ?>  ?=([?(%clay %behn) %writ *] sign)
    ?.  (~(has by sovereign) desk)  `this
    =*  riot  p.sign
    ?~  riot  :: docket file is gone
      =.  sovereign  (~(del by sovereign) desk)
      [~[kick:so] this]
    =*  cage  r.u.riot
    ?.  =(%docket p.cage)  `this
    =+  !<(=docket:docket q.cage)
    =/  =treaty  (treaty-from-docket:cc desk docket)
    =.  sovereign  (~(put by sovereign) desk treaty)
    =*  so  ~(. so:cc desk)
    :_(this [give warp ~]:so)
  --

::
++  on-fail  on-fail:def
++  on-leave  on-leave:def
--
|_  =bowl:gall
++  io  ~(. agentio bowl)
++  pass  pass:io
::
++  treaty-from-docket
  |=  [=desk =docket:docket]
  =+  .^(=cass:clay %cw (scry:io desk /desk/docket))
  =+  .^(hash=@uv %cz (scry:io desk ~))
  [our.bowl desk da+da.cass hash docket]
::  +al: Side effects for allies
++  al
  |_  =ship
  ++  pass    ~(. ^pass /ally/(scot %p ship))
  ++  watch   (watch:pass [ship dap.bowl] /alliance) 
  ++  leave   (leave:pass ship dap.bowl)
  --
::  +cg: Cage construction
++  cg
  |%
  ++  ally-update      |=(=update:ally ally-update-0+!>(update))
  ++  alliance-update  |=(=update:alliance alliance-update-0+!>(update))
  ++  treaty  |=(t=^treaty treaty+!>(t))
  ++  treaty-update  |=(u=update:^treaty treaty-update-0+!>(u))
  --
::  +ca: Card construction
++  ca
  |%
  ++  watch-docket  (~(watch-our pass /docket) %docket /dockets)
  ++  ally-update  |=(=update:ally (fact:io (ally-update:cg update) /allies ~))
  ++  alliance-update  
    |=(=update:alliance (fact:io (alliance-update:cg update) /alliance ~))
  --
::  +tr: engine for treaties
++  tr
  |_  [=ship =desk]
  ++  pass  ~(. ^pass path)
  ++  path  /treaty/(scot %p ship)/[desk]
  ++  dock  [ship dap.bowl]
  ++  watch  (watch:pass dock path)
  ++  watching  (~(has by wex.bowl) [path dock])
  ++  safe-watch  `(unit card)`?:(watching ~ `watch)
  ++  leave  (leave:pass dock)
  ++  give  
    =/  t=treaty  (~(got by treaties) ship desk)
    (fact:io (treaty:cg t) /treaties path ~)
  --
::  +so: engine for sovereign treaties
++  so
  |_  =desk
  ++  wire  /sovereign/[desk]
  ++  pass  ~(. ^pass wire)
  ++  path  /treaty/(scot %p our.bowl)/[desk]
  ++  get-docket  .^(docket:docket %cx (scry:io desk /desk/docket))
  ++  warp
    (warp-our:pass desk `[%next %x da+now.bowl /desk/docket])
  ++  kick
    (kick:io path ~)
  ++  give
    =/  t=treaty  (~(got by sovereign) desk)
    (fact:io (treaty:cg t) /sovereign path ~)
  ++  publish
    (poke-our:pass %hood kiln-permission+!>([desk / &]))
  --
--




  





