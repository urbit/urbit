/-  docket, *treaty
/+  default-agent, agentio, verb, dbug
|%
::  TODO: update before livenet deploy
++  default-ally  ~zod
::
+$  card  card:agent:gall
+$  state-1
  $:  %1
      treaties=(map [=ship =desk] treaty)
      sovereign=(map desk treaty)
      entente=alliance
      =allies:ally
      direct=(set [=ship =desk])
  ==
--
^-  agent:gall
%+  verb  &
%-  agent:dbug
=|  state-1
=*  state  -
=<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    io    ~(. agentio bowl)
    pass  pass:io
    cc    ~(. +> bowl)
++  on-init
  ?:  =(our.bowl default-ally)  `this
  (on-poke %ally-update-0 !>([%add default-ally]))
++  on-save  !>(state)
++  on-load
  |=  =vase
  |^  =/  old=state-any
        ?:  |(?=([~ *] q.vase) ?=([^ *] q.vase))
          (state-0-to-1 !<(state-0 vase))
        !<(state-any vase)
      `this(state old)
  ::
  +$  state-any  state-1
  +$  state-0
    $:  treaties=(map [=ship =desk] treaty-0)
        sovereign=(map desk treaty-0)
        entente=alliance
        =allies:ally
        direct=(set [=ship =desk])
    ==
  +$  treaty-0
    [=ship =desk =case hash=@uv docket=docket-0]
  +$  docket-0
    $:  %1
        title=@t
        info=@t
        color=@ux
        href=href-0
        image=(unit cord)  :: url:docket
        =version:docket
        website=cord  ::  url:docket
        license=cord
    ==
  +$  href-0
    $%  [%glob base=term =glob-location:docket]
        [%site =path]
    ==
  ::
  ++  state-0-to-1
    |=  old=state-0
    ^-  state-1
    :-  %1
    %=  old
      treaties   (~(run by treaties.old) treaty-0-to-1)
      sovereign  (~(run by sovereign.old) treaty-0-to-1)
    ==
  ++  treaty-0-to-1
    |=  t=treaty-0
    ^-  treaty
    t(href.docket (href-0-to-1 href.docket.t))
  ++  href-0-to-1
    |=  h=href-0
    ^-  href:docket
    ?.  ?=(%glob -.h)  h
    [%glob base 0v0 glob-location]:h
  --
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  (team:title [our src]:bowl)
  |^
  ?+  mark  (on-poke:def mark vase)
    %ally-update-0      (ally-update !<(update:ally vase))
    %alliance-update-0  (alliance-update !<(update:alliance vase))
  ::
      %noun
    =+  ;;([%add =desk] q.vase)
    =/  =docket:docket  ~(get-docket so:cc desk)
    =/  =treaty  (treaty-from-docket:cc desk docket)
    =.  sovereign  (~(put by sovereign) desk treaty)
    `this
  ==
  ::
  ++  ally-update
    |=  =update:ally
    ^-  (quip card _this)
    =-  [[(ally-update:ca:cc update) -.-] +.-]
    ?<  ?=(?(%ini %new) -.update)
    =*  ship  ship.update
    ?<  =(ship our.bowl)
    =*  al   ~(. al:cc ship.update)
    ?-  -.update
      %add  [~[watch:al] this(allies (~(put by allies) ship *alliance))]
      %del  [~[leave:al] this(allies (~(del by allies) ship))]
    ==
  ::
  ++  alliance-update
    |=  =update:alliance
    ^-  (quip card _this)
    =-  [[(alliance-update:ca:cc update) -.-] +.-]
    ?+  -.update  !!
    ::
        %add
      =,  update
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
      =,  update
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
    ?:  =(our.bowl ship)
      :_(this (fact-init:io treaty+!>((~(got by sovereign) desk)))^~)
    ?^  treat=(~(get by treaties) [ship desk])
      :_  this
      (fact-init:io treaty+!>(u.treat))^~
    ?>  =(our.bowl src.bowl)
    =.  direct  (~(put in direct) [ship desk])
    :_(this (drop ~(safe-watch tr:cc [ship desk])))
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
    [%x %alliance ~]      ``(alliance-update:cg:ca %ini entente)
    [%x %default-ally ~]  ``ship+!>(default-ally)
    [%x %allies ~]        ``(ally-update:cg:ca %ini allies)
  ::
     [%x %treaties @ ~]
    =/  =ship  (slav %p i.t.t.path)
    =/  alliance  (~(get ju allies) ship)
    =/  allied
      %-  ~(gas by *(map [^ship desk] treaty))
      %+  skim  ~(tap by treaties)
      |=  [ref=[^ship desk] =treaty]
      (~(has in alliance) ref)
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
          ~(safe-watch tr:cc s desk)
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
      =:  treaties  (~(del by treaties) ship desk)
          direct    (~(del in direct) ship desk)
        ==
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
