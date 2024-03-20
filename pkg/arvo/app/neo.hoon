/-  neo, sole-sur=sole
/+  default-agent, dbug, verb, shoe, ford=ford-parser
|%
++  pave  pave:neo

++  sole
  |%
  +$  id  sole-id:sole-sur
  +$  action  sole-action:sole-sur
  --
++  show-iota
  |=  i=iota
  ^-  @t
  ?@  i  i  (scot i)
+$  pith   pith:neo
+$  card  card:shoe
+$  race-form  _*eval-form:(pike:neo ,ewer:neo)
+$  race
  $:  rout=term
      form=race-form
      grab=(list item:pike:neo)
      have=(list clot:goon:neo)
  ==
+$  shell
  $:  cwd=name:neo
      race=(unit race)
  ==
+$  hull
  $%  [%clot =clot:goon:neo]
      [%ls ~]
      [%tree depth=@ud]
      [%show ~]
      [%cd =name:neo]
      [%poke rout=@tas]
      [%comm ~]
      [%clay ~]
  ==
+$  state-0
  $:  %0
      apex=(axal:neo room:neo)
      :: diary=(axal:neo memo:neo)
      :: dead=(map @uvH (axal:neo room:neo))
      hear=(jug name:neo sound:neo)
      =fleet:neo
      husks=(jug stud:neo name:neo)
      shells=(map id:sole shell)
      races=(map id:sole race)
      hear=(map name:neo sound:neo)
  ==
++  is-parent-p
  |=  [parent=path kid=path]
  ^-  ?
  ?~  parent  &
  ?~  kid     |
  ?.  =(i.parent i.kid)
    |
  $(parent t.parent, kid t.kid)

++  is-parent
  |=  [parent=pith kid=pith]
  ^-  ?
  ?~  parent  &
  ?~  kid     |
  ?.  =(i.parent i.kid)
    |
  $(parent t.parent, kid t.kid)
++  get-ship
  |=  =pith
  ^-  @p
  ?>  ?=([[%p @] *] pith)
  +.i.pith
--
=|  state-0
=*  state  -
=<
  %-  agent:dbug
  %+  verb  |
  %-  (agent:shoe hull)  
  |_  =bowl:gall
  +*  this  .
      run   ~(. +> [bowl ~])
      def   ~(. (default-agent this %|) bowl)
  ++  on-init  
    ^-  (quip card _this)
    =^  cards  state
      abet:init:run
    [cards this]
  ++  on-save  !>(state)
  ++  on-load
    |=  =vase
    =+  !<(old=state-0 vase)
    `this(state old)
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      abet:(poke:run mark vase)
    [cards this]
  ++  on-watch  
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      abet:(watch:run path)
    [cards this]
  ::
  ++  on-leave  on-leave:def
  ++  on-agent  
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      abet:(take-agent:run wire sign)
    [cards this]
  ++  on-arvo   
    |=  [=(pole knot) syn=sign-arvo]
    ^-  (quip card _this)
    ?.  ?=([%deal pit=*] pole)
      `this
    =^  cards  state
      abet:(take-arvo:run (pave pit.pole) syn)
    [cards this]
  ++  on-fail   on-fail:def
  ++  on-peek   on-peek:def
  ++  command-parser 
    |=  =id:sole
    ~(parser walk:run id)
  ++  tab-list
    |=  [=id:sole query=@t]
    (~(tab walk:run id) query)
  ++  on-command
    |=  [=id:sole =hull] 
    =^  cards  state
      abet:(~(do walk:run id) hull)
    [cards this]
  ::
  ++  can-connect
    |=  =id:sole
    =(our src):bowl
  ++  on-connect
    |=  =id:sole
    =^  cards  state
      abet:(conn:run id)
    [cards this]
  ++  on-disconnect
    |=  =id:sole
    =^  cards  state
      abet:~(drop walk:run id)
    [cards this]
  --
|_  [=bowl:gall cards=(list card)]
++  abet  [(flop cards) state]
++  run  .
++  emit  |=(card run(cards [+< cards]))
++  emil  |=(caz=(list card) run(cards (welp (flop caz) cards)))
++  of-top  ~(. of:neo apex)
++  clay-beak  ^-  path
  /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)
++  clay-lib
  |=  lib=term
  ^-  code:neo
  clay/:(welp clay-beak /lib/[lib]/hoon)
++  init
  ^+  run
  =/  =name:neo  [our.bowl #/src/reef]
  =+  .^(neo-vase=vase %ca (welp clay-beak /sur/neo/hoon))
  =/  reef=vase  (slop !>(..zuse) neo-vase(p [%face %neo p.neo-vase]))
  =/  =card:neo  [(en-pith:name:neo name) %make (clay-lib %ford-reef) `!>(reef) ~]
  (on-card card)
++  sync-room
  |=  [=stud:neo =name:neo]
  ^+  run
  =/  =wire  sync/(pout pith.name) 
  =.  run  abet:(~(init xeno name) stud/stud)
  (emit %pass wire %agent [ship.name dap.bowl] %watch [%sync %init (pout pith.name)])
++  our-sys-pith  `pith:neo`[p/our.bowl ~]
::
++  take-arvo
  |=  [=pith syn=sign-arvo]
  ^+  run
  ?:  ?=(%remote -.pith)
    !! :: abet:(~(take xeno pith) syn)
  ?:  ?=([%husk @ *] pith)
    !! :: (~(take husk i.t.pith) (pout t.t.pith) syn)
  abet:(take-arvo:(abed:arvo our-sys-pith pith) pith syn)
::
++  forward-poke
  |=  [=name:neo pok=*]
  ^+  run
  =/  =wire  forward/(en-path:name:neo name)
  =/  =dock  [ship.name dap.bowl]
  =-  (emit %pass wire %agent dock %poke -)
  noun+!>(`card:neo`[(en-pith:name:neo name) %poke pok])
++  print-dbug
  |=  veb=?
  |^  ^+  same
  %-  %*(. slog pri 1)
  %-  lure
  :+  %rose  [ret "Shrubbery" sep]
  :*  leaf/"Local"
      (local-axal *pith apex)
      leaf/"Remote"
      %+   turn  ~(tap by fleet)
      |=  [=ship =brig:neo]
      (remote-axal [p/ship]~ brig)
  ==
  ++  ret  [',' (reap 4 ' ')]
  ++  sep  *tape
  ++  remote-kids
    |=  [=pith =(axal:neo cell:neo)]
    ^-  tank
    ?:  =(~ kid.axal)
      leaf/"No children"
    :+  %rose  [ret "Kids:" sep]
    %+  murn  ~(tap by kid.axal)
    |=  [=iota a=(axal:neo cell:neo)]
    ^-  (unit tank)
    ?:  &(veb =(pith ~) |(=(iota %src) =(iota %pre)))
      ~
    `(remote-axal (snoc pith iota) a)
  ++  remote-axal
    |=  [=pith =(axal:neo cell:neo)]
    ^-  tank
    :+  %rose  [ret (en-tape:pith:neo pith) sep]
    ^-  (list tank)
    %-  snoc
    :_  (remote-kids pith axal)
    ^-  (list tank)
    ?~  fil.axal
      ~[leaf/"No data"]
    =/  =cell:neo  u.fil.axal
    ?:  =(case.cell 0)
      ~[leaf/"No data at this path"]
    :~  leaf/"State"
        ?:  (lth 10.000 (met 3 (jam q.state.cell)))
          leaf/"Too large to print"
        (sell state.cell)
          
        leaf/"Case: {(scow %ud case.cell)}"
      ::
      ::
        leaf/"Source: {<p.span.cell>}"
    ==

  ++  local-kids
    |=  [=pith =(axal:neo room:neo)]
    ^-  tank
    ?:  =(~ kid.axal)
      leaf/"No children"
    :+  %rose  [ret "Kids:" sep]
    %+  murn  ~(tap by kid.axal)
    |=  [=iota a=(axal:neo room:neo)]
    ^-  (unit tank)
    ?:  &(veb =(pith ~) |(=(iota %src) =(iota %pre)))
      ~
    `(local-axal (snoc pith iota) a)
  ++  local-axal
    |=  [=pith =(axal:neo room:neo)]
    ^-  tank
    :+  %rose  [ret (en-tape:pith:neo pith) sep]
    ^-  (list tank)
    %-  snoc
    :_  (local-kids pith axal)
    ^-  (list tank)
    ?~  fil.axal
      ~[leaf/"No data"]
    =/  =room:neo  u.fil.axal
    ?:  =(case.icon.room 0)
      ~[leaf/"No data at this path"]
    :*  leaf/"State"
        ?:  (lth 10.000 (met 3 (jam q.state.icon.room)))
          leaf/"Too large to print"
        (sell state.icon.room)
          
        leaf/"Case: {(scow %ud case.icon.room)}"
      ::
      ::
        leaf/"Source: {<p.span.room>}"
        ^-  (list tank)
        ?:  =(~ conf.room)
          ~
        :_  ~
        :+  %rose  [" " "Dependencies" sep]
        %+  turn  ~(tap by conf.room)
        |=  [=term p=^pith]
        leaf/"{<term>} -> {(en-tape:pith:neo p)}"
    ==
  --
::
++  poke
  |=  [=mark =vase]
  ^+  run
  ?:  =(%neo-move mark)
    =+  !<(=move:neo vase)
    (on-move move)
  ?>  ?=(%noun mark)
  ?:  =(%clay q.vase)
    copy-clay
  ?:  ?=([%dbug veb=?] q.vase)
    ?>  =(our src):bowl
    %-  (print-dbug veb.q.vase)
    run
  ?:  ?=(%out -.q.vase)
    =+  ;;(=out:neo +.q.vase)
    (do-out out)
  =+  ;;(=card:neo q.vase)
  =/  =name:neo  (de-pith:name:neo p.card) 
  ?.  =(our.bowl ship.name)
    ?>  ?=(%poke -.q.card)
    (forward-poke name val.q.card)
  (on-card card)
++  on-card
  |=  =card:neo
  (on-move our-sys-pith card)
::
++  on-move
  |=  =move:neo
  =/  =name:neo  (de-pith:name:neo p.q.move)
  ~&  move/name
  ?>  =(our.bowl ship.name)
  abet:(apply:(abed:arvo p.move pith.name) move)
::
++  watch
  |=  =(pole knot)
  ^+  run
  ?+    pole  ~|(bad-path/pole !!)
      [%sync rest=*]
    ?+    rest.pole  !!
        [%init path=*]
      =/  =pith:neo  (pave path.rest.pole)
      =-  (emit %give %fact ~ neo-watch+!>(-))
      =/  ros=(map pith:neo room:neo)
        ~(tar of:neo (dip:of-top pith))
      %+  turn  ~(tap by ros)
      |=  [p=pith:neo =room:neo]
      ^-  update:neo
      ?>  ?=(%stud -.p.span.room) 
      [(welp pith p) p.p.span.room [case %init q.state]:icon.room]
    ==
  ==
++  do-out
  |=  =out:neo
  ^+  run
  =;  new=_run
    =.  run  new
    ~&  (turn ~(val by fleet) |=(b=brig:neo ~(key by ~(tar of:neo b))))
    run
  ?-  -.out
    %sync  (sync-room [stud name]:out)
    %stop  run
  ==
++  take-agent
  |=  [=(pole knot) =sign:agent:gall]
  |^  ^+  run
  ?+  pole  ~|(on-agent-bad-wire/pole !!)
    [%test ~]       test-wire 
    [%sync rest=*]  (sync (pave rest.pole))
    [%forward rest=*]  (forward (pave rest.pole))
    [%local rest=*]    (take-local-agent (pave rest.pole) sign)
  ==
  ++  test-wire
    ?.  ?=(%poke-ack -.sign)
      !!
    %.  run
    ?~  p.sign
      same
    (slog leaf/"nack on test wire" u.p.sign)

  ++  forward
    |=  =pith
    ?.  ?=(%poke-ack -.sign)
      ~|(weird-forward-sign/-.sign !!)
    %.  run
    ?~  p.sign
      same
    (slog leaf/"failed forward poke {(spud (pout pith))}" u.p.sign)
      

  ++  sync
    |=  =pith
    ?+    -.sign  ~|(weird-sync-sign/-.sign !!)
        %watch-ack
      %.  run
      ?~  p.sign  same
      %+  slog
        leaf/"Failed sync from {(spud (pout pith))}" 
      u.p.sign
    ::  TODO: security vuln, confused deputy
        %fact
      ?>  ?=(%neo-watch p.cage.sign)
      (hear-watch !<(watch:neo q.cage.sign))
    ::
        %kick
      ~&  'TODO: resub logic'
      run
    ==
  --
++  pro
  |_  =stud:neo
  ++  grab
    =/  rom  (got:of-top pith)
    =+  !<([cac=(unit vase) *] state.icon.rom)
    cac
  ++  built
    (has:of-top pith)
  ++  pith
    `pith:neo`(pave path)
  ++  path
    ^-  ^path
    =-  ~&(pro/[stud -] -)
    :-  %src
    ?@  stud
      /std/pro/[stud]
    ?:  =(our.bowl ship.stud)
      /our/[desk.stud]/pro/[mark.stud]
    :+  %ext  (scot %p ship.stud)
    /[desk.stud]/pro/[mark.stud]
  ++  exists
    (exists-file path)
  --
++  root
  /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/neo
++  exists-file
  |=  pax=path
  =/  p=path
    (welp root pax)
  =.  p  (snoc p %hoon)
  =-  ~&(exists/[p -] -)
  .^(? %cu p)

++  copy-clay
  |^  ^+  run
  =+  .^(paths=(list path) %ct root)
  |-  ^+  run
  ?~  paths
    run
  =^  pat=(unit path)  run
    (read-file i.paths)
  ?~  pat
    $(paths t.paths)
  $(paths (snoc t.paths u.pat))
  ++  lib
    |_  =name:neo
    ++  path
      ^-  ^path
      ?>  =(our.bowl ship.name)
      [%src (pout pith.name)]
    ++  built
      (has:of-top %src pith.name)
    ++  pith
      [p/ship.name pith.name]
    ++  exists
      (exists-file path)
    --
  ++  do-make
    |=  [=pith:neo lib=term sta=(unit vase) =conf:neo]
    =/  =name:neo  (de-pith:name:neo pith)
    =.  run  (on-card pith %make (clay-lib lib) sta conf)
    ?:  =(lib %sym)
      run
    =/  rom  (got:of-top pith.name)
    =+  !<([cache=(unit vase) *] state.icon.rom)
    ?.  !=(~ cache)
      ~|  conf/conf
      !!
    run
      

  ++  slop
    |=  [wer=pith a=pith b=pith]
    ~|  %ford-slop
    %^  do-make  wer  %ford-slop
    `(~(gas by *conf:neo) a/a b/b ~)
  ++  face
    |=  [wer=pith face=pith sut=pith]
    ~|  %ford-face
    %^  do-make  wer  %ford-face
    `(~(gas by *conf:neo) face/(ours face) sut/(ours sut) ~)
  ++  same
    |=  [wer=pith from=pith]
    ~|  %ford-same
    %^  do-make  wer  %ford-same
    `(~(gas by *conf:neo) src/from ~)
  ++  ours
    |=  p=pith:neo  `pith:neo`[p/our.bowl p]
  ++  make-pros
    =|  idx=@ud
    |=  [pat=pith pros=(list pro:ford)]
    ^+  run
    ?~  pros
      %+  same  (ours pat)
      ?:  =(0 idx)
        (ours #/src/reef)
      (ours (snoc pat ud/(dec idx)))
    =/  wer=pith  (snoc pat ud/idx)
    =/  fac=pith  (snoc wer %face)
    =/  fav=pith  (snoc fac %term)
    =.  run
      (do-make (ours fav) %sym `!>(face.i.pros) ~)
    =.  run
      (face (ours fac) fav ~(pith pro stud.i.pros))
    =/  prev=pith
      %-  ours
      ?:  =(idx 0)
        #/src/reef
      (snoc pat ud/(dec idx))
    =.  run
      (slop (ours wer) prev (ours fac))
    $(pros t.pros, idx +(idx))
  ++  make-prelude
    |=  [pax=pith =file:ford]
    ^-  [pith _run]
    =/  pre-path=pith  [%pre pax]
    [pre-path (make-pros pre-path pro.file)]
  ++  read-file
    |=  pax=path
    ^-  [(unit path) _run]
    =.  pax
      ?>  ?=(^ pax)
      t.pax
    =+  .^(src=@t %cx (welp root pax))
    =/  =file:ford
      ~|  parsing/pax
      (scan (trip src) apex:rein:ford)
    ~&  imports/[pro lib]:file
    =/  has-imports=?
      ?&  (levy pro.file |=(pro:ford ~(exists pro stud)))
          (levy lib.file |=(lib:ford ~(exists lib name)))
      ==
    ?.  has-imports
      ~|  %no-imports
      !!
    =/  built-imports=?
      ?&  (levy pro.file |=(pro:ford ~(built pro stud)))
          (levy lib.file |=(lib:ford ~(built lib name)))
      ==
    ?.  built-imports
      [`[%neo pax] run]
    =/  pit=pith  (pave (snip pax))
    =^  pre=pith  run  
      (make-prelude pit file)
    =/  =conf:neo
      (~(gas by *conf:neo) [%sut (ours pre)] ~)
    [~ (do-make (ours pit) %nhoon `!>([~ hoon.file]) conf)]
  --
++  get-val-at-path
  |=  =pith
  ^-  (unit vase)
  ?~  val=(get:of-top pith)
    ~
  `state.icon.u.val
::
++  check-conf
  |=  [conf=(map term pith) =deps:neo]
  ^-  (set term)
  %+  roll  ~(tap by deps)
  |=  [[=term required=? =port:neo] out=(set term)]
  ?.  &(required !(~(has by conf) term))
    out
  (~(put in out) term)
++  hear-watch
  |=  =watch:neo
  ^+  run
  ?~  watch
    run
  ~|  hear/pith.i.watch
  =.  run  
    abet:(~(hear xeno [src.bowl pith.i.watch]) +.i.watch)
  $(watch t.watch)
++  husk
  |_  =code:neo
  +*  s-husk  ?>(?=(%stud -.code) ~(. stud-husk p.code))
      c-husk  ?>(?=(%clay -.code) ~(. clay-husk p.code))
  ++  firm
    ?:  ?=(%stud -.code)
      firm:s-husk
    firm:c-husk
  ++  firm-vase
    ?:  ?=(%stud -.code)
      firm-vase:s-husk
    firm-vase:c-husk
  ++  wire
    ?:  ?=(%stud -.code)
      wire:s-husk
    wire:c-husk
  --
++  stud-husk
  |_  =stud:neo
  ++  pith
    ^-  pith:neo
    :-  %src
    %-  pave:neo
    ?@  stud 
      /std/imp/[stud]
    ?:  =(our.bowl ship.stud)
      /our/[desk.stud]/imp/[mark.stud]
    /her/(scot %p ship.stud)/[desk.stud]/imp/[mark.stud]
  ++  firm-vase
    =/  rom=room:neo  (got:of-top pith)
    =+  !<([cac=(unit vase) *] state.icon.rom)
    (need cac)
  ++  firm
    !<(=firm:neo firm-vase)
  ++  wire
    %+  welp  /husk/stud
    (pout pith)
  --
++  clay-husk
  |_  =path
  ++  firm-vase
    .^(vase %ca path)
  ++  firm
    ^-  firm:neo
    !<(=firm:neo firm-vase)
  ++  wire
    ^-  ^wire
    (welp /husk/clay path)
  ++  watch
    ^+  run
    run
::    ?>  ?=(@ stud)
::    =/  =riff:clay
::      [q.byk.bowl `[%sing %a da/now.bowl spur]]
::    =/  wir  (snoc wire %build)
::    (emit %pass wir %arvo %c %warp our.bowl riff)
::  ++  take
::    |=  [=(pole knot) syn=sign-arvo]
::    ^+  run
::    ?>  ?=([%build ~] pole)
::    ?>  ?=([%clay %writ *] syn)
::    ?~  p.syn
::      ~&  bad-take-husk/pole
::      =.  husks  (~(del in husks) stud)
::      run
::    ?:  =(~ (~(get ju husks) stud))
::      run
::    watch
  ::
  --
++  xeno
  |_  =name:neo
  ++  xeno  .
  ++  abet  run
  ++  brig  (~(gut by fleet) ship.name *(axal:neo cell:neo))
  ++  cell  (~(gut of:neo brig) pith.name *cell:neo)
  ++  got   (~(got of:neo (~(got by fleet) ship.name)) pith.name)
  ++  gut   |=(c=cell:neo (~(gut of:neo (~(got by fleet) ship.name)) pith.name c))
  ++  has   (~(has of:neo brig) pith.name)
  ++  put
    |=  =cell:neo
    =/  =brig:neo  brig
    =.  brig  (~(put of:neo brig) pith.name cell)
    =.  fleet  (~(put by fleet) ship.name brig)
    xeno
  ++  wire  `^wire`xeno/(pout (en-pith:name:neo name))
  ++  dock  `^dock`[ship.name dap.bowl]
  ++  init
    |=  cod=code:neo
    ::  =.  run  (~(start husk src) name)
    (put 0 *vase [cod ~(firm husk cod)] *jail:neo)
  ++  watch
    =/  =path  [%sync %init (pout pith.name)]
    =.  run
      (emit %pass wire %agent dock %watch path)
    xeno
  ++  hear
    |=  [=stud:neo case=@ud =diff:neo]
    ^+  xeno
    ~|  hear-name/name
    =?  xeno  !has
      (init stud/stud)
    =+  cel=got 
    =/  firm  ~(firm-vase husk p.span.cel)
    =/  =vase
      ?:  ?=(%poke -.diff)
        =/  func  (slap firm !,(*hoon reduce:form))
        !<(vase (slym func p.diff))
      =+  ;;(state=stud:neo +:(slap firm !,(*hoon state)))
      =/  func   (need ~(grab pro stud))
      (slym func p.diff)
    (put cel(case case, state vase))
  --
::
++  take-local-agent
  |=  [=pith =sign:agent:gall]
  ^+  run
  ?+    -.sign  !!
      %poke-ack 
    %.  run
    ?~  p.sign
      same
    %+  slog
      leaf/"Poke-ack failed for shrub {(en-tape:pith:neo pith)}" 
    u.p.sign
  ::
  ==
::
++  give-nack
  |=  [src=pith err=tang]
  =/  nam=name:neo  (de-pith:name:neo src)
  ?:  =(1 1)  !!
  ?.  =(our.bowl ship.nam)
    run
  ?:  =(/ pith.nam) :: special case outside
    run
  run
++  do-hear
  |=  new=(list name:neo)
  ^+  run
  ?~  new
    run
  =.  run  (take-sound i.new)
  $(new t.new)
::
++  take-sound
  |=  =name:neo
  :: TODO: is ordering important here?
  =/  sounds  ~(tap in (~(get ju hear) name))
  |-  ^+  run
  ?~  sounds
    run
  =;  new=_run
    $(run new, sounds t.sounds)
  ?-  -.i.sounds
    %dep   (dep-change p.i.sounds name q.i.sounds)
    %sync  (send-sync name)
  ==
++  dep-change
  |=  [=term from=name:neo to=pith]
  abet:(take-neo:(abed:arvo to to) %conf %val term)
++  give
  |=  =gift:agent:gall
  (emit %give gift)
::
++  send-sync
  |=  =name:neo
  ^+  run
  ?.  =(our.bowl ship.name)
    ~&  sync-for-foreign/ship.name
    run
  =/  =room:neo  (got:of-top pith.name)
  =/  =cage
    :-  %neo-update
    !>  ^-  update:neo
    ?>  ?=(%stud -.p.span.room)
    [pith.name p.p.span.room [case %init q.state]:icon.room]
  =;  paths=(list path)
    ?:  =(~ paths)
      run
    (give %fact paths cage)
  =/  target=path
    %+  welp  /sync/init
    (pout pith.name)
  =-  ~(tap in -)
  %-  ~(gas in *(set path))
  %+  murn  ~(val by sup.bowl)
  |=   [=ship =path]
  ?.  (is-parent-p target path)
    ~
  `path
::
++  arvo
  =+  verb=&
  |_  $:  [src=pith init=pith here=pith]
          [done=(list move:neo) down=(list move:neo) up=(list move:neo) change=(set pith)]
          [old=state-0 err=(unit tang)]
      ==
  ++  abet  
    ?~  err  
      %-  (slog (turn up trace-card))
      =.  cards  (welp cards (turn up deal))
      (do-hear (turn ~(tap in change) (lead our.bowl)))
    =.  state  old
    (give-nack src u.err)
  ++  abed
    |=  [source=pith ini=pith]
    ^+  arvo
    %_  arvo
      src   source
      init  ini
      here  ini
      old   state
    ==
  ++  deal
    |=  =move:neo
    ^-  card
    :+  %pass  local/(pout p.move)
    ^-  note:agent:gall
    =/  her=ship
      ~|  p.move
      ?>  ?=([[%p @p] *] p.q.move)
      +.i.p.q.move
    [%agent [her dap.bowl] %poke neo-move+!>(move)]
  ++  arvo  .
  ++  emit  |=(=move:neo arvo(down [move down]))
  ++  trace-card
    |=  =move:neo
    ^-  tank
    :-  %leaf
    "{(en-tape:pith:neo p.move)} -> {(en-tape:pith:neo p.q.move)}: {<-.q.q.move>}"
  ++  trace
    |=  =tang
    ?.  verb  same
    %.  tang
    %*  .  slog
      pri  2
    ==
  ++  inside  (cury is-parent init)
  ++  echo  arvo  :: TODO walk done
  ++  work
    ^+  arvo
    |-  ^+  arvo
    ?^  err
      arvo
    ?~  down
      arvo
    =/  nex=move:neo  i.down
    =/  new-arvo  (apply:arvo(down t.down) nex) :: XX: weird compiler?
    $(arvo new-arvo, done (snoc done nex))
  ++  take-neo
    |=  syn=sign-neo:neo
    %-  (trace leaf/"{(en-tape:pith:neo here)}: take-neo {<-.syn>}" ~)
    =^  caz=(list card:neo)  arvo  
      (soft-site |.(si-abet:(si-take-neo:site syn)))
    (ingest caz)
  ::
  ++  take-arvo
    |=  [=pith syn=sign-arvo]
    ^+  arvo
    =^  caz=(list card:neo)  arvo
      (soft-site |.(si-abet:(si-take-arvo:site syn)))
    (ingest caz)
  ++  poke
    |=  val=*
    ^-  (quip card:neo _arvo)
    (soft-site |.(si-abet:(si-poke:site val)))
  ::
  ++  apply
    |=  =move:neo
    ^+  arvo
    ?.  =(~ err)
      arvo
    =.  src   p.move
    =/  =name:neo  (de-pith:name:neo p.q.move)
    =.  here       pith.name
    %-  (trace leaf/"{<-.q.q.move>} {(spud (pout here))}" ~)
    =^  caz=(list card:neo)  arvo
      ?+    -.q.q.move  !!
          %make  (make +.q.q:move)
          %poke  (poke +.q.q:move)
          %link   !!
        :: :-  ~
        ::=.  run  (link [p from.q src.q]:note)
        :: 0arvo
      ==
    (ingest caz)
  ++  ingest
    |=  caz=(list card:neo)
    ^+  arvo
    =/  =pith  here
    =.  up
      %+  welp  up
      %+  murn  caz
      |=  =card:neo
      ^-  (unit move:neo)
      :: =/  inside  +.card
      =/  =name:neo  (de-pith:name:neo p.card)
      ?.  =(our.bowl ship.name)
        `[pith card]
      ?.  (is-parent pith pith.name)
        ~
      `[pith card]

    =.  down
      %-  welp
      :_  down
      %+  murn  caz
      |=  =card:neo
      ^-  (unit move:neo)
      =/  =name:neo  (de-pith:name:neo p.card)
      ?.  =(our.bowl ship.name)
        ~
      ?.  (is-parent pith pith.name)
        ~
      `[pith card]
    work
  ::
  ++  make
    |=  [src=code:neo init=(unit vase) =conf:neo]
    =/  =firm:neo  ~(firm husk src)
    ~&  here
    :: =.  run        (~(start husk src) our.bowl pith)
    =/  =form:neo  form:firm
    =/  =span:neo  [src firm]
    =/  =icon:neo  [1 (init:form init) ~ ~]
    =/  =deps:neo  deps:firm
    ?>  =(~ (check-conf conf deps:firm))
    =/  =room:neo  [span conf icon]
    =.  apex  (put:of-top here room)
    (soft-site |.(si-abet:si-born:site))

  ++  soft-site
    |=  tap=(trap (quip card:neo _arvo))
    ^-  (quip card:neo _arvo)
    =/  res=(each (quip card:neo _arvo) tang)
      (mule tap)
    ?:  ?=(%& -.res)
      p.res
    =.  err  `p.res
    `arvo
  ::
  ++  site
    =/  =room:neo
      (got:of-top here)
    =|  cards=(list card:neo)
    |%  
    ++  site  .
    ++  si-emil  |=(caz=(list card:neo) site(cards (welp cards caz)))
    ++  si-abet
      =.  apex  (put:of-top here room)
      [cards arvo]
    ::
    ++  si-resolve-kids  ~
    ++  si-resolve-deps
      %-  ~(gas by *(map term [pith vase]))
      ^-  (list [term pith vase])
      %+  murn  ~(tap by deps:si-firm)
      |=  [=term required=? =port:neo]
      ^-  (unit [^term pith vase])
      =/  dep=(unit pith)  (~(get by conf.room) term)
      ?~  dep
        ~|  invariant-missing-required-conf/term
        ?<  required
        ~
      =/  =name:neo  (de-pith:name:neo u.dep)
      ?>  =(our.bowl ship.name)
      =/  val  (get-val-at-path pith.name)
      ?~  val
        ~|  invariant-no-value-at-path/pith.name
        !!
      `[term u.dep u.val]
      ::  TODO type this w/ port??
    ++  si-bowl    
      [src.bowl our.bowl [p/our.bowl here] now.bowl si-resolve-deps si-resolve-kids]
    ++  si-form    ~(. form:si-firm [si-bowl icon.room])
    ++  si-firm    q.span.room
    ++  si-tell
      =/  paths  si-sync-paths
      =.  change  (~(put in change) here)
      ?:  =(paths ~)
        site
      =.  run   
        (^emit %give %fact paths neo-watch+!>(si-grab-watch))
      site
    ++  si-grab-watch
      ^-  watch:neo
      ?.  ?=(%stud -.p.span.room)
        *watch:neo
      [here p.p.span.room [case %init q.state]:icon.room]~
    ++  si-sync-paths
      ^-  (list path)
      =-  ~(tap in -)
      %-  ~(gas in *(set path))
      %+  murn  ~(val by sup.bowl)
      |=  [=ship =path]
      ::  TODO: tighten
      ^-  (unit _path)
      ?.  ?=([%sync *] path)  
        ~
      `path
    ++  si-born   
      ^+  site
      =.  site  (si-emil born:si-form)
      si-tell
    ++  si-poke
      |=  val=*
      ^+  site
      =/  old-state  state.icon.room
      =.  state.icon.room   (reduce:si-form val)
      =.  site  (si-emil (call:si-form old-state val))
      ?:  =(old-state state.icon.room)
        site
      =.  case.icon.room  +(case.icon.room)
      si-tell
    ::
    ++  si-take-neo
      |=  syn=sign-neo:neo
      ^+  site
      site :: (si-emil (take:si-form neo/syn))
    ++  si-take-arvo
      |=  syn=sign-arvo
      site :: (si-emil (take:si-form arvo/syn))
    --
  --
++  sock
  |_  for=@tas
  ++  spur
    ^-  path
    /lib/plan/[for]/hoon
  ++  resolve
    ^-  path
    %+  welp
      /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)
    spur
  ++  sock-vase
    .^(vase %ca resolve)
  ++  plan  form:(pike:neo ewer:neo)
  ++  get  
    !<(plan sock-vase)
  --
++  conn
  |=  =id:sole
  =/  =shell  [[our.bowl ~] ~]
  =.  shells  (~(put by shells) id shell)
  ~(start walk id)
++  tell
  |_  =name:neo
  ++  get
    ^-  (unit room:neo)
    ?.  =(our.bowl ship.name)
      ~
    (get:of-top pith.name)
  ++  kids  (desc 0)
  ++  desc
    =/  dip
      ?>  =(our.bowl ship.name)
      (dip:of-top pith.name)
    |=  depth=@ud
    =*  loop  $
    ^-  (list (list dime))
    %-  zing
    %+  turn  ~(tap by kid.dip)
    |=  [seg=iota ax=(axal:neo room:neo)]
    ^-  (list (list dime))
    =.  pith.name  (snoc pith.name seg)
    =/  res  ?:(=(depth 0) ~ loop(dip ax, depth +(depth)))
    ?~  fil.ax  
      :_  res
      ~[t/(spat (pout pith.name)) t/'Directory' ud/0]
    :_  res
    row:item
  ++  item
    =/  rom  get
    |%
    ++  code
      |=  =code:neo
      ^-  @t
      %-  spat
      :-  -.code
      ?:  ?=(%clay -.code)
        (rear (snip p.code))^~
      ^-  path
      ?@  p.code
        /kelvin/(scot %ud zuse)/[p.code]
      =,  p.code
      /(scot %p ship)/[desk]/[mark]
    ++  row
      ^-  (list dime)
      ?~  rom
        ~
      :~  t/(spat (pout pith.name))
          t/(code p.span.u.rom)
          ud/case.icon.u.rom
      ==
    --
  ++  show
    ^-  tang
    =/  rom  get
    %-  lure
    ?~  rom
      leaf/"No data"
    (sell state.icon.u.rom)
  --
++  walk
  |_  =id:sole
  ++  start 
    abet:prompt:peel
  ++  drop
    ^+  run
    run(shells (~(del by shells) id))
  ++  peel
    =/  =shell  (~(got by shells) id)
    |%  
    ++  peel  .
    ++  abet  
      run(shells (~(put by shells) id shell))
    ++  tell  ~(. ^tell cwd.shell)
    ++  race   (need race.shell)
    ++  has-race  !=(~ race.shell)
    ++  add-race
      |=  r=^race
      =.  race.shell  `r
      peel
    ++  del-race
      =.  race.shell  ~
      peel
    ++  prompt
      |^
      =;  ef=shoe-effect:shoe
        =.  run  (shoe-ef ef)
        peel
      :-  %sole
      :^  %pro  &  %foo
      ^-  styx
      =,  shell
      :-  [un ~['/' (scot %p ship.cwd)]]
      %-  snoc
      :_  '> '
      ^-  (list @t)
      %-  zing
      ^-  (list (list @t))
      %+  turn  pith.cwd
      |=  =iota:neo
      ^-  (list @t)
      =-  ~['/' -]
      ?@  iota  iota
      (scot iota)
      ++  un
        `styl`[`%un ~ ~]
      --
    ++  cwd
      |%
      ++  get  cwd.shell
      ++  set  |=(n=name:neo =.(cwd.shell n prompt))
      --
    --
  ++  lily
    |*  [naf=@ sab=rule]
    =+  vex=(sab [1 1] (trip naf))
    ?~  q.vex  ~  
    [~ u=p.u.q.vex] 
  ++  default-list
    ^-  (list [@t tank]) 
    :~  'ls'^leaf/"List child shrubs at current path"
        'cd'^leaf/"Change directory"
        '.'^leaf/"Print node at path"
        't'^leaf/"List child shrubs at current path, recursively"
    ==
  ++  tab
    |=  query=@t 
    =/  query  (trip query)
    =+  vex=(parser [1 1] query)
    ?~  q.vex
      default-list
    =/  [[? =hull] =nail]  u.q.vex
    =/  parsed  (scag (sub (lent query) (lent q.nail)) query)
    |^  ^-  (list [@t tank])
    ?+  -.hull  ~
      %cd  (cd name.hull)
    ==
    ++  cd
      |=  =name:neo
      ^-  (list [@t tank])
      =/  dip  (dip:of-top pith.name)
      =/  last
        ?:(=(~ pith.name) %$ (rear pith.name))
      =/  remove-len  (met 3 (show-iota last))
      =?  pith.name  =([~ ~] dip)
        (snip pith.name)
      =?  parsed     =([~ ~] dip)
        (scag (sub (lent parsed) remove-len) parsed)
      =.  dip
        (dip:of-top pith.name)
      ?:  =(~ kid.dip)
        ~
      %+  turn  ~(tap by kid.dip)
      |=  [seg=iota ax=(axal:neo room:neo)]
      ^-  [@t tank]
      :_  *tank
      %+  cat  3
      :-  (crip parsed)
      ?@  seg  seg
      (scot seg)
    --
  ++  parser
    |^  ^+  |~(nail *(like [? hull]))
    %+  stag  |
    ?:  has-race:peel
      %+  stag  %clot
      clot
    ;~  pose
      :: (csym %ls (easy ~))

      (cold ls/~ (jest 'ls'))
      (cold show/~ dot)
      (stag %tree ;~(pfix (jest 't') dem:ag))
      (stag %poke ;~(pfix (jest 'p') ace sym))
    ::
      cd
      ;~(pfix wut (cold clay/~ (jest 'clay')))
      ;~(pfix hax (cold comm/~ (star prn)))
    ==
    ++  cd
      :: ^-  _|~(nail *(like hull))
      :: %+  csym  %cd 
      %+  stag  %cd
      %+  ifix  [(jest 'cd ') (easy ~)]
      ;~  pose
        rule:name:neo
        %+  sear
          |=  [kets=(list *) =pith:neo]
          ^-  (unit name:neo)
          =/  cwd  get:cwd:peel
          =/  up  (lent kets)
          ?:  (gth up (lent pith.cwd))
            ~
          =.  pith.cwd  (scag (sub (lent pith.cwd) up) pith.cwd)
          `cwd(pith (welp pith.cwd pith))
        ;~(plug (star ket) (more fas spot:stip:neo))
      ==
    ++  csym
      |*  [term=* rul=rule]
      (stag term ;~(pfix (jest term) rul))
    ++  clot
      :: ^-  _|~(nail *(like [? gait]))
      =/  race  race:peel
      ^-  $-(nail (like clot:goon:neo))
      ?>  ?=(^ grab.race)
      ?+  scar.i.grab.race  !!
        %cord   (stag %cord (cook crip (star prn)))
        %patud  (stag %patud dem:ag)
      ==
  --
  ++  shoe-ef
    |=  ef=shoe-effect:shoe
    ^+  run
    (emit %shoe ~[id] ef)
  ++  do 
    |=  =hull
    |^  ^+  run
    ?-  -.hull 
      %clot   abet:(clot:hike clot.hull)
      %show   (shoe-ef %sole %tan show:tell:peel)
      %ls     (tree 0)
      %cd     abet:(set:cwd:peel name.hull)
      %tree   (tree depth.hull)
      %poke   (poke rout.hull)
      %comm   run
      %clay   clay
    ==
    ++  clay
      =/  rom  (got:of-top pith:get:cwd:peel)
      =+  !<([cac=(unit vase) *] state.icon.rom)
      =/  desc=@t 
        ?~  cac
          'No cache'
        'Has cache'
      =/  sho=shoe-effect:shoe
        [%sole %klr ~[desc]]
      (shoe-ef sho)

    ++  poke
      |=  rout=@tas
      ^+  run
      =/  bad=shoe-effect:shoe
        [%sole %klr ~['No pokes here']]
      =/  cwd  get:cwd:peel
      ?~  rom=(get:of-top pith.cwd)
        (shoe-ef bad)
      =/  =plan:sock  ~(get sock rout)
      =/  =race  [rout plan ~ ~]
      =.  run  abet:(add-race:peel rout plan ~ ~)
      abet:start:hike
        
    ++  tree
      |=  dep=@ud
      %-  shoe-ef  
      :-  %table  
      :+  (limo tas/%path tas/%code tas/%case ~)
        (limo 40 40 8 ~)
      (desc:tell:peel dep)
    --
  ++  hike
    =/  =race  race:peel
    |%
    ++  abet   `_run`?~(grab.race abet:del-race:peel abet:(add-race:peel race))
    ++  make-bowl
      `bowl:pike:neo`[our.bowl get:cwd:peel eny.bowl now.bowl]
    ++  hike   .
    ++  start  ^+(hike (take ~))
    ++  clot
      |=  =clot:goon:neo
      ^+  hike
      =.  have.race  (snoc have.race clot)
      =.  grab.race
        ?>  ?=(^ grab.race)
        t.grab.race
      ?^  grab.race
        show-grab
      =/  have  have.race
      =.  have.race  ~
      (take `[%grab have])
    ++  take
      |=  syn=(unit sign:pike:neo)
      |-  ^+  hike
      =/  pike  (pike:neo ewer:neo)
      =^  res=eval-result:pike  form.race
        (take:pike form.race [make-bowl syn])
      ?-    -.res
          %done
        =/  =pith:neo  (en-pith:name:neo get:cwd:peel)
        =/  =card:neo  [pith %poke q.q.value.res]
        =.  run  (on-card card) 
        =;  ef=shoe-effect:shoe
          =.  run  (shoe-ef ef)
          hike
        :+  %sole  %klr
        =/  =styl  [~ ~ `%g]
        =/  txt=styx  ~['Poke success']
        `styx`~[styl^txt]
      ::
          %fail
        =.  races  (~(del by races) id)
        hike
          %emit  
        ?-    -.car.res
        :: TODO: actually scry
            %peek  $(syn `[%peek addr-info/!>(['New York' 'NY'])])
            %grab
          =.  grab.race  items.car.res
          show-grab
        ==
      ==
    ++  show-grab
      |-  ^+  hike
      ?~  grab.race
        hike
      =/  =item:pike:neo  i.grab.race
      =;  ef=shoe-effect:shoe
        =.  run  (shoe-ef ef)
        hike
      :+  %sole  %klr
      ~[(crip "{(trip lede.item)}: {(trip info.item)}")]
    --

::  ++  start
::    |=  soc=@tas
::    ^+  run
::    =/  =race
::      [soc ~(form sock soc) ~ ~]
::    =.  races  (~(put by races) id race)
::    (take ~)
    :: ?~  
    :: =/  =wire  /race/(scot %p who.id)/[ses.id]
    :: (emit %pass wire %agent [
  --
--
