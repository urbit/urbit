!:
::  dill (4d), terminal handling
::
|=  our=ship
=,  dill
=>  |%                                                  ::  interface tiles
+$  gill  (pair ship term)                              ::  general contact
--                                                      ::
=>  |%                                                  ::  console protocol
+$  axle                                                ::
  $:  %7                                                ::
      hey=(unit duct)                                   ::  default duct
      dug=(map @tas axon)                               ::  conversations
      eye=(jug @tas duct)                               ::  outside observers
      ear=(set duct)                                    ::  syslog listeners
      lit=?                                             ::  boot in lite mode
      egg=_|                                            ::  see +take, removeme
  ==                                                    ::
+$  axon                                                ::  dill session
  $:  ram=term                                          ::  console program
      tem=(unit (list dill-belt))                       ::  pending, reverse
      wid=_80                                           ::  terminal width
  ==                                                    ::
+$  log-level  ?(%hush %soft %loud)                     ::  none, line, full
--  =>                                                  ::
|%                                                      ::  protocol outward
+$  mess                                                ::
  $%  [%dill-poke p=(hypo poke)]                        ::
  ==                                                    ::
+$  move  [p=duct q=(wind note gift)]                   ::  local move
+$  note                                                ::  out request $->
  $~  [%d %verb ~]                                      ::
  $%  $:  %$                                            ::
          $>(?(%verb %whey) waif)                       ::
      ==                                                ::
      $:  %c                                            ::
          $>  $?  %merg                                 ::  merge desks
                  %perm                                 ::  change permissions
                  %warp                                 ::  wait for clay hack
                  %zest                                 ::
              ==                                        ::
          task:clay                                     ::
      ==                                                ::
      $:  %d                                            ::
          $>  $?  %crud                                 ::
                  %heft                                 ::
                  %text                                 ::
                  %verb                                 ::
              ==                                        ::
          task:dill                                     ::
      ==                                                ::
      $:  %g                                            ::
          $>(%deal task:gall)                           ::
      ==                                                ::
      $:  %j                                            ::
          $>  $?  %dawn                                 ::
                  %fake                                 ::
              ==                                        ::
          task:jael                                     ::
  ==  ==                                                ::
+$  sign                                                ::  in result $<-
  $~  [%dill %blit ~]                                   ::
  $%  $:  %behn                                         ::
          $%  $>(%writ gift:clay)                       ::  XX %slip
              $>(%mere gift:clay)                       ::  XX %slip
      ==  ==                                            ::
      $:  %clay                                         ::
          $>  $?  %mere                                 ::
                  %writ                                 ::
              ==                                        ::
          gift:clay                                     ::
      ==                                                ::
      $:  %dill                                         ::
          $>(%blit gift:dill)                           ::
      ==                                                ::
      $:  %gall                                         ::
          $>(%unto gift:gall)                           ::
  ==  ==                                                ::
::::::::                                                ::  dill tiles
--
=|  all=axle
|=  [now=@da eny=@uvJ rof=roof]                         ::  current invocation
=>  ~%  %dill  ..part  ~
    |%
    ++  as                                              ::  per cause
      =|  moz=(list move)
      |_  [hen=duct ses=@tas axon]
      ++  abet                                          ::  resolve
        ^-  [(list move) axle]
        [(flop moz) all(dug (~(put by dug.all) ses +<+>))]
      ::
      ++  call                                          ::  receive input
        |=  kyz=task
        ^+  +>
        ?+    -.kyz  ~&  [%strange-kiss -.kyz]  +>
          %hail  (send %hey ~)
          %belt  (send `dill-belt`p.kyz)
          %blew  (send(wid p.p.kyz) %rez p.p.kyz q.p.kyz)
          %heft  (pass /whey %$ whey/~)
          %meld  (dump kyz)
          %pack  (dump kyz)
          %crop  (dump trim+p.kyz)
          %verb  (pass /verb %$ kyz)
        ::
            %seat
          %^  pass  /seat  %g
          :+  %deal   [our our /dill]
          [%hood %poke %kiln-install !>([desk.kyz our desk.kyz])]
        ==
      ::
      ++  crud
        |=  [err=@tas tac=tang]
        =-  +>.$(moz (weld - moz))
        %+  turn
          ~(tap in ear.all)
        (late %give %logs %crud err tac)
      ::
      ++  dump                                          ::  pass down to hey
        |=  git=gift
        ?>  ?=(^ hey.all)
        +>(moz [[u.hey.all %give git] moz])
      ::
      ++  done                                          ::  gift to viewers
        |=  git=gift
        =-  +>.$(moz (weld - moz))
        %+  turn
          ~(tap in (~(get ju eye.all) ses))
        |=(=duct [duct %give git])
      ::
      ++  deal                                          ::  pass to %gall
        |=  [=wire =deal:gall]
        (pass wire [%g %deal [our our /dill] ram deal])
      ::
      ++  pass                                          ::  pass note
        |=  [=wire =note]
        +>(moz :_(moz [hen %pass wire note]))
      ::
      ++  from                                          ::  receive blit
        |=  bit=dill-blit
        ^+  +>
        ?:  ?=(%qit -.bit)
          (dump %logo ~)
        ::TODO  so why is this a (list blit) again?
        (done %blit bit ~)
      ::
      ++  sponsor
        ^-  ship
        =/  dat=(unit (unit cage))
          (rof `[our ~ ~] /dill j/[[our sein/da/now] /(scot %p our)])
        ;;(ship q.q:(need (need dat)))
      ::
      ++  init                                          ::  initialize
        (pass /merg/base [%c %merg %kids our %base da+now %init])
      ::
      ++  mere                                          ::  continue init
        ^+  .
        =/  myt  (flop (fall tem ~))
        =.  tem  ~
        =.  ..mere  (pass /zest %c %zest %base %live)
        =.  ..mere  (show-desk %kids)
        =.  ..mere  (open ~)
        |-  ^+  ..mere
        ?~  myt  ..mere
        $(myt t.myt, ..mere (send i.myt))
      ::
      ++  into                                          ::  preinitialize
        |=  gyl=(list gill)
        =.  tem  `(turn gyl |=(a=gill [%yow a]))
        (pass / [%c %warp our %base `[%sing %y [%ud 1] /]])
      ::
      ++  open
        |=  gyl=(list gill)
        ::TODO  should allow handlers from non-base desks
        ::TODO  maybe ensure :ram is running?
        =.  +>  peer
        %+  roll  gyl
        |=  [g=gill _..open]
        (send [%yow g])
      ::
      ++  send                                          ::  send action
        |=  bet=dill-belt
        ^+  +>
        ?^  tem
          +>(tem `[bet u.tem])
        (deal /send/[ses] [%poke [%dill-poke !>([ses bet])]])
      ::
      ++  peer
        (deal /peer/[ses] %watch /dill/[ses])
      ::
      ++  pull
        (deal /peer/[ses] %leave ~)
      ::
      ++  show-desk                                     ::  permit reads on desk
        |=  des=desk
        (pass /show [%c %perm des / r+`[%black ~]])
      ::
      ++  take                                          ::  receive
        |=  [tea=wire sih=sign]
        ^+  +>
        ?-    sih
            [%gall %unto *]
          ::  ~&  [%take-gall-unto +>.sih]
          ?-    -.+>.sih
              %raw-fact   !!
              %kick       peer
              %poke-ack   ?~(p.p.+>.sih +>.$ (crud %coup u.p.p.+>.sih))
              %watch-ack
            ?~  p.p.+>.sih
              +>.$
            (dump:(crud %reap u.p.p.+>.sih) %logo ~)
          ::
              %fact
            ?.  ?=(%dill-blit p.cage.p.+>.sih)
              +>.$
            (from ;;(dill-blit q.q.cage.p.+>.sih))
          ==
        ::
            [?(%behn %clay) %writ *]
          init
        ::
            [?(%behn %clay) %mere *]
          ?:  ?=(%& -.p.sih)
            mere
          (mean >%dill-mere-fail< >p.p.p.sih< q.p.p.sih)
        ::
            [%dill %blit *]
          (done +.sih)
        ==
      --
    ::
    ++  ax                                              ::  make ++as from name
      |=  [hen=duct ses=@tas]
      ^-  (unit _as)
      =/  nux  (~(get by dug.all) ses)
      ?~  nux  ~
      (some ~(. as hen ses u.nux))
    ::
    ++  aw                                              ::  make ++as from wire
      |=  [hen=duct wir=wire]
      ^-  (unit _as)
      %+  ax  hen
      ?+  wir  %$
        [?(%peer %send) @ *]  i.t.wir
      ==
    --
|%                                                      ::  poke+peek pattern
++  call                                                ::  handle request
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^+  [*(list move) ..^$]
  =/  task=task
    ~|  wrapped-task
    ((harden task) wrapped-task)
  ~|  -.task
  ::  unwrap session tasks, default to session %$
  ::
  =^  ses=@tas  task
    ?:(?=(%shot -.task) +.task [%$ task])
  ::  error notifications "downcast" to %crud
  ::
  =?  task  ?=(^ dud)
    ~|  %crud-in-crud
    ?<  ?=(%crud -.task)
    [%crud -.task tang.u.dud]
  ::
  ::  the boot event passes thru %dill for initial duct distribution
  ::
  ?:  ?=(%boot -.task)
    ?>  ?=(?(%dawn %fake) -.p.task)
    ?>  =(~ hey.all)
    =.  hey.all  `hen
    =/  boot
      ((soft $>($?(%dawn %fake) task:jael)) p.task)
    ?~  boot
      ~&  %dill-no-boot
      ~&  p.task
      ~|  invalid-boot-event+hen  !!
    =.  lit.all  lit.task
    [[hen %pass / %j u.boot]~ ..^$]
  ::  we are subsequently initialized.
  ::
  ?:  ?=(%init -.task)
    ?>  =(~ dug.all)
    ::  configure new terminal, setup :hood and %clay
    ::
    =*  duc  (need hey.all)
    =/  app  %hood
    =/  say  (tuba "<awaiting {(trip app)}, this may take a minute>")
    =/  zon=axon  [app input=[~ ~] width=80]
    ::
    =^  moz  all  abet:(~(into as duc %$ zon) ~)
    =.  eye.all   (~(put ju eye.all) %$ duc)
    [moz ..^$]
  ::  %flog tasks are unwrapped and sent back to us on our default duct
  ::
  ?:  ?=(%flog -.task)
    ?~  hey.all
      [~ ..^$]
    ::  this lets lib/helm send %heft a la |mass
    ::
    =?  p.task  ?=([%crud %hax-heft ~] p.task)  [%heft ~]
    ::
    $(hen u.hey.all, wrapped-task p.task)
  ::  %vega and %trim notifications come in on an unfamiliar duct
  ::
  ?:  ?=(?(%trim %vega) -.task)
    [~ ..^$]
  ::  %knob used to set a verbosity level for an error tag,
  ::  but dill no longer prints errors itself, so implementing %knob
  ::  has become a recommendation to error printers (like drum).
  ::  remove this when %knob gets removed from lull, next™ kelvin release.
  ::
  ?:  ?=(%knob -.task)
    ~&  [%dill %knob-deprecated]
    [~ ..^$]
  ::  %open opens a new dill session
  ::
  ?:  ?=(%open -.task)
    ?:  (~(has by dug.all) ses)
      ::TODO  should we allow, and just send the %yow blits?
      ~|  [%cannot-open-existing ses]
      !!
    =/  zon=axon  [p.task ~ width=80]
    =^  moz  all  abet:(~(open as hen ses zon) q.task)
    =.  eye.all  (~(put ju eye.all) ses hen)
    [moz ..^$]
  ::  %shut closes an existing dill session
  ::
  ?:  ?=(%shut -.task)
    ?:  =(%$ ses)
      ~|  %cannot-shut-default-session
      !!
    =/  nus
      ~|  [%no-session ses]
      (need (ax hen ses))
    ::NOTE  we do deletion from state outside of the core,
    ::      because +abet would re-insert.
    ::TODO  send a %bye blit? xx
    =^  moz  all  abet:pull:nus
    =.  dug.all   (~(del by dug.all) ses)
    =.  eye.all   (~(del by eye.all) ses)
    [moz ..^$]
  ::  %view opens a subscription to the target session, on the current duct
  ::
  ?:  ?=(%view -.task)
    =/  nus
      ::  crash on viewing non-existent session
      ::
      ~|  [%no-session ses]
      (need (ax hen ses))
    ::  register the viewer and send a %hey so they get the full screen
    ::
    =^  moz  all
      abet:(send:nus %hey ~)
    :-  moz
    ..^$(eye.all (~(put ju eye.all) ses hen))
  ::  %flee closes a subscription to the target session, from the current duct
  ::
  ?:  ?=(%flee -.task)
    :-  ~
    ..^$(eye.all (~(del ju eye.all) ses hen))
  ::  %logs opens or closes a subscription to system output
  ::
  ?:  ?=(%logs -.task)
    =.  ear.all
      ?~  p.task  (~(del in ear.all) hen)
      (~(put in ear.all) hen)
    [~ ..^$]
  ::  if we were $told something, give %logs to all interested parties
  ::
  ?:  ?=(?(%crud %talk %text) -.task)
    :_  ..^$
    (turn ~(tap in ear.all) (late %give %logs task))
  ::
  =/  nus
    (ax hen ses)
  ?~  nus
    ::  session :ses does not exist
    ::  could be before %boot (or %boot failed)
    ::
    ~&  [%dill-call-no-session ses hen -.task]
    [~ ..^$]
  ::
  =^  moz  all  abet:(call:u.nus task)
  [moz ..^$]
::
++  load                                                ::  import old state
  =<  |=  old=any-axle
      ?-  -.old
        %7  ..^$(all old)
        %6  $(old (axle-6-to-7 old))
        %5  $(old (axle-5-to-6 old))
        %4  $(old (axle-4-to-5 old))
      ==
  |%
  +$  any-axle  $%(axle axle-6 axle-5 axle-4)
  ::
  +$  axle-6
    $:  %6
        hey=(unit duct)
        dug=(map @tas axon)
        eye=(jug @tas duct)
        lit=?
        veb=(map @tas log-level)
        egg=_|
    ==
  ::
  ++  axle-6-to-7
    |=  a=axle-6
    ^-  axle
    [%7 hey dug eye ~ lit egg]:a
  ::
  +$  axle-5
    $:  %5
        hey=(unit duct)                                   ::  default duct
        dug=(map @tas axon)                               ::  conversations
        eye=(jug @tas duct)                               ::  outside listeners
        lit=?                                             ::  boot in lite mode
        veb=(map @tas log-level)
    ==
  ::
  ++  axle-5-to-6
    |=  a=axle-5
    ^-  axle-6
    :: [%6 hey `(map @tas axon)`dug eye lit veb |]
    a(- %6, veb [veb.a &])
  ::
  +$  axle-4
    $:  %4
        hey=(unit duct)
        dug=(map duct axon-4)
        eye=(jug duct duct)
        lit=?
        veb=(map @tas log-level)
    ==
  ::
  +$  axon-4
    $:  ram=term
        tem=(unit (list dill-belt-4))
        wid=_80
        pos=$@(@ud [@ud @ud])
        see=$%([%lin (list @c)] [%klr stub])
    ==
  ::
  +$  dill-belt-4
    $%  [%ctl p=@c]
        [%met p=@c]
        dill-belt
    ==
  ::
  ++  axle-4-to-5
    |=  axle-4
    ^-  axle-5
    :-  %5
    =-  [hey nug nay lit veb]
    %+  roll  ~(tap by dug)
    |=  [[=duct =axon-4] nug=(map @tas axon) nay=(jug @tas duct)]
    =/  ses=@tas
      ~|  [%unexpected-duct duct]
      ?>(=([//term/1]~ duct) %$)
    :-  (~(put by nug) ses (axon-4-to-5 axon-4))
    %+  ~(put by nay)  ses
    (~(put in (~(get ju eye) duct)) duct)
  ::
  ++  axon-4-to-5
    |=  axon-4
    ^-  axon
    =;  tem  [ram tem wid]
    ?~  tem  ~
    %-  some
    %+  turn  u.tem
    |=  b=dill-belt-4
    ^-  dill-belt
    ?.  ?=(?(%ctl %met) -.b)  b
    [%mod -.b p.b]
  --
::
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem
  ::
  ?.  ?=(%& -.why)  ~
  =*  his  p.why
  ::
  ::  only respond for the local identity, %$ desk, current timestamp
  ::
  ?.  ?&  =(&+our why)
          =([%$ %da now] lot)
          =(%$ syd)
      ==
    ~
  ::  /%x//whey           (list mass)   memory usage labels
  ::  /dy/sessions        (set @tas)    all existing sessions
  ::  /du/sessions/[ses]  ?             does session ses exist?
  ::
  ?+  [ren tyl]  ~
    [%x %$ %whey ~]     =-  ``mass+!>(`(list mass)`-)
                        [hey+&+hey.all dug+&+dug.all ~]
  ::
    [%y %sessions ~]    ``noun+!>(~(key by dug.all))
    [%u %sessions @ ~]  ``noun+!>((~(has by dug.all) (snag 1 tyl)))
  ==
::
++  stay  all
::
++  take                                                ::  process move
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^+  [*(list move) ..^$]
  ?^  dud
    ~|(%dill-take-dud (mean tang.u.dud))
  ::
  =;  [moz=(list move) lax=_..^$]
    =?  moz  egg.all.lax
      ::  dill pre-release (version %5) in some cases ended up in a state
      ::  where it had both an old-style and new-style subscription open
      ::  for the default session. here, we obliterate both and establish
      ::  only the new-style subscription.
      ::
      =/  hey  (need hey.all.lax)
      =/  =sack  [our our /dill]
      :*  [hey %pass / %g %deal sack %hood %leave ~]
          [hey %pass [%peer %$ ~] %g %deal sack %hood %leave ~]
          [hey %pass [%peer %$ ~] %g %deal sack %hood %watch [%dill %$ ~]]
          moz
      ==
    =.  egg.all.lax  |
    [moz lax]
  ::
  =/  nus  (aw hen tea)
  ?~  nus
    ::  :tea points to an unrecognized session
    ::
    ~&  [%dill-take-no-session tea -.hin +<.hin]
    [~ ..^$]
  =^  moz  all  abet:(take:u.nus tea hin)
  [moz ..^$]
--
