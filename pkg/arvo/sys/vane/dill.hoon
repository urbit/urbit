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
  $:  %6                                                ::
      hey=(unit duct)                                   ::  default duct
      dug=(map @tas axon)                               ::  conversations
      eye=(jug @tas duct)                               ::  outside listeners
      lit=?                                             ::  boot in lite mode
      $=  veb                                           ::  vane verbosities
      $~  (~(put by *(map @tas log-level)) %hole %soft) ::  quiet packet crashes
      (map @tas log-level)                              ::
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
          %talk  (talk p.kyz)
          %text  (fore (tuba p.kyz) ~)
          %crud  ::  (send `dill-belt`[%cru p.kyz q.kyz])
                 (crud p.kyz q.kyz)
          %blew  (send(wid p.p.kyz) %rez p.p.kyz q.p.kyz)
          %heft  (pass /whey %$ whey/~)
          %meld  (dump kyz)
          %pack  (dump kyz)
          %crop  (dump trim+p.kyz)
          %verb  (pass /verb %$ kyz)
        ::
            %seat
          %^  pass  /seat  %g
          :+  %deal   [our our]
          [%hood %poke %kiln-install !>([desk.kyz our desk.kyz])]
        ==
      ::
      ++  crud
        |=  [err=@tas tac=(list tank)]
        ::  unknown errors default to %loud
        ::
        =/  lev=log-level  (~(gut by veb.all) err %loud)
        ::  apply log level for this error tag
        ::
        ?-  lev
          %hush  +>.$
          %soft  (fore (tuba "crud: %{(trip err)} event failed") ~)
          %loud  (talk leaf+"crud: %{(trip err)} event failed" (flop tac))
        ==
      ::
      ++  talk
        |=  tac=(list tank)
        %-  fore
        %-  zing
        %+  turn  tac
        |=  a=tank
        (turn (~(win re a) [0 wid]) tuba)
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
        (pass wire [%g %deal [our our] ram deal])
      ::
      ++  pass                                          ::  pass note
        |=  [=wire =note]
        +>(moz :_(moz [hen %pass wire note]))
      ::
      ++  fore                                          ::  send dill output
        ::NOTE  there are still implicit assumptions
        ::      about the underlying console app's
        ::      semantics here. specifically, trailing
        ::      newlines are important to not getting
        ::      overwritten by the drum prompt, and a
        ::      bottom-of-screen cursor position gives
        ::      nicest results. a more agnostic solution
        ::      will need to replace this arm, someday.
        ::      perhaps +send this to .ram instead?
        ::
        |=  liz=(list (list @c))
        ~?  !=(%$ ses)  [%d %foreing-in-session ses]
        ^+  +>
        =.  +>
          =|  biz=(list blit)
          |-  ^+  +>.^$
          ?~  liz  (done %blit [%hop 0] [%wyp ~] biz)
          $(liz t.liz, biz (welp biz [%put i.liz] [%nel ~] ~))
        ::  since dill is acting on its own accord,
        ::  we %hey the term app so it may clean up.
        ::
        (send %hey ~)
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
          (rof `[our ~ ~] j/[[our sein/da/now] /(scot %p our)])
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
  ~|  wrapped-task
  =/  task=task  ((harden task) wrapped-task)
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
  ::  %knob sets a verbosity level for an error tag
  ::
  ?:  ?=(%knob -.task)
    =.  veb.all  (~(put by veb.all) tag.task level.task)
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
  ::
  =/  nus
    (ax hen ses)
  ?~  nus
    ::  session :ses does not exist
    ::  could be before %boot (or %boot failed)
    ::
    ~&  [%dill-call-no-session ses hen -.task]
    =/  tan  ?:(?=(%crud -.task) q.task ~)
    [((slog (flop tan)) ~) ..^$]
  ::
  =^  moz  all  abet:(call:u.nus task)
  [moz ..^$]
::
++  load                                                ::  import old state
  =<  |=  old=any-axle
      ?-  -.old
        %6  ..^$(all old)
        %5  $(old (axle-5-to-6 old))
        %4  $(old (axle-4-to-5 old))
      ==
  |%
  +$  any-axle  $%(axle axle-5 axle-4)
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
    ^-  axle
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
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem
  ::
  ?.  ?=(%& -.why)  ~
  =*  his  p.why
  ::TODO  don't special-case whey scry
  ::
  ?:  &(=(ren %$) =(tyl /whey))
    =/  maz=(list mass)
      :~  hey+&+hey.all
          dug+&+dug.all
      ==
    ``mass+!>(maz)
  ::  only respond for the local identity, %$ desk, current timestamp
  ::
  ?.  ?&  =(&+our why)
          =([%$ %da now] lot)
          =(%$ syd)
      ==
    ~
  ::  /dy/sessions        (set @tas)    all existing sessions
  ::  /du/sessions/[ses]  ?             does session ses exist?
  ::
  ?+  [ren tyl]  ~
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
      :*  [hey %pass / %g %deal [our our] %hood %leave ~]
          [hey %pass [%peer %$ ~] %g %deal [our our] %hood %leave ~]
          [hey %pass [%peer %$ ~] %g %deal [our our] %hood %watch [%dill %$ ~]]
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
