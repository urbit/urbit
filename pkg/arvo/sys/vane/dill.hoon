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
  $:  %5                                                ::
      hey=(unit duct)                                   ::  default duct
      dug=(map @tas axon)                               ::  conversations
      eye=(jug @tas duct)                               ::  outside listeners
      lit=?                                             ::  boot in lite mode
      $=  veb                                           ::  vane verbosities
      $~  (~(put by *(map @tas log-level)) %hole %soft) ::  quiet packet crashes
      (map @tas log-level)                              ::
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
  $%  [%dill-belt p=(hypo dill-belt)]                   ::
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
          $>  $?  %conf                                 ::
                  %deal                                 ::
                  %goad                                 ::
              ==                                        ::
          task:gall                                     ::
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
          $>  $?  %onto                                 ::
                  %unto                                 ::
              ==                                        ::
          gift:gall                                     ::
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
          %flow  +>
          %hail  (send %hey ~)
          %belt  (send `dill-belt`p.kyz)
          %text  (fore (tuba p.kyz) ~)
          %crud  ::  (send `dill-belt`[%cru p.kyz q.kyz])
                 (crud p.kyz q.kyz)
          %blew  (send %rez p.p.kyz q.p.kyz)
          %heft  (pass /whey %$ whey/~)
          %meld  (dump kyz)
          %pack  (dump kyz)
          %crop  (dump trim+p.kyz)
          %verb  (pass /verb %$ kyz)
        ==
      ::
      ++  crud
        |=  [err=@tas tac=(list tank)]
        ::  unknown errors default to %loud
        ::
        =/  lev=log-level  (~(gut by veb.all) err %loud)
        ::  apply log level for this error tag
        ::
        =/  =wall
          ?-  lev
            %hush  ~
            %soft  ~["crud: %{(trip err)} event failed"]
            %loud  :-  "crud: %{(trip err)} event failed"
                   %-  zing
                   %+  turn  (flop tac)
                   |=(a=tank (~(win re a) [0 wid]))
          ==
        ?:  =(~ wall)  +>.$
        (fore (turn wall tuba))
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
        ?:  ?=(%mor -.bit)
          |-  ^+  +>.^$
          ?~  p.bit  +>.^$
          $(p.bit t.p.bit, +>.^$ ^$(bit i.p.bit))
        ?:  ?=(%qit -.bit)
          (dump %logo ~)
        (done %blit [bit ~])
      ::  XX move
      ::
      ++  sein
        ^-  ship
        =/  dat=(unit (unit cage))
          (rof `[our ~ ~] j/[[our sein/da/now] /(scot %p our)])
        ;;(ship q.q:(need (need dat)))
      ::
      ++  init                                          ::  initialize
        (pass /merg/home [%c %merg %kids our %home da+now %init])
      ::
      ++  mere                                          ::  continue init
        ^+  .
        =/  myt  (flop (fall tem ~))
        =/  can  (clan:title our)
        =.  tem  ~
        =.  +>  (pass / %g %conf ram)
        =?  +>  ?=(?(%earl %duke %king) can)
          (ota sein %kids)
        ::  make kids desk publicly readable, so syncs work.
        ::
        =.  +>  (show %kids)
        =.  +>  hood-set-boot-apps
        =.  +>  peer
        |-  ^+  +>+
        ?~  myt  +>+
        $(myt t.myt, +>+ (send i.myt))
      ::
      ++  into                                          ::  preinitialize
        |=  gyl=(list gill)
        =.  tem  `(turn gyl |=(a=gill [%yow a]))
        (pass / [%c %warp our %home `[%sing %y [%ud 1] /]])
      ::
      ++  send                                          ::  send action
        |=  bet=dill-belt
        ^+  +>
        ?^  tem
          +>(tem `[bet u.tem])
        (deal /send/[ses] [%poke [%dill-belt -:!>(bet) bet]])
      ::
      ++  hood-set-boot-apps
        (deal / [%poke %drum-set-boot-apps !>(lit.all)])
      ::
      ++  peer
        (deal /peer/[ses] [%watch /drum])  ::TODO  rename subscription path
      ::
      ++  show                                          ::  permit reads on desk
        |=  des=desk
        (pass /show [%c %perm des / r+`[%black ~]])
      ::
      ++  ota
        |=  syn=[ship desk]
        (deal /sync %poke %kiln-ota !>(`syn))
      ::
      ++  take                                          ::  receive
        |=  [tea=wire sih=sign]
        ^+  +>
        ?-    sih
            [%gall %onto *]
          ?-  -.+>.sih
            %|  (crud %onto p.p.+>.sih)
            %&  (fore (tuba "{(t q)}ed %{(t p)}":[t=trip p.p.sih]) ~)
          ==
        ::
            [%gall %unto *]
          ::  ~&  [%take-gall-unto +>.sih]
          ?-  -.+>.sih
            %poke-ack   ?~(p.p.+>.sih +>.$ (crud %coup u.p.p.+>.sih))
            %kick       peer
            %watch-ack  ?~  p.p.+>.sih
                          +>.$
                        (dump:(crud %reap u.p.p.+>.sih) %logo ~)
            %fact       (from ;;(dill-blit q:`vase`+>+>.sih))
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
    ::REVIEW  can anything relevant happen between %boot and %init?
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
        %5  ..^$(all old)
        %4  $(old (axle-4-to-5 old))
      ==
  |%
  +$  any-axle  $%(axle axle-4)
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
    ^-  axle
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
  ::TODO  scry endpoints for /sessions
  ~
::
++  stay  all
::
++  take                                                ::  process move
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^+  [*(list move) ..^$]
  ?^  dud
    ~|(%dill-take-dud (mean tang.u.dud))
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
