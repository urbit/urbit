!:
::  dill (4d), terminal handling
::
|=  pit=vase
=,  dill
=>  |%                                                  ::  interface tiles
+$  gill  (pair ship term)                              ::  general contact
--                                                      ::
=>  |%                                                  ::  console protocol
+$  axle                                                ::
  $:  %4  ::TODO  replace ducts with session ids        ::
      hey=(unit duct)                                   ::  default duct
      dug=(map duct axon)                               ::  conversations
      eye=(jug duct duct)                               ::  outside listeners
      lit=?                                             ::  boot in lite mode
      $=  veb                                           ::  vane verbosities
      $~  (~(put by *(map @tas log-level)) %hole %soft) ::  quiet packet crashes
      (map @tas log-level)                              ::
  ==                                                    ::
+$  axon                                                ::  dill per duct
  $:  ram=term                                          ::  console program
      tem=(unit (list dill-belt))                       ::  pending, reverse
      wid=_80                                           ::  terminal width
      pos=@ud                                           ::  cursor position
      see=$%([%lin (list @c)] [%klr stub])              ::  current line
  ==                                                    ::
+$  log-level  ?(%hush %soft %loud)                     ::  none, line, full
--  =>                                                  ::
|%                                                      ::  protocol outward
+$  mess                                                ::
  $%  [%dill-belt p=(hypo dill-belt)]                   ::
  ==                                                    ::
+$  move  [p=duct q=(wind note gift:able)]              ::  local move
+$  note                                                ::  out request $->
  $~  [%d %verb ~]                                      ::
  $%  $:  %c                                            ::
          $>  $?  %merg                                 ::  merge desks
                  %perm                                 ::  change permissions
                  %warp                                 ::  wait for clay hack
              ==                                        ::
          task:able:clay                                ::
      ==                                                ::
      $:  %d                                            ::
          $>  $?  %crud                                 ::
                  %heft                                 ::
                  %init                                 ::  XX obsolete?
                  %lyra                                 ::
                  %text                                 ::
                  %veer                                 ::
                  %verb                                 ::
              ==                                        ::
          task:able:dill                                ::
      ==                                                ::
      $:  %g                                            ::
          $>  $?  %conf                                 ::
                  %deal                                 ::
                  %goad                                 ::
              ==                                        ::
          task:able:gall                                ::
      ==                                                ::
      $:  %j                                            ::
          $>  $?  %dawn                                 ::
                  %fake                                 ::
              ==                                        ::
          task:able:jael                                ::
  ==  ==                                                ::
+$  sign                                                ::  in result $<-
  $~  [%j %init *@p]                                    ::
  $%  $:  %b                                            ::
          $%  $>(%writ gift:able:clay)                  ::  XX %slip
              $>(%mere gift:able:clay)                  ::  XX %slip
      ==  ==                                            ::
      $:  %c                                            ::
          $>  $?  %mere                                 ::
                  %note                                 ::
                  %writ                                 ::
              ==                                        ::
          gift:able:clay                                ::
      ==                                                ::
      $:  %d                                            ::
          $>(%blit gift:able:dill)                      ::
      ==                                                ::
      $:  %g                                            ::
          $>  $?  %onto                                 ::
                  %unto                                 ::
              ==                                        ::
          gift:able:gall                                ::
      ==                                                ::
      $:  %j                                            ::
          $>  $?  %init                                 ::
              ==                                        ::
          gift:able:jael                                ::
  ==  ==                                                ::
::::::::                                                ::  dill tiles
--
=|  all=axle
|=  [our=ship now=@da eny=@uvJ rof=roof]                ::  current invocation
=*  ski  (en-sley rof)
=>  ~%  %dill  ..is  ~
    |%
    ++  as                                              ::  per cause
      =|  moz=(list move)
      |_  [hen=duct axon]
      ++  abet                                          ::  resolve
        ^-  [(list move) axle]
        [(flop moz) all(dug (~(put by dug.all) hen +<+))]
      ::
      ++  call                                          ::  receive input
        |=  kyz=task:able
        ^+  +>
        ?+    -.kyz  ~&  [%strange-kiss -.kyz]  +>
          %flow  +>
          %harm  +>
          %hail  (send %hey ~)
          %belt  (send `dill-belt`p.kyz)
          %text  (from %out (tuba p.kyz))
          %crud  ::  (send `dill-belt`[%cru p.kyz q.kyz])
                 (crud p.kyz q.kyz)
          %blew  (send %rez p.p.kyz q.p.kyz)
          %heft  (dump %whey ~)
          %lyra  (dump kyz)
          %meld  (dump kyz)
          %pack  (dump kyz)
          %crop  (dump trim+p.kyz)
          %veer  (dump kyz)
          %verb  (dump kyz)
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
        |-  ^+  +>.^$
        ?~  wall  +>.^$
        $(wall t.wall, +>.^$ (from %out (tuba i.wall)))
      ::
      ++  dump                                          ::  pass down to hey
        |=  git=gift:able
        ?>  ?=(^ hey.all)
        +>(moz [[u.hey.all %give git] moz])
      ::
      ++  done                                          ::  return gift
        |=  git=gift:able
        =-  +>.$(moz (weld - moz))
        %+  turn
          :-  hen
          ~(tap in (~(get ju eye.all) hen))
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
      ++  from                                          ::  receive blit
        |=  bit=dill-blit
        ^+  +>
        ?:  ?=(%mor -.bit)
          |-  ^+  +>.^$
          ?~  p.bit  +>.^$
          $(p.bit t.p.bit, +>.^$ ^$(bit i.p.bit))
        ?:  ?=(%out -.bit)
          %+  done  %blit
          :~  [%lin p.bit]
              [%mor ~]
              see
              [%hop pos]
          ==
        ?:  ?=(%klr -.bit)
          %+  done  %blit
          :~  [%klr p.bit]
              [%mor ~]
              see
              [%hop pos]
          ==
        ?:  ?=(%pro -.bit)
          =.  see  [%lin p.bit]
          (done %blit [see [%hop pos] ~])
        ?:  ?=(%pom -.bit)
          ::NOTE  treat "styled prompt" without style as plain prompt,
          ::      to allow rendering by older runtimes
          ::TODO  remove me once v0.10.9+ has high/guaranteed adoption
          ::
          ?:  (levy p.bit (cork head |*(s=stye =(*stye s))))
            $(bit [%pro (zing (turn p.bit tail))])
          =.  see  [%klr p.bit]
          (done %blit [see [%hop pos] ~])
        ?:  ?=(%hop -.bit)
          (done(pos p.bit) %blit [bit ~])
        ?:  ?=(%qit -.bit)
          (dump %logo ~)
        (done %blit [bit ~])
      ::  XX move
      ::
      ++  sein
        |=  who=ship
        ;;  ship
        %-  need  %-  need
        %-  (sloy-light ski)
        [[151 %noun] %j our %sein da+now /(scot %p who)]
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
          (ota (sein our) %kids)
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
        (deal / [%poke [%dill-belt -:!>(bet) bet]])
      ::
      ++  hood-set-boot-apps
        (deal / [%poke %drum-set-boot-apps !>(lit.all)])
      ::
      ++  peer
        (deal / [%watch /drum])
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
            [%j %init *]
          ::  pass thru to unix
          ::
          +>(moz :_(moz [hen %give +.sih]))
        ::
            [%g %onto *]
          ::  ~&  [%take-gall-onto +>.sih]
          ?-  -.+>.sih
            %|  (crud %onto p.p.+>.sih)
            %&  (done %blit [%lin (tuba "{<p.p.sih>}")]~)
          ==
        ::
            [%g %unto *]
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
            [%c %note *]
          (from %out (tuba p.sih ' ' ~(ram re q.sih)))
        ::
            [?(%b %c) %writ *]
          init
        ::
            [?(%b %c) %mere *]
          ?:  ?=(%& -.p.sih)
            mere
          (mean >%dill-mere-fail< >p.p.p.sih< q.p.p.sih)
        ::
            [%d %blit *]
          (done +.sih)
        ==
      --
    ::
    ++  ax                                              ::  make ++as
      |=  hen=duct
      ^-  (unit _as)
      =/  nux  (~(get by dug.all) hen)
      ?~  nux  ~
      (some ~(. as hen u.nux))
    --
|%                                                      ::  poke+peek pattern
++  call                                                ::  handle request
  |=  $:  hen=duct
          dud=(unit goof)
          type=*
          wrapped-task=(hobo task:able)
      ==
  ^+  [*(list move) ..^$]
  ~|  wrapped-task
  =/  task=task:able  ((harden task:able) wrapped-task)
  ::
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
      ((soft $>($?(%dawn %fake) task:able:jael)) p.task)
    ?~  boot
      ~&  %dill-no-boot
      ~&  p.task
      ~|  invalid-boot-event+hen  !!
    =.  lit.all  lit.task
    [[hen %pass / %j u.boot]~ ..^$]
  ::  we are subsequently initialized. single-home
  ::
  ?:  ?=(%init -.task)
    ?>  =(~ dug.all)
    ::  configure new terminal, setup :hood and %clay
    ::
    =*  duc  (need hey.all)
    =/  app  %hood
    =/  see  (tuba "<awaiting {(trip app)}, this may take a minute>")
    =/  zon=axon  [app input=[~ ~] width=80 cursor=(lent see) lin+see]
    ::
    =^  moz  all  abet:(~(into as duc zon) ~)
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
  ::
  ?:  ?=(%view -.task)
    ::  crash on viewing non-existent session
    ::
    ~|  [%no-session session.task]
    ?>  =(~ session.task)
    =/  session  (need hey.all)
    =/  =axon    (~(got by dug.all) session)
    ::  register the viewer and send them the prompt line
    ::
    :-  [hen %give %blit [see.axon]~]~
    ..^$(eye.all (~(put ju eye.all) session hen))
  ::
  ?:  ?=(%flee -.task)
    :-  ~
    ~|  [%no-session session.task]
    ?>  =(~ session.task)
    =/  session  (need hey.all)
    ..^$(eye.all (~(del ju eye.all) session hen))
  ::
  =/  nus  (ax hen)
  =?  nus  &(?=(~ nus) ?=(^ hey.all))
    ::TODO  allow specifying target session in task
    (ax u.hey.all)
  ?~  nus
    ::  :hen is an unrecognized duct
    ::  could be before %boot (or %boot failed)
    ::
    ~&  [%dill-call-no-flow hen -.task]
    =/  tan  ?:(?=(%crud -.task) q.task ~)
    [((slog (flop tan)) ~) ..^$]
  ::
  =^  moz  all  abet:(call:u.nus task)
  [moz ..^$]
::
++  load                                                ::  import old state
  |=  old=axle
  ..^$(all old)
::
++  scry
  |=  [lyc=gang cyr=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  cyr
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
  ::  /dx/sessions//line    blit    current line (prompt) of default session
  ::  /dx/sessions//cursor  @ud     current cursor position of default session
  ::TODO  support asking for specific sessions once session ids are real
  ::
  ?.  ?=(%x ren)  ~
  ?+  tyl  ~
      [%sessions %$ *]
    ?~  hey.all                                [~ ~]
    ?~  session=(~(get by dug.all) u.hey.all)  [~ ~]
    ?+  t.t.tyl  ~
      [%line ~]    ``blit+!>(`blit`see.u.session)
      [%cursor ~]  ``atom+!>(pos.u.session)
    ==
  ==
::
++  stay  all
::
++  take                                                ::  process move
  |=  [tea=wire hen=duct dud=(unit goof) hin=(hypo sign)]
  ^+  [*(list move) ..^$]
  ?^  dud
    ~|(%dill-take-dud (mean tang.u.dud))
  ::
  =/  nus  (ax hen)
  ?~  nus
    ::  :hen is an unrecognized duct
    ::  could be before %boot (or %boot failed)
    ::
    ~&  [%dill-take-no-flow hen -.q.hin +<.q.hin]
    [~ ..^$]
  =^  moz  all  abet:(take:u.nus tea q.hin)
  [moz ..^$]
--
