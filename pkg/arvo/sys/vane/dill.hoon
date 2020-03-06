!:
::  dill (4d), terminal handling
::
|=  pit/vase
=,  dill
=>  |%                                                  ::  interface tiles
++  gill  (pair ship term)                              ::  general contact
--                                                      ::
=>  |%                                                  ::  console protocol
++  axle                                                ::
  $:  $2                                                ::
      hey/(unit duct)                                   ::  default duct
      dug/(map duct axon)                               ::  conversations
      lit/?                                             ::  boot in lite mode
      dog/_|                                            ::  auto-goad
      $=  hef                                           ::  other weights
      $:  a/(unit mass)                                 ::
          b/(unit mass)                                 ::
          c/(unit mass)                                 ::
          e/(unit mass)                                 ::
          f/(unit mass)                                 ::
          g/(unit mass)                                 ::
          i/(unit mass)                                 ::
          j/(unit mass)                                 ::
      ==                                                ::
      $=  veb                                           ::  vane verbosities
      $~  (~(put by *(map @tas log-level)) %hole %soft) ::  quiet packet crashes
      (map @tas log-level)                              ::
  ==                                                    ::
++  axon                                                ::  dill per duct
  $:  ram/term                                          ::  console program
      tem/(unit (list dill-belt))                       ::  pending, reverse
      wid/_80                                           ::  terminal width
      pos/@ud                                           ::  cursor position
      see/(list @c)                                     ::  current line
  ==                                                    ::
+$  log-level  ?(%hush %soft %loud)                     ::  none, line, full
--  =>                                                  ::
|%                                                      ::  protocol outward
++  mess                                                ::
  $%  {$dill-belt p/(hypo dill-belt)}                   ::
  ==                                                    ::
++  move  {p/duct q/(wind note gift:able)}              ::  local move
++  note                                                ::  out request $->
  $~  [%d %verb ~]                                      ::
  $%  $:  %a                                            ::
          $>(%wegh task:able:ames)                      ::
      ==                                                ::
      $:  %b                                            ::
          $>  $?  %wait                                 ::
                  %wegh                                 ::
              ==                                        ::
          task:able:behn                                ::
      ==                                                ::
      $:  %c                                            ::
          $>  $?  %merg                                 ::  merge desks
                  %perm                                 ::  change permissions
                  %warp                                 ::  wait for clay hack
                  %wegh                                 ::  memory measure
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
      $:  %e                                            ::
          $>(%wegh task:able:eyre)                      ::
      ==                                                ::
      $:  %f                                            ::
          $>(%wegh task:able:ford)                      ::
      ==                                                ::
      $:  %g                                            ::
          $>  $?  %conf                                 ::
                  %deal                                 ::
                  %goad                                 ::
                  %wegh                                 ::
              ==                                        ::
          task:able:gall                                ::
      ==                                                ::
      $:  %i                                            ::
          $>(%wegh task:able:iris)                      ::
      ==                                                ::
      $:  %j                                            ::
          $>  $?  %dawn                                 ::
                  %fake                                 ::
                  %wegh                                 ::
              ==                                        ::
          task:able:jael                                ::
  ==  ==                                                ::
++  sign                                                ::  in result $<-
  $~  [%j %init *@p]                                    ::
  $%  $:  %a                                            ::
          $%  $>(%mass gift:able:ames)                  ::
      ==  ==                                            ::
      $:  %b                                            ::
          $%  $>  $?  %mass                             ::
                      %wake                             ::
                  ==                                    ::
              gift:able:behn                            ::
              $>(%writ gift:able:clay)                  ::  XX %slip
              $>(%mere gift:able:clay)                  ::  XX %slip
      ==  ==                                            ::
      $:  %c                                            ::
          $>  $?  %mass                                 ::
                  %mere                                 ::
                  %note                                 ::
                  %writ                                 ::
              ==                                        ::
          gift:able:clay                                ::
      ==                                                ::
      $:  %d                                            ::
          $>(%blit gift:able:dill)                      ::
      ==                                                ::
      $:  %e                                            ::
          $>(%mass gift:able:eyre)                      ::
      ==                                                ::
      $:  %f                                            ::
          $>(%mass gift:able:ford)                      ::
      ==                                                ::
      $:  %g                                            ::
          $>  $?  %mass                                 ::
                  %onto                                 ::
                  %unto                                 ::
              ==                                        ::
          gift:able:gall                                ::
      ==                                                ::
      $:  %i                                            ::
          $>(%mass gift:able:iris)                      ::
      ==                                                ::
      $:  %j                                            ::
          $>  $?  %init                                 ::
                  %mass                                 ::
              ==                                        ::
          gift:able:jael                                ::
  ==  ==                                                ::
::::::::                                                ::  dill tiles
--
=|  all/axle
|=  [our=ship now=@da eny=@uvJ ski=sley]                ::  current invocation
=>  ~%  %dill  ..is  ~
    |%
    ++  as                                              ::  per cause
      =|  moz/(list move)
      |_  [hen=duct axon]
      ++  abet                                          ::  resolve
        ^-  {(list move) axle}
        [(flop moz) all(dug (~(put by dug.all) hen +<+))]
      ::
      ++  auto                                          ::  stage automation
        ^+  .
        ?.  dog.all  .
        =.  dog.all  |
        (pass /auto/one [%b %wait +(now)])
      ::
      ++  auto-wake                                     ::  resume automation
        |=  [=wire error=(unit tang)]
        ?+  wire
          ?~  error
            ~|(behn-bad-wake+wire !!)
          (crud %wake u.error)
        ::
            [%auto %one ~]
          ?~  error
            ~&  %behn-goad
            (pass / [%g %goad force=| ~])
          ::  %goad crashed, wait again, then force
          ::
          ~&  %behn-goad-retry
          %.  [/auto/two [%b %wait +(now)]]
          pass:(crud %goad u.error)
        ::
            [%auto %two ~]
          ?~  error
            ~&  %behn-goad-again
            (pass / [%g %goad force=& ~])
          ::  %goad crashed again, bail out
          ::
          ~&  %behn-goad-fail
          (crud %goad u.error)
        ==
      ::
      ++  call                                          ::  receive input
        |=  kyz/task:able
        ^+  +>
        ?+    -.kyz  ~&  [%strange-kiss -.kyz]  +>
          $flow  +>
          $harm  +>
          $hail  auto:(send %hey ~)
          $belt  (send `dill-belt`p.kyz)
          $text  (from %out (tuba p.kyz))
          $crud  ::  (send `dill-belt`[%cru p.kyz q.kyz])
                 (crud p.kyz q.kyz)
          $blew  (send %rez p.p.kyz q.p.kyz)
          $heft  heft
          $lyra  (dump kyz)
          $pack  (dump kyz)
          $veer  (dump kyz)
          $verb  (dump kyz)
        ==
      ::
      ++  crud
        |=  {err/@tas tac/(list tank)}
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
        |=  git/gift:able
        ?>  ?=(^ hey.all)
        +>(moz [[u.hey.all %give git] moz])
      ::
      ++  done                                          ::  return gift
        |=  git/gift:able
        +>(moz :_(moz [hen %give git]))
      ::
      ++  deal                                          ::  pass to %gall
        |=  [=wire =deal:gall]
        (pass wire [%g %deal [our our] ram deal])
      ::
      ++  pass                                          ::  pass note
        |=  [=wire =note]
        +>(moz :_(moz [hen %pass wire note]))
      ::
      ++  from                                          ::  receive belt
        |=  bit/dill-blit
        ^+  +>
        ?:  ?=($mor -.bit)
          |-  ^+  +>.^$
          ?~  p.bit  +>.^$
          $(p.bit t.p.bit, +>.^$ ^$(bit i.p.bit))
        ?:  ?=($out -.bit)
          %+  done  %blit
          :~  [%lin p.bit]
              [%mor ~]
              [%lin see]
              [%hop pos]
          ==
        ?:  ?=($klr -.bit)
          %+  done  %blit
          :~  [%lin (cvrt:ansi p.bit)]
              [%mor ~]
              [%lin see]
              [%hop pos]
          ==
        ?:  ?=($pro -.bit)
          (done(see p.bit) %blit [[%lin p.bit] [%hop pos] ~])
        ?:  ?=($pom -.bit)
          =.  see  (cvrt:ansi p.bit)
          (done %blit [[%lin see] [%hop pos] ~])
        ?:  ?=($hop -.bit)
          (done(pos p.bit) %blit [bit ~])
        ?:  ?=($qit -.bit)
          (dump %logo ~)
        (done %blit [bit ~])
      ::
      ++  ansi
        |%
        ++  cvrt                                        ::  stub to (list @c)
          |=  a/stub                                    ::  with ANSI codes
          ^-  (list @c)
          %-  zing  %+  turn  a
          |=  a/(pair stye (list @c))
          ^-  (list @c)
          ;:  weld
              ?:  =(0 ~(wyt in p.p.a))  ~
              `(list @c)`(zing (turn ~(tap in p.p.a) ef))
              (bg p.q.p.a)
              (fg q.q.p.a)
              q.a
              ?~(p.p.a ~ (ef ~))
              (bg ~)
              (fg ~)
          ==
        ::
        ++  ef  |=(a/^deco (scap (deco a)))             ::  ANSI effect
        ::
        ++  fg  |=(a/^tint (scap (tint a)))             ::  ANSI foreground
        ::
        ++  bg                                          ::  ANSI background
          |=  a/^tint
          %-  scap
          =>((tint a) [+(p) q])                         ::  (add 10 fg)
        ::
        ++  scap                                        ::  ANSI escape seq
          |=  a/$@(@ (pair @ @))
          %-  (list @c)
          :+  27  '['                                   ::  "\033[{a}m"
          ?@(a :~(a 'm') :~(p.a q.a 'm'))
        ::
        ++  deco                                        ::  ANSI effects
          |=  a/^deco  ^-  @
          ?-  a
            ~   '0'
            $br  '1'
            $un  '4'
            $bl  '5'
          ==
        ::
        ++  tint                                        ::  ANSI colors (fg)
          |=  a/^tint
          ^-  (pair @ @)
          :-  '3'
          ?-  a
            $k  '0'
            $r  '1'
            $g  '2'
            $y  '3'
            $b  '4'
            $m  '5'
            $c  '6'
            $w  '7'
            ~  '9'
           ==
        --
      ::
      ++  heft
        =<  (pass /heft/ames [%a %wegh ~])
        =<  (pass /heft/behn [%b %wegh ~])
        =<  (pass /heft/clay [%c %wegh ~])
        =<  (pass /heft/eyre [%e %wegh ~])
        =<  (pass /heft/ford [%f %wegh ~])
        =<  (pass /heft/gall [%g %wegh ~])
        =<  (pass /heft/iris [%i %wegh ~])
        =<  (pass /heft/jael [%j %wegh ~])
        .
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
        (pass /merg/home [%c %merg %home our %base da+now %init])
      ::
      ++  mere                                          ::  continue init
        ^+  .
        =/  myt  (flop (fall tem ~))
        =/  can  (clan:title our)
        =.  tem  ~
        =.  +>  (pass / [%g %conf [[our ram] our %home]])
        =.  +>  (sync %home our %base)
        =?  +>  ?=(?($earl $duke $king) can)
          (sync %base (sein our) %kids)
        =?  +>  ?=(?($duke $king $czar) can)
          ::  make kids desk publicly readable, so syncs work.
          ::
          (show %kids):(sync %kids our %base)
        =.  +>  autoload
        =.  +>  hood-set-boot-apps
        =.  +>  peer
        |-  ^+  +>+
        ?~  myt  +>+
        $(myt t.myt, +>+ (send i.myt))
      ::
      ++  into                                          ::  preinitialize
        |=  gyl/(list gill)
        =.  tem  `(turn gyl |=(a/gill [%yow a]))
        (pass / [%c %warp our %base `[%sing %y [%ud 1] /]])
      ::
      ++  send                                          ::  send action
        |=  bet/dill-belt
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
        |=  des/desk
        (pass /show [%c %perm des / r+`[%black ~]])
      ::
      ++  sync
        |=  syn/{desk ship desk}
        (deal /sync [%poke %hood-sync -:!>(syn) syn])
      ::
      ++  autoload
        (deal /autoload [%poke %kiln-start-autoload [%atom %n `~] ~])
      ::
      ++  take                                          ::  receive
        |=  {tea/wire sih/sign}
        ^+  +>
        ?-    sih
            {?($a $b $c $e $f $g $i $j) $mass *}
          (wegh -.sih p.sih)
        ::
            [%j %init *]
          ::  pass thru to unix
          ::
          +>(moz :_(moz [hen %give +.sih]))
        ::
            {$g $onto *}
          ::  ~&  [%take-gall-onto +>.sih]
          ?-  -.+>.sih
            %|  (crud %onto p.p.+>.sih)
            %&  (done %blit [%lin (tuba "{<p.p.sih>}")]~)
          ==
        ::
            {$g $unto *}
          ::  ~&  [%take-gall-unto +>.sih]
          ?-  -.+>.sih
            $poke-ack   ?~(p.p.+>.sih +>.$ (crud %coup u.p.p.+>.sih))
            $kick       peer
            $watch-ack  ?~  p.p.+>.sih
                          +>.$
                        (dump:(crud %reap u.p.p.+>.sih) %logo ~)
            $fact       (from ;;(dill-blit q:`vase`+>+>.sih))
          ==
        ::
            {$c $note *}
          (from %out (tuba p.sih ' ' ~(ram re q.sih)))
        ::
            {?($b $c) $writ *}
          init
        ::
            {?($b %c) $mere *}
          ?:  ?=(%& -.p.sih)
            mere
          (mean >%dill-mere-fail< >p.p.p.sih< q.p.p.sih)
        ::
            {$d $blit *}
          (done +.sih)
        ::
            {$b $wake *}
          (auto-wake tea error.sih)
        ==
      ::  +wegh: receive a memory report from a vane and maybe emit full report
      ::
      ++  wegh
        |=  [lal=?(%a %b %c %e %f %g %i %j) mas=mass]
        ^+  +>
        ::  update our listing of vane responses with this new one
        ::
        =.  hef.all
          ?-  lal
            %a  ~?(?=(^ a.hef.all) %double-mass-a hef.all(a `mas))
            %b  ~?(?=(^ b.hef.all) %double-mass-b hef.all(b `mas))
            %c  ~?(?=(^ c.hef.all) %double-mass-c hef.all(c `mas))
            %e  ~?(?=(^ e.hef.all) %double-mass-e hef.all(e `mas))
            %f  ~?(?=(^ f.hef.all) %double-mass-f hef.all(f `mas))
            %g  ~?(?=(^ g.hef.all) %double-mass-g hef.all(g `mas))
            %i  ~?(?=(^ i.hef.all) %double-mass-i hef.all(i `mas))
            %j  ~?(?=(^ j.hef.all) %double-mass-j hef.all(j `mas))
          ==
        ::  if not all vanes have responded yet, no-op
        ::
        ?.  ?&  ?=(^ a.hef.all)
                ?=(^ b.hef.all)
                ?=(^ c.hef.all)
                ?=(^ e.hef.all)
                ?=(^ f.hef.all)
                ?=(^ g.hef.all)
                ?=(^ i.hef.all)
                ?=(^ j.hef.all)
            ==
          +>.$
        ::  clear vane reports from our state before weighing ourself
        ::
        ::    Otherwise, the state of vanes printed after this one get absorbed
        ::    into Dill's %dot catchall report.
        ::
        =/  ven=(list mass)  ~[u.a u.b u.c u.e u.f u.g u.i u.j]:hef.all
        =>  .(hef.all [~ ~ ~ ~ ~ ~ ~ ~])
        ::  wegh ourself now that our state doesn't include other masses
        ::
        =/  self=mass
          :+  %dill  %|
          :~  hey+&+hey.all
              dug+&+dug.all
              dot+&+all
          ==
        ::  produce the memory report for all vanes
        ::
        (done %mass %vanes %| [self ven])
      --
    ::
    ++  ax                                              ::  make ++as
      |=  hen/duct
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
    =/  zon=axon  [app input=[~ ~] width=80 cursor=(lent see) see]
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
  =/  nus  (ax hen)
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
  =>  |%
      ::  without .dog
      ::
      ++  axle-one
        $:  $1
            hey/(unit duct)
            dug/(map duct axon)
            lit/?
            $=  hef
            $:  a/(unit mass)
                b/(unit mass)
                c/(unit mass)
                e/(unit mass)
                f/(unit mass)
                g/(unit mass)
                i/(unit mass)
                j/(unit mass)
            ==
            $=  veb
            $~  (~(put by *(map @tas log-level)) %hole %soft)
            (map @tas log-level)
        ==
      ::
      ++  axle-both
        $%(axle-one axle)
      --
  ::
  |=  old=axle-both
  ?-  -.old
    %1  $(old [%2 [hey dug lit dog=& hef veb]:old])
    %2  ..^$(all old)
  ==
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=(%& -.why)  ~
  =*  his  p.why
  [~ ~]
::
++  stay  all
::
++  take                                                ::  process move
  |=  {tea/wire hen/duct dud/(unit goof) hin/(hypo sign)}
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
