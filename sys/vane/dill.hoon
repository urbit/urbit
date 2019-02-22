!:
::  dill (4d), terminal handling
::
|=  pit/vase
=,  dill
=>  |%                                                  ::  interface tiles
++  gill  (pair ship term)                              ::  general contact
--                                                      ::
=>  |%                                                  ::  console protocol
++  all-axle  ?(axle)                                   ::
++  axle                                                ::
  $:  $0                                                ::
      hey/(unit duct)                                   ::  default duct
      dug/(map duct axon)                               ::  conversations
      $=  hef                                           ::  other weights
      $:  a/(unit mass)                                 ::
          b/(unit mass)                                 ::
          c/(unit mass)                                 ::
          e/(unit mass)                                 ::
          f/(unit mass)                                 ::
          g/(unit mass)                                 ::
          j/(unit mass)                                 ::
      ==                                                ::
  ==                                                    ::
++  axon                                                ::  dill per duct
  $:  ram/term                                          ::  console program
      tem/(unit (list dill-belt))                       ::  pending, reverse
      wid/_80                                           ::  terminal width
      pos/@ud                                           ::  cursor position
      see/(list @c)                                     ::  current line
  ==                                                    ::
--  =>                                                  ::
|%                                                      ::  protocol outward
++  mess                                                ::
  $%  {$dill-belt p/(hypo dill-belt)}                   ::
  ==                                                    ::
++  move  {p/duct q/(wind note gift:able)}              ::  local move
++  note-ames                                           ::  weird ames move
  $%  {$wegh $~}                                        ::
  ==                                                    ::
++  note-behn                                           ::
  $%  {$wegh $~}                                        ::
  ==                                                    ::
++  note-clay                                           ::
  $%  {$merg p/@tas q/@p r/@tas s/case t/germ:clay}     ::  merge desks
      {$warp p/ship q/riff:clay}                        ::  wait for clay hack
      {$wegh $~}                                        ::
      {$perm p/desk q/path r/rite:clay}                 ::  change permissions
  ==                                                    ::
++  note-dill                                           ::  note to self, odd
  $%  {$crud p/@tas q/(list tank)}                      ::
      {$heft $~}                                        ::
      {$init p/ship}                                    ::
      {$lyra p/@t q/@t}                                 ::  upgrade kernel
      {$text p/tape}                                    ::
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
++  note-eyre                                           ::
  $%  {$wegh $~}                                        ::
  ==                                                    ::
++  note-ford                                           ::
  $%  {$wegh $~}                                        ::
  ==                                                    ::
++  note-gall                                           ::
  $%  {$conf dock $load ship desk}                      ::
      {$deal p/sock q/cush:gall}                        ::
      {$wegh $~}                                        ::
  ==                                                    ::
++  note-jael                                           ::
  $%  $:  %dawn                                         ::  boot from keys
          =seed:able:jael                               ::    identity params
          spon=ship                                     ::    sponsor
          czar=(map ship [=life =pass])                 ::    galaxy table
          turf=(list turf)                              ::    domains
          bloq=@ud                                      ::    block number
          node=(unit purl:eyre)                         ::    gateway url
          snap=(unit snapshot:jael)                     ::    head start
      ==                                                ::
      [%fake our=ship]                                  ::  boot fake
      [%wegh ~]
  ==                                                    ::
++  note                                                ::  out request $->
  $%  {$a note-ames}                                    ::
      {$b note-behn}                                    ::
      {$c note-clay}                                    ::
      {$d note-dill}                                    ::
      {$e note-eyre}                                    ::
      {$f note-ford}                                    ::
      {$g note-gall}                                    ::
      {$j note-jael}                                    ::
  ==                                                    ::
++  sign-ames                                           ::
  $%  {$nice $~}                                        ::
      {$send p/lane:ames q/@}                           ::
      {$mass p/mass}                                    ::
  ==                                                    ::
++  sign-behn                                           ::
  $%  {$mass p/mass}                                    ::
  ==                                                    ::
++  sign-clay                                           ::
  $%  {$mere p/(each (set path) (pair term tang))}      ::
      {$note p/@tD q/tank}                              ::
      {$writ p/riot:clay}                               ::
      {$mass p/mass}                                    ::
      {$mack p/(unit tang)}                             ::
  ==                                                    ::
++  sign-dill                                           ::
  $%  {$blit p/(list blit)}                             ::
  ==                                                    ::
++  sign-eyre                                           ::
  $%  {$mass p/mass}                                    ::
  ==                                                    ::
++  sign-ford                                           ::
  $%  {$mass p/mass}                                    ::
  ==                                                    ::
++  sign-gall                                           ::
  $%  {$onto p/(each suss:gall tang)}                   ::
      {$unto p/cuft:gall}                               ::
      {$mass p/mass}                                    ::
  ==                                                    ::
++  sign-jael                                           ::
  $%  [%init p=ship]                                    ::
      [%mass p=mass]
  ==                                                    ::
++  sign                                                ::  in result $<-
  $%  {$a sign-ames}                                    ::
      {$b sign-behn}                                    ::
      {$c sign-clay}                                    ::
      {$d sign-dill}                                    ::
      {$e sign-eyre}                                    ::
      {$f sign-ford}                                    ::
      {$g sign-gall}                                    ::
      {%j sign-jael}                                    ::
  ==
::::::::                                                ::  dill tiles
--
=|  all/axle
|=  [our=ship now=@da eny=@uvJ ski=sley]                ::  current invocation
=>  |%
    ++  as                                              ::  per cause
      =|  moz/(list move)
      |_  [hen=duct axon]
      ++  abet                                          ::  resolve
        ^-  {(list move) axle}
        [(flop moz) all(dug (~(put by dug.all) hen +<+))]
      ::
      ++  call                                          ::  receive input
        |=  kyz/task:able
        ^+  +>
        ?+    -.kyz  ~&  [%strange-kiss -.kyz]  +>
          $flow  +>
          $harm  +>
          $hail  (send %hey ~)
          $belt  (send `dill-belt`p.kyz)
          $text  (from %out (tuba p.kyz))
          $crud  ::  (send `dill-belt`[%cru p.kyz q.kyz])
                 (crud p.kyz q.kyz)
          $blew  (send %rez p.p.kyz q.p.kyz)
          $heft  heft
          $lyra  (dump kyz)
          $veer  (dump kyz)
          $verb  (dump kyz)
        ==
      ::
      ++  crud
        |=  {err/@tas tac/(list tank)}
        =+  ^=  wol  ^-  wall
            :-  (trip err)
            (zing (turn (flop tac) |=(a/tank (~(win re a) [0 wid]))))
        |-  ^+  +>.^$
        ?~  wol  +>.^$
        $(wol t.wol, +>.^$ (from %out (tuba i.wol)))
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
        %_    .
            moz
          :*  [hen %pass /heft/ames %a %wegh ~]
              [hen %pass /heft/behn %b %wegh ~]
              [hen %pass /heft/clay %c %wegh ~]
              [hen %pass /heft/eyre %e %wegh ~]
              [hen %pass /heft/ford %f %wegh ~]
              [hen %pass /heft/gall %g %wegh ~]
              [hen %pass /heft/jael %j %wegh ~]
              moz
          ==
        ==
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
        ~&  [%dill-init our ram]
        ^+  .
        =.  moz  :_(moz [hen %pass / %c %merg %home our %base da+now %init])
        .
      ::
      ++  mere                                          ::  continue init
        ~&  [%dill-mere our ram]
        ^+  .
        =/  myt  (flop (need tem))
        =/  can  (clan:title our)
        =.  tem  ~
        =.  moz  :_(moz [hen %pass ~ %g %conf [[our ram] %load our %home]])
        =.  +>  (sync %home our %base)
        =.  +>  ?:  ?=(?($czar $pawn) can)  +>
                (sync %base (sein our) %kids)
        =.  +>  ?.  ?=(?($duke $king $czar) can)  +>
                ::  make kids desk publicly readable, so syncs work.
                ::
                (show %kids):(sync %kids our %base)
        =.  +>  autoload
        =.  +>  peer
        |-  ^+  +>+
        ?~  myt  +>+
        $(myt t.myt, +>+ (send i.myt))
      ::
      ++  into                                          ::  preinitialize
        |=  gyl/(list gill)
        %_    +>
            tem  `(turn gyl |=(a/gill [%yow a]))
            moz
          :_  moz
          [hen %pass / %c %warp our %base `[%sing %y [%ud 1] /]]
        ==
      ::
      ++  send                                          ::  send action
        |=  bet/dill-belt
        ?^  tem
          +>(tem `[bet u.tem])
        %_    +>
            moz
          :_  moz
          [hen %pass ~ %g %deal [our our] ram %poke [%dill-belt -:!>(bet) bet]]
        ==
      ++  peer
        %_    .
            moz
          :_(moz [hen %pass ~ %g %deal [our our] ram %peer /drum])
        ==
      ::
      ++  show                                          ::  permit reads on desk
        |=  des/desk
        %_    +>.$
            moz
          :_  moz
          [hen %pass /show %c %perm des / r+`[%black ~]]
        ==
      ::
      ++  sync
        |=  syn/{desk ship desk}
        %_    +>.$
            moz
          :_  moz
          :*  hen  %pass  /sync  %g  %deal  [our our]
              ram  %poke  %hood-sync  -:!>(syn)  syn
          ==
        ==
      ::
      ++  autoload
        %_    .
            moz
          :_  moz
          :*  hen  %pass  /autoload  %g  %deal  [our our]
              ram  %poke  %kiln-start-autoload  [%atom %n `~]  ~
          ==
        ==
      ::
      ++  pump                                          ::  send diff ack
        %_    .
            moz
          :_(moz [hen %pass ~ %g %deal [our our] ram %pump ~])
        ==
      ::
      ++  take                                          ::  receive
        |=  sih/sign
        ^+  +>
        ?-    sih
            {?($a $b $c $e $f $g $j) $mass *}
          (wegh -.sih p.sih)
        ::
            {$a $nice *}
          ::  ~&  [%take-nice-ames sih]
          +>
        ::
            [%j %init *]
          ::  pass thru to unix
          ::
          +>(moz :_(moz [hen %give +.sih]))
        ::
            {$a $send *}
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
            $coup  ?~(p.p.+>.sih +>.$ (crud %coup u.p.p.+>.sih))
            $quit  peer
            $reap  ?~  p.p.+>.sih
                     +>.$
                   (dump:(crud %reap u.p.p.+>.sih) %logo ~)
            $diff  pump:(from ((hard dill-blit) q:`vase`+>+>.sih))
          ==
        ::
            {$c $note *}
          (from %out (tuba p.sih ' ' ~(ram re q.sih)))
        ::
            {$c $writ *}
          init
        ::
            {$c $mere *}
          ?:  ?=(%& -.p.sih)
            mere
          (mean >%dill-mere-fail< >p.p.p.sih< q.p.p.sih)
        ::
            {$c $mack *}
          ?~  p.sih  +>.$
          (mean >%dill-clay-nack< u.p.sih)
        ::
            {$d $blit *}
          (done +.sih)
        ==
      ::  +wegh: receive a memory report from a vane and maybe emit full report
      ::
      ++  wegh
        |=  {lal/?($a $b $c $e $f $g $j) mas/mass}
        ^+  +>
        ::  update our listing of vane responses with this new one
        ::
        =.  hef.all
          ?-  lal
            $a  ~?(?=(^ a.hef.all) %double-mass-a hef.all(a `mas))
            $b  ~?(?=(^ b.hef.all) %double-mass-b hef.all(b `mas))
            $c  ~?(?=(^ c.hef.all) %double-mass-c hef.all(c `mas))
            $e  ~?(?=(^ e.hef.all) %double-mass-e hef.all(e `mas))
            $f  ~?(?=(^ f.hef.all) %double-mass-f hef.all(f `mas))
            $g  ~?(?=(^ g.hef.all) %double-mass-g hef.all(g `mas))
            $j  ~?(?=(^ j.hef.all) %double-mass-j hef.all(j `mas))
          ==
        ::  if not all vanes have responded yet, no-op
        ::
        ?.  ?&  ?=(^ a.hef.all)
                ?=(^ b.hef.all)
                ?=(^ c.hef.all)
                ?=(^ e.hef.all)
                ?=(^ f.hef.all)
                ?=(^ g.hef.all)
                ?=(^ j.hef.all)
            ==
          +>.$
        ::  clear vane reports from our state before weighing ourself
        ::
        ::    Otherwise, the state of vanes printed after this one get absorbed
        ::    into Dill's %dot catchall report.
        ::
        =/  ven=(list mass)  ~[u.a u.b u.c u.e u.g u.f u.j]:hef.all
        =>  .(hef.all [~ ~ ~ ~ ~ ~ ~])
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
          type=*
          wrapped-task=(hobo task:able)
      ==
  ^+  [*(list move) ..^$]
  =/  task=task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ((hard task:able) p.wrapped-task)
  ::  the boot event passes thru %dill for initial duct distribution
  ::
  ?:  ?=(%boot -.task)
    ?>  ?=(?(%dawn %fake) -.p.task)
    ?>  =(~ hey.all)
    =.  hey.all  `hen
    =/  boot  ((soft note-jael) p.task)
    ?~  boot
      ~|  invalid-boot-event+hen  !!
    :_(..^$ [hen %pass / %j u.boot]~)
  ::  we are subsequently initialized. single-home
  ::
  ?:  ?=(%init -.task)
    ?>  =(~ dug.all)
    ::  configure new terminal, setup :hood and %clay
    ::
    =*  duc  (need hey.all)
    =/  app  %hood
    =/  see  (tuba "<awaiting {(trip app)}, this may take a minute>")
    =/  zon=axon  [app input=[~ ~] width=80 cursor=0 see]
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
  ::  a %sunk notification from %jail comes in on an unfamiliar duct
  ::
  ?:  ?=(%sunk -.task)
    [~ ..^$]
  ::  a %vega notification on kernel upgrade comes in on an unfamiliar duct
  ::
  ?:  ?=(%vega -.task)
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
++  load                                                ::  trivial
  |=  old/all-axle
  ..^$(all old)
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
  |=  {tea/wire hen/duct hin/(hypo sign)}
  ^+  [*(list move) ..^$]
  =/  nus  (ax hen)
  ?~  nus
    ::  :hen is an unrecognized duct
    ::  could be before %boot (or %boot failed)
    ::
    ~&  [%dill-take-no-flow hen -.q.hin +<.q.hin]
    [~ ..^$]
  =^  moz  all  abet:(take:u.nus q.hin)
  [moz ..^$]
--
