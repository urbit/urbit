!:
::  dill (4d), terminal handling   
::
|=  pit/vase
=,  dill
=>  |%                                                  ::  interface tiles
++  gill  (pair ship term)                              ::  general contact
--                                                      ::
=>  |%                                                  ::  console protocol
++  all-axle  ?(old-axle axle)                          ::
++  old-axle                                            ::  all dill state
  $:  $2                                                ::
      ore/(unit ship)                                   ::  identity once set
      hey/(unit duct)                                   ::  default duct
      dug/(map duct axon)                               ::  conversations
  ==                                                    ::
++  axle                                                ::
  $:  $3                                                ::
      ore/(unit ship)                                   ::  identity once set
      hey/(unit duct)                                   ::  default duct
      dug/(map duct axon)                               ::  conversations
      $=  hef                                           ::  other weights
      $:  a/(unit mass)                                 ::
          b/(unit mass)                                 ::
          c/(unit mass)                                 ::
          e/(unit mass)                                 ::
          f/(unit mass)                                 ::
          g/(unit mass)                                 ::
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
  $%  {$make p/(unit @t) q/@ud r/@ s/?}                 ::
      {$sith p/@p q/@uw r/?}                            ::
  ==                                                    ::
++  note-clay                                           ::
  $%  {$merg p/@p q/@tas r/@p s/@tas t/case u/germ:clay}::  merge desks
      {$warp p/sock q/riff:clay}                       ::  wait for clay hack
  ==                                                    ::
++  note-dill                                           ::  note to self, odd
  $%  {$crud p/@tas q/(list tank)}                      ::
      {$heft $~}                                        ::
      {$init p/ship}                                    ::
      {$text p/tape}                                    ::
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$vega p/path}                                    ::  reboot by path
      {$velo p/@t q/@t}                                 ::  reboot by path
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
++  note-gall                                           ::
  $%  {$conf dock $load ship desk}                      ::
      {$deal p/sock q/cush:gall}                       ::
  ==                                                    ::
++  note                                                ::  out request $->
  $?  {?($a $b $c $e $f $g) $wegh $~}                   ::
  $%  {$a note-ames}                                    ::
      {$c note-clay}                                    ::
      {$d note-dill}                                    ::
      {$g note-gall}                                    ::
  ==  ==                                                ::
++  sign-ames                                           ::
  $%  {$nice $~}                                        ::
      {$init p/ship}                                    ::
  ==                                                    ::
++  sign-clay                                           ::
  $%  {$mere p/(each (set path) (pair term tang))}      ::
      {$note p/@tD q/tank}                              ::
      {$writ p/riot:clay}                              ::
  ==                                                    ::
++  sign-dill                                           ::
  $%  {$blit p/(list blit)}                             ::
  ==                                                    ::
++  sign-gall                                           ::
  $%  {$onto p/(each suss:gall tang)}                  ::
      {$unto p/cuft:gall}                              ::
  ==                                                    ::
++  sign                                                ::  in result $<-
  $?  {?($a $b $c $e $f $g) $mass p/mass}               ::
  $%  {$a sign-ames}                                    ::
      {$c sign-clay}                                    ::
      {$d sign-dill}                                    ::  
      {$g sign-gall}                                    ::
  ==  ==                                                ::
::::::::                                                ::  dill tiles
--
=|  all/axle
|=  {now/@da eny/@ ski/sley}                            ::  current invocation
=>  |%
    ++  as                                              ::  per cause
      |_  $:  {moz/(list move) hen/duct our/ship}
              axon
          ==
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
          $tick  =+  ^=  ges  ^-  gens:ames
                     :-  %en
                     =+  can=(clan:title p.kyz)
                     ?-  can
                       $czar  [%czar ~]
                       $duke  [%duke %anon ~]
                       $earl  [%earl (scot %p p.kyz)]
                       $king  [%king (scot %p p.kyz)]
                       $pawn  [%pawn ~]
                     ==
                 =+  yen=(scot %p (shax :(mix %ticket eny now)))
                 =+  ^=  beg  ^-  {his/@p tic/@p yen/@t ges/gens:ames}
                     [p.kyz q.kyz yen ges]
                 =+  cmd=[%hood %poke `cage`[%helm-begin !>(beg)]]
                 %=    +>.$
                     moz 
                   :_(moz [hen %pass ~ %g %deal [our our] cmd])
                 ==
          $veer  (dump kyz)
          $vega  (dump kyz)
          $velo  (dump kyz)
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
              `(list @c)`(zing (turn (~(tap in p.p.a)) ef))
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
          |=  a/$^((pair @ @) @)
          %-  (list @c)
          :+  27  '['                                   ::  "\033[{a}m"
          ?@(a :~(a 'm') :~(p.a q.a 'm'))
        ::
        ++  deco                                        ::  ANSI effects
          |=  a/^deco  ^-  @
          ?-  a
            $~   '0'
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
            $~  '9'
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
              moz
          ==
        ==
      ::
      ++  init                                          ::  initialize
        ~&  [%dill-init our ram]
        =+  myt=(flop (need tem))
        =+  can=(clan:title our)
        =.  tem  ~
        =.  moz  :_(moz [hen %pass / %c %merg our %home our %base da+now %init])
        =.  moz  :_(moz [hen %pass ~ %g %conf [[our ram] %load our %home]])
        =.  +>  ?:  ?=(?($czar $pawn) can)  +>
                (sync %base (sein:title our) %kids)
        =.  +>  ?:  ?=(?($czar $pawn) can)
                  (sync %home our %base)
                (init-sync %home our %base)
        =.  +>  ?.  ?=(?($duke $king $czar) can)  +>
                (sync %kids our %base)
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
          :*  hen
              %pass
              /
              %c
              [%warp [our our] %base `[%sing %y [%ud 1] /]]
          ==
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
      ++  init-sync
        |=  syn/{desk ship desk}
        %_    +>.$
            moz
          :_  moz
          :*  hen  %pass  /init-sync  %g  %deal  [our our]
              ram  %poke  %hood-init-sync  -:!>(syn)  syn
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
            {?($a $b $c $e $f $g) $mass *}
          (wegt -.sih p.sih)
        ::
            {$a $nice *}
          ::  ~&  [%take-nice-ames sih]
          +>
        ::
            {$a $init *}
          +>(moz :_(moz [hen %give +.sih]))
        ::
            {$c $mere *}
          ?:  ?=($& -.p.sih)
            +>.$
          (mean >%dill-mere-fail< >p.p.p.sih< q.p.p.sih)
        ::
            {$g $onto *}
          ::  ~&  [%take-gall-onto +>.sih]
          ?-  -.+>.sih
            $|  (crud %onto p.p.+>.sih)
            $&  (done %blit [%lin (tuba "{<p.p.sih>}")]~)
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
            $doff  !!
          ==
        ::
            {$c $note *}
          (from %out (tuba p.sih ' ' ~(ram re q.sih)))
        ::
            {$c $writ *}
          init
        ::
            {$d $blit *}
          (done +.sih)
        ==
      ::
      ++  wegh
        ^-  mass
        :-  %dill
        :-  %|
        :~  all+[%& [ore hey dug]:all]
        ==
      ::
      ++  wegt
        |=  {lal/?($a $b $c $e $f $g) mas/mass}
        ^+  +>
        =.  hef.all
          ?-  lal
            $a  ~?(?=(^ a.hef.all) %double-mass-a hef.all(a `mas))
            $b  ~?(?=(^ b.hef.all) %double-mass-b hef.all(b `mas))
            $c  ~?(?=(^ c.hef.all) %double-mass-c hef.all(c `mas))
            $e  ~?(?=(^ e.hef.all) %double-mass-e hef.all(e `mas))
            $f  ~?(?=(^ f.hef.all) %double-mass-f hef.all(f `mas))
            $g  ~?(?=(^ g.hef.all) %double-mass-g hef.all(g `mas))
          ==
        ?.  ?&  ?=(^ a.hef.all)
                ?=(^ b.hef.all)
                ?=(^ c.hef.all)
                ?=(^ e.hef.all)
                ?=(^ f.hef.all)
                ?=(^ g.hef.all)
            ==
          +>.$
        %+  done(hef.all [~ ~ ~ ~ ~ ~])
          %mass
        =>  [hef.all d=wegh]
        [%vanes %| ~[u.a u.c d u.e u.f u.g u.b]]
      --
    ::
    ++  ax                                              ::  make ++as
      |=  {hen/duct kyz/task:able}                           ::
      ?~  ore.all  ~
      =+  nux=(~(get by dug.all) hen)
      ?^  nux  
        (some ~(. as [~ hen u.ore.all] u.nux))
      ?.  ?=($flow -.kyz)  ~
      %-  some
      %.  q.kyz
      %~  into  as
          :-  [~ hen u.ore.all]
          :*  p.kyz
              [~ ~]
              80
              0
              (tuba "<awaiting {(trip p.kyz)}, this may take a few minutes>")
      ==  ==
    --
|%                                                      ::  poke+peek pattern
++  call                                                ::  handle request
  |=  $:  hen/duct
          hic/(hypo (hobo task:able))
      ==
  ^+  [p=*(list move) q=..^$]
  =>  %=    .                                           ::  XX temporary
          q.hic
        ^-  task:able
        ?:  ?=($soft -.q.hic)
          ::  ~&  [%dill-call-soft (@tas `*`-.p.q.hic)]
          ((hard task:able) p.q.hic)
        ?:  (~(nest ut -:!>(*task:able)) | p.hic)  q.hic
        ~&  [%dill-call-flub (@tas `*`-.q.hic)]
        ((hard task:able) q.hic)
      ==
  ?:  ?=($boot -.q.hic)
    :_(..^$ [hen %pass ~ (note %a p.q.hic)]~)
  ?:  ?=($flog -.q.hic)
    ::  ~&  [%dill-flog +.q.hic]
    ?:  ?=({$crud $hax-init {$leaf *} $~} p.q.hic)
      =+  him=(slav %p (crip p.i.q.p.q.hic))
      :_(..^$ ?~(hey.all ~ [u.hey.all %give %init him]~))
    ?:  ?=({$crud $hax-heft $~} p.q.hic)
      :_(..^$ ?~(hey.all ~ [u.hey.all %slip %d %heft ~]~))
    :_(..^$ ?~(hey.all ~ [u.hey.all %slip %d p.q.hic]~))
  =.  hey.all  ?^(hey.all hey.all `hen)
  ?:  ?=($init -.q.hic)
    ::  ~&  [%call-init hen]
    ?:  =(ore.all `p.q.hic)
      [[hen %give q.hic]~ ..^$]
    =:  ore.all  `p.q.hic
        dug.all   ~
      ==
    =^  moz  all  abet:(need (ax (need hey.all) [%flow %hood ~]))
    ?:  |((lth p.q.hic 256) (gte p.q.hic (bex 64)))  [moz ..^$] ::  XX HORRIBLE
    [:_(moz [(need hey.all) %give %init p.q.hic]) ..^$]
  =+  nus=(ax hen q.hic)
  ?~  nus
    ~&  [%dill-no-flow q.hic]
    [~ ..^$]
  =^  moz  all  abet:(call:u.nus q.hic)
  [moz ..^$]
::
++  doze
  |=  {now/@da hen/duct}
  ^-  (unit @da)
  ~
::
++  load                                                ::  trivial
  |=  old/all-axle
  ?:  ?=($2 -.old)
    $(old [%3 ore hey dug ~ ~ ~ ~ ~ ~]:old)
  ..^$(all old)
  ::  |=  old=*   ::  diable
  ::  ..^$(ore.all `~zod)
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=($& -.why)  ~
  =*  his  p.why
  [~ ~]
::
++  stay  all 
::
++  take                                                ::  process move
  |=  {tea/wire hen/duct hin/(hypo sign)}
  ^+  [p=*(list move) q=..^$]
  ?:  =(~ ore.all)
    ?:  ?=({$a $init *} q.hin)
      ::  ~&  [%take-init hen]
      =.  hey.all  ?^(hey.all hey.all `hen)
      [[[hen %give +.q.hin] ~] ..^$]
      ::  [~ ..^$]
    ~&  [%take-back q.hin]
    [~ ..^$]
  ?.  (~(has by dug.all) hen)
    ~&  [%take-weird-sign q.hin]
    ~&  [%take-weird-hen hen]
    [~ ..^$]
  =+  our=?>(?=(^ ore.all) u.ore.all)
  =^  moz  all  
    abet:(~(take as [~ hen our] (~(got by dug.all) hen)) q.hin)
  [moz ..^$]
--

