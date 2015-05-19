!:
::  dill (4d), terminal handling   
::
|=  pit=vase
=>  |%                                                  ::  interface tiles
++  gill  (pair ship term)                              ::  general contact
--                                                      ::
=>  |%                                                  ::  console protocol
++  all-axle  ?(old-axle axle)                          ::
++  old-axle                                            ::  all dill state
  $:  %2                                                ::
      ore=(unit ship)                                   ::  identity once set
      hey=(unit duct)                                   ::  default duct
      dug=(map duct axon)                               ::  conversations
  ==                                                    ::
++  axle                                                ::
  $:  %3                                                ::
      ore=(unit ship)                                   ::  identity once set
      hey=(unit duct)                                   ::  default duct
      dug=(map duct axon)                               ::  conversations
      $=  hef                                           ::  other weights
      $:  a=(unit mass)                                 ::
          c=(unit mass)                                 ::
          e=(unit mass)                                 ::
          f=(unit mass)                                 ::
          g=(unit mass)                                 ::
          t=(unit mass)                                 ::
      ==                                                ::
  ==                                                    ::  
++  axon                                                ::  dill per duct
  $:  ram=term                                          ::  console program
      tem=(unit (list dill-belt))                       ::  pending, reverse
      wid=_80                                           ::  terminal width
      pos=@ud                                           ::  cursor position
      see=(list ,@c)                                    ::  current line
  ==                                                    ::
--  =>                                                  ::
|%                                                      ::  protocol below
++  gift  gift-dill                                     ::  out result <-$
++  kiss  kiss-dill                                     ::  in request ->$
--  =>                                                  ::
|%                                                      ::  protocol outward
++  mess                                                ::
  $%  [%dill-belt p=(hypo dill-belt)]                   ::
  ==                                                    ::
++  cuft                                                ::  internal gift
  $%  [%coup p=(unit tang)]                             ::  poke result
      [%quit ~]                                         ::  close subscription
      [%reap p=(unit tang)]                             ::  peer result
      [%diff p=cage]                                    ::  subscription output
  ==                                                    ::
++  suss  (trel term ,@tas ,@da)                        ::  config report
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  note-ames                                           ::  weird ames move
  $%  [%make p=(unit ,@t) q=@ud r=@ s=?]                ::
      [%sith p=@p q=@uw r=?]                            ::
  ==                                                    ::
++  note-clay                                           ::
  $%  [%font p=@p q=@tas r=@p s=@tas]                   ::
      [%warp p=sock q=riff]                             ::  wait for clay, hack
  ==                                                    ::
++  note-dill                                           ::  note to self, odd
  $%  [%crud p=@tas q=(list tank)]                      ::
      [%heft ~]                                         ::
      [%init p=ship]                                    ::
      [%text p=tape]                                    ::
      [%veer p=@ta q=path r=@t]                         ::  install vane
      [%vega p=path]                                    ::  reboot by path
      [%verb ~]                                         ::  verbose mode
  ==                                                    ::
++  note-gall                                           ::
  $%  [%conf dock %load ship desk]                      ::
      [%deal p=sock q=cush]                             ::
  ==                                                    ::
++  note                                                ::  out request $->
  $?  [?(%a %c %e %f %g %t) %wegh ~]                    ::
  $%  [%a note-ames]                                    ::
      [%c note-clay]                                    ::
      [%d note-dill]                                    ::
      [%g note-gall]                                    ::
  ==  ==                                                ::
++  riff  ,[p=desk q=(unit rave)]                       ::  see %clay
++  sign-ames                                           ::
  $%  [%nice ~]                                         ::
      [%init p=ship]                                    ::
  ==                                                    ::
++  sign-gall                                           ::  see %gall
  $%  [%onto p=(unit tang)]                             ::
  ==                                                    ::
++  sign-clay                                           ::
  $%  [%mere p=(each (set path) (pair term tang))]      ::
      [%note p=@tD q=tank]                              ::
      [%writ p=riot]                                    ::
  ==                                                    ::
++  sign-dill                                           ::
  $%  [%blit p=(list blit)]                             ::
  ==                                                    ::
++  sign-gall                                           ::
  $%  [%onto p=(each suss tang)]                        ::
      [%unto p=cuft]                                    ::
  ==                                                    ::
++  sign-time                                           ::
  $%  [%wake ~]                                         ::
  ==                                                    ::
++  sign                                                ::  in result $<-
  $?  [?(%a %c %e %f %g %t) %mass p=mass]               ::
  $%  [%a sign-ames]                                    ::
      [%c sign-clay]                                    ::
      [%d sign-dill]                                    ::  
      [%g sign-gall]                                    ::
      [%t sign-time]                                    ::
  ==  ==                                                ::
::::::::                                                ::  dill tiles
--
=|  all=axle
|=  [now=@da eny=@ ski=sled]                            ::  current invocation
=>  |%
    ++  as                                              ::  per cause
      |_  $:  [moz=(list move) hen=duct our=ship]
              axon
          ==
      ++  abet                                          ::  resolve
        ^-  [(list move) axle]
        [(flop moz) all(dug (~(put by dug.all) hen +<+))]
      ::
      ++  call                                          ::  receive input
        |=  kyz=kiss
        ^+  +>
        ?+    -.kyz  ~&  [%strange-kiss -.kyz]  +>
          %flow  +>
          %harm  +>
          %hail  +>
          %belt  (send `dill-belt`p.kyz)
          %text  (from %out (tuba p.kyz))
          %crud  ::  (send `dill-belt`[%cru p.kyz q.kyz])
                 (crud p.kyz q.kyz)
          %blew  (send %rez p.p.kyz q.p.kyz)
          %heft  heft
          %veer  (dump kyz)
          %vega  (dump kyz)
          %verb  (dump kyz)
        ==
      ::
      ++  crud
        |=  [err=@tas tac=(list tank)]
        =+  ^=  wol  ^-  wall
            :-  (trip err)
            (zing (turn tac |=(a=tank (~(win re a) [0 wid]))))
        |-  ^+  +>.^$
        ?~  wol  +>.^$
        $(wol t.wol, +>.^$ (from %out (tuba i.wol)))
      ::
      ++  dump                                          ::  pass down to hey
        |=  git=gift
        ?>  ?=(^ hey.all)
        +>(moz [[u.hey.all %give git] moz])
      ::
      ++  done                                          ::  return gift
        |=  git=gift
        +>(moz :_(moz [hen %give git])) 
      ::
      ++  from                                          ::  receive belt
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
              [%lin see]
              [%hop pos]
          ==
        ?:  ?=(%pro -.bit)
          (done(see p.bit) %blit [[%lin p.bit] [%hop pos] ~])
        ?:  ?=(%hop -.bit)
          (done(pos p.bit) %blit [bit ~])
        ?:  ?=(%qit -.bit)
          (dump %logo ~)
        (done %blit [bit ~])
      ::
      ++  heft
        %_    .
            moz
          :*  [hen %pass /heft/ames %a %wegh ~]
              [hen %pass /heft/clay %c %wegh ~]
              [hen %pass /heft/eyre %e %wegh ~]
              [hen %pass /heft/ford %f %wegh ~]
              [hen %pass /heft/gall %g %wegh ~]
              [hen %pass /heft/time %t %wegh ~]
              moz
          ==
        ==
      ::
      ++  init                                          ::  initialize
        ~&  [%dill-init our ram]
        =+  myt=(flop (need tem))
        =+  can=(clan our)
        =.  tem  ~
        =.  moz  :_(moz [hen %pass / %c %font our %home our %base])
        =.  moz  ?.  ?=(?(%king %czar) can)  moz
                 :_(moz [hen %pass / %c %font our %kids our %base])
        =.  moz  :_(moz [hen %pass ~ %g %conf [[our ram] %load our %home]])
        =.  moz  :_(moz [hen %pass ~ %g %deal [our our] ram %peer /drum])
        |-  ^+  +>+
        ?~  myt  +>+
        $(myt t.myt, +>+ (send i.myt))
      ::
      ++  into                                          ::  preinitialize
        |=  gyl=(list gill)
        %_    +>
            tem  `(turn gyl |=(a=gill [%yow a]))
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
        |=  bet=dill-belt
        ?^  tem
          +>(tem `[bet u.tem])
        %_    +>
            moz
          :_  moz
          [hen %pass ~ %g %deal [our our] ram %poke [%dill-belt -:!>(bet) bet]]
        ==
      ::
      ++  pump                                          ::  send diff ack
        %_    .
            moz 
          :_(moz [hen %pass ~ %g %deal [our our] ram %pump ~])
        ==
      ::
      ++  take                                          ::  receive
        |=  sih=sign
        ^+  +>
        ?-    sih
            [?(%a %c %e %f %g %t) %mass *]
          (wegt -.sih p.sih)
        ::
            [%a %nice *]
          ::  ~&  [%take-nice-ames sih]
          +>
        ::
            [%a %init *]
          +>(moz :_(moz [hen %give +.sih]))
        ::
            [%c %mere *]
          ?:  ?=(%& -.p.sih)
            +>.$
          ~|  %dill-mere-fail
          ~|  p.p.p.sih
          |-
          ?~  q.p.p.sih  !!
          ~>  %mean.|.(i.q.p.p.sih)     ::  pull ford fail into stack trace
          $(q.p.p.sih t.q.p.p.sih) 
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
            %coup  ?~(p.p.+>.sih +>.$ (crud %coup u.p.p.+>.sih))
            %quit  ~&  %dill-console-quit
                   (dump %logo ~)
            %reap  ?~(p.p.+>.sih +>.$ (crud %reap u.p.p.+>.sih))
            %diff  pump:(from ((hard dill-blit) q:`vase`+>+>.sih))
          ==
        ::
            [%c %note *]
          (from %out (tuba p.sih ' ' ~(ram re q.sih)))
        ::
            [%c %writ *]
          init
        ::
            [%d %blit *]
          (done +.sih)
        ::
            [%t %wake *]
          ::  ~&  %dill-wake 
          +>
        ==
      ::
      ++  wegh
        ^-  mass
        :-  %dill
        :-  %|
        :~  all/`[ore hey dug]:all
        ==
      ::
      ++  wegt
        |=  [lal=?(%a %c %e %f %g %t) mas=mass]
        ^+  +>
        =.  hef.all
          ?-  lal
            %a  ~?(?=(^ a.hef.all) %double-mass-a hef.all(a `mas))
            %c  ~?(?=(^ c.hef.all) %double-mass-c hef.all(c `mas))
            %e  ~?(?=(^ e.hef.all) %double-mass-e hef.all(e `mas))
            %f  ~?(?=(^ f.hef.all) %double-mass-f hef.all(f `mas))
            %g  ~?(?=(^ g.hef.all) %double-mass-g hef.all(g `mas))
            %t  ~?(?=(^ t.hef.all) %double-mass-t hef.all(t `mas))
          ==
        ?.  ?&  ?=(^ a.hef.all)
                ?=(^ c.hef.all)
                ?=(^ e.hef.all)
                ?=(^ f.hef.all)
                ?=(^ g.hef.all)
                ?=(^ t.hef.all)
            ==
          +>.$
        %+  done(hef.all [~ ~ ~ ~ ~ ~])
          %mass
        =>  [hef.all d=wegh]
        [%vanes %| ~[u.a u.c d u.e u.f u.g u.t]]
      --
    ::
    ++  ax                                              ::  make ++as
      |=  [hen=duct kyz=kiss]                           ::
      ?~  ore.all  ~
      =+  nux=(~(get by dug.all) hen)
      ?^  nux  
        (some ~(. as [~ hen u.ore.all] u.nux))
      ?.  ?=(%flow -.kyz)  ~
      %-  some
      %.  q.kyz
      %~  into  as
      :-  [~ hen u.ore.all]
      :*  p.kyz
          [~ ~]
          80
          0
          (tuba "<awaiting {(trip p.kyz)}>")
      ==
    --
|%                                                      ::  poke/peek pattern
++  call                                                ::  handle request
  |=  $:  hen=duct
          hic=(hypo (hobo kiss))
      ==
  ^-  [p=(list move) q=_..^$]
  =>  %=    .                                           ::  XX temporary
          q.hic
        ^-  kiss
        ?:  ?=(%soft -.q.hic)
          ::  ~&  [%dill-call-soft (,@tas `*`-.p.q.hic)]
          ((hard kiss) p.q.hic)
        ?:  (~(nest ut -:!>(*kiss)) | p.hic)  q.hic
        ~&  [%dill-call-flub (,@tas `*`-.q.hic)]
        ((hard kiss) q.hic)
      ==
  ?:  ?=(%boot -.q.hic)
    :_(..^$ [hen %pass ~ (note %a p.q.hic)]~)
  ?:  ?=(%flog -.q.hic)
    ::  ~&  [%dill-flog +.q.hic]
    ?:  ?=([%crud %hax-init [%leaf *] ~] p.q.hic)
      =+  him=(slav %p (crip p.i.q.p.q.hic))
      :_(..^$ ?~(hey.all ~ [u.hey.all %give %init him]~))
    ?:  ?=([%crud %hax-heft ~] p.q.hic)
      :_(..^$ ?~(hey.all ~ [u.hey.all %slip %d %heft ~]~))
    :_(..^$ ?~(hey.all ~ [u.hey.all %slip %d p.q.hic]~))
  =.  hey.all  ?^(hey.all hey.all `hen)
  ?:  ?=(%init -.q.hic)
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
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load                                                ::  trivial
  |=  old=all-axle
  ?:  ?=(%2 -.old)
    $(old [%3 ore hey dug ~ ~ ~ ~ ~ ~]:old)
  ..^$(all old)
  ::  |=  old=*   ::  diable
  ::  ..^$(ore.all `~zod)
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas his=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit cage))
  [~ ~]
::
++  stay  all 
::
++  take                                                ::  process move
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^-  [p=(list move) q=_..^$]
  ?:  =(~ ore.all)
    ?:  ?=([%a %init *] q.hin)
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

