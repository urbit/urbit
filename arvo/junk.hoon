|%                                                      ::
--                                                      ::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    volume 2, Hoon libraries and compiler ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
~%    %hoon
    +
  ==
    %al    al
    %ap    ap
    %ut    ut
    %mute  mute
    %show  show
  ==
|%
  :::::::::::::::::::::::::::::::::::::::::::::::::::::   ::
::::              chapter 2a, basic unsigned math       ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 2b, basic containers          ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                Section 2bA, units                    ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cE, phonetic base            ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cF, signed and modular ints  ::
::
::
++  stat                                                ::  positive counter
  |*  a/$-(* *)
  |=  (trel ? a (map a @ud))
  ^+  r
  =+  (~(get by r) q)
  ?:  p
    (~(put by r) q ?~(- 1 +(u.-)))
  ?>  ?=(^ -)
  ?:(=(0 u.-) (~(del by r) q) (~(put by r) q (dec u.-)))
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 2d, containers                ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2dA, sets                     ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2dB, maps                     ::
::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2dC, queues                   ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2dD, casual containers        ::
::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 2e, miscellaneous libs        ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eA, packing                  ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eB, parsing (tracing)        ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eC, parsing (custom rules)   ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eD, parsing (combinators)    ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eE, parsing (composers)      ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eF, parsing (ascii)          ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eG, parsing (whitespace)     ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eH, parsing (idioms)         ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eI, parsing (external)       ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eJ, formatting (basic text)  ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eK, formatting (layout)      ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eL, formatting (path)        ::
::
::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eY, SHA-256 (move me)        ::
::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eZ, OLD rendering (kill me)  ::
::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 2f, Hoon proper               ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2fA, miscellaneous funs       ::
::                                                      ::
::
::
::

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  
::                section 2fB, macro expansion          ::
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2fC, prettyprinting           ::
::
--
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2fE, grammar                  ::
::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    profiling support; move me            ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    volume 3, Arvo models and skeleton    ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
~%  %arvo  +  ~
|%
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bE, Arvo core                ::
::
--
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    Postface                              ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
=+  pit=`vase`!>(.)                                     ::
!:
=+  bud=pit                                             ::  becomes tang
=+  vil=(viol p.bud)                                    ::  cached reflexives
=|  $:  lac/?                                           ::  laconic bit
        eny/@                                           ::  entropy
        niz/(pair worm (list {p/@tas q/vase}))          ::  modules
    ==                                                  ::
=<  |%
    ++  come  |=  {@ (list ovum) pone}                  ::  11
              ^-  {(list ovum) _+>}
              ~&  %hoon-come
              =^  rey  +>+  (^come +<)
              [rey +>.$]
    ++  keep  |=(* (^keep ((hard {@da path}) +<)))     ::  4
    ++  load  |=  {@ (list ovum) pane}                  ::  86
              ^-  {(list ovum) _+>}
              ~&  %hoon-load
              =^  rey  +>+  (^load +<)
              [rey +>.$]
    ++  peek  |=(* (^peek ((hard {@da path}) +<)))     ::  87
    ++  poke  |=  *                                     ::  42
              ^-  {(list ovum) *}
              =>  .(+< ((hard {now/@da ovo/ovum}) +<))
              ?:  =(%verb -.q.ovo)
                [~ +>.$(lac !lac)]
              ?:  ?=($veer -.q.ovo)
                [~ +>.$(+ (veer now q.ovo))]
              =^  ova  +>+  (^poke now ovo)
              |-  ^-  {(list ovum) *}
              ?~  ova
                [~ +>.^$]
              ?:  ?=($verb -.q.i.ova)
                $(ova t.ova, lac !lac)
              ?:  ?=($veer -.q.i.ova)
                $(ova t.ova, +>+.^$ (veer now q.i.ova))
              ?:  ?=($vega -.q.i.ova)
                (fall (vega now t.ova (path +.q.i.ova)) [~ +>.^$])
              ?:  ?=($mass -.q.i.ova)
                =+  avo=$(ova t.ova)
                :_  +.avo
                :_  -.avo
                %=    i.ova
                    q.q
                  :-  %userspace
                  :-  %|
                  :~  hoon+`pit
                      zuse+`bud
                      hoon-cache+`p.niz
                      q.q.i.ova
                      dot+`.
                  ==
                ==
              =+(avo=$(ova t.ova) [[i.ova -.avo] +.avo])
    ++  wish  |=(* (^wish ((hard @ta) +<)))            ::  20
    --
|%
++  come                                                ::  load incompatible
  |=  {yen/@ ova/(list ovum) nyf/pone}
  ^+  [ova +>]
  (load yen ova (turn nyf |=({a/@tas b/vise} [a (slim b)])))
::
++  keep                                                ::  wakeup delay
  |=  {now/@da hap/path}
  =>  .(+< ((hard {now/@da hap/path}) +<))
  (~(doos (is vil eny bud niz) now) hap)
::
++  load                                                ::  load compatible
  |=  {yen/@ ova/(list ovum) nyf/pane}
  ^+  [ova +>]
  =:  eny  yen
      q.niz  nyf
    ==
  |-  ^+  [ova +>.^$]
  ?~  ova
    [~ +>.^$]
  ?:  ?=($verb -.q.i.ova)
    $(ova t.ova, lac !lac)
  ?:  ?=($veer -.q.i.ova)
    $(ova t.ova, +>.^$ (veer *@da q.i.ova))
  =+(avo=$(ova t.ova) [[i.ova -.avo] +.avo])
::
++  peek                                                ::  external inspect
  |=  {now/@da hap/path}
  ^-  (unit)
  ?~  hap  [~ hoon]
  =+  rob=((sloy ~(beck (is vil eny bud niz) now)) [151 %noun] hap)
  ?~  rob  ~
  ?~  u.rob  ~
  [~ u.u.rob]
::
++  poke                                                ::  external apply
  |=  {now/@da ovo/ovum}
  =.  eny  (mix eny (shax now))
  ::  ~&  [%poke -.q.ovo]
  ^-  {(list ovum) _+>}
  =^  zef  niz
    (~(hurl (is vil eny bud niz) now) lac ovo)
  [zef +>.$]
::
++  vega                                                ::  reboot kernel
  |=  {now/@da ova/(list ovum) hap/path}
  ^-  (unit {p/(list ovum) q/*})
  =-  ?:(?=($| -.res) ((slog p.res) ~) `p.res)
  ^=  res  %-  mule  |.
  =+  pax=(weld hap `path`[%hoon ~])
  ~&  [%vega-start hap]
  =+  src=((hard @t) (need (peek now cx+pax)))
  =+  saz=(shax src)
  =+  gen=(rain hap src)
  ~&  %vega-parsed
  =+  ken=.*(0 q:(~(mint ut %noun) %noun gen))
  =+  ^=  nex
      =+  gat=.*(ken .*(ken [0 87]))
      (need ((hard (unit @)) .*([-.gat [[now ~] +>.gat]] -.gat)))
  ~&  [%vega-compiled hoon nex]
  ?>  (lte nex hoon)
  =+  gat=.*(ken .*(ken [0 ?:(=(nex hoon) 86 11)]))
  =+  sam=[eny ova q.niz]
  =+  raw=.*([-.gat [sam +>.gat]] -.gat)
  [[[~ %vega hap] ((list ovum) -.raw)] +.raw]
::
++  veer                                                ::  install vane/tang
  |=  {now/@da fav/curd}
  =>  .(fav ((hard {$veer lal/@ta pax/path txt/@t}) fav))
  =-  ?:(?=($| -.res) ((slog p.res) +>.$) p.res)
  ^=  res  %-  mule  |.
  ?:  =(%$ lal.fav)
    ~&  [%tang pax.fav `@p`(mug txt.fav)]
    =+  gen=(rain pax.fav txt.fav)
    =+  vax=(slap pit gen)
    +>.^$(bud vax)
  %_    +>.^$
      q.niz
    |-  ^+  q.niz
    ?~  q.niz
      ~&  [%vane `@tas`lal.fav pax.fav `@p`(mug txt.fav)]
      =+  vin=(vint lal.fav vil bud pax.fav txt.fav)
      ?~  vin
        q.niz
      [[lal.fav q.sew:u.vin] q.niz]
    ?.  =(lal.fav p.i.q.niz)
      [i.q.niz $(q.niz t.q.niz)]
      ~&  [%vane `@tas`lal.fav pax.fav `@p`(mug txt.fav)]
    :_  t.q.niz
    :-  p.i.q.niz
    q.sew:(ruck:(vent lal.fav vil bud [p.niz q.i.q.niz]) pax.fav txt.fav)
  ==
::
++  wish                                                ::  external compute
 |=  txt/@
  q:(slap bud (ream txt))
--
.  ==
