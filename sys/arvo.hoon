::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    Postface                              ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
~>  %slog.[0 leaf+"%arvo-assembly"]
=-  ~>  %slog.[0 leaf+"%arvo-assembled"]
    -
=<  ::
    ::  Arvo formal interface
    ::
    ::    this lifecycle wrapper makes the arvo door (multi-armed core)
    ::    look like a gate (function or single-armed core), to fit
    ::    urbit's formal lifecycle function. a practical interpreter
    ::    can ignore it.
    ::
    |=  [now=@da ovo=*]
    ^-  *
    ~>  %slog.[0 leaf+"arvo-event"]
    .(+> +:(poke now ovo))
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    volume 3, Arvo models and skeleton    ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
=>
|%
++  arch  {fil/(unit @uvI) dir/(map @ta $~)}            ::  fundamental node
++  arvo  (wind {p/term q/mill} mill)                   ::  arvo card
++  beam  {{p/ship q/desk r/case} s/path}               ::  global name
++  beak  {p/ship q/desk r/case}                        ::  garnish with beak
++  bone  @ud                                           ::  opaque duct
++  case                                                ::  version
          $%  {$da p/@da}                               ::  date
              {$tas p/@tas}                             ::  label
              {$ud p/@ud}                               ::  sequence
          ==                                            ::
++  desk  @tas                                          ::  ship desk case spur
++  dock  (pair @p term)                                ::  message target
++  cage  (cask vase)                                   ::  global metadata
++  cask  |*(a/mold (pair mark a))                      ::  global data
++  curd  {p/@tas q/*}                                  ::  typeless card
++  duct  (list wire)                                   ::  causal history
++  hypo  |*(a/mold (pair type a))                      ::  type associated
++  hobo  |*  a/mold                                    ::  kiss wrapper
          $?  $%  {$soft p/*}                           ::
              ==                                        ::
              a                                         ::
          ==                                            ::
++  mark  @tas                                          ::  content type
++  mash  |=(* (mass +<))                               ::  producing mass
++  mass  $~  [%$ [%& ~]]                               ::  memory usage
          (pair cord (each noun (list mash)))           ::
++  mill  (each vase milt)                              ::  vase+metavase
++  milt  {p/* q/*}                                     ::  metavase
++  monk  (each ship {p/@tas q/@ta})                    ::  general identity
++  muse  {p/@tas q/duct r/arvo s/@ud}                  ::  sourced move
++  move  {p/duct q/arvo}                               ::  arvo move
++  ovum  {p/wire q/curd}                               ::  typeless ovum
++  pane  (list {p/@tas q/vase})                        ::  kernel modules
++  pone  (list {p/@tas q/vise})                        ::  kernel modules old
+$  scry-sample
  [fur=(unit (set monk)) ren=@tas why=shop syd=desk lot=coin tyl=path]
+$  vane-sample
  [our=ship now=@da eny=@uvJ ski=slyd]
++  ship  @p                                            ::  network identity
++  sink  (trel bone ship path)                         ::  subscription
++  sley  $-  {* (unit (set monk)) term beam}           ::  namespace function
          (unit (unit cage))                            ::
++  slyd  $-  {* (unit (set monk)) term beam}           ::  super advanced
          (unit (unit (cask)))                          ::
++  slyt  $-({* *} (unit (unit)))                       ::  old namespace
+$  vane  [=vase =worm]
++  vile                                                ::  reflexive constants
          $:  typ/type                                  ::  -:!>(*type)
              duc/type                                  ::  -:!>(*duct)
              pah/type                                  ::  -:!>(*path)
              mev/type                                  ::  -:!>([%meta *vase])
          ==                                            ::
++  wind                                                ::  new kernel action
          |*  {a/mold b/mold}                           ::  forward+reverse
          $%  {$pass p/path q/a}                        ::  advance
              {$slip p/a}                               ::  lateral
              {$give p/b}                               ::  retreat
          ==                                            ::
++  wire  path                                          ::  event pretext
--
=>
~%  %hex  +>  ~
|%
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bE, Arvo core                ::
::
++  sloy
  ::  +sloy: adapter from old style scrys to new style scrys
  ::
  ::    This does path parsing which shows up hot, but removing the last +slay
  ::    here requires deeper interface changes.
  ::
  !:
  ~/  %sloy
  |=  sod/slyd
  ^-  slyt
  |=  {ref/* raw/*}
  =+  pux=((soft path) raw)
  ?~  pux  ~
  ?.  ?=({@ @ @ @ *} u.pux)  ~
  =+  :*  hyr=(slaw %tas i.u.pux)
          fal=(slaw %p i.t.u.pux)
          dyc=(slaw %tas i.t.t.u.pux)
          ved=(slay i.t.t.t.u.pux)
          tyl=t.t.t.t.u.pux
      ==
  ?~  hyr  ~
  ?~  fal  ~
  ?~  dyc  ~
  ?.  ?=(^ ved)  ~
  =/  ron=@tas  u.hyr
  =+  bed=[[u.fal u.dyc (case p.u.ved)] (flop tyl)]
  =+  bop=(sod ref ~ ron bed)
  ?~  bop  ~
  ?~  u.bop  [~ ~]
  [~ ~ +.q.u.u.bop]
::  +sloy-light: minimal parsing version of sloy
::
::    There are several places inside vanes where we manually call the scry
::    function raw, instead of passing it into +mink. In those cases, we're
::    paying the price to render the arguments as text, and then are
::    immediately parsing the passed in data. We can avoid that.
::
::    TODO: The entire scrying system needs to be cleaned up in a more
::    permanent way. This hack fixes up some print/parse costs, but doesn't
::    recover the print/parse costs of the scry itself, which we could prevent
::    if we didn't send (list @ta), but instead sent (list dime).
::
++  sloy-light
  ~/  %sloy-light
  |=  sod/slyd
  |=  [ref=* ron=@tas fal=@p dyc=@tas ved=case tyl=path]
  ::  we do not flop tyl because tyl wouldn't have been flopped by +en-beam
  ::
  =+  bed=[[fal dyc ved] tyl]
  =+  bop=(sod ref ~ ron bed)
  ?~  bop  ~
  ?~  u.bop  [~ ~]
  [~ ~ +.q.u.u.bop]
::
++  symp                                                ::  symbol or empty
  |=  a=*  ^-  @tas
  ?.(&(?=(@ a) ((sane %tas) a)) %$ a)
::
++  vent                                                ::  vane core
  |=  [who=ship lal=@tas vil=vile bud=vase =vane]
  ~%  %vent  +>+  ~
  |%
  ++  ruck                                              ::  update vase
    |=  {pax/path txt/@ta}
    ^+  +>
    =-  ?:(?=(%| -.res) ((slog p.res) +>.$) p.res)
    ^=  res  %-  mule  |.
    ::  XX should use real entropy and the real date
    ::
    =/  arg=vane-sample
      [who ~2000.1.1 *@uvJ =>(~ |~(* ~))]
    =+  rig=(slym vase.vane arg)
    =+  gen=(rain pax txt)
    =+  rev=(slym (slap bud gen) bud)
    =+  syg=(slym rev arg)
    ::  update the vane itself
    ::
    ::    We don't cache the +slap/+slam types because they're only used once
    ::    right here; they'll never be used again.
    ::
    =.  vase.vane
      ~|  %load-lost
      (slam (slap syg [%limb %load]) (slap rig [%limb %stay]))
    ::  prime the new compiler cache
    ::
    prime
  ::  reset and prime the worm cache for scrys
  ::
  ::    If the +slap/+slym in scry isn't cached, we spend the majority of
  ::    the time in a scry in the compiler. The +scry gate cannot have side
  ::    effects so we can't modify the cache at access time. So we seed the
  ::    cache with all the things +scry will need when we install the vane
  ::
  ++  prime
    ^+  ..prime
    ::
    %_    ..prime
        worm.vane
      ::  reset cache and add in vane activation entry
      ::
      =^  rig  worm.vane
        (~(slym wa *worm) vase.vane *vane-sample)
      ::  cache the access of the %scry arm
      ::
      +:(~(slap wa worm.vane) rig [%limb %scry])
    ==
  ::
  ++  wink                                              ::  deploy
    |=  {now/@da eny/@ ski/slyd}
    =^  rig  worm.vane
      ~|  [%failed-vane-activation-for lal]
      (~(slym wa worm.vane) vase.vane `vane-sample`[who +<])  ::  activate vane
    ~%  %wink  +>+>  ~
    |%
    ++  slid
      |=  {hed/mill tal/mill}
      ^-  mill
      ?:  &(?=(%& -.hed) ?=(%& -.tal))
        [%& (slop p.hed p.tal)]
      [%| [%cell p.p.hed p.p.tal] [q.p.hed q.p.tal]]
    ::
    ++  slix
      |=  hil/mill
      ^-  mill
      ?-  -.hil
        %&  [%& (slop [typ.vil p.p.hil] p.hil)]
        %|  [%| [%cell typ.vil p.p.hil] p.hil]
      ==
    ::
    ++  slur                                            ::  call gate on
      |=  {gat/vase hil/mill}
      ^-  (unit (pair vase worm))
      =^  sam  worm.vane
        ~|  [%failed-slot-in lal]
        (~(slot wa worm.vane) 6 gat)
      =^  hig  worm.vane
        ~|  [%failed-nest-in lal]
        ?-  -.hil
          %&  (~(nest wa worm.vane) p.sam p.p.hil)
          %|  (~(nets wa worm.vane) p.sam p.p.hil)
        ==
      ?.  hig
        ~
      ~|  [%failed-slym-in lal]
      `(~(slym wa worm.vane) gat +>.hil)
    ::
    ++  slur-a  ~/(%slur-a |=({gat/vase hil/mill} =+(%a (slur gat hil))))
    ++  slur-b  ~/(%slur-b |=({gat/vase hil/mill} =+(%b (slur gat hil))))
    ++  slur-c  ~/(%slur-c |=({gat/vase hil/mill} =+(%c (slur gat hil))))
    ++  slur-d  ~/(%slur-d |=({gat/vase hil/mill} =+(%d (slur gat hil))))
    ++  slur-e  ~/(%slur-e |=({gat/vase hil/mill} =+(%e (slur gat hil))))
    ++  slur-f  ~/(%slur-f |=({gat/vase hil/mill} =+(%f (slur gat hil))))
    ++  slur-g  ~/(%slur-g |=({gat/vase hil/mill} =+(%g (slur gat hil))))
    ++  slur-j  ~/(%slur-j |=({gat/vase hil/mill} =+(%j (slur gat hil))))
    ++  slur-z  ~/(%slur-z |=({gat/vase hil/mill} =+(%z (slur gat hil))))
    ::
    ++  slur-pro                                        ::  profiling slur
      ~/  %slur-pro
      |=  {lal/@tas gat/vase hil/mill}
      ?+  lal  (slur-z gat hil)
        $a  (slur-a gat hil)
        $b  (slur-b gat hil)
        $c  (slur-c gat hil)
        $d  (slur-d gat hil)
        $e  (slur-e gat hil)
        $f  (slur-f gat hil)
        $g  (slur-g gat hil)
        $j  (slur-j gat hil)
      ==
    ::
    ++  song                                            ::  reduce metacard
      ~/  %song                                         ::
      |=  mex/vase                                      ::  mex: vase of card
      ^-  (unit (pair mill worm))                       ::
      =^  hip  worm.vane  (~(nell wa worm.vane) p.mex)          ::
      ?.  hip  ~                                        ::  a card is a cell
      ?.  ?=($meta -.q.mex)  `[[%& mex] worm.vane]          ::  ordinary card
      =^  tiv  worm.vane  (~(slot wa worm.vane) 3 mex)          ::
      =^  hip  worm.vane  (~(nell wa worm.vane) p.tiv)          ::
      ?.  hip  ~                                        ::  a vase is a cell
      =^  vax  worm.vane  (~(slot wa worm.vane) 2 tiv)          ::
      =^  hip  worm.vane  (~(nest wa worm.vane) typ.vil p.vax)  ::
      ?.  hip  ~                                        ::  vase head is type
      %+  biff                                          ::
        =+  mut=(milt q.tiv)                            ::  card type, value
        |-  ^-  (unit (pair milt worm))                 ::
        ?.  ?=({$meta p/* q/milt} q.mut)  `[mut worm.vane]  ::  ordinary metacard
        =^  hip  worm.vane  (~(nets wa worm.vane) mev.vil p.mut)::
        ?.  hip  ~                                      ::  meta-metacard
        $(mut +.q.mut)                                  ::  descend into meta
      |=(a/(pair milt worm) `[[%| p.a] q.a])            ::  milt to mill
    ::
    ++  sump                                            ::  vase to move
      ~/  %sump
      |=  wec/vase
      ^-  (unit (pair move worm))
      %+  biff  ((soft duct) -.q.wec)
      |=  a/duct
      %+  bind
        =-  ?-  -.har
              %|  ~&  [%dead-card p.har]  ~             ::  XX properly log?
              %&  (some p.har)
            ==
        ^=  har  ^-  (each (pair arvo worm) term)
        =^  caq  worm.vane  (~(spot wa worm.vane) 3 wec)
        ?+    q.caq   [%| (cat 3 %funk (symp q.caq))]
        ::
            {$pass p/* q/@tas r/{p/@tas q/*}}
          %-  (bond |.([%| p.r.q.caq]))
          %+  biff  ((soft @) q.q.caq)
          |=  lal/@tas
          ?.  ((sane %tas) lal)  ~
          %+  biff  ((soft path) p.q.caq)
          |=  pax/path
          =^  yav  worm.vane  (~(spot wa worm.vane) 15 caq)
          %+  bind  (song yav)
          |=  {hil/mill vel/worm}
          [%& [%pass pax lal hil] vel]
        ::
            {$give p/{p/@tas q/*}}
          %-  (bond |.([%| p.p.q.caq]))
          =^  yav  worm.vane  (~(spot wa worm.vane) 3 caq)
          %+  bind  (song yav)
          |=  {hil/mill vel/worm}
          [%& [%give hil] vel]
        ::
            {$slip p/@tas q/{p/@tas q/*}}
          %-  (bond |.([%| p.q.q.caq]))
          %+  biff  ((soft @) p.q.caq)
          |=  lal/@tas
          ?.  ((sane %tas) lal)  ~
          =^  yav  worm.vane  (~(spot wa worm.vane) 7 caq)
          %+  bind  (song yav)
          |=  {hil/mill vel/worm}
          [%& [%slip lal hil] vel]
        ==
      |=(b/(pair arvo worm) [`move`[a p.b] q.b])
    ::
    ++  said                                            ::  vase to (list move)
      |=  vud/vase
      |-  ^-  (pair (list move) worm)
      ?:  =(~ q.vud)  [~ worm.vane]
      =^  hed  worm.vane  (~(slot wa worm.vane) 2 vud)
      =^  tal  worm.vane  (~(slot wa worm.vane) 3 vud)
      =^  mov  worm.vane  (need (sump hed))
      =^  moz  worm.vane  $(vud tal)
      [[mov moz] worm.vane]
    ::
    ++  scry                                            ::  read namespace
      ~/  %scry
      |=  $:  fur/(unit (set monk))
              ren/@t
              bed/beam
          ==
      ^-  (unit (unit (cask)))
      ::  ~&  [%arvo-scry ren bed]
      =/  old=scry-sample
        :*  fur
            ren
            [%& p.bed]
            q.bed
            `coin`[%$ r.bed]
            (flop s.bed)
        ==
      ^-  (unit (unit (cask)))
      =+  fun=-:(~(slap wa worm.vane) rig [%limb %scry])
      ::
      %-  (unit (unit (cask)))
      (slum q.fun old)
    ::
    ++  soar                                            ::  scrub vane
      |=  sev/vase
      ^-  vase
      ?:  &(=(-.q.vase.vane -.q.sev) =(+>.q.vase.vane +>.q.sev))
        vase.vane                                           ::  unchanged, use old
      =|  sam=vane-sample
      sev(+<.q sam(ski =>(~ |~(* ~))))                      ::  clear to stop leak
    ::
    ++  swim
      ~/  %swim
      |=  $:  org/@tas
              pux/(unit wire)
              hen/duct
              hil/mill
          ==
      ^-  [(list move) _vane]
      ~|  [%failed-swim lal org pux]
      ::  ~&  [%swim-wyt `@ud`~(wyt in worm.vane)]
      =+  ^=  pru
          ?~  pux
            ~|  [%swim-call-vane lal (symp ?@(+.p.hil +.p.hil +<.p.hil))]
            =^  vax  worm.vane  (~(slap wa worm.vane) rig [%limb %call])
            %^  slur-pro  lal  vax
            (slid [%& duc.vil hen] (slix hil))
          ~|  [%swim-take-vane lal (symp ?@(+.p.hil +.p.hil +<.p.hil))]
          =^  vax  worm.vane  (~(slap wa worm.vane) rig [%limb %take])
          %^  slur-pro  lal   vax
          ;:  slid
            [%& pah.vil u.pux]
            [%& duc.vil hen]
            (slix (slid [%& [%atom %tas `org] org] hil))
          ==
      ?~  pru
        ~&  [%swim-lost lal (symp +>-.hil)]
        [~ [vase.vane worm.vane]]
      =^  pro  worm.vane  (need pru)
      =^  moz  worm.vane  (~(slot wa worm.vane) 2 pro)
      =^  vem  worm.vane  (~(slot wa worm.vane) 3 pro)
      =^  sad  worm.vane  (said moz)
      [sad [(soar vem) worm.vane]]
    --
  --
::
++  vint                                                ::  create vane
  |=  $:  who=ship
          lal=@tas
          vil=vile
          bud=vase
          pax=path
          txt=@ta
      ==
  =-  ?:(?=(%| -.res) ((slog p.res) ~) (some p.res))
  ^=  res  %-  mule  |.
  ~|  [%failed-vint lal]
  =+  gen=(rain pax txt)
  ~&  [%vane-parsed `@p`(mug gen)]
  =+  pro=(vent who lal vil bud [(slym (slap bud gen) bud) *worm])
  ~&  [%vane-compiled `@p`(mug pro)]
  prime:pro
::
++  viol                                                ::  vane tools
  |=  but/type
  ^-  vile
  =+  pal=|=(a/@t ^-(type (~(play ut but) (vice a))))
  :*  typ=(pal '$:type')
      duc=(pal '$:duct')
      pah=(pal '$:path')
      mev=(pal '$:{$meta $vase}')
  ==
::
++  is                                                  ::  operate in time
  |=  [who=ship vil=vile eny=@ bud=vase vanes=(list [label=@tas =vane])]
  |_  now/@da
  ++  beck
    ^-  slyd
    |=  {* fur/(unit (set monk)) ron/term bed/beam}
    ^-  (unit (unit (cask)))
    =>  .(fur ?^(fur fur `[[%& p.bed] ~ ~]))            ::  XX heinous
    =+  lal=(end 3 1 ron)
    =+  ren=(@t (rsh 3 1 ron))
    |-  ^-  (unit (unit (cask)))
    ?~  vanes  ~
    ?.  =(lal label.i.vanes)  $(vanes t.vanes)
    ~|  [%failed-scry ron bed]
    %-  scry:(wink:(vent who lal vil bud vane.i.vanes) now eny ..^$)
    [fur ren bed]
  ::
  ++  dink                                              ::  vase by char
    |=  din/@tas  ^-  vase
    ?~(vanes !! ?:(=(din label.i.vanes) vase.vane.i.vanes $(vanes t.vanes)))
  ::
  ++  dint                                              ::  input routing
    |=  hap/path  ^-  @tas
    ?+  hap  !!
      {@ $ames *}  %a
      {@ $boat *}  %c
      {@ $newt *}  %a
      {@ $sync *}  %c
      {@ $term *}  %d
      {@ $http *}  %e
      {@ $behn *}  %b
    ==
  ::
  ++  hurl                                              ::  start loop
    |=  {lac/? ovo/ovum}
    ~?  &(!lac !=(%belt -.q.ovo))  ["" %unix -.q.ovo p.ovo]
    ::  ^-  {p/(list ovum) q/(pair worm (list {p/@tas q/vase}))}
    ^-  {p/(list ovum) q=(list [label=@tas =vane])}
    ?>  ?=(^ p.ovo)
    %+  kick  lac
    :~  :*  i.p.ovo
            ~
            :^  %pass  t.p.ovo
              (dint p.ovo)
            :+  %&
              [%cell [%atom %tas `%soft] %noun]
            [%soft q.ovo]
            0
        ==
    ==
  ::
  ++  race                                              ::  take
    |=  {org/@tas lal/@tas pux/(unit wire) hen/duct hil/mill =vane}
    ^-  [p=(list move) q=_vane]
    =+  ven=(vent who lal vil bud vane)
    ~|  [%failed-take lal]
    =+  win=(wink:ven now eny beck)
    (swim:win org pux hen hil)
  ::
  ++  fire                                              ::  execute
    |=  {org/term deh/@ud lal/term pux/(unit wire) hen/duct hil/mill}
    ^-  {{p/(list ovum) q/(list muse)} _vanes}
    ?:  &(?=(^ pux) ?=($~ hen))
      [[[[lal u.pux] (curd +>.hil)]~ ~] vanes]
    =+  naf=vanes
    |-  ^-  {{p/(list ovum) q/(list muse)} _vanes}
    ?~  naf  [[~ ~] ~]
    ?.  =(lal label.i.naf)
      =+  tuh=$(naf t.naf)
      [-.tuh [+<.tuh [i.naf +>.tuh]]]
    ::
    =+  fiq=(race org lal pux hen hil vane.i.naf)
    :-  [~ (turn p.fiq |=(a/move [lal p.a q.a +(deh)]))]
    [[label.i.naf q.fiq] t.naf]
  ::
  ++  jack                                              ::  dispatch card
    |=  {lac/? gum/muse}
    ^-  {{p/(list ovum) q/(list muse)} _vanes}
    ~|  %failed-jack
    ::  =.  lac  |(lac ?=(?(%g %f) p.gum))
    ::  =.  lac  &(lac !?=($b p.gum))
    %^    fire
        p.gum
      s.gum
    ?-    -.r.gum
        $pass
      ~?  &(!lac !=(%$ p.gum))
        :-  (runt [s.gum '|'] "")
        :^  %pass  [p.gum p.q.r.gum]
          [(symp +>-.q.q.r.gum) p.r.gum]
        q.gum
      [p.q.r.gum ~ [[p.gum p.r.gum] q.gum] q.q.r.gum]
    ::
        $give
      ?.  &(?=(^ q.gum) ?=(^ i.q.gum))
        ~|  [%jack-bad-duct q.gum]
        ~|  [%jack-bad-card p.gum (symp +>-.p.r.gum)]
        !!
      ~?  &(!lac |(!=(%blit +>-.p.r.gum) !=(%d p.gum)))
        :-  (runt [s.gum '|'] "")
        [%give p.gum (symp +>-.p.r.gum) `duct`q.gum]
      [i.i.q.gum [~ t.i.q.gum] t.q.gum p.r.gum]
    ::
        $slip
      ~?  !lac
        :-  (runt [s.gum '|'] "")
        [%slip p.gum (symp +>-.q.p.r.gum) q.gum]
      [p.p.r.gum ~ q.gum q.p.r.gum]
    ==
  ::
  ++  kick                                              ::  new main loop
    |=  {lac/? mor/(list muse)}
    =|  ova/(list ovum)
    |-  ^-  {p/(list ovum) q=(list [label=@tas =vane])}
    ?~  mor
      [ova vanes]
    =^  nyx  vanes  (jack lac i.mor)
    ::  we emit ova to unix in fifo order, but emit internal moves depth-first
    ::
    $(ova (weld ova p.nyx), mor (weld q.nyx t.mor))
  ::  +spam: kick every vane with :ovum
  ::
  ++  spam
    |=  [lac=? ovo=ovum]
    ^-  [(list ovum) (list [label=@tas =vane])]
    =/  card
      :+  %&
        [%cell [%atom %tas `%soft] %noun]
      [%soft q.ovo]
    %+  kick  lac
    %+  turn  vanes
    |=([label=@tas *] [label ~ [%pass p.ovo label card] 0])
  --
--
=<  ::  Arvo larval stage
    ::
    ::    The true Arvo kernel knows who it is. It should not *maybe*
    ::    have an identity, nor should it contain multitudes. This outer
    ::    kernel exists to accumulate identity, entropy, and the
    ::    standard library. Upon having done so, it upgrades itself into
    ::    the true Arvo kernel. Subsequent upgrades will fall through
    ::    the larval stage directly into the actual kernel.
    ::
    ::    For convenience, this larval stage also supports hoon compilation
    ::    with +wish and vane installation with the %veer event.
    ::
    =/  pit=vase  !>(..is)
    =|  $:  ::  who: our identity once we know it
            ::  eny: entropy once we learn it
            ::  bod: %zuse once we receive it
            ::
            who=(unit ship)
            eny=(unit @)
            bod=(unit vase)
        ==
    ::  larval Arvo structural interface
    ::
    |%
    ++  come  ^come                                     ::   4
    ++  load  ^load                                     ::  10
    ++  peek  |=(* ~)                                   ::  46
    ::
    ++  poke  |=  *                                     ::  47
              ^-  [(list ovum) *]
              =>  .(+< ((hard ,[now=@da ovo=ovum]) +<))
              ^-  [(list ovum) *]
              =.  +>.$
                ?+  -.q.ovo
                  ::  ignore unrecognized
                  ::
                  ~&  [%larval-ignore p.ovo -.q.ovo]
                  +>.$
                ::  install %zuse or vane
                ::
                    %veer
                  ^+  +>.$
                  ::  use the maximum comet if we don't know who we are yet
                  ::
                  =/  our
                    ?^  who
                      u.who
                    =/  fip=ship  (dec (bex 128))
                    ~>(%slog.[0 leaf+"arvo: larval identity {(scow %p fip)}"] fip)
                  =.  ..veer  (veer our now q.ovo)
                  +>.$(bod ?^(bod bod `bud.^poke))
                ::  add entropy
                ::
                    %wack
                  ^+  +>.$
                  ?>  ?=(@ q.q.ovo)
                  +>.$(eny `q.q.ovo)
                ::  become who you were born to be
                ::
                    %whom
                  ^+  +>.$
                  ?>  ?=(@ q.q.ovo)
                  +>.$(who `q.q.ovo)
                ==
              ::  upgrade once we've accumulated identity, entropy, and %zuse
              ::
              ?.  &(?=(^ who) ?=(^ eny) ?=(^ bod))
                [~ +>.$]
              ~>  %slog.[0 leaf+"arvo: metamorphosis"]
              =/  nyf
                (turn vanes.^poke |=([label=@tas =vane] [label vase.vane]))
              (load u.who now u.eny ova=~ u.bod nyf)
    ::
    ++  wish  |=  txt=*                                 ::  22
              ?>  ?=(@ txt)
              q:(slap ?~(bod pit u.bod) (ream txt))
    --
::
::  persistent arvo state
::
=/  pit=vase  !>(..is)                                  ::
=/  vil=vile  (viol p.pit)                              ::  cached reflexives
=|  $:  lac=?                                           ::  laconic bit
        eny=@                                           ::  entropy
        our=ship                                        ::  identity
        bud=vase                                        ::  %zuse
        vanes=(list [label=@tas =vane])                 ::  modules
    ==                                                  ::
=<  ::  Arvo structural interface
    ::
    |%
    ++  come  |=  {@ @ @ (list ovum) vise pone}         ::   4
              ^-  {(list ovum) _+>}
              ~&  %hoon-come
              =^  rey  +>+  (^come +<)
              [rey +>.$]
    ::
    ++  load  |=  {@ @ @ (list ovum) vase pane}         ::  10
              ^-  {(list ovum) _+>}
              ~&  %hoon-load
              =^  rey  +>+  (^load +<)
              [rey +>.$]
    ::
    ++  peek  |=(* (^peek ((hard {@da path}) +<)))      ::  46
    ::
    ++  poke  |=  *                                     ::  47
              ^-  [(list ovum) *]
              =>  .(+< ((hard ,[now=@da ovo=ovum]) +<))
              =^  ova  +>+.$  (^poke now ovo)
              =|  out=(list ovum)
              |-  ^-  [(list ovum) *]
              ?~  ova
                [(flop out) +>.^$]
              ::  upgrade the kernel
              ::
              ?:  ?=(%lyra -.q.i.ova)
                %+  fall
                  (vega now t.ova ({@ @} +.q.i.ova))
                [~ +>.^$]
              ::  iterate over effects, handling those on arvo proper
              ::  and passing the rest through as output
              ::
              =^  vov  +>+.^$  (feck now i.ova)
              =?  out  ?=(^ vov)  [+.vov out]
              $(ova t.ova)
    ::
    ++  wish  |=(* (^wish ((hard @ta) +<)))             ::  22
    --
::  Arvo implementation core
::
|%
++  come                                                ::  load incompatible
  |=  [who=ship now=@da yen=@ ova=(list ovum) dub=vise nyf=pone]
  ^+  [ova +>]
  =/  fyn  (turn nyf |=([a=@tas b=vise] [a (slim b)]))
  (load who now yen ova (slim dub) fyn)
::
++  load                                                ::  load compatible
  |=  [who=ship now=@da yen=@ ova=(list ovum) dub=vase nyf=pane]
  ^+  [ova +>]
  =:  our  who
      eny  yen
      bud  dub
      vanes  (turn nyf |=({a/@tas b/vise} [a [b *worm]]))
    ==
  =|  out=(list ovum)
  |-  ^-  [(list ovum) _+>.^$]
  ?~  ova
    [(flop out) +>.^$]
  ::  iterate over effects, handling those on arvo proper
  ::  and passing the rest through as output
  ::
  ::    In practice, the pending effects after an upgrade
  ::    are the %veer moves to install %zuse and the vanes,
  ::    plus a %vega notification that the upgrade is complete.
  ::
  ::    N.B. this implementation assumes that %vega will be
  ::    at the end of :ova.
  ::
  ?:  ?=(%vega -.q.i.ova)
    =^  zef  vanes
      (~(spam (is our vil eny bud vanes) now) lac i.ova)
    $(out [i.ova out], ova (weld t.ova zef))
  ::
  =^  vov  +>.^$  (feck now i.ova)
  =?  out  ?=(^ vov)  [+.vov out]
  $(ova t.ova)
::
++  peek                                                ::  external inspect
  |=  {now/@da hap/path}
  ^-  (unit)
  ?~  hap  [~ hoon-version]
  =+  rob=((sloy ~(beck (is our vil eny bud vanes) now)) [151 %noun] hap)
  ?~  rob  ~
  ?~  u.rob  ~
  [~ u.u.rob]
::
++  poke                                                ::  external apply
  |=  [now=@da ovo=ovum]
  =.  eny  (shaz (cat 3 eny now))
  ^-  [(list ovum) _+>.$]
  ::
  ::  These external events are actually effects on arvo proper.
  ::  They can also be produced as the effects of other events.
  ::  In either case, they fall through here to be handled
  ::  after the fact in +feck.
  ::
  ?:  ?=(?(%veer %verb %wack) -.q.ovo)
    [[ovo ~] +>.$]
  ::
  =^  zef  vanes
    (~(hurl (is our vil eny bud vanes) now) lac ovo)
  [zef +>.$]
::  +feck: handle an arvo effect
::
++  feck
  |=  [now=@da ovo=ovum]
  ^-  [(unit ovum) _+>.$]
  ?+  -.q.ovo
      ::  pass through unrecognized effect
      ::
      [[~ ovo] +>.$]
  ::  toggle event verbose event printfs
  ::
      %verb
    [~ +>.$(lac !lac)]
  ::  install %zuse or vane
  ::
      %veer
    [~ (veer our now q.ovo)]
  ::  add data to memory profile
  ::
      %mass
    =.  q.q.ovo
      :-  %userspace
      :-  %|
      :~  hoon+&+pit
          zuse+&+bud
          :+  %caches  %|
          %+  turn
            %+  sort  vanes
            |=([a=[lab=@tas *] b=[lab=@tas *]] (aor lab.a lab.b))
          |=([label=@tas =vane] [(cat 3 %vane- label) %& worm.vane])
          q.q.ovo
          :+  %vases  %|
          %+  turn
            %+  sort  vanes
            |=([a=[lab=@tas *] b=[lab=@tas *]] (aor lab.a lab.b))
          |=([label=@tas =vane] [(cat 3 %vane- label) %& vase.vane])
          dot+&+.
      ==
    [[~ ovo] +>.$]
  ::  add entropy
  ::
      %wack
    ?>  ?=(@ q.q.ovo)
    =.  eny  (shaz (cat 3 eny q.q.ovo))
    [~ +>.$]
  ==
::
++  vega                                                ::  reboot kernel
  |=  $:  ::  now: current date
          ::  ova: actions to process after reboot
          ::  hun: hoon.hoon source
          ::  arv: arvo.hoon source
          ::
          now=@da
          ova=(list ovum)
          hun=@t
          van=@t
      ==
  ^-  (unit (pair (list ovum) *))
  ::  virtualize; dump error if we fail
  ::
  =-  ?:(?=(%| -.res) ((slog p.res) ~) `p.res)
  ^=  res  %-  mule  |.
  ::  produce a new kernel and an effect list
  ::
  ^-  (pair (list ovum) *)
  ::  compile the hoon.hoon source with the current compiler
  ::
  =/  raw
    ~&  [%hoon-compile `@p`(mug hun)]
    (ride %noun hun)
  ::  activate the new compiler gate, producing +ride
  ::
  =/  cop  .*(0 +.raw)
  ::  find the hoon version number of the new kernel
  ::
  =/  nex
    (@ .*(cop q:(~(mint ut p.raw) %noun [%limb %hoon-version])))
  ?>  |(=(nex hoon-version) =(+(nex) hoon-version))
  ::  if we're upgrading language versions, recompile the compiler
  ::
  ::    hot: raw compiler formula
  ::
  =>  ?:  =(nex hoon-version)
        [hot=`*`raw .]
      ~&  [%hoon-compile-upgrade nex]
      =/  hot  (slum cop [%noun hun])
      .(cop .*(0 +.hot))
  ::  extract the hoon core from the outer gate (+ride)
  ::
  =/  hoc  .*(cop [%0 7])
  ::  compute the type of the hoon.hoon core
  ::
  =/  hyp  -:(slum cop [-.hot '+>'])
  ::  compile arvo
  ::
  =/  rav
    ~&  [%arvo-compile `@p`(mug hyp) `@p`(mug van)]
    (slum cop [hyp van])
  ::  activate arvo, and extract the arvo core from the outer gate
  ::
  =/  voc  .*(hoc [%7 +.rav %0 7])
  ::  entry gate: ++load for the normal case, ++come for upgrade
  ::
  =/  gat
    =/  arm  ?:(=(nex hoon-version) 'load' 'come')
    ::  compute the type of the arvo.hoon core
    ::
    =/  vip  -:(slum cop [-.rav '+>'])
    ::  compute the formula for the upgrade gate
    ::
    =/  fol  +:(slum cop [vip arm])
    ::  produce the upgrade gate
    ::
    .*(voc fol)
  ::  upgrade gate sample
  ::
  =/  sam
    :*  our
        now
        eny
        ::  tack a notification onto the pending effects
        ::
        (weld ova [`ovum`[/ %vega ~] ~])
        bud
        (turn vanes |=([label=@tas =vane] [label vase.vane]))
    ==
  ::  call into the new kernel
  ::
  =/  out  (slum gat sam)
  ::  add types to the product
  ::
  [((list ovum) -.out) +.out]
::  +veer: install %zuse or a vane
::
::    Identity is in the sample so the larval stage
::    can use this as well.
::
++  veer
  |=  [who=ship now=@da fav=curd]
  =>  .(fav ((hard {$veer lal/@ta pax/path txt/@t}) fav))
  =-  ?:(?=(%| -.res) ((slog p.res) +>.$) p.res)
  ^=  res  %-  mule  |.
  ?:  =(%$ lal.fav)
    ~&  [%tang pax.fav `@p`(mug txt.fav)]
    =+  gen=(rain pax.fav txt.fav)
    =+  vax=(slap pit gen)
    +>.^$(bud vax)
  %_    +>.^$
      vanes
    |-  ^+  vanes
    ?~  vanes
      ~&  [%vane `@tas`lal.fav pax.fav `@p`(mug txt.fav)]
      =+  vin=(vint who lal.fav vil bud pax.fav txt.fav)
      ?~  vin
        vanes
      [[lal.fav vane:u.vin] vanes]
    ?.  =(lal.fav label.i.vanes)
      [i.vanes $(vanes t.vanes)]
    ~&  [%vane `@tas`lal.fav pax.fav `@p`(mug txt.fav)]
    :_  t.vanes
    :-  label.i.vanes
    ~|  [%failed-vane-activation now lal.fav]
    vane:(ruck:(vent who lal.fav vil bud [vase.vane.i.vanes *worm]) pax.fav txt.fav)
  ==
::
++  wish                                                ::  external compute
  |=  txt/@
  q:(slap bud (ream txt))
--
