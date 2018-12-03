::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    Postface                              ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
~>  %slog.[0 leaf+"arvo-boot"]
=<  |=  {now/@da ovo/*}
    ^-  *
    ~>  %slog.[0 leaf+"arvo-event"]
    .(+> +:(poke now ovo))
~>  %slog.[0 leaf+"%arvo-assembly"]
=-  ~>  %slog.[0 leaf+"%arvo-assembled"]
    -
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
++  cage  (cask vase)                                   ::  global metadata
++  cask  |*(a/mold (pair mark a))                      ::  global data
++  cuff                                                ::  permissions
          $:  p/(unit (set monk))                       ::  can be read by
              q/(set monk)                              ::  caused or created by
          ==                                            ::
++  curd  {p/@tas q/*}                                  ::  typeless card
++  duct  (list wire)                                   ::  causal history
++  hypo  |*(a/mold (pair type a))                      ::  type associated
++  hobo  |*  a/mold                                    ::  kiss wrapper
          $?  $%  {$soft p/*}                           ::
              ==                                        ::
              a                                         ::
          ==                                            ::
++  kirk  (unit (set monk))                             ::  audience
++  lens                                                ::  observation core
  $_  ^?                                                ::
  |%  ++  u  *(unit (unit $~))                          ::  existence
      ++  v  *(unit (unit cage))                        ::  full history
      ++  w  *(unit (unit (unit cage)))                 ::  latest diff
      ++  x  *(unit (unit cage))                        ::  data at path
      ++  y  *(unit (unit arch))                        ::  directory
      ++  z  *(unit (unit cage))                        ::  current subtree
  --                                                    ::
++  marc                                                ::  structured mark
  $@  mark                                              ::  plain mark
  $%  {$tabl p/(list (pair marc marc))}                 ::  map
  ==                                                    ::
++  mark  @tas                                          ::  content type
++  mash  |=(* (mass +<))                               ::  producing mass
++  mass  $~  [%$ [%& ~]]                               ::  memory usage  
          (pair cord (each noun (list mash)))           ::
++  mill  (each vase milt)                              ::  vase+metavase
++  milt  {p/* q/*}                                     ::  metavase
++  monk  (each ship {p/@tas q/@ta})                    ::  general identity
++  muse  {p/@tas q/duct r/arvo}                        ::  sourced move
++  move  {p/duct q/arvo}                               ::  arvo move
++  ovum  {p/wire q/curd}                               ::  typeless ovum
++  pane  (list {p/@tas q/vase})                        ::  kernel modules
++  pone  (list {p/@tas q/vise})                        ::  kernel modules old
+$  scry-sample
  [fur=(unit (set monk)) ren=@tas why=shop syd=desk lot=coin tyl=path]
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
  !:
  |=  sod/slyd
  ^-  slyt
  |=  {ref/* raw/*}
  =+  pux=((soft path) raw)
  ?~  pux  ~
  ?.  ?=({@ @ @ @ *} u.pux)  ~
  =+  :*  hyr=(slay i.u.pux)
          fal=(slay i.t.u.pux)
          dyc=(slay i.t.t.u.pux)
          ved=(slay i.t.t.t.u.pux)
          tyl=t.t.t.t.u.pux
      ==
  ?.  ?=({$~ $$ $tas @} hyr)  ~
  ?.  ?=({$~ $$ $p @} fal)  ~
  ?.  ?=({$~ $$ $tas @} dyc)  ~
  ?.  ?=(^ ved)  ~
  =+  ron=q.p.u.hyr
  =+  bed=[[q.p.u.fal q.p.u.dyc (case p.u.ved)] (flop tyl)]
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
  |=  [lal=@tas vil=vile bud=vase =vane]
  ~%  %vent  +>+  ~
  |%
  ++  ruck                                              ::  update vase
    |=  {pax/path txt/@ta}
    ^+  +>
    =-  ?:(?=(%| -.res) ((slog p.res) +>.$) p.res)
    ^=  res  %-  mule  |.
    =+  arg=[~2000.1.1 0 =>(~ |~(* ~))]
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
        (~(slym wa *worm) vase.vane *[@da @ slyd])
      ::  cache the access of the %scry arm
      ::
      =^  fun  worm.vane  (~(slap wa worm.vane) rig [%limb %scry])
      ::  cache the call to +mint that the +slym in +scry will do
      ::
      +:(~(mint wa worm.vane) p.fun [%limb %$])
    ==
  ::
  ++  wink                                              ::  deploy
    |=  {now/@da eny/@ ski/slyd}
    =^  rig  worm.vane  (~(slym wa worm.vane) vase.vane +<)  ::  activate vane
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
      =^  sam  worm.vane  (~(slot wa worm.vane) 6 gat)
      =^  hig  worm.vane
        ?-  -.hil
          %&  (~(nest wa worm.vane) p.sam p.p.hil)
          %|  (~(nets wa worm.vane) p.sam p.p.hil)
        ==
      ?.  hig
        ~
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
      =+  pro=-:(~(slym wa worm.vane) fun old)
      ?~  q.pro  ~
      ?~  +.q.pro  [~ ~]
      =/  dat  +>.q.pro
      [~ ~ (mark -.dat) +.dat]
    ::
    ++  soar                                            ::  scrub vane
      |=  sev/vase
      ^-  vase
      ?:  &(=(-.q.vase.vane -.q.sev) =(+>.q.vase.vane +>.q.sev))
        vase.vane                                           ::  unchanged, use old
      sev(+<.q [*@da *@ =>(~ |~(* ~))])                 ::  clear to stop leak
    ::
    ++  swim
      ~/  %swim
      |=  $:  org/@tas
              pux/(unit wire)
              hen/duct
              hil/mill
          ==
      ^-  [(list move) _vane]
      ::  ~&  [%swim-wyt `@ud`~(wyt in worm.vane)]
      =+  ^=  pru
          ?~  pux
            ~|  [%swim-call-vane lal ({term $~} +.p.hil)]
            =^  vax  worm.vane  (~(slap wa worm.vane) rig [%limb %call])
            %^  slur-pro  lal  vax
            (slid [%& duc.vil hen] (slix hil))
          ~|  [%swim-take-vane lal ({term $~} +.p.hil)]
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
  |=  {lal/@tas vil/vile bud/vase pax/path txt/@ta}     ::
  =-  ?:(?=(%| -.res) ((slog p.res) ~) (some p.res))
  ^=  res  %-  mule  |.
  =+  gen=(rain pax txt)
  ~&  [%vane-parsed `@p`(mug gen)]
  =+  pro=(vent lal vil bud [(slym (slap bud gen) bud) *worm])
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
  |=  {vil/vile eny/@ bud/vase vanes=(list [label=@tas =vane])}
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
    %-  scry:(wink:(vent lal vil bud vane.i.vanes) now (shax now) ..^$)
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
    ~?  &(!lac !=(%belt -.q.ovo))  [%unix -.q.ovo p.ovo]
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
        ==
    ==
  ::
  ++  race                                              ::  take
    |=  {org/@tas lal/@tas pux/(unit wire) hen/duct hil/mill =vane}
    ^-  [p=(list move) q=_vane]
    =+  ven=(vent lal vil bud vane)
    =+  win=(wink:ven now (shax now) beck)
    (swim:win org pux hen hil)
  ::
  ++  fire                                              ::  execute
    |=  {org/term lal/term pux/(unit wire) hen/duct hil/mill}
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
    [[~ (turn p.fiq |=(a/move [lal a]))] [[label.i.naf q.fiq] t.naf]]
  ::
  ++  jack                                              ::  dispatch card
    |=  {lac/? gum/muse}
    ^-  {{p/(list ovum) q/(list muse)} _vanes}
    ::  =.  lac  |(lac ?=(?(%g %f) p.gum))
    ::  =.  lac  &(lac !?=($b p.gum))
    %+  fire
      p.gum
    ?-    -.r.gum
        $pass
      ~?  &(!lac !=(%$ p.gum))
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
        [%give p.gum (symp +>-.p.r.gum) `duct`q.gum]
      [i.i.q.gum [~ t.i.q.gum] t.q.gum p.r.gum]
    ::
        $slip
      ~?  !lac  [%slip p.gum (symp +>-.q.p.r.gum) q.gum]
      [p.p.r.gum ~ q.gum q.p.r.gum]
    ==
  ::
  ++  kick                                              ::  new main loop
    |=  {lac/? mor/(list muse)}
    =|  ova/(list ovum)
    |-  ^-  {p/(list ovum) q=(list [label=@tas =vane])}
    ?~  mor  [(flop ova) vanes]
    =^  nyx  vanes  (jack lac i.mor)
    $(ova (weld p.nyx ova), mor (weld q.nyx t.mor))
  --
--
=+  pit=`vase`!>(.)                                     ::
=+  bud=pit                                             ::  becomes tang
::  =+  vil=(viol p.bud)                                ::  cached reflexives
=+  vil=(viol p.bud)                                    ::  cached reflexives
=|  $:  lac/?                                           ::  laconic bit
        eny/@                                           ::  entropy
        vanes=(list [label=@tas =vane])                 ::  modules
    ==                                                  ::
=<  |%
    ++  come  |=  {@ (list ovum) pone}                  ::  22
              ^-  {(list ovum) _+>}
              ~&  %hoon-come
              =^  rey  +>+  (^come +<)
              [rey +>.$]
    ++  load  |=  {@ (list ovum) pane}                  ::  46
              ^-  {(list ovum) _+>}
              ~&  %hoon-load
              =^  rey  +>+  (^load +<)
              [rey +>.$]
    ++  peek  |=(* (^peek ((hard {@da path}) +<)))      ::  47
    ++  poke  |=  *                                     ::  10
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
                %+  fall
                  (vega now t.ova (path +<.q.i.ova) (path +>.q.i.ova))
                [~ +>.^$]
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
                      ::  hoon-cache+`p.niz
                      q.q.i.ova
                      dot+`.
                  ==
                ==
              =+(avo=$(ova t.ova) [[i.ova -.avo] +.avo])
    ++  wish  |=(* (^wish ((hard @ta) +<)))             ::  4
    --
|%
++  come                                                ::  load incompatible
  |=  {yen/@ ova/(list ovum) nyf/pone}
  ^+  [ova +>]
  (load yen ova (turn nyf |=({a/@tas b/vise} [a (slim b)])))
::
++  load                                                ::  load compatible
  |=  {yen/@ ova/(list ovum) nyf/pane}
  ^+  [ova +>]
  =:  eny  yen
      vanes  (turn nyf |=({a/@tas b/vise} [a [b *worm]]))
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
  ?~  hap  [~ hoon-version]
  =+  rob=((sloy ~(beck (is vil eny bud vanes) now)) [151 %noun] hap)
  ?~  rob  ~
  ?~  u.rob  ~
  [~ u.u.rob]
::
++  poke                                                ::  external apply
  |=  {now/@da ovo/ovum}
  =.  eny  (mix eny (shaz now))
  ::  ~&  [%poke -.q.ovo]
  ^-  {(list ovum) _+>}
  =^  zef  vanes
    (~(hurl (is vil eny bud vanes) now) lac ovo)
  [zef +>.$]
::                                                
++  veke                                                ::  build new kernel
  |=  {now/@da hap/path zup/path}
  ^-  *
  =-  ?:(?=(%& -.res) p.res ((slog p.res) ~))
  ^=  res  %-  mule  |.
  =/  pax  (weld hap /hoon)
  =/  wax  (weld zup /hoon)
  ~&  [%vega-start-hoon hap]
  =/  src  ((hard @t) (need (peek now cx+pax)))
  =/  arv  ((hard @t) (need (peek now cx+wax)))
  ::  construct  =>(hoon =>(+7 arvo))
  ::
  =/  gen=hoon
    :+  %tsbn  (rain hap src)
    :+  %tsld  (rain zup arv)
    [%$ 7]
  ~&  %vega-parsed
  =/  fol  q:(~(mint ut %noun) %noun gen)
  ~&  %vega-compiled
  ::  evaluate :fol to produce the Arvo gate,
  ::  then produce the Arvo core at +7
  ::
  .*(0 [%7 fol %0 7])
::
++  vega                                                ::  reboot kernel
  |=  {now/@da ova/(list ovum) hap/path zup/path}
  ^-  (unit {p/(list ovum) q/*})
  =-  ?:(?=(%| -.res) ((slog p.res) ~) `p.res)
  ^=  res  %-  mule  |.
  =/  ken  (veke now hap zup)
  ~&  [%vega-kernel `@ux`(mug ken)] 
  =/  nex
    ::  call +peek at +47
    ::
    %-  need
    %-  (hard (unit @))
    .*(ken [%9 2 %10 [6 %1 now ~] [%9 47 %0 1]])
  ~&  [%vega-compiled hoon-version nex]
  ?>  (lte nex hoon-version)
  ::  entropy, pending effects, vanes
  ::
  =/  sam
    :+  eny  ova
    (turn vanes |=([label=@tas =vane] [label vase.vane]))
  ::  +load at +46 or +come at +22
  ::
  =/  axe  ?:(=(nex hoon-version) 46 22)
  =/  out
    .*(ken [%9 2 %10 [6 %1 sam] [%9 axe %0 1]])
  ::  add a reset notification to the pending effects
  ::
  [[[~ %vega hap] ((list ovum) -.out)] +.out]
::
++  veer                                                ::  install vane/tang
  |=  {now/@da fav/curd}
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
      =+  vin=(vint lal.fav vil bud pax.fav txt.fav)
      ?~  vin
        vanes
      [[lal.fav vane:u.vin] vanes]
    ?.  =(lal.fav label.i.vanes)
      [i.vanes $(vanes t.vanes)]
      ~&  [%vane `@tas`lal.fav pax.fav `@p`(mug txt.fav)]
    :_  t.vanes
    :-  label.i.vanes
    vane:(ruck:(vent lal.fav vil bud [vase.vane.i.vanes *worm]) pax.fav txt.fav)
  ==
::
++  wish                                                ::  external compute
 |=  txt/@
  q:(slap bud (ream txt))
--

