::  ::  %eyre, http servant
!?  164
::::
|=  pit/vase
=,  eyre
=,  wired
=,  unity
=,  format
=,  mimes:html
=,  html
=>  =~
|%                                                      ::  interfaces
++  move  {p/duct q/(wind note gift:able)}              ::  local move
++  note                                                ::  out request $->
          $%  $:  $a                                    ::  to %ames
          $%  {$want p/ship q/{path *}}                 ::
          ==  ==                                        ::
              $:  $b                                    ::  to  %behn
          $%  {$wait p/@da}                             ::
              {$rest p/@da}                             ::
          ==  ==                                        ::
              $:  $d                                    ::  to %dill
          $%  {$flog p/{$crud p/@tas q/(list tank)}}    ::
          ==  ==                                        ::
              $:  $e                                    ::  to self
          $%  {$thud ~}                                ::  proxied death
              {$this p/? q/clip r/httq}                 ::  proxied request
              {$meta vase}                              ::  type check
          ==  ==                                        ::
              $:  $f                                            ::
          $%  [%build live=? schematic=schematic:ford]
              [%kill ~]
          ==  ==
              $:  $g                                    ::  to %gall
          $%  {$deal p/sock q/cush:gall}               ::  full transmission
          ==  ==                                        ::
              $:  %j                                    ::  to %jael
          $%  [%turf ~]                                 ::  view domains
          ==  ==  ==                                    ::
++  sign                                                ::  in result $<-
          $%  $:  $a                                    ::  by %ames
          $%  {$woot p/ship q/coop}                     ::  acknowledgment
          ==  ==                                        ::
              $:  $b                                    ::  by %behn
          $%  {$wake ~}                                ::  timer activate
          ==  ==                                        ::
              $:  $g                                    ::  by %gall
          $%  {$unto p/cuft:gall}                      ::  within agent
          ==  ==                                        ::
              $:  $e                                    ::  by self
          $%  {$thou p/httr}                            ::  response for proxy
          ==  ==                                        ::
              $:  $f
          $%  [%made date=@da result=made-result:ford]  ::
          ==  ==
              $:  %j                                    ::  from %jael
          $%  [%turf turf=(list turf)]                  ::  bind to domains
          ==  ==                                        ::
              $:  @tas                                  ::  by any
          $%  {$crud p/@tas q/(list tank)}              ::
          ==  ==  ==                                    ::
++  ixor  @t                                            ::  oryx hash
++  whir  $@  ~                                        ::  wire subset
          $%  {$ac p/hole q/whir}                       ::  cookied
              {$at p/hole q/whir}                       ::  authenticated
              {$ay p/knot q/knot ~}     :: /ay/@p/@uvH ::  remote duct
              {$ha p/path}               :: /ha/[beak]  ::  GET request
              {$he p/whir}                              ::  HEAD request
              {$hi p/knot q/mark ~}                    ::  outbound HTTP
              {$se p/whir-se q/{user (list @t)}}        ::  outbound to domain
              {$si ~}                                  ::  response done
              {$of p/ixor q/$@(~ whir-of)}             ::  associated view
              {$ow p/ixor ~}                           ::  dying view
              {$on ~}                                  ::  dependency
          ==                                            ::
++  whir-of  {p/@taxp q/term r/?($mess $lens) s/wire}   ::  path in dock
++  whir-se  ?($core vi-arm)                            ::  build/call
++  vi-arm
  $?  $filter-request                                   ::  ++out mod request
      $filter-response                                  ::  ++res use result
      $receive-auth-response                            ::  ++bak auth response
      $receive-auth-query-string                        ::  ++in handle code
      $out
      $res
      $bak
      $in
  ==                                                    ::
--                                                      ::
|%                                                      ::  models
++  bolo                                                ::  eyre state
  $:  $1                                                ::  version
      dom/(set (list @t))                               ::  domain names
      fig/http-config                                   ::  config
      por/{clr/@ud sek/(unit @ud)}                      ::  live ports
      wel/wank                                          ::  .well-known
      gub/@t                                            ::  random identity
      top/beam                                          ::  ford serve prefix
      ged/duct                                          ::  client interface
      ded/(set duct)                                    ::  killed requests
      lyv/(map duct live)                               ::  living requests
      pox/(map @uvH duct)                               ::  proxied sessions
      ask/{p/@ud q/(map @ud {p/duct q/hiss})}           ::  outgoing by number
      kes/(map duct @ud)                                ::  outgoing by duct
      ney/@uvI                                          ::  rolling entropy
      dop/(map host ship)                               ::  host aliasing
      liz/(jug @uvH (each duct ixor))                   ::  ford depsets
      wup/(map hole cyst)                               ::  secure sessions
      sop/(map hole {ship ?})                           ::  foreign sess names
      wix/(map ixor stem)                               ::  open views
      sec/(map {user (list @t)} driv)                   ::  security drivers
  ==                                                    ::
::
++  driv                                                ::  driver state
  %+  pair  (unit $@(~ vase))                          ::  main core
  {liv/? req/(qeu (trel duct mark vase:hiss))}          ::  waiting requests
::
++  live                                                ::  in flight
  $%  {$exec p/whir}                                    ::  ford build
      {$wasp p/(list @uvH)}                             ::  ford deps
      {$xeno p/ship}                                    ::  proxied request
      {$poll p/ixor}                                    ::  session state
  ==
++  cyst                                                ::  client session
  $:  ced/cred                                          ::  credential
      {him/ship aut/(set ship)}                         ::  authenticated
      cug/(list @t)                                     ::  unacked cookies
      lax/@da                                           ::  last used
      way/(map ship {purl duct})                        ::  waiting auth
      vew/(set oryx)                                    ::  open views XX expire
  ==                                                    ::
::
++  stem                                                ::  client view
  $:  him/ship                                          ::  static identity
      ude/(unit {p/duct q/?})                           ::  stream long-poll?
      era/@da                                           ::  next wake
      die/@da                                           ::  collection date
      sus/(set {dock $json wire path})                  ::  subscriptions
      eve/{p/@u q/(map @u even)}                        ::  queued events
      med/(qeu duct)                                    ::  waiting /~/to/
  ==
++  even                                                ::  client event
  $%  {$news p/@uvH}
      {$quit p/{dock path}}
      {$rush p/{dock path} q/json}
  ==
::
++  perk                                                ::  parsed request
  $%  {$auth p/perk-auth}
      {$away ~}
      {$oath p/knot q/(list @t)}
      {$bugs p/?($as $to) ~}
      {$beam p/beam}
      {$deps p/?($put $delt) q/@uvH}
      {$mess p/dock q/mark r/wire s/json}
      {$poll p/{i/@uvH t/(list @uvH)}}
      {$spur p/spur}
      {$subs p/?($put $delt) q/{dock $json wire path}}
      {$view p/ixor q/{~ u/@ud}}
      ::{$view p/ixor q/{~ u/@ud} r/(unit @dr)}
  ==
::
++  perk-auth                                           ::  parsed auth
  $%  {$at p/pork}                                      ::  inject auth
      {$del p/(unit ship)}
      {$get him/ship rem/pork}
      {$js ~}
      {$json ~}
      {$try him/ship paz/(unit cord)}
      {$xen ses/hole rem/pork}
  ==
::
++  pest                                                ::  result
  $@  ~
  $%  {$$ p/httr}                                       ::  direct response
      {$red ~}                                         ::  parent redirect
      {$bake p/whir q/mark r/coin s/beam}               ::  ford request
      {$js p/@t}                                        ::  script
      {$json p/json}                                    ::  data
      {$html p/manx}                                    ::  successful page
      {$htme p/manx}                                    ::  authentication fail
  ==
::
++  wank                                                ::  .well-known ankh
  $~  [~ ~]
  {p/(unit mime) q/(map @ta wank)}
--                                                      ::
|%
++  eat-headers
  |=  hed/(list {p/@t q/@t})  ^-  math
  %+  roll  hed
  |=  {a/{p/cord q/cord} b/math}
  =.  p.a  (crip (cass (trip p.a)))
  (~(add ja b) p.a q.a)
::
++  fcgi                                                ::  credential caboose
  |=  {quy/quay ced/cred}  ^-  coin
  :+  %many
    [%blob ced]
  |-  ^-  (list coin)
  ?~  quy  [%$ %n ~]~
  [[%$ %t p.i.quy] [%$ %t q.i.quy] $(quy t.quy)]
::
++  gsig  |=({a/dock b/?($mess $lens) c/path} [(scot %p p.a) q.a b c])
++  session-from-cookies
  |=  {nam/@t maf/math}
  ^-  (unit hole)
  (from-cookies maf |=({k/@t v/@} &(=(nam k) !=('~' v))))
::
++  ship-from-cookies
  |=  maf/math  ^-  (unit ship)
  (biff (from-cookies maf |=({k/@ @} =(%ship k))) (slat %p))
::
++  from-cookies
  |=  {maf/math fil/$-({@t @t} ?)}
  =+  `cot/(list @t)`(~(get ju maf) 'cookie')
  =+  `cok/quay`(zing `(list quay)`(murn cot (curr rush cock:de-purl)))
  |-  ^-  (unit cord)
  ?~  cok  ~
  ?:((fil i.cok) [~ q.i.cok] $(cok t.cok))
::
++  pack                                                ::  light path encoding
  |=  {a/term b/path}  ^-  knot
  %+  rap  3  :-  (wack a)
  (turn b |=(c/knot (cat 3 '_' (wack c))))
::
++  pick                                                ::  light path decoding
  =+  fel=(most cab (sear wick urt:ab))
  |=(a/knot `(unit {p/term q/path})`(rush a fel))
::
++  wush
  |=  {wid/@u tan/tang}
  ^-  wall
  (zing (turn tan |=(a/tank (wash 0^wid a))))
::
++  yank                                                ::  get .well-known
  |=  [wel=wank pat=path]
  ^-  (unit mime)
  ?~  pat  p.wel
  =/  wan  (~(get by q.wel) i.pat)
  ?~  wan  ~
  $(wel u.wan, pat t.pat)
::
++  dank                                                ::  put/del .well-known
  |=  [wel=wank pat=path mim=(unit mime)]
  ^-  wank
  ?~  pat  wel(p mim)
  =/  wan  (~(get by q.wel) i.pat)
  ?:  &(?=(~ wan) ?=(~ mim))
    wel
  :-  p.wel
  %+  ~(put by q.wel)
    i.pat
  $(wel ?~(wan *wank u.wan), pat t.pat)
::
++  add-cookies
  |=  {cug/(list @t) hit/httr}  ^-  httr
  ?~  cug  hit
  =+  cuh=(turn `(list @t)`cug |=(a/@t set-cookie+a))
  hit(q (weld cuh q.hit))
::
++  add-json                                            ::  inject window.urb
  |=  {urb/json jaz/cord}  ^-  cord
  =-  (cat 3 (crip -) jaz)
  """
  var _urb = {(en-json urb)};
  window.urb = window.urb || \{}; for(k in _urb) window.urb[k] = _urb[k];

  """
::
++  ares-to-json
  |=  err/ares  ^-  json
  =-  (pairs:enjs fail+s+typ mess+(tape:enjs mez) ~)
  ^-  {typ/term mez/tape}
  ?~  err  [%fail "Unknown Error"]
  [p.u.err (of-wall (wush 160 q.u.err))]
::
++  resp                                                ::  mimed response
  |=  {sas/@uG mit/mite rez/@}  ^-  httr
  ::  (weld (turn cug |=(a=@t ['set-cookie' a]))
  [sas ~[content-type+(en-mite mit)] [~ (as-octs rez)]]
::
++  favi                                                ::  XX favicon
  0w3.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~LX-.~~HW-.L~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.Rdjk~.VWuDL.-3wUf.~zEWe.~Yj4N.f~Y~f.P~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~-~LX.~~lBp.m~~nR.Zv~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.RZvn~.GqCF~.Qt7h~.Ya2wH.~0000.~M000.fY000.
  3~0w8.2~Qx8.if~eP.IX~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~fP.Y~QB9.ivY00.03~k5.1g~Z~.vT~~~.~~~~~.~~~~~.~~~~~.FWuD~.
  CpCp~.P8OcL.Y0003.~0000.~M000.fY000.3~000.0~M00.0fY00.03~00.00~Nk.l5v-W.KHH~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~-QJ.bj~00.
  00~M0.00fY0.003~6.hAp~S.FGqL-.6xEr~.oC9y~.NUu7L.Y0003.~0000.~M000.fY000.3~000.
  0~M00.0fY00.03~00.00~M0.00fY0.003~0.000~N.sn5~~.fPY~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~Z7.hQvYr.6NL~0.000~M.000fY.0003~.0000~.
  M000f.Y0003.~0000.~M000.fY000.3~000.0~M00.0fYJb.iT~sT.dP~Vu.nB~ZZ.vnT~a.iAF~M.
  000fY.0003~.0000~.VGqCL.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~Y.D9OvY.B9in~.0000~.M000f.Y0003.~0000.~M000.fY000.3~000.0~M41.
  0vZ1g.k7~Ha.OI~~n.RZv~~.~~~~~.~~~~~.~~~~~.HW-L~.jAVe~.M000f.YNcj7.~YLbO.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.-byUL.
  ZzoSf.~3MYf.~M000.fY000.3~000.0~MQd.3vZik.Bb~Kb.yU~~P.Y~f~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~IXeP.~ezEW.~WGGG.L~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~pSt.D~DFW.u~Uu7.x~-tD.
  pT~RZ.vn~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~IXe.
  P~-LH.W~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~0000.00000.00000.00000.00000.50000.00002.000g0.00400.000w0.000a0.00000.
  00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.
  00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.
  00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.
  00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.
  00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.00000.
  00000.00000.3~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.Rdjk~.~bOYL.~~~~~.~~~~~.~~TZ~.v-ZLr.T~r6N.I~Rtn.l~-rC.
  VL~-~.LX~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.ZLrS~.OMIbf.Z2gAb.~JHqS.~V-vD.~Y-fz.
  X~000.0~M00.0fY00.03~00.00~S1.wof~U.-fz~~.~~~~~.~~~~~.~~~~~.~~~~~.~DV-v.ZDpSv.
  ~0000.~M000.fY000.3~000.0~Qp6.hL-FG.qD~LX.-~~Qt.7h~Yw.823~Y.LbO~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~LX-.~WCFG.vZtnl.T~rmR.J~Yf3.M~~~~.~~~~~.~~~~~.~~~~J.XuT~N.
  Yv7~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.~~~~~.
  ~~~~~.~~~~~.~0000.00000.00000.00000.00000.1g000.00002.000g0.00200.000g0.000a0.
  001kU.001gE.02000.g0082.00000.C0005.a00w0.04001.0g008.00g00
++  js                                                  ::  static javascript
  |%
  ++  poll                                              :: dependency long-poll
    '''
    urb.tries = 0
    urb.call = function() {
      urb.wreq = new XMLHttpRequest()
      urb.wreq.open('GET', "/~/on.json?"+urb.deps.join('&'), true)
      urb.wreq.addEventListener('load', function() {
        // if(~~(this.status / 100) == 4)
        //   return document.write(this.responseText)
        if(this.status === 200) {
          var dep = JSON.parse(this.responseText)
          urb.onupdate(dep)
          urb.dewasp(dep)
        }
        urb.keep()
      })
      urb.wreq.addEventListener('error', urb.keep)
      urb.wreq.addEventListener('abort', urb.keep)
      urb.wreq.send()
    }
    urb.keep = function() {
      setTimeout(urb.call,1000*urb.tries)
      urb.tries++
    }
    urb.onupdate = function(){document.location.reload()}
    urb.call()
    urb.wasp = function(deh){
      if (!deh) return;
      if (urb.deps.indexOf(deh) !== -1) return;
      urb.deps.push(deh)
      urb.wreq.abort() // trigger keep
    }
    urb.dewasp = function(deh){
      var index = urb.deps.indexOf(deh)
      if (-1 !== index) {
        urb.deps.splice(index,1)
        urb.wreq.abort() // trigger keep
      }
    }

    '''
  ::
  ++  auth-redir
    'document.location.pathname = "/~~"+document.location.pathname'
  ::
  ++  auth
    '''
    var req = function(url,dat,cb){
      var xhr = new XMLHttpRequest()
      xhr.open('POST', url, true)
      dat.oryx = urb.oryx
      xhr.send(JSON.stringify(dat))
      xhr.addEventListener('load', function(ev){
        if(this.status !== 200)
          return err.innerHTML = ":( " + Date.now() + "\n" + xhr.responseText
        else if(cb) return cb(xhr.responseText,ev)
      })
    }

    urb.foreign = /^\/~\/am/.test(window.location.pathname)
    urb.redirTo = function(url){
      document.title = "Redirecting"
      var mount = document.getElementById("pass") || document.body
      mount.outerHTML = "Redirecting to <a href=\""+url+"\">"+url+"</a>"
      document.location = url
    }
    urb.redir = function(ship){
      if(ship){
        var location = new URL(document.location)
        location.pathname = location.pathname.replace(/^\/~~|\/~\/as\/any/,'/~/as/~'+ship)
        urb.redirTo(location)
      }
      else urb.redirTo(
        document.location.hash.match(/#[^?]+/)[0].slice(1) +
        document.location.pathname.replace(
          /^\/~\/am\/[^/]+/,
          '/~/as/~' + urb.ship) +
        document.location.search
      )
    }
    if(urb.foreign && urb.auth.indexOf(urb.ship) !== -1){
      req("/~/auth.json?PUT",
          {ship:urb.ship,code:null},
          function(){urb.redir()})
    }
    urb.is_me = function(ship) {
      return (urb.ship === ship)
    }
    urb.submit = function(ship,pass){
      if(!urb.is_me(ship))
        return urb.redir(ship)
      req(
        "/~/auth.json?PUT",
        {ship:ship, code:pass},
        function(){
          if(urb.foreign) urb.redir()
          else document.location.reload()
      })
    }
    urb.away = function(){req("/~/auth.json?DELETE", {},
      function(){document.body.innerHTML = "" }
    )}
    '''
  --
++  xml
  |%
  ++  exit
    ;html
      ;head:title:"Accepted"
      ;body:"You may now close this window."
    ==
  ::
  ++  redir
    |=  url/tape
    ;html
      ;head:title:"Redirecting..."
      ;body
        ;p: Redirecting to ;{a/"{url}" "{url}"}
        ;script: setTimeout(function()\{document.location = {(en-json (tape:enjs url))}}, 3000)
      ==
    ==
  ::
  ++  login-page
    %+  titl  'Sign in - Urbit'
    ;=  ;div.container.top
          ;div.row
            ;div.col-md-4
              ;h1.sign: Sign in
            ==
            ;div.col-md-8
              ;p.ship
                ;label.sig: ~
                ;input#ship.mono(contenteditable "", placeholder "your-urbit");
              ==
              ;input#pass.mono(type "password", placeholder "passcode");
              ;h2.advice: Type +{;code:("+code")} in your dojo for your passcode.
              ;pre:code#err;
            ==
          ==
        ==
        ;script@"/~/at/~/auth.js";
        ;script:'''
                $(function() {
                  $ship = $('#ship')
                  $pass = $('#pass')
                  $ship.on('keydown', function(e) {
                    if(e.keyCode === 13 || e.keyCode === 9) {
                      if(!urb.is_me($ship.val().toLowerCase()))
                        urb.redir($ship.val().toLowerCase())
                      $pass.show()
                      $pass.focus()
                      e.preventDefault()
                    }
                  })
                  $ship.on('focus', function(e) {
                    $pass.hide()
                  })
                  $pass.on('keydown', function(e) {
                    if(e.keyCode === 13) {
                      urb.submit($ship.val().toLowerCase(),$pass.val())
                    }
                  })
                  if(window.ship) {
                    $ship.val(urb.ship)
                    $pass.focus()
                  } else {
                    $pass.hide()
                  }
                })
                '''
    ==
  ::
  ++  logout-page
    %+  titl  'Log out'
    ;=  ;div.container.top
          ;div.row
            ;div.col-md-10
              ;h1.sign: Bye!
              ;button#act(onclick "urb.away()"): Log out
              ;pre:code#err;
              ;script@"/~/at/~/auth.js";
            ==
          ==
        ==
      ==
  ::
  ++  poke-test
    %+  titl  'Poke'
    ;=  ;button(onclick "urb.testPoke('/~/to/hood/helm-hi.json')"): Hi anonymous
        ;button(onclick "urb.testPoke('/~/as/own/~/to/hood/helm-hi.json')"): Hi
        ;pre:code#err;
        ;script@"/~/at/~/auth.js";
        ;script:'''
                show = function(t){err.innerText = ":) " + Date.now() + "\n" + t}
                urb.testPoke = function(url){
                  req(url,{wire:"/",xyro:'test'}, show)
                }
                '''
    ==
  ++  titl
    |=  {a/cord b/marl}
    ;html
      ;head
        ;meta(charset "utf-8");
        ;meta(name "viewport", content "width=device-width, ".
        "height=device-height, initial-scale=1.0, user-scalable=0, ".
        "minimum-scale=1.0, maximum-scale=1.0");
        ;title:"{(trip a)}"
        ;script(type "text/javascript", src "//cdnjs.cloudflare.com/ajax/".
          "libs/jquery/2.1.1/jquery.min.js");
        ;link(rel "stylesheet", href "/===/web/lib/css/fonts.css");
        ;link(rel "stylesheet", href "/===/web/lib/css/bootstrap.css");
      ==
      ;body:"*{b}"
    ==
  --
--
|%                                                      ::  functions
++  ye                                                  ::  per event
  =|  $:  $:  hen/duct                                  ::  event floor
              $:  now/@da                               ::  event date
                  eny/@                                 ::  unique entropy
                  our/ship                              ::  current ship
                  sky/$-({* *} (unit))                  ::  system namespace
              ==                                        ::
              mow/(list move)                           ::  pending actions
          ==                                            ::
          bolo                                          ::  all vane state
      ==                                                ::
  =*  bol  ->
  ~%  %eyre-y  ..is  ~
  |%
  ++  abet                                              ::  resolve moves
    ^-  {(list move) bolo}
    [(flop mow) bol]
  ::
  ++  adit  .(ney (mix eny ney))                        ::  flip entropy
  ::
  ++  anon  `@p`(add our ^~((bex 64)))                  ::  pseudo-sub
  ++  apex                                              ::  accept request
    |=  kyz/task:able
    ^+  +>
    =.  p.top  our              ::  XX necessary?
    ?-    -.kyz
        ::  new unix process - learn of first boot or a restart.
        ::
        $born
      ::  save outgoing duct
      ::
      =.  ged  hen
      ::  give %form to start servers
      ::
      =.  mow  :_(mow [hen [%give %form fig]])
      ::  kill active incoming requests
      ::
      =.  +>.$
        =/  fok=(list duct)  ~(tap in ~(key by lyv))
        |-  ^+   +>.^$
        ?~  fok  +>.^$
        %=  $
          fok    t.fok
          +>.^$  ^$(hen i.fok, kyz [%thud ~])
        ==
      ::  kill active outgoing requests
      ::
      =.  +>.$
        =/  fok=(list duct)  (turn ~(val by q.ask) head)
        |-  ^+   +>.^$
        ?~  fok  +>.^$
        %=  $
          fok    t.fok
          +>.^$  ^$(hen i.fok, kyz [%them ~])
        ==
      ::  if galaxy, request %ames domain from %jael
      ::
      ::    %jael response handled in $turf branch of +axon.
      ::
      =?  mow  ?=(%czar (clan:title our))  :_(mow [hen %pass / %j %turf ~])
      ::  learn externally known domain names
      ::
      ::    If any are new, request certificate. XX currently unused. remove?
      ::    XX %born card also includes locally-known IPs. use for DNS?
      ::
      ::
      =/  mod/(set turf)
        %-  ~(gas in *(set turf))
        %+  turn
          (skim p.kyz |=(a=host ?=(%& -.a)))
        |=(a=host ?>(?=(%& -.a) p.a))
      =/  dif/(set (list @t))  (~(dif in mod) dom)
      =?  dom  ?=(^ dif)  (~(uni in dom) mod)
      =?  mow  ?=(^ dif)
        =/  cmd  [%acme %poke `cage`[%acme-order !>(dom)]]
        :_(mow [hen %pass /acme/order %g %deal [our our] cmd])
      +>.$
    ::
        $live
      +>.$(clr.por p.kyz, sek.por q.kyz)
    ::
        $rule
      ?-  -.p.kyz
          $cert
        ?:  =(secure.fig p.p.kyz)  +>.$
        =.  secure.fig  p.p.kyz
        +>.$(mow :_(mow [ged [%give %form fig]]))
      ::
          $turf
        =/  mod/(set turf)
          ?:  ?=(%put p.p.kyz)
            (~(put in dom) q.p.kyz)
          (~(del in dom) q.p.kyz)
        ?:  =(dom mod)  +>.$
        =/  cmd  [%acme %poke `cage`[%acme-order !>(mod)]]
        %=  +>.$
          dom  mod
          mow  :_(mow [hen %pass /acme/order %g %deal [our our] cmd])
        ==
      ==
    ::
        $serv
      =<  ~&([%serving (en-beam top)] .)
      ?^(p.kyz +>.$(top p.kyz) +>.$(q.top p.kyz))
    ::
        $crud
      +>.$(mow [[hen %slip %d %flog kyz] mow])
    ::
        $init                                           ::  register ownership
      %=  +>.$
        fig  [~ ?=(%king (clan:title our)) & &]
        top  [[our %home ud+0] /web]
      ==
    ::
        $sunk  +>
    ::
        ?($chis $this)                                  ::  inbound request
      %-  emule  |.  ^+  ..apex
      =*  sec  p.kyz    ::  ?                           ::  https bit
      =*  heq  r.kyz    ::  httq                        ::  request content
      =/  ryp=quri
        =+  (rush q.heq zest:de-purl)
        ?^(- u.- ~|(eyre-parse-url-failed+q.heq !!))
      =+  maf=(eat-headers r.heq)
      =+  ^=  pul  ^-  purl
          ?-  -.ryp
            %&  ?>(=(sec p.p.p.ryp) p.ryp)
            %|  =+  hot=(~(get ja maf) %host)
                ?>  ?=({@ ~} hot)
                [[sec (rash i.hot thor:de-purl)] p.ryp q.ryp]
          ==
      =.  p.p.pul  |(p.p.pul ?=(hoke r.p.pul))
      ?:  ?=($chis -.kyz)                               :: IPC escape hatch
        ~(lens handle pul [q.+.kyz |] [p.heq maf s.heq])
      =+  her=(host-to-ship r.p.pul)
      ?:  |(?=(~ her) =(our u.her))
        ~(apex handle pul [q.+.kyz |] [p.heq maf s.heq])
      =+  han=(sham hen)
      =.  pox  (~(put by pox) han hen)
      (ames-gram u.her %get han +.kyz)
    ::
        $them                                           ::  outbound request
      ?~  p.kyz
        ?~  sud=(~(get by kes) hen)
          ::  delete an element from q.ask by traversing to prevent leakage
          ::
          =.  q.ask
            =/  qas  ~(tap by q.ask)
            |-  ^+  q.ask
            ?~  qas
              q.ask
            ?:  =(hen p.q.i.qas)
              (~(del by q.ask) p.i.qas)
            $(qas t.qas)
          ::
          +>.$
        ::
        =.  +>.$
          %_  +>.$
            mow    :_(mow [ged [%give %thus u.sud ~]])
            q.ask  (~(del by q.ask) u.sud)
            kes    (~(del by kes) hen)
          ==
        ::
        =/  driver=(unit [key=[user (list @t)] val=driv])
          =/  drivers  ~(tap by sec)
          |-  ^-  (unit [key=[user (list @t)] val=driv])
          ?~  drivers  ~
          ::
          ?~  q=req.q.q.i.drivers
            $(drivers t.drivers)
          ::
          ?~  tip=~(top to q)
            $(drivers t.drivers)
          ::
          ?:  =(hen p.u.tip)
            `i.drivers
          $(drivers t.drivers)
        ::
        ?^  driver
          ~(cancel-request vi u.driver)
        (give-sigh(hen (tail hen)) %| [leaf+"canceled on %born"]~)
      ::  ~&  eyre-them+(en-purl p.u.p.kyz)
      %=  +>.$
        mow    :_(mow [ged [%give %thus p.ask p.kyz]])
        p.ask  +(p.ask)
        q.ask  (~(put by q.ask) p.ask hen u.p.kyz)
        kes    (~(put by kes) hen p.ask)
      ==
    ::
        $hiss                                           ::  outbound cage
      ::?~  p.kyz                                       ::  XX cancel
      ::  =+  sud=(need (~(get by kes) hen))
      ::  %=  +>.$
      ::    mow    :_(mow [ged [%give %thus sud ~]])
      ::    q.ask  (~(del by q.ask) sud)
      ::    kes    (~(del by kes) hen)
      ::  ==
      ::  ~&  eyre-them+(en-purl p.u.p.kyz)
      =+  usr=?~(p.kyz '~' (scot %ta u.p.kyz))
      (back-turbo hi+/[usr]/[q.kyz] %hiss r.kyz)
    ::
        $they                                           ::  inbound response
      =+  kas=(need (~(get by q.ask) p.kyz))
      ::  ~&  >  eyre-they+[p.q.kyz (en-purl p.q.kas)]
      %=  +>.$
        mow    :_(mow [p.kas [%give %thou q.kyz]])
        q.ask  (~(del by q.ask) p.kas)
      ==
    ::
        $thud                                           ::  cancel request
      ?.  (~(has by lyv) hen)
        ~&  dead-request+hen
        +>.$(ded (~(put in ded) hen))                   ::  uncaught requests
      =+  lid=(~(got by lyv) hen)
      =.  lyv  (~(del by lyv) hen)
      :: ~&  did-thud+[-.lid hen]
      ?-  -.lid
          $exec
        (pass-note p.lid %f %kill ~)
      ::
          $poll
        ?.  (~(has by wix) p.lid)
         +>.$
        poll-dead:(ire-ix p.lid)
      ::
          $xeno
        =+  han=(sham hen)
        =.  pox  (~(del by pox) han hen)
        (ames-gram p.lid %gib han)
      ::
          $wasp
        |-  ^+  +>.^$
        ?~  p.lid  +>.^$
        (del-deps:$(p.lid t.p.lid) i.p.lid %& hen)
      ==
    ::
        $well
      +>.$(wel (dank wel p.kyz q.kyz))
    ::
        $west                                           ::  remote request
      =.  mow  :_(mow [hen %give %mack ~])
      ::  TODO: make +gram and %west play nicely together
      ::
      ::    %west passes paths around, where all the paths are actually single
      ::    tags. The old version of +gram used [%tag ~] as tag types in its
      ::    definition, which no longer works.
      ::
      ::    Actually fixing this is a cross-ames-eyre surgery, for now hack it
      ::    by molding the incoming thing into a gram shape before we try to
      ::    soft it.
      ::
      =*  him  p.kyz
      ?~  -.q.kyz
        ~&  e+[%strange-west-wire him]
        ~!(%strange-west-wire !!)
      =+  mez=((soft gram) [i.-.q.kyz +.q.kyz])
      ?~  mez
        ~&  e+[%strange-west him]
        ~|(%strange-west !!)
      ?-  -.u.mez
        $aut  abet:(logon:(ses-ya p.u.mez) him)
        $hat  (foreign-hat:(ses-ya p.u.mez) him q.u.mez)
        $gib  (pass-note ay+(dray p+uv+~ him p.u.mez) [%e %thud ~])
        $get  (pass-note ay+(dray p+uv+~ him p.u.mez) [%e %this q.u.mez])
        $got
          ?.  (~(has by pox) p.u.mez)
            ~&  lost-gram-thou+him^p.u.mez
            +>.$
          =:  hen  (~(got by pox) p.u.mez)
              pox  (~(del by pox) p.u.mez)
            ==
          (give-thou q.u.mez)
      ::
        $lon
          ::  ~&  ses-ask+[p.u.mez sop (~(run by wup) ~)]
          ?:  (ses-authed p.u.mez)
            (ames-gram him %aut p.u.mez)
          =.  sop  (~(put by sop) p.u.mez him |)
          (ames-gram him %hat p.u.mez our-host)
      ::
        $get-inner
          %+  exec-turbo-live  ay+(dray p+uv+~ him p.u.mez)
          [%bake q.u.mez r.u.mez [[p q] s]:s.u.mez]
      ::
        $got-inner
          ?.  (~(has by pox) p.u.mez)
            ~&  lost-gram-inner+him^p.u.mez
            +>.$
          =:  hen  (~(got by pox) p.u.mez)
              pox  (~(del by pox) p.u.mez)
            ==
          ?-    -.q.u.mez
              %|
            =/  dep  0v0  ::XX remote dependency?
            (fail 500 dep p.q.u.mez)
          ::
              %&
            =/  res/(cask)  p.q.u.mez
            =/  dep  0v0  ::XX remote dependency?
            =/  bek  -.top  ::XX where is wrapper-renderer beak stored exactly
            :: XX store request mark
            =/  ext  (end 3 (sub (met 3 p.res) (met 3 '-elem')) p.res) :: %x-urb-elem -> %x-urb
            =+  norm=(norm-beak bek)
            ::
            %+  exec-turbo-live  ha+(en-beam bek ~)
            :^  %cast  [p q]:norm  ext
            :+  %call
              [%core [p q]:norm /hoon/wrap/[ext]/ren]
            [%vale [p q]:norm res]
          ==
      ::
        $not  +>.$(mow :_(mow [ged [%give %that him p.u.mez]]))
      ==
    ::
      $wegh  !!                                         ::  handled elsewhere
    ::
      $wise  (ames-gram p.kyz %not q.kyz)           ::  proxy notification
    ==
  ::
  ::++  axom                                              ::  old response
  ::  |=  [tee=whir hon=honk]
  ::  ^+  +>
  ::  ?+  tee  !!
  ::    ~          ?-(-.hon %nice (nice-json), %mean (mean-json 500 p.hon))
  ::    [%of @ ^]  (get-ack:(ire-ix p.tee) q.tee hon)
  ::  ==
  ++  axon                                              ::  accept response
    |=  {tee/whir sih/sign}
    ^+  +>
    ?:  &(?=({?($of $ow) ^} tee) !(~(has by wix) p.tee))
      ~&(dead-ire+[`whir`tee] +>)
    ?-    &2.sih
        $crud  +>.$(mow [[hen %slip %d %flog +.sih] mow])
    ::  $dumb
    ::    =.  +>  ?+(tee +> [%of ^] pop-duct:(ire-ix p.tee))
    ::    (emule |.(~|(gall-dumb+tee !!)))
    ::
        $woot  +>.$
    ::
        $thou
      ?+    -.tee  !!
        $ay  (ames-gram (slav %p p.tee) %got (slav %uv q.tee) |2.sih)
        $hi  (cast-thou q.tee httr+!>(p.sih))
        $se  (get-thou:(dom-vi q.tee) p.tee p.sih)
      ==
    ::
        $turf
      ?.  ?=(%czar (clan:title our))
        ~&  %strange-turf
        +>.$
      =/  mod/(set turf)
        %-  ~(gas in dom)
        %+  turn  turf.sih
        |=(a=turf (weld a /(crip +:(scow %p our))))
      ?:  =(dom mod)
        +>.$
      =/  cmd  [%acme %poke `cage`[%acme-order !>(mod)]]
      %=  +>.$
        dom  mod
        mow  :_(mow [hen %pass /acme/order %g %deal [our our] cmd])
      ==
    ::
        $unto                                           ::  app response
      ?>  ?=({$of @ ^} tee)
      =+  cuf=`cuft:gall`+>.sih
      ?-    -.cuf
          ?($coup $reap)
        ::  ~?  ?=($lens r.q.tee)  hen=hen^hcuf=-.cuf
        (get-ack:(ire-ix p.tee) q.tee ?~(p.cuf ~ `[-.cuf u.p.cuf]))
      ::
          $diff
        ?.  ?=($json p.p.cuf)
          :: ~>  %slog.`%*(. >[%backing p.p.cuf %q-p-cuf]< &3.+> (sell q.p.cuf))
          (back-turbo tee %json p.cuf)
        (get-rush:(ire-ix p.tee) q.tee ((hard json) q.q.p.cuf))
      ::
          $quit  (get-quit:(ire-ix p.tee) q.tee)
      ==
    ::
        $wake
      ?>  ?=({?($of $ow) @ ~} tee)
      ?:  ?=($ow -.tee)
        abut:(ire-ix p.tee)
      =>  wake:(ire-ix p.tee)
      (give-json 200 ~ (frond:enjs %beat %b &))
    ::
        $made
      =|  ses/(unit hole)
      |-  ^+  ..axon
      ?+    tee  ~&  [%tee tee]  !!
          {$si $~}  (give-turbo-sigh result.sih)
          {$se ^}   (get-made:(dom-vi q.tee) p.tee result:sih)
      ::
          {$ay ^}
        =/  res/(each (cask) tang)
          ?:  ?=(%incomplete -.result.sih)
            [%| tang.result.sih]
          ?:  ?=([%complete %error *] result.sih)
            [%| message.build-result.result.sih]
          [%& [p q.q]:(result-to-cage:ford build-result.result.sih)]
        (ames-gram (slav %p p.tee) %got-inner (slav %uv q.tee) res)
      ::
          {$ha *}
        %-  emule  |.  ^+  ..apex
        ?:  ?=([%incomplete *] result.sih)
          (fail-turbo 404 tang.result.sih)
        ?:  ?=([%complete %error *] result.sih)
          (fail-turbo 404 message.build-result.result.sih)
        =/  cay=cage  (result-to-cage:ford build-result.result.sih)
        ?:  ?=($red-quri p.cay)
          =+  url=(apex:en-purl ((hard quri) q.q.cay))
          (give-thou 307 [location+(crip url)]~ ~)
          :: (give-html:abet 200 ~ (redir:xml url))
        ?.  ?=($mime p.cay)
          =+  bek=(norm-beak -:(need (de-beam p.tee)))
          =+  tee-ses=?~(ses tee [%ac u.ses tee])
          ::  TODO: Why cast here? Shouldn't the cast wrap the previous result?
          ::
          (exec-turbo-live tee-ses [%cast [p q]:bek %mime [%$ cay]])
        =+  cug=?~(ses ~ cug:(~(got by wup) u.ses))
        =+  ((hard {mit/mite rez/octs}) q.q.cay)
        ::  TODO: This used to use dep for etag control.
        ::
        ::  =+  dep=(crip "W/{(en-json %s (scot %uv p.sih))}")
        =+  hit=[200 ~[content-type+(en-mite mit)] ~ rez]  :: etag+dep
        (give-thou (add-cookies cug hit))
      ::
          {$hi ^}
        ?.  ?=([%complete %success *] result.sih)
          (give-turbo-sigh result.sih)
        ::
        =/  cay/cage  (result-to-cage:ford build-result.result.sih)
        ?>  ?=($hiss p.cay)
        ?:  =('~' p.tee)
          (eyre-them tee q.cay)
        =+  usr=(slav %ta p.tee)
        =+  ((hard {pul/purl ^}) q.q.cay)
        ?.  ?=(%& -.r.p.pul)
          ~&  [%auth-lost usr (head:en-purl p.pul)]
          (eyre-them tee q.cay)
        (get-req:(dom-vi usr (scag 2 p.r.p.pul)) q.tee q.cay)
      ::
          {$of @ ^}
        ?:  ?=([%incomplete *] result.sih)
          ((slog tang.result.sih) +>.^$)
        ?:  ?=([%complete %error *] result.sih)
          ((slog message.build-result.result.sih) +>.^$)
        =/  cay=cage  (result-to-cage:ford build-result.result.sih)
        %+  get-rush:(ire-ix p.tee)  q.tee
        ?>  ?=($json p.cay)                    ::  XX others
        ((hard json) q.q.cay)
      ==
    ==
  ::
  ++  norm-beak  |=(bek/beak ?+(r.bek bek {$ud $0} bek(r da+now)))
  ++  emule
    |=  a/_|?(..emule)  ^+  ..emule
    ?:  [unsafe=|]
      (a)
    =+  mul=(mule a)
    ?~  -.mul  p.mul
    (fail 500 0v0 >%exit< p.mul)
  ::
  ++  ire-ix  |=(ire/ixor ~(. ix ire (~(got by wix) ire)))
  ++  dom-vi
    |=  {usr/knot dom/path}  ^+  vi    :: XX default to initialized user?
    ~(. vi [usr dom] (fall (~(get by sec) usr dom) *driv))
  ::
  ++  ses-authed
    |=  ses/hole
    =+  sap=(~(get by sop) ses)
    ?:  ?=({~ @ %&} sap)  &
    =+  cyz=(~(get by wup) ses)
    ?~  cyz  |
    (~(has in aut.u.cyz) our)
  ::
  ++  ses-ya  |=(ses/hole ~(. ya ses (~(got by wup) ses)))
  ++  our-host
    ^-  hart
    ::  XX get actual -F flag value
    ::  XX scry into %jael?
    ::
    ?:  [fake=|]  [| [~ 8.443] &+/localhost]
    `hart`[& ~ %& /org/urbit/(rsh 3 1 (scot %p our))]
  ::
  ++  eyre-them
    |=  {tea/whir vax/vase}
    (pass-note tea [%e %meta :(slop !>(%them) !>(~) vax)])
  ::
  ++  ames-gram
    |=  [him=ship gam=gram]
    ::  TODO: To make this work
    ::
    ~!  -.gam
    (pass-note ~ %a %want him [%e -.gam ~] +.gam)
  ::
  ++  back-turbo
    |=  [tea=whir mar=mark cay=cage]
    =/  disc  [p q]:(norm-beak -.top)
    %^  execute-turbo  tea  live=%.n
    ^-  schematic:ford
    [%cast disc mar [%$ cay]]
  ::
  ++  cast-thou  :: turbo
    |=  [mar=mark cay=cage]
    ?:  ?=($httr mar)
      (give-sigh %& cay)
    %^  execute-turbo  si+~  live=%.n
    =/  =beak  (norm-beak -.top)
    [%alts [%cast [p q]:beak mar $+cay] [%cast [p q]:beak %recoverable-error $+cay] ~]
  ::
  ++  del-deps
    |=  {a/@uvH b/(each duct ixor)}  ^+  +>.$
    ?:  =(`@`0 a)  +>.$
    =.  liz  (~(del ju liz) a b)
    :: ~&  del-deps+[a (~(get ju liz) a)]
    ?:  (~(has by liz) a)  +>.$
    =-  -(hen hen.+)
    ::  TODO: %wasp is no longer supported.
    ::
    ~&  %deprecated-del-deps
    +>.$
  ::
  ++  new-deps
    |=  {a/@uvH b/(each duct ixor)}  ^+  +>.$
    :: ~&  new-deps+[a b]
    ?:  =(`@`~ a)  +>.$
    =+  had=(~(has by liz) a)
    =.  liz  (~(put ju liz) a b)
    ?:  had  +>.$
    =-  -(hen hen.+)
    ::  TODO: %wasp is no longer supported.
    ::
    ~&  %deprecated-new-deps
    +>.$
  ::
  ++  exec-turbo-live
    |=  [tea=whir req=schematic:ford]
    =.  lyv  (~(put by lyv) hen [%exec tea])
    (execute-turbo tea live=%.n req)
  ::
  ++  execute-turbo
    ~/  %execute-turbo
    |=  [tea=whir live=? request=schematic:ford]
    %+  pass-note  tea
    :*  %f  %build  live
        [%dude |.([%leaf "eyre: execute {<tea>}"]) request]
    ==
  ::
  ++  add-links                                           :: x-urbit:// urls
    ~/  %add-links
    |=  a/wall  ^-  marl
    ?.  [x-urbit-links=&]  [;/((of-wall a))]~             :: default disable
    |-  ^-  marl
    ?~  a  ~
    =^  pax  i.a  ::  parse path if any
      ^-  {(unit path) tape}
      =/  vex  (fel:stab [1 1] i.a)
      ?~  q.vex  [~ i.a]
      [`p q.q]:u.q.vex
    ?~  pax  [;/("{i.a}\0a") $(a t.a)]
    :-  ;a/"x-urbit:{(spud u.pax)}":"{(spud u.pax)}"
    [;/("{i.a}\0a") $(a t.a)]
  ::
  ++  render-tang                                         ::  tanks to manx
    ~/  %render-tang
    |=  {dep/@uvH tan/tang}
    ;html
      ;head
        ;link(rel "stylesheet", href "/lib/base.css");
        ;title: server error
      ==
      ;body:div#c.err:pre:code:"*{(add-links (wush 80 tan))}"
      ;script@"/~/on/{<dep>}.js";
    ==
  ::
  ++  render-turbo-tang
    ~/  %render-turbo-tang
    |=  tan/tang
    ;html
      ;head
        ;link(rel "stylesheet", href "/lib/base.css");
        ;title: server error
      ==
      ;body:div#c.err:pre:code:"*{(add-links (wush 80 tan))}"
    ==
  ::
  ++  fail
    |=  {sas/@ud dep/@uvH mez/tang}
    ^+  +>
    :: (back ha+~ dep %tang !>(mez))  ::tang->urb chain may be source of failure
    (give-html sas ~ (render-tang dep mez))
  ::
  ++  fail-turbo                                        ::  failing faster
    |=  [sas=@ud mez=tang]
    ^+  +>
    :: (back ha+~ dep %tang !>(mez))  ::tang->urb chain may be source of failure
    (give-html sas ~ (render-turbo-tang mez))
  ::
  ++  give-html
    |=  {sas/@ud cug/(list @t) max/manx}
    %-  give-thou
    %+  add-cookies  cug
    (resp sas text+/html (crip (en-xml max)))
  ::
  ++  give-json
    |=  {sas/@uG cug/(list @t) jon/json}
    %-  give-thou
    %+  add-cookies  cug
    (resp sas application+/json (crip (en-json jon)))
  ::
  ++  give-thou                                       ::  done request
    |=  hit/httr
    ?:  (~(has in ded) hen)                           ::  request closed
      +>(ded (~(del in ded) hen))
    =.  lyv  (~(del by lyv) hen)
    +>(mow :_(mow [hen %give %thou hit]))
  ::
  ++  give-sigh                                       ::  userspace done
    |=  res/(each cage tang)
    =-  +>.$(mow :_(mow [hen %give %sigh `cage`-]))
    ?.  ?=(%| -.res)  p.res
    [%tang !>(p.res)]
  ::
  ++  give-turbo-sigh
    |=  result=made-result:ford
    =-  +>.$(mow :_(mow [hen %give %sigh `cage`-]))
    ?:  ?=(%incomplete -.result)
      [%tang !>(tang.result)]
    (result-to-cage:ford build-result.result)
  ::
  ++  mean-json  |=({sas/@uG err/ares} (give-json sas ~ (ares-to-json err)))
  ++  nice-json  |=(* (give-json 200 ~ (frond:enjs %ok %b &)))
  ::
  ++  pass-note  |=(noe/{whir note} +>(mow :_(mow [hen %pass noe])))
  ++  host-to-ship                                              ::  host to ship
    |=  hot/host
    ^-  (unit ship)
    :: =+  gow=(~(get by dop) hot)    ::  XX trust
    :: ?^  gow  gow
    ?.  ?=(%& -.hot)  ~
    =+  dom=(flop p.hot)                                ::  domain name
    ?~  dom  ~
    (rush i.dom fed:ag)
  ::
  ++  load-secret
    ^-  @ta
    =+  pax=/(scot %p our)/code/(scot %da now)/(scot %p our)
    %^  rsh  3  1
    (scot %p (@ (need (sky [151 %noun] %j pax))))
  ::
  ::
  ++  handle
    ~%  %eyre-h  ..is  ~
    |_  $:  {hat/hart pok/pork quy/quay}                ::  purl parsed url
            {cip/clip aut/?}                            ::  client ip nonymous?
            {mef/meth maf/math bod/(unit octs)}         ::  method+headers+body
        ==
    ++  abet  ..handle
    ++  done  .
    ++  teba  |*(a/$-(* _..handle) |*(b/* %_(done ..handle (a b))))
    ++  del-deps  (teba ^del-deps)
    ++  new-deps  (teba ^new-deps)
    ++  ames-gram  (teba ^ames-gram)
    ++  exec-turbo-live  (teba ^exec-turbo-live)
    ++  give-html  (teba ^give-html)
    ++  give-thou  (teba ^give-thou)
    ++  give-json  (teba ^give-json)
    ++  nice-json  (teba ^nice-json)
    ++  pass-note  (teba ^pass-note)
    ::
    ++  fcgi-cred
      ^-  cred
      ?:  aut  fcgi-cred:for-client
      %*(fcgi-cred for-client him anon)
    ::
    ++  apex
      =<  abet
      ^+  done
      =+  pez=process
      ?:  ?=(%| -.pez)  p.pez
      (resolve ~ p.pez)
    ::
    ++  lens
      =<  abet
      ::  (process-parsed [%mess [our %dojo] %lens-command /lens (need grab-json)])
      =^  orx  ..ya  new-view:(logon:for-client our)
      =+  vew=(ire-ix (oryx-to-ixor orx))
      ((teba new-lens.vew) grab-json)
    ::
    ++  resolve
      |=  {cug/(list @t) pez/pest}  ^+  done
      ?~  pez  done
      ?-  -.pez
          ~     (give-thou (add-cookies cug p.pez))
          $js    $(pez [%$ (resp 200 text+/javascript p.pez)])
          $json  (give-json 200 cug p.pez)
          $html  (give-html 200 cug p.pez)
          $htme  (give-html 401 cug p.pez)
          $bake
        %+  exec-turbo-live  p.pez
        ^-  schematic:ford
        :-  %alts  :~
          ^-  schematic:ford
          [%bake q.pez r.pez [[p q] s]:s.pez]
        ::
          ^-  schematic:ford
          [%bake %red-quri r.pez [[p q] s]:s.pez]
        ==
      ::
          $red
        =+  url=(en-purl hat pok(p [~ %html]) quy)
        ?+    p.pok  ~|(bad-redirect+[p.pok url] !!)
            ::  ignore css
            ::
            {~ $css}  !!
        ::
            {~ $js}
          $(pez [%js auth-redir:js])
        ::
            {~ $json}
          =/  red
            (pairs:enjs ok+b+| red+(tape:enjs url) ~)
          $(pez [%json red])
        ==
      ==
    ::
    ::
    ++  is-anon  =([~ ''] (~(get by (molt quy)) 'anon'))
    ++  check-oryx                    ::  | if json with bad oryx
      ^-  ?
      ?.  &(?=({~ $json} p.pok) ?=($post mef) ?=(^ bod) !is-anon)  &
      =+  oxe=grab-oryx
      ?~  oxe  |
      ?:  (~(has in vew.cyz:for-client) u.oxe)
        &
      ~&(bad-oryx+[u.oxe vew.cyz:for-client] &)         ::  XX security
    ::
    ++  grab-json
      ^-  json
      ?.  ?=(?($post $put $delt) mef)
        ~|(bad-method+mef !!)
      ?~  bod
        ~|(%no-body !!)
      (need (de-json q.u.bod))
    ::
    ++  grab-json-soft
      ^-  (unit json)
      ?.  ?=(?($post $put $delt) mef)
        ~
      ?~(bod ~ (de-json q.u.bod))
    ::
    ++  grab-oryx
      ^-  (unit oryx)
      =+  oxe=(biff grab-json-soft =>(dejs-soft (ot oryx+so ~)))
      ?^  oxe  oxe
      (~(get by (molt quy)) %oryx)
    ::
    ::
    ++  parse
      ^-  (each perk httr)
      |^  =+  hit=as-magic-filename
          ?^  hit  [%| u.hit]
          =+  hem=as-aux-request
          ?^  hem
            ?.  check-oryx
              ~|(%bad-oryx ~|([grab-oryx vew.cyz:for-client] !!))
            [%& u.hem]
          =+  bem=as-beam
          ?^  bem  [%& %beam u.bem]
          ?:  is-spur
            [%& %spur (flop q.pok)]
          ~|(strange-path+q.pok !!)
      ::
      ++  as-magic-filename
        ^-  (unit httr)
        ?+    [(fall p.pok %$) q.pok]  ~
            {?($ico $png) $favicon ~}
          :-  ~
          %^  resp  200  image+/png
          favi
        ::
            {$txt $robots ~}
          :-  ~
          %^  resp  200  text+/plain
          %-  of-wain:format
          :~  'User-agent: *'
              'Disallow: '
          ==
        ::
            {@tas $'.well-known' ^}  ::  XX file extension?
          =/  mim  (yank wel (tail q.pok))
          ?~  mim  ~
          `(resp 200 p.u.mim q.q.u.mim)
        ==
      ::
      ++  is-spur  |(?~(q.pok & ((sane %ta) i.q.pok)))
      ++  as-beam                                       :: /~sipnym/desk/3/...
        ^-  (unit beam)
        =+  =<  tyk=(drop-list (turn q.pok .))          :: a path whose elements
            |=(a/knot `(unit tyke)`(rush a gasp:vast))  :: are in /=foo==/=bar
        ?~  tyk  ~                                      :: syntax
        =+  %-  posh:(vang & (en-beam top))             :: that the base path
            [[~ (zing u.tyk)] ~]                        :: can interpolate into
        ?~  -  ~                                        ::
        =+  (plex:vast %clsg u)                         :: staticly, and make a
        (biff - de-beam)                                   :: valid beam
      ::
      ++  as-aux-request                                ::  /~/... req parser
        ^-  (unit perk)
        =.  mef
          ?.  ?=($post mef)  mef
          ?+    (skim quy |=({a/@t b/@t} &(=('' b) =(a (crip (cuss (trip a)))))))
              ~|(bad-quy+[req='"?PUT" or "?DELETE"' quy] !!)
            ~   mef
            {{$'DELETE' ~} ~}  %delt
            {{$'PUT' ~} ~}     %put
          ==
        |-
        ?:  ?=({$'~~' *} q.pok)                            ::  auth shortcuts
          $(q.pok ['~' %as %own t.q.pok])
        ?.  ?=({$'~' @ *} q.pok)  ~
        :-  ~  ^-  perk
        =*  pef  i.t.q.pok
        =+  but=t.t.q.pok                 ::  XX  =*
        ?+    pef  ~|(pfix-lost+`path`/~/[pef] !!)
            $debug  ((hard perk) [%bugs but])
            $away  [%away ~]
            $ac
          ?~  but  ~|(no-host+`path`/~/[pef] !!)
          =+  `dom/host`~|(bad-host+i.but (rash i.but thos:de-purl))
          ?:  ?=(%| -.dom)  ~|(auth-ip+dom !!)
          =-  [%oath - p.dom]
          ~|  bad-user+`path`t.but
          ?>  ?=({@ $in ~} t.but)
          =+  in-quy=(rush i.t.but ;~(pfix cab fque:de-purl))
          ?~  in-quy
            (slav %ta i.t.but)
          =+  src=~|(no+u.in-quy (~(got by (malt quy)) u.in-quy))
          p:(need (pick src))  ::  allow state=usr_other-data
        ::
            $at  [%auth %at pok(q but)]
            $am  ?~(but !! [%auth %xen i.but pok(q t.but)])
            $as
          :+  %auth  %get
          ~|  bad-ship+?~(but ~ i.but)
          ?~  but  !!
          :_  pok(q t.but)
          ?+  i.but  (slav %p i.but)
            $anon  anon
            $own   our
          ==
        ::
            $on
          :-  %poll
          ?^  but  [(raid but %uv ~)]~
          =+  dep=((hard (list {@ ~})) quy)
          =<  ?~(. !! .)
          (turn dep |=({a/@tas ~} (slav %uv a)))
        ::
            $of
          :+  %view  ?>(?=({@ ~} but) i.but)
          ?>  ?=({{$poll @} ~} quy)
::          :^  %view
::          ?>  ?=({@ ~} but)
::          i.but
::          ?>  ?=({{$poll @} *} quy)     ::  XX eventsource
          [~ (rash q.i.quy dem)]
::          ?:  ?=({{$t @} ~} +.quy)
::            =/  s  (rash q.i.t.quy dem)
::            `(yule [0 0 0 s ~])
::          ~
        ::
            $to
          =+  ^-  dir/{p/ship q/term r/mark}
              ~|  bad-mess+but
              ?+  but  !!
                {@ @ ~}    [our (raid but %tas %tas ~)]
                {@ @ @ ~}  (raid but %p %tas %tas ~)
              ==
          =;  x/{wir/wire mez/json}
            [%mess [p q]:dir r.dir wir.x mez.x]
          =+  wir=(~(get by (molt quy)) 'wire')
          ?^  wir  [(stab u.wir) grab-json]          ::  XX distinguish
          %.(grab-json =>(dejs (ot wire+(cu stab so) xyro+same ~)))
        ::
            $in
          ~|  expect+[%post 'application+json' /'@uv' '?PUT/DELETE']
          ?>  &(?=(?($delt $put) mef) ?=($@(~ {~ $json}) p.pok))
          [%deps mef (raid but %uv ~)]
        ::
            $is
          ?~  but
            ~|(no-app+but=but !!)
          |-  ^-  perk
          ?~  p.pok  $(p.pok [~ %json])
          ?.  ?=($json u.p.pok)
            ~|(is+stub+u.p.pok !!)      ::  XX marks
          ?:  ((sane %tas) i.but)
            $(but [(scot %p our) but])
          ?>  ?=(?($delt $put) mef)
          =+  :-  hap=[(slav %p i.but) (slav %tas -.t.but)]
              wir=%.(grab-json =>(dejs (ot wire+(cu stab so) ~)))
          [%subs mef hap u.p.pok wir +.t.but]
        ::
            $auth
          :-  %auth
          |-  ^-  perk-auth
          ?+    p.pok  !!
              ~         $(p.pok [~ %json])
              {~ $js}    [%js ~]
              {~ $json}
            ?+    mef  ~|(bad-meth+mef !!)
                $get   [%json ~]
                $put
              ~|  parsing+bod
              :-  %try
              %.(grab-json =>(dejs (ot ship+(su fed:ag) code+(mu so) ~)))
            ::
                $delt
              ::  XX: restored old code to fix for redo
              ::  ~|  parsing+bod
              ::  :-  %del
              ::  %.(grab-json =>(dejs-soft (ot ship+(su fed:ag))))
              ::
              ~|  parsing+bod
              =+  jon=(need (de-json q:(need bod)))
              ?>  ?=($o -.jon)
              =+  sip=(~(get by p.jon) %ship)
              [%del ?~(sip ~ [~ ((su:dejs fed:ag) u.sip)])]
        ==  ==
        ==
      --
    ::
    :: process-payload handles the translation of a payload for post.
    :: currently this involves treating the payload as a urlencoded
    :: request. In the future it's possible the payload could be
    :: a specific mark instead.
    ::
    ++  process-payload
      ^-  {quay meth}
      ?+  mef  [quy mef]
        $post  [`quay`(weld quy `quay`(rash q:(need bod) yquy:de-purl)) %get]
      ==
    ++  process
      ^-  (each pest _done)
      =+  pet=parse
      ?:  ?=(%| -.pet)
        [%& %$ p.pet]
      (process-parsed p.pet)
    ::
    ++  process-parsed
      |=  hem/perk  ^-  (each pest _done)
      ?-    -.hem
          $auth  (process-auth p.hem)
          $away  [%& %html logout-page:xml]
          ?($beam $spur)
        =^  payload  mef  process-payload
        =/  ext  %+  fall  p.pok
          ?:  ?=(%spur -.hem)
            %urb
          ?:  =(our p.p.hem)
            %urb
          %x-urb
        =+  bem=?-(-.hem $beam p.hem, $spur [-.top (weld p.hem s.top)])
        ~|  bad-beam+q.bem
        ?<  =([~ 0] (sky [151 %noun] %cw (en-beam bem(+ ~, r [%da now]))))
        ?:  ::!=(our p.bem) ::TODO also if it is?
            =('x-' (end 3 2 ext))
          =.  ext  (cat 3 ext '-elem')
          =.  -.bem  (norm-beak -.bem)
          =/  han  (sham hen)
          =.  pox  (~(put by pox) han hen)
          =+  arg=(fcgi payload fcgi-cred)
          [%| (ames-gram p.bem %get-inner han ext arg bem)]
        =+  wir=`whir`[%ha (en-beam -.bem ~)]
        =.  wir  ?+(mef !! $get wir, $head [%he wir])
        =.  r.bem  ?+(r.bem r.bem {$ud $0} da+now)
        =+  arg=(fcgi payload fcgi-cred)
        =+  [%bake wir ext arg bem]
        ?.(aut [%& `pest`-] [%| `_done`(resolve ~ -)])
      ::
          $bugs
        ?-  p.hem
          $as  (show-login-page)
          $to  [%& %html poke-test:xml]
        ==
      ::
          $deps
        =+  ire=need-ixor
        ?>  (~(has by wix) ire)  ::  XX made redundant by oryx checking
        =<  [%| (nice-json)]
        ?-  p.hem
          $put   (new-deps q.hem %| ire)
          $delt  (del-deps q.hem %| ire)
        ==
      ::
          $mess
        :-  %|
        =^  orx  ..ya  ?:(is-anon new-view:for-client [(need grab-oryx) ..ya])
        =+  vew=(ire-ix (oryx-to-ixor orx))
        ((teba new-mess.vew) p.hem r.hem q.hem %json !>(`json`s.hem))
      ::
          $oath
        ?.  (~(has by sec) [p q]:hem)
          ~|(no-driver+[p q]:hem !!)
        [%| %.(quy (teba get-quay:(dom-vi [p q]:hem)))]
      ::
          $poll
        ?:  ?=({~ $js} p.pok)  ::  XX treat non-json cases?
          =+  deps=[%a (turn `(list @uvH)`p.hem |=(a/@ s+(scot %uv a)))]
          [%& %js (add-json (frond:enjs %deps deps) poll:js)]
        =.  lyv  (~(put by lyv) hen %wasp p.hem)
        |-
          =.  done  (new-deps i.p.hem %& hen)
          ?~  t.p.hem  [%| done]
          $(p.hem t.p.hem)
      ::
          $subs
        ?-  p.hem
          $put   [%| ((teba add-subs:for-view) q.hem)]
          $delt  [%| ((teba del-subs:for-view) q.hem)]
        ==
      ::
          $view
        ~|  lost-ixor+p.hem
::        [%| ((teba poll:(ire-ix p.hem)) u.q.hem r.hem)]
        [%| ((teba poll:(ire-ix p.hem)) u.q.hem)]
      ==
    ::
    ++  process-auth
      |=  ham/perk-auth  ^-  (each pest _done)
      =+  yac=for-client
      ?-    -.ham
          $js    [%& %js auth:js]
          $json  =^  jon  ..ya  stat-json.yac
                 [%| (give-json 200 cug.yac jon)]
          $xen   (show-login-page ~ ses.ham)
      ::
          $at
        =.  ..ya  abet.yac
        =+  pez=process(pok p.ham, aut |)
        ?.  ?=(%& -.pez)  ~|(no-inject+p.ham !!)
        ?~  p.pez  pez
        ?+    -.p.pez  ~&(bad-inject+p.pez !!)
            $red  pez
            $bake
          =.  ya  abet.yac
          [%| (resolve ~ p.pez(p [%at ses.yac p.p.pez]))]
        ::
            $js
          =^  jon  ..ya  stat-json.yac
          [%| (resolve cug.yac p.pez(p (add-json jon p.p.pez)))]
        ==
      ::
          $del
        =.  ..ya  (logoff:yac p.ham)
        =/  cug
          :~  (set-cookie cookie-prefix '~')
              (set-cookie %ship '~')
          ==
        [%| (give-json 200 cug (frond:enjs %ok %b &))]
      ::
          $get
        |-
        ~|  aute+ham
        ?:  |(=(anon him.ham) (~(has in aut.yac) him.ham))
          =.  ..ya  abet.yac(him him.ham)
          =+  pez=process(pok rem.ham, aut &)
          ?:  ?=(%| -.pez)  pez
          [%| (resolve ~ p.pez)]
        ?.  =(our him.ham)
          [%| ((teba foreign-auth.yac) him.ham hat rem.ham quy)]
        (show-login-page ~)
      ::
          $try
        :-  %|
        ?.  =(our him.ham)
          ~|(stub-foreign+him.ham !!)
        ?.  ?|  (~(has in aut.yac) him.ham)
                ?~(paz.ham | =(u.paz.ham load-secret))
            ==
          ~|(%auth-fail !!)
        =^  jon  ..ya  stat-json:(logon:yac him.ham)
        =.  cug.yac  :_(cug.yac (set-cookie %ship (scot %p him.ham)))
        (give-json 200 cug.yac jon)
      ==
    ::
    ++  show-login-page
      |=  ses/(unit hole)  ^-  (each pest _done)
      ?.  ?=($@(~ {~ $html}) p.pok)
        [%& %red ~]
      ?~  ses
        [%& %htme login-page:xml]
      ?:  (~(has by wup) u.ses)
        [%& %htme login-page:xml]
      =+  yac=(new-ya u.ses)
      =+  =-  lon=?~(- | (~(has in aut.u.-) our))
          (biff (session-from-cookies cookie-prefix maf) ~(get by wup))
      =.  yac  ?.(lon yac (logon.yac our))
      [%| (give-html(..ya abet.yac) 401 cug.yac login-page:xml)]
    ::
    ++  cookie-prefix  (rsh 3 1 (scot %p our))
    ++  cookie-domain
      ^-  cord
      ?-  r.hat
        {%| @}  (cat 3 '; Domain=' (rsh 3 1 (scot %if p.r.hat)))
        {%& $org $urbit *}  '; Domain=.urbit.org'
        {%& @ @ *}  =-  (rap 3 "; Domain={-}{i.p.r.hat ~}")
                    (turn (flop `path`t.p.r.hat) |=(a/knot (cat 3 a '.')))

        {%& *}  ''  ::  XX security?
      ==
    ::
    ++  set-cookie
      |=  {key/@t val/@t}
      %+  rap  3  :~
        key  '='  val
        ::  '; HttpOnly'  ?.(sec '' '; Secure')  ::  XX security
        cookie-domain
        '; Path=/; HttpOnly'
      ==
    ++  need-ixor  (oryx-to-ixor (need grab-oryx))
    ++  for-view  ^+(ix (ire-ix need-ixor))
    ::
    ++  for-client                        ::  stateful per-session engine
      ^+  ya
      =+  pef=cookie-prefix
      =+  lig=(session-from-cookies pef maf)
      ?~  lig
        (new-ya (rsh 3 1 (scot %p (end 6 1 ney))))
      =+  cyz=(~(get by wup) u.lig)
      ?~  cyz
        ~&  bad-cookie+u.lig
        (new-ya (rsh 3 1 (scot %p (end 6 1 ney))))
      ~(. ya u.lig u.cyz(cug ~))
    ::
    ++  new-ya  |=(ses/hole ~(. ya ses (new-cyst ses)))
    ++  new-cyst
      |=  ses/hole
      =*  sec  p.hat
      ^-  cyst
      :*  ^-  cred
          :*  hat(p sec)
              ~
              'not-yet-implemented'
              ::(rsh 3 1 (scot %p (end 6 1 (shaf %oryx ses))))
          ::
              =+  lag=(~(get by maf) %accept-language)
              ?~(lag ~ ?~(u.lag ~ [~ i.u.lag]))
          ::
              cip
              ~
          ==
          [`@p`(mix anon (lsh 5 1 (rsh 5 1 (shaf %ship ses)))) ~]
      ::
          [(set-cookie cookie-prefix ses)]~
      ::
          now
          ~
          ~
        ::  [1 ~]
      ==
    --
  ::
  ++  oryx-to-ixor  |=(a/oryx (rsh 3 1 (scot %p (end 6 1 (shas %ire a)))))
  ++  ya                                                ::  session engine
    ~%  %eyre-y  ..is  ~
    =|  {ses/hole cyst}
    =*  cyz  ->
    |%
    ++  abet  ..ya(wup (~(put by wup) ses cyz))
    ++  abut  ..ya(wup (~(del by wup) ses))
    ++  foreign-auth
      |=  {him/ship pul/purl}  ^+  ..ya
      =.  way  (~(put by way) him pul hen)
      ~&  asking-foreign+him
      (ames-gram:abet him [%lon ses])
    ::
    ++  foreign-hat
      |=  {him/ship hat/hart}  ^+  ..ya
      ~|  way
      ?.  (~(has by way) him)  :: XX crashes should be handled by ames
        ~&(strange-auth+[way him hat] ..ya)
      =^  pul  hen  (~(got by way) him)
      =:  way       (~(del by way) him)
          dop       (~(put by dop) r.hat him)
          q.q.pul   ['~' %am ses q.q.pul]
        ==
      =+  url=(welp (en-purl pul(p hat)) '#' (head:en-purl p.pul))
      %-  give-thou:abet
      (add-cookies cug [307 [location+(crip url)]~ ~])
    ::
    ++  logon
      |=  her/ship
      %_  +>
        him   her
        aut   (~(put in aut) her)
        ..ya
          ::  ~&  logon+[our her ses]
          ?.  =(our her)
            ..ya
          =+  sap=(~(get by sop) ses)
          ::  ~&  sap+sap
          ?.  ?=({~ @ %|} sap)
            ..ya
          (ames-gram -.u.sap %aut ses)
      ==
    ++  logoff
      |=  her/(unit ship)  ^+  ..ya
      ?~  her  abut
      =.  aut  (~(del in aut) u.her)
      ?~  aut  abut
      abet(him ?.(=(u.her him) him n.aut))
    ::
    ++  new-view
      ^+  [*oryx ..ya]
      =+  orx=`@t`(rsh 3 1 (scot %p (shaf %orx eny)))
      =.  vew  (~(put in vew) orx)
      =+  ire=(oryx-to-ixor orx)
      =.  ..ix  ~(init ix ire %*(. *stem him him, p.eve 1))
      ::  ~&  stat-ire+`@t`ire
      [orx abet]
    ::
    ++  fcgi-cred  %_(ced aut (~(put ju aut.ced) %$ (scot %p him)))
    ::  XX move
    ::
    ++  sein
      |=  who=ship
      ;;  ship
      (need (sky [[151 %noun] %j (en-beam [our %sein da+now] /(scot %p who))]))
    ::
    ++  stat-json
      ^+  [*json ..ya]
      =^  orx  ..ya  new-view
      :_  ..ya
      =/  j  enjs
      %-  pairs.j  :~
        oryx+s+orx
        ixor+s+(oryx-to-ixor orx)
        sein+(ship.j (sein our))
        ship+(ship.j our)
        user+(ship.j him)
        auth+a+(turn ~(tap in aut) ship.j)
      ==
    --
  ::
  ++  ix
    ~%  %eyre-x  ..is  ~
    =|  {ire/ixor stem}
    =*  sem  ->
    |%
    ++  done  .
    ++  abet  ..ix(wix (~(put by wix) ire sem))
    ++  abut
      =+  sub=~(tap in sus)
      |-  ^+  ..ix
      ?^  sub  $(sub t.sub, ..ix (pul-subs i.sub))
      =.  +>  poll-rest
      ..ix(wix (~(del by wix) ire))
    ::
    ++  teba  |*(a/$-(* _..ix) |*(b/* %_(done ..ix (a b))))
    ++  give-json  (teba ^give-json)
    ++  pass-note  (teba ^pass-note)
    ++  hurl-note
      |=  {a/{dock ?($mess $lens) path} b/note}  ^+  ..ix
      =:  med  (~(put to med) hen)
          hen  `~
        ==
      :: ~&  >  hurl+[&2.b ire a]
      (pass-note:abet [%of ire (gsig a)] b)
    ::
    ++  init
      =.  die  (add ~h2 now)
      abet(mow :_(mow [`/ %pass ow+/[ire] [%b %wait die]]))
    ::
    ++  refresh
      =.  mow  :_(mow [`/ %pass ow+/[ire] [%b %rest die]])
      =.  die  (add ~h2 now)
      done(mow :_(mow [`/ %pass ow+/[ire] [%b %wait die]]))
    ::
    ++  add-even
      |=  a/even  ^+  eve
      [+(p.eve) (~(put by q.eve) p.eve a)]
    ::
    ++  new-lens
      |=  jon/json  ^+  ..ix
      =.  +>.$
        %+  pass-note
          [%of ire (gsig [our %dojo] lens+/)]
        [%g %deal [him our] %dojo %peel %lens-json /sole]
      =.  +>.$
        %+  pass-note
          [%of ire (gsig [our %dojo] lens+/)]
        [%g %deal [him our] %dojo %punk %lens-command %json !>(`json`jon)]
      abet
    ::
    ++  new-mess
      |=  {a/dock b/wire c/mark d/cage}  ^+  ..ix
      (hurl-note [a mess+b] [%g %deal [him -.a] +.a %punk c d])
    ::
    ++  add-subs
      |=  {a/dock $json b/wire c/path}  ^+  ..ix
      ?:  (~(has in sus) +<)  ~|(duplicate+c !!)
      =.  sus  (~(put in sus) +<)
      (hurl-note [a mess+b] [%g %deal [him -.a] +.a %peel %json c])
    ::
    ++  pul-subs
      |=  {a/dock $json b/wire c/path}  ^+  ..ix
      =.  sus  (~(del in sus) +<)
      (hurl-note [a mess+b] [%g %deal [him -.a] +.a %pull ~])
    ::
    ++  del-subs                      ::  XX per path?
      |=  {a/dock $json b/wire c/path}  ^+  ..ix
      =.  ..ix  (pul-subs +<)
      (nice-json:pop-duct:(ire-ix ire))          ::  XX gall ack
    ::
    ++  get-lens
      |=  {a/whir-of fec/json}  ^+  ..ix
      ?~  fec  ..ix                   ::  nulled event we don't care about
      =.  +>.$
        %+  pass-note
          `whir`[%of ire (gsig [our %dojo] lens+/)]
        `note`[%g %deal [him our] %dojo %pull ~]
      abet:(give-json 200 ~ fec)
    ::
    ++  get-rush
      |=  {a/whir-of b/json}  ^+  ..ix
      ?:  ?=($lens r.a)
        (get-lens a b)
      (get-even [%rush [[(slav %p p.a) q.a] s.a] (frond:enjs %json b)])
    ::
    ++  get-quit
      |=  a/whir-of  ^+  ..ix
      =/  doc=dock  [(slav %p p.a) q.a]
      =.  sus  (~(del in sus) [doc %json s.a s.a])
      (get-even [%quit [doc s.a]])
    ::
    ++  get-ack
      |=  {a/whir-of b/(unit {term tang})}  ^+  ..ix
      ?:  ?=($lens r.a)
        (ack-lens b)
      ?:  =(~ med)  ~&  resp-lost+ire  ..ix
      ?~  b  (nice-json:pop-duct)
      (mean-json:pop-duct 500 b)
    ::
    ++  ack-lens
      |=  a/(unit (pair term tang))  ^+  ..ix
      ?~  a
        ..ix  :: (give-json 200 ~ (joba %okey-dokey %b &))
      =+  tag=(flop `tang`[>[%eyre-lens-fail p.u.a]< q.u.a])
      %-  (slog tag)
      abet:(give-json 500 ~ (wall:enjs (wush 160 tag)))
    ::
    ++  get-even
      |=  ven/even  ^+  ..ix
      =+  num=p.eve
      =.  eve  (add-even ven)
      =<  abet
      ?~  ude  done
      =.  hen  p.u.ude
      (give-even:pass-rest(ude ~) q.u.ude num ven)
    ::
    ++  give-even
      |=  {pol/? num/@u ven/even}  ^+  done
      =:  q.eve  (~(del by q.eve) (dec num))              ::  TODO ponder a-2
          mow    ?.(?=($rush -.ven) mow mow:(pass-took [- %mess +]:p.ven))
        ==
      ?>  pol                         ::  XX eventstream
      %^  give-json  200  ~
      %^  pairs:enjs  id+(numb:enjs num)  type+[%s -.ven]
      ?-  -.ven
        $news  ~[from+[%s (scot %uv p.ven)]]
        $quit  ~[from+(subs-to-json p.ven)]
        $rush  ~[from+(subs-to-json p.ven) data+q.ven]
      ==
    ::
    ++  pass-wait  (pass-note of+/[ire] [%b %wait era])
    ++  pass-rest
      =.  lyv  (~(del by lyv) hen)
      (pass-note of+/[ire] [%b %rest era])
    ::
    ++  pass-took
      |=  a/{p/dock ?($mess $lens) wire}
      %+  pass-note(hen `~)
        [%of ire (gsig a)]
      [%g %deal [him -.p.a] +.p.a %pump ~]
    ::
    ++  pop-duct  =^(ned med ~(get to med) abet(hen ned))
    ++  poll
      |=  a/@u  ^+  ..ix
::      |=  [a/@u t=(unit @dr)]
::      ^+  ..ix
      =<  abet
      =.  ..poll  refresh
      ?:  =(a p.eve)
        =.  ..poll  poll-rest
        =.  era  (add ~s8 now)
::          ?~  t  (add ~s30 now)
::          (add u.t now)
        =.  lyv  (~(put by lyv) hen [%poll ire])
        pass-wait(ude [~ hen &])
      ?:  (gth a p.eve)  ~|(seq-high+cur=p.eve !!)
      =+  ven=~|(seq-low+cur=p.eve (~(got by q.eve) a))
      (give-even & a ven)
    ::
    ++  poll-rest
      ?~  ude  done
      %*(. pass-rest(hen p.u.ude) hen hen)
    ::
    ++  poll-dead
      ^+  ..ix
      =<  abet
      ?.  =(ude [~ hen &])
        done  :: old long poll
      pass-rest(ude ~)
    ::
    ++  subs-to-json
      |=  {a/dock b/path}
      %-  pairs:enjs  :~
        ship+[%s (rsh 3 1 (scot %p p.a))]
        appl+[%s q.a]
        path+(tape:enjs (spud b))
      ==
    ++  wake  ^+(..ix abet(ude ~))  ::  XX other effects?
    ::  XX unused
    ++  print-subs  |=({a/dock b/path} "{<p.a>}/{(trip q.a)}{(spud b)}")
    --
  ++  vi                                                ::  auth engine
    ~%  %eyre-v  ..is  ~
    |_  $:  {usr/user dom/path}
            cor/(unit $@(~ vase))
            {liv/? req/(qeu {p/duct q/mark r/vase:hiss})}
        ==
    ++  self  .
    ++  abet  +>(sec (~(put by sec) +<- +<+))
    ++  execute-turbo
      |=  [wir=whir-se live=? schematic=schematic:ford]
      (execute-turbo:abet se+[wir usr dom] live schematic)
    ++  dead-this  |=(a/tang (fail:abet 500 0v0 a))
    ++  dead-hiss  |=(a/tang pump(req ~(nap to req), ..vi (give-sigh %| a)))
    ::
    ++  eyre-them
      |=  [a=whir-se b=vase]
      ::  block requests until we get a response to this request
      ::
      =.  liv  |
      (eyre-them:abet se+[a usr dom] b)
    ::
    ++  pass-note  |=({a/whir-se b/note} (pass-note:abet se+[a usr dom] b))
    ::  XX block reqs until correct core checked in?
    ++  warn  |=(a/tang ((slog (flop a)) abet))
    ++  with  |*({a/vase b/$-(vase abet)} |=(c/vase (b (slam a c))))
    ++  root-beak  `beak`[our %home da+now]
    ::
    ::  Main
    ::
    ++  cor-type  ?~(cor %void ?~(u.cor %void p.u.cor))
    ++  has-arm  ~(has in (silt (sloe cor-type)))
    ++  build
      %^  execute-turbo  %core  live=%.y
      :::+  %dude  [|.(+)]:>%mod-samp<
      ^-  schematic:ford
      :+  %mute
        ^-  schematic:ford
        [%core [[our %home] [%hoon (flop %_(dom . sec+dom))]]]
      ^-  (list (pair wing schematic:ford))
      :*  [[%& 12]~ %$ bale+!>(*(bale @))]  :: XX specify on type?
          ?~  cor  ~
          ?~  u.cor  ~
          ?:  (has-arm %discard-state)  ~
          ?:  (has-arm %update)
            [[%& 13]~ ride+[limb+%update prep-cor]]~
          [[%& 13]~ %$ noun+(slot 13 u.cor)]~
      ==
    ::
    ++  call
      |=  {arm/vi-arm sam/cage}
      %^  execute-turbo  arm  live=%.n
      call+[ride+[limb+arm prep-cor] [%$ sam]]
    ::
    ++  prep-cor  ^-  schematic:ford
      ?~  cor  ~|(%no-core !!)
      ?~  u.cor  ~|(%nil-driver !!)
      :+  %$  %core
      %_    u.cor
          +12.q
        =+  ^=  ato
            %-  sky
            [[151 %noun] %cx (en-beam root-beak [%atom (flop %_(dom . sec+dom))])]
        =+  key=?~(ato '' ;;(@t u.ato))  ::  XX jael
        =.  key
          ?~  key  ''
          %-  (bond |.(~&(bad-key+[dom key] '')))
          =+  (slaw %uw key)
          ?~(- ~ (de:crub:crypto load-secret u))  :: XX clay permissions
        `(bale)`[[our now (shas %bale eny) root-beak] [usr dom] key]
      ==
    ::
    ++  pump
      ^+  abet
      ?~  cor
        build
      ?.  liv
        ~&  e+vi+pump-blocked+[dom ~(wyt in req)]
        abet
      =+  ole=~(top to req)
      ?~  ole  abet
      ::  process hiss
      =.  hen  p.u.ole
      ?~  u.cor  (eyre-them %filter-request r.u.ole)  :: don't process
      (call %filter-request hiss+r.u.ole)
    ::
    ++  fin-httr
      |=  vax/vase
      =^  ole  req  ~|  %eyre-no-queue  ~(get to req)
      =>  .(ole `{p/duct q/mark *}`ole)             :: XX types
      =.  ..vi  (cast-thou(hen p.ole) q.ole httr+vax)    :: error?
      pump
    ::
    ::  Interfaces
    ::
    ++  get-quay  |=(quy/quay (call %receive-auth-query-string quay+!>(quy)))
    ++  get-req   |=(a/{mark vase:hiss} pump(req (~(put to req) hen a)))
    ++  get-thou
      |=  {wir/whir-se hit/httr}
      =.  liv  &
      ?+  wir  !!
        ?($receive-auth-query-string $in)  (call %receive-auth-response httr+!>(hit))
        ?($filter-request $out)
          ?.  (has-arm %filter-response)  (fin-httr !>(hit))
          (call %filter-response httr+!>(hit))
      ==
    ::
    ++  get-made
      |=  [wir/whir-se result=made-result:ford]  ^+  abet
      ::  |=  {wir/whir-se dep/@uvH res/(each cage tang)}  ^+  abet
      ?:  ?=($core wir)  (made-core result)
      %.  result
      ?-  wir
        ?($filter-request $out)             made-filter-request
        ?($filter-response $res)            made-filter-response
        ?($receive-auth-response $bak)      made-receive-auth-response
        ?($receive-auth-query-string $in)   made-receive-auth-query-string
      ==
    ::
    ++  made-core
      |=  [result=made-result:ford]
      ::  |=  {dep/@uvH gag/(each cage tang)}
      :: ~&  got-update/dep
      ::  =.  ..vi  (pass-note %core [%f [%wasp our dep &]])
      ?:  ?=([%complete %success *] result)
        =/  =cage  (result-to-cage:ford build-result.result)
        pump(cor `q:cage)
      ?:  &(=(~ cor) =(%$ usr))
        =.  cor  `~
        pump ::(cor `~)  :: userless %hiss defaults to "nop" driver
      %-  warn
      ?:  ?=(%incomplete -.result)
        tang.result
      ?>  ?=(%error -.build-result.result)
      message.build-result.result
    ::
    ++  made-filter-request
      %+  on-ford-fail  dead-hiss
      %+  on-error  warn  |.
      %-  handle-moves  :~
        give+do-give
        send+(do-send %filter-request)
        show+do-show
      ==
    ::
    ++  made-filter-response
      %+  on-error  dead-hiss  |.
      %-  handle-moves  :~
        give+do-give
        send+(do-send %filter-request)
        redo+_pump
      ==
    ::
    ++  made-receive-auth-query-string
      %+  on-error  dead-this  |.
      (handle-moves send+(do-send %receive-auth-query-string) ~)
    ::
    ++  made-receive-auth-response
      %+  on-error  dead-this  |.
      %-  handle-moves  :~
        give+do-give
        send+(do-send %receive-auth-query-string)
        redo+_pump(..vi (give-html 200 ~ exit:xml))
      ==
    ::
    ::  Result handling
    ::
    ::    XX formal dill-blit %url via hood
    ++  auth-print  |=({$show a/purl} (slog auth-tank leaf+(en-purl a) ~))
    ++  auth-tank
      =>  =-  ?~(usr - rose+["@" `~]^~[leaf+(trip usr) -])
          rose+["." `~]^(turn (flop dom) |=(a/cord leaf+(trip a)))
      rose+[" " `~]^~[leaf+"To authenticate" . leaf+"visit:"]
    ::
    ++  do-give  (with !>(|=({$give a/httr} a)) fin-httr)
    ++  do-show  (with !>(auth-print) _abet)
    ++  do-send
      |=  wir/whir-se  ^-  $-(vase _abet)
      |=  res/vase
      (eyre-them wir (slam !>(|=({$send a/hiss} a)) res))
    ::
    ++  cancel-request  ~&  %cancel-request
                        (dead-hiss(liv &) [leaf+"canceled"]~)
    ::
    ++  handle-moves
      |=  a/(list {p/term q/$-(vase _abet)})
      |=  b/vase
      ~>  %nil.
        ~|  %bad-sec-move  :: XX move ~| into ?> properly
        ?>((~(nest ut p:!>(*sec-move)) %& p.b) ~)
      =+  opt=|.((silt (turn a head)))
      |-
      ?~  a  ~|(allowed=(opt) !!)
      ?:  =(p.i.a -.q.b)
        (q.i.a (sped b))
      $(a t.a)
    ::
    ++  on-ford-fail
      |=  {err/$-(tang _abet) try/$-(made-result:ford _abet)}
      |=  a/made-result:ford  ^+  abet
      ?:  ?=(%incomplete -.a)
        (err tang.a)
      ?:  ?=(%error -.build-result.a)
        (err message.build-result.a)
      (try a)
    ::
    ++  on-error
      |=  {err/$-(tang _abet) handle-move/_|.(|~(vase abet))}
      |=  a=made-result:ford  ^+  abet
      =+  try=(possibly-stateful |=(b/_self (handle-move(+ b))))  :: XX types
      ?:  ?=(%incomplete -.a)
        (err tang.a)
      ?:  ?=(%error -.build-result.a)
        (err message.build-result.a)
      =/  =cage  (result-to-cage:ford build-result.a)
      =-  ?-(-.- %& p.-, %| (err p.-))
      (mule |.(~|(driver+dom ~|(bad-res+p.q.cage (try q.cage)))))
    ::
    ++  possibly-stateful
      |=  han/$-(_self $-(vase _abet))  :: XX |.(|+(vase abet))
      |=  res/vase  ^+  abet
      ?:  ?=({@ *} q.res)
        =.  p.res  (~(fuse ut p.res) p:!>(*{@ *}))
        ((han self) res)
      ?.  ?=({{@ *} *} q.res)
        ~|(%misshapen-result !!)
      =.  p.res  (~(fuse ut p.res) p:!>(*{{@ *} *}))
      =+  [mow=(slot 2 res) roc=(slot 3 res)]
      =-  ((han self(cor (some roc))) mow):+  ::  XX better stateless asserts
      =+  typ=cor-type
      ~|  %core-mismatch
      ?>((~(nest ut typ) & p.roc) ~)
--  --
--
.   ==
=|  bolo
=*  bol  -
|=  [our=ship now=@da eny=@uvJ ski=sley]                ::  current invocation
^?                                                      ::  opaque core
|%                                                      ::
++  call                                                ::  handle request
  |=  $:  hen=duct
          type=*
          wrapped-task=(hobo task:able)
      ==
  ::
  =/  task=task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ((hard task:able) p.wrapped-task)
  ::
  ^+  [*(list move) ..^$]
  ?:  ?=($wegh -.task)
    :_  ..^$  :_  ~
    :^  hen  %give  %mass
    :-  %eyre
    :-  %|
    :~  dependencies+[%& liz]  sessions+[%& wup]  views+[%& wix]
        ducts+[%| ~[dead+[%& ded] proxy+[%& pox] outgoing+[%& ask]]]
        hosts+[%& dop]
        misc+[%& bol]
    ==
  =+  ska=(sloy ski)
  =+  sky=|=({* *} `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ^+  [p=*(list move) q=..^$]
  =.  gub  ?.(=(`@`0 gub) gub (cat 3 (rsh 3 1 (scot %p (end 6 1 eny))) '-'))
  =^  mos  bol
    abet:(apex:~(adit ye [hen [now eny our sky] ~] bol) task)
  [mos ..^$]
::
++  load                                                ::  take previous state
  |=  old/bolo
  ..^$(+>- old)
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=(%& -.why)  ~
  =*  who  p.why
  =+  ska=(sloy ski)
  =+  sky=|=({* *} `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  ?.  ?=($$ ren)  [~ ~]
  ?.  ?=($$ -.lot)  [~ ~]
  ?+    syd  [~ ~]
      $serv
    ``[%path !>((en-beam top))]
  ::
      $host
    %-  (lift (lift |=(a/hart [%hart !>(a)])))
    ^-  (unit (unit hart))
    ?.  =(our who)
      ?.  =([%da now] p.lot)  [~ ~]
      ~&  [%e %scry-foreign-host who]
      ~  :: XX add non-scry binding to $hat gram
    =.  p.lot  ?.(=([%da now] p.lot) p.lot [%tas %real])
    ?+  p.lot  [~ ~]
      {$tas $fake}  ``[& [~ 8.443] %& /localhost]       :: XX from unix
      {$tas $real}
        =/  hot=host  [%& ?^(dom n.dom /localhost)]
        =/  sek=?    &(?=(^ sek.por) !?=(hoke hot))
        =/  por=(unit @ud)
          ?.  sek
            ?:(=(80 clr.por) ~ `clr.por)
          ?>  ?=(^ sek.por)
          ?:(=(443 u.sek.por) ~ sek.por)
        ``[sek por hot]
    ==
  ==
::
++  stay  `bolo`+>-.$
++  take                                                ::  accept response
  |=  {tea/wire hen/duct hin/(hypo sign)}
  ^+  [*(list move) ..^$]
  =+  ska=(sloy ski)
  =+  sky=|=({* *} `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ^+  [p=*(list move) q=..^$]
  =.  gub  ?.(=(`@`0 gub) gub (cat 3 (rsh 3 1 (scot %p (end 6 1 eny))) '-'))
  =+  tee=((soft whir) tea)
  ?~  tee  ~&  [%e %lost -.q.hin hen]  [~ ..^$]
  =^  mos  bol
    =<  abet
    (axon:~(adit ye [hen [now eny our sky] ~] bol) u.tee q.hin)
  [mos ..^$]
::
--
