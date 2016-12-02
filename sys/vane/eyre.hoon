!:  ::  %eyre, http servant
!?  164
::::
|=  pit/vase
=,  ^eyre
=,  crypto
=,  bytes:eyre
=,  js:eyre
=,  url:eyre
=,  wired
=,  xml:eyre
=,  unity
=,  format
=>  =~
|%                                                      ::  interfaces
++  move  {p/duct q/(wind note gift:able)}              ::  local move
++  note                                                ::  out request $->
          $%  $:  $a                                    ::  to %ames
          $%  {$wont p/sock q/{path *}}                 ::
          ==  ==                                        ::
              $:  $b                                    ::  to  %behn
          $%  {$wait p/@da}                             ::
              {$rest p/@da}                             ::
          ==  ==                                        ::
              $:  $d                                    ::  to %dill
          $%  {$flog p/{$crud p/@tas q/(list tank)}}    ::
          ==  ==                                        ::
              $:  $e                                    ::  to self
          $%  {$thud $~}                                ::  proxied death
              {$this p/? q/clip r/httq}                 ::  proxied request
              {$meta vase}                              ::  type check
              {$mini-jael-task *}                       :: XX types
          ==  ==                                        ::
              $:  $f                                    ::  to %ford
          $%  {$exec p/@p q/(unit {beak silk:^ford})}   ::
              {$wasp p/@p q/@uvH r/?}                   ::
          ==  ==                                        ::
              $:  $g                                    ::  to %gall
          $%  {$deal p/sock q/cush:^gall}               ::  full transmission
          ==  ==  ==                                    ::
++  sign                                                ::  in result $<-
          $?  $:  $a                                    ::  by %ames
          $%  {$woot p/ship q/path r/coop}              ::  acknowledgment
              {$went ship cape:^ames}                   ::  XX ignore
          ==  ==                                        ::
              $:  $b                                    ::  by %behn
          $%  {$wake $~}                                ::  timer activate
          ==  ==                                        ::
              $:  $g                                    ::  by %gall
          $%  {$unto p/cuft:^gall}                      ::  within agent
          ==  ==                                        ::
              $:  $e                                    ::  by self
          $%  {$thou p/httr}                            ::  response for proxy
              {$mini-jael-gift *}                       :: XX types
          ==  ==                                        ::
              $:  $f                                    ::  by %ford
          $%  {$made p/@uvH q/gage:^ford}                     ::
              {$news p/@uvH}                            ::
          ==  ==                                        ::
              $:  @tas                                  ::  by any
          $%  {$crud p/@tas q/(list tank)}              ::
          ==  ==  ==                                    ::
++  mini-jael-task
  $%  {$save-cookie ses/hole own/?}
      {$kill-cookie ses/hole}
      {$save-token ses/hole tok/ixor}
      {$live-token ses/hole tok/ixor}
  ==
++  mini-jael-gift
  $%  {$cookie-ack him/@p}
      {$token-ack $~}
      {$token-beat $~}
      {$token-dead $~}
  ==
++  mini-jael-scry
  $%  {$pass him/ship pas/@t}       :: ?
      {$cook ses/hole}              :: (unit ship)
      {$ixor ses/hole tok/ixor}     :: ?
  ==
++  ixor  @t                                            ::  oryx hash
++  mend  ?($get $head)                                 ::  amend after building
++  whir  $@  $~                                        ::  wire subset
          $%  {$ac p/whir-ac}                           ::  finish request
              {$at p/cord:beak q/whir-ac}               ::  build request
              {$ay p/knot:ship q/knot:@uvH $~}          ::  remote duct
              {$hi p/knot q/mark $~}                    ::  outbound HTTP
              {$se p/whir-se q/{user (list @t)}}        ::  outbound to domain
              {$si $~}                                  ::  response done
              {$le $~}                                  ::  stateless lens req
              {$of p/ixor q/$@($~ whir-of)}             ::  associated view
              {$ow p/ixor $~}                           ::  dying view
              {$on $~}                                  ::  dependency
              {$je p/whir-je}
          ==
++  whir-je
  $%  {$ses p/hole $~}
      {$ire p/hole q/ixor $~}
      {$liv p/hole q/ixor $~}
  ==
++  whir-ac  {p/?($$ hole) q/mend r/$@($~ {p/@t $~})}   ::  auth? filter cookie?
++  whir-of  {p/knot:ship q/term s/wire}                ::  path in dock
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
  $:  $7                                                ::  version
      hov/(unit ship)                                   ::  master for remote
      top/beam                                          ::  ford serve prefix
      ged/duct                                          ::  client interface
      ded/(set duct)                                    ::  killed requests
      lyv/(map duct live)                               ::  living requests
      pox/(map @uvH duct)                               ::  proxied sessions
      ask/{p/@ud q/(map @ud {p/duct q/hiss})}           ::  outgoing by number
      kes/(map duct @ud)                                ::  outgoing by duct
      ney/@uvI                                          ::  rolling entropy
      liz/(jug @uvH (each duct ixor))                   ::  ford depsets
      wix/(map ixor stem)                               ::  open views
      sec/(map {user (list @t)} driv)                   ::  security drivers
      jel/mini-jael-state
  ==                                                    ::
::
++  je-per-ship
  $:  cok/(map hole die/@da)
      tok/(map oryx {hen/duct ses/hole die/@da liv/(unit @da)})
  ==
++  mini-jael-state
  $:  primary/(map ship je-per-ship)
      secondary/(map hole ship)
  ==
++  driv                                                ::  driver state
  %+  pair  (unit $@($~ vase))                          ::  main core
  {liv/? req/(qeu (trel duct mark vase:hiss))}          ::  waiting requests
::
++  live                                                ::  in flight
  $%  {$exec p/whir}                                    ::  ford build
      {$wasp p/(list @uvH)}                             ::  ford deps
      {$xeno p/ship}                                    ::  proxied request
      {$poll p/ixor}                                    ::  session state
  ==
::
++  stem                                                ::  client view
  $:  him/ship                                          ::  user
      pol/(unit duct)                                   ::  long-poll
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
      {$away $~}
      {$oath p/knot q/(list @t)}
      {$bugs p/?($as $to) $~}
      {$beam p/beam}
      {$deps p/?($put $delt) q/@uvH}
      {$mess p/{dock mark wire s/json}}
      {$poll p/{i/@uvH t/(list @uvH)}}
      {$spur p/spur}
      {$subs p/?($put $delt) q/{dock $json wire path}}
      {$view p/ixor q/{$~ u/@ud}}
  ==
::
++  perk-auth                                           ::  parsed auth
  $%  {$at p/pork}                                      ::  inject auth
      {$del p/(unit ship)}
      {$get him/ship rem/pork}
      {$js $~}
      {$json $~}
      {$try him/ship paz/(unit cord)}
  ==
::
++  pest                                                ::  result
  $@  $~
  $%  {$$ p/httr}                                       ::  direct response
      {$red $~}                                         ::  parent redirect
      {$bake p/mend q/mark r/coin s/beam}               ::  ford request
      {$js p/@t}                                        ::  script
      {$json p/json}                                    ::  data
      {$html p/manx}                                    ::  successful page
      {$htme p/manx}                                    ::  authentication fail
  ==
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
++  gsig  |=({a/dock b/path} [(scot %p p.a) q.a b])
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
  =+  `cok/quay`(zing `(list quay)`(murn cot (curr rush cock:urlp)))
  |-  ^-  (unit cord)
  ?~  cok  ~
  ?:((fil i.cok) [~ q.i.cok] $(cok t.cok))
::
++  pack                                                ::  light path encoding
  |=  {a/term b/path}  ^-  knot
  %+  rap  3  :-  (wack a)
  (turn b |=(c/knot (cat 3 '_' (wack c))))
::
++  puck                                                ::  light path decoding
  =+  fel=(most cab (sear wick urt:ab))
  |=(a/knot `(unit {p/term q/path})`(rush a fel))
::
++  wush
  |=  {wid/@u tan/tang}
  ^-  wall
  (zing (turn tan |=(a/tank (wash 0^wid a))))
::
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
  var _urb = {(pojo urb)};
  window.urb = window.urb || \{}; for(k in _urb) window.urb[k] = _urb[k];

  """
::
++  ares-to-json
  |=  err/ares  ^-  json
  =-  (jobe fail+s+typ mess+(jape mez) ~)
  ^-  {typ/term mez/tape}
  ?~  err  [%fail "Unknown Error"]
  [p.u.err (of-wall (wush 160 q.u.err))]
::
++  resp                                                ::  mimed response
  |=  {sas/@uG mit/mite rez/@}  ^-  httr
  ::  (weld (turn cug |=(a=@t ['set-cookie' a]))
  [sas ~[content-type+(moon mit)] [~ (taco rez)]]
::
++  add-links                                           :: x-urbit:// urls
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
      var location = new URL(document.location)
      location.pathname = location.pathname.replace(/^\/~~|\/~\/as\/any/,'/~/as/~'+ship)
      urb.redirTo(location)
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
          document.location.reload()
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
        ;script: setTimeout(function()\{document.location = {(pojo (jape url))}}, 3000)
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
  ::
  ::
  ++  je
    |=  him/ship
    =+  (fall (~(get by primary.jel) him) *je-per-ship)
    |%
    ++  abet  +>.$(primary.jel (~(put by primary.jel) him +<.abet))
    ++  wake
      |=  wir/whir-je  ^+  ..je
      =<  abet
      ?-  -.wir
        $ses  (kill-cookie p.wir)
        $ire  (kill-token q.wir)
        $liv  (beat-token q.wir)
      ==
    ::
    ++  apex
      |=  kyz/mini-jael-task  ^+  ..je
      =<  abet
      ?-    -.kyz
        $save-cookie  (save-cookie ses.kyz)
        $kill-cookie  (kill-cookie ses.kyz)
        $save-token  (save-token ses.kyz tok.kyz)
        $live-token  (live-token ses.kyz tok.kyz)
      ==
    ::
    ++  save-cookie
      |=  ses/hole
      ?<  (~(has by secondary.jel) ses)
      =.  secondary.jel  (~(put by secondary.jel) ses him)
      =/  die  (add now ~d7)
      =.  +>.$  (reset-timer je+ses+/[ses] ~ `die)
      %_  +>.$
        cok   (~(put by cok) ses die)
        ..je  (jael-give [%cookie-ack him])
      ==
    ::
    ++  live-cookie
      |=  ses/hole
      =/  ole  (~(got by cok) ses)
      =/  die  (add now ~d7)
      =.  +>.$  (reset-timer je+ses+/[ses] `die.ole `die)
      %_  +>.$
        cok  (~(put by cok) ses die)
        secondary.jel  (~(put by secondary.jel) ses him)
      ==
    ::
    ++  kill-cookie
      |=  ses/hole                    :: XX actively kill tokens?
      =/  ole  (~(got by cok) ses)
      =.  +>.$  (reset-timer je+ses+/[ses] `die.ole ~)
      %_  +>.$
        cok  (~(del by cok) ses)
        secondary.jel  (~(del by secondary.jel) ses)
      ==
    ::
    ++  save-token
      |=  {ses/hole ire/ixor}
      =/  die  (add now ~d7)
      =.  +>.$  (reset-timer je+ire+/[ses]/[ire] ~ `die)
      %_  +>.$
        tok   (~(put by tok) ire [hen ses die ~])
        ..je  (jael-give [%token-ack ~])
      ==
    ::
    ++  live-token
      |=  {ses/hole ire/ixor}
      =/  ole  (~(got by tok) ire)
      ?.  (~(has by cok) ses)  ~&(expired-session+ses +>.$) :: XX
      ?>  =(ses ses.ole) :: XX caught beforehand?
      =.  +>.$  (live-cookie ses)
      =+  [liv=`(add ~s30 now) die=(add ~d1 now)]
      =.  +>.$  (reset-timer je+liv+/[ses]/[ire] liv.ole liv)
      =.  +>.$  (reset-timer je+ire+/[ses]/[ire] `die.ole `die)
      %_  +>.$
        tok   (~(put by tok) ire [hen ses die liv])
      ==
    ::
    ++  kill-token
      |=  ire/ixor
      =/  ole  (~(got by tok) ire) 
      =.  +>.$  (reset-timer je+liv+/[ses.ole]/[ire] liv.ole ~)
      =.  +>.$  (reset-timer je+ire+/[ses.ole]/[ire] `die.ole ~)
      %_  +>.$
        tok   (~(del by tok) ire)
        ..je  (jael-give(hen hen.ole) [%token-dead ~])
      ==
    ::
    ++  beat-token
      |=  ire/ixor
      =/  ole  (~(got by tok) ire)
      %_  +>.$
        tok   (~(put by tok) ire ole(liv ~))
        ..je  (jael-give(hen hen.ole) [%token-beat ~])
      ==
    ::
    ::
    ++  jael-give
      |=(mini-jael-gift %_(..je mow :_(mow [hen %give %mini-jael-gift +<])))
    ::
    ++  reset-timer
      |=  {wir/whir ole/(unit time) new/(unit time)}
      =.  mow  ?~(ole mow :_(mow [`/ %pass wir [%b %rest u.ole]]))
      =.  mow  ?~(new mow :_(mow [`/ %pass wir [%b %wait u.new]]))
      +>.$
    --
  ::
  ++  scry-jael
    |=  a/mini-jael-scry
    ^-  $%({$bean ?} {$u-ship (unit ship)})
    ?-    -.a
        $pass
      :-  %bean
      ?>  =(our him.a)  :: only own password known
      =(|2.a load-secret)
    ::
        $cook
      :-  %u-ship
      (~(get by secondary.jel) ses.a)
    ::
        $ixor
      :-  %bean
      =/  loc  (~(got by primary.jel) (~(got by secondary.jel) ses.a))
      =(ses.a ses:(~(got by tok.loc) tok.a))
    ==
  ::
  ::
  ++  apex                                              ::  accept request
    |=  kyz/task:able
    ^+  +>
    =.  our  ?~(hov our u.hov)  ::  XX
    =.  p.top  our              ::  XX necessary?
    ?-    -.kyz
        $mini-jael-task
      =/  kyz-je  ;;(mini-jael-task +.kyz)
      =;  him  (apex:(je him) kyz-je)
      ?-    -.kyz-je
          $save-cookie
        ?:  own.kyz-je  our
        `@p`(mix anon (lsh 5 1 (rsh 5 1 (shaf %ship ses.kyz-je))))
      ::
          $kill-cookie  (~(got by secondary.jel) ses.kyz-je)
          $save-token   (~(got by secondary.jel) ses.kyz-je)
          $live-token   (~(got by secondary.jel) ses.kyz-je)
      ==
    ::
        $born  +>.$(ged hen)                            ::  register external
        $serv
      =<  ~&([%serving (en-beam top)] .)
      ?^(p.kyz +>.$(top p.kyz) +>.$(q.top p.kyz))
    ::
        $crud
      +>.$(mow [[hen %slip %d %flog kyz] mow])
    ::
        $init                                           ::  register ownership
      =.  our  ?~(hov p.kyz (min u.hov p.kyz))
      +>.$(hov [~ our], top [[our %home ud+0] /web])
    ::
        ?($chis $this)                                  ::  inbound request
      %-  emule  |.  ^+  ..apex
      =*  sec  p.kyz    ::  ?                           ::  https bit
      =*  heq  r.kyz    ::  httq                        ::  request content
      =+  ryp=`quri`(rash q.heq zest:urlp)
      =+  maf=(eat-headers r.heq)
      =+  ^=  pul  ^-  purl
          ?-  -.ryp
            $&  ?>(=(sec p.p.p.ryp) p.ryp)
            $|  =+  hot=(~(get ja maf) %host)
                ?>  ?=({@ $~} hot)
                [[sec (rash i.hot thor:urlp)] p.ryp q.ryp]
          ==
      =.  p.p.pul  |(p.p.pul ?=(hoke r.p.pul))
      ?:  ?=($chis -.kyz)                               :: IPC escape hatch
        ~(as-lens handle pul [q.+.kyz |] [p.heq maf s.heq])
      =+  her=(host-to-ship r.p.pul)
      ?:  |(?=($~ her) =(our u.her))
        ~(apex handle pul [q.+.kyz |] [p.heq maf s.heq])
      =+  han=(sham hen)
      =.  pox  (~(put by pox) han hen)
      (ames-gram u.her [%get ~] han +.kyz)
    ::
        $them                                           ::  outbound request
      ?~  p.kyz
        =+  sud=(need (~(get by kes) hen))
        %=  +>.$
          mow    :_(mow [ged [%give %thus sud ~]])
          q.ask  (~(del by q.ask) sud)
          kes    (~(del by kes) hen)
        ==
      ::  ~&  eyre-them+(earn p.u.p.kyz)
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
      ::  ~&  eyre-them+(earn p.u.p.kyz)
      =+  usr=?~(p.kyz '~' (scot %ta u.p.kyz))
      (back hi+/[usr]/[q.kyz] %hiss r.kyz)
    ::
        $they                                           ::  inbound response
      =+  kas=(need (~(get by q.ask) p.kyz))
      ::  ~&  >  eyre-they+[p.q.kyz (earn p.q.kas)]
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
      :: ~&  did-thud+[-.lid hen]
      ?-  -.lid
          $exec
        (pass-note p.lid %f [%exec our ~])
      ::
          $poll
        ?.  (~(has by wix) p.lid)
         +>.$
        poll-dead:(ire-ix p.lid)
      ::
          $xeno
        =+  han=(sham hen)
        =.  pox  (~(del by pox) han hen)
        (ames-gram p.lid [%gib ~] han)
      ::
          $wasp
        |-  ^+  +>.^$
        ?~  p.lid  +>.^$
        (del-deps:$(p.lid t.p.lid) i.p.lid %& hen)
      ==
    ::
        $went
      ::  this won't happen until we send responses.
      !!
    ::
        $west                                           ::  remote request
      =.  mow  :_(mow [hen %give %mack ~])
      =+  mez=((soft gram) q.kyz)
      ?~  mez
        ~&  e+[%strange-west p.kyz]
        ~|(%strange-west !!)
      ?-  -<.u.mez        
        $gib  (pass-note ay+(dray p+uv+~ q.p.kyz p.u.mez) [%e %thud ~])
        $get  (pass-note ay+(dray p+uv+~ q.p.kyz p.u.mez) [%e %this q.u.mez])
        $got
          ?.  (~(has by pox) p.u.mez)
            ~&  lost-gram-thou+p.kyz^p.u.mez
            +>.$
          =:  hen  (~(got by pox) p.u.mez)
              pox  (~(del by pox) p.u.mez)
            ==
          (give-thou q.u.mez)
      ==
    ::
      $wegh  !!                                         ::  handled elsewhere
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
    =.  our  ?~(hov our u.hov)  ::  XX
    ?:  &(?=({?($of $ow) ^} tee) !(~(has by wix) p.tee))
      ~&(dead-ire+[`whir`tee ({term term $~} +.sih)] +>)
    ?-    &2.sih
        $mini-jael-gift
      =/  gif  ;;(mini-jael-gift |2.sih)
      ?:  ?=($cookie-ack -.gif)
        :: XX probably should wait for this instead of pulling ship out via scry
        +>.$
      ?>  ?=({$of @ $~} tee)
      (get-jael:(ire-ix p.tee) gif)
    ::
        $crud  +>.$(mow [[hen %slip %d %flog +.sih] mow])
    ::  $dumb
    ::    =.  +>  ?+(tee +> [%of ^] pop-duct:(ire-ix p.tee))
    ::    (emule |.(~|(gall-dumb+tee !!)))
    ::
        $woot  +>.$
        $went
          :: XX eyre sends no wests, so should get no wents
          ::~&  e+unexpected+sih
          +>.$
    ::
    ::
        $thou
      ?+    -.tee  !!
        $ay  (ames-gram (slav %p p.tee) got+~ (slav %uv q.tee) |2.sih)
        $hi  (cast-thou q.tee httr+!>(p.sih))
        $se  (get-thou:(dom-vi q.tee) p.tee p.sih)
      ==
    ::
        $unto                                           ::  app response
      ?>  ?=($%({$le $~} {$of @ ^}) tee)
      =+  cuf=`cuft:^gall`+>.sih
      ?-    -.cuf
          ?($coup $reap)
        =/  ack  ?~(p.cuf ~ `[-.cuf u.p.cuf])
        ?:  ?=($le -.tee)  (~(get-ack lens ~) ack)
        (get-ack:(ire-ix p.tee) q.tee ack)
      ::
          $doff  !!
          $diff
        ?.  ?=($json p.p.cuf)
          :: ~>  %slog.`%*(. >[%backing p.p.cuf %q-p-cuf]< &3.+> (sell q.p.cuf))
          (back tee %json p.cuf)
        =/  jon  ((hard json) q.q.p.cuf)
        ?:  ?=($le -.tee)  (~(get-diff lens ~) jon)
        (get-rush:(ire-ix p.tee) q.tee jon)
      ::
          $quit
        ~&  quit+tee 
        ?:  ?=($le -.tee)  ~(get-quit lens ~)
        (get-quit:(ire-ix p.tee) q.tee)
      ==
    ::
        $wake
      ?>  ?=($je -.tee)
      %.(p.tee wake:(je (~(got by secondary.jel) p.p.tee)))
    ::
        $news                                         ::  dependency updated
      ?:  ?=({$se *} tee)
        (get-news:(dom-vi q.tee) p.sih)
      ?.  ?=({$on $~} tee)
        ~&(e+lost+[tee hen] +>.$)
      %+  roll  (~(tap in (~(get ju liz) p.sih)))
      =<  .(con ..axon(liz (~(del by liz) p.sih)))
      |=  {sus/(each duct ixor) con/_..axon}
      =.  ..axon  con
      ?-  -.sus
        $&  (give-json(hen p.sus) 200 ~ %s (scot %uv p.sih))
        $|  (get-even:(ire-ix p.sus) +.sih)
      ==
    ::
        $made
      ?<  ?=($tabl -.q.sih)
      =.  our  (need hov)                             ::  XX
      ?-    tee
          $@($~ {?($on $ay $ow $je) *})  ~|(e+ford+lost+tee !!)
          {$of @ $~}  ~|(e+ford+lost+tee !!)
          {$si $~}  (give-sigh q.sih)
          {$se ^}   (get-made:(dom-vi q.tee) p.tee [p q]:sih)
          {$hi ^}
        ?:  ?=($| -.q.sih)
          (give-sigh q.sih)  ::  XX crash?
        =*  cay  p.q.sih
        ?>  ?=($hiss p.cay)
        ?:  =('~' p.tee)
          (eyre-them tee q.cay)
        =+  usr=(slav %ta p.tee)
        =+  ((hard {pul/purl ^}) q.q.cay)
        ?.  ?=($& -.r.p.pul)
          ~&  [%auth-lost usr (head:earn p.pul)]
          (eyre-them tee q.cay)
        (get-req:(dom-vi usr (scag 2 p.r.p.pul)) q.tee q.cay)
      ::
::           {$hi ^}
::         ?:  ?=($| -.q.sih)
::           (give-sigh q.sih)  ::  XX crash?
::         =*  cay  p.q.sih
::         ?>  ?=($hiss p.cay)
::         (eyre-them p.tee q.cay)
      ::
          {$le $~}
        ?:  ?=($| -.q.sih)
          ((slog p.q.sih) +>.$)             ::  XX get-ack (some)?
        %-  ~(get-diff lens ~)
        ?>  ?=($json p.p.q.sih)                    ::  XX others
        ((hard json) q.q.p.q.sih)
      ::
          {$of @ ^}
        ?:  ?=($| -.q.sih)
          ((slog p.q.sih) +>.$)             ::  XX get-even %mean
        %+  get-rush:(ire-ix p.tee)  q.tee
        ?>  ?=($json p.p.q.sih)                    ::  XX others
        ((hard json) q.q.p.q.sih)
      ::
          {$at ^}
        %-  emule  |.  ^+  ..apex
        ?.  ?=($& -.q.sih)
          (fail 404 p.sih p.q.sih)
        =^  cay  ..ya  ::  inject stat-json
          =*  cay  p.q.sih
          ?~  p.q.tee  [cay ..ya]
          (add-auth p.q.tee cay)  :: XX block on session save?
        ?:  ?=($red-quri p.cay)
          =+  url=(apex:earn ((hard quri) q.q.cay))
          (give-thou 307 [location+(crip url)]~ ~)
          :: (give-html:abet 200 ~ (redir:xml url))
        ?.  ?=($mime p.cay)
          =+  bek=(norm-beak -:(need (de-beam (need (puck p.tee)))))
          (exec-live ac+q.tee bek [%flag [p.sih `~] %cast %mime [%$ cay]])
        (give-mime q.tee p.sih cay)
      ::
          {$ac ^}
        %-  emule  |.  ^+  ..apex
        ?.  ?=($& -.q.sih)
          (fail 404 p.sih p.q.sih)
        =*  cay  p.q.sih
        ?>  ?=($mime p.cay)
        (give-mime p.tee p.sih cay)
      ==
    ==
  ::
  ++  give-mime
    |=  {{ses/?($~ hole) men/mend dom/?($~ {p/@t $~})} dep/@uvH cay/cage}
    ^+  +>.$
    ?>  ?=($mime -.cay)
    ~|  q.q.cay
    =+  cug=?~(dom ~ ?~(ses !! [(set-cookie p.dom cookie-prefix ses)]~))
    =+  ((hard {mit/mite rez/octs}) q.q.cay)
    =+  dep=(crip "W/{(pojo %s (scot %uv dep))}")
    =+  bod=?-(men $get `rez, $head ~)
    =+  hit=[200 ~[etag+dep content-type+(moon mit)] bod]
    (give-thou (add-cookies cug hit))
  ::
  ++  add-auth
    |=  {ses/hole cay/cage}  ^-  {cage _..ya}
    ?.  ?=($js -.cay)
      ~&  e+at-lost+-.cay
      [cay ..ya]
    ?>  ?=(@ q.q.cay)
    =^  jon  ..ya  ~(stat-json ya ses)
    [cay(q.q (add-json jon q.q.cay)) ..ya]
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
  ++  our-host  `hart`[& ~ %& /org/urbit/(rsh 3 1 (scot %p our))]
  ::                  [| [~ 8.443] `/localhost]       :: XX testing
  ::
  ++  eyre-them
    |=  {tea/whir vax/vase}
    (pass-note tea [%e %meta :(slop !>(%them) !>(~) vax)])
  ::
  ++  ames-gram
    |=({him/ship gam/gram} (pass-note ~ %a %wont [our him] [%e -.gam] +.gam))
  ::
  ++  jael-note
    |=({tea/whir kyz/mini-jael-task} (pass-note tea %e %mini-jael-task kyz))
  ::
  ++  back                                              ::  %ford bounce
    |=  {tea/whir mar/mark cay/cage}
    (execute tea (norm-beak -.top) [%cast mar $+cay])
  ::
  ++  cast-thou
    |=  {mar/mark cay/cage}
    ?:  ?=($httr mar)  (give-sigh %& cay)
    %^  execute  si+~  (norm-beak -.top)
    [%alts [%cast mar $+cay] [%cast %recoverable-error $+cay] ~]
  ::
  ++  del-deps
    |=  {a/@uvH b/(each duct ixor)}  ^+  +>.$
    ?:  =(`@`0 a)  +>.$
    =.  liz  (~(del ju liz) a b)
    :: ~&  del-deps+[a (~(get ju liz) a)]
    ?:  (~(has by liz) a)  +>.$
    =-  -(hen hen.+)
    (pass-note(hen `~) on+~ %f [%wasp our a |])
  ::
  ++  new-deps
    |=  {a/@uvH b/(each duct ixor)}  ^+  +>.$
    :: ~&  new-deps+[a b]
    ?:  =(`@`~ a)  +>.$
    =+  had=(~(has by liz) a)
    =.  liz  (~(put ju liz) a b)
    ?:  had  +>.$
    =-  -(hen hen.+)
    (pass-note(hen `~) on+~ %f [%wasp our a &])
  ::
  ++  ford-req  |=({bek/beak kas/silk:^ford} [%f [%exec our `[bek kas]]])
  ++  exec-live
    |=  {tea/whir req/{beak silk:^ford}}
    =.  lyv  (~(put by lyv) hen [%exec tea])
    (execute tea req)
  ::
  ++  execute
    |=  {tea/whir bek/beak sil/silk:^ford}
    %+  pass-note  tea
    :^  %f  %exec  our
    `[bek [%dude |.(leaf+"eyre: execute {<tea>}") sil]]
  ::
  ++  fail
    |=  {sas/@ud dep/@uvH mez/tang}
    ^+  +>
    :: (back ha+~ dep %tang !>(mez))  ::tang->urb chain may be source of failure
    (give-html sas ~ (render-tang dep mez))
  ::
  ++  give-html
    |=  {sas/@ud cug/(list @t) max/manx}
    %-  give-thou
    %+  add-cookies  cug
    (resp sas text+/html (crip (print max)))
  ::
  ++  give-json
    |=  {sas/@uG cug/(list @t) jon/json}
    %-  give-thou
    %+  add-cookies  cug
    (resp sas application+/json (crip (pojo jon)))
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
    ?.  ?=($| -.res)  p.res
    [%tang !>(p.res)]
  ::
  ++  mean-json  |=({sas/@uG err/ares} (give-json sas ~ (ares-to-json err)))
  ++  nice-json  |=(* (give-json 200 ~ (joba %ok %b &)))
  ::
  ++  pass-note  |=(noe/{whir note} %_(+> mow :_(mow [hen %pass noe])))
  ++  host-to-ship                                              ::  host to ship
    |=  hot/host
    ^-  (unit ship)
    :: =+  gow=(~(get by dop) hot)    ::  XX trust
    :: ?^  gow  gow
    ?.  ?=($& -.hot)  ~
    =+  dom=(flop p.hot)                                ::  domain name
    ?~  dom  ~
    (rush i.dom fed:ag)
  ::
  ++  load-secret
    ^-  @ta
    =+  pax=/(scot %p our)/code/(scot %da now)/(scot %p our)
    %^  rsh  3  1
    (scot %p (@ (need (sky [151 %noun] %a pax))))
  ::
  ++  cookie-prefix  (rsh 3 1 (scot %p our))
  ++  set-cookie
    |=  {domain/@t key/@t val/@t}
    %+  rap  3  :~
      key  '='  val
      ::  '; HttpOnly'  ?.(sec '' '; Secure')  ::  XX security
      domain
      '; Path=/; HttpOnly'
    ==
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
    ++  teba  |*(a/$-(* ..handle) |*(b/* %_(done ..handle (a b))))
    ++  del-deps  (teba ^del-deps)
    ++  new-deps  (teba ^new-deps)
    ++  exec-live  (teba ^exec-live)
    ++  give-html  (teba ^give-html)
    ++  give-thou  (teba ^give-thou)
    ++  give-json  (teba ^give-json)
    ++  nice-json  (teba ^nice-json)
    ++  pass-note  (teba ^pass-note)
    ::
    ++  fcgi-cred
      ^-  cred
      =/  him  
        ?.  aut  anon
        (need get-user:for-client)
      %*  .  *cred
        hut  hat
        orx  'not-yet-implemented'
        acl
          =+  lag=(~(get by maf) %accept-language)
          ?~(lag ~ ?~(u.lag ~ [~ i.u.lag]))
      ::
        :: cip  cip :: XX performance
        aut  (~(put ju ^+(aut:*cred ~)) %$ (scot %p him))
      ==
    ::
    ++  apex
      =<  abet
      ^+  done
      =+  oar=(host-to-ship r.hat)
      =.  our  ?~(oar our u.oar)  ::  XX
      =+  pez=process
      ?:  ?=($| -.pez)  p.pez
      (resolve ~ p.pez)
    ::
    ++  as-lens
      =<  abet
      ::  (process-parsed [%mess [our %dojo] %lens-command /lens grab-json])
      ((teba ~(new lens ~)) grab-json)
    ::
    ++  resolve
      |=  {cug/(list @t) pez/pest}  ^+  done
      ?~  pez  done
      ?-  -.pez
          $~     (give-thou (add-cookies cug p.pez))
          $js    $(pez [%$ (resp 200 text+/javascript p.pez)])
          $json  (give-json 200 cug p.pez)
          $html  (give-html 200 cug p.pez)
          $htme  (give-html 401 cug p.pez)
          $bake  (resolve-bake ~ ~ +.pez)
      ::
          $red
        =+  url=(earn hat pok(p [~ %html]) quy)
        ?+    p.pok  ~|(bad-redirect+[p.pok url] !!)
            {$~ $js}
          $(pez [%js auth-redir:js])
            {$~ $json}
          $(pez [%json (jobe ok+b+| red+(jape url) ~)])
        ==
      ==
    ::
    ++  resolve-bake
      |=  {ses/(unit hole) dom/(unit @t) men/mend mar/mark arg/coin bem/beam}
      =+  wir=[%at (pack [- +]:(en-beam -.bem ~)) (fall ses %$) men ?~(dom ~ [u.dom]~)]
      =.  -.bem  (norm-beak -.bem)
      =+  req=[%bake mar arg bem]
      =+  red=[%bake %red-quri arg bem]
      (exec-live wir -.bem `silk:^ford`[%alts ~[req red]])
    ::
    ::
    ++  is-anon  =([~ ''] (~(get by (molt quy)) 'anon'))
    ++  check-oryx                    ::  | if json with bad oryx
      ^-  ?
      ?.  &(?=({$~ $json} p.pok) ?=($post mef) ?=(^ bod) !is-anon)  &
      =+  oxe=grab-oryx
      ?~  oxe  |
      =/  ses  (session-from-cookies cookie-prefix maf)
      ?~  ses  ~&(%oryx-no-cookie &)  :: XX security
      ?~  ~(get-user ya u.ses)  ~&(%oryx-bad-cookie |)
      =/  ire  (oryx-to-ixor u.oxe)
      ?~  (~(get by wix) ire)  ~&(bad-oryx+u.oxe &)  :: XX security?
      =+  (scry-jael %ixor u.ses ire)
      ?>  ?=($bean -<)
      ?.  ->
        ~&(oryx-ses-mismatch+[orx=u.oxe u.ses] &)  :: XX security
      &
    ::
    ++  grab-json
      ^-  json
      ?.  ?=(?($post $put $delt) mef)
        ~|(bad-method+mef !!)
      ?~  bod
        ~|(%no-body !!)
      (need (poja q.u.bod))
    ::
    ++  grab-json-soft
      ^-  (unit json)
      ?.  ?=(?($post $put $delt) mef)
        ~
      ?~(bod ~ (poja q.u.bod))
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
              ~|(%bad-oryx ~|([grab-oryx ses:for-client] !!))
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
            {?($ico $png) $favicon $~}
          :-  ~
          %^  resp  200  image+/png
          favi
        ::
            {$txt $robots $~}
          :-  ~
          %^  resp  200  text+/plain
          %-  of-wain:format
          :~  'User-agent: *'
              'Disallow: '
          ==
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
        =+  (plex:vast %conl u)                         :: staticly, and make a
        (biff - de-beam)                                   :: valid beam
      ::
      ++  as-aux-request                                ::  /~/... req parser
        ^-  (unit perk)
        =.  mef
          ?.  ?=($post mef)  mef
          ?+    (skim quy |=({a/@t b/@t} &(=('' b) =(a (crip (cuss (trip a)))))))
              ~|(bad-quy+[req='"?PUT" or "?DELETE"' quy] !!)
            $~   mef
            {{$'DELETE' $~} $~}  %delt
            {{$'PUT' $~} $~}     %put
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
          =+  `dom/host`~|(bad-host+i.but (rash i.but thos:urlp))
          ?:  ?=($| -.dom)  ~|(auth-ip+dom !!)
          =-  [%oath - p.dom]
          ~|  bad-user+`path`t.but
          ?>  ?=({@ $in $~} t.but)
          =+  in-quy=(rush i.t.but ;~(pfix cab fque:urlp))
          ?~  in-quy
            (slav %ta i.t.but)
          =+  src=~|(no+u.in-quy (~(got by (malt quy)) u.in-quy))
          p:(need (puck src))  ::  allow state=usr_other-data
        ::
            $at  [%auth %at pok(q but)]
            $as
          :+  %auth  %get
          ~|  bad-ship+?~(but ~ i.but)
          ?~  but  !!
          :_  pok(q t.but)
          ?+  i.but  (slav %p i.but)
            $anon  anon
            $own   (fall (ship-from-cookies maf) our)
          ==
        ::
            $on
          :-  %poll
          ?^  but  [(raid but %uv ~)]~
          =+  dep=((hard (list {@ $~})) quy)
          =<  ?~(. !! .)
          (turn dep |=({a/@tas $~} (slav %uv a)))
        ::
            $of
          :+  %view  ?>(?=({@ $~} but) i.but)
          ?>  ?=({{$poll @} $~} quy)     ::  XX eventsource
          [~ (rash q.i.quy dem)]
        ::
            $to
          =+  ^-  dir/{p/ship q/term r/mark}
              ~|  bad-mess+but
              ?+  but  !!
                {@ @ $~}    [our (raid but %tas %tas ~)]
                {@ @ @ $~}  (raid but %p %tas %tas ~)
              ==
          =;  x/{wir/wire mez/json}
            [%mess [p q]:dir r.dir wir.x mez.x]
          =+  wir=(~(get by (molt quy)) 'wire')
          ?^  wir  [(stab u.wir) grab-json]          ::  XX distinguish
          %.(grab-json =>(dejs (ot wire+(cu stab so) xyro+same ~)))
        ::
            $in
          ~|  expect+[%post 'application+json' /'@uv' '?PUT/DELETE']
          ?>  &(?=(?($delt $put) mef) ?=($@($~ {$~ $json}) p.pok))
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
              $~         $(p.pok [~ %json])
              {$~ $js}    [%js ~]
              {$~ $json}
            ?+    mef  ~|(bad-meth+mef !!)
                $get   [%json ~]
                $put
              ~|  parsing+bod
              :-  %try
              %.(grab-json =>(dejs (ot ship+(su fed:ag) code+(mu so) ~)))
            ::
                $delt
              ~|  parsing+bod
              :-  %del
              %.(grab-json =>(dejs-soft (ot ship+(su fed:ag))))
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
        $post  [`quay`(weld quy `quay`(rash q:(need bod) yquy:urlp)) %get]
      ==
    ++  process
      ^-  (each pest _done)
      =+  pet=parse
      ?:  ?=($| -.pet)
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
        =+  ext=(fall p.pok %urb)
        =+  bem=?-(-.hem $beam p.hem, $spur [-.top (weld p.hem s.top)])
        ~|  bad-beam+q.bem
        ?<  =([~ 0] (sky [151 %noun] %cw (en-beam bem(+ ~, r [%da now]))))
        =+  men=?+(mef !! $get mef, $head mef)  :: redact result
        =+  arg=(fcgi payload fcgi-cred)
        =+  [%bake men ext arg bem]
        ?.(aut [%& `pest`-] [%| `_done`(resolve ~ -)])
      ::
          $bugs
        ?-  p.hem
          $as  show-login-page
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
        ?.  is-anon
          ((teba new-mess:for-view) p.hem(s [%json !>(`json`s.p.hem)]))
        =^  orx  ..ya  new-view:for-client
        =+  vew=(ire-ix (oryx-to-ixor orx))
        ((teba new-mess.vew) p.hem(s [%json !>(`json`s.p.hem)]))
      ::
          $oath
        ?.  (~(has by sec) [p q]:hem)
          ~|(no-driver+[p q]:hem !!)
        [%| %.(quy (teba get-quay:(dom-vi [p q]:hem)))]
      ::
          $poll
        ?:  ?=({$~ $js} p.pok)  ::  XX treat non-json cases?
          =+  deps=[%a (turn `(list @uvH)`p.hem |=(a/@ s+(scot %uv a)))]
          [%& %js (add-json (joba %deps deps) poll:js)]
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
        [%| ((teba poll:(ire-ix p.hem)) u.q.hem ses:for-client)]
      ==
    ::
    ++  process-auth
      |=  ham/perk-auth  ^-  (each pest _done)
      =+  yac=for-client
      ?-    -.ham
          $js    [%& %js auth:js]
          $json  =/  cug  (set-cookie -):yac
                 =^  jon  ya  stat-json.yac  :: XX block on session save?
                 [%| (give-json 200 cug jon)]
      ::
          $at
        =.  ..ya  abet.yac
        =+  pez=process(pok p.ham, aut |)
        ?.  ?=($& -.pez)  ~|(no-inject+p.ham !!)
        ?~  p.pez  pez
        ?+    -.p.pez  ~&(bad-inject+p.pez !!)
            $red  pez
            $bake
          =.  ya  abet.yac
          [%| (resolve-bake `ses.yac dom.yac +.p.pez)]
        ::
            $js
          =/  cug  (set-cookie -):yac
          =^  jon  ya  stat-json.yac  :: XX block on session save?
          [%| (resolve cug p.pez(p (add-json jon p.p.pez)))]
        ==
      ::
          $del
        =.  ..ya  abut:yac
        =/  cug
          :~  (set-cookie cookie-domain cookie-prefix '~')
              (set-cookie cookie-domain %ship '~')
          ==
        [%| (give-json 200 cug (joba %ok %b &))]
      ::
          $get
        |-
        ~|  aute+ham
        ?:  |(=(anon him.ham) =(get-user.yac `him.ham))
          =+  pez=process(pok rem.ham, aut &)
          ?:  ?=($| -.pez)  pez
          [%| (resolve ~ p.pez)]
        ?.  =(our him.ham)
          ~|(sso-disabled+[our him.ham] !!)
        show-login-page
      ::
          $try
        :-  %|
        ?.  =(our him.ham)
          ~|(stub-foreign+him.ham !!)
        ?.  ?|  =(get-user.yac `him.ham)
                ?~(paz.ham | (check-password him.ham u.paz.ham))
            ==
          ~|(%auth-fail !!)
        =.  yac  (for-authed-client him.ham)
        =/  cug  (set-cookie -):yac
        =^  jon  ya  stat-json.yac
        (give-json 200 cug jon) :: XX wait for session save?
      ==
    ::
    ++  check-password
      |=  pas/{ship @t}  ^-  ?
      =+  (scry-jael %pass pas)
      ?>  ?=($bean -<)
      ->
    ::
    ++  show-login-page
      ^-  (each pest _done)
      ?.  ?=($@($~ {$~ $html}) p.pok)
        [%& %red ~]
      [%& %htme login-page:xml]
    ::
    ++  need-ixor  (oryx-to-ixor (need grab-oryx))
    ++  for-view  ^+(ix (fix-user:(ire-ix need-ixor) ses:for-client))
    ::
    ++  random-session  (rsh 3 1 (scot %p (end 6 1 ney)))
    ++  for-authed-client
      |=  him/ship  ^+  [dom=*(unit @t) ya]
      ?>  =(him our)  :: XX SSO
      (new-ya &)
    ::
    ++  for-client                        ::  stateful per-session engine
      ^+  [dom=*(unit @t) ya]
      =+  pef=cookie-prefix
      =+  lig=(session-from-cookies pef maf)
      ?~  lig
        (new-ya |)
      ?~  ~(get-user ya u.lig)
        ~&  bad-cookie+u.lig
        (new-ya |)
      [~ ~(. ya u.lig)]
    ::

    ++  cookie-domain
      ^-  cord
      ?-  r.hat
        {$| @}  (cat 3 '; Domain=' (rsh 3 1 (scot %if p.r.hat)))
        {$& $org $urbit *}  '; Domain=.urbit.org'
        {$& @ @ *}  =-  (rap 3 "; Domain={-}{i.p.r.hat ~}")
                    (turn (flop `path`t.p.r.hat) |=(a/knot (cat 3 a '.')))
        {$& *}  ''  ::  XX security?
      ==
    ::
    ++  new-ya  |=(own/? [`cookie-domain %.(own ~(new ya random-session))])
    --
  ::
  ++  oryx-to-ixor  |=(a/oryx (rsh 3 1 (scot %p (end 6 1 (shas %ire a)))))
  ++  ya                                                ::  session engine
    ~%  %eyre-y  ..is  ~
    |_  ses/hole
    ++  abet  ..ya
    ++  abut  (jael-note / %kill-cookie ses)
    ++  new  |=(own/? +>(..ya (jael-note / %save-cookie ses own)))
    ::
    ++  set-cookie
      |=  domain/(unit @t)  ^-  (list @t)
      ?~  domain  ~
      [(^set-cookie u.domain cookie-prefix ses)]~
    ::
    ++  new-view
      ^+  [*oryx ..ya]
      =+  orx=`@t`(rsh 3 1 (scot %p (shaf %orx eny)))
      =+  ire=(oryx-to-ixor orx)
      [orx %.(ses ~(init ix ire %*(. *stem him anon, p.eve 1)))] :: XX fix him on ack?
    ::
    ++  stat-json
      ^+  [*json ..ya]
      =^  orx  ..ya  new-view
      :_  ..ya
      %-  jobe  :~
        oryx+s+orx
        ixor+s+(oryx-to-ixor orx)
        sein+(jape +:<(sein:title our)>)
        ship+(jape +:<our>)
        user+(jape +:<(fall get-user anon)>) :: XX crash on unsaved session?
      ==
    ::
    ++  get-user
      ^-  (unit ship)
      =+  (scry-jael %cook ses)
      ?>  ?=($u-ship -<)
      ->
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
      =+  sub=(~(tap in sus))
      |-  ^+  ..ix
      ?^  sub  $(sub t.sub, ..ix (pul-subs i.sub))
      ..ix(wix (~(del by wix) ire))
    ::
    ++  teba  |*(a/$-(* ..ix) |*(b/* %_(done ..ix (a b))))
    ++  give-json  (teba ^give-json)
    ++  pass-note  (teba ^pass-note)
    ++  hurl-note
      |=  {a/{dock path} b/note}  ^+  ..ix
      =:  med  (~(put to med) hen)
          hen  `~
        ==
      :: ~&  >  hurl+[&2.b ire a]
      (pass-note:abet [%of ire (gsig a)] b)
    ::
    ++  init
      |=  ses/hole  ^+  ..ix
      (jael-note:abet of+/[ire] %save-token ses ire)
    ::
    ++  fix-user
      |=  ses/hole  ^+  +>
      ?.  =(anon him)  +>
      +>(him (need ~(get-user ya ses)))  :: XX set correct value on session create
    ::
    ++  add-even
      |=  a/even  ^+  eve
      [+(p.eve) (~(put by q.eve) p.eve a)]
    ::
    ++  new-mess
      |=  {a/dock b/mark c/wire d/cage}  ^+  ..ix
      (hurl-note [a c] [%g %deal [him -.a] +.a %punk b d])
    ::
    ++  add-subs
      |=  {a/dock $json b/wire c/path}  ^+  ..ix
      ?:  (~(has in sus) +<)  ~|(duplicate+c !!)
      =.  sus  (~(put in sus) +<)
      (hurl-note [a b] [%g %deal [him -.a] +.a %peel %json c])
    ::
    ++  pul-subs
      |=  {a/dock $json b/wire c/path}  ^+  ..ix
      =.  sus  (~(del in sus) +<)
      (hurl-note [a b] [%g %deal [him -.a] +.a %pull ~])
    ::
    ++  del-subs                      ::  XX per path?
      |=  {a/dock $json b/wire c/path}  ^+  ..ix
      =.  ..ix  (pul-subs +<)
      (nice-json:pop-duct:(ire-ix ire))          ::  XX gall ack
    ::
    ++  get-rush
      |=  {a/whir-of b/json}  ^+  ..ix
      (get-even [%rush [[(slav %p p.a) q.a] s.a] (joba %json b)])
    ::
    ++  get-quit
      |=  a/whir-of  ^+  ..ix
      (get-even [%quit [[(slav %p p.a) q.a] s.a]])
    ::
    ++  get-ack
      |=  {a/whir-of b/(unit {term tang})}  ^+  ..ix
      ?:  =(~ med)  ~&  resp-lost+ire  ..ix
      ?~  b  (nice-json:pop-duct)
      (mean-json:pop-duct 500 b)
    ::
    ++  get-even
      |=  ven/even  ^+  ..ix
      =+  num=p.eve
      =.  eve  (add-even ven)
      =<  abet
      ?~  pol  done
      =.  hen  u.pol
      (give-even(pol ~) num ven)
    ::
    ++  give-even
      |=  {num/@u ven/even}  ^+  done
      =:  q.eve  (~(del by q.eve) (dec num))              ::  TODO ponder a-2
          mow    ?.(?=($rush -.ven) mow mow:(pass-took [- %mess +]:p.ven))
        ==
      %^  give-json  200  ~
      %^  jobe  id+(jone num)  type+[%s -.ven]
      ?-  -.ven
        $news  ~[from+[%s (scot %uv p.ven)]]
        $quit  ~[from+(subs-to-json p.ven)]
        $rush  ~[from+(subs-to-json p.ven) data+q.ven]
      ==
    ::
    ++  pass-took
      |=  a/{p/dock wire}
      %+  pass-note(hen `~)
        [%of ire (gsig a)]
      [%g %deal [him -.p.a] +.p.a %pump ~]
    ::
    ++  pop-duct  =^(ned med ~(get to med) abet(hen ned))
    ++  poll
      |=  {seq/@u ses/hole}  ^+  ..ix
      =<  abet
      =.  ..ix  (jael-note of+/[ire] %live-token ses ire)
      ?:  =(seq p.eve)
        =.  lyv  (~(put by lyv) hen [%poll ire])
        done(pol `hen)
      ?:  (gth seq p.eve)  ~|(seq-high+cur=p.eve !!)
      =+  ven=~|(seq-low+cur=p.eve (~(got by q.eve) seq))
      (give-even seq ven)
    ::
    ++  poll-dead
      ^+  ..ix
      =<  abet
      ?.  =(pol `hen)
        done  :: old long poll
      done(pol ~)
    ::
    ++  subs-to-json
      |=  {a/dock b/path}
      %-  jobe  :~
        ship+[%s (rsh 3 1 (scot %p p.a))]
        appl+[%s q.a]
        path+(jape (spud b))
      ==
    ::
    ++  get-jael
      =*  jael-gift-token  :: XX types
        =>  (mini-jael-gift /token-ack)
        ?>(?=(?($token-ack $token-dead $token-beat) -) _.)
      |=  a/jael-gift-token  ^+  ..ix
      ?-  -.a
        $token-ack  abet
        $token-dead  abut :: notify?
        $token-beat
          ?~  pol  abet  :: recieved other response
          ~?  !=(hen u.pol)  [%oryx-beat-weird-duct hen]
          (give-json:abet(pol ~, hen u.pol) 200 ~ (joba %beat %b &))
      ==
    --
  ++  lens                                              :: urb.py engine
    =/  him  our                      :: XX other uses?
    |_  $~                            :: XX stateful?
    ++  abet  ..lens
    ++  new
      |=  jon/json  ^+  ..lens
      =.  ..lens
        %+  pass-note  [%le ~]
        [%g %deal [him our] %dojo %peel %lens-json /sole]
      =.  ..lens
        %+  pass-note  [%le ~]
        [%g %deal [him our] %dojo %punk %lens-command %json !>(`json`jon)]
      abet
    ::
    ++  get-ack
      |=  a/(unit (pair term tang))  ^+  ..lens
      ?~  a
        ..lens  :: (give-json 200 ~ (joba %okey-dokey %b &))
      =+  tag=(flop `tang`[>[%eyre-lens-fail p.u.a]< q.u.a])
      %-  (slog tag)
      (give-json:abet 500 ~ (jape (of-wall (wush 160 tag))))
    ::
    ++  get-diff
      |=  fec/json  ^+  ..lens
      ?~  fec  ..lens                 ::  nulled event we don't care about
      =.  ..lens
        %+  pass-note  [%le ~]
        `note`[%g %deal [him our] %dojo %pull ~]
      (give-json:abet 200 ~ fec)
    ::
    ++  get-quit  (give-json:abet 500 ~ (joba %quit b+&))
    --
  ++  vi                                                ::  auth engine
    ~%  %eyre-v  ..is  ~
    |_  $:  {usr/user dom/path}
            cor/(unit $@($~ vase))
            {liv/? req/(qeu {p/duct q/mark r/vase:hiss})}
        ==
    ++  self  .
    ++  abet  +>(sec (~(put by sec) +<- +<+))
    ++  execute  
      |=({a/whir-se b/{beak silk:^ford}} (execute:abet se+[a usr dom] b))
    ++  dead-this  |=(a/tang (fail:abet 500 0v0 a))
    ++  dead-hiss  |=(a/tang pump(req ~(nap to req), ..vi (give-sigh %| a)))
    ++  eyre-them  |=({a/whir-se b/vase} (eyre-them:abet se+[a usr dom] b))
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
      %^  execute  %core  root-beak
      :::+  %dude  [|.(+)]:>%mod-samp<
      ^-  silk:^ford
      :^  %mute  core+[root-beak (flop %_(dom . sec+dom))]
        [[%& 12]~ %$ bale+!>(*(bale @))]  :: XX specify on type?
      ?~  cor  ~
      ?~  u.cor  ~
      ?:  (has-arm %discard-state)  ~
      ?:  (has-arm %update)
        [[%& 13]~ ride+[limb+%update prep-cor]]~
      [[%& 13]~ %$ noun+(slot 13 u.cor)]~
    ::
    ++  call
      |=  {arm/vi-arm sam/cage}
      %^  execute  arm  root-beak
      call+[ride+[limb+arm prep-cor] [%$ sam]]
    ::
    ++  prep-cor  ^-  silk:^ford
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
          ?~(- ~ (de:crua load-secret u))  :: XX clay permissions
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
      =^  ole  req  ~(get to req)
      =>  .(ole `{p/duct q/mark *}`ole)             :: XX types
      =.  ..vi  (cast-thou(hen p.ole) q.ole httr+vax)    :: error?
      pump
    ::
    ::  Interfaces
    ::
    ++  get-news  _build
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
      |=  {wir/whir-se dep/@uvH res/(each cage tang)}  ^+  abet
      ?:  ?=($core wir)  (made-core dep res)
      %.  res
      ?-  wir
        ?($filter-request $out)             made-filter-request
        ?($filter-response $res)            made-filter-response
        ?($receive-auth-response $bak)      made-receive-auth-response
        ?($receive-auth-query-string $in)   made-receive-auth-query-string
      ==
    ::
    ++  made-core
      |=  {dep/@uvH gag/(each cage tang)}
      :: ~&  got-update/dep
      =.  ..vi  (pass-note %core [%f [%wasp our dep &]])
      ?~  -.gag  pump(cor `q.p.gag)
      ?:  &(=(~ cor) =(%$ usr))
        =.  cor  `~
        pump ::(cor `~)  :: userless %hiss defaults to "nop" driver
      (warn p.gag)
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
    ++  auth-print  |=({$show a/purl} (slog auth-tank leaf+(earn a) ~))
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
      =.  liv  |  :: block requests until a reponse is given
      (eyre-them wir (slam !>(|=({$send a/hiss} a)) res))
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
        (q.i.a (spec b))
      $(a t.a)
    ::
    ++  on-ford-fail
      |=  {err/$-(tang _abet) try/$-((each cage tang) _abet)}
      |=  a/(each cage tang)  ^+  abet
      ?-(-.a $| (err p.a), $& (try a))
    ::
    ++  on-error
      |=  {err/$-(tang _abet) handle-move/_|.(|~(vase abet))}
      |=  a/(each cage tang)  ^+  abet
      =+  try=(possibly-stateful |=(b/_self (handle-move(+ b))))  :: XX types
      ?:  ?=($| -.a)  (err p.a)
      =-  ?-(-.- $& p.-, $| (err p.-))
      (mule |.(~|(driver+dom ~|(bad-res+p.q.p.a (try q.p.a)))))
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
|=  {now/@da eny/@ ski/sley}                            ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  call                                                ::  handle request
  |=  $:  hen/duct
          hic/(hypo (hobo task:able))
      ==
  =>  %=    .                                           ::  XX temporary
          q.hic
        ^-  task:able
        ?:  ?=($soft -.q.hic)
          ((hard task:able) p.q.hic)
        ?:  (~(nest ut -:!>(*task:able)) | p.hic)  q.hic
        ~&  [%eyre-call-flub (@tas `*`-.q.hic)]
        ((hard task:able) q.hic)
      ==
  ^+  [p=*(list move) q=..^$]
  ?:  ?=($wegh -.q.hic)
    :_  ..^$  :_  ~
    :^  hen  %give  %mass
    :-  %eyre
    :-  %|
    :~  dependencies+[%& liz]  views+[%& wix]
        ducts+[%| ~[dead+[%& ded] proxy+[%& pox] outgoing+[%& ask]]]
        misc+[%& bol]
    ==
  =+  our=`@p`0x100  ::  XX  sentinel
  =+  ska=(sloy ski)
  =+  sky=|=({* *} `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ^+  [p=*(list move) q=..^$]
  =^  mos  bol
    abet:(apex:~(adit ye [hen [now eny our sky] ~] bol) q.hic)
  [mos ..^$]
::
++  doze                                                ::  require no timer
  |=  {now/@da hen/duct}
  ^-  (unit @da)
  ~
::
++  load                                                ::  take previous state
  =+  bolo-6={$6 _%*(+ *bolo lyv *(map duct ^), wix [*(map) *(map)])}
  =+  driv-5=_=>(*driv [cor=p req=req.q])
  =+  bolo-5={$5 _=+(*bolo-6 +.-(sec (~(run by sec.-) driv-5)))}
  =+  bolo-4={$4 _%*(+ *bolo-5 lyv *(map duct ^))}
  =/  bolo  _%*(. *bolo lyv **)
  ::|=  *  %.  (bolo +<)
  |=  old/?(bolo bolo-6 bolo-5 bolo-4)
  ?-  -.old
    $7  ..^$(+>- old(lyv ~))
    $6  $(old [%7 +.old(lyv ~, wix ~)])
    $5  $(old [%6 +.old(sec (~(run by sec.old) |=(driv-5 [cor & req])))])
    $4  $(old [%5 +.old(lyv ~)])          :: minor leak
  ==
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=($& -.why)  ~
  =*  who  p.why
  =+  our=(need hov)                  :: XX single home
  =+  ska=(sloy ski)
  =+  sky=|=({* *} `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  ?.  ?=($$ ren)  [~ ~]
  ?.  ?=($$ -.lot)  [~ ~]
  ?+    syd  [~ ~]
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
        ``~(our-host ye [`duct`~[/] [now eny our sky] ~] bol)
    ==
  ==
::
++  stay  `bolo`+>-.$
++  take                                                ::  accept response
  |=  {tea/wire hen/duct hin/(hypo sign)}
  ^+  [p=*(list move) q=..^$]
  =+  our=`@p`0x100  ::  XX  sentinel
  =+  ska=(sloy ski)
  =+  sky=|=({* *} `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ^+  [p=*(list move) q=..^$]
  =+  tee=((soft whir) tea)
  ?~  tee  ~&  [%e %lost -.q.hin hen]  [~ ..^$]
  =^  mos  bol
    =<  abet
    (axon:~(adit ye [hen [now eny our sky] ~] bol) u.tee q.hin)
  [mos ..^$]
--
