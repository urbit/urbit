!:  ::  %eyre, http servant
!?  164
::::
|=  pit=vase
=>  =~
|%                                                      ::  interfaces
++  gift  gift-eyre                                     ::  out result <-$
++  kiss  kiss-eyre                                     ::  in request ->$
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  note                                                ::  out request $->
          $%  $:  %a                                    ::  to %ames
          $%  [%wont p=sock q=[path *]]                 ::
          ==  ==                                        ::
              $:  %b                                    ::  to  %behn
          $%  [%wait p=@da]                             ::
              [%rest p=@da]                             ::
          ==  ==                                        ::
              $:  %d                                    ::  to %dill
          $%  [%flog p=[%crud p=@tas q=(list tank)]]    ::
          ==  ==                                        ::
              $:  %e                                    ::  to self
          $%  [%thud ~]                                 ::  proxied death
              [%this p=? q=clip r=httq]                 ::  proxied request
              [%meta vase:,[%them (unit httr)]]         ::  type check
          ==  ==                                        ::
              $:  %f                                    ::  to %ford
          $%  [%exec p=@p q=(unit ,[beak silk])]        ::
              [%wasp p=@p q=@uvH r=?]                   ::
          ==  ==                                        ::
              $:  %g                                    ::  to %gall
          $%  [%deal p=sock q=cush]                     ::  full transmission
          ==  ==  ==                                    ::
++  sign                                                ::  in result $<-
          $?  $:  %a                                    ::  by %aformedmes
          $%  [%woot p=ship q=coop]                     ::
          ==  ==                                        ::
              $:  %b                                    ::  by %behn
          $%  [%wake ~]                                 ::  timer activate
          ==  ==                                        ::
              $:  %g                                    ::  by %gall
          $%  [%unto p=cuft]                            ::  within agent
          ==  ==                                        ::
              $:  %e                                    ::  by self
          $%  [%thou p=httr]                            ::  response for proxy
          ==  ==                                        ::
              $:  %f                                    ::  by %ford
          $%  [%made p=@uvH q=gage]                     ::
              [%news p=@uvH]                            ::
          ==  ==                                        ::
              $:  @tas                                  ::  by any
          $%  [%crud p=@tas q=(list tank)]              ::
          ==  ==  ==                                    ::
++  ixor  ,@t                                           ::  oryx hash
++  whir  $|  ~                                         ::  wire subset
          $%  [%at p=hole q=whir]                       ::  authenticated
              [%ay p=span:ship q=span:,@uvH ~]          ::  remote duct
              [%ha p=path:beak]                         ::  GET request
              [%he p=whir]                              ::  HEAD request
              [%hi p=mark ~]                            ::  outbound HTTP
              [%se p=whir-se q=(list ,@t)]              ::  outbound to domain 
              [%si ~]                                   ::  response done
              [%of p=ixor q=$|(~ whir-of)]              ::  associated view
              [%ow p=ixor ~]                            ::  dying view
              [%on ~]                                   ::  dependency
          ==                                            ::
++  whir-of  ,[p=span:ship q=term r=wire]               ::  path in dock
++  whir-se
  $?  %core                                             ::  build agent
      %bak                                              ::  ++bak auth response
      %out                                              ::  ++out mod request
      %in                                               ::  ++in handle code 
  ==                                                    ::
--                                                      ::
|%                                                      ::  models
++  bolo                                                ::  eyre state
  $:  %4                                                ::  version
      gub=@t                                            ::  random identity
      hov=(unit ship)                                   ::  master for remote
      ged=duct                                          ::  client interface
      ded=(set duct)                                    ::  killed requests
      lyv=(map duct live)                               ::  living requests
      pox=(map ,@uvH duct)                              ::  proxied sessions
      ask=[p=@ud q=(map ,@ud ,[p=duct q=hiss])]         ::  outgoing by number
      kes=(map duct ,@ud)                               ::  outgoing by duct
      ney=@uvI                                          ::  rolling entropy
      dop=(map host ship)                               ::  host aliasing
      liz=(jug ,@uvH (each duct ixor))                  ::  ford depsets
      wup=(map hole cyst)                               ::  secure sessions
      sop=(map hole ,[ship ?])                          ::  foreign sess names
      wix=(map ixor stem)                               ::  open views
      sec=(map (list ,@t) driv)                         ::  security drivers
  ==                                                    ::
::
++  driv  %+  pair  (unit vase)                         ::  driver state
          (qeu (trel duct mark vase:hiss))              ::  waiting requests
::
++  live                                                ::  in flight
  $%  [%exec p=whir]                                    ::  ford build
      [%wasp p=(list ,@uvH)]                            ::  ford deps
      [%xeno p=ship]                                    ::  proxied request
      [%poll p=ixor]                                    ::  session state
  ==
++  cyst                                                ::  client session
  $:  ced=cred                                          ::  credential
      [him=ship aut=(set ship)]                         ::  authenticated
      cug=(list ,@t)                                    ::  unacked cookies
      lax=@da                                           ::  last used
      way=(map ship ,[purl duct])                       ::  waiting auth
      vew=(set oryx)                                    ::  open views XX expire
  ==                                                    ::
::
++  stem                                                ::  client view
  $:  him=ship                                          ::  static identity
      ude=(unit ,[p=duct q=?])                          ::  stream, long-poll?
      era=@da                                           ::  next wake
      die=@da                                           ::  collection date
      sus=(set ,[dock %json wire path])                 ::  subscriptions
      eve=[p=@u q=(map ,@u even)]                       ::  queued events
      med=(qeu duct)                                    ::  waiting /~/to/
  ==
::++  honk  $%([%nice ~] [%mean p=ares])                  ::  old gall result
++  even                                                ::  client event
  $%  [%news p=@uvH]
      [%quit p=[dock path]]
      [%rush p=[dock path] q=json]
  ==
::
++  perk                                                ::  parsed request
  $%  [%auth p=perk-auth]
      [%away ~]
      [%oath p=(list ,@t)]
      [%bugs p=?(%as %to) ~]
      [%beam p=beam]
      [%deps p=?(%put %delt) q=@uvH]
      [%mess p=dock q=mark r=wire s=json]
      [%poll p=[i=@uvH t=(list ,@uvH)]]
      [%spur p=spur]
      [%subs p=?(%put %delt) q=[dock %json wire path]]
      [%view p=ixor q=[~ u=@ud]]
  ==
::
++  perk-auth                                           ::  parsed auth
  $%  [%at p=pork]                                      ::  inject auth
      [%del p=(unit ship)]
      [%get him=ship rem=pork]
      [%js ~]
      [%json ~]
      [%try him=ship paz=(unit cord)]
      [%xen ses=hole rem=pork]
  ==
::
++  pest                                                ::  result
  $|  ~
  $%  [%$ p=httr]                                       ::  direct response
      [%red ~]                                          ::  parent redirect
      [%boil p=whir q=term r=beam]                      ::  ford request
  :: 
      [%js p=@t]                                        ::  script
      [%json p=json]                                    ::  data
      [%html p=manx]                                    ::  successful page
      [%htme p=manx]                                    ::  authentication fail 
  ==
--                                                      ::
|%
++  eat-headers
  |=  hed=(list ,[p=@t q=@t])  ^-  math
  %+  roll  hed
  |=  [a=[p=cord cord] b=math] 
  =.  p.a  (cass (trip p.a))
  (~(add ja b) a)
::
++  fcgi                                                ::  credential caboose
  |=  [quy=quay ced=cred]  ^-  coin
  :*  %many
      [%$ %ta ~]
      [%blob ced]
      |-  ^-  (list coin)
      ?~  quy  ~
      [[%$ %t p.i.quy] [%$ %t q.i.quy] $(quy t.quy)]
  ==
::
++  gsig  |=([a=dock b=path] [(scot %p p.a) q.a b])
++  session-from-cookies
  |=  [nam=@t maf=math]
  ^-  (unit hole)
  (from-cookies maf |=([k=@t v=@] &(=(nam k) !=('~' v))))
::
++  ship-from-cookies
  |=  maf=math  ^-  (unit ship)
  (biff (from-cookies maf |=([k=@ @] =(%ship k))) (slat %p))
::
++  from-cookies
  |=  [maf=math fil=$+([@t @t] ?)]
  =+  `cot=(list ,@t)`(~(get ju maf) 'cookie')
  =+  `cok=quay`(zing `(list quay)`(murn cot (curr rush cock:epur)))
  |-  ^-  (unit cord)
  ?~  cok  ~
  ?:((fil i.cok) [~ q.i.cok] $(cok t.cok))
::
++  wush
  |=  [wid=@u tan=tang]
  ^-  tape
  =+  rolt=|=(a=wall `tape`?~(a ~ :(weld i.a "\0a" $(a t.a))))
  (rolt (turn tan |=(a=tank (rolt (wash 0^wid a)))))
::
::
++  add-cookies
  |=  [cug=(list ,@t) hit=httr]  ^-  httr
  ?~  cug  hit
  =+  cuh=(turn `(list ,@t)`cug |=(a=@t set-cookie/a))
  hit(q (weld cuh q.hit))
::
++  inject                                            ::  inject dependency
  |=  [dep=@uvH max=[[%html ~] [[%head ~] hed=marl] [[%body ~] tal=marl] ~]]
  ^-  manx
  =:  hed.max  :_(hed.max ;meta(charset "utf-8", urb_injected "");)
      tal.max  (welp tal.max ;script(urb_injected ""):"{(trip etag:js)}" ~)
    ==
  ?~  dep  max
  max(hed :_(hed.max ;script@"/~/on/{<dep>}.js"(urb_injected "");))
::
++  add-json                                            ::  inject window.urb
  |=  [urb=json jaz=cord]  ^-  cord 
  =-  (cat 3 (crip -) jaz)
  """
  var _urb = {(pojo urb)}
  window.urb = window.urb || \{}; for(k in _urb) window.urb[k] = _urb[k]
  
  """
::
++  ares-to-json
  |=  err=ares  ^-  json
  =-  (jobe fail/s/typ mess/(jape mez) ~)
  ^-  [typ=term mez=tape]
  ?~  err  [%fail "Unknown Error"]
  [p.u.err (wush 160 q.u.err)]
::
++  resp                                                ::  mimed response
  |=  [sas=@uG mit=mite rez=@]  ^-  httr
  ::  (weld (turn cug |=(a=@t ['set-cookie' a]))
  [sas ~[content-type/(moon mit)] [~ (taco rez)]]
::
++  render-tang                                         ::  tanks to manx
  |=  tan=tang
  ;html
    ;head
      ;link(rel "stylesheet", href "/home/lib/base.css"); ::  XX desk
      ;title: server error
    ==
    ;body:div#c.err:pre:code:"{(wush 80 tan)}"
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
      urb.wreq.open('GET', urb.wurl, true)
      urb.wreq.addEventListener('load', function() {
        // if(~~(this.status / 100) == 4)
        //   return document.write(this.responseText)
        if(this.status !== 205) {
          return urb.keep()
        }
        urb.onupdate()
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
      var old = /[^/]*$/.exec(urb.wurl)[0]
      var deps = old.replace(/^on.json\?|.json$/,'').split('&')
      if (deps.indexOf(deh) !== -1) return;
      deps.push(deh)
      urb.wurl = "/~/on.json?"+deps.join('&')
      urb.wreq.abort() // trigger keep 
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
    urb.redir = function(ship){
      if(ship) document.location.pathname =
        document.location.pathname.replace(/^\/~~|\/~\/as\/any/,'/~/as/~'+ship)
      else document.location = 
        document.location.hash.match(/#[^?]+/)[0].slice(1) +
        document.location.pathname.replace(
          /^\/~\/am\/[^/]+/,
          '/~/as/~' + urb.ship) +
        document.location.search
    }
    if(urb.foreign && urb.auth.indexOf(urb.ship) !== -1){
      req("/~/auth.json?PUT",
          {ship:urb.ship,code:null},
          function(){urb.redir()})
    }
    urb.submit = function(){
      if(urb.ship !== $ship.text().toLowerCase())
        return urb.redir($ship.text().toLowerCase())    //  XX redundant?
      req(
        "/~/auth.json?PUT", 
        {ship:$ship.text().toLowerCase(), code:pass.value},
        function(){
          if(urb.foreign) urb.redir()
          else document.location.reload()
      })
    }
    urb.away = function(){req("/~/auth.json?DELETE", {}, 
      function(){document.getElementById("c").innerHTML = "<p>Goodbye.</p>" }
    )}
    '''
  ++  etag
    '''
    if(!window.urb) window.urb = {}
    urb.waspFrom = function(sel,attr){
      Array.prototype.map.call(document.querySelectorAll(sel), 
        function(ele){
          if(!ele[attr] || (new URL(ele[attr])).host != document.location.host)
            return;
          var xhr = new XMLHttpRequest()
          xhr.open("HEAD", ele[attr])
          xhr.send()
          xhr.onload = function(){
            var dep = this.getResponseHeader("etag")
            if(dep) urb.wasp(JSON.parse(dep.substr(2)))
    }})}
    if(urb.wasp){urb.waspFrom('script','src'); urb.waspFrom('link','href')}
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
  ++  login-page
    %+  titl  'Log in :urbit'
    ;=  ;h1: Please log in
        ;p.ship 
          ;div.sig: ~
          ;span#ship(contenteditable "");
        ==
        ;input#pass(type "password");
        ;h2.advice: (Your login code has been printed to your console.)
        ;pre:code#err;
        ;script@"/~/at/~/auth.js";
        ;script:'''
                $(function() {
                  $ship = $('#ship')
                  $pass = $('#pass')
                  $ship.on('keydown', function(e) { 
                    if(e.keyCode === 13 || e.keyCode === 9) {
                      if(urb.ship !== $ship.text().toLowerCase())
                        urb.redir($ship.text().toLowerCase())
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
                      urb.submit()
                    }
                  })
                  if(window.ship) {
                    $ship.text(urb.ship)
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
    ;=  ;h1: Goodbye ~;{span#ship}.
        ;button#act(onclick "urb.away()"): Log out
        ;pre:code#err;
        ;script@"/~/at/~/auth.js";
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
    |=  [a=cord b=marl] 
    ;html
      ;head
        ;meta(charset "utf-8");
        ;meta(name "viewport", content "width=device-width, ".
        "height=device-height, initial-scale=1.0, user-scalable=0, ".
        "minimum-scale=1.0, maximum-scale=1.0");
        ;title:"{(trip a)}" 
        ;script(type "text/javascript", src "//cdnjs.cloudflare.com/ajax/".
          "libs/jquery/2.1.1/jquery.min.js");
        ;link(rel "stylesheet", href "/home/lib/base.css");
      ==
      ;body:div#c:"*{b}"
    ==
  --
--
|%                                                      ::  functions
++  ye                                                  ::  per event
  =|  $:  $:  hen=duct                                  ::  event floor
              $:  now=@da                               ::  event date
                  eny=@                                 ::  unique entropy
                  our=ship                              ::  current ship
                  sky=$+(* (unit))                      ::  system namespace
              ==                                        ::
              mow=(list move)                           ::  pending actions
          ==                                            ::
          bolo                                          ::  all vane state
      ==                                                ::
  =*  bol  ->
  |%
  ++  abet                                              ::  resolve moves
    ^-  [(list move) bolo]
    [(flop mow) bol]
  ::
  ++  adit  .(ney (mix eny ney))                        ::  flip entropy
  ::
  ++  anon  `@p`(add our ^~((bex 64)))                  ::  pseudo-sub
  ++  apex                                              ::  accept request
    |=  kyz=kiss
    ^+  +>
    =.  our  ?~(hov our u.hov)  ::  XX
    ?-    -.kyz
        %born  +>.$(ged hen)                            ::  register external
        %crud
      +>.$(mow [[hen %slip %d %flog kyz] mow])
    ::
        %init                                           ::  register ownership
      +>.$(hov ?~(hov [~ p.kyz] [~ (min u.hov p.kyz)]))
    ::
        %this                                           ::  inbound request
      %-  emule  |.  ^+  ..apex
      =*  sec  p.kyz    ::  ?                           ::  https bit
      =*  heq  r.kyz    ::  httq                        ::  request content
      =+  ryp=`quri`(rash q.heq zest:epur)
      =+  maf=(eat-headers r.heq)
      =+  ^=  pul  ^-  purl
          ?-  -.ryp
            &  ?>(=(sec p.p.p.ryp) p.ryp)
            |  =+  hot=(~(get ja maf) %host)
               ?>  ?=([@ ~] hot)
               [[sec (rash i.hot thor:epur)] p.ryp q.ryp]
          ==
      =.  p.p.pul  |(p.p.pul ?=(hoke r.p.pul))
      =+  her=(host-to-ship r.p.pul)
      ?:  |(?=(~ her) =(our u.her))
        (handle pul [q.+.kyz |] [p.heq maf s.heq])
      =+  han=(sham hen)
      =.  pox  (~(put by pox) han hen)
      (ames-gram u.her [%get ~] han +.kyz)
    ::
        %them                                           ::  outbound request
      ?~  p.kyz
        =+  sud=(need (~(get by kes) hen))
        %=  +>.$
          mow    :_(mow [ged [%give %thus sud ~]])
          q.ask  (~(del by q.ask) sud)
          kes    (~(del by kes) hen)
        ==
      ::  ~&  eyre-them/(earn p.u.p.kyz)
      %=  +>.$
        mow    :_(mow [ged [%give %thus p.ask p.kyz]])
        p.ask  +(p.ask)
        q.ask  (~(put by q.ask) p.ask hen u.p.kyz)
        kes    (~(put by kes) hen p.ask)
      ==
    ::
        %hiss                                           ::  outbound cage 
      ::?~  p.kyz                                       ::  XX cancel
      ::  =+  sud=(need (~(get by kes) hen))
      ::  %=  +>.$
      ::    mow    :_(mow [ged [%give %thus sud ~]])
      ::    q.ask  (~(del by q.ask) sud)
      ::    kes    (~(del by kes) hen)
      ::  ==
      ::  ~&  eyre-them/(earn p.u.p.kyz)
      (back hi//[p.kyz] %hiss q.kyz)
    ::
        %they                                           ::  inbound response
      =+  kas=(need (~(get by q.ask) p.kyz))
      ::  ~&  >  eyre-they/[p.q.kyz (earn p.q.kas)]
      %=  +>.$
        mow    :_(mow [p.kas [%give %thou q.kyz]])
        q.ask  (~(del by q.ask) p.kas)
      ==
    ::
        %thud                                           ::  cancel request
      ?.  (~(has by lyv) hen)
        ~&  dead-request/hen
        +>.$(ded (~(put in ded) hen))                   ::  uncaught requests
      =+  lid=(~(got by lyv) hen)
      :: ~&  did-thud/[-.lid hen]
      ?-  -.lid
          %exec
        (pass-note p.lid %f [%exec our ~])
      ::
          %poll
        ?.  (~(has by wix) p.lid)
         +>.$ 
        poll-dead:(ire-ix p.lid)
      ::
          %xeno
        =+  han=(sham hen)
        =.  pox  (~(del by pox) han hen)
        (ames-gram p.lid [%gib ~] han)
      ::
          %wasp
        |-  ^+  +>.^$
        ?~  p.lid  +>.^$
        (del-deps:$(p.lid t.p.lid) i.p.lid %& hen)
      ==
    ::
        %west                                           ::  remote request
      =.  mow  :_(mow [hen %give %mack ~])
      =+  mez=((soft gram) q.kyz)
      ?~  mez
        ~&  e/[%strange-west p.kyz]
        ~|(%strange-west !!)
      ?-  -<.u.mez
        %aut  abet:(logon:(ses-ya p.u.mez) q.p.kyz)
        %hat  (foreign-hat:(ses-ya p.u.mez) q.p.kyz q.u.mez)
        %gib  (pass-note ay/(dray p/uv/~ q.p.kyz p.u.mez) [%e %thud ~])
        %get  (pass-note ay/(dray p/uv/~ q.p.kyz p.u.mez) [%e %this q.u.mez])
        %got
          ?.  (~(has by pox) p.u.mez)
            ~&  lost-gram-thou/p.kyz^p.u.mez
            +>.$
          =:  hen  (~(got by pox) p.u.mez)
              pox  (~(del by pox) p.u.mez)
            ==
          (give-thou q.u.mez)
      ::
        %lon  
          ~&  ses-ask/[p.u.mez sop (~(run by wup) ,~)]
          ?:  (ses-authed p.u.mez)  
            (ames-gram q.p.kyz aut/~ p.u.mez)
          =.  sop  (~(put by sop) p.u.mez q.p.kyz |)
          (ames-gram q.p.kyz hat/~ p.u.mez our-host)
      ==
    ::
      %wegh  !!                                         ::  handled elsewhere
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
    |=  [tee=whir typ=type sih=sign]
    ^+  +>
    =.  our  ?~(hov our u.hov)  ::  XX
    ?:  &(?=([?(%of %ow) ^] tee) !(~(has by wix) p.tee))
      ~&(dead-ire/[`whir`tee (,[term term ~] +.sih)] +>)
    ?-    &2.sih
        %crud  +>.$(mow [[hen %slip %d %flog +.sih] mow])
    ::  %dumb  
    ::    =.  +>  ?+(tee +> [%of ^] pop-duct:(ire-ix p.tee))
    ::    (emule |.(~|(gall-dumb/tee !!)))
    ::
        %woot  +>.$
        %thou
      ?+    -.tee  !!
        %ay  (ames-gram (slav %p p.tee) got/~ (slav %uv q.tee) |2.sih)
        %hi  (cast-thou p.tee p.sih)
        %se  (get-thou:(dom-vi q.tee) p.tee p.sih)
      ==
    ::
        %unto                                           ::  app response
      ?>  ?=([%of @ ^] tee)
      =+  cuf=`cuft`+>.sih
      ?-    -.cuf
          ?(%coup %reap)
        (get-ack:(ire-ix p.tee) q.tee ?~(p.cuf ~ `[-.cuf u.p.cuf]))
      ::
          %doff  !!
          %diff
        ?.  ?=(%json p.p.cuf)
          :: ~>  %slog.`%*(. >[%backing p.p.cuf %q-p-cuf]< &3.+> (sell q.p.cuf))
          (back tee %json p.cuf)
        (get-rush:(ire-ix p.tee) q.tee ((hard json) q.q.p.cuf))
      ::
          %quit  ~&(quit/tee (get-quit:(ire-ix p.tee) q.tee))
      ==
    ::
        %wake
      ?>  ?=([?(%of %ow) @ ~] tee)
      ?:  ?=(%ow -.tee)
        abut:(ire-ix p.tee)
      =>  wake:(ire-ix p.tee)
      (give-json 200 ~ (joba %beat %b &))
    ::
        %news                                         ::  dependency updated
      ?:  ?=([%se *] tee)
        build:(dom-vi q.tee)
      ?.  ?=([%on ~] tee)
        ~&(e/lost/[tee hen] +>.$)
      %+  roll  (~(tap in (~(get ju liz) p.sih)))
      =<  .(con ..axon(liz (~(del by liz) p.sih)))
      |=  [sus=(each duct ixor) con=_..axon]
      =.  ..axon  con
      ?-  -.sus
        %&  (give-json(hen p.sus) 205 ~ %b &) 
        %|  (get-even:(ire-ix p.sus) +.sih)
      ==
    ::
        %made
      ?<  ?=(%tabl -.q.sih)
      =.  our  (need hov)                             ::  XX
      |-  ^+  ..axon
      ?-    tee
          $|(~ [?(%on %ay %ow) *])  ~|(e/ford/lost/tee !!)
          [%of @ ~]  ~|(e/ford/lost/tee !!)
          [%si ~]  (give-sigh q.sih)
          [%se ^]  (get-made:(dom-vi q.tee) p.tee [p q]:sih)
          [%hi ^]
        ?:  ?=(%| -.q.sih)
          (give-sigh q.sih)  ::  XX crash?
        =*  cay  p.q.sih
        ?>  ?=(%hiss p.cay)
        =+  ((hard ,[pul=purl ^]) q.q.cay)
        ?.  ?=(%& -.r.p.pul)
          (eyre-them hi//[p.tee] q.cay)
        (get-req:(dom-vi p.r.p.pul) p.tee q.cay)
      ::
::           [%hi ^]
::         ?:  ?=(%| -.q.sih)
::           (give-sigh q.sih)  ::  XX crash?
::         =*  cay  p.q.sih
::         ?>  ?=(%hiss p.cay)
::         (eyre-them p.tee q.cay)
      ::
          [%he *]                     ::  XX hack
        =.  ..axon  $(tee p.tee)
        %_  ..axon
          mow  %+  turn  mow
               |=  a=move
               ?+  q.a  a
                 [%give %thou *]  a(r.p.p.q ~)
                 [%pass ^]        ?.(=(p.tee p.q.a) a a(p.q tee))
        ==     ==
      ::
          [%of @ ^]
        ?:  ?=(%| -.q.sih)
          ((slog p.q.sih) +>.^$)             ::  XX get-even %mean
        %+  get-rush:(ire-ix p.tee)  q.tee
        =*  cay  p.q.sih
        ?>  ?=(%json p.cay)                    ::  XX others
        ((hard json) q.q.cay)
      ::
          [%at ^]
        ?.  ?=([%& %js ^] q.sih)
          ~&  e/at-lost/p.tee
          $(tee q.tee)
        =*  cay  p.q.sih
        ?>  ?=(@ q.q.cay)
        =+  cyz=(~(got by wup) p.tee)
        =^  jon  ..ya  ~(stat-json ya p.tee cyz)
        $(tee q.tee, q.q.p.q.sih (add-json jon q.q.cay))
      ::
          [%ha *]
        %-  emule  |.  ^+  ..apex
        ?.  ?=(%& -.q.sih)
          (fail 404 p.sih p.q.sih)
        =*  cay  p.q.sih
        ?.  ?=(%mime p.cay)
          =+  bek=-:(need (tome p.tee))
          =+  bik=?+(r.bek bek [%ud %0] bek(r da/now))
          =-  (execute tee bik [%flag [p.sih `~] -])
          =-  `silk`[%cast %mime `[p.cay -]]
          ?.  ?=([%ud 0] r.bek)  q.cay
          ?+  p.cay  q.cay          :: inject dependency long-poll
            %urb  =<  (slam !>(.) q.cay)
                  |=  urb=manx
                  ~|  [%malformed-urb urb]
                  ?>  ?=([[%html ~] [[%head ~] *] [[%body ~] *] ~] urb)
                  (inject p.sih urb)
          ==
        ~|  q.q.cay
        =+  ((hard ,[mit=mite rez=octs]) q.q.cay)
        =+  dep=(crip "W/{(pojo %s (scot %uv p.sih))}")
        (give-thou 200 ~[etag/dep content-type/(moon mit)] ~ rez)
      ==
    ==
  ::
  ++  root-beak  `beak`[our %home da/now]               ::  XX
  ++  emule
    |=  a=_|?(..emule)  ^+  ..emule
    =+  mul=(mule a)
    ?~  -.mul  p.mul
    (fail 500 0v0 >%exit< p.mul)
  ::
  ++  ire-ix  |=(ire=ixor ~(. ix ire (~(got by wix) ire)))
  ++  dom-vi  
    |=  dom=path  ^+  vi
    ~(. vi dom (fall (~(get by sec) dom) *driv))
  ::
  ++  ses-authed 
    |=  ses=hole
    =+  sap=(~(get by sop) ses)
    ?:  ?=([~ @ %&] sap)  &
    =+  cyz=(~(get by wup) ses)
    ?~  cyz  |
    (~(has in aut.u.cyz) our)
  ::
  ++  ses-ya  |=(ses=hole ~(. ya ses (~(got by wup) ses)))
  ++  our-host  `hart`[& ~ `/org/urbit/(rsh 3 1 (scot %p our))]
  ::                  [| [~ 8.445] `/localhost]       :: XX testing
  ::
  ++  eyre-them
    |=  [tea=whir vax=vase:hiss]
    (pass-note tea [%e %meta :(slop !>(%them) !>(~) vax)])
  ::
  ++  ames-gram
    |=([him=ship gam=gram] (pass-note ~ %a %wont [our him] [%e -.gam] +.gam))
  ::
  ++  back                                              ::  %ford bounce
    |=  [tea=whir mar=mark cay=cage]
    (pass-note tea (ford-req root-beak [%cast mar `cay]))
  ::
  ++  cast-thou
    |=  [mar=mark hit=httr]
    =+  cay=[%httr !>(`httr`hit)]
    ?:  ?=(%httr mar)  (give-sigh %& cay)
    (back si/~ mar cay)
  ::
  ++  del-deps
    |=  [a=@uvH b=(each duct ixor)]  ^+  +>.$
    ?~  a  +>.$
    =.  liz  (~(del ju liz) a b)
    :: ~&  del-deps/[a (~(get ju liz) a)]
    ?:  (~(has by liz) a)  +>.$
    =-  -(hen hen.+)
    (pass-note(hen `~) on/~ %f [%wasp our a |])
  ::
  ++  new-deps
    |=  [a=@uvH b=(each duct ixor)]  ^+  +>.$
    :: ~&  new-deps/[a b]
    ?~  a  +>.$
    =+  had=(~(has by liz) a)
    =.  liz  (~(put ju liz) a b)
    ?:  had  +>.$
    =-  -(hen hen.+)
    (pass-note(hen `~) on/~ %f [%wasp our a &])
  ::
  ++  ford-req  |=([bek=beak kas=silk] [%f [%exec our `[bek kas]]])
  ++  execute  
    |=  [tea=whir req=[beak silk]]
    =.  lyv  (~(put by lyv) hen [%exec tea])
    (pass-note tea (ford-req req))
  ::
  ++  fail
    |=  [sas=@ud dep=@uvH mez=tang]
    ^+  +>
    :: (back ha/~ dep %tang !>(mez))  ::tang->urb chain may be source of failure
    (give-html sas ~ (inject dep (render-tang mez)))
  ::
  ++  give-html
    |=  [sas=@ud cug=(list ,@t) max=manx]
    %-  give-thou
    %+  add-cookies  cug
    (resp sas text//html (crip (poxo max)))
  ::
  ++  give-json
    |=  [sas=@uG cug=(list ,@t) jon=json]
    %-  give-thou
    %+  add-cookies  cug
    (resp sas application//json (crip (pojo jon)))
  ::
  ++  give-thou                                       ::  done request
    |=  hit=httr
    ?:  (~(has in ded) hen)                           ::  request closed
      +>(ded (~(del in ded) hen))
    =.  lyv  (~(del by lyv) hen)
    +>(mow :_(mow [hen %give %thou hit]))
  ::
  ++  give-sigh                                       ::  userspace done
    |=  res=(each cage tang)
    =-  +>.$(mow :_(mow [hen %give %sigh `cage`-]))
    ?.  ?=(%| -.res)  p.res
    [%tang !>(p.res)]
  ::
  ++  mean-json  |=([sas=@uG err=ares] (give-json sas ~ (ares-to-json err)))
  ++  nice-json  |=(* (give-json 200 ~ (joba %ok %b &)))
  ::
  ++  pass-note  |=(noe=[whir note] +>(mow :_(mow [hen %pass noe])))
  ++  host-to-ship                                              ::  host to ship
    |=  hot=host
    ^-  (unit ship)
    :: =+  gow=(~(get by dop) hot)    ::  XX trust
    :: ?^  gow  gow
    ?.  ?=(& -.hot)  ~
    =+  dom=(flop p.hot)                                ::  domain name
    ?~  dom  ~
    (rush i.dom fed:ag)
  ::
  ++  load-secret
    ^-  @ta
    =+  pax=/(scot %p our)/code/(scot %da now)/(scot %p our)
    %^  rsh  3  1
    (scot %p (,@ (need (sky %a pax))))
  ::
  ::
  ++  handle
    |=  $:  [hat=hart pok=pork quy=quay]                ::  purl, parsed url
            [cip=clip aut=?]                            ::  client ip, nonymous?
            [mef=meth maf=math bod=(unit octs)]         ::  method/headers/body
        ==
    =<  apex
    |%
    ++  abet  ..handle
    ++  done  .
    ++  teba  |*(a=$+(* ..handle) |*(b=* %_(done ..handle (a b))))
    ++  execute  (teba ^execute)
    ++  del-deps  (teba ^del-deps)
    ++  new-deps  (teba ^new-deps)
    ++  give-html  (teba ^give-html)
    ++  give-thou  (teba ^give-thou)
    ++  give-json  (teba ^give-json)
    ++  nice-json  (teba ^nice-json)
    ++  pass-note  (teba ^pass-note)
    ::
    ++  ford-boil
      |=  [wir=whir ext=term bem=beam]
      =+  yac=for-client
      =.  him.yac  ?.(aut anon him.yac)
      =:  r.bem  ?+(r.bem r.bem [%ud %0] da/now)
          s.bem  [%web ~(rent co (fcgi quy fcgi-cred.yac)) s.bem]
        ==
      (execute wir -.bem [%boil ext bem ~])
    ::
    ::
    ++  apex                                              
      =<  abet
      ^+  done
      =+  oar=(host-to-ship r.hat)
      =.  our  ?~(oar our u.oar)  ::  XX
      =+  pez=process
      ?:  ?=(%| -.pez)  p.pez
      (resolve ~ p.pez)
    ::
    ++  resolve
      |=  [cug=(list ,@t) pez=pest]  ^+  done
      ?~  pez  done
      ?-  -.pez
          ~  (give-thou (add-cookies cug p.pez))
          %js  $(pez [~ (resp 200 text//javascript p.pez)])
          %json  (give-json 200 cug p.pez)
          %html  (give-html 200 cug p.pez)
          %htme  (give-html 401 cug p.pez)
          %boil  (ford-boil +.pez)
          %red
        =+  url=(earn hat pok(p [~ %html]) quy)
        ?+    p.pok  ~|(bad-redirect/[p.pok url] !!)
            [~ %js]
          $(pez [%js auth-redir:js])
            [~ %json]
          $(pez [%json (jobe ok/b/| red/(jape url) ~)])
        ==
      ==
    ::
    ::
    ++  is-anon  =([~ ''] (~(get by (mo quy)) 'anon'))
    ++  check-oryx                    ::  | if json with bad oryx
      ^-  ?
      ?.  &(?=([~ %json] p.pok) ?=(%post mef) ?=(^ bod) !is-anon)  &
      =+  oxe=grab-oryx
      ?~  oxe  |
      ?:  (~(has in vew.cyz:for-client) u.oxe)
        &
      ~&(bad-oryx/[u.oxe vew.cyz:for-client] &)         ::  XX security
    ::
    ++  grab-json
      ?.  ?=(?(%post %put %delt) mef)
        ~
      ?~(bod ~ (poja q.u.bod))
    ::
    ++  need-body  |*(a=fist:jo (need (biff grab-json a)))
    ++  grab-oryx
      ^-  (unit oryx)
      =+  oxe=(biff grab-json (ot oryx/so ~):jo)
      ?^  oxe  oxe
      (~(get by (mo quy)) %oryx)
    ::
    ::
    ++  parse
      ^-  (each perk httr)
      |^  =+  hit=as-magic-filename
          ?^  hit  [%| u.hit]
          =+  bem=as-beam
          ?^  bem  [%& %beam u.bem]
          ?.  check-oryx
            ~|(%bad-oryx ~|([grab-oryx vew.cyz:for-client] !!))
          =+  hem=as-aux-request
          ?^  hem  [%& u.hem]
          ~|(strange-path/q.pok !!)
      ::
      ++  as-magic-filename
        ^-  (unit httr)
        ?+    [(fall p.pok %$) q.pok]  ~
            [?(%ico %png) %favicon ~]
          :-  ~
          %^  resp  200  image//png
          favi
        ::
            [%txt %robots ~]
          :-  ~
          %^  resp  200  text//plain
          %-  role
          :~  'User-agent: *'
              'Disallow: /'
          ==
        ==
      ::
      ++  as-beam
        ^-  (unit beam)
        |-
        ?~  q.pok
          $(q.pok /index)
        ?.  ((sane %tas) i.q.pok)
          (tome q.pok)
        `[[our i.q.pok ud/0] (flop t.q.pok)]
      ::
      ++  as-aux-request                                ::  /~/... req parser
        ^-  (unit perk)
        =.  mef
          ?.  ?=(%post mef)  mef
          ?+    (skim quy |=([a=@t b=@t] &(=('' b) =(a (cuss (trip a))))))
              ~|(bad-quy/[req='"?PUT" or "?DELETE"' quy] !!)
            ~   mef
            [[%'DELETE' ~] ~]  %delt
            [[%'PUT' ~] ~]     %put
          ==
        |-
        ?:  ?=([%'~~' *] q.pok)                            ::  auth shortcuts
          $(q.pok ['~' %as %own t.q.pok])
        ?.  ?=([%'~' @ *] q.pok)  ~
        :-  ~  ^-  perk
        =*  pef  i.t.q.pok
        =+  but=t.t.q.pok                 ::  XX  =*
        ?+    pef  ~|(pfix-lost/`path`/~/[pef] !!)
            %debug  ;;(perk [%bugs but])
            %away  [%away ~]
            %ac
          ?~  but  ~|(no-host/`path`/~/[pef] !!)
          =+  `dom=host`~|(bad-host/i.but (rash i.but thos:urlp))
          ?>  ?=([%auth ~] t.but)
          ?:  ?=(%| -.dom)  ~|(auth-ip/dom !!)
          [%oath p.dom]
        ::
            %at  [%auth %at pok(q but)]
            %am  ?~(but !! [%auth %xen i.but pok(q t.but)])
            %as
          :+  %auth  %get
          ~|  bad-ship/?~(but ~ i.but)
          ?~  but  !!
          :_  pok(q t.but)
          ?+  i.but  (slav %p i.but)
            %anon  anon
            %own   (fall (ship-from-cookies maf) our)
          ==
        ::
            %on
          :-  %poll
          ?^  but  [(raid but %uv ~)]~
          =+  dep=((hard (list ,[@ ~])) quy)
          =<  ?~(. !! .)
          (turn dep |=([a=@tas ~] (slav %uv a)))
        ::
            %of
          :+  %view  ?>(?=([@ ~] but) i.but)
          ?>  ?=([[%poll @] ~] quy)     ::  XX eventsource
          [~ (rash q.i.quy dem)]
        ::
            %to
          =+  ^-  dir=[p=ship q=term r=mark]
              ~|  bad-mess/but
              ?+  but  !!
                [@ @ ~]    [our (raid but %tas %tas ~)]
                [@ @ @ ~]  (raid but %p %tas %tas ~)
              ==
          :^  %mess  [p q]:dir  r.dir
          =+  wir=(~(get by (mo quy)) 'wire')
          ?^  wir  [(stab u.wir) (need grab-json)]         ::  XX distinguish
          (need-body (ot wire/(cu stab so) xyro/some ~):jo)
        ::
            %in
          ~|  expect/[%post 'application/json' /'@uv' '?PUT/DELETE']
          ?>  &(?=(?(%delt %put) mef) ?=($|(~ [~ %json]) p.pok))
          [%deps mef (raid but %uv ~)]
        ::
            %is
          ?~  but
            ~|(no-app/but=but !!)
          |-  ^-  perk
          ?~  p.pok  $(p.pok [~ %json])
          ?.  ?=(%json u.p.pok)
            ~|(is/stub/u.p.pok !!)      ::  XX marks
          ?:  ((sane %tas) i.but)
            $(but [(scot %p our) but])
          ?>  ?=(?(%delt %put) mef)
          =+  :-  hap=[(slav %p i.but) (slav %tas -.t.but)]
              wir=(need-body (ot wire/(cu stab so) ~):jo)
          [%subs mef hap u.p.pok wir +.t.but]
        ::
            %auth
          :-  %auth
          |-  ^-  perk-auth
          ?+    p.pok  !!
              ~          $(p.pok [~ %json])
              [~ %js]    [%js ~]
              [~ %json]
            ?+    mef  ~|(bad-meth/mef !!)
                %get   [%json ~]
                %put
              ~|  parsing/bod
              [%try (need-body (ot ship/(su fed:ag) code/(mu so) ~):jo)]
            ::
                %delt
              ~|  parsing/bod
              =+  jon=(need (poja q:(need bod)))
              ?>  ?=(%o -.jon)
              =+  sip=(~(get by p.jon) %ship)
              [%del ?~(sip ~ [~ (need ((su:jo fed:ag) u.sip))])]
        ==  ==
        ==
      --
    ::
    ++  process
      ^-  (each pest ,_done)
      =+  pet=parse
      ?:  ?=(%| -.pet)
        [%& ~ p.pet]
      (process-parsed p.pet)
    ::
    ++  process-parsed
      |=  hem=perk  ^-  (each pest ,_done)
      ?-    -.hem
          %auth  (process-auth p.hem)
          %away  [%& %html logout-page:xml]
          ?(%beam %spur)
        =+  ext=(fall p.pok %urb)
        =+  bem=?-(-.hem %beam p.hem, %spur [root-beak p.hem])
        =+  wir=`whir`[%ha (tope -.bem ~)]
        =.  wir  ?+(mef !! %get wir, %head [%he wir])
        ~|  bad-beam/q.bem
        ?<  =([~ 0] (sky %cw (tope bem(+ ~, r [%da now]))))
        =+  [wir ext bem]
        ?.(aut [%& %boil -] [%| (ford-boil -)])  ::  XX properly
      ::
          %bugs  
        ?-  p.hem
          %as  (show-login-page)
          %to  [%& %html poke-test:xml]
        ==
      ::
          %deps
        =+  ire=need-ixor
        ?>  (~(has by wix) ire)  ::  XX made redundant by oryx checking
        =<  [%| (nice-json)]
        ?-  p.hem
          %put   (new-deps q.hem %| ire)
          %delt  (del-deps q.hem %| ire)
        ==
      ::
          %mess
        :-  %|
        =^  orx  ..ya   ?:(is-anon new-view:for-client [(need grab-oryx) ..ya])
        =+  vew=(ire-ix (oryx-to-ixor orx))
        ((teba new-mess.vew) p.hem r.hem q.hem %json !>(`json`s.hem))
      ::
          %oath
        ?.  (~(has by sec) p.hem)
          ~|(no-driver/p.hem !!)
        [%| %.(quy (teba get-quay:(dom-vi p.hem)))]
      ::
          %poll
        ?:  ?=([~ %js] p.pok)  ::  XX treat non-json cases?
          =+  polling-url=['/' (apex:earn %| pok(u.p %json) quy)]
          [%& %js (add-json (joba %wurl (jape polling-url)) poll:js)]
        =.  lyv  (~(put by lyv) hen %wasp p.hem)
        |-
          =.  done  (new-deps i.p.hem %& hen)
          ?~  t.p.hem  [%| done]
          $(p.hem t.p.hem)
      ::
          %subs
        ?-  p.hem
          %put   [%| ((teba add-subs:for-view) q.hem)]
          %delt  [%| ((teba del-subs:for-view) q.hem)]
        ==
      ::
          %view
        ~|  lost-ixor/p.hem
        [%| ((teba poll:(ire-ix p.hem)) u.q.hem)]
      ==
    ::
    ++  process-auth
      |=  ham=perk-auth  ^-  (each pest ,_done)
      =+  yac=for-client
      ?-    -.ham
          %js    [%& %js auth:js]
          %json  =^  jon  ..ya  stat-json.yac
                 [%| (give-json 200 ~ jon)]
          %xen   (show-login-page ~ ses.ham)
      ::
          %at
        =.  ..ya  abet.yac
        =+  pez=process(pok p.ham, aut |)
        ?.  ?=(%& -.pez)  ~|(no-inject/p.ham !!)
        ?~  p.pez  pez
        ?+    -.p.pez  ~&(bad-inject/p.pez !!)
            %red  pez
            %boil
          =.  ya  abet.yac 
          [%| (resolve ~ p.pez(p [%at ses.yac p.p.pez]))]
        ::
            %js
          =^  jon  ..ya  stat-json.yac
          [%| (resolve cug.yac p.pez(p (add-json jon p.p.pez)))]
        ==
      ::
          %del  
        =.  ..ya  (logoff:yac p.ham)
        =+  cug=[(set-cookie cookie-prefix '~')]~
        [%| (give-json 200 cug (joba %ok %b &))]
      ::
          %get
        |-
        ~|  aute/ham
        ?:  |(=(anon him.ham) (~(has in aut.yac) him.ham))
          =.  ..ya  abet.yac(him him.ham)
          =+  pez=process(pok rem.ham, aut &)
          ?:  ?=(%| -.pez)  pez
          [%| (resolve ~ p.pez)]
        ?.  =(our him.ham)
          [%| ((teba foreign-auth.yac) him.ham hat rem.ham quy)]
        (show-login-page ~)
      ::
          %try
        :-  %|
        ?.  =(our him.ham)
          ~|(stub-foreign/him.ham !!)
        ?.  ?|  (~(has in aut.yac) him.ham) 
                ?~(paz.ham | =(u.paz.ham load-secret))
            ==
          ~&  code=`@t`load-secret
          ~|([%try 'code' %in %console] !!)  ::  XX security
        =^  jon  ..ya  stat-json:(logon:yac him.ham)
        =.  cug.yac  :_(cug.yac (set-cookie %ship (scot %p him.ham)))
        (give-json 200 cug.yac jon)
      ==
    ::
    ++  show-login-page
      |=  ses=(unit hole)  ^-  (each pest ,_done)
      %-  (slog leaf/"Login code for {(scow %p our)} is: {(trip load-secret)}" ~)
      ?.  ?=($|(~ [~ %html]) p.pok)
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
        [%| @]  (cat 3 '; Domain=' (rsh 3 1 (scot %if p.r.hat)))
        [%& %org %urbit *]  '; Domain=.urbit.org'
        [%& @ @ *]  =-  (rap 3 "; Domain={-}{i.p.r.hat ~}")
                    (turn (flop `path`t.p.r.hat) |=(a=span (cat 3 a '.')))
                    
        [%& *]  ''  ::  XX security?
      ==
    ::
    ++  set-cookie
      |=  [key=@t val=@t]
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
        ~&  bad-cookie/u.lig
        (new-ya (rsh 3 1 (scot %p (end 6 1 ney))))
      ~(. ya u.lig u.cyz(cug ~)) 
    ::
    ++  new-ya  |=(ses=hole ~(. ya ses (new-cyst ses)))
    ++  new-cyst
      |=  ses=hole
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
          [anon ~]
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
  ++  oryx-to-ixor  |=(a=oryx (rsh 3 1 (scot %p (end 6 1 (shas %ire a)))))
  ++  ya                                                ::  session engine
    =|  [ses=hole cyst]
    =*  cyz  ->
    |%
    ++  abet  ..ya(wup (~(put by wup) ses cyz))
    ++  abut  ..ya(wup (~(del by wup) ses))
    ++  foreign-auth
      |=  [him=ship pul=purl]  ^+  ..ya
      =.  way  (~(put by way) him pul hen)
      ~&  asking-foreign/him
      (ames-gram:abet him [lon/~ ses])
    ::
    ++  foreign-hat
      |=  [him=ship hat=hart]  ^+  ..ya
      ~|  way
      =^  pul  hen  (~(got by way) him)
      =:  way       (~(del by way) him)
          dop       (~(put by dop) r.hat him)  
          q.q.pul   ['~' %am ses q.q.pul]
        ==
      =+  url=(welp (earn pul(p hat)) '#' (head:earn p.pul))
      %-  give-thou:abet
      (add-cookies cug [307 [location/(crip url)]~ ~])
    ::
    ++  logon
      |=  her=ship
      %_  +>
        him   her
        aut   (~(put in aut) her)
        ..ya
          ~&  logon/[our her ses]
          ?.  =(our her)
            ..ya
          =+  sap=(~(get by sop) ses)
          ~&  sap
          ?.  ?=([~ @ %|] sap)
            ..ya
          (ames-gram -.u.sap aut/~ ses)
      ==
    ++  logoff    
      |=  her=(unit ship)  ^+  ..ya
      ?~  her  abut
      =.  aut  (~(del in aut) u.her)
      ?~  aut  abut
      abet(him ?.(=(her him) him n.aut))
    ::
    ++  new-view
      ^+  [*oryx ..ya]
      =+  orx=`@t`(rsh 3 1 (scot %p (shaf %orx eny)))
      =.  vew  (~(put in vew) orx)
      =+  ire=(oryx-to-ixor orx)
      =.  ..ix  ~(init ix ire %*(. *stem him him, p.eve 1))
      ::  ~&  stat-ire/`@t`ire
      [orx abet]
    ::
    ++  fcgi-cred  %_(ced aut (~(put ju aut.ced) %$ (scot %p him)))
    ++  stat-json
      ^+  [*json ..ya]
      =^  orx  ..ya  new-view
      :_  ..ya
      %-  jobe  :~
        oryx/s/orx
        ixor/s/(oryx-to-ixor orx)
        ship/(jape +:<our>)
        user/(jape +:<him>)
        auth/a/(turn (~(tap in aut)) |=(a=@p (jape +:<a>)))
      ==
    --
  ::
  ++  ix
    =|  [ire=ixor stem]
    =*  sem  ->
    |%
    ++  done  .
    ++  abet  ..ix(wix (~(put by wix) ire sem))
    ++  abut
      =+  sub=(~(tap in sus))
      |-  ^+  ..ix
      ?^  sub  $(sub t.sub, ..ix (pul-subs i.sub))
      =.  +>  poll-rest
      ..ix(wix (~(del by wix) ire))
    ::
    ++  teba  |*(a=$+(* ..ix) |*(b=* %_(done ..ix (a b))))
    ++  give-json  (teba ^give-json)
    ++  pass-note  (teba ^pass-note)
    ++  hurl-note 
      |=  [a=[dock path] b=note]  ^+  ..ix
      =:  med  (~(put to med) hen)
          hen  `~
        ==
      :: ~&  >  hurl/[&2.b ire a]
      (pass-note:abet [%of ire (gsig a)] b)
    ::
    ++  init
      =.  die  (add ~d1 now) 
      abet(mow :_(mow [`/ %pass ow//[ire] [%b %wait die]]))
    ::
    ++  refresh
      =.  mow  :_(mow [`/ %pass ow//[ire] [%b %rest die]])
      =.  die  (add ~d1 now) 
      done(mow :_(mow [`/ %pass ow//[ire] [%b %wait die]]))
    ::
    ++  add-even
      |=  a=even  ^+  eve
      [+(p.eve) (~(put by q.eve) p.eve a)]
    ::
    ++  new-mess
      |=  [a=dock b=wire c=mark d=cage]  ^+  ..ix
      (hurl-note [a b] [%g %deal [him -.a] +.a %punk c d])
    ::
    ++  add-subs
      |=  [a=dock %json b=wire c=path]  ^+  ..ix
      ?:  (~(has in sus) +<)  ~|(duplicate/c !!)
      =.  sus  (~(put in sus) +<)
      (hurl-note [a b] [%g %deal [him -.a] +.a %peel %json c])
    ::
    ++  pul-subs
      |=  [a=dock %json b=wire c=path]  ^+  ..ix
      =.  sus  (~(del in sus) +<)
      (hurl-note [a b] [%g %deal [him -.a] +.a %pull ~])
    ::
    ++  del-subs                      ::  XX per path?
      |=  [a=dock %json b=wire c=path]  ^+  ..ix
      =.  ..ix  (pul-subs +<)
      (nice-json:pop-duct:(ire-ix ire))          ::  XX gall ack
    ::
    ++  get-rush
      |=  [a=whir-of b=json]  ^+  ..ix
      (get-even [%rush [[(slav %p p.a) q.a] r.a] (joba %json b)])
    ::
    ++  get-quit
      |=  a=whir-of  ^+  ..ix
      (get-even [%quit [[(slav %p p.a) q.a] r.a]])
    ::
    ++  get-ack
      |=  [a=whir-of b=(unit ,[term tang])]  ^+  ..ix
      ?:  =(~ med)  ~&  resp-lost/ire  ..ix
      ?~  b  (nice-json:pop-duct)
      (mean-json:pop-duct 500 b)
    ::
    ++  get-even
      |=  ven=even  ^+  ..ix
      =+  num=p.eve
      =.  eve  (add-even ven)
      =<  abet
      ?~  ude  done
      =.  hen  p.u.ude
      (give-even:pass-rest(ude ~) q.u.ude num ven)
    ::
    ++  give-even
      |=  [pol=? num=@u ven=even]  ^+  done
      =:  q.eve  (~(del by q.eve) (dec num))              ::  TODO ponder a-2
          mow    ?.(?=(%rush -.ven) mow mow:(pass-took p.ven))
        ==
      ?>  pol                         ::  XX eventstream  
      %^  give-json  200  ~
      %^  jobe  id/(jone num)  type/[%s -.ven]
      ?-  -.ven
        %news  ~[from/[%s (scot %uv p.ven)]]
        %quit  ~[from/(subs-to-json p.ven)]
        %rush  ~[from/(subs-to-json p.ven) data/q.ven]
      ==
    ::
    ++  pass-wait  (pass-note of//[ire] [%b %wait era])
    ++  pass-rest
      =.  lyv  (~(del by lyv) hen)
      (pass-note of//[ire] [%b %rest era])
    ::
    ++  pass-took
      |=  a=[p=dock wire]
      %+  pass-note(hen `~)
        [%of ire (gsig a)] 
      [%g %deal [him -.p.a] +.p.a %pump ~]
    ::
    ++  pop-duct  =^(ned med ~(get to med) abet(hen ned))
    ++  poll
      |=  a=@u  ^+  ..ix
      =<  abet
      =.  ..poll  refresh
      ?:  =(a p.eve)
        =.  ..poll  poll-rest
        =.  era  (add ~s30 now)
        =.  lyv  (~(put by lyv) hen [%poll ire])
        pass-wait(ude [~ hen &])
      ?:  (gth a p.eve)  ~|(seq-high/cur=p.eve !!)
      =+  ven=~|(seq-low/cur=p.eve (~(got by q.eve) a))
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
      |=  [a=dock b=path]
      %-  jobe  :~
        ship/[%s (rsh 3 1 (scot %p p.a))]
        appl/[%s q.a] 
        path/(jape (spud b))
      ==
    ++  wake  ^+(..ix abet(ude ~))  ::  XX other effects?
    ::  XX unused
    ++  print-subs  |=([a=dock b=path] "{<p.a>}/{(trip q.a)}{(spud b)}")
    --
  ++  vi                                                ::  auth engine
    |_  [dom=path cor=(unit vase) req=(qeu ,[p=duct q=mark r=vase:hiss])]
    ++  abet  +>(sec (~(put by sec) +<))
    ++  dead-hiss  |=(a=tang (give-sigh:abet %| a))
    ++  dead-this  |=(a=tang (fail:abet 500 0v0 a))
    ++  pass-note  |=([a=whir-se b=note] (pass-note:abet se/[a dom] b))
    ++  eyre-them  |=([a=whir-se b=vase] (eyre-them:abet se/[a dom] b))
    ::  XX block reqs until correct core checked in?
    ++  warn  |=(a=tang ((slog (flop a)) abet))
    ++  pump
      ^+  abet
      ?~  cor
        build
      =+  ole=~(top to req)
      ?~  ole  abet
      ::  process hiss
      (call(hen p.u.ole) %out hiss/r.u.ole)
    ::
    ++  call
      |=  [arm=?(%bak %out %in) sam=cage]
      ?~  cor  ~|(%no-core !!)
      =.  u.cor  (slap u.cor cncb/[[`1]~ [[`12]~ bczp/%null]~])
      =+  call/[ride/[cnzy/arm `core/u.cor] `sam]
      (pass-note arm (ford-req root-beak -))
    ::
    ++  get-upd  
      |=  [dep=@uvH gag=(each cage tang)]
      ~&  got-upd/dep
      =.  ..vi  (pass-note %core [%f [%wasp our dep &]])
      ?-(-.gag %| (warn p.gag), %& pump(cor `q.p.gag))
    ::
    ++  get-made
      |=  [wir=whir-se dep=@uvH res=(each cage tang)]  ^+  abet
      ?-  wir
        %core  (get-upd dep res)
        %bak   (res-bak res)
        %out   (res-out res)
        %in    (res-in res)
      ==
    ::
    ++  get-thou
      |=  [wir=whir-se hit=httr]
      ?+  wir  !!
        %in  (call %bak httr/!>(hit))
        %out
          =^  ole  req  ~(get to req)
          =>  .(ole `[p=duct q=mark *]`ole)             :: XX types
          =.  ..vi  (cast-thou(hen p.ole) q.ole hit)    :: error?
          pump
      ==
    ::
    ++  auth-tank
      =>  rose/["." `~]^(turn (flop dom) |=(a=cord leaf/(trip a)))
      rose/[" " `~]^~[leaf/"To authenticate" . leaf/"visit:"]
    ::
    ::    XX formal dill-blit %url via hood
    ++  auth-print  |=([%| a=purl] (slog auth-tank leaf/(earn a) ~))
    ::
    ++  on-error
      |=  [err=$+(tang _abet) try=$+(vase _abet)]
      |=  a=(each cage tang)  ^+  abet
      ?:  ?=(%| -.a)  (err p.a)
      =-  ?-(-.- %& p.-, %| (err p.-))
      (mule |.(~|(driver/dom ~|(bad-res/p.q.p.a (try q.p.a)))))
    ::
    ++  res-in
      %+  on-error  dead-this
      |=  res=vase  ^+  abet
      =.  res  (spec res)
      ?+  -.q.res  !!                                   :: bad type
        %|  ?>(?=(%retry +.p.res) ~|(%retry-stub !!))
        %&  (eyre-them %in (slam !>(|=([%& a=hiss] a)) res))
      ==
    ::
    ++  res-bak
      %+  on-error  dead-this
      |=  res=vase  ^+  abet
      =+  ~|(%split [mow=(slot 2 res) cor=(slot 3 res)])
      =.  ^cor
        ?~  ^cor  ~|(%lost-core !!)
        (some cor)
      =.  mow  (spec mow)
      ?+  -.q.mow  !!                                   :: bad type
        %&  ~|(unexpected-hiss/%bak !!)
        %|  ?>  ?=(%retry +.q.mow)
            =.  ..vi  (give-html 200 ~ exit:xml)
            pump
      ==
    ::
    ++  res-out
      |=  a=(each cage tang)  ^+  abet
      ?:  ?=(%| -.a)
        (dead-hiss(req ~(nap to req)) p.a)
      %.  a
      %+  on-error  warn
      |=  res=vase  ^+  abet
      =.  res  (spec res)
      ?+  -.q.res  !!                                   :: bad type
        %|  =+((slam !>(auth-print) res) abet)
        %&  (eyre-them %out (slam !>(|=([%& a=hiss] a)) res))
      ==
    ::
    ++  get-quay
      |=  quy=quay  ^+  abet
      ?~  cor
        ~|(%no-core !!)
      (call %in quay/!>(quy))
    ::
    ++  rebuild  build(cor ~)
    ++  build
      =-  (pass-note:abet se/core/dom (ford-req root-beak -))
      =+  sil=core/[root-beak (flop %_(dom . sec/dom))]
      ?~  cor
        sil
      =+  usr=(mule |.((slot 13 u.cor)))
      ?:  ?=(%| -.usr)
        ~&(no-samp/dom sil)
      mute/[sil [~[`13] `noun/p.usr]~]
    ::
    ++  get-req  |=(a=[mark vase:hiss] pump(req (~(put to req) hen a)))
--  --
--
.   ==
=|  bolo
=*  bol  -
|=  [now=@da eny=@ ski=sled]                            ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  call                                                ::  handle request
  |=  $:  hen=duct
          hic=(hypo (hobo kiss))
      ==
  =>  %=    .                                           ::  XX temporary
          q.hic
        ^-  kiss
        ?:  ?=(%soft -.q.hic)
          ((hard kiss) p.q.hic)
        ?:  (~(nest ut -:!>(*kiss)) | p.hic)  q.hic
        ~&  [%eyre-call-flub (,@tas `*`-.q.hic)]
        ((hard kiss) q.hic)
      ==
  ^-  [p=(list move) q=_..^$]
  ?:  ?=(%wegh -.q.hic)
    :_  ..^$  :_  ~
    :^  hen  %give  %mass
    :-  %eyre
    :-  %|
    :~  dependencies/`liz  sessions/`wup  views/`wix
        ducts/[%| ~[dead/`ded proxy/`pox outgoing/`ask]]
        hosts/`dop
        misc/`bol
    ==
  =+  our=`@p`0x100  ::  XX  sentinel
  =+  ska=(slod ski)
  =+  sky=|=(* `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ^-  [p=(list move) q=_..^$]
  =.  gub  ?.(=(0 gub) gub (cat 3 (rsh 3 1 (scot %p (end 6 1 eny))) '-'))
  =^  mos  bol
    abet:(apex:~(adit ye [hen [now eny our sky] ~] bol) q.hic)
  [mos ..^$]
::
++  doze                                                ::  require no timer
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load                                                ::  take previous state
  ::  =<  -
  ::  :-  |=  a=_%*(. *bolo lyv *(map duct ?(live [%exec wire])))
  ::      =>  .(lyv.a (~(run by lyv.a) (hard live)))
  ::      %_(..^$ +>- a)
  =+  bolo-3=,_[%3 %*(+ *bolo |13 &14.+.bol)]
  =+  even-2=?(even [%mean p=[dock path] *])            ::  old %quit
  =+  ^=  stem-2                                        ::  no die, sus
      ,_=+(*stem -(|3 |5.-, q.eve *(map ,@u even-2)))
  =+  bolo-2=,_[%2 %*(+ *bolo-3 wix *(map ixor stem-2))]
  =+  bolo-1=,_[%1 +(|4 |5.+)]:*bolo-2                  ::  no lyv
  |=  old=?(bolo bolo-3 bolo-2 bolo-1)
  ^+  ..^$
  ?-  -.old
    %4  ..^$(+>- old)
    %3  $(old [%4 +(wix [wix ~])]:old)
    %2  =+  evn=|=(a=even-2 ?+(-.a a %mean [%quit p.a]))
        =+  stm=|=(a=stem-2 a(|3 [now ~ |3.a(q.eve (~(run by q.eve.a) evn))]))
        $(old [%3 +.old(wix (~(run by wix.old) stm))])
    %1  $(old [%2 +(|4 [~ |4.+])]:old)
  ==
::
++  scry
  |=  [our=(unit (set monk)) ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  ~
::
++  stay  `bolo`+>-.$
++  take                                                ::  accept response
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^-  [p=(list move) q=_..^$]
  =+  our=`@p`0x100  ::  XX  sentinel
  =+  ska=(slod ski)
  =+  sky=|=(* `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ^-  [p=(list move) q=_..^$]
  =.  gub  ?.(=(0 gub) gub (cat 3 (rsh 3 1 (scot %p (end 6 1 eny))) '-'))
  =+  tee=((soft whir) tea)
  ?~  tee  ~&  [%e %lost -.q.hin hen]  [~ ..^$]
  =^  mos  bol
    =<  abet
    %^  axon:~(adit ye [hen [now eny our sky] ~] bol)  u.tee
      (~(peek ut p.hin) %free 3) 
    q.hin
  [mos ..^$]
--
