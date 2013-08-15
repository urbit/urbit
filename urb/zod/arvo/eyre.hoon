!:
::  eyre (4e), http servant
::
|=  pit=vase
^-  vane                                                ::  kernel instrument
=>  =~
|%
++  bolo                                                ::  eyre state
    $:  wig=(map duct (list rout))                      ::  server routes
        ged=duct                                        ::  client interface
        giv=[p=@ud q=(map ,@ud duct)]                   ::  incoming requests
        ask=[p=@ud q=(map ,@ud ,[p=duct q=hiss])]       ::  outgoing requests
        kes=(map duct ,@ud)                             ::  outgoing requests
    ==
::
++  ecco                                                ::  eat headers
  |=  hed=(list ,[p=@t q=@t])
  =+  mah=*math
  |-  ^-  math
  ?~  hed  mah
  =+  cus=(cass (rip 3 p.i.hed))
  =+  zeb=(~(get by mah) cus)
  $(hed t.hed, mah (~(put by mah) cus ?~(zeb [q.i.hed ~] [q.i.hed u.zeb])))
::
++  hone                                                ::  host match
  |=  [fro=host too=host]  ^-  ?
  ?-    -.fro
      |  =(too fro)
      &
    ?&  ?=(& -.too)
        |-  ^-  ?
        ?~  p.too  &
        ?~  p.fro  |
        ?:  !=(i.p.too i.p.fro)  |
        $(p.too t.p.too, p.fro t.p.fro)
    ==
  ==
::
++  loot                                                ::  match route
  |=  [uri=purl rut=rout]
  ^-  (unit scud)
  ?.  |-  ^-  ?
      ?~  p.rut  |
      =(i.p.rut `host`r.p.uri)
    ~
  =+  tac=*path
  |-  ^-  (unit scud)
  ?~  q.rut
    :-  ~
    :-  :(weld (flop q.q.uri) tac s.rut)
    `scar`[p.uri (flop tac) p.q.uri s.rut]
  ?:  |(?=(~ q.q.uri) !=(i.q.rut i.q.q.uri))
    ~
  $(q.rut t.q.rut, q.q.uri t.q.q.uri, tac [i.q.rut tac])
--
. ==
=|  bolo
|=  [now=@da eny=@ sky=||(* (unit))]                    ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  beat                                                ::  process move
  |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
  =>  .(fav ((hard card) fav))
  ^-  [p=(list move) q=vane]
  ?+    -.fav
    [[[wru hen fav] ~] ..^$]
  ::
      %band                                             ::  set/clear route
    [~ ..^$(wig ?~(q.fav (~(del by wig) hen) (~(put by wig) hen q.fav)))]
  ::
      %born
    [~ ..^$(ged hen)]      ::  XX retry all gets, abort all puts
  ::
      %crud
    [[[wru [/d hen] %flog fav] ~] ..^$]
  ::
      %that                                             ::  response by us
    =+  neh=(need (~(get by q.giv) p.fav))
    :_  ..^$(q.giv (~(del by q.giv) p.fav))
    :_  ~
    :+  ~  neh
    :-  %thou
    ^-  httr
    ?-  -.q.fav
      %mid  [200 ~[content-type/(moon p.q.fav)] [~ q.q.fav]]
      %ham  [200 ~[content-type/'text/html'] [~ (tact (xmlt p.q.fav ~))]]
      %raw  p.q.fav
    ==
  ::
      %them                                             ::  outgoing request
    ?~  p.fav
      =+  sud=(need (~(get by kes) hen))
      :-  [[~ ged [%thus sud ~]] ~]
      ..^$(q.ask (~(del by q.ask) sud), kes (~(del by kes) hen))
    :-  [[~ ged [%thus p.ask p.fav]] ~]
    %=  ..^$
      p.ask  +(p.ask)
      q.ask  (~(put by q.ask) p.ask hen u.p.fav)
      kes    (~(put by kes) hen p.ask)
    ==
  ::
      %they                                             ::  response to us
    =+  kas=(need (~(get by q.ask) p.fav))
    :-  [[~ p.kas [%thou q.fav]] ~]
    ..^$(q.ask (~(del by q.ask) p.kas))
  ::
      %this                                             ::  request to us
    =+  ryp=`quri`(rash q.r.fav zest:epur)
    =+  mah=(ecco r.r.fav)
    =+  ^=  pul  ^-  purl
        ?-  -.ryp
          &  ?>(=(p.fav p.p.p.ryp) p.ryp)
          |  =+  hot=(~(get by mah) %host)
             ?>  ?=([~ @ ~] hot)
             [[p.fav (rash i.u.hot thor:epur)] p.ryp q.ryp]
        ==
    =+  het=`hate`[pul (shaf %this q.fav) [p.r.fav mah s.r.fav]]
    =+  gew=`(list ,[p=duct q=(list rout)])`(~(tap by wig) ~)
    =+  ^=  faw  
        |-  ^-  (list ,[p=duct q=scud])
        ?~  gew  ~
        =+  mor=$(gew t.gew)
        =+  ^=  woy
            |-  ^-  (list scud)
            ?~  q.i.gew  ~
            =+  mor=$(q.i.gew t.q.i.gew)
            =+  lut=(loot pul i.q.i.gew)
            ?~(lut mor [u.lut mor])
        ?~  woy  mor
        ::  ?^  t.woy  [[[~ hen [%thou 500 ~ ~]] ~] ..^^$]
        [[p.i.gew i.woy] mor]
    ?~  faw  [[[~ hen [%thou 404 ~ ~]] ~] ..^$]
    ::  ?^  t.faw  [[[~ hen [%thou 500 ~ ~]] ~] ..^$]
    :-  [[~ p.i.faw `card`[%thee p.giv [q.i.faw r.pul] *cred r.het]] ~]
    ..^$(p.giv +(p.giv), q.giv (~(put by q.giv) p.giv hen))
  ==
::
++  come  
  |=  old=vase
  ^-  vane
  ~|(%load-nest-eyre !!)
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  flee  stay
++  load
  |=  new=vase
  ^-  vane
  ?.  (~(nest ut -:!>(`bolo`+>-.^$)) & p.new)
    (come new)
  ..^$(+>- (bolo q.new))
::
++  raze  
  ^-  vane
  ..$(+>- *bolo)
::
++  scry
  |=  [our=ship ren=@tas who=ship syd=disc lot=coin tyl=path]
  ^-  (unit)
  ~
::
++  stay  `vase`!>(`bolo`+>-.$)
--
