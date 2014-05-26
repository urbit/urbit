!:  ::  %gall, user-level extension
!?  164
::::
|=  pit=vase
^-  vane
=>  =~
|%                                                      ::  clams
++  axle                                                ::  all %gall state
  $:  ven=%0                                            ::
      pol=(map ship mast)                               ::  apps by ship
  ==                                                    ::
++  bone  ,@ud                                          ::  opaque duct
++  bull  ,[p=bone q=(mold note gift)]                  ::  urbit action
++  bump                                                ::  urbit event
  $:  ost=bone                                          ::  channel
      him=ship                                          ::  client
      law=(unit chop)                                   ::  authority
      val=curd                                          ::  data
  ==                                                    ::
++  icon                                                ::  output type
  $:  
++  wind  ,[p=cork q=@da r=(unit ,@dr)]                 ::  timeline
++  gift                                                ::  outbound result
  %$  [%back ~]                                         ::  acknowledgment
      [%crud p=(list tank)]                             ::  error notification
      [%seen p=@da q=*]                                 ::  wind update
  ==                                                    ::
++  kiss                                                ::  inbound request
  $%  [%send p=sock q=term r=curd]                      ::  urbit message
      [%turd p=(list tank) q=kiss]                      ::  error rebound
      [%user p=(unit chop) q=kiss]                      ::  restriction
      [%wind p=ship q=wind r=path]                      ::  subscription
  ==                                                    ::
++  sign  curd                                          ::  inbound result V-<
++  note  curd                                          ::  outbound request V->
++  hypo  |*(a=$+(* *) (pair type a))
++  debt                                                ::  pending operation
  $%  [%pear p=ship q=(unit chop) r=curd]               ::  message
      [%peer p=(unit chop) q=wind r=path]               ::  subscription
      [%peak p=(hypo card)]                             ::  result
  ==
++  seat                                                ::  living app
  $:  huv=(unit vase)                                   ::  current hive
      dep=(set ,[p=ship q=desk])                        ::  dependencies
      orm=(unit ,@da)                                   ::  last buildtime
      eny=@                                             ::  entropy
      lat=@da                                           ::  time of last tick
      tik=@ud                                           ::  tick computed
      zos=(map bone ,[p=(unit chop) q=duct])            ::  bone to duct
      zam=[p=@ud q=(map duct ,@ud)]                     ::  duct to bone
      vey=(qeu ,[p=bone q=debt])                        ::  blocked queue
  ==                                                    ::
++  mast                                                ::  apps by ship
  $:  bum=(map ,@ta seat)                               ::  apps by name
  ==                                                    ::
++  mitt                                                ::  execution context
  $:  $:  ost=bone                                      ::  source
          pax=path                                      ::  hivepath
          law=(unit chop)                               ::  rights
      ==                                                ::
++  wren                                                ::  waiting card
  $:  tig=mitt                                          ::  context
      fav=card                                          ::  content
  ==                                                    ::
--                                                      ::
|%                                                      ::  implicit clams
++  cork  ?(%u %v %w %x %y %z)                          ::  wind angle
++  gift                                                ::
  
++  hawk  (gaff card)                                   ::
++  hide                                                ::  engine state
  $:  own=[p=ship q=@tas]                               ::  static identity
      bec=beak                                          ::  installation path
    ^=  seq                                             ::  dynamic sequence
      $:  num=@ud                                       ::  change number
          eny=@                                         ::  entropy
          lat=@da                                       ::  date of last tick
  ==  ==                                                ::
++  hive                                                ::  typed engine
  $_  ^?                                                ::
  |_  hide                                              ::  hive state
  ++  pack  *                                           ::  save
  ++  pear                                              ::  request
    |+  $:  ost=bone                                    ::  channel
            law=(unit chop)                             ::  authority
            him=ship                                    ::  client
            fov=(hypo card)                             ::  favor
        ==                                              ::
    :_(+> (list bull))                                  ::
  ::                                                    :: 
  ++  peer                                              ::  subscribe
    |+  %

  ++  poke  |+(lima [*(list ,[p=bone q=hawk]) +>])      ::  do
  ++  peek  |+(lira *(unit (unit luge)))                ::  see
  ++  prep  |+(* +>)                                    ::  load
  ==                                                    ::
++  lima                                                ::  wren other side
  $:  $:  ost=bone                                      ::  source
          law=(unit chop)                               ::  permission
          pax=path                                      ::  logical position
      ==
      fav=*                                             ::  present event
  ==                                                    ::
++  lira                                                ::  general query
  $:  for=(set monk)                                    ::  p.u.law
      roc=cork                                          ::  desired aspect
      pax=path                                          ::  internal path
  ==                                                    ::
--                                                      ::
|%                                                      ::
++  ho                                                  ::  per card
  =|  $:  ost=bone                                      ::  opaque cause
          hen=duct                                      ::  kernel cause
          law=(unit chop)                               ::  rights
          mow=(list move)                               ::  direct moves
          now=@da                                       ::  current date
          eny=@                                         ::  current entropy
          our=@p                                        ::  ship
          app=@tas                                      ::  application name
          sat=seat                                      ::  application state
      ==                                                ::
  |%
  ++  abet
    ^-  [(list move) seat]
    [(flop mow) sat]
  ::
  ++  apex                                              ::  returning card
    |=  [pan=term pax=path fav=card]
    ?+  pan  !!
      %boot  (boot pax fav)
      %poke  (poke pax fav)
      %user  (axon pax fav)
      %wait  (wait pax fav)
    ==
  ::
  ++  axon                                              ::  user card
    |=  [pax=path fav=card]
    ?>  ?=(
  ::
  ++  babe                                              ::  poke or wait
    |=  [pax=path fav=card]
    ?>  ?=(%meta -.fav)
          
    ?~  huv.sat
      (bait pax fav)
    %=    +>
        huv.sat  ~
    =+  gat=(slap u.huv.sat [%cnzy %poke])
    =+  sam=:(slop !>(law) !>(pax) cav)
    =+
  ::
  ++  boot                                              ::  boot result
    |=  rew=wren
    ?>  ?=(%made -.fav.rew)
    ?-  -.p.fav.rew
      |  (drip [%crud %gall p.p.fav])
      &  (dear p.fav q.q.fav)
    ==
  ::
  ++  poke                                              ::  poke result
    |=  rew=wren
    ?>  ?=(%made -.fav.rew)
    ?-  -.p.fav.rew
      |  (drip [%crud %gall p.p.fav])
      &  (pone q.q.p.fav)
    ==
  ::
  ++  pone                                              ::  poke product
    |=  vax=vase
    ^+  +>
    ?~  q.vax  +>
    =+  hed=(slot 2 vax)
    =.  +>.$  (poof (slot 2 vax))
    $(vax (slot 3 vax))
  ::
  ++  poof                                              ::  apply effect
    |=  vax=vase
    ^+  +>
    =+  nab=(,@ q:(slot 2 vax))
    =+  vib=(need (~(get by zos.sat) nab))
    =:  ost  nab
        law  p.vib
        hen  q.vib
      ==
    (puss (slot 3 vax))
  ::
  ++  puss                                              ::  poke gaff
    |=  vax=vase
    ^+  +>
    =.  vax  (spec vax)
    ?+    -.q.vax  !!
        %call
      =+  ney=`@tas`(sane %tas ((hard ,@) q:(slot 6 vax)))
      =+  cav=(quod (slot 7 vax))
      (call ney %used [%meta our cav])
    ::
        %drip
      (drip [%meta our (slot 3 vax)]) 
    ::
        %stop
      +>.$(zos (~(del by zos) ost))
    == 
  ::
  ++  quod                                              ::  typecheck hive
    |=  vax=vase
    vax                                                 ::  XX
  ::
  ++  kick                                              ::  run engine
    |-  ^+  +
    ?~  vey.sat  +.$
    ?~  huv.sat  +.$
    =^  lef  vey.sat  ~(get to vey.sat)
     
  ++  blam                                              ::  block on slam
    |=  [cod=@tas gat=vase sam=vase]
    ^+  +>
     
  ++  dear                                              ::  reset 
    |=  [dep=(set beam) vax=vase]
    deep(huv.sat `vax)

  ::
  ++  call                                              ::  advance a card
    |=  [vay=term pan=term fav=card]
    ^+  +>
    %=  +>  mow  :_  mow
      ^-  move
      :+  [~ %iron our]
        ^-  duct
        [[vay ~] [/g (scot %p our) app pan ~]
      fav
    ==
  ::
  ++  drip                                              ::  return a card
    |=  fav=card
    ^+  +>
    +>(mow :_(mow [[~ %iron our] hen fav]))
  ::
  ++  spec                                              ::  specialize kelp
    |=  vax=vase
    ^-  vase
    ?.  &(?=(^ q.vax) ?=(@ -.q.vax))
      vax
    %+  slap  vax 
    ^-  twig
    :+  %wtgr
      [%wtts [%dtzz %tas -.q.vax] [%$ 2]]
    [%$ 1]
  ::
  ++  more
    |-  ^+  +
    ?>  ?=(^ huv.sat)
    ?~  vey.sat  +.$
    =^  lef  vey.sat  ~(get to vey.sat) 
  ::
  ++  axon                                              ::  advancing card
    |=  fav=card
    ?~  huv.sat                                         ::  if blocked
      (bait pax cav)
    ?~  pax                                             ::  call, essentially
      =+  ^=  sam
          ;:  slop
            !>(law)
          ==
      =+  gat=(slap u.huv.sat [%cnzy %poke])
    !!
  ::
  ::
  ++  bait                                              ::  boot/wait
    |=  [pax=path cav=vase]
    ^+  +>
    ?>  ?=(~ huv.sat)
    =+  new==(~ vey.sat)
    =.  vey.sat  (~(put to vey.sat) `wren
    ?:  =(~ vey.sat)
  ::
  ++  boob                                              ::
  ::
  ++  boot                                              ::  send boot
    ^+  .
    (dyke %boot %boil %core [(scot %p our) %main (scot %da now) %app ~])
  ::
  ++  dyke                                              ::  send to compute
    |=  [pan=@tas kas=silk]
    ^+  +>
    %=  +>  mow  :_  mow
      ^-  move
      :+  [~ %iron our]
        ^-  duct
        [/f [/g (scot %p our) app pan ~]
      ^-  card
      [%exec kas]
    ==
  ::
  ++  feed
    |=  
    
  --
--
.  ==
=|  axle
=*  lex  -
|=  $:  now=@da 
        eny=@ 
        $=  ski
        $+  (unit (set monk)) 
        (unit (unit (hypo icon)))
^?                                                      ::  opaque core
|%                                                      ::
++  toss                                                ::  inbound card
  |=  [
  !!
++  dive
  |=  [hen=duct loc=path 
  !!
++  beat                                                ::  process move
  |=  [pax=path hen=duct typ=type fav=card]
  ^-  [p=(list move) q=vane]
  =+  ^=  def
      |.  ^-  seat
      %*  .  *seat
        eny  (shax (mix now eny))
        lat  now
      ==
  =^  mos  lex
    ?:  =(~ tea)
      ?.  ?=(%mean -.fav)  ~|(%gall-what !!)
      =+  ^=  mat  ^-  mast
          =+  mat=(~(get by pol.lex) p.fav)
          ?~(mat *mast u.mat)
      =+  ^=  sat  ^-  seat
          =+  sat=(~(get by bum.mat) q.fav)
          ?~(sat *def u.sat)
      =^  ost  sat  
        ^-  [bone seat]
        =+  byn=(~(get by zam.sat) hen)
        ?^  byn  u.byn
        [p.zam.sat sat(p.zam +(p.zam), q.zam (~(put by q.sam) hen p.zam.sat)]
      =+  ^=  tox
        :*  ost
            now
            eny
            r.fav
            *(list move)
            p.fav
            q.fav
            sat
        ==
      =^   mos  sat  abet:(~(axon za tox) fav)
      :-  mos
      %=  lex
        pol  (~(put by pol.lex) p.fav mat(bum (~(put by bum.mat) q.fav sat)))
      ==
    ?>  ?=([@ @ *] tea)
      =+  [our=(need (slaw %p i.tea)) app=(need (slaw %tas i.t.tea))]
      =+  mat=(need (~(get by pol.lex) our))
      =+  sat=(need (~(get by bum.mat) app))
      =+  ost=(need (~(get by q.zam.sat) hen))
      =+  law=p:(need (~(get by zos.sat) ost))
      =+  ^=  tox
        :*  ost
            now
            eny
            law
            *(list move)
            our
            app
            sat
        ==
      =^   mos  sat  abet:(~(axon za tox) t.t.tea fav)
      :-  mos
      lex(pol (~(put by pol.lex) p.fav mat(bum (~(put by bum.mat) q.fav sat))))
    ==
  [`(list move)`mos ..^$]
::
++  come
  |=  [sam=? old=vase]
  ^-  vane
  (load old)
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load
  |=  old=vase
  ^-  vane
  ?.  (~(nest ut -:!>(`axle`+>-.^$)) | p.old)
    ~&  %ford-reset
    ..^$
  ..^$(+>- (axle q.old))
::
++  raze
  ^-  vane
  ..$(+>- *axle)
::
++  scry
  |=  [our=ship ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit))
  ~
::
++  stay
  `vase`!>((colt `axle`+>-.$))
++  vern  [164 0]
--
