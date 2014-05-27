!:  ::  %gall, user-level extension
!?  164
::::
|=  pit=vase
=>  =~                                                  ::  preface
|%                                                      ::  structures
++  axle                                                ::  all %gall state
  $:  ven=%0                                            ::  state version
      pol=(map ship mast)                               ::  apps by ship
  ==                                                    ::
++  bone  ,@ud                                          ::  opaque duct
++  cork  ?(%u %v %w %x %y %z)                          ::  view angle
++  gift                                                ::  out result <-$
  $%  [%back ~]                                         ::  acknowledgment
      [%crud p=(list tank)]                             ::  error notification
      [%seen p=@da q=*]                                 ::  wind update
  ==                                                    ::
++  hasp  ,[p=ship q=term]                              ::  app identity
++  kiss                                                ::  in request ->$
  $%  [%mess p=hasp q=(disk)]                           ::  urbit message
      [%puke p=(list tank) q=kiss]                      ::  rising error
      [%user p=chop q=kiss]                             ::  restriction
      [%show p=hasp q=pipe]                             ::  subscription
      [%soft p=*]                                       ::  soft kiss
  ==                                                    ::
++  mast                                                ::  apps by ship
  $:  bum=(map ,@ta seat)                               ::  apps by name
  ==                                                    ::
++  move  ,[p=duct q=(mold note gift)]                  ::  typed move
++  note  curd                                          ::  out request $->
++  pipe  (pair (pair cork wind) path)                  ::  simple query
++  seat                                                ::  the living app
  $:  $=  zam                                           ::  opaque duct system
      $:  p=@ud                                         ::  sequence
          q=(map duct ,[p=bone q=(unit chop)])          ::  by duct
          r=(map bone duct)                             ::  by bone
  ==  ==                                                ::  
++  sign  curd                                          ::  in result $-<
++  wind                                                ::  request/subscribe
  $%  [%da p=@da q=(unit ,@dr)]                         ::  timeline
      [%ud p=@ud q=(unit ,@ud)]                         ::  sequence
      [%tas p=@tas]                                     ::  label
  ==                                                    ::
--                                                      ::
|%                                                      ::  functions
++  byby                                                ::  double bind
  |*  [a=(unit (unit)) b=$+(* *)]
  ?~  a  ~
  ?~  u.a  [~ u=~]
  [~ u=[~ u=(b u.u.a)]]
::                                                      ::
++  colt                                                ::  reduce to save
  |=  all=axle                                          ::
  all
::
++  read                                                ::  read permission
  |=  law=(unit chop)
  ^-  (unit (set monk))
  ?~(law [~ ~] p.u.law)
::
++  rite                                                ::  write permission
  |=  law=(unit chop)
  ^-  (unit (set monk))
  ?~(law ~ `q.u.law)
::
++  grom                                                ::  merge sets
  |*  [one=(set) two=(set)]
  ^+  one
  (~(gas in one) (~(tap in two) ~))                     ::  XX ugh
::
++  grum                                                ::  merge maps
  |*  [one=(map) two=(map)]
  ^+  one
  (~(gas by one) (~(tap by two) ~))                     ::  XX ugh
::
++  limp                                                ::  merge authorities
  |=  [a=(unit chop) b=(unit chop)]
  ^-  (unit chop)
  ?~  a  b
  ?~  b  a
  :-  ~
  :-  ?~(p.u.a ~ ?~(p.u.b ~ `(grom u.p.u.b u.p.u.a)))
  (grom q.u.b q.u.a)
::
++  lump                                                ::  position
  |=  pax=path
  ^-  [p=[p=ship q=term] q=path]
  ?>  ?=([@ @ *] pax)
  :-  :-  (need (slaw %p i.pax)) 
      (need ((sand %tas) i.t.pax))
  t.t.pax
--
.  ==                                                   ::  end preface
=|  all=axle                                            ::  all vane state
|=  $:  now=@da                                         ::  urban time
        eny=@                                           ::  entropy
        ska=$+(* (unit (unit)))                         ::  activate
    ==                                                  ::  opaque core
    ::  $=  ski                                         ::  namespace
    ::  $+  $:  (unit (set monk))                       ::  rights
    ::          path                                    ::  name
    ::  ==  (unit (unit (disk)))                        ::  known/any/value
=<  ^?
    |%                                                  ::  vane interface
    ++  call                                            ::  handle request
      |=  [hen=duct hil=(hypo kiss)]
      |-  ^-  [p=(list move) q=_..^^$]
      ?:  ?=(%soft -.q.hil)
        $(q.hil ((hard kiss) p.q.hil))
      =+  ^=  way
        =|  $=  way
            $:  puc=(unit (list tank))
                law=(unit chop)
            ==
        |-  ^-  $:  _way 
                    hap=hasp
                    act=(each (disk) pipe)
                ==
        ?-  -.q.hil
          %mess  [way p.q.hil %& q.q.hil]
          %puke  $(puc.way `p.q.hil)
          %user  $(law.way (limp `p.q.hil law.way)) 
          %show  [way p.q.hil %| q.q.hil]
        ==
      =+  bor=(boar:(goat hap.way) hen law.way)
      =<  abet
      ?^  puc.way  (puke:bor u.puc.way act.way)
      ?-  -.act.way
        &  (mess:bor p.act.way)
        |  (show:bor p.act.way)
      ==
    ::    
    ++  take                                            ::  accept response
      |=  [pax=path hen=duct hil=(hypo card)]           ::
      ^-  [(list move) _..^$]
      =+  lum=(lump pax)
      abet:(more:(bear:(gaur p.lum) hen) q.lum hil)
    ::
    ++  scry
      |=  $:  use=(unit (set monk))
              ren=@tas
              who=ship 
              syd=desk 
              lot=coin 
              tyl=path
          ==
      ^-  (unit (unit))
      =+  ^=  vew
        %.  :-  use
            :-  [who syd ((hard case) p.lot)]
            (flop tyl)
        |=  $:  use=(unit (set monk))                   ::  observers
                bid=bead                                ::  position
            ==                                          ::
        (beef:(gaur p.bid q.bid) use r.bid s.bid)
      ?+    ren  ~
        %u  u.vew
        %v  v.vew
        %w  w.vew
        %x  x.vew
        %y  y.vew
        %z  z.vew
      ==
    ::
    ::  XXX old junk - fix and/or destroy
    ::
              ++  come
                |=  [sam=? old=vase]
                ^+  ..^$
                (load old)
              ::
              ++  doze
                |=  [now=@da hen=duct]
                ^-  (unit ,@da)
                ~
              ::
              ++  load
                |=  old=vase
                ^+  ..^$
                ?.  (~(nest ut -:!>(`axle`+>-.^$)) | p.old)
                  ~&  %gall-reset
                  ..^$
                ..^$(all (axle q.old))
              ::
              ++  raze
                ^+  ..$
                ..$(all *axle)
              ::
              ++  stay  `vase`!>((colt `axle`+>-.$))
              ++  vern  [164 0]
    -- 
|%                                                      ::  inner core
++  gaur                                                ::  take and go
  |=  [our=@p app=@tas]
  =+  mat=(need (~(get by pol.all) our))
  =+  sat=(need (~(get by bum.mat) app))
  ~(. go [our app] mat sat)
::
++  goat                                                ::  call and go
  |=  [our=@p app=@tas]
  =+  ^=  mat  ^-  mast                               
      =+  mat=(~(get by pol.all) our)
      ?~(mat *mast u.mat)
  =+  ^=  sat  ^-  seat
      =+  sat=(~(get by bum.mat) app)
      ?^  sat  u.sat
      *seat
      ::  %*  .  *seat
      ::    eny  (shax (mix now eny))
      ::    lat  now
      ::  ==
  ~(. go [our app] mat sat)
::
++  go                                                  ::  application core
  |_  $:  $:  our=@p                                    ::  application owner
              app=@tas                                  ::  application name
          ==                                            ::
          mat=mast                                      ::  per owner
          sat=seat                                      ::  per application
      ==                                                ::
  ++  abet                                              ::  resolve
    %_    ..$
        all
      %_  all
        pol  %+  ~(put by pol.all)  our 
             mat(bum (~(put by bum.mat) app sat))
      ==
    ==
  ::
  ++  bear                                              ::  write backward
    |=  hen=duct
    =+  orf=(need (~(get by q.zam.sat) hen))
    ~(. bo:~(. au (read q.orf)) hen p.orf (rite q.orf) ~)
  ::
  ++  beef                                              ::  read in
    |=  [use=(unit (set monk)) lok=case pax=path]
    ^-  view
    ?.  =([%da now] lok)  *view
    (~(lens au use) pax)
  ::
  ++  boar                                              ::  write forward
    |=  $:  hen=duct                                    ::  cause
            law=(unit chop)                             ::  permissions
        ==
    =^  orf  zam.sat
      =+  orf=(~(get by q.zam.sat) hen)
      ?^  orf
        [[p=p.u.orf q=(limp law q.u.orf)] zam.sat]
      :^  [p=p.zam.sat q=law]  +(p.zam.sat)
        (~(put by q.zam.sat) hen [p.zam.sat law])
      (~(put by r.zam.sat) p.zam.sat hen)
    ~(. bo:~(. au (read q.orf)) hen p.orf (rite q.orf) ~)
  ::
  ++  au                                                ::  read
    |_  use=(unit (set monk))                           ::  read permission
    ++  abet  ^abet                                     ::  resolve
    ++  lens                                            ::  view
      |=  pax=path
      ^-  view
      *view
    ::
    ++  bo
      |_  $:  hen=duct                                  ::  system cause
              ost=bone                                  ::  opaque cause
              seg=(unit (set monk))                     ::  write permission
              mow=(list move)                           ::  actions
          ==
      ++  abet  [(flop mow) ^abet]                      ::  resolve
      ++  mess                                          ::  general message
        |=  dur=(disk)                                  ::  content
        ^+  +>
        +>
      ::
      ++  more                                          ::  process result
        |=  $:  pax=path                                ::  internal position
                hil=(hypo curd)                         ::  urbit event
            ==
        ^+  +> 
        +>
      ::
      ++  puke                                          ::  propagate error
        |=  $:  tan=(list tank) 
                act=(each (disk) pipe)
            ==
        ^+  +>
        +>
      ::
      ++  show                                          ::  subscribe
        |=  pyp=pipe                                    ::  subscription
        ^+  +>
        +>
      --
    --
  --
--
