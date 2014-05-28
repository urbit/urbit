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
++  cork  ?(%u %v %w %x %y %z)                          ::  viewport
++  gift                                                ::  out result <-$
  $%  [%back p=?]                                       ::  ack good/bad
      [%barn p=(rapt (disk))]                           ::  %v report
      [%crud p=(list tank)]                             ::  error notification
      [%dash p=(rapt null)]                             ::  %u report
      [%dish p=(rapt (disk))]                           ::  %z report
      [%diff p=(rapt (unit (disk)))]                    ::  %w report
      [%file p=(rapt (disk))]                           ::  %x report
      [%fold p=(rapt arch)]                             ::  %y report
      [%ruse p=gift]                                    ::  user-wrapped
  ==                                                    ::
++  hasp  ,[p=ship q=term]                              ::  app identity
++  kiss                                                ::  in request ->$
  $%  [%nuke ~]                                         ::  
      [%mess p=hasp q=(disk)]                           ::  urbit message
      [%puke p=(list tank) q=kiss]                      ::  inbound error
      [%user p=(unit chop) q=kiss]                      ::  restriction
      [%show p=hasp q=pipe]                             ::  subscription
      [%soft p=*]                                       ::  soft kiss
  ==                                                    ::
++  knob                                                ::  pending action
  $%  [%boot ~]                                         ::  boot/reboot
      [%mess p=(disk)]                                  ::  message
      [%puke p=(each (disk) pipe) q=(list tank)]        ::  error
      [%show p=(unit pipe)]                             ::  subscription
      [%take p=path q=vase]                             ::  user result
      [%boot ~]                                         ::  reboot
  ==                                                    ::
::                                                      ::
++  mast                                                ::  apps by ship
  $:  bum=(map ,@ta seat)                               ::  apps by name
  ==                                                    ::
++  move  ,[p=duct q=(mold note gift)]                  ::  typed move
++  note  curd                                          ::  out request $->
++  pipe  (pair (pair cork wind) path)                  ::  simple query
++  seat                                                ::  the living app
  $:  huv=(unit vase)                                   ::  application vase
      qic=(unit toil)                                   ::  project
      vey=(qeu toil)                                    ::  pending calls
      tik=@ud                                           ::  state tick
    $=  zam                                             ::  opaque duct system
      $:  p=@ud                                         ::  sequence
          q=(map duct ,[p=bone q=(unit chop)])          ::  by duct
          r=(map bone duct)                             ::  by bone
  ==  ==                                                ::  
++  sign                                                ::  in result $-<
  $%  [%made p=(each beet (list tank))]                 ::  computed result
      [%ruse p=curd]                                    ::
  ==                                                    ::
++  toil  (pair duct knob)                              ::  work in progress
++  wind                                                ::  request/subscribe
  $%  [%da p=@da q=(unit ,@dr)]                         ::  timeline
      [%ud p=@ud q=(unit ,@ud)]                         ::  sequence
      [%tas p=@tas]                                     ::  label
  ==                                                    ::
++  rapt  |*(a=$+(* *) (pair ,@da a))                   ::  versioned result
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
++  ride                                                ::  all permission
  |=  [use=(unit (set monk)) say=(unit (set monk))]
  ^-  (unit chop)
  ?~(say ~ `[use u.say])
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
++  limp                                                ::  merge chops
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
          %user  $(law.way (limp p.q.hil law.way)) 
          %show  [way p.q.hil %| q.q.hil]
        ==
      =+  bor=(boar:(goat hap.way) hen law.way)
      =<  abet  =<  work
      ?^  puc.way  (puke:bor u.puc.way act.way)
      ?-  -.act.way
        &  (mess:bor p.act.way)
        |  (show:bor p.act.way)
      ==
    ::    
    ++  take                                            ::  accept response
      |=  [pax=path hen=duct hil=(hypo sign)]           ::
      ^-  [(list move) _..^$]
      =+  lum=(lump pax)
      =<  abet  =<  work
      (more:(bear:(gaur p.lum) hen) q.lum hil)
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
      =+  ^=  vew                                       ::  XX future scry
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
    ::  XXX old vane junk - fix and/or destroy
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
  ++  away                                              ::  application path
    |=  pax=path  ^-  path
    [(scot %p our) app pax]
  ::
  ++  bear                                              ::  write backward
    |=  hen=duct
    =+  orf=(need (~(get by q.zam.sat) hen))
    ~(apex bo:~(. au (read q.orf)) hen p.orf (rite q.orf) ~)
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
    ~(apex bo:~(. au (read q.orf)) hen p.orf (rite q.orf) ~)
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
              say=(unit (set monk))                     ::  write permission
              mow=(list move)                           ::  actions
          ==
      ++  abet  [(flop mow) ^abet]                      ::  resolve
      ++  apex
        ^+  .
        ?.  &(=(~ huv.sat) =(~ qic.sat) =(~ vey.sat))  .
        %_(. vey.sat (~(put to vey.sat) hen [%boot ~]))
      ::
      ++  bing                                          ::  reset to duct
        |=  neh=duct
        =+  orf=(need (~(get by q.zam.sat) hen))
        %_    +>.$
            hen  neh
            ost  p.orf
            use  (read q.orf)
            say  (rite q.orf)
        ==
      ::
      ++  core  |=(vax=vase (cove %core vax))           ::  core as silk
      ++  cove                                          ::  cage as silk
        |=  cay=cage
        ^-  silk
        [%done ~ cay]
      ::
      ++  ford                                          ::  exec to ford
        |=  [pan=term kas=silk]
        %_    +>.$
            mow
          :_(mow [hen [%toss %f (away [%s pan ~]) [%exec our `kas]]])
        ==
      ::
      ++  gate                                          ::  gate as silk
        |=  [arm=term kas=silk]
        ^-  silk
        [%pass kas [%1 [%cnzy arm]]]
      ::
      ++  give                                          ::  return card
        |=  gip=gift
        %_(+> mow [[hen %give gip] mow])
      ::
      ++  gone  %_(+> qic.sat ~)                        ::  done work
      ++  home                                          ::  load application
        ^-  silk
        :+  %boil  %core
        [(scot %p our) %main (scot %da now) %app ~]
      ::
      ++  mess                                          ::  accept message
        |=  dur=(disk)                                  ::  content
        ^+  +>
        %_(+> vey.sat (~(put to vey.sat) hen %mess dur))
      ::
      ++  more                                          ::  accept result
        |=  $:  pax=path                                ::  internal position
                hil=(hypo sign)                         ::  urbit event
            ==
        ^+  +>
        ?:  ?=([%u *] pax)
          ?.  ?=(%ruse -.q.hil)  
            ~&  [%more-card -.q.hil pax]
            !!
          %_    +>.$
              vey.sat 
            %-  ~(put to vey.sat) 
            `toil`[hen `knob`[%take t.pax (spec (slot 3 hil))]]
          ==
        ?>  ?=([%s @ ~] pax)
        ?>  !=(~ qic.sat)
        ?+    i.t.pax  !!
            %boot
          ?>  ?=([~ * %boot ~] qic.sat)
          ?>  ?=(%made -.q.hil)
          ?-  -.p.q.hil
            &  (morn p.p.q.hil)
            |  (mort p.p.q.hil)
          ==
        ==
      ::
      ++  morn                                          ::  successful boot
        |=  [dep=(set beam) cay=cage]
        ~&  [%boot-good our app]
        %_(+> qic.sat ~, huv.sat `q.cay)
      ::
      ++  mort                                          ::  failed boot 
        |=  tan=(list tank)
        ~&  [%boot-lost our app]
        %_(+> mow :_(mow [hen [%give %crud tan]]))
      ::
      ++  nile  [%done ~ [%$ [%atom %n] ~]]             ::  null silk
      ++  puke                                          ::  propagate error
        |=  $:  tan=(list tank) 
                act=(each (disk) pipe)
            ==
        %_(+> vey.sat (~(put to vey.sat) hen %puke act tan))
      ::
      ++  show                                          ::  subscribe
        |=  pyp=pipe                                    ::  subscription
        ^+  +>
        %_(+> vey.sat (~(put to vey.sat) hen %show pyp))
      ::
      ++  work                                          ::  eat queue
        ^+  .
        ~&  %gall-work
        ?:  |(?=(^ qic.sat) =(~ vey.sat))  .            ::  nothing to do
        =^  yev  vey.sat  [p q]:~(get to vey.sat)
        work:(yawn:(bing p.yev) q.yev)
      ::
      ++  yawn                                          ::  start event
        |=  kon=knob
        ^+  +>
        =.  qic.sat  `[hen kon]
        ?-    -.kon
            %boot
          ~&  %yawn-boot
          %+  ford  %boot
          ^-  silk
          :+  %call
            (gate %prep home)
          ?~(huv.sat nile [nile (gate %save u.huv.sat)])
        ::
            %mess
          ~&  %yawn-mess
          %+  ford  %mess
          ^-  silk
          ?~  huv.sat
            ~&  [%mess-none our app]
            gone:(give %back |)
          :+  %call
            (gate %poke u.huv.sat)
          %+  cove  %$
          :(slop [[%atom %ud] ost] !>((ride use say)) q.p.kon)
          !!
        ::
            %show  !!
            %take  !!
        ==
      --
    --
  --
--
