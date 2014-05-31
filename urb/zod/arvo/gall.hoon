!:  ::  %gall, user-level applications
!?  164
::::
|=  pit=vase
=>  =~                
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  structures
++  axle                                                ::  all %gall state
          $:  %0                                        ::  state version
              pol=(map ship mast)                       ::  apps by ship
          ==                                            ::
++  bone  ,@ud                                          ::  opaque duct
++  gift                                                ::  out result <-$
          $%  [%back p=?]                               ::  %mess ack good/bad
              [%bock p=path q=?]                        ::  %show on/off
              [%crud p=(list tank)]                     ::  error
              [%meta p=vase]                            ::  application gift
          ==                                            ::
++  kiss                                                ::  in request ->$
          $%  [%mess p=hasp q=(disk)]                   ::  urbit message
              [%nuke p=hasp]                            ::  reset this duct
              ::  [%puke p=(list tank) q=kiss]          ::  inbound error
              [%user p=(unit chop) q=kiss]              ::  restriction
              [%show p=hasp q=path r=(unit plan)]       ::  subscription
              [%soft p=*]                               ::  soft kiss
          ==                                            ::
++  knob                                                ::  pending action
          $%  [%boot ~]                                 ::  boot/reboot
              [%crud p=(list tank)]                     ::  error
              [%mess p=(disk)]                          ::  message
              [%nuke ~]                                 ::  reboot
              [%show p=path q=(unit plan)]              ::  subscription
              [%take p=path q=vase]                     ::  user result
          ==                                            ::
++  mast                                                ::  apps by ship
          $:  bum=(map ,@ta seat)                       ::  apps by name
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  typed move
++  note  curd                                          ::  out request $->
++  rapt  |*(a=$+(* *) (qual path path ,@da a))         ::  versioned result
++  scar                                                ::  opaque duct system
          $:  p=@ud                                     ::  bone sequence
              q=(map duct ,[p=bone q=(unit chop)])      ::  by duct
              r=(map bone duct)                         ::  by bone
          ==                                            ::  
++  seat                                                ::  the living app
          $:  huv=(unit vase)                           ::  application vase
              qic=(unit toil)                           ::  project
              vey=(qeu toil)                            ::  pending calls
              orm=(unit ,@da)                           ::  last buildtime
              ped=(set (pair ship desk))                ::  dependencies
              zam=scar                                  ::  opaque ducts
          ==                                            ::
++  sign                                                ::  in result $-<
          $%  [%made p=(each beet (list tank))]         ::  computed result
              [%ruse p=curd]                            ::  user wrapper
              [%writ p=riot]                            ::  version result
          ==                                            ::
++  suit                                                ::  application gift
          $%  [%back p=?]                               ::  %mess ack good/bad
              [%bock p=path q=?]                        ::  %show on/off
              [%boom ~]                                 ::  reset all ducts
              [%bust ~]                                 ::  reset this duct
              [%barn p=(rapt (disk))]                   ::  %v report
              [%crud p=(list tank)]                     ::  error notification
              [%dash p=(rapt null)]                     ::  %u report
              [%dish p=(rapt (disk))]                   ::  %z report
              [%diff p=(rapt (unit (disk)))]            ::  %w report
              [%file p=(rapt (disk))]                   ::  %x report
              [%fold p=(rapt arch)]                     ::  %y report
              [%rasp p=(disk)]                          ::  response message
          ==                                            ::
++  toil  (pair duct knob)                              ::  work in progress
--  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  functions   
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
        ska=sled                                        ::  activate
    ==                                                  ::  opaque core
=<  ^?
    |%                                                  ::  vane interface
    ++  call                                            ::  handle request
      |=  [hen=duct hil=(hypo kiss)]
      |-  ^-  [p=(list move) q=_..^^$]
      =+  =|  law=(unit chop)
          |-  ^-  $:  law=(unit chop)
                      hap=hasp
                      kon=knob
                  ==
          ?-  -.q.hil
            %mess  [law p.q.hil %mess q.q.hil]
            %show  [law p.q.hil %show q.q.hil r.q.hil]
            %soft  $(q.hil ((hard kiss) p.q.hil))
            %nuke  [law p.q.hil %nuke ~]
            %user  $(q.hil q.q.hil, law (limp p.q.hil law))
          ==
      abet:work:(quem:(boar:(goat hap) hen law) kon)
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
      ^-  (unit (unit (pair lode ,*)))
      =+  ^=  vew  ^-  lens                             ::  XX future scry
        %.  :-  use
            :-  [who syd ((hard case) p.lot)]
            (flop tyl)
        |=  $:  use=(unit (set monk))                   ::  observers
                bid=bead                                ::  position
            ==                                          ::
        (beef:(gaur p.bid q.bid) use r.bid s.bid)
      %+  bind
        ?+    ren  ~
          %u  u.vew
          %v  v.vew
          %w  w.vew
          %x  x.vew
          %y  y.vew
          %z  z.vew
        ==
      |=(a=(unit) (bind a |=(b=* [%noun b])))
    ::
    ++  doze
      |=  [now=@da hen=duct]
      ^-  (unit ,@da)
      ~
    ::
    ++  load
      |=  old=axle
      ^+  ..^$
      ..^$(all old)
    ::
    ++  stay  `axle`+>-.$
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
    ^-  lens
    ?.  =([%da now] lok)  *lens
    (~(show au use) pax)
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
    ++  show                                            ::  view
      |=  pax=path
      ^-  lens
      ?~  huv.sat  *lens
      =+  gat=(slap u.huv.sat [%cnzy %peek])
      =+  cor=(slam gat !>(pax))
      =+  ^=  dek
          |*  fun=$+(vase *)
          |=  nam=@tas
          =+  vax=(slap cor [%cnzy nam])
          ^-  (unit (unit fun))
          ?:  =(~ q.vax)  ~
          ?:  =([~ ~] q.vax)  [~ ~]
          [~ ~ (fun (slot 7 vax))]
      =+  ^=  nib
          |=  vax=vase
          ((hard null) q.vax)
      =+  ^=  yob
          |=  vax=vase  ^-  cage
          [((hard lode) -.q.vax) (slot 3 vax)]
      =+  ^=  yar
          |=  vax=vase  ^-  arch
          ((hard arch) q.vax)
      =+  ^=  dif
          |=  vax=vase  ^-  (unit cage)
          ?:  =(~ q.vax)  ~
          [~ (yob (slot 3 vax))]
      |%
      ++  u  ((dek nib) %u)
      ++  v  ((dek yob) %v)
      ++  w  ((dek dif) %w)
      ++  x  ((dek yob) %x)
      ++  y  ((dek yar) %y)
      ++  z  ((dek yob) %z)
      --
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
      ++  cave                                          ::  vase as silk
        |=  vax=vase
        [%done ~ %$ vax]
      ::
      ++  conf                                          ::  configured core
        |=  vax=vase
        ^-  silk
        :+  %mute  (core vax)
        :~  [[%$ 6]~ (cave !>([[our app] now eny]))]
        ==
      ++  core  |=(vax=vase (cove %core vax))           ::  core as silk
      ++  cove                                          ::  cage as silk
        |=  cay=cage
        ^-  silk
        [%done ~ cay]
      ::
      ++  drug                                          ::  set dependencies
        |=  pen=(set (pair ship desk))
        ^+  +>
        =+  ^=  new  ^-  (list move)
            %+  turn
              %+  skip  (~(tap in pen) ~)
              |=(a=(pair ship desk) (~(has in ped.sat) a))
            |=  a=(pair ship desk)
            :-  hen
            :^  %toss  %c  (away %s %drug (scot %p p.a) q.a ~)
            `card`[%warp [our p.a] q.a ~ %| [%da now] [%da (add now ~d1000)]]
        =+  ^=  old  ^-  (list move)
            %+  turn
              %+  skip  (~(tap in ped.sat) ~)
              |=(a=(pair ship desk) (~(has in pen) a))
            |=  a=(pair ship desk)
            :-  hen
            :^  %toss  %c  (away %s %drug (scot %p p.a) q.a ~)
            `card`[%warp [our p.a] q.a ~]
        %_(+>.$ ped.sat pen, mow :(weld new old mow))
      ::
      ++  drum                                          ::  raw dependencies
        |=  dep=(set bead)
        ^+  +>
        ?>  ?=(^ orm.sat)
        %-  drug
        =+  ped=`(set (pair ship desk))`[[our %main] ~ ~]
        =+  mav=(~(tap by dep) ~)
        |-  ^+  ped
        ?~  mav  ped
        ?:  =(r.i.mav [%da u.orm.sat])
          $(mav t.mav, ped (~(put in ped) p.i.mav q.i.mav))
        $(mav t.mav)
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
      ++  gone  %_(. qic.sat ~)                         ::  done work
      ++  game                                          ::  invoke core
        |=  [[pan=term arm=term] vax=vase sam=vase]
        %+  ford  pan
        [%call (gate arm (conf vax)) (cove %$ sam)]
      ::
      ++  home                                          ::  load application
        ^-  silk
        :+  %boil  %core
        [[our %main [%da now]] %app ~]
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
            [hen [%take t.pax (spec (slot 3 hil))]]
          ==
        ?>  ?=([%s @ ~] pax)
        ?>  !=(~ qic.sat)
        ?+    i.t.pax  !!
            %boot
          ~&  %more-boot
          ?>  ?=([~ * %boot ~] qic.sat)
          ?>  ?=(%made -.q.hil)
          ?-  -.p.q.hil
            &  (drum:(morn:gone q.q.p.p.q.hil) p.p.p.q.hil)
            |  (mort p.p.q.hil)
          ==
        ::
            %drug
          ~&  %more-drug
          ?>  ?=(%writ -.q.hil)
          ?~  p.q.hil  +>.$
          +>.$(vey.sat (~(put to vey.sat) hen %boot ~))
        ::
            %step
          ~&  %more-step
          ?>  ?=(%made -.q.hil)
          ?-  -.p.q.hil
            &  %-  obey:(morn:gone (slot 3 q.q.p.p.q.hil))
               (slot 2 q.q.p.p.q.hil)
            |  (give %crud p.p.q.hil)
          ==
        ==
      ::
      ++  morn                                          ::  successful boot
        |=  vax=vase
        %_(+> huv.sat `vax)
      ::
      ++  mort                                          ::  failed boot 
        |=  tan=(list tank)
        ~&  [%boot-lost our app]
        (give %crud tan)
      ::
      ++  nile  [%done ~ [%$ [%atom %n] ~]]             ::  null silk
      ++  obey                                          ::  process result
        |=  vax=vase
        %_(+> mow (weld (flop (said vax)) mow))
      ::
      ++  quem                                          ::  queue action
        |=  kon=knob                                    ::  content
        %_(+> vey.sat (~(put to vey.sat) hen kon))
      ::
      ++  said
        |=  vud=vase
        |-  ^-  (list move)
        ?:  =(~ q.vud)  ~
        [(sump (slot 2 vud)) $(vud (slot 3 vud))]
      ::
      ++  show                                          ::  subscribe
        |=  [pax=path pul=(unit plan)]                  ::  subscription
        ^+  +>
        %_(+> vey.sat (~(put to vey.sat) hen %show pax pul))
      ::
      ++  sump
        |=  wec=vase
        ^-  move
        :-  ((hard duct) -.q.wec)
        =+  caq=(spec (slot 3 wec))
        ?+    q.caq   ~&(%sump-bad !!)
        ::
            [%toss p=@tas q=* r=[p=@tas q=*]]
          :^  %toss  (need ((sand %tas) ((hard ,@) p.q.caq)))
            ((hard path) q.q.caq)
          [%meta (spec (slot 15 caq))]
        ::
            [%give p=[p=@tas q=*]]
          [%give [%meta (spec (slot 3 caq))]]
        ::
            [%slip p=@tas q=[p=@tas q=*]]
          :+  %slip
            (need ((sand %tas) ((hard ,@) p.q.caq)))
          [%meta (spec (slot 7 caq))]
        ==
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
          =.  orm.sat  `now
          %+  ford  %boot
          ^-  silk
          :+  %call
            (gate %prep home)
          ?~  huv.sat  nile
          [nile (gate %save (conf u.huv.sat))]
        ::
            %crud
          ~&  %yawn-crud
          ?~  huv.sat
            ~&  [%crud-none our app]
            gone:(give %crud p.kon)
          %^  game  [%step %pain]  u.huv.sat
          !>([ost use p.kon])
        ::
            %mess
          ~&  %yawn-mess
          ?~  huv.sat
            ~&  [%mess-none our app]
            gone:(give %back |)
          %^  game  [%step %poke]  u.huv.sat
          :(slop [[%atom %ud] ost] !>((ride use say)) q.p.kon)
        ::
            %nuke
          ~&  %yawn-mess
          ?~  huv.sat
            ~&  [%nuke-none our app]
            gone
          (game [%step %punk] u.huv.sat !>([ost ~]))
        ::
            %show
          ~&  %yawn-show
          ?~  huv.sat
            ~&  [%show-none our app]
            gone:(give %bock p.kon |)
          %^  game  [%step %peer]  u.huv.sat
          !>([ost use p.kon q.kon])
        ::
            %take
          ~&  %yawn-take
          ?>  ?=(^ huv.sat)
          %^  game  [%step %peck]  u.huv.sat
          :(slop [[%atom %ud] ost] !>((ride use say)) !>(p.kon) q.kon)
        ==
      --
    --
  --
--
