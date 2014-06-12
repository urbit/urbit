!:  ::  %gall, user-level applications
!?  164
::::
|=  pit=vase
=>  =~                
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    structures
++  axle                                                ::  all %gall state
          $:  %0                                        ::  state version
              pol=(map ship mast)                       ::  apps by ship
          ==                                            ::
++  bone  ,@ud                                          ::  opaque duct
++  chop  ,[p=@ud q=@da]                                ::  revision/date
++  gift                                                ::  out result <-$
          $%  [%back p=?]                               ::  %mess ack good/bad
              [%crud p=@tas q=(list tank)]              ::  error
              [%rasp p=cage]                            ::  reaction message
              [%rump p=chop]                            ::  updates stop
              [%rush p=chop q=cage]                     ::  difference
              [%rust p=chop q=cage]                     ::  full update
              [%meta p=vase]                            ::  meta-gift

          ==                                            ::
++  hasp  ,[p=ship q=term]                              ::  app identity
++  kiss                                                ::  in request ->$
          $%  [%show p=hasp q=ship r=path]              ::  subscribe
              [%cuff p=(unit cuff) q=kiss]              ::  controlled kiss
              [%mess p=hasp q=ship r=cage]              ::  message
              [%nuke p=hasp]                            ::  clear all state
          ==                                            ::
++  knob                                                ::  pending action
          $%  [%boot ~]                                 ::  boot/reboot
              [%crud p=@tas q=(list tank)]              ::  error
              [%mess p=ship q=cage]                     ::  message
              [%show p=ship q=path]                     ::  subscribe
              [%nuke ~]                                 ::  clear duct
              [%take p=path q=vase]                     ::  user result
          ==                                            ::
++  mast                                                ::  apps by ship
          $:  bum=(map ,@ta seat)                       ::  apps by name
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  typed move
++  note                                                ::  out request $->
          $%  [%exec p=@p q=(unit silk)]                ::  see %ford
              [%meta p=vase]                            ::  meta-note
              [%warp p=sock q=riff]                     ::  see %clay
          ==                                            ::
++  rapt  |*(a=$+(* *) (qual path path ,@da a))         ::  versioned result
++  rave                                                ::  see %clay
          $%  [& p=mood]                                ::  single request
              [| p=moat]                                ::  change range
          ==                                            ::
++  riff  ,[p=desk q=(unit rave)]                       ::  see %clay
++  scar                                                ::  opaque duct system
          $:  p=@ud                                     ::  bone sequence
              q=(map duct ,[p=bone q=(unit cuff)])      ::  by duct
              r=(map bone duct)                         ::  by bone
          ==                                            ::  
++  seat                                                ::  the living app
          $:  huv=(unit vase)                           ::  application vase
              qic=(unit toil)                           ::  current project
              vey=(qeu toil)                            ::  pending projects
              tik=@ud                                   ::  build number
              orm=(unit ,@da)                           ::  build date
              sup=(map duct (pair ship path))           ::  subscribers
              ped=(set (pair ship desk))                ::  active depends
              zam=scar                                  ::  opaque ducts
          ==                                            ::
++  sign                                                ::  in result $-<
          $%  [%made p=(each bead (list tank))]         ::  by %ford
              [%ruse p=curd]                            ::  user wrapper
              [%writ p=riot]                            ::  by %clay
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
  |=  law=(unit cuff)
  ^-  (unit (set monk))
  ?~(law [~ ~] p.u.law)
::
++  ride                                                ::  all permission
  |=  [use=(unit (set monk)) say=(unit (set monk))]
  ^-  (unit cuff)
  ?~(say ~ `[use u.say])
::
++  rite                                                ::  write permission
  |=  law=(unit cuff)
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
++  limp                                                ::  merge cuffs
  |=  [a=(unit cuff) b=(unit cuff)]
  ^-  (unit cuff)
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
      |=  [hen=duct hic=(hypo (hobo kiss))]
      =>  .(q.hic ?.(?=(%soft -.q.hic) q.hic ((hard kiss) p.q.hic)))
      |-  ^-  [p=(list move) q=_..^^$]
      =+  =|  law=(unit cuff)
          |-  ^-  $:  law=(unit cuff)
                      hap=hasp
                      kon=knob
                  ==
          ?-  -.q.hic
            %cuff  $(q.hic q.q.hic, law (limp p.q.hic law))
            %mess  [law p.q.hic %mess q.q.hic r.q.hic]
            %show  [law p.q.hic %show q.q.hic r.q.hic]
            %nuke  [law p.q.hic %nuke ~]
          ==
      abet:work:(quem:(boar:(goat hap) hen law) kon)
    ::    
    ++  take                                            ::  accept response
      |=  [pax=path hen=duct hin=(hypo sign)]           ::
      ^-  [p=(list move) q=_..^$]
      =+  lum=(lump pax)
      =<  abet  =<  work
      (more:(bear:(gaur p.lum) hen) q.lum hin)
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
                bid=beam                                ::  position
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
            law=(unit cuff)                             ::  permissions
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
        ?.  &(=(~ huv.sat) =(~ qic.sat) =(~ vey.sat) =(~ ped.sat))  .
        %_(. vey.sat (~(put to vey.sat) hen [%boot ~]))
      ::
      ++  bing                                          ::  reset to duct
        |=  neh=duct
        =+  orf=(need (~(get by q.zam.sat) neh))
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
        ::  (core vax)
        :+  %mute  (core vax)
        :~  [[%$ 12]~ (cave !>([[our app] 0 0 eny now]))]
        ==
      ++  core  |=(vax=vase (cove %core vax))           ::  core as silk
      ++  cove                                          ::  cage as silk
        |=  cay=cage
        ^-  silk
        [%done ~ cay]
      ::
      ++  deal                                          ::  advance tick
        ^+  .
        =.  tik.sat  +(tik.sat)
        =+  pys=(~(tap by sup.sat) ~)
        ::  ~&  [%gall-deal tik.sat pys]
        |-  ^+  +>.$
        ?~  pys  +>.$
        =.  +>.$  $(pys t.pys)
        %=    +>.$
            vey.sat
          (~(put to vey.sat) [p.i.pys [%show q.i.pys]])
        ==
      ::
      ++  drug                                          ::  set dependencies
        |=  pen=(set (pair ship desk))
        ::  ~&  [%drug %pen pen]
        ::  ~&  [%drug %ped ped.sat]
        ^+  +>
        =+  ^=  new  ^-  (list move)
            %+  turn
              %+  skip  (~(tap in pen) ~)
              |=(a=(pair ship desk) (~(has in ped.sat) a))
            |=  a=(pair ship desk)
            :-  hen
            :^  %toss  %c  (away %s %drug (scot %p p.a) q.a ~)
            [%warp [our p.a] q.a ~ %| [%da now] [%da (add now ~d1000)]]
        =+  ^=  old  ^-  (list move)
            %+  turn
              %+  skip  (~(tap in ped.sat) ~)
              |=(a=(pair ship desk) (~(has in pen) a))
            |=  a=(pair ship desk)
            :-  hen
            :^  %toss  %c  (away %s %drug (scot %p p.a) q.a ~)
            [%warp [our p.a] q.a ~]
        %_(+>.$ ped.sat pen, mow :(weld new old mow))
      ::
      ++  drum                                          ::  raw dependencies
        |=  dep=(set beam)
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
        |=  [pax=path kas=silk]
        %_    +>.$
            mow
          :_(mow [hen [%toss %f (away [%s pax]) [%exec our `kas]]])
        ==
      ::
      ++  give                                          ::  return card
        |=  gip=gift
        %_(+> mow [[hen %give gip] mow])
      ::
      ++  gone  %_(. qic.sat ~)                         ::  done work
      ++  game                                          ::  invoke core
        |=  [[arm=term pax=path] vax=vase sam=vase]
        %+  ford  pax
        [%call (harm arm (conf vax)) (cove %$ sam)]
      ::
      ++  harm                                          ::  arm as silk
        |=  [arm=term kas=silk]
        ^-  silk
        [%pass kas [%1 [%cnzy arm]]]
      ::
      ++  home                                          ::  load application
        ^-  silk
        :+  %boil  %core
        [[our %main [%da now]] app %app ~]
      ::
      ++  more                                          ::  accept result
        |=  $:  pax=path                                ::  internal position
                hin=(hypo sign)                         ::  urbit event
            ==
        ^+  +>
        ?:  ?=([%u *] pax)
          ?.  ?=(%ruse -.q.hin)
            ~&  [%more-card -.q.hin pax]
            !!
          %_    +>.$
              vey.sat 
            %-  ~(put to vey.sat) 
            [hen [%take t.pax (spec (slot 3 hin))]]
          ==
        ?>  ?=([%s @ *] pax)
        ?+    i.t.pax  !!
            %boot
          ?>  ?=([~ * %boot ~] qic.sat)
          ?>  ?=(%made -.q.hin)
          ?-  -.p.q.hin
            &  ~&  [%boot-good our app tik.sat]
               deal:(drum:(morn:gone q.q.p.p.q.hin) p.p.p.q.hin)
            |  ~&  [%boot-lost our app tik.sat]
               (drum:(mort:gone p.p.q.hin) ~)
          ==
        ::
            %drug
          ?>  ?=(%writ -.q.hin)
          ?>  ?=([@ @ ~] t.t.pax)
          =+  :*  our=(need (slaw %p i.t.t.pax))
                  syd=(need ((sand %tas) i.t.t.t.pax)) 
              ==
          =.  ped.sat  (~(del by ped.sat) [our syd])
          ?~  p.q.hin  
            +>.$
          +>.$(vey.sat (~(put to vey.sat) hen %boot ~))
        ::
            %step
          ?>  ?=(%made -.q.hin)
          ?-  -.p.q.hin
            &  ::  ~&  %step-good
               %-  obey:(morn:gone (slot 3 q.q.p.p.q.hin))
               (slot 2 q.q.p.p.q.hin)
            |  ::  ~&  %step-fail
               (give %crud %made p.p.q.hin)
          ==
        ::
            %show
          ?>  ?=(%made -.q.hin)
          ?>  ?=([@ *] t.t.pax)
          =+  you=(need (slaw %p i.t.t.pax))
          ?-  -.p.q.hin
            &  =.  sup.sat  (~(put by sup.sat) hen you t.t.t.pax)
               %-  obey:(morn:gone (slot 3 q.q.p.p.q.hin))
               (slot 2 q.q.p.p.q.hin)
            |  ::  ~&  %step-fail
               (give %crud %made p.p.q.hin)
          ==
        ==
      ::
      ++  morn                                          ::  successful boot
        |=  vax=vase
        ^+  +>
        %_(+> huv.sat `vax)
      ::
      ++  mort                                          ::  failed boot 
        |=  tan=(list tank)
        (give %crud %boot-lost tan)
      ::
      ++  nile  [%done ~ [%$ [%cube 0 [%atom %n]] ~]]   ::  null silk
      ++  obey                                          ::  process result
        |=  vax=vase
        %_(+> mow (weld (flop (said vax)) mow))
      ::
      ++  quem                                          ::  queue action
        |=  kon=knob                                    ::  content
        ^+  +>
        %_(+> vey.sat (~(put to vey.sat) hen kon))
      ::
      ++  said
        |=  vud=vase
        |-  ^-  (list move)
        ?:  =(~ q.vud)  ~
        [(sump (slot 2 vud)) $(vud (slot 3 vud))]
      ::
      ++  show                                          ::  subscribe
        |=  [you=ship pax=path]                         ::  subscription
        %_(+> vey.sat (~(put to vey.sat) hen %show you pax))
      ::
      ++  nuke                                          ::  end 
        %_(. vey.sat (~(put to vey.sat) hen %nuke ~))
      ::
      ++  sumo                                          ::  standard gift 
        |=  vig=vase
        ^-  gift
        ?+    q.vig  [%meta vig]
            [%rasp *]
          :+  %rasp
            ((hard lode) +<.q.vig) 
          (slot 7 vig)
        ::
            [%rust *]
          :^    %rust
              [tik.sat now]
            ((hard lode) +<.q.vig)
          (slot 7 vig)
        ==
      ::
      ++  sump
        |=  wec=vase
        ^-  move
        :-  (need (~(get by r.zam.sat) ((hard bone) -.q.wec)))
        =+  caq=(spec (slot 3 wec))
        ?+    q.caq   ~&(%sump-bad !!)
        ::
            [%toss p=@tas q=* r=[p=@tas q=*]]
          :^  %toss  (need ((sand %tas) ((hard ,@) p.q.caq)))
            ((hard path) q.q.caq)
          [%meta (spec (slot 15 caq))]
        ::
            [%give p=[p=@tas q=*]]
          [%give (sumo (spec (slot 3 caq)))]
        ::
            [%slip p=@tas q=[p=@tas q=*]]
          :+  %slip
            (need ((sand %tas) ((hard ,@) p.q.caq)))
          [%meta (spec (slot 7 caq))]
        ==
      ::
      ++  work                                          ::  eat queue
        ^+  .
        ?:  |(?=(^ qic.sat) =(~ vey.sat))  .            ::  nothing to do
        =^  yev  vey.sat  [p q]:~(get to vey.sat)
        work:(yawn:(bing p.yev) q.yev)
      ::
      ++  yawn                                          ::  start event
        |=  kon=knob
        ^+  +>
        ::  ~&  [%gall-yawn ost -.kon]
        =.  qic.sat  `[hen kon]
        ?-    -.kon
            %boot
          =.  orm.sat  `now
          %+  ford  /boot
          ^-  silk
          :+  %call
            (harm %prep home)
          ?~  huv.sat  
            nile
          [nile (harm %save (conf u.huv.sat))]
        ::
            %crud
          ?~  huv.sat
            ~&  [%crud-none our app]
            gone:(give %crud p.kon q.kon)
          %^  game  [%pain /step]  u.huv.sat
          !>([ost use p.kon])
        ::
            %nuke
          ::  ~&  %yawn-nuke
          gone
        ::
            %mess
          ?~  huv.sat
            ~&  [%mess-none our app]
            gone:(give %back |)
          %^  game  [%poke /step]  u.huv.sat
          :(slop [[%atom %ud] ost] !>((ride use say)) q.q.kon)
        ::
            %show
          ?~  huv.sat
            ~&  [%show-none our app]
            gone
          %^  game  [%peer [%show (scot %p p.kon) q.kon]]  u.huv.sat
          !>([ost p.kon q.kon])
        ::
            %take
          ?>  ?=(^ huv.sat)
          %^  game  [%peck /step]  u.huv.sat
          :(slop [[%atom %ud] ost] !>((ride use say)) !>(p.kon) q.kon)
        ==
      --
    --
  --
--
