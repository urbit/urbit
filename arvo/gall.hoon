::  ::  %gall, user-level applications
!?  164
::::
|=  pit=vase
=>  =~                
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    structures
++  axle                                                ::  all %gall state
          $:  %0                                        ::  state version
              pol=(map ship mast)                       ::  apps by ship
          ==                                            ::
++  bead  ,[p=(set beam) q=cage]                        ::  computed result
++  bone  ,@ud                                          ::  opaque duct
++  gift                                                ::  out result <-$
          $%  [%back p=?]                               ::  %mess ack good/bad
              [%crud p=@tas q=(list tank)]              ::  physical error
              [%dumb ~]                                 ::  close duct
              [%rasp ~]                                 ::  message failure
              [%meta p=vase]                            ::  meta-gift
          ==                                            ::
++  hasp  ,[p=ship q=term]                              ::  app identity
++  kiss                                                ::  in request ->$
          $%  [%init p=ship]                            ::  initialize owner
              [%show p=hasp q=ship r=path]              ::  subscribe
          ::  [%cuff p=(unit cuff) q=kiss]              ::  controlled kiss
              [%mess p=hasp q=ship r=cage]              ::  message
              [%nuke p=hasp]                            ::  clear duct
          ==                                            ::
++  knob                                                ::  pending action
          $%  [%boot ~]                                 ::  begin boot
              [%crud p=@tas q=(list tank)]              ::  error
              [%load p=cage]                            ::  continue boot
              [%mess p=ship q=cage]                     ::  message
              [%show p=ship q=path]                     ::  subscribe
              [%nuke ~]                                 ::  clear duct
              [%take p=path q=vase]                     ::  user result
          ==                                            ::
++  mast                                                ::  apps by ship
          $:  hun=duct                                  ::  control duct
              bum=(map ,@ta seat)                       ::  apps by name
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  typed move
++  note                                                ::  out request $->
          $?  $:  %c                                    ::  to %clay
          $%  [%warp p=sock q=riff]                     ::
          ==  ==                                        ::
              $:  %f                                    ::  to %ford
          $%  [%exec p=@p q=(unit silk)]                ::
          ==  ==                                        ::
              $:  @tas                                  ::  to any 
          $%  [%meta p=vase]                            ::
          ==  ==  ==                                    ::
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
              onz=(unit (pair duct path))               ::  live fords
              vey=(qeu toil)                            ::  pending projects
              nuc=(set duct)                            ::  nuked ducts
              tik=@ud                                   ::  build number
              act=@ud                                   ::  action number
              lat=@da                                   ::  last change
              orm=(unit ,@da)                           ::  build date
              sup=(map bone (pair ship path))           ::  subscribers
              peq=(map bone ,@uvI)                      ::  peekers
              ped=(set (pair ship desk))                ::  active depends
              zam=scar                                  ::  opaque ducts
          ==                                            ::
++  sign                                                ::  in result $-<
          $?  [?(%a %b %c %d %e %g) @tas *]
              [%f %made p=(each bead (list tank))]
          ==
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
      ?:  ?=(%init -.q.hic)
        [p=~ q=..^$(pol.all (~(put by pol.all) p.q.hic hen ~))]
      |-  ^-  [p=(list move) q=_..^^$]
      =+  =|  law=(unit cuff)
          |-  ^-  $:  law=(unit cuff)
                      hap=hasp
                      kon=knob
                  ==
          ?-  -.q.hic
            ::  %cuff  $(q.hic q.q.hic, law (limp p.q.hic law))
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
      ^-  (unit (unit (pair logo ,*)))
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
  =+  mat=(need (~(get by pol.all) our))
  =+  ^=  sat  ^-  seat
      =+  syt=(~(get by bum.mat) app)
      ?^  syt  u.syt
      %*  .  *seat
          zam
        ^-  scar
        :+  1
          [[hun.mat 0 ~] ~ ~]
        [[0 hun.mat] ~ ~]
      ==
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
          [((hard logo) -.q.vax) (slot 3 vax)]
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
      ++  apex                                          ::  enter
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
        |=  kas=silk
        ^-  silk
        :+  %mute  kas
        :~  [[%$ 12]~ (cave !>([[our app] sup.sat [act.sat eny now]]))]
        ==
      ++  core  |=(vax=vase (cove %core vax))           ::  core as silk
      ++  cove                                          ::  cage as silk
        |=  cay=cage
        ^-  silk
        [%done ~ cay]
      ::
      ++  deal                                          ::  reboot
        ^+  .
        =.  tik.sat  +(tik.sat)
        =+  pys=(~(tap by sup.sat) ~)
        ::  ~&  [%gall-deal tik.sat pys]
        |-  ^+  +>.$
        ?~  pys  +>.$
        =.  +>.$  $(pys t.pys)
        %=    +>.$
            vey.sat
          %-  ~(put to vey.sat)
          :-  (need (~(get by r.zam.sat) p.i.pys))
          [%show q.i.pys]
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
            :-  hun.mat
            :^  %pass  (away %w %drug (scot %p p.a) q.a ~)  %c
            [%warp [our p.a] q.a ~ %| [%da now] [%da (add now ~d1000)]]
        =+  ^=  old  ^-  (list move)
            %+  turn
              %+  skip  (~(tap in ped.sat) ~)
              |=(a=(pair ship desk) (~(has in pen) a))
            |=  a=(pair ship desk)
            :-  hun.mat
            :^  %pass  (away %w %drug (scot %p p.a) q.a ~)  %c
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
        ^+  +>
        %_    +>
            mow      :_(mow [hen %pass (away pax) %f [%exec our `kas]])
            onz.sat  `[hen pax]
        ==
      ::
      ++  give                                          ::  give a gift
        |=  gip=gift
        %_(+> mow [[hen %give gip] mow])
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
      ++  mack                                          ::  apply standard
        |=  sih=sign
        ?>  ?=(%f -.sih) 
        ^-  [(unit (list tank)) _+>]
        ?-  -.p.+.sih
          &  :-  ~
             %-  obey:(morn (slot 3 q.q.p.p.+.sih))
             (slot 2 q.q.p.p.+.sih)
          |  [`p.p.+.sih (give %crud %made p.p.+.sih)]
        ==
      ::
      ++  meek                                          ::  apply peek
        |=  sih=sign
        ^-  [(unit cage) _+>]
        ?>  ?=(%f -.sih) 
        ?-  -.p.+.sih
          &  =+  vax=`vase`q.q.p.p.+.sih
             ?.  &(?=(^ q.vax) ?=(@ -.q.vax))
               [~ (give %crud %peek-lame *(list tank))]
             ::  ~>  %slog.[0 (skol p:(slot 3 vax))]
             :-  `[((hard logo) -.q.vax) (slot 3 vax)]
             +>.$
          |  [~ (give %crud %made p.p.+.sih)]
        ==
      ::
      ++  mick                                          ::  apply w/depends
        |=  sih=sign
        ?>  ?=(%f -.sih)
        ^-  [(unit (set beam)) _+>]
        ?-  -.p.+.sih
          &  :-  `p.p.p.+.sih
             %-  obey:(morn (slot 3 q.q.p.p.+.sih))
             (slot 2 q.q.p.p.+.sih)
          |  [~ (give %crud %made p.p.+.sih)]
        ==
      ::
      ++  murk                                          ::  apply park
        |=  sih=sign
        ^-  [(unit cage) _+>]
        ?>  ?=(%f -.sih) 
        ?-  -.p.+.sih
          &  [`q.p.p.+.sih +>.$]
          |  [~ (give %crud %made p.p.+.sih)]
        ==
      ::
      ++  more                                          ::  accept result
        |=  $:  pax=path                                ::  internal position
                hin=(hypo sign)                         ::  typed event
            ==
        ^+  +>
        ?+  -.pax  !!
            %s                                          ::  core operation
          ?>  ?&  ?=([@ *] t.pax)
                  !=(~ qic.sat)
                  =(`[hen pax] onz.sat)
              == 
          =:  onz.sat  ~
              qic.sat  ~
            ==
          ?+    i.t.pax  !!
              %park
            =^  gyd  +>.$  (murk q.hin)
            ?~  gyd
              +>.$
            (quen %load u.gyd)
          ::
              %peek 
            ?>  ?=([@ *] t.t.pax)
            =+  you=(need (slaw %p i.t.t.pax))
            =^  gyd  +>.$  (meek q.hin)
            ?~   gyd
              (give [%dumb ~]) 
            =+  kee=[you t.t.t.pax]
            =+  ash=(sham q.q.u.gyd)
            ?:  =(`ash (~(get by peq.sat) ost))
              +>.$
            %-  %=  give
                  peq.sat  (~(put by peq.sat) ost ash)
                  sup.sat  (~(put by sup.sat) ost kee)
                ==
            :-  %meta
            ^-  vase
            :-  :+  %cell  [%cube %rust %atom %tas] 
                [%cell [%atom %tas] p.q.u.gyd]
            [%rust p.u.gyd q.q.u.gyd]
          ::
              %peer 
            ?>  ?=([@ *] t.t.pax)
            =+  you=(need (slaw %p i.t.t.pax))
            =^  gud  +>.$  (mack q.hin)
            ?^  gud  
              (give [%dumb ~])
            +>.$(sup.sat (~(put by sup.sat) ost [you t.t.t.pax]))
          ::
              %poke
            =^  gud  +>.$  (mack q.hin)
            ?^  gud  (give %rasp ~)
            +>.$
          ::
              %pour
            =^  gud  +>.$  (mack q.hin)
            ?^  gud  ~&  -.gud  +>.$
            +>.$
          ::
              %prep
            =^  gad  +>.$  (mick q.hin)
            ?~  gad  (drum ~)
            deal:(drum u.gad)
          ::
              %pull
            =^  gud  +>.$  (mack q.hin)
            ?^  gud  +>.$
            +>.$(sup.sat (~(del by sup.sat) ost))
          ==
        :: 
            %u                                          ::  user request
          %_    +>.$
              vey.sat 
            (~(put to vey.sat) [hen [%take t.pax hin]])
          ==
        ::
            %w                                          ::  autoboot
          ?>  ?=([%drug @ @ ~] t.pax) 
          =+  :*  sin=((hard ,[%c %writ p=riot]) q.hin)
                  our=(need (slaw %p i.t.t.pax))
                  syd=(need ((sand %tas) i.t.t.t.pax)) 
              ==
          =.  ped.sat  (~(del by ped.sat) [our syd])
          ?~  p.+.sin  
            +>.$
          +>.$(vey.sat (~(put to vey.sat) hen %boot ~))
        ==
      ::
      ++  morn                                          ::  install core
        |=  vax=vase
        ^+  +>
        =+  new=?~(huv.sat & !=(+<+.q.vax +<+.q.u.huv.sat))
        =.  huv.sat  `vax
        ?.  new  +>.$
        =:  act.sat  +(act.sat)
            lat.sat  now
          ==
        =+  pex=(~(tap by peq.sat) ~)
        |-  ^+  +>.^$
        ?~  pex  +>.^$
        ::  ~&  [%morn-peek p.i.pex (need (~(get by sup.sat) p.i.pex))]
        %=    $
            pex    t.pex
            +>.^$  %-  quem(hen (need (~(get by r.zam.sat) p.i.pex)))
                   [%show (need (~(get by sup.sat) p.i.pex))]
        ==
      ::
      ++  mort                                          ::  failed boot 
        |=  tan=(list tank)
        (give %crud %boot-lost tan)
      ::
      ++  nile  [%done ~ [%$ [%cube 0 [%atom %n]] ~]]   ::  null silk
      ++  obey                                          ::  process app moves
        |=  vax=vase
        %_(+> mow (weld (flop (said vax)) mow))
      ::
      ++  quem                                          ::  queue action
        |=  kon=knob                                    ::  content
        ^+  +>
        =.  +>  ?.  ?=(%nuke -.kon)  +> 
            ?.  &(?=(^ onz.sat) =(hen p.u.onz.sat))  +>
            %=    +>
                onz.sat  ~
                mow  
              :_(mow [hen %pass (away q.u.onz.sat) %f [%exec our ~]])
            ==
        +>.$(vey.sat (~(put to vey.sat) hen kon))
      ::
      ++  quen                                          ::  push on front
        |=  kon=knob
        ^+  +>
        =+  yov=(~(tap by vey.sat) ~)                   ::  XX ++pun
        +>.$(vey.sat (~(gas to *(qeu toil)) `_yov`[[hen kon] yov]))
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
        [%meta vig]
      ::
      ++  sump
        |=  wec=vase
        ^-  move
        :-  (need (~(get by r.zam.sat) ((hard bone) -.q.wec)))
        =+  caq=(spec (slot 3 wec))
        ?+    q.caq   ~&(%sump-bad !!)
        ::
            [%pass p=* q=@tas r=[p=@tas q=*]]
          :^  %pass  (away %u ((hard path) p.q.caq))
            (need ((sand %tas) ((hard ,@) q.q.caq)))
          [%meta (spec (slot 15 caq))]
        ::
            [%give p=[p=@tas q=*]]
          [%give (sumo (spec (slot 3 caq)))]
        ==
      ::
      ++  warm                                          ::  vase has arm
        |=  cog=@tas 
        ^-  ?
        ?~  huv.sat  |
        !=(~ q:(~(fino ut p.u.huv.sat) 0 %free cog))
      ::
      ++  work                                          ::  eat queue
        |-  ^+  +
        ?:  |(?=(^ qic.sat) =(~ vey.sat))  +.$          ::  nothing to do
        =^  yev  vey.sat  [p q]:~(get to vey.sat)
        ?:  (~(has in nuc.sat) p.yev)  $
        work:(yawn:(bing p.yev) q.yev)
      ::
      ++  yawl                                          ::  invoke core
        |=  [[arm=term pax=path] vax=vase sam=vase]
        ^+  +>
        %+  ford  [%s arm pax]
        [%call (harm arm (conf (core vax))) (cove %$ sam)]
      ::
      ++  yawn                                          ::  start event
        |=  kon=knob
        ^+  +>
        ::  ~&  [%gall-yawn ost -.kon]
        =.  qic.sat  `[hen kon]
        ?-    -.kon
            %boot
          =.  orm.sat  `now
          %+  ford  /s/park
          ^-  silk
          :-  home
          ?~  huv.sat  nile
          ?:  =(~ q.u.huv.sat)  nile
          :-  nile
          ?.  (warm %park)
            [%done ~ %$ (slot 13 u.huv.sat)]
          (harm %park (conf (core u.huv.sat)))
        ::
            %load
          =+  [hom=(slot 2 q.p.kon) old=(slot 3 q.p.kon)]
          %+  ford  /s/prep
          ?.  (warm(huv.sat `hom) %prep)
            :-  nile
            ?:  =(~ q.old)
              (core hom)
            :+  %mute  `silk`(core hom)
            :~  [[%$ 13]~ (cave (slot 3 old))]
            ==
          [%call (harm %prep (conf (core hom))) [nile (cave old)]]
        ::
            %crud
          (give(qic.sat ~) %crud p.kon q.kon)
        ::
            %nuke
          ?.  (warm %pull)
            +>.$(qic.sat ~)
          ?>  ?=(^ huv.sat)
          (yawl [%pull ~] u.huv.sat [[%atom %ud] ost])
        ::
            %mess
          =+  ^=  cog  ^-  term
              ?:  =(%$ p.q.kon)  %poke
              =+  goc=(cat 3 'poke-' p.q.kon)
              ?:((warm goc) goc %poke)
          ?.  (warm cog)
            (give(qic.sat ~) %rasp ~)
          ?>  ?=(^ huv.sat)
          =+  sam=:(slop [[%atom %ud] ost] [[%atom %p] p.kon] q.q.kon)
          ::  ~&  [%mess-poke cog]
          %+  ford  /s/poke
          [%call (harm cog (conf (core u.huv.sat))) (cove %$ sam)]
        ::
            %show
          ?:  (warm %peer)
            =+  sam=!>([ost p.kon q.kon])
            ?>  ?=(^ huv.sat)
            =.  peq.sat  (~(del by peq.sat) ost)
            (yawl [%peer (scot %p p.kon) q.kon] u.huv.sat sam)
          ?:  (warm %peek)  
            =+  sam=!>([p.kon q.kon])
            ?>  ?=(^ huv.sat)
            (yawl [%peek (scot %p p.kon) q.kon] u.huv.sat sam)
          (give(qic.sat ~) %dumb ~)
        ::
            %take
          ?.  (warm %pour)
            +>.$(qic.sat ~)
          ?>  ?=(^ huv.sat)
          =+  sam=(slop !>(p.kon) q.kon)
          %+  ford  /s/pour
          [%call (harm %pour (conf (core u.huv.sat))) (cove %$ sam)]
        ==
      --
    --
  --
--
