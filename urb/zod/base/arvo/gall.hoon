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
++  bead  ,[p=(set beam) q=gage]                        ::  computed result
++  bone  ,@ud                                          ::  opaque duct
++  club                                                ::  agent action
  $%  [%peer p=path]                                    ::  subscribe
      [%poke p=cage]                                    ::  apply
      [%pull ~]                                         ::  unsubscribe
      [%pump ~]                                         ::  pump yes/no
  ==                                                    ::
++  cuft                                                ::  internal gift
  $%  [%coup p=(unit tang)]                             ::  poke result
      [%diff p=cage]                                    ::  subscription output
      [%quit ~]                                         ::  close subscription
      [%reap p=(unit tang)]                             ::  peer result
  ==                                                    ::
++  cuss  (pair term club)                              ::  internal kiss
++  suss  (trel term ,@tas ,@da)                        ::  config report
++  gift                                                ::  out result <-$
          $%  [%back p=?]                               ::  %mess ack good/bad
              [%crud p=@tas q=(list tank)]              ::  physical error
              [%dumb ~]                                 ::  close duct
              [%gone p=hapt]                            ::  app death
              [%mass p=mass]                            ::  memory usage
              [%mean p=ares]                            ::  message failure
              [%meta p=vase]                            ::  meta-gift
              [%nice ~]                                 ::  message success
          ==                                            ::
++  hapt  ,[p=ship q=path]                              ::  app instance
++  hath  ,[p=ship q=term]                              ::  app identity
++  kiss                                                ::  in request ->$
          $%  [%init p=ship]                            ::  initialize owner
              [%show p=hapt q=ship r=path]              ::  subscribe
              [%sire p=term q=hapt]                     ::  create subapp
          ::  [%cuff p=(unit cuff) q=kiss]              ::  controlled kiss
              [%mess p=hapt q=ship r=cage]              ::  typed message
              [%nuke p=hapt q=ship]                     ::  clear duct
              [%rote p=sack q=path r=*]                 ::  remote request
              [%roth p=sack q=path r=*]                 ::  remote response
              [%took p=hapt q=ship]                     ::  remote acknowledge
              [%wegh ~]                                 ::  report memory
              [%wipe p=hapt]                            ::  forget app
          ==                                            ::
++  knob                                                ::  pending action
          $%  [%boot ~]                                 ::  begin boot
              [%cede ~]                                 ::  selficide
              [%cide p=span]                            ::  subprocessicide
              [%crud p=@tas q=(list tank)]              ::  error
              [%feel ~]                                 ::  touch
              [%load p=cage]                            ::  continue boot
              [%mess p=ship q=cage]                     ::  typed message
              [%show p=ship q=path]                     ::  subscribe
              [%sire p=term q=span]                     ::  spawn subprocess
              [%nuke p=ship]                            ::  clear duct
              [%take p=path q=vase]                     ::  user result
              [%took p=ship]                            ::  rush queue drained
              [%told p=ship]                            ::  rush queue filled
          ==                                            ::
++  mast                                                ::  apps by ship
          $:  hun=duct                                  ::  control duct
              sap=(map ship scad)                       ::  foreign contacts
              bum=(map path seat)                       ::  instances by path
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  typed move
++  note                                                ::  out request $->
          $?  $:  %a                                    ::  to %ames
          $%  [%wont p=sock q=path r=*]                 ::
          ==  ==                                        ::
              $:  %b                                    ::  to %behn
          $%  [%deal p=sock q=cuss]                     ::  full transmission
          ==  ==                                        ::
              $:  %c                                    ::  to %clay
          $%  [%warp p=sock q=riff]                     ::
          ==  ==                                        ::
              $:  %f                                    ::  to %ford
          $%  [%exec p=@p q=beak r=(unit silk)]         ::
              [%wasp p=@p q=@uvH]                       ::
          ==  ==                                        ::
              $:  %g                                    ::  to %gall
          $%  [%show p=hapt q=ship r=path]              ::
              [%sire p=term q=hapt]                     ::
              [%mess p=hapt q=ship r=cage]              ::
              [%nuke p=hapt q=ship]                     ::
              [%took p=hapt q=ship]                     ::
          ==  ==                                        ::
              $:  @tas                                  ::  to any
          $%  [%meta p=vase]                            ::
          ==  ==  ==                                    ::
++  riff  ,[p=desk q=(unit rave)]                       ::  see %clay
++  scad                                                ::  opaque for foreign
          $:  p=@ud                                     ::  index
              q=(map duct ,@ud)                         ::  by duct
              r=(map ,@ud duct)                         ::  by index
          ==                                            ::
++  scar                                                ::  opaque duct system
          $:  p=@ud                                     ::  bone sequence
              q=(map duct ,[p=bone q=(unit cuff)])      ::  by duct
              r=(map bone duct)                         ::  by bone
          ==                                            ::
++  roon                                                ::  foreign response
          $%  [%d p=mark q=*]                           ::  diff (rush)
              [%e p=ares]                               ::  error
              [%f p=mark q=*]                           ::  full refresh (rust)
              [%k ~]                                    ::  message response
          ==                                            ::
++  rook                                                ::  foreign request
          $%  [%m p=mark q=*]                           ::  message
              [%s p=path]                               ::  subscribe
              [%u ~]                                    ::  cancel/unsubscribe
          ==                                            ::
++  seat                                                ::  the living app
          $:  app=term                                  ::  app name
              $:  huv=(unit vase)                       ::  application vase
                  qic=(unit toil)                       ::  current project
                  onz=(unit (pair duct path))           ::  live fords
                  vey=(qeu toil)                        ::  pending projects
              ==                                        ::
              nuc=(set duct)                            ::  nuked ducts
              $:  tik=@ud                               ::  build number
                  act=@ud                               ::  action number
                  lat=@da                               ::  last change
                  orm=(unit ,@da)                       ::  build date
              ==                                        ::
              mom=(unit duct)                           ::  parent duct
              cub=(map span term)                       ::  offspring
              $:  sup=(map bone (pair ship path))       ::  subscribers
                  pus=(jug path bone)                   ::  srebircsbus
                  peq=(map bone ,@uvI)                  ::  peekers
                  qel=(map bone ,@ud)                   ::  rush queue length
              ==                                        ::
              ped=@uvH                                  ::  active depends
              zam=scar                                  ::  opaque ducts
          ==                                            ::
++  silk                                                ::  see %ford
          $&  [p=silk q=silk]                           ::
          $%  [%boil p=mark q=beam r=path]              ::
              [%call p=silk q=silk]                     ::
              [%done p=(set beam) q=cage]               ::
              [%dude p=tank q=silk]                     ::
              [%mute p=silk q=(list (pair wing silk))]  ::
              [%ride p=twig q=silk]                     ::
              [%vale p=mark q=ship r=*]                 ::
          ==                                            ::
++  sill                                                ::  see %ford
          $%  [%dirt p=twig]                            ::
          ==                                            ::
++  sign                                                ::  in result $<-
          $?  [?(%c %d %e %t) @tas *]                   ::
              $:  %a                                    ::  by %ames
          $%  [%init p=@p]                              ::  only for :begin
              [%woot p=ship q=coop]                     ::
              [%went p=ship q=cape]                     ::  only for apps
          ==  ==                                        ::
              $:  %b                                    ::  by %behn
          $%  [%onto p=(each suss tang)]                ::
              [%unto p=cuft]                            ::
          ==  ==                                        ::
              $:  %g                                    ::  by %gall
          $%  [%init p=@p]                              ::
              [%crud p=@tas q=(list tank)]              ::
              [%dumb ~]                                 ::
              [%gone p=hapt]                            ::
              [%logo p=@]                               ::
              [%mean p=ares]                            ::
              [%nice ~]                                 ::
              [%rush p=mark q=*]                        ::
              [%rust p=mark q=*]                        ::
              [%sage p=path q=*]                        ::
              [%verb ~]                                 ::
              [%veer p=@ta q=path r=@t]                 ::
              [%vega p=path]                            ::
          ==  ==                                        ::
              $:  %f                                    ::  by %ford
          $%  [%made p=@uvH q=(each gage tang)]         ::
              [%news ~]                                 ::
          ==  ==  ==                                    ::
++  toil  (pair duct knob)                              ::  work in progress
--  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  functions
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
  ^-  [p=hapt q=path]
  ?.  ?=([@ @ *] pax)
    ~&  [%lump-path-bad pax]
    !!
  :-  :-  (slav %p i.pax)
      (need (pick i.t.pax))
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
      ^-  [p=(list move) q=_..^$]
      =>  .(q.hic ?.(?=(%soft -.q.hic) q.hic ((hard kiss) p.q.hic)))
      ?-    -.q.hic
          %init
        [p=~ q=..^$(pol.all (~(put by pol.all) p.q.hic hen ~ ~))]
      ::
          %rote
        (gawk hen p.q.hic q.q.hic ((hard ,[@ud rook]) r.q.hic))
      ::
          %roth
        (gawd hen p.q.hic q.q.hic ((hard ,[@ud roon]) r.q.hic))
      ::
          %wegh
        :_  ..^$  :_  ~
        :^  hen  %give  %mass
        :-  %|
        :~  all/`all
            ::  cor/`..^$
        ==
      ::
          %wipe
        =+  mat=(~(got by pol.all) p.p.q.hic)
        ~?  !(~(has by bum.mat) q.p.q.hic)  [%wipe-lost q.p.q.hic]
        =.  bum.mat  (~(del by bum.mat) q.p.q.hic)
        =.  pol.all  (~(put by pol.all) p.p.q.hic mat)
        [p=~ q=..^$]
      ::
          ?(%mess %show %nuke %took %sire)
        |-  ^-  [p=(list move) q=_..^^$]
        =+  =|  law=(unit cuff)
            |-  ^-  $:  law=(unit cuff)
                        hap=hapt
                        kon=knob
                    ==
            :-  law
            ?-  -.q.hic
              ::  %cuff  $(q.hic q.q.hic, law (limp p.q.hic law))
              %mess  [p %mess q r]:q.hic
              %show  [p %show q r]:q.hic
              %nuke  [p %nuke q]:q.hic
              %took  [p %took q]:q.hic
              %sire  [[p.q +.q.q] %sire p -.q.q]:q.hic
            ==
        ((goad hen law) p.hap q.hap kon)
      ==
    ::
    ++  take                                            ::  accept response
      |=  [pax=path hen=duct hin=(hypo sign)]           ::
      ^-  [p=(list move) q=_..^$]
      ?:  ?=(%crud +<.q.hin)
        ~&  [%gall-crud-error pax hen]
        ~&  [%gall-crud-data (,[@tas (list tank)] +>.q.hin)]
        ?>  ?=(%g -.q.hin)
        ?~  pax  ~&  %strange-path  [~ ..^$]
        =+  lum=(lump t.pax)
        =+  mat=(~(get by pol.all) p.p.lum)
        ?~  mat  ~&  %no-ship  [~ ..^$]
        =+  sat=(~(get by bum.u.mat) q.p.lum)
        ?~  sat  ~&  %no-app  [~ ..^$]
        :-  `(list move)`[hen %give %crud p.q.hin q.q.hin]~
        %=    ..^$                                      ::  XX maybe call work?
            pol.all
          %+  ~(put by pol.all)  p.p.lum
          %=    u.mat
              bum
            %+  ~(put by bum.u.mat)  q.p.lum
            u.sat(qic ~)
          ==
        ==
      ?:  ?=([%r *] pax)
        (gave hen t.pax q.hin)
      ?:  ?=([%x *] pax)
        (gasp hen t.pax q.hin)
      ?>  ?=([%a *] pax)
      =+  lum=(lump t.pax)
      =+  mat=(~(get by pol.all) p.p.lum)
      ?~  mat  [~ ..^$]
      =+  sat=(~(get by bum.u.mat) q.p.lum)
      ?~  sat  [~ ..^$]
      ::  ?.  (~(has by q.zam.u.sat) hen)
      ::  ~&  [%app-lost pax hen p.lum q.lum]
      ::  [~ ..^$]
      =<  abet  =<  work
      (more:(bear:(gaff p.lum) hen) q.lum hin)
    ::
    ++  scry
      |=  $:  use=(unit (set monk))
              ren=@tas
              who=ship
              syd=desk
              lot=coin
              tyl=path
          ==
      ^-  (unit (unit cage))
      =+  ^=  vew  ^-  lens                             ::  XX future scry
        %.  :-  use
            :-  [who syd ((hard case) p.lot)]
            (flop tyl)
        |=  $:  use=(unit (set monk))                   ::  observers
                bid=beam                                ::  position
            ==                                          ::
        (beef:(gaff p.bid q.bid ~) use r.bid s.bid)
      %+  bind
        ?+    ren  ~
          %u  u.vew
          %v  v.vew
          %w  w.vew
          %x  x.vew
          %y  y.vew
          %z  z.vew
        ==
      |=(a=(unit) (bind a |=(b=* [%noun !>(b)])))
    ::
    ++  doze
      |=  [now=@da hen=duct]
      ^-  (unit ,@da)
      ~&  %nighty-night
      [~ (add now ~s4)]
    ::
    ++  load
      |=  old=axle
      ^+  ..^$
      ..^$(all old)
    ::
    ++  stay  `axle`+>-.$
    --
|%                                                      ::  inner core
::
++  leak  |=(our=ship `beak`[our %home %da now])        ::  default beak
++  best                                                ::  cage to gift
  |=  [sem=?(%rush %rust) cay=cage]
  ^-  gift
  :-  %meta
  ^-  vase
  :-  :+  %cell  [%cube sem %atom %tas]
      [%cell [%cube p.cay %atom %tas] p.q.cay]
  [sem p.cay q.q.cay]
::
++  gaff                                                ::  take and go
  |=  [our=@p imp=path]
  =+  mat=(~(got by pol.all) our)
  =+  sat=(~(got by bum.mat) imp)
  ~(. go [our imp] mat sat)
::
++  gape                                                ::  %r send query
  |=  [hen=duct law=(unit cuff)]
  |=  [our=@p imp=path kon=knob]
  ^-  [(list move) _..^^$]
  ?>  ?=(?(%mess %show %nuke %took) -.kon)
  ?:  ?=(%took -.kon)
    ::  ~&  [%gape-took our imp hen]
    [~ ..^^$]
  =+  you=`ship`?-(-.kon %mess p.kon, %nuke p.kon, %show p.kon)
  =+  mut=(~(get by pol.all) you)
  ?~  mut
    ~&  [%gape-lost you hen]
    !!
  =+  mat=u.mut
  =+  sad==+(sad=(~(get by sap.mat) our) ?^(sad u.sad [.(p 1)]:*scad))
  =^  num  sad
      =+  nym=(~(get by q.sad) hen)
      ?^  nym  [u.nym sad]
      :-  p.sad
      :+  +(p.sad)
        (~(put by q.sad) hen p.sad)
      (~(put by r.sad) p.sad hen)
  :-  =+  ^=  roc  ^-  rook
          ?-  -.kon
            %mess  [%m p.q.kon q.q.q.kon]
            %nuke  [%u ~]
            %show  [%s q.kon]
          ==
      ^-  (list move)
      :~  :-  hen
          :+  %pass
            [%x -.roc (scot %p you) (scot %p our) (scot %ud num) imp]
          `note`[%a %wont [you our] [%q %ge imp] [num roc]]
      ==
  %=    ..^^$
      pol.all
    %+  ~(put by pol.all)
      you
    mat(sap (~(put by sap.mat) our sad))
  ==
::
++  gasp                                                ::  %x take
  |=  [hen=duct pax=path sih=sign]
  ^-  [(list move) _..^$]
  ?+    -.sih  !!
      %a
    ?.  ?=(%woot +<.sih)
      ~&  [%gall-bad-gasp-a pax=pax lgsih=+<.sih]
      ~&  [%gall-bad-gasp-b pax=pax sih=sih]  `..^$
    :_  ..^$  :_  ~
    ?~  q.sih
      [hen %give %nice ~]
    [hen %give %mean u.q.sih]
  ::
      %f
    ?.  ?=(%made +<.sih)
      ~&  [%gall-bad-gasp +<.sih]  `..^$
    :_  ..^$
    :_  ~
    :-  hen
    ?-    -.q.+.sih
        %|
      [%give %crud %gasp-crud p.q.+.sih]
    ::
        %&
      ?.  ?=(@ p.p.q.+.sih)  ~|  %bad-marc  !!
      =+  cay=`cage`p.q.+.sih
      ?+  -.pax  !!
        %d  [%give (best %rush cay)]
        %f  [%give (best %rust cay)]
  ==  ==
    ==
::
++  gave                                                ::  %r take
  |=  [hen=duct pax=path sih=sign]
  ^-  [(list move) _..^$]
  ?>  ?=([@ @ @ @ *] pax)
  =+  :*  our=`ship`(slav %p i.t.pax)
          you=`ship`(slav %p i.t.t.pax)
          num=(slav %ud i.t.t.t.pax)
          imp=`path`t.t.t.t.pax
      ==
  ::  ~&  [%gall-gave hen -.pax [our you num imp]]
  :_  ..^$
  =+  rod=|=(ron=roon `note`[%a %wont [our you] [%q %gh imp] num ron])
  ?+  -.pax  !!
    %z  ?+    -.sih  !!
            %a  :_  ~  :-  hen
                [%pass [%r pax] %g %took [our imp] you]
        ::
            %f
          ?<  ?=(%news -.+.sih)
          :_  ~  :-  hen
          ?-  -.q.+.sih
            %|  [%give %mean ~ %ford-fail p.q.+.sih]
            %&  ?.  ?=(@ p.p.q.+.sih)  ~|  %bad-marc  !!
                [%pass [%r pax] %g %mess [our imp] you `cage`p.q.+.sih]
          ==
        ::
            %g
          :_  ~  :-  hen
          ?-  -.+.sih
            %crud  !!
            %dumb  !!
            %gone  !!
            %init  !!
            %logo  !!
            %mean  [%pass [%r pax] (rod %e p.+.sih)]
            %nice  [%give %nice ~]
            %rush  [%pass [%r pax] (rod %d p.+.sih q.+.sih)]
            %rust  ::  ~&  [%gave-rust [our you num imp] hen]
                   [%pass [%r pax] (rod %f p.+.sih q.+.sih)]
            %sage  !!
            %verb  !!
            %veer  !!
            %vega  !!
          ==
        ==
    %u  !!
  ==
::
++  gawd                                                ::  %r handle response
  |=  [hen=duct saq=sack imp=path num=@ud ron=roon]
  ^-  [p=(list move) q=_..^$]
  ?:  =(0 num)  ~&  %shouldnt-get-zero  `..^$
  =+  mat=(~(got by pol.all) p.saq)
  =+  sad=(~(got by sap.mat) q.saq)
  =+  neh=(~(got by r.sad) num)
  :_  ..^$
  :-  [hen %give %nice ~]  :_  ~
  ^-  move  :-  neh
  ?-  -.ron
    %d  [%pass /x/d %f %exec p.saq (leak p.saq) ~ %vale p.ron q.saq q.ron]
    %e  [%give %mean p.ron]
    %f  [%pass /x/f %f %exec p.saq (leak p.saq) ~ %vale p.ron q.saq q.ron]
    %k  [%give %nice ~]
  ==
::
++  gawk                                                ::  %r call/request
  |=  [hen=duct saq=sack imp=path num=@ud rok=rook]
  ^-  [p=(list move) q=_..^$]
  :_  ..^$  :_   ~
  ^-  move  :-  hen
  :+  %pass
    :*  %r
        ::  ?-(-.rok %m %m, %s %s, %u %s)
        %z
        (scot %p p.saq)
        (scot %p q.saq)
        (scot %ud num)
        imp
    ==
  ^-  note
  ?-  -.rok
    %m  [%f %exec p.saq (leak p.saq) ~ %vale p.rok q.saq q.rok]
    %s  [%g %show [p.saq imp] q.saq p.rok]
    %u  [%g %nuke [p.saq imp] q.saq]
  ==
::
++  gent                                                ::  seat in mast
  |=  [our=@p imp=path mat=mast]
  =+  ^=  sat  ^-  seat
      =+  syt=(~(get by bum.mat) imp)
      ?^  syt  u.syt
      ?.  ?=([@ ~] imp)  ~&  [%app-not-found imp]  !!
      %*  .  *seat
          app  i.imp
          zam
        ^-  scar
        :+  1
          [[hun.mat 0 ~] ~ ~]
        [[0 hun.mat] ~ ~]
      ==
  =.  bum.mat  (~(put by bum.mat) imp sat)
  ~(. go [our imp] mat sat)
::
++  goad                                                ::  handle request
  |=  [hen=duct law=(unit cuff)]
  |=  [our=@p imp=path kon=knob]
  ^-  [(list move) _..^^$]
  =+  mut=(~(get by pol.all) our)
  ?^  mut
    ?:  &(?=([@ @ *] imp) !(~(has by bum.u.mut) imp))   ::  %took for dead imps
      [~ ..^^$]
    abet:work:(quem:(boar:(gent our imp u.mut) hen law) kon)
  ((gape hen law) our imp kon)
::
++  go                                                  ::  application core
  |_  $:  $:  our=@p                                    ::  application owner
              imp=path                                  ::  application name
          ==                                            ::
          mat=mast                                      ::  per owner
          sat=seat                                      ::  per application
      ==                                                ::
  ++  abet                                              ::  resolve
    %_    ..$
        all
      %_  all
        pol  %+  ~(put by pol.all)  our
             ?.  (~(has by bum.mat) imp)
                ::  ~&  [%abet-gone imp]
                mat
             ::  ~&  [%onzes imp=imp onz=onz.sat]
             mat(bum (~(put by bum.mat) imp sat))
      ==
    ==
  ::
  ++  able                                              ::  bone to duct
    |=  ost=bone  ^-  duct
    ::  ?:  =(0 ost)
    ::  hun.mat
    (~(got by r.zam.sat) ost)
  ::
  ++  away                                              ::  application path
    |=  pax=path  ^-  path
    [%a (scot %p our) ?~(imp %$ (pack imp)) pax]
  ::
  ++  bear                                              ::  write backward
    |=  hen=duct
    =+  orf=(fall (~(get by q.zam.sat) hen) [p=0 q=*(unit cuff)])
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
      =+  cor=(slam gat !>([our pax]))
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
          [((hard mark) -.q.vax) (slot 3 vax)]
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
        =+  orf=(fall (~(get by q.zam.sat) neh) [p=0 q=*(unit cuff)])
        %_    +>.$
            hen  neh
            ost  p.orf
            use  (read q.orf)
            say  (rite q.orf)
        ==
      ::
      ++  birf
        |=  [wir=wire hon=duct caq=vase]
        ^-  move
        ?>  ?=([%pass p=* q=%g r=[p=%sire q=term r=span]] q.caq)
        [hon %pass wir %g %sire q.r.q.caq our r.r.q.caq imp]
      ::
      ++  blow
        ^+  .
        ::  ~&  [%gall-blow ost]
        =>  (give %mean ~)
        =+  pax=+:(fall (~(get by sup.sat) ost) *[ship path])
        %=  +
          qic.sat  ~
          sup.sat  (~(del by sup.sat) ost)
          pus.sat  (~(del ju pus.sat) pax ost)
          peq.sat  (~(del by peq.sat) ost)
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
        :_  ~
        :-  [%$ 12]~
        (cave !>([[our app.sat imp] cub.sat sup.sat pus.sat [act.sat eny now]]))
      ::
      ++  core  |=(vax=vase (cove %core vax))           ::  core as silk
      ++  cove                                          ::  cage as silk
        |=  cay=cage
        ^-  silk
        [%done ~ cay]
      ::
      ++  deal                                          ::  reboot
        .(tik.sat +(tik.sat))
      ::
      ++  deff
        |=  [wir=wire hon=duct caq=vase]
        ^-  toil
        ?>  ?=([%pass p=* q=%g r=[p=%cide q=span]] q.caq)
        ::  ~&  [%deff imp cub.sat]
        [hon r.q.caq]
      ::
      ++  drum                                          ::  set dependencies
        |=  dep=@uvH
        ^+  +>
        ?~  dep  ~&(%drum-none +>.$) 
        ?:  =(dep ped.sat)  +>.$
        =+  pax=(away %w %drum (scot %uv dep) ~)
        %_  +>.$
          ped.sat  dep
          mow  :_(mow [hun.mat %pass pax %f %wasp our dep])  ::  XX cancel old
        ==
      ::
      ++  ford                                          ::  exec to ford
        |=  [pax=path kas=silk]
        ^+  +>
        =.  kas
          :+  %dude
            leaf/"error in app {<app.sat>} on {<our>} at instance {<imp>}"
          kas
        %_  +>
          mow      :_(mow [hen %pass (away pax) %f [%exec our (leak our) `kas]])
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
        [%ride [%cnzy arm] kas]
      ::
      ++  home                                          ::  load application
        ^-  silk
        =+  let=((hard ,@) q.q:(need (need (ska ~ %cw [our %home %da now] /))))
        [%boil %core [[our %home %ud ?:(=(let 0) 1 let)] app.sat %app ~] ~]
      ::
      ++  leav
        %_  .
          bum.mat  (~(del by bum.mat) imp)
          qic.sat  ~
          mow 
            ?~  mom.sat  mow
            :_(mow [u.mom.sat %give %gone our imp])
          vey.sat
            %-  ~(gas by vey.sat)
            %+  turn  (~(tap by cub.sat))
            |=([a=span @] [hen %cide a])
        ==
      ::
      ++  mack                                          ::  apply standard
        |=  sih=sign
        ?>  ?=([%f %made *] sih)
        ^-  [(unit (list tank)) _+>]
        ?-  -.q.+.sih
          &  :-  ~
             %-  obey:(morn (slot 3 q.p.q.+.sih))
             (slot 2 q.p.q.+.sih)
          |  [`p.q.+.sih (give %crud %mack-made p.q.+.sih)]
        ==
      ::
      ++  meek                                          ::  apply peek
        |=  sih=sign
        ^-  [(unit cage) _+>]
        ?>  ?=([%f %made *] sih)
        ?-  -.q.+.sih
          &  =+  vax=`vase`q.p.q.+.sih
             ?.  &(?=(^ q.vax) ?=(@ -.q.vax))
               [~ (give %crud %peek-lame *(list tank))]
             ::  ~>  %slog.[0 (skol p:(slot 3 vax))]
             :-  `[((hard mark) -.q.vax) (slot 3 vax)]
             +>.$
          |  [~ (give %crud %meek-made p.q.+.sih)]
        ==
      ::
      ++  mick                                          ::  apply w/depends
        |=  sih=sign
        ?>  ?=([%f %made *] sih)
        ^-  [? _+>]
        :-  -.q.+.sih
        ?-  -.q.+.sih
          &  %-  obey:(morn (slot 3 q.p.q.+.sih))
             (slot 2 q.p.q.+.sih)
          |  (give %crud %mick-made p.q.+.sih)
        ==
      ::
      ++  muck                                          ::  apply part
        |=  sih=sign
        ^-  [(unit (list tank)) _+>]
        ?>  ?=([%f %made *] sih)
        ?-  -.q.+.sih
          &  [~ (obey q.p.q.+.sih)]
          |  [`p.q.+.sih (give %crud %muck-made p.q.+.sih)]
        ==
      ::
      ++  murk                                          ::  apply park
        |=  sih=sign
        ^-  [[p=@uvH q=(unit cage)] _+>]
        ?>  ?=([%f %made *] sih)
        ?-  -.q.+.sih
          &  ?.  ?=(@ p.p.q.+.sih)  ~|  %bad-marc  !!
             [[p.+.sih `p.q.+.sih] +>.$]
          |  [[p.+.sih ~] (give %crud %murk-made p.q.+.sih)]
        ==
      ::
      ++  more                                          ::  domestic take
        |=  $:  pax=path                                ::  internal position
                hin=(hypo sign)                         ::  typed event
            ==
        ^+  +>
        ?+  -.pax  !!
            %s                                          ::  core operation
          ?.  ?&  ?=([@ *] t.pax)
                  !=(~ qic.sat)
                  =(`[hen pax] onz.sat)
              ==
            ~&  :*  %gall-mystery
                    imp  pax
                    ?~(qic.sat ~ [p -.q]:u.qic.sat)
                    onz.sat
                ==
            +>.$
          =:  onz.sat  ~
              qic.sat  ~
            ==
          ?+    i.t.pax  !!
          ::
              %park
            =^  gyd  +>.$  (murk q.hin)
            ?~  q.gyd  (drum p.gyd)
            =.  +>.$  (drum p.gyd)
            (quen %load u.q.gyd)
          ::
              %part
            =^  gud  +>.$  (muck q.hin)
            ?^  gud  +>.$
            leav
          ::
              %peek
            ?>  ?=([@ *] t.t.pax)
            =+  you=(need (slaw %p i.t.t.pax))
            =^  gyd  +>.$  (meek q.hin)
            ?~   gyd
              =.  +>.$  (give %mean ~ %peer-fail ~)
              (give [%dumb ~])
            =+  kee=[you t.t.t.pax]
            =+  ash=(sham q.q.u.gyd)
            ?:  =(`ash (~(get by peq.sat) ost))
              +>.$
            =.  +>.$
              ?:  (~(has by sup.sat) ost)  +>.$  (give %nice ~)
            %-  %=  give
                  peq.sat  (~(put by peq.sat) ost ash)
                  sup.sat  (~(put by sup.sat) ost kee)
                  pus.sat  (~(put ju pus.sat) +.kee ost)
                ==
            (best %rust u.gyd)
          ::
              %peer
            ::  ~&  [%peer-goning onz=onz.sat]
            ?>  ?=([@ *] t.t.pax)
            =+  you=(need (slaw %p i.t.t.pax))
            =^  gud  +>.$  (mack q.hin)
            ?^  gud
              =.  +>.$  (give %mean ~ %peer-fail ~)
              (give [%dumb ~])
            =.  +>.$  (give %nice ~)
            %=  +>.$
              sup.sat  (~(put by sup.sat) ost [you t.t.t.pax])
              pus.sat  (~(put ju pus.sat) t.t.t.pax ost)
            ==
          ::
              %poke
            =^  gud  +>.$  (mack q.hin)
            ?^  gud  (give %mean ~ %poke-mack-fail u.gud)
            +>.$
          ::
              %pour
            =^  gud  +>.$  (mack q.hin)
            ?^  gud  ~&  -.gud  +>.$
            +>.$
          ::
              %prep
            =^  gad  +>.$  (mick q.hin)
            ?.  gad  +>.$
            deal
          ::
              %pull
            =^  gud  +>.$  (mack q.hin)
            ?^  gud  +>.$
            blow
          ==
        ::
            %u                                          ::  user request
          %_    +>.$
              vey.sat
            (~(put to vey.sat) [hen [%take t.pax hin]])
          ==
        ::
            %w                                          ::  autoboot
          ?>  ?=([%drum @ ~] t.pax)
          ?>  ?=([%f %news ~] q.hin)
          :: ?>  ?=([%drug @ @ ~] t.pax)
          :: =+  :-  sin=((hard ,[%c %writ p=(unit)]) q.hin)
          ::     [our syd]=(raid t.t.pax %p %tas ~)
          :: ::~&  [%sync-notified `@p`our `@ta`syd]
          :: =.  ped.sat  (~(del by ped.sat) [our syd])
          :: ?~  p.+.sin
          ::   +>.$
          +>.$(vey.sat (~(put to vey.sat) hen %boot ~))
        ==
      ::
      ++  morn                                          ::  install core
        |=  vax=vase
        ^+  +>
        =+  new=?~(huv.sat & !=(+<+.q.vax +<+.q.u.huv.sat))
        ::  ?.  ?=(%core -.p.vax)
        ::    ~|  [%morn-not-core -.p.vax app.sat imp]
        ::    ~>  %mean.|.((skol p.vax))
        ::    !!
        =.  huv.sat  `vax
        ?.  new  +>.$
        =:  act.sat  +(act.sat)
            lat.sat  now
          ==
        =+  pex=(~(tap by peq.sat) ~)
        |-  ^+  +>.^$
        ?~  pex  +>.^$
        %=  $
          pex    t.pex
          +>.^$  (quem(hen (able p.i.pex)) [%show (~(got by sup.sat) p.i.pex)])
        ==
      ::
      ++  mort                                          ::  failed boot
        |=  tan=(list tank)
        (give %crud %boot-lost tan)
      ::
      ++  nile  [%done ~ [%$ [%cube 0 [%atom %n]] ~]]   ::  null silk
      ++  obey                                          ::  process app moves
        |=  vax=vase
        =^  sax  mow  (said vax)
        +>.$(vey.sat (~(gas to vey.sat) sax))
      ::
      ++  quem                                          ::  queue action
        |=  kon=knob                                    ::  content
        ^+  +>
        =.  +>  ?.  ?=(%nuke -.kon)  +>
            ?.  &(?=(^ onz.sat) =(hen p.u.onz.sat))  +>
            ~&  [%nukeando imp=imp onz=onz.sat]
            %=    +>
                onz.sat  ~
                mow
              :_(mow [hen %pass (away q.u.onz.sat) %f [%exec our (leak our) ~]])
            ==
        +>.$(vey.sat (~(put to vey.sat) hen kon))
      ::
      ++  quen                                          ::  push on front
        |=  kon=knob
        ^+  +>
        =+  yov=(~(tap by vey.sat) ~)                   ::  XX ++pun
        +>.$(vey.sat (~(gas to *(qeu toil)) `_yov`[[hen kon] yov]))
      ::
      ++  said                                          ::  sayz, done wrong
        |=  vud=vase
        =-  [p.fob (weld (flop q.fob) mow)]
        ^=  fob
        |-  ^-  (pair (list toil) (list move))
        ?:  =(~ q.vud)  [~ ~]
        =+  sud=(sump (slot 2 vud))
        =+  res=$(vud (slot 3 vud))
        :-  ?~  -.sud 
              -.res 
            [u.-.sud -.res]
        ?~  +.sud 
          +.res 
        [u.+.sud +.res]
      ::
      ++  sayz                                          ::  dissect app moves
        |=  vud=vase
        =|  toy=(list toil)
        |-  ^-  [(list toil) (list move)]
        ?:  =(~ q.vud)  [toy mow]
        =+  sud=(sump (slot 2 vud))
        %=    $
          vud  (slot 3 vud)
          toy  ?~(-.sud toy [u.-.sud toy])
          mow  ?~(+.sud mow [u.+.sud mow])
        ==
      ::
      ++  show                                          ::  subscribe
        |=  [you=ship pax=path]                         ::  subscription
        %_(+> vey.sat (~(put to vey.sat) hen %show you pax))
      ::
      ++  sumo                                          ::  standard gift
        |=  vig=vase
        ^-  gift
        [%meta vig]
      ::
      ++  sump
        |=  wec=vase
        ^-  [(unit toil) (unit move)]
        =+  ost=((hard bone) -.q.wec)
        =+  hon=(able ost)
        =+  caq=(spec (slot 3 wec))
        ?+    q.caq   ~&(%sump-bad !!)
        ::
            [%pass p=* q=@tas r=[p=@tas q=*]]
          =+  wir=(away %u ((hard path) p.q.caq))
          ?:  ?=(%cide p.r.q.caq)  [`(deff wir hon caq) ~]
          ?:  ?=(%sire p.r.q.caq)  [~ `(birf wir hon caq)]
          :^  ~  ~  hon
          :^  %pass  wir
            (need ((sand %tas) ((hard ,@) q.q.caq)))
          [%meta (spec (slot 15 caq))]
        ::
            [%give p=[p=@tas q=*]]
          ?:  ?=(%mean p.p.q.caq)
            :-  `[hon %nuke our]
            `[hon %give %mean (ares q.p.q.caq)]
          :-  ?.  ?|  ?=(?(%rush %rust) p.p.q.caq)
                      ?&  ?=(%meta p.p.q.caq)
                          ?=([* ?(%rush %rust) *] q.p.q.caq)
                  ==  ==
                ~
              `[hon %told our]
          :+  ~  hon
          :-  %give
          ?:  ?=(%nice p.p.q.caq)  [%nice ~]
          (sumo (spec (slot 3 caq)))
        ==
      ::
      ++  warm                                          ::  vase has arm
        |=  cog=@tas
        ^-  ?
        ?~(huv.sat | (slab cog p.u.huv.sat))
      ::
      ++  work                                          ::  eat queue
        |-  ^+  +
        ::  ~&  >  :*  %workeando
        ::             our=our
        ::             vey==(~ vey.sat)
        ::             ^=  qic
        ::             ?:  ?=(^ qic.sat)
        ::               [p -.q]:u.qic.sat
        ::             ~
        ::         ==
        ?:  |(?=(^ qic.sat) =(~ vey.sat))  +.$          ::  nothing to do
        =^  yev  vey.sat  [p q]:~(get to vey.sat)
        ?:  (~(has in nuc.sat) p.yev)  $
        work:(yawn:(bing p.yev) q.yev)
      ::
      ++  xeno
        |=  [pim=path kon=knob]
        =^  mew  ..$.go
          ((goad($.go +:abet) hen ~) our pim kon)
        =.  mat  (~(got by pol.all) our)
        =.  sat  (fall (~(get by bum.mat) imp) sat)
        +>.$(mow (weld (flop mew) mow))
      ::
      ++  yawl                                          ::  invoke core
        |=  [[arm=term pax=path] tac=tank vax=vase sam=vase]
        ^+  +>
        %+  ford  [%s arm pax]
        :+  %dude  tac
        [%call (harm arm (conf (core vax))) (cove %$ sam)]
      ::
      ++  yawn                                          ::  start event
        |=  kon=knob
        ^+  +>
        ::  ~&  [%gall-yawn ost imp -.kon]
        =.  qic.sat  `[hen kon]
        ?-    -.kon
            %boot
          =.  orm.sat  `now
          %+  ford  /s/park
          ::  :+  %dude  leaf/"booting"
          ^-  silk
          :-  home
          ?~  huv.sat  nile
          ?:  =(~ q.u.huv.sat)  nile
          :-  nile
          ?.  (warm %park)
            [%done ~ %$ (slot 13 u.huv.sat)]
          (harm %park (conf (core u.huv.sat)))
        ::
            %cede
          ?:  (warm %part)
            =+  sam=!>(ost)
            ?>  ?=(^ huv.sat)
            (yawl /part leaf/"parting" u.huv.sat sam)
          leav
        ::
            %cide
          ?~  p.kon
            ?~  imp    +>.$(qic.sat ~)
            ?~  t.imp
              $(kon [%cede ~])
            =.  qic.sat  ~
            (xeno t.imp %cide i.imp)
          ?.  (~(has by bum.mat) [p.kon imp])
            ~&  >  [%cide-missed p.kon imp]  +>.$(qic.sat ~)
          ::~&  [%cide-found p.kon imp]
          =.  +>.$  (xeno [p.kon imp] %cede ~)
          %_  +>.$
            cub.sat  (~(del by cub.sat) p.kon)
            qic.sat  ~
          ==
        ::
            %feel
          +>.$(qic.sat ~)
        ::
            %took
          =+  qol=(~(get by qel.sat) ost)
          ::  ~&  [%yawn-took-has ost qol [our hen]]
          %=    +>.$
              qic.sat  ~
              qel.sat  
            ?~  qol
              ::  ~&  [%took-underflow our hen]
              qel.sat
            ?:  =(`1 qol)
              (~(del by qel.sat) ost)
            (~(put by qel.sat) ost (dec u.qol))
          ==
        ::
            %told
          =+  qol=(~(get by qel.sat) ost)
          ::  ~&  [%yawn-told-has ost qol [our hen]]
          =+  qul=?~(qol 1 +(u.qol))
          =.  qel.sat  (~(put by qel.sat) ost qul)
          ?:  =(10 qul)
            ~&  [%yawn-told-full ost our hen]
            +>.$(qic.sat ~, vey.sat (~(put to vey.sat) hen %nuke p.kon))
          +>.$(qic.sat ~)
        ::
            %load
          =+  [hom=(slot 2 q.p.kon) old=(slot 3 q.p.kon)]
          %+  ford  /s/prep
          :+  %dude  leaf/"prepping"
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
            blow
          ?>  ?=(^ huv.sat)
          (yawl [%pull ~] leaf/"pulling" u.huv.sat [[%atom %ud] ost])
        ::
            %mess
          =+  ^-  cog=term
              =-  |-  ?~  goz  ?:((warm %pock) %pock %poke)
                      ?:  (warm i.goz)  i.goz
                      $(goz t.goz)
              ^-  goz=(list term)
              ?:  =(%$ p.q.kon)
                /pock
              =+  ^=  goc
                  |=  [a=term b=(list term)]
                  [(cat 3 'pock-' a) (cat 3 'poke-' a) b]
              =+  ofs=(met 3 app.sat)
              ?.  .=  (cat 3 app.sat '-')   ::  XX temporary, until /=home=/bin
                  (end 3 +(ofs) p.q.kon)
                (goc p.q.kon /pock)
              :(goc p.q.kon (rsh 3 ofs p.q.kon) /pock)
          =+  hyp=?=(%pock (end 3 4 cog))
          =+  ^-  err=tape
              ?.(?=(?(%poke %pock) cog) <cog> "{<cog>} with mark {<p.q.kon>}")
          ?.  (warm cog)
            (give(qic.sat ~) %mean ~ %poke-find-fail leaf/err ~)
          ?>  ?=(^ huv.sat)
          =+  ^=  sam
              ;:  slop
                [[%atom %ud] ost] 
                [[%atom %p] p.kon] 
                ?.(hyp q.q.kon (slop !>(p.q.q.kon) q.q.kon))
              ==
          ::  ~&  [%mess-poke cog]
          %+  ford  /s/poke
          :+  %dude  leaf/err
          [%call (harm cog (conf (core u.huv.sat))) (cove %$ sam)]
        ::
            %show
          ::  ~&  showing/[app.sat imp q.kon]
          ?:  (warm %peer)
            =+  sam=!>([ost p.kon q.kon])
            ?>  ?=(^ huv.sat)
            =.  peq.sat  (~(del by peq.sat) ost)
            (yawl [%peer (scot %p p.kon) q.kon] leaf/"peering" u.huv.sat sam)
          ?:  (warm %peek)
            =+  sam=!>([p.kon q.kon])
            ?>  ?=(^ huv.sat)
            (yawl [%peek (scot %p p.kon) q.kon] leaf/"peeking" u.huv.sat sam)
          ~&  [%show-dumb app.sat imp q.kon]
          (give(qic.sat ~) %dumb ~)
        ::
            %sire
          ?:  (~(has by bum.mat) [q.kon imp])
            ~&  >  %sire-redundant  +>.$(qic.sat ~)
          ::~&  [%sire-made p.kon imp]
          =:    cub.sat  (~(put by cub.sat) q.kon p.kon)
                qic.sat  ~
                bum.mat
              %+  ~(put by bum.mat)  [q.kon imp]
              %*  .  *seat
                  app  p.kon
                  mom  `hen
                  zam
                ^-  scar
                :+  1
                  [[hun.mat 0 ~] ~ ~]
                [[0 hun.mat] ~ ~]
              ==
            ==
          (xeno [q.kon imp] %feel ~)
        ::
            %take
          ?:  ?&  ?=([%g %rush @ *] q.q.kon)
                  |((warm %posh) (warm (cat 3 'posh-' &3.q.q.kon)))
              ==
            ?>  ?=(^ huv.sat)
            =+  [goc gil]=[(cat 3 'posh-' &3.q.q.kon) (spec (slot 7 q.kon))]
            =.  -  ?:((warm goc) [goc (slot 3 gil)] [%posh gil])
            =+  sam=:(slop [[%atom %ud] ost] !>(p.kon) gil)
            %+  ford  /s/pour
            :+  %dude  leaf/"pouring"
            :+  %dude  (skol p.gil)
            [%call (harm goc (conf (core u.huv.sat))) (cove %$ sam)]
          ?:  (warm %purr)
            ?>  ?=(^ huv.sat)
            =+  sam=:(slop [[%atom %ud] ost] !>(p.kon) !>(p.q.kon) q.kon)
            %+  ford  /s/pour
            :+  %dude  leaf/"purring"
            [%call (harm %purr (conf (core u.huv.sat))) (cove %$ sam)]
          ?.  (warm %pour)
            +>.$(qic.sat ~)
          ?>  ?=(^ huv.sat)
          =+  sam=:(slop [[%atom %ud] ost] !>(p.kon) q.kon)
          %+  ford  /s/pour
          :+  %dude  leaf/"pouring"
          [%call (harm %pour (conf (core u.huv.sat))) (cove %$ sam)]
        ==
  --  --
--  --
