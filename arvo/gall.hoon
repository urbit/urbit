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
++  bead  ,[p=(set beam) q=cage]                        ::  computed result
++  bone  ,@ud                                          ::  opaque duct
++  gift                                                ::  out result <-$
          $%  [%back p=?]                               ::  %mess ack good/bad
              [%crud p=@tas q=(list tank)]              ::  physical error
              [%dumb ~]                                 ::  close duct
              [%gone p=hapt]                            ::  app death
              [%mean p=ares]                            ::  message failure
              [%meta p=vase]                            ::  meta-gift
              [%nice ~]                                 ::  message success
          ==                                            ::
::++  hasp  ,[p=ship q=term]                              ::  app identity
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
              $:  %c                                    ::  to %clay
          $%  [%warp p=sock q=riff]                     ::
          ==  ==                                        ::
              $:  %f                                    ::  to %ford
          $%  [%exec p=@p q=(unit silk)]                ::
          ==  ==                                        ::
              $:  %g                                    ::  to %gall
          $%  [%show p=hapt q=ship r=path]              ::
              [%sire p=term q=hapt]                     ::
              [%mess p=hapt q=ship r=cage]              ::
              [%nuke p=hapt q=ship]                     ::
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
          $%  [%d p=mark q=*]                           ::  diff
              [%e p=ares]                               ::  error
              [%f p=mark q=*]                           ::  full refresh
              [%k ~]                                    ::  message response
          ==                                            ::
++  rook                                                ::  foreign request
          $%  [%m p=mark q=*]                           ::  message
              [%s p=path]                               ::  subscribe
              [%u ~]                                    ::  cancel/unsubscribe
          ==                                            ::
++  seat                                                ::  the living app
          $:  app=term                                  ::  app name
              huv=(unit vase)                           ::  application vase
              qic=(unit toil)                           ::  current project
              onz=(unit (pair duct path))               ::  live fords
              vey=(qeu toil)                            ::  pending projects
              nuc=(set duct)                            ::  nuked ducts
              tik=@ud                                   ::  build number
              act=@ud                                   ::  action number
              lat=@da                                   ::  last change
              orm=(unit ,@da)                           ::  build date
              mom=(unit duct)                           ::  parent duct
              cub=(map span term)                       ::  offspring
              sup=(map bone (pair ship path))           ::  subscribers
              pus=(jug path bone)                       ::  srebircsbus
              peq=(map bone ,@uvI)                      ::  peekers
              ped=(set (pair ship desk))                ::  active depends
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
          $%  [%made p=(each bead (list tank))]         ::
          ==  ==  ==                                    ::
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
++  lamp
  |=  con=(unit coin)
  ^-  (unit path)
  ?.  ?=([~ %many *] con)  ~
  %-  zl:jo
  %+  turn  p.u.con
  |=  tem=coin
  ?.(?=([%$ %ta @] tem) ~ (some q.p.tem))
::
++  lump                                                ::  position
  |=  pax=path
  ^-  [p=hapt q=path]
  ?>  ?=([@ @ *] pax)
  :-  :-  (slav %p i.pax)
      (need (lamp (slay i.t.pax))) 
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
      ?-    -.q.hic
          %init
        [p=~ q=..^$(pol.all (~(put by pol.all) p.q.hic hen ~ ~))]
          %rote
        (gawk hen p.q.hic q.q.hic ((hard ,[@ud rook]) r.q.hic))
          %roth
        (gawd hen p.q.hic q.q.hic ((hard ,[@ud roon]) r.q.hic))
          %wipe
        =+  mat=(~(got by pol.all) p.p.q.hic)
        ~?  !(~(has by bum.mat) q.p.q.hic)  [%wipe-lost q.p.q.hic]
        =.  bum.mat  (~(del by bum.mat) q.p.q.hic)
        =.  pol.all  (~(put by pol.all) p.p.q.hic mat)
        [p=~ q=..^$]
          ?(%mess %show %nuke %sire)
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
              %sire  [[p.q +.q.q] %sire p -.q.q]:q.hic
            ==
        ((goad hen law) p.hap q.hap kon)
      ==
    ::
    ++  take                                            ::  accept response
      |=  [pax=path hen=duct hin=(hypo sign)]           ::
      ^-  [p=(list move) q=_..^$]
      ?:  ?=(%crud +<.q.hin)
        ~&  [%gall-crud-error pax hen q.hin]
        ?>  ?=(%g -.q.hin)
        ?~  pax  ~&  %strange-path  [~ ..^$]
        =+  lum=(lump t.pax)
        =+  mat=(~(get by pol.all) p.p.lum)
        ?~  mat  ~&  %no-ship  [~ ..^$]
        =+  sat=(~(get by bum.u.mat) q.p.lum)
        ?~  sat  ~&  %no-app  [~ ..^$]
        :-  `(list move)`[hen %give %crud p.q.hin q.q.hin]~
        ~&  [%crud-stuff qic=?~(qic.u.sat ~ [p -.q]:u.qic.u.sat) onz=onz.u.sat]
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
      ?.  (~(has by q.zam.u.sat) hen)  ~&  %app-lost  [~ ..^$]
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
      ^-  (unit (unit (pair mark ,*)))
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
      |=(a=(unit) (bind a |=(b=* [%noun b])))
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
  ?>  ?=(?(%mess %show %nuke) -.kon)
  =+  you=`ship`?-(-.kon %mess p.kon, %nuke p.kon, %show p.kon)
  =+  mat=(~(got by pol.all) you)
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
      %f
    :_  ..^$
    :_  ~
    :-  hen
    ?-    -.p.+.sih
        %|
      [%give %crud %gasp-crud p.p.+.sih]
    ::
        %&
      =+  cay=`cage`q.p.p.+.sih
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
  :_  ..^$
  =+  rod=|=(ron=roon `note`[%a %wont [our you] [%q %gh imp] num ron])
  ?+  -.pax  !!
    %m  ?:  ?=(%a -.sih)  ~
        ?+    -.sih  ~&  [%gall-gave-m -.sih]  !!
            %f
          :_  ~  :-  hen
          ?-  -.p.+.sih
            %|  [%give %mean ~ %ford-fail p.p.+.sih]
            %&  [%pass [%r pax] %g %mess [our imp] you `cage`q.p.p.+.sih]
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
            %mean  [%give %mean p.+.sih]
            %nice  [%give %nice ~]
            %rush  !!
            %rust  !!
            %sage  !!
            %verb  !!
            %veer  !!
            %vega  !!
          ==
        ==
    %s  ?+    -.sih  !!
            %a  ~
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
            %rust  [%pass [%r pax] (rod %f p.+.sih q.+.sih)]
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
    %d  [%pass /x/d `note`[%f %exec p.saq ~ %vale p.ron q.saq q.ron]]
    %e  [%give %mean p.ron]
    %f  [%pass /x/f `note`[%f %exec p.saq ~ %vale p.ron q.saq q.ron]]
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
        ?-(-.rok %m %m, %s %s, %u %s)
        (scot %p p.saq)
        (scot %p q.saq)
        (scot %ud num)
        imp
    ==
  ^-  note
  ?-  -.rok
    %m  [%f %exec p.saq ~ %vale p.rok q.saq q.rok]
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
  ++  away                                              ::  application path
    |=  pax=path  ^-  path
    =+  imc=[%many (turn imp |=(a=span [%$ %ta a]))]
    [%a (scot %p our) ~(rent co imc) pax]
  ::
  ++  bear                                              ::  write backward
    |=  hen=duct
    =+  orf=(~(got by q.zam.sat) hen)
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
        =+  orf=(~(got by q.zam.sat) neh)
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
        =>  (give %nice ~)
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
        %_    +>.$
            mow
          :_  mow
          [(~(got by r.zam.sat) p.i.pys) %slip [%g %show [our imp] q.i.pys]]
        ==
        ::  %=    +>.$
        ::      vey.sat
        ::    %-  ~(put to vey.sat)
        ::    :-  (~(got by r.zam.sat) p.i.pys)
        ::    [%show q.i.pys]
        ::  ==
      ::
      ++  deff
        |=  [wir=wire hon=duct caq=vase]
        ^-  toil
        ?>  ?=([%pass p=* q=%g r=[p=%cide q=span]] q.caq)
        ::  ~&  [%deff imp cub.sat]
        [hon r.q.caq]
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
            ::~&  [%sync-subscribe our p.a q.a]
            [%warp [our p.a] q.a ~ %| [%da +(now)] [%da (add now ~d1000)] /]
        =+  ^=  old  ^-  (list move)
            %+  turn
              %+  skip  (~(tap in ped.sat) ~)
              |=(a=(pair ship desk) (~(has in pen) a))
            |=  a=(pair ship desk)
            :-  hun.mat
            :^  %pass  (away %w %drug (scot %p p.a) q.a ~)  %c
            ~&  [%sync-unsubscribe our p.a q.a]
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
        =.  kas
          :+  %dude
            leaf/"error in app {<app.sat>} on {<our>} at instance {<imp>}"
          kas
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
        [%ride [%cnzy arm] kas]
      ::
      ++  home                                          ::  load application
        ^-  silk
        [%boil %core [[our %main [%da now]] app.sat %app ~] ~]
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
        ?>  ?=(%f -.sih)
        ^-  [(unit (list tank)) _+>]
        ?-  -.p.+.sih
          &  :-  ~
             %-  obey:(morn (slot 3 q.q.p.p.+.sih))
             (slot 2 q.q.p.p.+.sih)
          |  [`p.p.+.sih (give %crud %mack-made p.p.+.sih)]
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
             :-  `[((hard mark) -.q.vax) (slot 3 vax)]
             +>.$
          |  [~ (give %crud %meek-made p.p.+.sih)]
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
          |  [~ (give %crud %mick-made p.p.+.sih)]
        ==
      ::
      ++  muck                                          ::  apply part
        |=  sih=sign
        ^-  [(unit (list tank)) _+>]
        ?>  ?=(%f -.sih)
        ?-  -.p.+.sih
          &  [~ (obey q.q.p.p.+.sih)]
          |  [`p.p.+.sih (give %crud %muck-made p.p.+.sih)]
        ==
      ::
      ++  murk                                          ::  apply park
        |=  sih=sign
        ^-  [(unit cage) _+>]
        ?>  ?=(%f -.sih)
        ?-  -.p.+.sih
          &  [`q.p.p.+.sih +>.$]
          |  [~ (give %crud %murk-made p.p.+.sih)]
        ==
      ::
      ++  moar                                          ::  foreign take
        |=  $:  pax=path
                sih=sign
            ==
        ^+  +>
        !!
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
            ?~  gyd
              +>.$
            (quen %load u.gyd)
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
            ?~  gad  (drum ~)
            deal:(drum u.gad)
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
          ?>  ?=([%drug @ @ ~] t.pax)
          =+  :*  sin=((hard ,[%c %writ p=riot]) q.hin)
                  our=(need (slaw %p i.t.t.pax))
                  syd=(need ((sand %tas) i.t.t.t.pax))
              ==
          ::~&  [%sync-notified `@p`our `@ta`syd]
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
        %=    $
            pex    t.pex
            +>.^$  %-  quem(hen (~(got by r.zam.sat) p.i.pex))
                   [%show (~(got by sup.sat) p.i.pex)]
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
        |-  ^-  [(list toil) (list move)]
        ?:  =(~ q.vud)  [~ mow]
        =+  sud=(sump (slot 2 vud))
        =+  res=$(vud (slot 3 vud))
        :-  ?~(-.sud -.res [u.-.sud -.res])
        ?~(+.sud +.res [u.+.sud +.res])
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
        =+  hon=(need (~(get by r.zam.sat) ((hard bone) -.q.wec)))
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
          :^  ~  ~  hon
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
        ::  ~&  [%work imp ?~(qic.sat ~ [~ -.q.u.qic.sat (turn (~(tap by vey.sat)) |=(toil -.q))])]
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
          :+  %dude  leaf/"booting"
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
          =+  ^=  cog  ^-  term
              ?:  =(%$ p.q.kon)  %poke
              =+  goc=(cat 3 'poke-' p.q.kon)
              ?:((warm goc) goc %poke)
          ?.  (warm cog)
            (give(qic.sat ~) %mean ~ %poke-find-fail ~)
          ?>  ?=(^ huv.sat)
          =+  sam=:(slop [[%atom %ud] ost] [[%atom %p] p.kon] q.q.kon)
          =+  err=?.(=(%poke cog) <cog> "%poke with mark <p.q.kon>")
          ::  ~&  [%mess-poke cog]
          %+  ford  /s/poke
          :+  %dude  leaf/"poking {err}"
          [%call (harm cog (conf (core u.huv.sat))) (cove %$ sam)]
        ::
            %show
          ?:  (warm %peer)
            =+  sam=!>([ost p.kon q.kon])
            ?>  ?=(^ huv.sat)
            =.  peq.sat  (~(del by peq.sat) ost)
            (yawl [%peer (scot %p p.kon) q.kon] leaf/"peering" u.huv.sat sam)
          ?:  (warm %peek)
            =+  sam=!>([p.kon q.kon])
            ?>  ?=(^ huv.sat)
            (yawl [%peek (scot %p p.kon) q.kon] leaf/"peeking" u.huv.sat sam)
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
