!:  ::  %eyre, http servant
!?  164
::::
|=  pit=vase
=>  =~
|%                                                      ::  interfaces
++  bead  ,[p=(set beam) q=cage]                        ::  computed result
++  chop  ,[p=@ud q=@da]                                ::  see 
++  gift                                                ::  out result <-$
          $%  [%send p=lane q=@]                        ::  transmit packet
              [%thou p=httr]                            ::  raw http response
              [%thus p=@ud q=(unit hiss)]               ::  http request/cancel
         ==                                             ::
++  hasp  ,[p=ship q=term]                              ::  see %gall
++  kiss                                                ::  in request ->$
          $%  [%born ~]                                 ::  new unix process
              [%clug p=ship q=hole r=@ud]               ::  XX terrible
              [%crud p=@tas q=(list tank)]              ::  XX rethink
              [%init p=@p]                              ::  report install
              [%them p=(unit hiss)]                     ::  outbound request
              [%they p=@ud q=httr]                      ::  inbound response
              [%this p=? q=clip r=httq]                 ::  inbound request
              [%thud ~]                                 ::  inbound cancel
              [%wart p=sock q=@tas r=path s=*]          ::  urbit message
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  note                                                ::  out request $->
          $%  $:  %a                                    ::  to %ames
          $%  [%want p=sock q=path r=*]                 ::
          ==  ==                                        ::
              $:  %b                                    ::  to %batz
          $%  [%hail ~]                                 ::
              [%line p=@t]                              ::
              [%ling ~]                                 ::
          ==  ==                                        ::
              $:  %c                                    ::  to %clay
          $%  [%warp p=sock q=riff]                     ::
          ==  ==                                        ::
              $:  %d                                    ::  to %dill
          $%  [%flog p=[%crud p=@tas q=(list tank)]]    ::
          ==  ==                                        ::
              $:  %e                                    ::  to %eyre
          $%  [%clug p=ship q=hole r=@ud]               ::  XX terrible
              [%crud p=@tas q=(list tank)]              ::  XX rethink
              [%this p=? q=clip r=httq]                 ::
              [%thud ~]                                 ::
          ==  ==                                        ::
              $:  %f                                    ::  to %ford
          $%  [%exec p=@p q=(unit silk)]                ::
          ==  ==                                        ::
              $:  %g                                    ::  to %gall
          $%  [%mess p=hasp q=ship r=cage]              ::
              [%nuke p=hasp q=ship]                     ::
              [%show p=hasp q=ship r=path]              ::
          ==  ==  ==                                    ::
++  rave                                                ::  see %clay
          $%  [| p=moat]                                ::
          ==                                            ::
++  riff  ,[p=desk q=(unit rave)]                       ::  see %clay
++  sign                                                ::  in result $<-
          $?  $:  %a                                    ::  by %ames
          $%  [%send p=lane q=@]                        ::
              [%waft p=sock q=*]                        ::
              [%went p=ship q=cape]                     ::
          ==  ==                                        ::
              $:  %b                                    ::  by %batz
          $%  [%helo p=path q=prod]                     ::
              [%talk p=tank]                            ::
              [%tell p=(list ,@t)]                      ::
              [%text p=tape]                            ::
              [%warn p=tape]                            ::
          ==  ==                                        ::
              $:  %c                                    ::  by %clay
          $%  [%writ p=riot]                            ::
          ==  ==                                        ::
              $:  %e                                    ::  by %eyre
          $%  [%thou p=httr]                            ::
          ==  ==                                        ::
              $:  %f                                    ::  by %ford
          $%  [%made p=(each bead (list tank))]         ::
          ==  ==                                        ::
              $:  %g                                    ::  by %gall
          $%  [%dumb ~]                                 ::
              [%mean p=(unit ,[p=term q=(list tank)])]  ::
              [%nice ~]                                 ::
              [%rush p=logo q=*]                        ::
              [%rust p=logo q=*]                        ::
          ==  ==                                        ::
              $:  @tas                                  ::  by any
          $%  [%crud p=@tas q=(list tank)]              ::
          ==  ==  ==                                    ::
--                                                      ::
|%                                                      ::  models
++  bolo                                                ::  eyre state
  $:  %0                                                ::  version
      gub=@t                                            ::  random identity
      hov=(unit ship)                                   ::  master for remote
      ged=duct                                          ::  client interface
      ney=@uvI                                          ::  rolling entropy
      dop=(map host ship)                               ::  host aliasing
      own=(map ship serf)                               ::  domestic servers
      fon=(map ship rote)                               ::  foreign servers
      ask=[p=@ud q=(map ,@ud ,[p=duct q=hiss])]         ::  outgoing by number
      kes=(map duct ,@ud)                               ::  outgoing by duct
      lor=(map duct dual)                               ::  incoming by duct
  ==                                                    ::
++  clue                                                ::  console
  $:  ino=@ud                                           ::  input sequence
      ono=@ud                                           ::  (lent out)
      voy=(map ,@ud (list ,@ud))                        ::  waiters (q.rey)
      out=(list json)                                   ::  output commands
  ==                                                    ::
++  cyst                                                ::  client session
  $:  ced=cred                                          ::  credential
      cow=(map ,@ud clue)                               ::  consoles
      cug=(list ,@t)                                    ::  unacked cookies
      lax=@da                                           ::  last used
      sok=(map ,@ud (trel term ship sink))              ::  live apps by reqno
      rey=[p=@ud q=(map ,@ud pimp)]                     ::  live requests
  ==                                                    ::
++  dual  ,[p=@ud q=(each ,[p=ship q=hole] ship)]       ::  request handle
++  dude  ,[p=@tas q=@]                                 ::  client identity
++  loco  ,[p=? q=(unit ,@tas) r=path]                  ::  logical construct
++  pest                                                ::  request in progress
  $|  $?  %new                                          ::  virgin
          %way                                          ::  waiting
      ==                                                ::
  $%  [%err p=@ud q=(list tank)]                        ::  error report
      [%fin p=love]                                     ::  ready to send
      [%fud p=(each bead (list tank))]                  ::  function finished
      [%haz p=riot]                                     ::  clay responded
      [%raw p=hiss]                                     ::  wild url
      [%who p=@tas q=@ta]                               ::  awaiting auth
  ==                                                    ::
++  pimp                                                ::  traced request
  $:  ful=?                                             ::  | === HEAD
      fur=(unit logo)                                   ::  type goal
      hen=duct                                          ::  event trace
      som=seam                                          ::  logical request
      pez=pest                                          ::  request state
      vaz=(list ,[p=cord q=tape])                       ::  variables 
      sip=(list manx)                                   ::  scripts in result
  ==                                                    ::
++  rote                                                ::  remote server
  $:  cnt=@ud                                           ::  number served
      sor=@p                                            ::  home sponsor
      rem=[p=@ud q=(map ,@ud duct)]                     ::  active requests
  ==                                                    ::
++  seam                                                ::  logical request
  $%  [%ape p=term q=ship r=@ud s=@ud t=@ud]            ::  subscribe pull
      [%aph p=term q=ship r=@ud s=@ud t=json]           ::  app heartbeat
      [%apg p=term q=ship r=logo s=path]                ::  app get/start
      [%apm p=term q=ship r=@ud s=@ud t=json]           ::  message send
      [%aps p=term q=ship r=@ud s=@ud t=path]           ::  subscribe
      [%apu p=term q=ship r=@ud s=@ud]                  ::  unsubscribe
      [%cog p=@ud q=@ud]                                ::  console get
      [%con p=@ud]                                      ::  console face
      [%cop p=@ud q=@ud r=json]                         ::  console put
      [%det p=desk q=moat]                              ::  load changes
      [%fun p=term q=tube r=(list manx)]                ::  functional
      [%lon p=seal]                                     ::  authentication flow
      [%red p=purl]                                     ::  redirect
      [%sil p=@ud q=silk]                               ::  status and silk
  ==                                                    ::
++  serf                                                ::  local server
  $:  pef=@t                                            ::  server prefix
      wup=(map hole cyst)                               ::  secure sessions
      cah=(map cash vase)                               ::  compilation cache
  ==                                                    ::
++  sink                                                ::  page push system
  $:  bet=[num=@ud tim=@da]                             ::  heartbeat
      meg=@ud                                           ::  message counter
      haw=(map ,@ud swig)                               ::  subscriptions
  ==                                                    ::
++  swig                                                ::  update channel
  $:  cnt=@ud                                           ::  updates produced
      toy=@ud                                           ::  updates sent
      wan=(map ,@ud ,@ud)                               ::  upno to reqno
      red=(map ,@ud (unit (each cage cage)))            ::  ready for pickup
  ==                                                    ::
--                                                      ::
|%
++  coss                                                ::  cookie search
  |=  [nam=@t mah=math]
  ^-  (unit hole)
  =+  ^=  cok  ^-  (list ,@t)
      =+  cok=(~(get by mah) 'cookie')
      ?~(cok ~ u.cok)
  |-  ^-  (unit hole)
  ?~  cok  ~
  =+  mar=`(unit (list ,[p=@t q=@t]))`(rush i.cok cock:epur)
  ?~  mar  $(cok t.cok)
  |-  ^-  (unit hole)
  ?~  u.mar  ^$(cok t.cok)
  ?:(=(nam p.i.u.mar) [~ q.i.u.mar] $(u.mar t.u.mar))
::
++  ecce                                                ::  JS from moth
  |=  moh=moth
  ^-  (unit json)
  ?.  =([~ 'text/json' ~] (~(get by q.moh) 'content-type'))  ~
  ?~  r.moh  ~
  `(unit json)`(rush q.u.r.moh apex:poja)
::
++  ecci                                                ::  ecce w/oryx
  |=  [orx=oryx moh=moth]
  ^-  (unit json)
  =+  jun=(ecce moh)
  ?~  jun  ~
  ?.  ?=(%o -.u.jun)  ~
  ?.  =([~ %s orx] (~(get by p.u.jun) %oryx)) 
    ~&  [%ecci-oryx u.jun]
    ~
  =+  nuj=(~(get by p.u.jun) %xyro)
  ?~(nuj [~ ~] [~ u.nuj])
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
++  lopo                                                ::  cage to love
  |=  cay=cage
  ^-  love
  ?>  ?=(%mime p.cay)
  ((hard love) [%mid q.q.cay])
::
++  loga                                                ::  tanks to manx
  |=  [til=tape mog=(list manx) tac=(list tank)]
  ^-  manx
  =+  ^=  wol
      |-  ^-  wall
      ?~  tac  ~
      (weld `wall`[~(ram re i.tac) ~] $(tac t.tac))
  =+  ^=  tax
      |-  ^-  (list manx)
      (turn wol |=(a=tape [/p ;"{a}"]))
  ;html
    ;head
      ;title: {til}
    ==
    ;body
      ;code
        ;*  (weld tax mog)
      ==
    ==
  ==
::
++  lofa                                                ::  scripts in head
  |=  [mog=(list manx) luv=love]
  ^-  love
  ?:  =(~ mog)  luv
  ?+    -.luv  luv
      %mid
    =+  str=(trip q.q.luv)
    =+  scr=|-(^-(tape ?~(mog ~ (xmlt & i.mog $(mog t.mog)))))
    =+  rep=(need (repg "<head>" str (weld "<head>" scr)))
    [%mid p.luv (tact rep)]
  ==
++  lofe                                                ::  variables in head
  |=  [vaz=(list ,[p=cord q=tape]) luv=love]
  %-  lofa
  :_  luv
  :_  ~
  ^-  manx
  :-  [%script ~]
  (turn vaz |=([a=cord b=tape] :/("var {(trip a)}={b};")))
::
++  lofi                                                ::  insert in body
  |=  [mog=(list manx) luv=love]
  ^-  love
  ?:  =(~ mog)  luv
  ?+    -.luv  luv
      %mid
    =+  str=(trip q.q.luv)
    =+  scr=|-(^-(tape ?~(mog "</body>" (xmlt & i.mog $(mog t.mog)))))
    =+  rep=(need (repg "</body>" str scr))
    [%mid p.luv (tact rep)]
  ==
::
++  loft                                                ::  love to response
  |=  luv=love
  ^-  httr
  ?-  -.luv
    %mid  [200 ~[content-type/(moon p.luv)] [~ q.luv]]
    %ham  [200 ~[content-type/'text/html'] [~ (tact (xmlt | p.luv ~))]]
    %raw  p.luv
    %wan  :+  200
            ~[content-type/'text/plain']
          :-  ~
          %-  taco
          %+  rap  3
          |-  ^-  (list ,@)
          ?~(p.luv ~ [i.p.luv 10 $(p.luv t.p.luv)])
    %zap  :+  p.luv
            ~[content-type/'text/html']
          [~ (tact (xmlt | (loga "server error" ~ q.luv) ~))]
  ==
--
|%                                                      ::  functions
++  ye                                                  ::  per event
  =|  $:  $:  hen=duct                                  ::  event floor
              $:  now=@da                               ::  event date
                  eny=@                                 ::  unique entropy
                  sky=$+(* (unit))                      ::  system namespace
              ==                                        ::
              mow=(list move)                           ::  pending actions
          ==                                            ::
          bolo                                          ::  all vane state
      ==                                                ::
  =*  bol  ->
  |%
  ++  abet
    ^-  [(list move) bolo]
    [(flop mow) bol]
  ::
  ++  adit
    .(ney (mix eny ney))
  ::
  ++  axon
    |=  [tea=wire typ=type sih=sign]
    ^+  +>
    ?-    -.+.sih
        %crud
      +>.$(mow [[hen %slip %d %flog +.sih] mow])
    ::
        ?(%dumb %mean %nice %rush %rust)
      ?>  ?=([%hoop @ @ @ @ ~] tea)
      ~&  [%dumb-tea tea]
      =+  ^=  ouy
          %-  yolk:(gale (slav %p i.t.tea) i.t.t.tea)
          (slav %ud i.t.t.t.tea)
      ?~  ouy
        +>.$
      ~&  [%axon-fun `@dr`(sub now tim.bet.siq:beat:u.ouy)]
      ?:  (lth ~s20 (sub now tim.bet.siq:beat:u.ouy))
        abet:work:amok:u.ouy
      =+  woy=(yule:u.ouy (slav %ud i.t.t.t.t.tea))
      =<  abet  =<  work  =<  abet
      ?-  -.+.sih
          %dumb
        dumb:woy
          %mean
        =+  ^=  jso
            %-  jobe
            :-  [%ok %b |]
            ?~  p.+.sih  ~
            :+  [%err %s p.u.p.+.sih]
              :+  %res  %s 
              %-  crip
              %+  slag  2
              ^-  tape
              %+  roll  q.u.p.+.sih
              |=  [p=tank q=tape]
              :(weld q "\\n" ~(ram re p))
            ~
        (hear:woy ~ %& %json !>(jso))
          %nice
        (hear:woy ~ %& %json !>((joba %ok %b &)))
          ?(%rust %rush)
        =+  cay=`cage`[p.+.sih (slot 3 (spec (slot 3 [typ +.sih])))]
        (hear:woy ~ ?:(?=(%rust -.+.sih) [%& cay] [%| cay]))
      ==
    ::
        %made
      ?.  ?=([%honk @ @ @ ~] tea)
        +>.$
      %-  galt
      [(slav %p i.t.tea) i.t.t.tea (slav %ud i.t.t.t.tea) p.+.sih]
    ::
        %thou                                           ::  remote return
      ?>  ?=([@ @ *] tea)
      (hajj (slav %p i.tea) (slav %p i.t.tea) t.t.tea p.+.sih)
    ::
        %waft
      ?.  ?=([%hork @ ~] tea)
        +>.$
      (gosh q.p.+.sih (slav %ud i.t.tea) ((hard httr) q.+.sih))
    ::
        %went
      +>.$
    ::
        %writ
      ?.  ?=([%hoot @ @ @ ~] tea)
        +>.$
      %-  gout
      [(slav %p i.t.tea) i.t.t.tea (slav %ud i.t.t.t.tea) p.+.sih]
    ::
        ?(%helo %tell %text %talk %warn)
      ?.  ?=([%cons @ @ @ ~] tea)
        +>.$
      %-  goat
      [(slav %p i.t.tea) i.t.t.tea (slav %ud i.t.t.t.tea) sih]
    ::
        %send
      +>.$(mow [[hen %give +.sih] mow])
    ==
  ::
  ++  apex
    |=  kyz=kiss
    ^+  +>
    ?-    -.kyz
        %born  +>.$(ged hen)                            ::  register external
        %clug
      =+  ^=  ouy
          %-  yolk:(gale p.kyz q.kyz)
          r.kyz
      ?~  ouy
        +>.$
      ~&  [%axon-fun `@dr`(sub now tim.bet.siq:beat:u.ouy)]
      ?:  (lth ~m2 (sub now tim.bet.siq:beat:u.ouy))
        abet:work:amok:u.ouy
      =<  abet  =<  work  =<  abet
      (hear:(yule:u.ouy 2) ~ %& %json !>((joba %i-see %s %you)))
        %crud
      +>.$(mow [[hen %slip %d %flog kyz] mow])
        %init                                           ::  register ownership
      %_    +>.$
          hov  ?~(hov [~ p.kyz] [~ (min u.hov p.kyz)])
          own
        %+  ~(put by own)
          p.kyz
        ^-  serf
        :*  (cat 3 gub (rsh 3 1 (scot %p p.kyz)))
            ~
            ~
        ==
      ==
    ::
        %them                                           ::  outbound request
      ?~  p.kyz
        =+  sud=(need (~(get by kes) hen))
        %=  +>.$
          mow    :_(mow [ged [%give %thus sud ~]])
          q.ask  (~(del by q.ask) sud)
          kes    (~(del by kes) hen)
        ==
      %=  +>.$
        mow    :_(mow [ged [%give %thus p.ask p.kyz]])
        p.ask  +(p.ask)
        q.ask  (~(put by q.ask) p.ask hen u.p.kyz)
        kes    (~(put by kes) hen p.ask)
      ==
    ::
        %they                                           ::  inbound response
      =+  kas=(need (~(get by q.ask) p.kyz))
      %=  +>.$
        mow    :_(mow [p.kas [%give %thou q.kyz]])
        q.ask  (~(del by q.ask) p.kas)
      ==
    ::
        %this                                           ::  inbound request
      =*  sec  p.kyz    ::  ?                           ::  https bit
      =*  heq  r.kyz    ::  httq                        ::  request content
      =+  ryp=`quri`(rash q.heq zest:epur)
      =+  mah=(ecco r.heq)
      =+  ^=  pul  ^-  purl
          ?-  -.ryp
            &  ?>(=(sec p.p.p.ryp) p.ryp)
            |  =+  hot=(~(get by mah) %host)
               ?>  ?=([~ @ ~] hot)
               [[sec (rash i.u.hot thor:epur)] p.ryp q.ryp]
          ==
      =.  p.p.pul  |(p.p.pul =([& /localhost] r.p.pul))
      (hell pul +.kyz [p.heq mah s.heq])
    ::
        %thud                                           ::  cancel request
      =+  dul=(~(get by lor) hen)
      ?~  dul  +>.$
      =.  lor  (~(del by lor) hen)
      ?-  -.q.u.dul
        &  =+  boy=(myth p.p.q.u.dul q.p.q.u.dul)
           ?~(boy +>.$ abet:(idle:u.boy p.u.dul))
        |  (hops p.q.u.dul p.u.dul)
      ==
    ::
        %wart                                           ::  remote request
      ?+    q.kyz
        ~&  [%strange-wart p.kyz q.kyz]
        +>.$
      ::
          %pr  (hare p.p.kyz r.kyz q.p.kyz s.kyz)
          %pc  (here p.p.kyz q.p.kyz s.kyz)
      ==
    ==
  ::
  ++  doss                                              ::  host to ship
    |=  hot=host
    ^-  (unit ship)
    =+  gow=(~(get by dop) hot)
    ?^  gow  gow
    ?.  &(?=(& -.hot) ?=(^ p.hot))  ~
    (rush -:(flop p.hot) fed:ag)
  ::
  ++  fail                                              ::  request failed
    |=  [sas=@ud str=tape]
    ^+  +>
    %-  muff
    :-  %thou
    ^-  httr
    [sas ~[content-type/'text/plain'] [~ (tact str)]]
  ::
  ++  gale                                              ::  ya from response
    |=  [our=ship ses=hole]
    =+  sef=(need (~(get by own) our))
    =+  cyz=(need (~(get by wup.sef) ses))
    ~(. ya [our ses] sef cyz)
  ::
  ++  galt
    |=  [our=ship ses=hole num=@ud mez=(each bead (list tank))]
    ^+  +>
    =+  suf=(~(get by own) our)
    ?~  suf  +>.$
    =+  cuz=(~(get by wup.u.suf) ses)
    ?~  cuz  +>.$
    abet:work:(~(inch ya [our ses] u.suf u.cuz) num mez)
  ::
  ++  goat                                              ::  console response
    |=  [our=ship ses=hole num=@ud sih=sign]
    =+  suf=(~(get by own) our)
    ?~  suf  +>.$
    =+  cuz=(~(get by wup.u.suf) ses)
    ?~  cuz  +>.$
    abet:work:(~(dodo ya [our ses] u.suf u.cuz) num sih)
  ::
  ++  gosh                                              ::  receive %pr response
    |=  [him=ship num=@ud har=httr]
    ^+  +>
    =+  ryt=(~(get by fon) him)
    ?~  ryt  +>.$
    =+  zur=(~(get by q.rem.u.ryt) num)
    ?~  zur  +>.$
    %_  +>.$
      mow  :_(mow [u.zur [%give %thou har]])
      fon  (~(put by fon) him u.ryt(q.rem (~(del by q.rem.u.ryt) num)))
    ==
  ::
  ++  gout                                              ::  receive %clay
    |=  [our=ship ses=hole num=@ud rot=riot]
    ^+  +>
    =+  suf=(~(get by own) our)
    ?~  suf  +>.$
    =+  cuz=(~(get by wup.u.suf) ses)
    ?~  cuz  +>.$
    abet:work:(~(iota ya [our ses] u.suf u.cuz) num rot)
  ::
  ++  haji                                              ::  send %pc login
    |=  [our=ship him=ship ses=hole]
    ^+  +>
    +>.$(mow :_(mow [hen %pass ~ %a [%want [our him] [%r %pc ~] ses]]))
  ::
  ++  hajj                                              ::  send %pr response
    |=  [our=ship him=ship tus=path har=httr]
    ^+  +>
    +>.$(mow :_(mow [hen %pass ~ %a [%want [our him] [%r %pr tus] har]]))
  ::
  ++  hare                                              ::  receive request
    |=  [our=ship tus=path him=ship hor=*]
    ^+  +>
    =+  hux=((hard (unit httx)) hor)
    %_    +>.$
        mow
      :_  mow
      :-  hen
      :^  %pass  [(scot %p our) (scot %p him) tus]
        %e
      ?~(hux [%thud ~] [%this u.hux])
    ==
  ::
  ++  here                                              ::  receive auth
    |=  [our=ship him=ship hez=*]
    ^+  +>
    =+  boy=(myth our (need ((sand %ta) ((hard ,@) hez))))
    ?~(boy +>.$ abet:(auth:u.boy him))
  ::
  ++  hell                                              ::  request, no ship
    |=  [pul=purl hyx=httx moh=moth]
    ^+  +>
    =+  hon=(horn pul q.hyx moh)
    ?^  hon  (muff u.hon)
    ::  =^  wiq  q.q.pul
    ::      ?~  q.q.pul  [~ ~]
    ::      =+  nam=(cat 3 '~' i.q.q.pul)
    ::      =+  gow=(rush i.q.q.pul fed:ag)
    ::      ^-  [(unit ship) (list ,@t)]
    ::      ?~(gow [~ q.q.pul] [gow t.q.q.pul])
    ::  =+  oar=`(unit ship)`?^(wiq wiq (doss r.p.pul))
    =+  oar=(fall (doss r.p.pul) (need hov))
    ?.  (home oar)
      (hork oar hyx)
    (huff oar q.hyx pul moh)
  ::
  ++  home                                              ::  do we own?
    |=  who=ship
    ^-  ?
    ?:  (~(has by own) who)  &
    ?:  (~(has by fon) who)  |
    !=(~ (sky /a/(scot %p who)/buck/(scot %da now)))
  ::
  ++  hoot                                              ::  clay request
    |=  [our=ship num=@ud ses=hole rif=riff]
    %_    +>
        mow
      :_  mow
      :+  hen  %pass
      [[%hoot (scot %p our) ses (scot %ud num) ~] %c [%warp [our our] rif]]
    ==
  ::
  ++  hone                                              ::  kill ford
    |=  [our=ship num=@ud ses=hole]
    %_    +>
        mow
      :_  mow
      [hen %pass [%honk (scot %p our) ses (scot %ud num) ~] %f [%exec our ~]]
    ==
  ::
  ++  honk                                              ::  ford request
    |=  [our=ship num=@ud ses=hole kas=silk]
    ::  ~&  [%honk our num ses kas]
    %_    +>
        mow
      :_  mow
      [hen %pass [%honk (scot %p our) ses (scot %ud num) ~] %f [%exec our `kas]]
    ==
  ::
  ++  hops                                              ::  cancel remote
    |=  [him=ship num=@]
    ^+  +>
    =+  mun=(scot %ud num)
    =+  rot=(need (~(get by fon) him))
    %_    +>.$
        mow
      :_  mow
      :-  hen
      :^  %pass  [%hork (scot %p sor.rot) mun ~]
        %a 
      [%want [sor.rot him] [%q %pr %e %hork mun ~] ~]
    ==
  ::
  ++  hork                                              ::  remote request
    |=  [him=ship hyx=httx]
    ^+  +>
    =+  ^=  sur  ^-  (unit ship)
        ?^  hov  hov
        ?^  own  [~ p.n.own]
        ~
    ?~  sur  (fail 500 "no vessel available to proxy {<him>}")
    ?.  (gth (met 3 him) (met 3 u.sur))                 ::  very permissive
      (fail 500 "{<u.sur>} cannot proxy for {<him>}")
    =+  ^=  rot  ^-  rote
        =+  rut=(~(get by fon) him)
        ?^  rut  u.rut
        [0 u.sur [0 ~]]
    =+  num=p.rem.rot
    =+  mun=(scot %ud num)
    %_    +>.$
        lor  (~(put by lor) hen num [%| him])
        mow
      :_  mow
      :-  hen
      :^  %pass  [%hork (scot %p sor.rot) mun ~]
        %a
      [%want [sor.rot him] [%q %pr %e %hork mun ~] [~ hyx]]
    ::
        fon
      %+  ~(put by fon)  him
      %_  rot
        cnt  +(cnt.rot)
        p.rem  +(p.rem.rot)
        q.rem  (~(put by q.rem.rot) num hen)
      ==
    ==
  ::
  ++  horn                                              ::  irregular request
    |=  [pul=purl cip=clip moh=moth]
    ^-  (unit gift)
    =-  ?:  &(=(/favicon q.q.pul) ?=([~ ?(%ico %png)] p.q.pul))
          :-  ~
          :-  %thou
          ^-  httr
          [200 ~[content-type/'image/png'] [~ (taco fac)]]
        ?:  &(=(/robots q.q.pul) ?=([~ %txt] p.q.pul))
          :-  ~
          :-  %thou
          ^-  httr
          [200 ~[content-type/'text/plain'] [~ (taco rob)]]
        ~
    :*
        ^=  rob
      %-  roly
      :~  'User-agent: *'
          'Disallow: /'
      ==
    ::
        ^=  fac
      0w89.wgGV4.jAl90.00003.sV4OG.IJjfa.1vYpi.gRxB9.3m6kA.dopig.
      RxB93.m6kAd.opigR.xB93m.6kAdo.pigRx.B93m6.kAdop.igRxB.93m6k.
      Adopi.gRxBf.vGSfy.m8hQj.T-DiD.7kqvH.vEpA3.3vH-C.in~Tq.l8U0n.
      1FVhj.w9E1A.NIF6w.4j9v~.VZ0~B.9flkB.IY90B.-ieSV.Ky8Q~.4~07s.
      JcXFC.DtI-1.GGz-1.V-olV.g3wsv.ZCQM1.BJbVj.Vwiv0.uo7Gh.4qsxA.
      92ZYU.tJ5uH.yiIzV.FwvJR.UUq6z.cpKIG.Hck9v.qGDm1.PY2rM.itxLB.
      fn0Bo.5DO8x.oO7KE.kYh-P.NiKp1.HT88j.Mu3ZK.ciKsU.TnlkV.0Zo77.
      12ciy.nY3dM.7nDnY.GVgGh.ZllpO.SFHFb.p1Ae0.uUpXV.eqFvS.pkBRl.
      jv0MP.ilRHP.1HwtK.GFptt.2KdpP.RsYqI.wRHEG.j~LZQ.I06qJ.fP0Pp.
      77qjo.s0PU0.rGGg6.lgNvc.~CZE~.bSp9j.EGHF~.UqYB6.l4Y~Z.P~GGE.
      LwrJs.ZvYV-.U4Wh4.04dws.6HeuZ.2ZF7A.y4MN5.3vsCj.QHzjW.4lflk.
      WU6X0.AmMws.vbMfB.3e1s~.aeE7W.0hQPH.ODvMf.cvgzb.Y15Ah.01384.
      YwVPT.KzILB.PlaqN.pNlvw.fdQ79.~mPpo.YaHqw.fnWGB.QYM4F.w3E0b.
      0o~n-.faydD.zlllI.0nU3D.w5rlI.4nrSG.VkhPM.NTkpa.eoXzw.9AEYN.
      auZGt.99gxL.8RlsI.aXMXX.tFVhX.V4kj8.yczjh.faRI3.JTg1H.-0uZM.
      JA6rR.z0~pO.uXiSg.rvU27.A58MU.TBijQ.23F1J.CCIYE.IO8w-.cMlMA.
      hvKh4.zY16M.gjRlk.v--9h.TNNRR.HhIGo.8kZXk.Wb74j.faHlk.6V-Vw.
      jMan8.yb37R.Q2h42.Or3Nw.Pp39w.jZ--3.-jwZH.U~3Za.Uu0u6.bNAOP.
      U2jux.Jqo2R.O8x1~.ecZvL.30ug~.qpoFw.vwtqD.Vb6EI.cZQyO.EN-xl.
      nlsLC.dT099.apOh5.SEeDz.07-GE.xFzZk.KcmCl.SJWF5.v3u1x.Uq1Cj.
      tV~hG.YuGGb.SgpdR.xHaBh.S3eEv.q0mSg.RZh8s.wxhnk.EcNvW.GccZQ.
      yO0Jb.n18hs.BLFx2.iigqf.AhsKS.LWqby.TUEmv.gmmhR.6DW3w.uLR0Y.
      QQBC8.YoQ63.g8m8i.iq3B-.SxwLn.jLbh3.l7cq3.eVQmV.5O2df.SXBkv.
      Y3LLb.denQq.GvR0R.P3Gh4.2iiq2.h-srW.o0ZZ-.HIrdj.npm5n.pnv07.
      vyT77.43WGP.Bciiq.zt1cI.7A4xB.zK9xm.-tV6x.ZdA6P.pheXQ.aSz4X.
      Zj2bS.C1UPx.~c1dS.xwF3b.6jZ-M.WI2eQ.e69Qw.DGFly.tTze-.GGbZU.
      qJ-m-.fD8yI.Adktz.oqTsF.F7ltA.6no6T.~fWJU.0gRsp.-P88x.a9I9b.
      Adkvz.ory8J.Ouhfu.H8c-U.2HLgE.Wi4xH.3AEGK.VjkS-.Z5hMx.UN5o~.
      Y~EWp.7LGox.IQxpt.cgONH.CEyKJ.jjTdM.GJ9HL.RloJZ.xuRtL.JZ7jg.
      ZZj6w.2AOoM.CENdS.xxegZ.RzTdh.i-1hZ.N1HPF.EqHU0.XzN6K.mBedG.
      uvBiL.HqpmY.Bl9z2.qzqA8.WzKqz.h~S1J.K2QHQ.Dy-CM.7RO0l.QksW3.
      mpFnx.fy-Pa.p7xhW.SboOd.fOBon.mCgSX.Z38Qe.dMHUC.79wje.wziG5.
      6Xtn7.ksEHO.xkBrO.e7yFe.vNaYx.FgDsI.BS9y8.AELs-.C9-DB.FAZI-.
      wKt2N.8qQhA.Apxm7.O5yIB.X51l9.Kduxm.SRA5N.UYi6I.MrySX.RZXrT.
      8UcY2.zUAfu.SOcUK.vZrDL.vBAHb.eOo~N.7J3sR.eJhSo.4~YE1.5k0h5.
      51RqS.b0jyR.RfhON.4Dt07.idahL.5isLK.eeBv3.znQxC.9LXkE.xKghP.
      Ia-R0.AgmB5.pGGIA.slCGu.CtR5q.NrzHh.1bscz.8CsWC.KH4it.LLrWm.
      UlRdr.lUGji.W76xr.kVAmO.6oAYS.7nXX~.kfeM2.TSS2m.JOCAb.sFFWg.
      4xH3C.MDKh4.FZso1.tXwUJ.Taq5K.8yS24.xHr4M.Kvu~E.HTpka.-Zg3f.
      KEXFS.qCKwh.l1KRN.c9H8A.HFcSw.rePCF.Iy93m.njkMZ.IEyiq.lFq3y.
      gRFzg.uL9tz.zP8du.Y1ZWP.PtQ6G.gzIt5.K8hNz.UAdpM.Q43L6.IMHx5.
      N8qPh.EfX8G.UC~68.S93ms.d18Vh.adkOx.GLkTI.khFcL.ZWG5G.Adoeh.
      hx~As.hci6I.Uq2pG.ykqHO.yUAdq.gQ7FD.4sOjn.IwGGw.UAdqo.Q4jVN.
      eJP8c.xQlm~.8nJ1y.gRF3g.oSPAM.fuqE0.M~23y.gRHyo.gngjF.ceM3n.
      V~uQy.93m-9.xa-3N.T80~v.GzR-g.HqBGA.mi4xH.3AMOL.mCjT5.Blqab.
      60ruw.HDV~k.Tj~fX.Swx8u.ZFOoi.m1GUF.Gs4-q.0kfxh.H8yjt.OCXGL.
      PYGTY.23LgI.Wl4x6.8bI3e.MXeVb.h6rL9.DXWyt.8wJ8W.HalWR.itqp3.
      pkrSC.8bQSM.HLV2J.G7sCj.QtGEi.AkSwI.A4P0J.gJ85j.MuMUY.nkT45.
      -rkqv.BFBFU.KGd98.qRs~A.iblOv.mVKWx.Z19cs.AxHc6.UIKJc.NIHW8.
      EnOEy.fygRG.29bbR.FBDVL.Ter6T.SBKat.MFBPE.AfuO9.kBHV~.QstE-.
      VaYNV.qpfhL.sFHj0.eFphG.U6Hw6.EsVox.7kpks.N6bRk.GMLY~.HWBVj.
      Snx6X.0GY2b.GhzmW.udfRF.jTgLC.uPWGL.fIwM6.16Ah4.NFZjz.Ftln7.
      KQ-k-.0SO8H.xrqcw.MXZG9.6BZsJ.zULJU.NPDy3.aewMa.3auiA.Ysei3.
      YQJGB.PlCAQ.S5YPU.uGEtI.wQrw1.cy8Sd.bFYuX.GGWZS.DSq1Y.O8ELq.
      cR6kA.dopig.RxB93.m6kAd.opigR.xB93m.6kAdo.pigRx.B93m6.kAdop.
      igRxB.93m6k.Adopi.gRxB9.3m6kA.doSsI.1Tves.7Fb5l.hneus.VDLsZ.
      ~P3DD.D~CpI.BALPA.rxSTT.fuXa4.gP3Yv.sPKOY.KSMKP.balqk.xjbEH.
      idkbq.Elo0N.dHjkM.vEBiq.BC-Rb.IKMiB.JiaoS.x3mLy.Jr6P5.ToiS2.
      gAz4y.qNHiI.k7WIl.9EJGb.iJ2Tp.NQ5H5.VpSni.By-OX.TfvYs.plRic.
      rpPJD.7xkgk.h9BMw.001EY.XFJDs.CYKpn.1xoTd.HrCAK.tTtT0.6lOon.
      tQpCZ.jt5x5.t1A00.01UCO.x20ts.d003n.3g00s.RB8s0.A0002.8p0xY.
      20w82.5h9gD.c4000.0l9ny.s0000.0o8p0.0006g.0001i.h4x93.g0000.
      Eq2wR.7jB29
    ==
  ::
  ++  huff                                              ::  request by ship
    |=  [our=ship cip=clip pul=purl moh=moth]
    =*  sec  p.p.pul
    =+  ^=  sef  ^-  serf
        =+  suf=(~(get by own) our)
        ?^  suf  u.suf
        =+  sef=*serf
        sef(pef (cat 3 gub (rsh 3 1 (scot %p our))))    ::  XX transitional
    =+  ^=  saw  ^-  [p=hole q=cyst]
        =+  lig=(coss pef.sef q.moh)
        ?^  lig
          =+  cyz=(need (~(get by wup.sef) u.lig))
          [u.lig cyz(cug ~)]
        =+  ses=(rsh 3 1 (scot %p (end 6 1 ney)))
        :-  ses
        ^-  cyst
        :*  ^-  cred
            :*  [sec q.p.pul r.p.pul]
                ~
                (rsh 3 1 (scot %p (end 6 1 (shaf %oryx ses))))
            ::
                =+  lag=(~(get by q.moh) %accept-language)
                ?~(lag ~ ?~(u.lag ~ [~ i.u.lag]))
            ::
                cip
                ~
            ==
        ::
            ~
        ::
            :_  ~
            %^  cat  3
              (cat 3 (cat 3 pef.sef '=') ses)
            ::  (cat 3 '; HttpOnly' ?.(sec '' '; Secure'))
            '; Path=/; HttpOnly'
        ::
            now
            ~
            [1 ~]
        ==
    abet:work:(~(into ya [our p.saw] sef q.saw) pul moh)
  ::
  ++  muff                                              ::  return card
    |=  gef=gift
    +>(mow :_(mow [hen %give gef]))
  ::
  ++  myth                                              ::  load session
    |=  [our=ship ses=hole]
    =+  suf=(~(get by own) our)
    ?~  suf  ~
    =+  cuz=(~(get by wup.u.suf) ses)
    ?~  cuz  ~
    [~ u=~(. ya [our ses] u.suf u.cuz)]
  ::
  ++  noon                                              ::  login page
    |=  [our=ship whu=(unit ship) rul=tape ruf=tape]
    ^-  manx
    =+  ^=  sic  ^-  manx
      ;script
        ;
        ; var seal = {
        ;   who: goal,
        ;   url: burl,
        ;   pas: null
        ; }
        ; var hist = []
        ; var hind = 0
        ; $(
        ;   function() {
        ;     $input = $('#input .line')
        ;     $prompt = $('#input .prompt')
        ;     $prompt.addClass('prefix')
        ;     $output = $('#output')
        ;     $input.focus()
        ;     $('body').click(function() { $input.focus() })
        ;     ctrl = false;
        ;
        ;     start = function(ship) {
        ;       $prompt.text('vessel: ~')
        ;       $input.attr('placeholder', 'ship-name')
        ;       if(ship) {
        ;         $input.val(ship)
        ;       }
        ;     }
        ;
        ;     ident = function() {
        ;       seal.who = $input.val()
        ;
        ;       if( (seal.who.length != 13) &&
        ;           (seal.who.length != 6) &&
        ;           (seal.who.length != 3) )
        ;       {
        ;         $output.text('not a ship name - try again.');
        ;         return false;
        ;       }
        ;
        ;       if(seal.who !== host) {
        ;         var foreign = {oth: host, ses: session};
        ;         var all = $.extend({}, seal, foreign);
        ;
        ;         console.log('redirect')
        ;         window.location="http://"+seal.who+".urbit.org/gul"
        ;                         + $.params(all);
        ;         return false;
        ;       }
        ;
        ;       $output.text($prompt.text() + " " + seal.who)
        ;       $input.val('')
        ;       $input.attr('placeholder', 'ronber-bacnub-hanmev-labnyd')
        ;       $prompt.text('secret: ~')
        ;
        ;       return true;
        ;     }
        ;
        ;     login = function() {
        ;       seal.pas = $input.val()
        ;
        ;       output = $output.html()
        ;       console.log($output.html())
        ;       $output.html(output.replace('sorry. please try again.<br>',''))
        ;
        ;       $.post(form, seal, function(data,xhr,status) {
        ;         console.log(data);
        ;         if(data.ok == true) {
        ;           document.location = data.next;
        ;         } else {
        ;           $output.prepend('sorry. please try again.<br>')
        ;         }
        ;       })
        ;     }
        ;
        ;     steps = [ident,login]
        ;     step = 0
        ;     start(seal.who)
        ;     if(seal.who) {
        ;       ident()
        ;       step++
        ;     }
        ;
        ;     $input.on('keydown', function(e) {
        ;       if(e.keyCode == 17) {
        ;         ctrl = true
        ;         return;
        ;       }
        ;
        ;       if(e.keyCode == 68 &&
        ;         ctrl == true &&
        ;         step == 1) {
        ;         $output.text('')
        ;         step = 0
        ;         start(null)
        ;         return;
        ;       }
        ;
        ;       if(e.keyCode == 13) {
        ;         if(steps[step]() && step < steps.length-1)
        ;           step++
        ;         return;
        ;       }
        ;     });
        ;
        ;     $input.on('keyup', function(e) {
        ;       if(e.keyCode == 17) {
        ;         ctrl = false
        ;       }
        ;     });
        ;   })
      ==
    =+  ^=  cof
      ;=
        ; var host = '{(trip (rsh 3 1 (scot %p our)))}';
        ; var goal = '{?~(whu ~ (trip (rsh 3 1 (scot %p u.whu))))}';
        ; var burl = '{rul}';
        ; var form = '{ruf}';
      ==
    =>  .(+.sic (weld `marl`cof `marl`+.sic))
    =+  jqu="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
    =+  ^=  hed
      ;head
        ;title: urbit login
        ;script(type "text/javascript", src jqu);
        ;style
          ; body {
          ;   margin: 60px 120px;
          ;   font: normal 12px "Menlo" monospace;
          ;   background-color: #000;
          ;   color: #fff;
          ; }
          ;
          ; #output {
          ;
          ; }
          ;
          ; #input .prompt {
          ;   display: inline-block;
          ;   margin-right: 12px;
          ; }
          ;
          ; #input .line {
          ;   outline: none;
          ;   width: 80%;
          ;   border: 0;
          ;   background-color: transparent;
          ;   color: #fff;
          ;   font: normal 12px "Menlo" monospace;
          ; }
        ==
      ==
    =+  ^=  bod  ^-  manx
      ;body
        ;div#output;
        ;div#input
          ;div.prompt;
          ;input.line(type "text");
        ==
      ==
    =.  +.bod  (weld `marl`+.bod `marl`~[sic])
    ;html
      ;+  hed
      ;+  bod
    ==
  ::
  ++  ya                                                ::  session engine
    =|  [[our=ship ses=hole] serf cyst]
    =*  sef  ->-
    =*  cyz  ->+
    |%
    ++  abet                                            ::  resolve engine
      ^+  ..ya
      %=    ..ya
          own
        (~(put by own) our sef(wup (~(put by wup) ses cyz)))
      ==
    ::
    ++  auth                                            ::  remote authorize
      |=  him=ship
      %_(+> aut.ced (~(put ju aut.ced) %$ (scot %p him)))
    ::
    ++  dodo                                            ::  console s->c
      |=  [con=@ud sih=sign]
      ^+  +>
      =+  cal=(need (~(get by cow) con))
      =+  ^=  jon  ^-  json
          ?:  ?=(%helo -.+.sih)
            %+  joba  %helo
            %-  jobe
            :~  [%path [%a `(list jval)`(turn p.+.sih |=(a=@ta [%s a]))]]
                [%prod ~[%a [%s p.q.+.sih] (jape q.q.+.sih) (jape r.q.+.sih)]]
            ==
          %+  joba  %text
          :-  %a  ^-  (list jval)
          ?+  -.+.sih  ~|(-.+.sih !!)
            %tell  (turn p.+.sih |=(a=@t [%s a]))
            %text  [%s (crip p.+.sih)]~
            %talk  (turn (~(win re p.+.sih) [0 80]) |=(a=tape [%s (crip a)]))
            %warn  [%s (crip '!' p.+.sih)]~
          ==
      =+  ^=  yov  ^-  (list ,@ud)
          =+  yov=(~(get by voy.cal) ono.cal)
          ?~(yov ~ u.yov)
      =:  voy.cal  (~(del by voy.cal) ono.cal)
          ono.cal  +(ono.cal)
          out.cal  [jon out.cal]
        ==
      =.  cow  (~(put by cow) con cal)
      ?~(yov +>.$ (dove ~[%a (jone ono.cal) (jone ino.cal) jon] yov))
    ::
    ++  iota                                            ::  fun change response
      |=  [num=@ud rot=riot]
      ^+  +>
      =+  pup=(~(get by q.rey) num)
      ?~  pup
        ~&  [%iota-lost ses num rot]
        +>.$
      ?>  ?=(%way pez.u.pup)
      +>.$(q.rey (~(put by q.rey) num u.pup(pez [%haz rot])))
    ::
    ++  dove                                            ::  console waiting
      |=  [jon=json yov=(list ,@ud)]
      ^+  +>
      =+  noz=`pest`[%fin %mid /text/json (tact (pojo jon))]
      |-  ^+  +>.^$
      ?~  yov  +>.^$
      =+  pup=(~(get by q.rey) i.yov)
      ?~  pup  $(yov t.yov)
      ?>  ?=(%way pez.u.pup)
      $(yov t.yov, q.rey (~(put by q.rey) i.yov u.pup(pez noz)))
    ::
    ++  duti                                            ::  heartbeat script
      ;script:'''
              var heart = {
                seqn: 0,
                trys: 0,
                dely: 10000,
              
                beat: function() {
                  var method, perm, url, $this
              
                  method = "put"
                  perm = "tih"
                  url = [perm,user,appl,port,heart.seqn]
                  url = "/"+url.join("/")
              
                  $this = this
              
                  var xhr = new XMLHttpRequest()
                  xhr.open(method.toUpperCase(), url)
                  xhr.setRequestHeader("content-type", "text/json")
                  xhr.send(JSON.stringify({oryx:oryx, xyro: {heart:"beat"}}))
                  xhr.onload = function () {
                    heart.seqn++
                    heart.trys = 0
                    setTimeout(heart.beat,heart.dely)
                  }
                  xhr.onerror = function() {
                    heart.trys++
                    setTimeout(heart.beat,heart.dely*heart.trys)
                  }
                }
              }
              heart.beat()
              '''
    ++  duty                                            ::  reload script
      ;script
        ; var tries = 0;
        ; var cnt = 0;
        ; var next = "/gie/"+user+"/"+appl+"/"+port+"/0/"+(cnt + 1);
        ; call = function() {
        ;   xhr = new XMLHttpRequest();
        ;   xhr.open('GET', next, true);
        ;   xhr.addEventListener('load', function() {
        ;     if ( this.status >= 500 ) {
        ;       return delay();
        ;     }
        ;     cnt++;
        ;     if ( this.status >= 400 ) {
        ;       document.alert("neighbor, please.");
        ;     }
        ;     document.location.reload();
        ;   });
        ;   xhr.addEventListener('error', delay);
        ;   xhr.addEventListener('abort', delay);
        ;   xhr.send();
        ; }
        ; delay = function() {
        ;   setTimeout(call,1000*tries);
        ;   tries++;
        ; }
        ; call();
      ==
    ::
    ++  fape                                            ::  dispatch %ape
      |=  [fur=(unit term) you=@p paw=path]
      ^-  (unit seam)
      ?>  ?=(~ fur)
      ?>  ?=([@ @ @ @ ~] paw)
      :-  ~
      :*  %ape
          (need ((sand %tas) i.paw))
          you
          (slav %ui (cat 3 '0i' i.t.paw))
          (slav %ui (cat 3 '0i' i.t.t.paw))
          (slav %ui (cat 3 '0i' i.t.t.t.paw))
      ==
    ::
    ++  fapg                                            ::  dispatch %apg
      |=  [fur=(unit term) you=@p paw=path]
      ^-  (unit seam)
      =+  for=?^(fur u.fur %html)
      ?>  ?=(^ paw)
      :-  ~
      :*  %apg
          (need ((sand %tas) i.paw))
          you
          for
          (turn t.paw |=(a=@ `@ta`(need ((sand %ta) a))))
      == 
    ::
    ++  faph                                            ::  dispatch %aph
      |=  [fur=(unit term) you=@p paw=path moh=moth]
      ^-  (unit seam)
      ?>  ?=(~ fur)
      ?>  ?=([@ @ @ ~] paw)
      :-  ~
      :*  %aph
          (need ((sand %tas) i.paw))
          you
          (slav %ui (cat 3 '0i' i.t.paw))
          (slav %ui (cat 3 '0i' i.t.t.paw))
          (need (ecci orx.ced moh))
      ==
    ::
    ++  fapm                                            ::  dispatch %apm
      |=  [fur=(unit term) you=@p paw=path moh=moth]
      ^-  (unit seam)
      ?>  ?=(~ fur)
      ?>  ?=([@ @ @ ~] paw)
      :-  ~
      :*  %apm
          (need ((sand %tas) i.paw))
          you
          (slav %ui (cat 3 '0i' i.t.paw))
          (slav %ui (cat 3 '0i' i.t.t.paw))
          (need (ecci orx.ced moh))
      ==
    ::
    ++  faps                                            ::  dispatch %aps
      |=  [fur=(unit term) you=@p paw=path moh=moth]
      ^-  (unit seam)
      ?>  ?=(~ fur)
      ?>  ?=([@ @ @ *] paw)
      ?>  !=(~ (ecci orx.ced moh))
      :-  ~
      :*  %aps
          (need ((sand %tas) i.paw))
          you
          (slav %ui (cat 3 '0i' i.t.paw))
          (slav %ui (cat 3 '0i' i.t.t.paw))
          (turn t.t.t.paw |=(a=@ `@ta`(need ((sand %ta) a))))
      ==
    ::
    ++  fapu                                            ::  dispatch %apu
      |=  [fur=(unit term) you=@p paw=path]
      ^-  (unit seam)
      ?>  ?=(~ fur)
      ?>  ?=([@ @ @ ~] paw)
      :-  ~
      :*  %apu
          (need ((sand %tas) i.paw))
          you
          (slav %ui (cat 3 '0i' i.t.paw))
          (slav %ui (cat 3 '0i' i.t.t.paw))
      ==
    ::
    ++  flea                                            ::  permissive decimal
      |=  txt=@t
      ^-  (unit ,@ud)
      =+  zac=(slay txt)
      ?:  ?=([~ %$ %ud @] zac)
        [~ q.p.u.zac]
      =+  soc=(rush txt dim:ag)
      ?~(soc ~ [~ u.soc])
    ::
    ++  foal                                            ::  url from query
      |=  [nam=@t yaq=(map ,@t ,@t)]
      ^-  (unit purl)
      =+  uru=(~(get by yaq) %url)
      ?~  uru  ~
      (rush u.uru auri:epur)
    ::
    ++  folk                                            ::  seal from query
      |=  quy=quay
      ^-  (unit seal)
      =+  yaq=(~(gas by *(map ,@t ,@t)) quy)
      =+  pyl=(foal %url yaq)
      =+  wit==(%yes (fall (~(get by yaq) %wit) %no))
      =+  huw=(~(get by yaq) %who)
      =+  whu=?~(huw ~ (rush u.huw fed:ag))
      =+  pus=(~(get by yaq) %pas)
      =+  tuh=(~(get by yaq) %oth)
      =+  thu=?~(tuh ~ (rush u.tuh fed:ag))
      =+  sus=(~(get by yaq) %ses)
      =+  foy=?~(sus ~ ?~(thu ~ [~ u.thu u.sus]))
      ?~  pyl  ~
      ?:  &(wit |(=(~ whu) =(~ ses)))  ~
      ::  ~&  [%folk-quay whu u.pyl wit foy pus]
      [~ whu u.pyl wit foy pus]
    ::
    ++  fool                                            ::  domestic login get
      |=  quy=quay
      ^-  (unit seam)
      (bind (folk quy) |=(a=seal [%lon a]))
    ::
    ++  foom                                            ::  domestic login post
      |=  moh=moth
      ^-  (unit seam)
      ?.  ?&  ?=(^ r.moh)
              ::  .=  [~ 'application/x-www-form-urlencoded' ~]
              ::  (~(get by q.moh) 'content-type')
          ==  ~
      =+  yuq=(rush q.u.r.moh yquy:epur)
      ?~(yuq ~ (fool u.yuq))
    ::
    ++  flub                                            ::  console request
      |=  [paw=(list ,@t) muh=(unit moth)]
      ^-  (unit seam)
      ?:  ?=([@ ~] paw)
        ?^  muh  ~
        =+  fee=(flea i.paw)
        ?~  fee  ~
        [~ %con u.fee]
      ?.  ?=([@ @ ~] paw)  ~
      =+  [fee=(flea i.paw) fum=(flea i.t.paw)]
      ?.  &(?=(^ fee) ?=(^ fum))  ~
      ?:  |(?=(~ muh) ?=(~ r.u.muh))
        [~ %cog u.fee u.fum]
      ?.  =([~ 'text/json' ~] (~(get by q.u.muh) 'content-type'))
        ~
      =+  jun=`(unit json)`(rush q.u.r.u.muh apex:poja)
      ?~  jun
        ~
      [~ %cop u.fee u.fum u.jun]
    ::
    ++  flux                                            ::  credential caboose
      |=  [nyp=path quy=quay]
      ^-  coin
      :*  %many
          [%many (turn nyp |=(a=span [%$ %ta a]))]
          [%blob ced]
          |-  ^-  (list coin)
          ?~  quy  ~
          [[%$ %t p.i.quy] [%$ %t q.i.quy] $(quy t.quy)]
      ==
    ::
    ++  foin                                            ::  version request
      |=  [fur=(unit term) paw=(list ,@t) quy=quay]
      ^-  (unit seam)
      ?.  ?&  ?=(~ fur)
              ?=(~ quy)
              ?=([@ @ ~] paw)
              ((sane %tas) i.t.paw)
          ==  ~
      %+  bind
        ^-  (unit moat)
        =+  soy=(slay i.paw)
        ?~  soy  ~
        ?+    u.soy
          ~
        ::
            [%$ ?(%da %ud %tas) @]
          [~ (case p.u.soy) (case p.u.soy)]
        ::
            [%many [%$ ?(%da %ud %tas) @] [%$ ?(%da %ud %tas) @] ~]
          [~ (case i.p.u.soy) (case i.t.p.u.soy)]
        ==
      |=  mot=moat
      `seam`[%det i.t.paw mot]
    ::
    ++  funk                                            ::  functional request
      |=  [nep=@tas fur=(unit term) paw=(list ,@t) quy=quay]
      ^-  (unit seam)
      =+  won==(%n (rsh 3 2 nep))
      %+  bind
        ^-  (unit ,[logo tube])
        =+  ^=  zac  ^-  (unit ,[p=@ta q=path])
            ?:  won
              [~ (scot %da now) paw]
            ?~  paw  ~
            =+  zac=(slay i.paw)
            ?.  ?=([~ %$ ?(%ud %da %tas) *] zac)  ~
            [~ i.paw t.paw]
        ?:  ?|  ?=(~ zac)
                ?=(~ q.u.zac)
                !(levy t.q.u.zac (sane %ta))
            ==  ~
        :+  ~  ?~(fur %html u.fur)
        ^-  tube
        :*  (scot %p our)
            i.q.u.zac
            p.u.zac
            t.q.u.zac
        ==
      |=  [for=logo toe=tube]
      ^-  seam
      :^  %fun  for
        toe(s (weld s.toe `path`[%web ~(rent co (flux [nep ~] quy)) ~]))
      ?.  won  ~
      :_  ~
      =-  =+  pey=(cat 3 (end 3 2 nep) %v)
          =+  ven=+((,@ (need (sky %cw p.toe q.toe r.toe ~))))
          =+  ^=  cal  :/
              "path='".
              "/{(trip pey)}".
              "/{(scow %ud ven)}".
              "/{(trip q.toe)}';"
          [-.sac [cal +.sac]]
      ^=  sac
      ;script
        ; 
        ; tries = 0;
        ; call = function() {
        ;   xhr = new XMLHttpRequest();
        ;   xhr.open('GET', path, true);
        ;   xhr.addEventListener('load', function() {
        ;     if(this.status !== 200) {
        ;       return keep();
        ;     }
        ;     document.location.reload();
        ;   });
        ;   xhr.addEventListener('error', keep);
        ;   xhr.addEventListener('abort', keep);
        ;   xhr.send();
        ; }
        ; keep = function() {
        ;   setTimeout(call,1000*tries);
        ;   tries++;
        ; }
        ; call();
      ==
    ::
    ++  holt                                            ::  login redirect
      |=  [whu=(unit ship) pul=purl]
      ^-  (unit seam)
      :+  ~
        %red
      ::  :+  [& q.p.pul r.p.pul]
      %+  earl  our
      :+  [p.p.pul q.p.pul r.p.pul]
        [~ /gul]
      :-  [%url (crip (urle (earn (earl our pul))))]
      ?~  whu  ~
      [%who (rsh 3 1 (scot %p u.whu))]~
    ::
    ++  holy                                            ::  structured request
      |=  [pul=purl moh=moth]
      ^-  (unit seam)
      ?:  &(=(%get p.moh) ?=([~ [@ ~]] q.pul))          ::  app shortcut
        ::  XX  use credential to figure out gog/gig
        $(q.pul [`%html [%gog i.q.q.pul ~]])
      ?~  q.q.pul  ~
      =*  nep  i.q.q.pul
      =+  paw=t.q.q.pul
      =+  [one=(end 3 1 nep) two=(cut 3 [1 1] nep) tri=(cut 3 [2 1] nep)]
      ?.  ?&  ?-  p.moh
                %conn  |                                ::  connect
                %delt  |                                ::  delete
                %get   =(%g one)                        ::  get
                %head  =(%g one)                        ::  head
                %opts  |                                ::  options
                %post  =(%p one)                        ::  post
                %put   =(%t one)                        ::  put
                %trac  |                                ::  trace
              ==
          ::
              ?+  two  |
                %e  &                                   ::  stranger
                %u  p.p.pul                             ::  guest
                %i  p.p.pul                             ::  neighbor
                %o  p.p.pul                             ::  identified
              ==
          ::
              ?=  $?  %g                                ::  app get
                      %c                                ::  console
                      %e                                ::  app update
                      %f                                ::  functional
                      %v                                ::  functional version
                      %l                                ::  local login
                      %m                                ::  app message
                      %r                                ::  app response
                      %s                                ::  app subscribe
                      %h                                ::  app heartbeat
                      %n                                ::  now
                      %u                                ::  app unsubscribe
                      %z                                ::  app version
                  ==
                  tri
          ::
              !&(=(%c tri) !=(%o two))
              =(3 (met 3 nep))
          ==
        ~
      =^  yun  paw
          ?+  two  ~
            ?(%e %u)  [`@`(shaf %fake ses) paw]
            %i        ?~  paw  ~ 
                      [(slav %p (cat 3 '~' i.paw)) t.paw]
            %o        [our paw]
          ==
      ::  ?:  &(=(%i two) =(~ aut.ced))
      ::    (holt ~ pul)
      ::  ?:  &(=(%o two) !(~(has ju aut.ced) %$ (scot %p our)))
      ::    (holt [~ our] pul)
      ?+    one  ~
          %g
        ?+  tri  ~
          ?(%f %n)  (funk nep p.q.pul paw r.pul)
          %v        (foin p.q.pul paw r.pul)
          %c        (flub paw ~)
          %l        (fool r.pul)
          %g        (fapg p.q.pul yun paw)
          %e        (fape p.q.pul yun paw)
        ==
      ::
          %p
        ?+  tri  ~
          %l  (foom moh)
          %m  (fapm p.q.pul yun paw moh)
          %s  (faps p.q.pul yun paw moh)
          %u  (fapu p.q.pul yun paw)
        ==
      ::
          %t
        ?+  tri  ~
          %c  (flub paw [~ moh])
          %h  (faph p.q.pul yun paw moh)
          %m  (fapm p.q.pul yun paw moh)
          %s  (faps p.q.pul yun paw moh)
          %u  (fapu p.q.pul yun paw)
        ==
      ==
    ::
    ++  idle                                            ::  cancel request
      |=  num=@ud
      ^+  +>
      =+  pup=(~(get by q.rey) num)
      ?~  pup  +>.$
      =.  q.rey  (~(del by q.rey) num)
      ?.  ?=(%way pez.u.pup)  +>.$
      ?:  ?=(%det -.som.u.pup)
        +>.$(..ya (hoot our num ses `riff`[p.som.u.pup ~]))
      ?:  ?=(%fun -.som.u.pup)
        +>.$(..ya (hone our num ses))
      +>.$
    ::
    ++  bush                                            ::  error response
      |=  [cod=@ud msg=@t num=@ud]
      ^+  +>
      =+  pup=(~(get by q.rey) num)
      ?~  pup  +>.$
      %=    +>.$
          q.rey
        %+  ~(put by q.rey)  num
        %=    u.pup
            pez
          `pest`[%fin %raw cod ~[content-type/'text/plain'] `(taco msg)]
        ==
      ==
    ::
    ++  bust                                            ::  no-content response
      |=  [cod=@ud num=@ud]
      ^+  +>
      =+  pup=(~(get by q.rey) num)
      ?~  pup  +>.$
      ::  ?>  ?=(%way pez.u.pup)
      %=    +>.$
          q.rey
        %+  ~(put by q.rey)  num
        u.pup(pez [%fin %raw cod *mess `*octs])
      ==
    ::
    ++  inch                                            ::  function built
      |=  [num=@ud mez=(each bead (list tank))]
      ^+  +>
      =+  pup=(~(get by q.rey) num)
      ?~  pup
        ~&  [%inch-lost ses num mez]
        +>.$
      ?>  ?=(%way pez.u.pup)
      +>.$(q.rey (~(put by q.rey) num u.pup(pez [%fud mez])))
    ::
    ++  into                                            ::  introduce
      |=  [pul=purl moh=moth]
      ^+  +>
      =+  num=p.rey
      %=    +>.$
          lor    (~(put by lor) hen num [%& our ses])
          p.rey  +(num)
          q.rey
        %+  ~(put by q.rey)  num
        ^-  pimp
        :*  !?=(%head p.moh)
            p.q.pul
            hen
            *seam
            `pest`[%raw pul moh]
            ~
            ~
        ==
      ==
    ::
    ++  iota                                            ::  change response
      |=  [num=@ud rot=riot]
      ^+  +>
      =+  pup=(~(get by q.rey) num)
      ?~  pup
        ~&  [%iota-lost ses num rot]
        +>.$
      ?>  ?=(%way pez.u.pup)
      +>.$(q.rey (~(put by q.rey) num u.pup(pez [%haz rot])))
    ::
    ++  lass                                            ::  load secret
      ^-  @ta
      %^  rsh  3  1
      (scot %p (,@ (need (sky %a (scot %p our) %code (scot %da now) ~))))
    ::
    ++  step                                            ::  step in work
      |-  ^+  +
      =^  zib  +.$
          =+  yub=q.rey
          |-  ^-  [(list ,[p=@ud q=pimp]) _+.^$]
          ?~  yub  [~ +.^$]
          =^  sid  +.^$  $(yub l.yub)
          =^  dex  +.^$  $(yub r.yub)
          =^  top  +.^$  (wink n.yub)
          =+  pot=`(list ,[p=@ud q=pimp])`?~(top ~ [[p.n.yub u.top] ~])
          [:(weld pot dex sid) +.^$]
      +.$(q.rey (~(gas by `_q.rey`~) zib))
    ::
    ++  wink                                            ::  advance request
      |=  [num=@ud pip=pimp]
      ^-  [(unit pimp) _+>]
      ?-    pez.pip
          %way  [[~ pip] +>.$]
          %new
        ?-    -.som.pip
            %ape                                        ::  stream update
          ::  ~&  :~  %eyre-ape
          ::          [%owner our]
          ::          [%requester num]
          ::          [%app p.som.pip]
          ::          [%user q.som.pip]
          ::          [%instance r.som.pip]
          ::          [%stream s.som.pip]
          ::         [%request t.som.pip]
          ::    ==
          :-  [~ pip(pez %way)]
          (yoke num +.som.pip)
        ::
            %apg                                        ::  simple get
          ::  ~&  :~  %eyre-apg
          ::          [%owner our]
          ::          [%requester num]
          ::          [%app p.som.pip]
          ::          [%user q.som.pip]
          ::          [%logo r.som.pip]
          ::          [%path s.som.pip]
          ::      ==
          :-  [~ pip(pez %way)]
          (yokg num p.som.pip q.som.pip s.som.pip)
        ::
            %aph                                        ::  heartbeat
          ~&  [%wink-aph +.som.pip]
          :-  [~ pip(pez %way)]
          (yokh num +.som.pip)
        ::
            %apm                                        ::  message 
          ::  ~&  [%wink-apm +.som.pip]
          :-  [~ pip(pez %way)]
          (yokm num +.som.pip)
        ::
            %aps                                        ::  subscribe
          ::  ~&  [%wink-aps +.som.pip]
          :-  [~ pip(pez %way)]
          (yoks num +.som.pip)
        ::
            %apu                                        ::  unsubscribe
          ::  ~&  [%wink-apu +.som.pip]
          :-  [~ pip(pez %way)]
          (yoku num +.som.pip)
        ::
            %con
          :_  +>.$
          =+  cal==+(cal=(~(get by cow) p.som.pip) ?^(cal u.cal *clue))
          =+  ^=  obj
              %-  jobe
              :~  sent/(jone ino.cal)
                  recv/(jone ono.cal)
                  ownr/[%s (rsh 3 1 (scot %p our))]
              ==
          =+  sez=:/("seq={(pojo obj)}")
          =+  jqu="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
          =+  ^=  sac
              ;script
                ; 
                ; var hist = []
                ; var hind = 0
                ; $(
                ;   function() {
                ;     $input = $('#input .line')
                ;     $prompt = $('#input .prompt')
                ;     $output = $('#output')
                ;     $input.focus()
                ;
                ;     path = document.location.pathname
                ;     if(path.slice(-1) == "/")
                ;       path = path.slice(0,-1)
                ;     seq.prom = path.split("/").pop()
                ;
                ;     $('body').click(function() {
                ;       $input.focus()
                ;     })
                ;
                ;     send = function(com) {
                ;       if(com.line !== undefined &&
                ;         com.line.indexOf('error') != -1) {
                ;         com = {error:true}
                ;       }
                ;       console.log('sending')
                ;       console.log(com)
                ;       $.ajax('/'+seq.ownr+'/toc/'+seq.prom+'/'+seq.sent, {
                ;         type: 'PUT',
                ;         contentType: 'text/json',
                ;         data: JSON.stringify(com),
                ;         success: function(data,status,xhr) {
                ;           seq.sent++
                ;         },
                ;         error: function(data,status,xhr) {
                ;           if(data.responseJSON.lines !== undefined)
                ;             addLines(data.responseJSON.lines)
                ;           seq.sent++
                ;         }
                ;       })
                ;     }
                ;
                ;     recv = function() {
                ;       $.ajax('/'+seq.ownr+'/goc/'+seq.prom+'/'+seq.recv, {
                ;         type:'GET',
                ;         success: function(data,status,xhr) {
                ;           console.log(data);
                ;           seq.recv = data[0];
                ;           seq.send = data[1];
                ;           msg = data[2];
                ;           if(msg.text !== undefined) {
                ;             addLines(msg.text)
                ;             $('body').scrollTop($('.line').position().top)
                ;           }
                ;           if(msg.helo !== undefined)
                ;             changePrompt(msg.helo)
                ;           recv()
                ;         },
                ;         error: function(data,status,xhr) {
                ;           console.log('error')
                ;           seq.recv++
                ;         }
                ;       })
                ;     }
                ;
                ;     recv()
                ;
                ;     send({hail:true})
                ;
                ;     addLines = function(lines) {
                ;       $output.append(lines.join("<br />")+"<br />")
                ;     }
                ;
                ;     changePrompt = function(helo) {
                ;       $prompt.text(helo.prod[1]);
                ;     }
                ;
                ;     ctrl = false
                ;
                ;     $input.on('keydown', function(e) {
                ;       console.log('keydown')
                ;       console.log(e.keyCode)
                ;       if(e.keyCode == 17) {
                ;         ctrl = true
                ;         return;
                ;       }
                ;       if(e.keyCode == 88 && ctrl == true) {
                ;         console.log('ling')
                ;         send({ling:true})
                ;         return;
                ;       }
                ;
                ;       if(e.keyCode == 69 && ctrl == true) {
                ;         console.log('^e')
                ;         $input[0].selectionStart =
                ;             $input[0].selectionEnd =
                ;             $input.val().length;
                ;         return
                ;       }
                ;       if(e.keyCode == 65 && ctrl == true) {
                ;         console.log('^a')
                ;         $input[0].selectionStart = $input[0].selectionEnd = 0
                ;         return
                ;       }
                ;
                ;
                ;       if(e.keyCode == 40) {
                ;         if(hist.length > 1) {
                ;           hind++
                ;           if(hind > hist.length-1) {
                ;             hind = hist.length-1
                ;           }
                ;         }
                ;       }
                ;       if(e.keyCode == 38) {
                ;         if(hist.length > 1) {
                ;           hind--
                ;           if(hind < 0) {
                ;             hind = 0
                ;           }
                ;         }
                ;       }
                ;       if(e.keyCode == 38 ||
                ;       e.keyCode == 40) {
                ;         console.log('set from hist')
                ;         $input.val(hist[hind])
                ;         setTimeout(function() {
                ;           console.log(hist[hind].length)
                ;           $input[0].setSelectionRange
                ;               (hist[hind].length,hist[hind].length)
                ;         }, 0)
                ;         return;
                ;       }
                ;
                ;       if(e.keyCode == 13) {
                ;         val = $input.val()
                ;         send({line:val})
                ;         $output.append($('.prompt').text()+" "+val+"<br>")
                ;         hind = hist.length-1
                ;         if(hind<0)
                ;           hind = 0
                ;         hist[hind] = val
                ;         hist.push('')
                ;         hind = hist.length-1
                ;         $input.val('')
                ;         $('body').scrollTop($('.line').position().top)
                ;         return;
                ;       }
                ;       if(hind == hist.length-1)
                ;         hist[hind] = $input.val()
                ;     });
                ;
                ;     $input.on('keyup', function(e) {
                ;       if(e.keyCode == 17) {
                ;         ctrl = false
                ;       }
                ;     });
                ;   }
                ; )
              ==
          =+  ^=  ham
              ;html
                ;head
                  ;title: urbit {<our>}/{<p.som.pip>}
                  ;script(type "text/javascript", src jqu);
                  ;style
                    ; body {
                    ;   margin: 60px 120px;
                    ;   font: normal 12px "Menlo" monospace;
                    ;   background-color: #000;
                    ;   color: #fff;
                    ; }
                    ;
                    ; #output {
                    ;   line-height: 18px;
                    ;   white-space: pre;
                    ; }
                    ;
                    ; #input .prompt {
                    ;   display: inline-block;
                    ;   margin-right: 12px;
                    ; }
                    ;
                    ; #input .line {
                    ;   outline: none;
                    ;   width: 80%;
                    ;   border: 0;
                    ;   background-color: transparent;
                    ;   color: #fff;
                    ;   font: normal 12px "Menlo" monospace;
                    ; }
                  ==
                ==
                ;body
                  ;div#output;
                  ;div#input
                    ;div.prompt;
                    ;input.line(type "text");
                  ==
                  ;+  [-.sac `marl`[sez +.sac]]
                ==
              ==
          [~ pip(pez [%fin %ham ham])]
        ::
            %cog
          =+  cal==+(cal=(~(get by cow) p.som.pip) ?^(cal u.cal *clue))
          ?.  (lth q.som.pip ono.cal)
            :-  [~ pip(pez %way)]
            %=  +>.$  cow
              %+  ~(put by cow)
                p.som.pip
              =+  val=(~(get by voy.cal) q.som.pip)
              cal(voy (~(put by voy.cal) q.som.pip ?~(val [num ~] [num u.val])))
            ==
          :_  +>.$
          =+  ^=  jon  ^-  json
              :~  %a
                (jone ono.cal)
                (jone ino.cal)
                (snag (sub ono.cal q.som.pip) out.cal)
              ==
          [~ pip(pez [%fin %mid /text/json (tact (pojo jon))])]
        ::
            %cop
          =+  cal==+(cal=(~(get by cow) p.som.pip) ?^(cal u.cal *clue))
          ?.  =(q.som.pip ino.cal)
            =.  cow  (~(put by cow) p.som.pip cal)
            :_  +>.$
            [~ pip(pez [%err 500 [%leaf "cop: {<q.som.pip>}, {<ino.cal>}."]~])]
          =+  ^=  fuv  ^-  (unit note)
              ?.  ?=(%o -.r.som.pip)  ~
              =+  lin=(~(get by p.r.som.pip) %line)
              ?^  lin  ?.(?=(%s -.u.lin) ~ [~ %b %line p.u.lin])
              =+  syc=(~(get by p.r.som.pip) %hail)
              ?^  syc  [~ %b %hail ~]
              =+  lig=(~(get by p.r.som.pip) %ling)
              ?^  lig  [~ %b %ling ~]
              ~
          :_  %_    +>.$
                  cow  (~(put by cow) p.som.pip cal(ino +(ino.cal)))
                  mow
                ?~  fuv  mow
                :_  mow
                :-  hen
                :+  %pass  [%cons (scot %p our) ses (scot %ud p.som.pip) ~]
                u.fuv
              ==
          [~ `pimp`pip(pez `pest`[%fin %raw 200 ~ ~])]
        ::
            %det
          :-  [~ pip(pez %way)]
          +>.$(..ya (hoot our num ses [p.som.pip ~ [%| q.som.pip]]))
        ::
            %fun
          :-  [~ pip(pez %way)]
          =+  bem=`beam`(need (tome q.som.pip))
          =+  bek=`beak`[p.bem q.bem r.bem]
          =+  kas=`silk`[%cast %mime bek `silk`[%boil p.som.pip bem ~]]
          +>.$(..ya (honk our num ses kas))
        ::
            %lon
          ?^  pus.p.som.pip
            ?.  =(lass u.pus.p.som.pip)
              :_  +>.$
              :-  ~
              %=    pip
                  pez
                [%fin %mid /text/json (tact (pojo (jobe [%ok [%b |]]~)))]
              ==
            =.  aut.ced  (~(put ju aut.ced) %$ (scot %p (need whu.p.som.pip)))
            ?~  foy.p.som.pip
              =+  ^=  jon
                %-  jobe
                :~  [%ok [%b &]]
                    [%next (jape (earn pul.p.som.pip))]
                ==
              :_(+>.$ [~ pip(pez [%fin %mid /text/json (tact (pojo jon))])])
            =.  ..ya   (haji our u.foy.p.som.pip)
            =+  ^=  lup  ^-  purl
              :+  ^-  hart
                  :+  &   ~
                  [%& ~[%org %urbit (rsh 3 1 (scot %p p.u.foy.p.som.pip))]]
                ^-  pork
                [~ /gul]
              ^-  quay
              :~  [%who (rsh 3 1 (scot %p (need whu.p.som.pip)))]
                  [%url (crip (earn pul.p.som.pip))]
                  [%wit %yes]
              ==
            =+  jon=(jobe ~[[%ok [%b &]] [%next (jape (earn lup))]])
            :_(+>.$ [~ pip(pez [%fin %mid /text/json (tact (pojo jon))])])
          :_  +>.$
          ?:  wit.p.som.pip
            [~ pip(pez [%who %$ (scot %p (need whu.p.som.pip))])]
          =+  rul=(earn pul.p.som.pip)
          =+  ruf=(earn (earl our pul.p.som.pip(q.q /pul, r ~)))
          =+  ham=(noon our whu.p.som.pip rul ruf)
          [~ pip(pez [%fin %ham ham])]
        ::
            %red
          :_  +>.$
          :-  ~
          %=    pip
              pez
            :-  %fin
            :-  %raw
            :+  301
              [%location (crip (earn p.som.pip))]~
            ~
          ==
        ::
            %sil
          :-  [~ pip(pez %way)]
          +>.$(..ya (honk our num ses q.som.pip))
        ==
      ::
          [%err *]
        [~ +>.$(..ya (muff(hen hen.pip) [%thou (loft `love`[%zap +.pez.pip])]))]
      ::
          [%fin *]
        =+  har=(loft p.pez.pip)
        =.  q.har  (weld (turn cug |=(a=@t ['set-cookie' a])) q.har)
        [~ +>.$(..ya (muff(hen hen.pip) [%thou har]))]
      ::
          [%haz *]
        :_  +>.$
        [~ pip(pez [%fin %wan 'Hello, world' ~])]
      ::
          [%fud *]
        =+  ^=  mog  ^-  (list manx)
            ?:  ?=(%fun -.som.pip)  
              (weld r.som.pip sip.pip)
            sip.pip 
        :_  +>.$
        :-  ~
        %=    pip
            pez
          ^-  pest
          ?-  -.p.pez.pip
            |  =+  mad=(loga "server error" mog p.p.pez.pip)
               :-  %fin
               :-  %raw
               ^-  httr
               :+  500
                 ~[content-type/'text/html']
               [~ (tact (xmlt | mad ~))]
            &  [%fin (lofe vaz.pip (lofi mog (lopo q.p.p.pez.pip)))]
          ==
        ==
      ::
          [%raw *]
        :_  +>.$
        ^-  (unit pimp)
        :-  ~
        =+  hoy=(holy p.pez.pip)
        ?~  hoy
          pip(pez [%err 404 [[%leaf "invalid request"] ~]])
        pip(som u.hoy, pez %new)
      ::
          [%who *]
        :_  +>.$
        ?.((~(has ju aut.ced) p.pez.pip q.pez.pip) [~ pip] [~ pip(pez %new)])
      ==
    ::
    ++  work
      |-  ^+  +
      =+  sez=step
      ?:  =(rey.sez rey)  sez
      $(+ sez)
    ::
    ++  yoke                                            ::  long poll
      |=  [num=@ud app=term you=ship nap=@ud suq=@ud cnt=@ud]     
      ^+  +>
      =+  yon=(yolk nap) 
      ?~  yon  (bust 204 num)
      abet:(hire:(yule:u.yon suq) cnt num)
    ::
    ++  yokg                                            ::  main call
      |=  [num=@ud app=term you=ship pax=path]
      ^+  +>
      ?<  (~(has by sok) num)
      abet:(~(self yo num app you [[*@ud now] *[@ud (map ,@ud swig)]]) pax)
    ::
    ++  yokh                                            ::  heartbeat
      |=  [num=@ud app=term you=ship nap=@ud cnt=@ud jon=json]
      ^+  +>
      =+  yon=(yolk nap)
      ~&  [%yokh-1 cnt]
      ?~  yon  (bust 204 num)
      ~&  [%yokh-2 cnt]
      abet:(beat:u.yon cnt num jon)
    ::
    ++  yokm                                            ::  message
      |=  [num=@ud app=term you=ship nap=@ud cnt=@ud jon=json]
      ^+  +>
      =+  yon=(yolk nap)
      ?~  yon  (bust 204 num)
      abet:(post:u.yon cnt num jon)
    ::
    ++  yoks                                            ::  subscribe
      |=  [num=@ud app=term you=ship nap=@ud suq=@ud pax=path]
      =+  yon=(yolk nap)
      ?~  yon  (bust 204 num)
      abet:(scud:u.yon suq num pax)
    ::
    ++  yoku                                            ::  unsubscribe
      |=  [num=@ud app=term you=ship nap=@ud suq=@ud]
      !!
    :: 
    ++  yolk                                            ::  yo by instance
      |=  nap=@ud
      =+  suy=(~(get by sok) nap)
      ?~  suy  ~
      (some ~(. yo nap u.suy))
    ::
    ++  yo                                              ::  app instance
      |_  $:  nap=@ud                                   ::  instance number
              app=term                                  ::  application name
              you=ship                                  ::  client identity
              siq=sink                                  ::  instance state
          ==
      ++  abet                                          ::  resolve
        %_  ..yo
          sok  (~(put by sok) nap [app you siq])
        ==
      ::
      ++  amok                                          ::  demolish
        ^+  ..yo
        ~&  %amok-time
        =+  wuh=(~(tap by haw.siq) ~)
        |-  ^+  ..yo
        ?~  wuh  
          %=  ..yo
            sok  (~(del by sok) nap)
          ==
        $(wuh t.wuh, ..amok (pass p.i.wuh `note`[%g %nuke [our app] you]))
      ::
      ++  beat
        |=  [cnt=@ud num=@ud jon=json]
        ^+  +>
        ?.  =(cnt num.bet.siq)
          +>.$(..yo (bust 204 num))
        =.  +>.$
          ?.  =(cnt num.bet.siq)  +>.$
          %+  pass(bet.siq [+(num.bet.siq) now])
            2
          `note`[%e %clug our ses nap]
        ~&  [%beat cnt num jon]
        (hire:(yule 2) cnt num)
      ::
      ++  hoop                                          ::  request path
        |=  suq=@ud
        ^-  path
        :~  %hoop
            (scot %p our)
            ses
            (scot %ud nap)
            (scot %ud suq)
        ==
      ::
      ++  pass                                          ::  pass 
        |=  [suq=@ud noh=note]
        ^+  +>
        +>(mow [[hen %pass (hoop suq) noh] mow])
      ::
      ++  post                                          ::  transmit
        |=  [cnt=@ud num=@ud jon=json]
        ^+  +>
        =.  +>.$  
          ?.  =(cnt meg.siq)  +>.$
          %+  pass(meg.siq +(meg.siq))  
            1
          `note`[%g %mess [our app] you [%json !>(jon)]]
        ?.  =(+(cnt) meg.siq)
          +>.$(..yo (bust 204 num))
        (hire:(yule 1) cnt num)
      ::
      ++  scud                                          ::  subscribe
        |=  [suq=@ud num=@ud pax=path]
        =.  +>.$  (pass suq `note`[%g %show [our app] you pax])
        (hire:(yule suq) 0 num)
      ::
      ++  self                                          ::  request main
        |=  pax=path
        ^+  +>
        (hire:(yule:(pass 0 [%g %show [our app] you pax]) 0) 0 nap)
      ::
      ++  yule                                          ::  swig state
        |=  suq=@ud
        ~(. yu suq =+(wig=(~(get by haw.siq) suq) ?~(wig *swig u.wig)))
      ::
      ++  yu                                            ::  swig state
        |_  [suq=@ud wig=swig]
        ++  abet                                        ::  resolve
          %_(..yu haw.siq (~(put by haw.siq) suq wig))
        ::
        ++  amok
          %_(..yu haw.siq (~(del by haw.siq) suq))
        ::
        ++  busk                                        ::  seam result
          |=  $:  num=@ud 
                  cnt=@ud
                  huq=(unit (each cage cage))
              ==
          ^+  +>
          =+  pup=(~(get by q.rey) num)
          ?~  pup  +>.$   ::  XX a hack
          =.  wig  ?.  =(toy.wig cnt)  
                     ?>(=(toy.wig +(cnt)) wig)
                   %=    wig
                       toy  +(toy.wig)
                       red  
                     ?:  =(0 toy.wig)  red.wig
                     :: ~&  [%busk [%sent cnt] [%lost (dec toy.wig)]]
                     (~(del by red.wig) (dec toy.wig))
                   ==

          ::  =+  pip=(need (~(get by q.rey) num))
          =+  pip=u.pup
          =+  ^=  sip
              ?.  =(%apg -.som.pip)  sip.pip
              [duti duty sip.pip]
          ?~  huq  +>.$(..yo (bust 404 num))
          %=    +>.$
              q.rey
            %+  ~(put by q.rey)  num
            ^-  pimp
            =+  quo=|=(a=cord :(weld "\"" (trip a) "\""))
            %=    pip
                pez  %new
                vaz  :~  [%ship (quo (rsh 3 1 (scot %p our)))]
                         [%appl (quo app)]
                         [%port (trip (rsh 3 2 (scot %ui nap)))]
                         [%auto "true"]
                         [%oryx (quo orx.ced)]
                         [%user (quo (rsh 3 1 (scot %p our)))]
                     ==
                sip  sip
                som  ^-  seam
                     ~&  [%busk-realz suq]
                     :+  %sil 
                       ?:(-.u.huq 200 203) 
                     =+  bek=`beak`[our %main [%da now]]
                     =+  don=`silk`[%done ~ `cage`p.u.huq]
                     ^-  silk
                     :^  %cast  %mime  bek
                     ?~  fur.pip  don
                     `silk`[%cast u.fur.pip bek don]
            ==
          ==
        ::
        ++  dumb                                        ::  reset
          ^+  ..yu
          =+  dum=(~(tap by wan.wig) ~)
          |-  ^+  ..yu
          ?~  dum  amok
          $(dum t.dum, ..yo (bust 404 q.i.dum))
        ::
        ++  hear                                        ::  produce
          |=  huq=(unit (each cage cage))
          ^+  ..yu
          =<  abet 
          =+  cnt=cnt.wig
          =+  dul=(~(get by wan.wig) cnt)
          ~&  :~  %yu-hear 
                  [%instance nap]
                  [%produced cnt]
                  ?~(dul %unrequested [%requester u.dul])
              ==
          =:  cnt.wig  +(cnt.wig)
              wan.wig  ?~(dul wan.wig (~(del by wan.wig) cnt.wig))
              red.wig  (~(put by red.wig) cnt.wig huq)
            ==
          ?~(dul +>.$ (busk u.dul cnt huq))
        ::
        ++  hire                                        ::  consume
          |=  [cnt=@ud num=@ud]
          ^+  ..yu
          =<  abet
          ::  ~&  :~  %yu-hire 
          ::          [%instance nap]
          ::          [%produced cnt.wig] 
          ::          [%request cnt]
          ::          [%dispatched toy.wig]
          ::          [%requester num] 
          ::      ==
          ?:  |((lth +(cnt) toy.wig) (gth cnt toy.wig))
            ~&  [%hire-improper [%request cnt] [%dispatched toy.wig]]
            +>.$(..yo (bust 204 num))
          ?:  (gte cnt cnt.wig)
            ::  ~&  %hire-wait
            =+  old=(~(get by wan.wig) cnt)
            =.  wan.wig  (~(put by wan.wig) cnt num)
            +>.$(..yo ?~(old ..yo (bust 204 u.old)))
          =+  rud=(~(get by red.wig) cnt)
          ?~  rud
            ::  ~&  %hire-bust
            +>.$(..yo (bust 204 num)) 
          ::  ~&  %hire-send
          (busk num cnt u.rud)
        --
      --
    --
  --
--
.  ==
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
  =+  ska=(slod ski)
  =+  sky=|=(* `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ^-  [p=(list move) q=_..^$]
  =.  gub  ?.(=(0 gub) gub (cat 3 (rsh 3 1 (scot %p (end 6 1 eny))) '-'))
  =^  mos  bol
    abet:(apex:~(adit ye [hen [now eny sky] ~] bol) q.hic)
  [mos ..^$]
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load
  |=  old=bolo
  ^+  ..^$
  ..^$(+>- old)
::
++  scry
  |=  [our=(unit (set monk)) ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair logo ,*)))
  ~
::
++  stay  `bolo`+>-.$
++  take                                                ::  accept response
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^-  [p=(list move) q=_..^$]
  =+  ska=(slod ski)
  =+  sky=|=(* `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ^-  [p=(list move) q=_..^$]
  =.  gub  ?.(=(0 gub) gub (cat 3 (rsh 3 1 (scot %p (end 6 1 eny))) '-'))
  =^  mos  bol
    =<  abet
    %^  axon:~(adit ye [hen [now eny sky] ~] bol)  tea 
      (~(peek ut p.hin) %free 3) 
    q.hin
  [mos ..^$]
--
