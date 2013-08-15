!:
::          %bede, the shell.  This file is in the public domain.
::
=<
  |%
  ++  bede                                              ::  terminal handling
    ^-  vane                                            ::  kernel instrument
    =|                                                  ::  system state
        $:  deb=(map flag _*berg)                       ::  state by terminal
        ==                                              ::
    |=  [now=@da eny=@ sky=||(* (unit))]                ::  current invocation
    ^?                                                  ::  opaque core
    |%                                                  ::  poke/peek pattern
    ++  beat                                            ::  process move
      |=  [whu=(unit flag) tea=tire hen=hose fav=card]
      ^-  [p=(list move) q=vane]
      ?>  ?=(^ whu)
      =+  ^=  beg  ^+  *berg
          =+  beg=(~(get by deb) u.whu)
          ?^(beg u.beg (berg u.whu))
      =+  yub=(leap:(beg now (shax now) sky) tea hen fav)
      :-  p.yub
      ..^$(deb (~(put by deb) u.whu q.yub))
    ::
    ++  scry
      |=  [our=flag ren=@tas his=flag lot=coin tyl=path]
      ^-  (unit)
      ~
    --
  --
|%
++  berg                                                ::  repl/shell
  |=  who=flag                                          ::  per identity
  =+  nub=`vase`!>(+>)                                  ::  system + libraries
  =+  :*  ^=  vax                                       ::  chestnut vases
            :*  sot=(slap nub (vice 'slot'))            ::
            ==                                          ::
          ^=  gen                                       ::  chestnut genes
            :*  yom=(vice '*(set ,@tas)')               ::
                zim=(vice '*(map ,@tas ,*)')            ::
            ==                                          ::
          ^=  typ                                       ::  chestnut types
            =+  pal=~(play ut p.nub)                    ::
            :*  cof=(pal (vice '*conf'))                ::
                gee=(pal (vice '*gene'))                ::
                liz=(pal (vice '*(list ,@)'))           ::
                pah=(pal (vice '*path'))                ::
                noq=(pal (vice '*note'))                ::
                tak=(pal (vice '*task'))                ::
                vas=(pal (vice '*vase'))                ::
            ==                                          ::
          hub=(hoof stub)                               ::  stage extension
      ==                                                ::
  =+  :*  hit=[p=0 q=*(list tape)]                      ::  source history
          sur=[p=0 q=*(qeu vase)]                       ::  result history
          hox=(scot %p who)                             ::  identity string
          cwd=*path                                     ::  working directory
          way=*(map ,@ta vase)                          ::  variables
          pak=*(list ,[p=@ud q=path])                   ::  prompt stack
          lif=*dock                                     ::  processes
      ==
  |=  [now=@da eny=@ sky=_|+(* *(unit))] 
  =+  wen=(scot %da now)
  =+  wer=`path`[%cx wen hox (weld cwd [hub ~])]
  =+  rew=(flop wer)
  =+  vez=(vang & wer)
  |%
  ++  do                                                ::  userspace code
    |%
    ++  dive                                            ::  opts to gene
      |=  cux=cone
      ^-  gene
      :-  :+  %cncl
            [%cnsg [%gas ~] [%cnbc %in] yom.gen]
          [%clsg (turn p.cux |=(a=@ [%dtpt %tas a]))]
      :+  %cncl
        [%cnsg [%gas ~] [%cnbc %by] zim.gen]
      :-  %clsg
      %+  turn  q.cux
      |=([p=@tas q=crow] [[%dtpt %tas p] (doul q)])
    ::
    ++  doul                                            ::  crow to gene
      |=  woc=crow
      ^-  gene
      ?-    -.woc
          %c
        [%cnhp $(woc p.woc) $(woc [%p q.woc]) ~]
      ::
          %f
        %+  scan  (trip ((hard ,@) .^(p.woc)))
        (full (ifix [gay gay] tall:vez(wer p.woc)))
      ::
          %g
        |-  ^-  gene
        ?~  p.woc
          q.woc
        [%tsgr ^$(woc [%f i.p.woc]) $(p.woc t.p.woc)]
      ::
          %l
        |-  ^-  gene
        ?~  p.woc
          [%bcts %null]
        [^$(woc i.p.woc) $(p.woc t.p.woc)]
      ::
          %p
        |-  ^-  gene
        ?~  p.woc  !!
        ?~  t.p.woc
          ^$(woc i.p.woc)
        [^$(woc i.p.woc) $(p.woc t.p.woc)]
      ==
    ::
    ++  echo                                            ::  echo argument
      |=  [yun=vase woc=crow]
      ^-  tank
      =+  vax=(slap yun (doul woc))
      (dish:ut ~(dole ut p.vax) q.vax) 
    ::
    ++  ecto                                            ::  type only
      |=  [yun=vase woc=crow]
      ^-  tank
      (dial:ut ~(dole ut (~(play ut p.yun) (doul woc))))
    ::
    ++  fane                                            ::  deliver note 
      |=  [pux=path nog=note tas=vase]
      ^-  vase
      %+  slam  tas
      ;:  slop
        [[%atom %da] now] 
        [pah.typ pux] 
        [noq.typ nog]
      ==
    ::
    ++  fapp                                            ::  launch app
      |=  [yun=vase pax=path cux=cone arg=crow]
      ^-  vase
      %+  slam
        %+  slam
          %+  slam
            (slap nub (doul [%f pax]))
          ;:  slop 
            [[%atom %p] who] 
            [[%atom %da] now] 
            [[%atom %%] (shax :(mix eny now (shax p.lif)))]
            [pah.typ pax]
          ==
        (slap yun (dive cux))
      (slap yun (doul arg))
    --
  ++  lo                                                ::  command parsers
    |%
    ++  htap 
      ;~  pfix  fas
        %+  cook
          |=  [a=reef b=path]
          =+  pot=(flop (scag q.a rew))
          =+  gaw=(slag q.a rew)
          =+  pre=?:(p.p.a (scag q.p.a (flop gaw)) (flop (slag q.p.a gaw)))
          (weld pre ?~(pot b (weld b pot)))
        ;~  plug
          ;~  plug
            ;~  pose
              (cold [%| 0] lus)
              (cook |=(a=(list) [%| (lent a)]) (plus tis))
              (cook |=(a=(list) [%& (lent a)]) (star tar))
            ==
            (cook |=(a=(list) (lent a)) (star buc))
          ==
          ;~(sfix (more (cook |=(a=coin ~(rent co a)) nuck:so) fas) fas)
        ==
      ==
    ::
    ++  ipto                                            ::  lights
      %+  knee  *(list ,@tas)  |.  ~+
      %+  cook
        |=  a=(list (list ,@tas))  ^-  (list ,@tas)
        ?~(a ~ (weld i.a $(a t.a)))
      %+  most
        ;~(plug com ace)
      ;~  pose
        (cook |=(a=@ [a ~]) sym)
        (cook |=(a=@ (rip 3 a)) ;~(pfix lus sym))
      ==
    ::
    ++  ipty                                            ::  notices
      %+  knee  *(list ,[p=@tas q=crow])  |.  ~+
      (most ace ;~(plug sym ;~(pfix fas worc)))
    ::
    ++  kral                                            ::  parse lark
      ;~  pose
        ;~  pfix  col
          ;~  pose
            ;~  pfix  ;~(plug (just 'c') (just 'd') gap)
              (stag %cd htap)
            ==
          ::
            ;~  pfix  ;~(plug (just 't') (just 'o') gap)
              (stag %to (stag %p (most ace worc)))
            ==
          ::
            ;~  pfix  ;~(plug (just 'k') (just 'l') gap)
              (stag %kl (cook |=(a=dime ?>(?=(%ud p.a) q.a)) bisk:so))
            ==
          ::
            (cold [%nk ~] ;~(plug (just 'n') (just 'k')))
            (cold [%ps ~] ;~(plug (just 'p') (just 's')))
          ::
            %+  stag
              %go
            ;~  plug
              (thap %app)
              ;~  pose 
                %+  ifix  [kel ker]
                ;~  pose
                  ;~  plug
                    ipto
                    ;~(pose ;~(pfix ;~(plug sem ace) ipty) (easy ~))
                  ==
                  (stag ~ ipty)
                ==
                (easy [~ ~])
              ==
              (stag %l (star ;~(pfix ace worc)))
            ==
          ==
        ==
      ::
        (stag %eh (stag %p (most ace worc)))
      ==
    ::
    ++  thap
      |=  rol=@ta
      ;~  pose
        htap
        %+  cook                                        ::  XX bletcherous
          |=  a=path  ^-  path
          ?>  ?=([@ @ @ *] a)
          [i.t.t.a i.t.a i.a t.t.t.a]
        ;~  plug
          %+  cook
            |=(a=flag ~(rent co ~ %p a))
          ;~(pose ;~(pfix sig fed:ag) (easy who))
        ::
          %+  cook
            |=  a=[p=@ta q=(unit ,[p=@ta q=(unit ,@ta)])]
            ?~  q.a      [~(rent co ~ %da now) %cx %main rol p.a hub ~]
            ?~  q.u.q.a  [~(rent co ~ %da now) %cx p.a rol p.u.q.a hub ~]
                         [u.q.u.q.a %cx p.a rol p.u.q.a hub ~] 
          ;~  plug
            sym
            ;~  pose
              %+  stag  ~
              ;~  plug
                ;~(pfix fas sym)
                ;~(pose (stag ~ ;~(pfix fas sym)) (easy ~))
              ==
              (easy ~)
            ==
          ==
        ==
      ==
    ::
    ++  worc
      %+  knee  *crow  |.  ~+
      ;~  pose
        ;~  pfix  buc
          %+  cook
            |=  [a=path b=(list crow)]
            `crow`[%c [%f a] b]
          ;~  plug
            (thap %fit)
            (ifix [pel per] (most ace worc))
          ==
        ==
        (stag %g (stag ~ wide:vez))
      ==
    --
  ::
  ++  ride                                              ::  process context
    |=  $:  pid=tick                                    ::  process identity
            hen=hose                                    ::  current hose
            loz=(map path goal)                         ::  request state
            bor=(unit boar)                             ::  execution state
        ==                                              ::
    =+  duz=*(list move)                                ::  pending operations
    |%                                                  ::
    ++  beef                                            ::  raw product
              $:  p=(list card)                         ::  actions
                  q=(list slip)                         ::  requests
                  r=(unit boar)                         ::  state
              ==                                        ::
    ::
    ++  bust                                            ::  slice coal
      |=  [axe=axis vux=coal]
      ^-  coal
      =<  q
      %+  slam  sot.vax
      (slop [[%atom %%] axe] [vas.typ vux])
    ::
    ++  coax                                            ::  update request
      |=  [pux=path fav=card]
      ^+  +>
      =+  fug=(~(get by loz) pux)
      ?~  fug  +>.$
      ?+    -.u.fug  +>.$
          %up
        ?>  =(%line -.fav)
        +>.$(loz (~(del by loz) pux))
      ==
    ::
    ++  dost                                            ::  virtualize
      |=  :*  ton=toon
              fob=_|+((list path) *beef)
              fut=_|+(coal *beef)
          ==
      ^-  beef
      ?-  -.ton
        %0  (fut p.ton)
        %1  (fob ((list path) p.ton))
        %2  [[[%crud p.ton] ~] ~ ~]
      ==
    ::
    ++  emit                                            ::  return a card
      |=  fav=card
      +>(duz [[[~ who] hen fav] duz])
    ::
    ++  emir                                            ::  return cards
      |=  veq=(list card)
      +>(duz (weld (turn veq |=(a=card [[~ who] hen a])) duz))
    ::
    ++  envy                                            ::  advance a card
      |=  [dst=tire fav=card]
      +>(duz [[[~ who] [dst hen] fav] duz])
    ::
    ++  ergo
      |=  gez=(list path)
      ^-  beef
      :_  [~ ~]
      (turn gez |=(a=path [%text "? {~(ram re (dish:ut [~ %path] a))}"]))
    ::
    ++  fret                                            ::  process coal
      |=  poc=coal
      ^-  beef
      :-  ((hard (list card)) +:(bust 2 poc))
      =+  doy=(bust 3 poc)
      ?~  +.doy  [~ ~]
      :-  ((hard (list slip)) +>-.doy)
      [~ %& ~ ~ (bust 7 doy)]
    ::
    ++  gaff                                            ::  kill the process 
      ^+  .
      =+  ask=(~(tap by loz) *(list slip))
      |-  ^+  ..gaff
      ?~  ask
        ..gaff(loz ~)
      $(ask t.ask, ..gaff (geld i.ask))
    ::
    ++  geld                                            ::  abandon a slip
      |=  sip=slip
      ^+  +>
      ?+  -.q.sip  
             +>
        %up  +>(pak (skip pak |=([a=@ud b=path] =(b p.sip))))
      ==
    ::
    ++  germ                                            ::  add a slip
      |=  sip=slip
      ^+  +>
      ?+  -.q.sip  
             +>
        %up  +>(pak [[pid p.sip] pak])
      ==
    ::
    ++  gird                                            ::  process slips
      |=  ask=(list slip)
      ^+  +>
      =.  +>.$
        |-  ^+  +>.^$
        ?~  ask  +>.^$
        =+  tuq=(~(get by loz) p.i.ask)
        ?~  tuq  
          (germ i.ask)
        ?:  =(u.tuq q.i.ask) 
          +>.^$
        (goad i.ask)
      =+  zal=(~(tap by loz) ~)
      =.  loz  (~(gas by *(map path goal)) ask)
      |-  ^+  +>.^$
      ?~  zal  +>.^$
      ?:  (~(has by loz) p.i.zal)  +>.^$
      (geld i.zal)
    ::
    ++  goad                                            ::  modify a slip
      |=  sip=slip                                      
      ^+  +>
      ?+  -.q.sip  
             +>
        %ht  (germ sip)
        %up  !!   ::  this should actually work
      ==
    ::
    ++  haul                                            ::  apply a result
      |=  bof=beef
      ^+  +>
      =.    duz
          %+  weld 
            (turn p.bof |=(a=card [[~ who] hen a]))
          duz
      ?~  r.bof
        gaff
      =.  bor  r.bof
      (gird q.bof)
    ::
    ++  jerk                                            ::  deliver note
      |=  [pux=path nob=note]
      ^+  +>
      ?>  ?=(^ bor)
      ?>  ?=(& -.u.bor)
      ?>  ?=(~ p.u.bor)   ::  XX actually handle blocks
      %-  haul
      %^    dost
          (mong [fane:do [pux nob r.u.bor]] sky)
        ergo
      fret
    ::
    ++  loft                                            ::  execute command
      |=  kal=lark
      ^+  +>
      =+  wan=|=(a=(list path) [~ ~ ~ %| a kal])
      =+  hak=|=(a=* [[[%talk ((hard tank) a)] ~] ~ ~])
      ?-    -.kal
          %cd  +>.$(cwd p.kal)
          %eh  (haul (dost (mong [echo:do lube +.kal] sky) ergo hak))
          %go  (haul (dost (mong [fapp:do lube +.kal] sky) ergo fret))
          %kl  (envy /b/ [%kill p.kal])
          ::  %nk  (envy /b/ [%nuke ~])
          %nk  =+(sad=(dec 0) (envy /b/ [%nuke ~]))
          %ps  (emir view)
          %to  (haul (dost (mong [ecto:do lube +.kal] sky) ergo hak))
      ==
    ::
    ++  mete                                            ::  deliver line
      |=  fet=tape
      ^+  +>
      =>  .(p.hit +(p.hit), q.hit [fet q.hit])
      =+  zif=((full (ifix [gay gay] kral:lo)) [1 1] fet)
      ?^  q.zif 
        (loft p.u.q.zif)
      =+  duf=[p=~(rend co ~ %ud p.p.zif) q=~(rend co ~ %ud q.p.zif)]
      %=    +>.$
          duz
        :_  duz
        :+  [~ who]  hen 
        [%warn "<syntax error at [{p.duf} {q.duf}]>"]
      ==
    ::
    ++  nave                                            ::  resolve
      ^-  [(list move) _..^$]
      =+  tid=~(rend co ~ %ud pid)
      ?:  =(~ loz)
        =.  p.lif  ?:(=(pid (dec p.lif)) (dec p.lif) p.lif)
        ?.  (~(has by q.lif) pid)
          [duz ..^$]
        =.  ..nave  (envy /d/ %flog %text (weld "/ %" tid))
        [duz ..^$(q.lif (~(del by q.lif) pid))]
      ?>  ?=(^ bor)
      =>  %=    .
              ..nave
            %+  envy
              /d/
            [%flog %text (weld ?:((~(has by q.lif) pid) "= %" "^ %") tid)]
          ==
      [duz ..^$(q.lif (~(put by q.lif) pid [loz u.bor]))]
    ::
    --
  ::
  ++  leap                                              ::  dispatch event
    |=  [pex=path hen=hose fav=card]
    ^-  [p=(list move) q=_..^$]
    =<  ?+  -.fav  [[[[~ who] hen fav] ~] ..^$]
          %kill  (kill p.fav)
          %nuke  nuke
          %line  (gill (trip p.fav))
          %thee  (heat p.fav)
        ==
    |%
    ++  gill                                            ::  dispatch line
      |=  fet=tape
      ^-  [p=(list move) q=_..^^$]
      =<  nave
      ^+  *ride
      ?~  pak
        =+  pid=p.lif
        =.  p.lif  +(p.lif) 
        (mete:(ride pid hen ~ ~) fet)
      =+  [pid=p.i.pak pux=q.i.pak]
      =>  .(pak t.pak)
      =+  byr=(need (~(get by q.lif) pid))
      %.  [pux [%up (rap 3 fet)]]
      =<  jerk
      %.  [pux fav]
      =<  coax
      (ride pid hen p.byr [~ q.byr])
    :: 
    ++  heat                                            ::  dispatch http req
      |=  het=hate
      ^-  [p=(list move) q=_..^^$]
      =+  sud=(lout p.het)
      ?~  sud
        :_  ..^^$
        :~  :+  [~ who]
              hen
            :-  %that
            :-  %raw
            [404 ~ [~ (tact "http error 404")]]
        ==
      =+  byr=(need (~(get by q.lif) p.u.sud))
      =<  nave
      %.  [q.u.sud [%ht r.u.sud q.het r.het]]
      =<  jerk
      (ride p.u.sud hen p.byr [~ q.byr])
    ::
    ++  kill                                            ::  kill a process
      |=  pid=tick
      ^-  [p=(list move) q=_..^^$]
      =+  byr=(need (~(get by q.lif) pid))
      =<  nave
      =<  gaff
      (ride pid hen p.byr [~ q.byr])
    ::
    ++  nuke                                            ::  kill all processes
      =+  fen=(~(tap by q.lif) ~)
      |-  ^-  [(list move) _..^^$]
      ?~  fen
        [~ ..^^$]
      =^  mor  ..^^$  $(fen t.fen)
      =+  les=(kill p.i.fen)
      [(weld p.les mor) q.les]
    --
  ::
  ++  loot                                              ::  match route
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
  ::
  ++  lout                                              ::  request to process
    |=  uri=purl                                        ::  XX map iterator
    ^-  (unit ,[p=tick q=path r=scab])
    =+  fen=`(list ,[p=tick q=bear])`(~(tap by q.lif) ~)
    |-  ^-  (unit ,[p=tick q=path r=scab])
    ?~  fen  ~
    =+  ask=`(list slip)`(~(tap by p.q.i.fen) ~)
    |-  ^-  (unit ,[p=tick q=path r=scab])
    ?~  ask  ^$(fen t.fen)
    ?.  ?=([%ht *] q.i.ask)
      $(ask t.ask)
    |-  ^-  (unit ,[p=tick q=path r=scab])
    ?~  p.q.i.ask  ^$(ask t.ask)
    =+  sem=(loot uri i.p.q.i.ask)
    ?~  sem  
      $(p.q.i.ask t.p.q.i.ask)
    [~ p.i.fen p.i.ask `scab`[`oryx`r.i.p.q.i.ask r.uri u.sem]]
  ::
  ++  lube                                              ::  define subject
    ^-  vase
    ;:  slop
      ;:  slop
        %+  slop
          [[%atom %da] now]
        [[%atom %ta] ~(rent co [~ %da now])]
      ::
        %+  slop
          [[%atom %p] who]
        [[%atom %ta] ~(rent co [~ %p who])]
      ::
        [liz.typ q.hit]
      ==
    ::
      =+  voy=(~(tap to q.sur) ~)
      |-  ^-  vase
      ?~(voy [[%atom %n] ~] (slop i.voy $(voy t.voy)))
    ::
      ?~  way
        nub
      %-  slop
      :_  nub
      |-  ^-  vase
      ?+  way  !!     ::  XX some inference weirdness here?
        [* ~ ~]  [[%face p.n.way p.q.n.way] q.q.n.way]
        [* ~ ^]  (slop $(r.way ~) $(way r.way))
        [* ^ ~]  (slop $(l.way ~) $(way l.way))
        [* ^ ^]  :(slop $(r.way ~, l.way ~) $(way l.way) $(way r.way))
      ==
    ==
  ::
  ++  prot                                              ::  current prompt
    ^-  prod
    ?~  pak 
      [& (rip 3 (cat 3 hox '> '))]
    =+  byr=(need (~(get by q.lif) p.i.pak))
    =+  gol=(need (~(get by p.byr) q.i.pak))
    ?>(?=(%up -.gol) p.gol)
  ::
  ++  stay                                              ::  add to results
    |=  vax=vase
    %_    ..^$
        sur
      ?:  =(16 p.sur)
        [16 (~(put to q:~(get to q.sur)) vax)]
      [+(p.sur) (~(put to q.sur) vax)]
    ==
  ++  view                                              ::  render processes
    =+  fen=(~(tap by q.lif) ~)
    |-  ^-  (list card)
    ?~  fen  ~
    :_  $(fen t.fen)
    =+  cyv=[p.i.fen (~(tap by p.q.i.fen) ~)]
    [%talk >cyv<]
  --
--
