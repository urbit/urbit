!:::::
::  ::  %ford, new execution control
!?  164
::::
|=  pit=vase   
=>  =~
::  structures
|%
::
++  gift  gift-ford                                     ::  out result <-$
++  heel  path                                          ::  functional ending
++  kiss  kiss-ford                                     ::  in request ->$
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  note                                                ::  out request $->
          $%  $:  %c                                    ::  to %clay
          $%  [%warp p=sock q=riff]                     ::
          ==  ==  ==                                    ::
++  sign                                                ::  in result $<-
          $%  $:  %c                                    ::  by %clay
          $%  [%writ p=riot]                            ::
          ==  ==  ==                                    ::
--                                                      ::
|%                                                      ::  structures
++  axle                                                ::  all %ford state
  $:  %1                                                ::  version for update
      pol=(map ship baby)                               ::
  ==                                                    ::
++  baby                                                ::  state by ship
  $:  tad=[p=@ud q=(map ,@ud task)]                     ::  tasks by number
      dym=(map duct ,@ud)                               ::  duct to task number
      deh=(map ,@uvH deps)                              ::  depends by hash
      jav=(map ,* calx)                                 ::  cache
  ==                                                    ::
++  bolt                                                ::  gonadic edge
  |*  a=$+(* *)                                         ::  product clam
  $:  p=cafe                                            ::  cache
    $=  q                                               ::
      $%  [%0 p=(set beam) q=a]                         ::  depends/product
          [%1 p=(set ,[p=care q=beam r=tang])]          ::  blocks
          [%2 p=(set beam) q=tang]                      ::  depends/error
      ==                                                ::
  ==                                                    ::
::                                                      ::
++  burg                                                ::  gonadic rule
  |*  [a=$+(* *) b=$+(* *)]                             ::  from and to
  $+([c=cafe d=a] (bolt b))                             ::
::                                                      ::
++  cafe                                                ::  live cache
  $:  p=(set calx)                                      ::  used
      q=(map ,* calx)                                   ::  cache
  ==                                                    ::
::                                                      ::
++  calm                                                ::  cache metadata
  $:  laz=@da                                           ::  last accessed
      dep=(set beam)                                    ::  dependencies
  ==                                                    ::
++  calx                                                ::  concrete cache line
  $%  [%hood p=calm q=(pair beam gage) r=hood]          ::  compile
      [%bake p=calm q=(trel mark beam heel) r=(unit vase)]::  load
      [%slit p=calm q=[p=type q=type] r=type]           ::  slam type
      [%slim p=calm q=[p=type q=twig] r=(pair type nock)]::  mint
      [%slap p=calm q=[p=vase q=twig] r=vase]           ::  compute
      [%slam p=calm q=[p=vase q=vase] r=vase]           ::  compute
  ==                                                    ::
++  deps                                                ::  depend state
  $%  [%init p=(set beam)]                              ::  given out
      [%sent p=(set duct) q=(set beam)]                 ::  listener exists
      [%done ~]                                         ::  change seen
  ==                                                    ::
++  task                                                ::  problem in progress
  $:  nah=duct                                          ::  cause
      kas=silk                                          ::  problem
      keg=(map (pair term beam) cage)                   ::  block results
      kig=[p=@ud q=(map ,@ud ,[p=care q=beam])]         ::  blocks
  ==                                                    ::
--                                                      ::
|%                                                      ::
++  calf                                                ::  reduce calx
  |*  sem=*                                             ::  a typesystem hack
  |=  cax=calx
  ?+  sem  !!
    %hood  ?>(?=(%hood -.cax) r.cax)
    %bake  ?>(?=(%bake -.cax) r.cax)
    %slap  ?>(?=(%slap -.cax) r.cax)
    %slam  ?>(?=(%slam -.cax) r.cax)
    %slim  ?>(?=(%slim -.cax) r.cax)
    %slit  ?>(?=(%slit -.cax) r.cax)
  ==
::
++  calk                                                ::  cache lookup
  |=  a=cafe                                            ::
  |=  [b=@tas c=*]                                      ::
  ^-  [(unit calx) cafe]                                ::
  =+  d=(~(get by q.a) [b c])                           ::
  ?~  d  [~ a]                                          ::
  [d a(p (~(put in p.a) u.d))]                          ::
::                                                      ::
++  came                                                ::
  |=  [a=cafe b=calx]                                   ::  cache install
  ^-  cafe                                              ::
  a(q (~(put by q.a) [-.b q.b] b))                      ::
::                                                      ::
++  chub                                                ::  cache merge
  |=  [a=cafe b=cafe]                                   ::
  ^-  cafe                                              ::
  [(grom p.a p.b) (grum q.a q.b)]                       ::
::                                                      ::
++  faun  |=([a=cafe b=vase] (fine a `gage`[%noun b]))  ::  vase to cage
++  feel  |=([a=cafe b=gage] (fine a q.b))              ::  cage to vase
++  fest                                                ::  bolt to success
  |=  a=beam                                            ::
  |*([b=cafe c=*] (flag a (fine b [~ u=c])))            ::
::                                                      ::
++  fine  |*  [a=cafe b=*]                              ::  bolt from data
          [p=`cafe`a q=[%0 p=*(set beam) q=b]]          ::
++  flaw  |=  [a=cafe b=tang]                           ::  bolt from error
          [p=a q=[%2 p=*(set beam) q=b]]                ::
++  flag                                                ::  beam into deps
  |*  [a=beam b=(bolt)]                                 ::
  ?:  ?=(%1 -.q.b)  b
  =.  p.q.b  (~(put in p.q.b) a)
  b
::                                                      ::
++  flue  |=(a=cafe (fine a ~))                         ::  cafe to empty
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
++  lark                                                ::  filter arch names
  |=  [wox=$+(span (unit ,@)) arc=arch]
  ^-  (map ,@ span)
  %-  ~(gas by *(map ,@ span))
  =|  rac=(list (pair ,@ span))
  |-  ^+  rac
  ?~  r.arc  rac
  =.  rac  $(r.arc l.r.arc, rac $(r.arc r.r.arc))
  =+  gib=(wox p.n.r.arc)
  ?~(gib rac [[u.gib p.n.r.arc] rac])
::
++  norm                                                ::  normalize beam rev
  |=  [ska=sled bem=beam]
  %_  bem
    r  ?:  ?=(%ud -.r.bem)  r.bem
       =+  num=(ska ~ %cw bem(s ~))
       ?.  ?=([~ ~ * * @u] num)
         ~&  norm-lost/(tope bem(s ~))
         r.bem  ::  XX
       [%ud q.q.u.u.num]
  ==
::
++  za                                                  ::  per event
  =|  $:  $:  $:  our=ship                              ::  computation owner
                  bek=beak                              ::  desk context
                  hen=duct                              ::  event floor
              ==                                        ::
              $:  now=@da                               ::  event date
                  eny=@                                 ::  unique entropy
                  ska=sled                              ::  system namespace
              ==                                        ::
              mow=(list move)                           ::  pending actions
          ==                                            ::
          bay=baby                                      ::  all owned state
      ==                                                ::
  |%
  ++  abet                                              ::  resolve
    ^-  [(list move) baby]
    [(flop mow) bay]
  ::
  ++  apex                                              ::  call
    |=  kus=(unit silk)
    ^+  +>
    ?~  kus
      =+  nym=(~(get by dym.bay) hen)
      ?~  nym                                           ::  XX should never
        ~&  [%ford-mystery hen]
        +>.$
      =+  tas=(need (~(get by q.tad.bay) u.nym))
      amok:~(camo zo [u.nym tas])
    =+  num=p.tad.bay
    ?<  (~(has by dym.bay) hen)
    =:  p.tad.bay  +(p.tad.bay)
        dym.bay    (~(put by dym.bay) hen num)
      ==
    ~(exec zo [num `task`[hen u.kus ~ 0 ~]])
  ::
  ++  axon                                              ::  take
    |=  [num=@ud tik=@ud sih=sign]
    ^+  +>
    ?-    -.+.sih
        %writ
      =+  tus=(~(get by q.tad.bay) num)
      ?~  tus
        ~&  [%ford-lost num]
        +>.$
      (~(resp zo [num u.tus]) tik p.+.sih)
    ==
  ::
  ++  axun                                              ::  take rev update
    |=  [tea=wire dep=@uvH sup=spur sih=sign]
    ^+  +>
    ?-    -.+.sih
        %writ
      ?~  p.sih  +>.$ 
      =+  [dap=(~(got by deh.bay) dep) bem=`beam`[bek sup]]
      =-  ?~(dop con con(deh.bay (~(put by deh.bay) dep dop)))
      ^-  [dop=$|(~ _dap) con=_+>.$]
      ?-    -.dap
          %done  `+>.$                ::  writ redundant
          %init  ~|(never-subscribed/dep !!)
          %sent
        =-  [[%done ~] +>.$(mow (weld - mow))]
        ^-  (list move)
        %+  weld  (turn (~(tap in p.dap)) |=(hen=duct [hen %give %news ~]))
        =.  q.dap  (~(del in q.dap) bem)
        %+  turn  (~(tap in q.dap))                     ::  cancel outstanding
        |=  bem=beam
        =.  tea  [(scot %p our) (scot %uv dep) (tope bem)]
        [hen %pass tea %c %warp [our p.bem] q.bem ~]
      ==
    ==
  ::
  ++  awap                                              ::  get next revision
    ~%  %ford-w  ..is  ~
    |=  dep=@uvH
    ?~  dep
      ~&(dep-empty/hen +>.$)
    ?:  =(dep 0vtest)                 ::  upstream testing
      +>.$(mow :_(mow [hen %give %news ~]))
    =+  dap=(~(get by deh.bay) dep)
    ?~  dap  ~&(dep-missed/dep +>.$)  ::  XX  ~|  !!
    :: ~&  awap/[dep u.dap]
    ?-  -.u.dap
      %done  +>.$(mow :_(mow [hen %give %news ~]))
      %sent
        =.  p.u.dap  (~(put in p.u.dap) hen)
        +>.$(deh.bay (~(put by deh.bay) dep u.dap))
      %init
        %_    +>.$
            deh.bay 
          (~(put by deh.bay) dep [%sent [hen ~ ~] p.u.dap])
        ::
            mow
          =<  (welp :_(mow (turn (~(tap in p.u.dap)) .)))
          |=  bem=beam 
          :^  hen  %pass  [(scot %p our) (scot %uv dep) (tope bem)]
          [%c [%warp [our p.bem] q.bem ~ [%next %y r.bem (flop s.bem)]]]
    ==  ==
  ::
  ++  zo
    ~%    %ford-z
        ..is
      ==
        %fade  fade
        %fair  fair
        %fang  fang
        %lime  lime
        %lima  lima
        %link  link
        %lope  lope
        %make  make
        %meow  meow
      ==
    |_  [num=@ud task]
    ++  abet  %_(..zo q.tad.bay (~(put by q.tad.bay) num +<+))
    ++  amok  
      %_  ..zo  
        q.tad.bay  (~(del by q.tad.bay) num)
        dym.bay    (~(del by dym.bay) nah)
      ==
    ++  camo                                            ::  stop requests
      ^+  .
      =+  kiz=(~(tap by q.kig) *(list ,[p=@ud q=[p=care q=beam]]))
      |-  ^+  +>
      ?~  kiz  +>
      %=    $
          kiz  t.kiz
          mow
        :_  mow
        :-  hen
        :^    %pass
            [(scot %p our) (scot %ud num) (scot %ud p.i.kiz) (tope bek ~)]
          %c
        [%warp [our p.q.q.i.kiz] q.q.q.i.kiz ~]
      ==
    ::
    ++  camp                                            ::  request a file
      |=  [ren=care bem=beam]
      ^+  +>
      %=    +>
          kig  [+(p.kig) (~(put by q.kig) p.kig [ren bem])]
          mow
        :_  mow
        :-  hen
        :^    %pass
            [(scot %p our) (scot %ud num) (scot %ud p.kig) (tope bek ~)]
          %c
        ~&  >>  [%camping bem]
        [%warp [our p.bem] q.bem [~ %sing ren r.bem (flop s.bem)]]
      ==
    ::
    ++  clef                                            ::  cache a result
      |*  sem=*
      |*  [hoc=(bolt) fun=(burg)]
      ?-    -.q.hoc
          %2  hoc
          %1  hoc
          %0
        =^  cux  p.hoc  ((calk p.hoc) sem q.q.hoc)
        ?^  cux
          [p=p.hoc q=[%0 p=dep.p.u.cux q=((calf sem) u.cux)]]
        =+  nuf=(cope hoc fun)
        ?-    -.q.nuf
            %2  nuf
            %1  nuf
            %0
          :-  p=(came p.nuf `calx`[sem `calm`[now p.q.nuf] q.q.hoc q.q.nuf])
          q=q.nuf
        ==
      ==
    ::
    ++  coax                                            ::  bolt across
      |*  [hoc=(bolt) fun=(burg)]
      ?-  -.q.hoc
        %0  =+  nuf=$:fun(..+<- p.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              %0  [%0 p=(grom p.q.hoc p.q.nuf) q=[q.q.hoc q.q.nuf]]
              %1  q.nuf
              %2  q.nuf
            ==
        %1  =+  nuf=$:fun(..+<- p.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              %0  q.hoc
              %1  [%1 p=(grom p.q.nuf p.q.hoc)]
              %2  q.nuf
            ==
        %2  hoc
      ==
    ::
    ++  cool                                            ::  error caption
      |*  [cyt=trap hoc=(bolt)]
      ?.  ?=(%2 -.q.hoc)  hoc
      [p.hoc [%2 p.q.hoc *cyt q.q.hoc]]
    ::
    ++  cope                                            ::  bolt along
      |*  [hoc=(bolt) fun=(burg)]
      ?-  -.q.hoc
        %1  hoc
        %2  hoc
        %0  =+  nuf=(fun p.hoc q.q.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              %1  q.nuf
              %2  [%2 p=(grom `_p.q.nuf`p.q.hoc p.q.nuf) q=q.q.nuf]
              %0  [%0 p=(grom `_p.q.nuf`p.q.hoc p.q.nuf) q=q.q.nuf]
      ==   ==
    ::
    ++  coup                                            ::  toon to bolt
      |=  cof=cafe
      |*  [ton=toon fun=$+(* *)]
      :-  p=cof
      ^=  q
      ?-  -.ton
        %2  [%2 p=*(set beam) q=p.ton]
        %0  [%0 p=*(set beam) q=(fun p.ton)]
        %1  ::  ~&  [%coup-need ((list path) p.ton)]
            =-  ?-  -.faw
                  &  :-  %1
                     ^=  p
                     %-  sa
                     %+  turn  p.faw
                     |=(a=[care beam] [-.a +.a *tang])
                  |  [%2 p=*(set beam) q=p.faw]
                ==
            ^=  faw
            |-  ^-  (each (list (pair care beam)) tang)
            ?~  p.ton  [%& ~]
            =+  nex=$(p.ton t.p.ton)
            =+  err=|=(a=tape [%| leaf/a ?:(?=(& -.nex) ~ p.nex)])
            =+  pax=(path i.p.ton)
            ?~  pax  (err "blocking empty")
            ?.  ?=(%c (end 3 1 i.pax))
              (err "blocking not clay")
            =+  ren=((soft care) (rsh 3 1 i.pax))
            ?~  ren
              (err "blocking not care")
            =+  zis=(tome t.pax)
            ?~  zis
              (err "blocking not beam")
            ?-  -.nex
              &  [%& [u.ren u.zis] p.nex]
              |  nex
            ==
      ==
    ::
    ++  cowl                                            ::  each to bolt
      |=  cof=cafe
      |*  [tod=(each ,* tang) fun=$+(* *)]
      %+  (coup cof)
        ?-  -.tod
          %&  [%0 p=p.tod]
          %|  [%2 p=p.tod]
        ==
      fun
    ::
    ++  dash                                            ::  process cache
      |=  cof=cafe
      ^+  +>
      %_(+> jav.bay q.cof)
    ::
    ++  diff                                            ::  diff
      |=  [cof=cafe kas=silk kos=silk]
      ^-  (bolt gage)
      %.  [cof kas kos]
      ;~  cope
        ;~  coax
          |=([cof=cafe p=silk q=silk] (make cof p))
          |=([cof=cafe p=silk q=silk] (make cof q))
        ==
        |=  [cof=cafe cay=gage coy=gage]
        ?.  &(?=(@ p.cay) ?=(@ p.coy))
          (flaw cof leaf/"bad diff marc" ~)
        ?.  =(p.cay p.coy)
          %+  flaw  cof  :_  ~
          leaf/"diff on data of different marks: {(trip p.cay)} {(trip p.coy)}"
        ?:  =(q.q.cay q.q.coy)
          (fine cof %null [%atom %n] ~)
        %+  cope  (fang cof p.cay)
        |=  [cof=cafe pro=vase]
        ?.  (slab %grad p.pro)
          (flaw cof leaf/"no ++grad" ~)
        =+  gar=(slap pro [%cnzy %grad])
        ?.  (slab %form p.gar)
          ?.  (slab %sted p.gar)
            (flaw cof leaf/"no ++form:grad nor ++sted:grad" ~)
          =+  for=((soft ,@tas) q:(slap gar [%cnzy %sted]))
          ?~  for
            (flaw cof leaf/"bad ++sted:grad" ~)
          %^  make  cof  %diff
          :-  [%cast u.for [%done ~ cay]]
          [%cast u.for [%done ~ coy]]
        ?.  (slab %diff p.gar)
          (flaw cof leaf/"no ++diff:grad" ~)
        %+  cope  (keel cof pro [[%& 6]~ q.cay]~)
        |=  [cof=cafe pox=vase]
        %+  cope
          %^  maul  cof
            (slap (slap pox [%cnzy %grad]) [%cnzy %diff])
          q.coy
        |=  [cof=cafe dif=vase]
        =+  for=((soft ,@tas) q:(slap gar [%cnzy %form]))
        ?~  for
          (flaw cof leaf/"bad ++form:grad" ~)
        (fine cof u.for dif)
      ==
    ::
    ++  daze                                            ::  remember depends
      |=  dep=(set beam)
      ^+  [*@uvH deh.bay]
      ?~  dep  [0v0 deh.bay]
      =+  hap=(sham dep)
      ?:  (~(has by deh.bay) hap)
        [hap deh.bay]
      [hap (~(put by deh.bay) hap [%init dep])]
    ::
    ++  exec                                            ::  execute app
      ^+  ..zo
      ?:  !=(~ q.kig)  ..zo
      =+  bot=(make [~ jav.bay] kas)
      =.  ..exec  (dash p.bot)
      ?-  -.q.bot
        %0  =^  dep  deh.bay  (daze p.q.bot)
            amok:(expo [%made dep %& q.q.bot])
        %2  =^  dep  deh.bay  (daze p.q.bot)
            amok:(expo [%made dep %| q.q.bot])
        %1  =+  zuk=(~(tap by p.q.bot) ~)
            =<  abet
            |-  ^+  ..exec
            ?~  zuk  ..exec
            =+  foo=`_..exec`(camp p.i.zuk q.i.zuk)
            $(zuk t.zuk, ..exec foo)
      ==
    ::
    ++  expo                                            ::  return gift
      |=  gef=gift
      %_(+> mow :_(mow [hen %give gef]))
    ::
    ++  fade                                            ::  compile to hood
      |=  [cof=cafe for=mark bem=beam]
      ^-  (bolt hood)
      %+  cool  |.(leaf/"ford: fade {<[(tope bem)]>}")
      %+  cope  (make cof [%bake for bem ~])
      |=  [cof=cafe cay=gage]
      %+  (clef %hood)  (fine cof bem(r [%ud 0]) cay)
      ^-  (burg (pair beam gage) hood)
      |=  [cof=cafe bum=beam cay=gage]
      ::  ~&  fade/clef-miss/bem
      =+  rul=(fair bum)
      ?.  ?=(@ q.q.cay)
        (flaw cof ~)
      =+  vex=((full rul) [[1 1] (trip q.q.cay)])
      ?~  q.vex
        (flaw cof [%leaf "syntax error: {<p.p.vex>} {<q.p.vex>}"] ~)
      (fine cof p.u.q.vex)
    ::
    ++  fang                                            ::  protocol door
      |=  [cof=cafe for=mark]
      ^-  (bolt vase)
      =+  pax=/door/[for]/mar
      %+  cope
        (fade cof %hook [bek pax])
      abut:(meow [bek pax] ~)
    ::
    ++  fair                                            ::  hood parsing rule
      |=  bem=beam
      ?>  ?=([%ud 0] r.bem)           ::  XX sentinel
      =+  vez=(vang | (tope bem))
      =<  hood
      |%  
      ++  case
        %-  sear  
        :_  nuck:so
        |=  a=coin
        ?.  ?=([%$ ?(%da %ud %tas) *] a)  ~
        [~ u=(^case a)]
      ::
      ++  hath  (sear plex:vez (stag %clsg poor:vez))   ::  hood path
      ++  have  (sear tome ;~(pfix fas hath))           ::  hood beam
      ++  hood
        %+  ifix  [gay gay]
        ;~  plug
          ;~  pose
            (ifix [;~(plug fas wut gap) gap] dem)
            (easy zuse)
          ==
        ::
          ;~  pose
            (ifix [;~(plug fas hep gap) gap] (most ;~(plug com gaw) hoot))
            (easy ~)
          ==
        ::
          ;~  pose
            (ifix [;~(plug fas lus gap) gap] (most ;~(plug com gaw) hoof))
            (easy ~)
          ==
        ::
          (star ;~(sfix horn gap))
          (most gap hoop)
        ==
      ::
      ++  hoof
        %+  cook  |=(a=^hoof a)
        ;~  plug
          sym
          ;~  pose
            %+  stag  ~
            ;~(plug ;~(pfix fas case) ;~(pfix ;~(plug fas sig) fed:ag))
            (easy ~)
          ==
        ==
      ::
      ++  hoot
        ;~  pose
          (stag %| ;~(pfix tar hoof))
          (stag %& hoof)
        ==
      ::
      ++  hoop
        ;~  pose
          (stag %| ;~(pfix ;~(plug fas fas gap) have))
          (stag %& tall:vez)
        ==
      ::
      ++  horn
        =<  apex
        =|  tol=?
        |%
        ++  apex
          %+  knee  *^horn  |.  ~+
          ;~  pfix  fas
            ;~  pose
              (stag %toy ;~(sfix sym fas))
              (stag %ape ;~(pfix sig ape:read))
              (stag %arg ;~(pfix buc ape:read))
              (stag %day ;~(pfix bar day:read))
              (stag %dub ;~(pfix tis dub:read))
              (stag %fan ;~(pfix dot fan:read))
              (stag %for ;~(pfix com for:read))
              (stag %hel ;~(pfix cen hel:read))
              (stag %hub ;~(pfix pat day:read))
              (stag %man ;~(pfix tar man:read))
              (stag %nap ;~(pfix cab day:read))
              (stag %now ;~(pfix pam day:read))
              (stag %saw ;~(pfix sem saw:read))
              (stag %see ;~(pfix col see:read))
              (stag %sic ;~(pfix ket sic:read))
            ==
          ==
        ::
        ++  rail
          |*  [wid=_rule tal=_rule]
          ?.  tol  wid
          ;~(pose wid tal)
        ::
        ++  read
          |%  ++  ape
                %+  rail
                  (ifix [sel ser] (stag %cltr (most ace wide:vez)))
                ;~(pfix gap tall:vez)
          ::
              ++  day  
                %+  rail
                  apex(tol |) 
                ;~(pfix gap apex)
          ::
              ++  dub
                %+  rail  
                  ;~(plug sym ;~(pfix tis apex(tol |)))
                ;~(pfix gap ;~(plug sym ;~(pfix gap apex)))
          ::
              ++  fan
                %+  rail  fail 
                ;~(sfix (star ;~(pfix gap apex)) ;~(plug gap duz))
          ::
              ++  for
                %+  rail
                  ;~(plug (ifix [sel ser] hath) apex(tol |))
                ;~(pfix gap ;~(plug hath ;~(pfix gap apex)))
          ::
              ++  hel
                %+  rail
                  ;~(plug ;~(pose ;~(sfix dem:ag cen) (easy 0)) apex(tol |))
                ;~(pfix gap ;~(plug ;~(pose ;~(sfix dem:ag gap) (easy 0)) apex))
          ::
              ++  man
                %+  rail  fail
                %-  sear
                :_  ;~(sfix (star ;~(pfix gap apex)) ;~(plug gap duz))
                |=  fan=(list ^horn)
                =|  naf=(list (pair term ^horn))
                |-  ^-  (unit (map term ^horn))
                ?~  fan  (some (~(gas by *(map term ^horn)) naf))
                ?.  ?=(%dub -.i.fan)  ~
                $(fan t.fan, naf [[p.i.fan q.i.fan] naf])
          ::
              ++  saw
                %+  rail
                  ;~(plug ;~(sfix wide:vez sem) apex(tol |))
                ;~(pfix gap ;~(plug tall:vez ;~(pfix gap apex)))
          ::
              ++  see
                %+  rail  
                  ;~(plug ;~(sfix have col) apex(tol |))
                ;~(pfix gap ;~(plug have ;~(pfix gap apex)))
          ::
              ++  sic
                %+  rail  
                  ;~(plug ;~(sfix toil:vez ket) apex(tol |))
                ;~(pfix gap ;~(plug howl:vez ;~(pfix gap apex)))
          --
        ::
        --
      --
    ::
    ++  join
      |=  [cof=cafe for=mark kas=silk kos=silk]
      ^-  (bolt gage)
      %.  [cof kas kos]
      ;~  cope
        ;~  coax
          |=([cof=cafe p=silk q=silk] (make cof p))
          |=([cof=cafe p=silk q=silk] (make cof q))
        ==
        |=  [cof=cafe cay=gage coy=gage]
        ?.  &(?=(@ p.cay) ?=(@ p.coy))
          (flaw cof leaf/"bad join marc: {<p.cay>} {<p.coy>}" ~)
        %+  cope  (fang cof for)
        |=  [cof=cafe pro=vase]
        ?.  (slab %grad p.pro)
          (flaw cof leaf/"no ++grad" ~)
        =+  gar=(slap pro [%cnzy %grad])
        ?.  (slab %form p.gar)
          ?.  (slab %sted p.gar)
            (flaw cof leaf/"no ++form:grad nor ++sted:grad" ~)
          =+  too=((soft ,@tas) q:(slap gar [%cnzy %sted]))
          ?~  too
            (flaw cof leaf/"bad ++sted:grad" ~)
          (make cof %join u.too [%done ~ cay] [%done ~ coy])
        =+  fom=((soft ,@tas) q:(slap gar [%cnzy %form]))
        ?~  fom
          (flaw cof leaf/"bad ++form:grad" ~)
        ?.  &(=(u.fom p.cay) =(u.fom p.coy))
          %+  flaw  cof  :_  :_  ~
            leaf/"join on data of bad marks: {(trip p.cay)} {(trip p.coy)}"
          leaf/"expected mark {(trip u.fom)}"
        ?:  =(q.q.cay q.q.coy)
          (fine cof cay)
        ?.  (slab %join p.gar)
          (flaw cof leaf/"no ++join:grad" ~)
        %+  cope
          %^  maul  cof
            (slap (slap pro [%cnzy %grad]) [%cnzy %join])
          (slop q.cay q.coy)
        |=  [cof=cafe dif=vase]
        ?@  q.dif
          (fine cof %null dif)
        (fine cof u.fom (slot 3 dif))
      ==
    ::
    ++  mash
      |=  [cof=cafe for=mark mas=milk mos=milk]
      ^-  (bolt gage)
      %.  [cof r.mas r.mos]
      ;~  cope
        ;~  coax
          |=([cof=cafe p=silk q=silk] (make cof p))
          |=([cof=cafe p=silk q=silk] (make cof q))
        ==
        |=  [cof=cafe cay=gage coy=gage]
        ?.  &(?=(@ p.cay) ?=(@ p.coy))
          (flaw cof leaf/"bad mash marc: {<p.cay>} {<p.coy>}" ~)
        %+  cope  (fang cof for)
        |=  [cof=cafe pro=vase]
        ?.  (slab %grad p.pro)
          (flaw cof leaf/"no ++grad" ~)
        =+  gar=(slap pro [%cnzy %grad])
        ?.  (slab %form p.gar)
          ?.  (slab %sted p.gar)
            (flaw cof leaf/"no ++form:grad nor ++sted:grad" ~)
          =+  too=((soft ,@tas) q:(slap gar [%cnzy %sted]))
          ?~  too
            (flaw cof leaf/"bad ++sted:grad" ~)
          %+  make  cof
          [%mash u.too [p.mas q.mas [%done ~ cay]] [p.mos q.mos [%done ~ coy]]]
        =+  fom=((soft ,@tas) q:(slap gar [%cnzy %form]))
        ?~  fom
          (flaw cof leaf/"bad ++form:grad" ~)
        ?.  &(=(u.fom p.cay) =(u.fom p.coy))
          %+  flaw  cof  :_  :_  ~
            leaf/"mash on data of bad marks: {(trip p.cay)} {(trip p.coy)}"
          leaf/"expected mark {(trip u.fom)}"
        ?:  =(q.q.cay q.q.coy)
          (fine cof cay)
        ?.  (slab %mash p.gar)
          (fine cof %null [%atom %n] ~)
        %+  cope
          %^  maul  cof
            (slap (slap pro [%cnzy %grad]) [%cnzy %mash])
          ;:  slop
              (slop [[%atom %p] p.mas] [[%atom %tas] q.mas])
              (slop [[%atom %p] p.mos] [[%atom %tas] q.mos])
              q.cay
              q.coy
          ==
        |=  [cof=cafe dif=vase]
        (fine cof u.fom dif)
      ==
    ::
    ++  kale                                            ::  mutate
      |=  [cof=cafe kas=silk muy=(list (pair wing silk))]
      ^-  (bolt gage)
      %+  cope
        |-  ^-  (bolt (list (pair wing vase)))
        ?~  muy  (flue cof)
        %+  cope  (make cof q.i.muy)
        |=  [cof=cafe cay=gage]
        %+  cope  ^$(muy t.muy)
        |=  [cof=cafe rex=(list (pair wing vase))]
        (fine cof [[p.i.muy q.cay] rex])
      |=  [cof=cafe yom=(list (pair wing vase))]
      %+  cope  (make cof kas)
      |=  [cof=cafe cay=gage]
      %+  cope  (keel cof q.cay yom)
      |=  [cof=cafe vax=vase]
      (fine cof p.cay vax)
    ::
    ++  keel                                            ::  apply mutations
      |=  [cof=cafe suh=vase yom=(list (pair wing vase))]
      ^-  (bolt vase)
      %^  maim  cof 
        %+  slop  suh
        |-  ^-  vase
        ?~  yom  [[%atom %n] ~]
        (slop q.i.yom $(yom t.yom))
      ^-  twig
      :+  %cncb  [%& 2]~
      =+  axe=3
      |-  ^-  (list (pair wing twig))
      ?~  yom  ~
      :-  [p.i.yom [%$ (peg axe 2)]]
      $(yom t.yom, axe (peg axe 3))
    ::
    ++  lace                                            ::  load and check
      |=  [cof=cafe for=mark bem=beam arg=heel]
      ^-  (bolt (unit vase))
      %+  cope  (lend cof bem)
      |=  [cof=cafe arc=arch]
      ?^  q.arc
        (cope (cope (liar cof bem) (lake for)) (fest (norm ska bem)))
      ?:  (~(has by r.arc) %hook)
        %+  cope  (fade cof %hook bem)
        |=  [cof=cafe hyd=hood]
        %+  cope  (cope (abut:(meow bem arg) cof hyd) (lake for))
        (fest (norm ska bem))
      (flue cof)
    ::
    ++  lake                                            ::  check/coerce
      |=  for=mark
      |=  [cof=cafe sam=vase]
      ^-  (bolt vase)
      %+  cool  |.(leaf/"ford: check {<[for bek `@p`(mug q.sam)]>}")
      ?:  ?=(?(%gate %core %door) for)
        ::  ~&  [%lake-easy for bek]
        (fine cof sam)
      ?:  ?=(?(%hoon %hook) for)
        =+  mas=((soft ,@t) q.sam)
        ?~  mas
          (flaw cof [leaf/"ford: bad hoon or hook: {<[for bek]>}"]~)
        (fine cof [%atom %t] u.mas)
      %+  cope  (fang cof for)
      |=  [cof=cafe tux=vase]
      =+  bob=(slot 6 tux)
      ?:  (~(nest ut p.bob) | p.sam)
        (fine cof sam)
      ?.  (slob %grab p.tux)
        (flaw cof [%leaf "ford: no grab: {<[for bek]>}"]~)
      =+  gab=(slap tux [%cnzy %grab])
      ?.  (slob %noun p.gab)
        (flaw cof [%leaf "ford: no noun: {<[for bek]>}"]~)
      %+  cope  (maul cof (slap gab [%cnzy %noun]) [%noun q.sam])
      |=  [cof=cafe pro=vase]
      ?>  (~(nest ut p.bob) | p.pro)
      ?:  =(q.pro q.sam) 
        (fine cof p.bob q.pro)
      (flaw cof [%leaf "ford: invalid content: {<[for bek]>}"]~)
    ::
    ++  lamp                                            ::  normalize version
      |=  [cof=cafe bem=beam]
      ^-  (bolt beam)
      ?:  ?=(%ud -.r.bem)  (fine cof bem)
      =+  von=(save ~ %cw bem(s ~))
      ?~  von  [p=cof q=[%1 [%w bem ~] ~ ~]]
      (fine cof bem(r [%ud ((hard ,@) +.+:(need u.von))]))
    ::
    ++  lave                                            ::  validate
      |=  [cof=cafe for=mark his=ship som=*]
      ^-  (bolt vase)
      ((lake for) cof [%noun som])
    ::
    ++  lane                                            ::  type infer
      |=  [cof=cafe typ=type gen=twig]
      %+  (cowl cof)  (mule |.((~(play ut typ) gen)))
      |=(ref=type ref)
    ::
    ++  lash                                            ::  atomic sequence
      |=  wox=$+(span (unit ,@))
      |=  [cof=cafe bem=beam]
      ^-  (bolt (map ,@ span))
      %+  cope  (lend cof bem)
      |=  [cof=cafe arc=arch]
      (fine cof (lark wox arc))
    ::
    ++  lear                                            ::  load vase
      |=  [cof=cafe bem=beam]
      ^-  (bolt cage)
      =+  von=(save ~ %cx bem)
      ?~  von
        [p=*cafe q=[%1 [[%x bem ~] ~ ~]]]
      ?~  u.von
        (flaw cof leaf/"lear: file unavailable" (smyt (tope bem)) ~)
      (fine cof u.u.von)
    ::
    ++  lend                                            ::  load arch
      |=  [cof=cafe bem=beam]
      ^-  (bolt arch)
      =+  von=(save ~ %cy bem)
      ?~  von  [p=cof q=[%1 [%y bem ~] ~ ~]]
      (fine cof ((hard arch) q.q:(need u.von)))
    ::
    ++  liar                                            ::  load vase
      |=  [cof=cafe bem=beam]
      ^-  (bolt vase)
      =+  von=(save ~ %cx bem)
      ?~  von
        [p=*cafe q=[%1 [[%x bem ~] ~ ~]]]
      ?~  u.von
        (flaw cof leaf/"liar: file unavailable" (smyt (tope bem)) ~)
      (fine cof q.u.u.von)
    ::
    ++  lily                                            ::  translation targets
      |=  [cof=cafe for=mark]
      ^-  (bolt (list ,@tas))
      =+  raf=(fang cof for)
      ?:  =(%2 -.q.raf)  (fine p.raf ~)
      %+  cope  raf
      |=  [cof=cafe vax=vase]
      %+  fine  cof
      %+  weld
        ^-  (list ,@tas)
        ?.  (slob %garb p.vax)  ~
        =+  gav=((soft (list ,@tas)) q:(slap vax [%cnzy %garb]))
        ?~(gav ~ u.gav)
      ?.  (slob %grow p.vax)  ~
      =+  gow=(slap vax [%cnzy %grow])
      (sloe p.gow)
    ::
    ++  lima                                            ::  load at depth
      |=  [cof=cafe for=mark bem=beam arg=heel]
      %+  (clef %bake)  [p=cof q=[%0 p=[bem `~] q=[for bem arg]]]
      |=  [cof=cafe for=mark bem=beam arg=heel]
      ^-  (bolt (unit vase))
      %+  cope  (lend cof bem)
      |=  [cof=cafe arc=arch]
      ^-  (bolt (unit vase))
      ?:  (~(has by r.arc) for)
        (lace cof for bem(s [for s.bem]) arg)
      =+  haz=(turn (~(tap by r.arc) ~) |=([a=@tas b=~] a))
      ?~  haz  (flue cof)
      %+  cope  (lion cof for haz)
      |=  [cof=cafe wuy=(list ,@tas)]
      ?~  wuy  (flue cof)
      %+  cope  (make cof %bake i.wuy bem arg)
      |=  [cof=cafe hoc=gage]
      %+  cope  (lope cof i.wuy t.wuy q.hoc)
      |=  [cof=cafe vax=vase]
      ((fest bem) cof vax)
    ::
    ++  lime                                            ::  load beam
      |=  [cof=cafe for=mark bem=beam arg=heel]
      =+  [mob=bem mer=(flop arg)]
      |-  ^-  (bolt vase)
      %+  cope  (lima cof for mob (flop mer))
      |=  [cof=cafe vux=(unit vase)]
      ?^  vux  (fine cof u.vux)
      ?~  s.mob
        %+  flag
          (norm ska mob)
        (flaw cof leaf/"beam unavailable" (smyt (tope bem)) ~)
      ^$(s.mob t.s.mob, mer [i.s.mob mer])
    ::
    ++  link                                            ::  translate
      |=  [cof=cafe too=mark for=mark vax=vase]
      ^-  (bolt vase)
      ?:  =(too for)  (fine cof vax)
      ?:  |(=(%noun for) =(%$ for))
        ((lake too) cof vax)
      %+  cope  (fang cof for)
      |=  [cof=cafe pro=vase]
      ?:  &((slob %grow p.pro) (slob too p:(slap pro [%cnzy %grow])))
        %+  cope  (keel cof pro [[%& 6]~ vax]~)
        |=  [cof=cafe pox=vase]
        (maim cof pox [%tsgr [%cnzy %grow] [%cnzy too]])
      %+  cope  (fang cof too)
      |=  [cof=cafe pro=vase]
      =+  ^=  zat  ^-  (unit vase)
          ?.  (slob %grab p.pro)  ~
          =+  gab=(slap pro [%cnzy %grab])
          ?.  (slob for p.gab)  ~
          `(slap gab [%cnzy for])
      ?~  zat
        (flaw cof [%leaf "ford: no link: {<[for too]>}"]~)
      (maul cof u.zat vax)
    ::
    ++  lion                                            ::  translation search
      |=  [cof=cafe too=@tas fro=(list ,@tas)]
      ^-  (bolt (list ,@tas))
      =|  war=(set ,@tas)
      =<  -:(apex (fine cof fro))
      |%
      ++  apex
        |=  rof=(bolt (list ,@tas))
        ^-  [(bolt (list ,@tas)) _+>]
        ?.  ?=(%0 -.q.rof)  [rof +>.$]
        ?~  q.q.rof
          [[p.rof [%0 p.q.rof ~]] +>.$]
        =^  orf  +>.$  (apse p.rof i.q.q.rof)
        ?.  ?=(%0 -.q.orf)
          [orf +>.$]
        ?~  q.q.orf
          $(p.rof p.orf, q.q.rof t.q.q.rof)
        [[p.orf [%0 (grom p.q.rof p.q.orf) q.q.orf]] +>.$]
      ::
      ++  apse
        |=  [cof=cafe for=@tas]
        ^-  [(bolt (list ,@tas)) _+>]
        ?:  =(for too)
          [(fine cof [too ~]) +>.$]
        ?:  (~(has in war) for)  [(flue cof) +>]
        =.  war  (~(put in war) for)
        =^  hoc  +>.$  (apex (lily cof for))
        :_  +>.$
        %+  cope  hoc
        |=  [cof=cafe ked=(list ,@tas)]
        (fine cof ?~(ked ~ [for ked]))
      --
    ::
    ++  lope                                            ::  translation pipe
      |=  [cof=cafe for=mark yaw=(list mark) vax=vase]
      ^-  (bolt vase)
      ?~  yaw  (fine cof vax)
      %+  cope  (link cof i.yaw for vax)
      |=  [cof=cafe yed=vase]
      ^$(cof cof, for i.yaw, yaw t.yaw, vax yed)
    ::
    ++  mail                                            ::  cached mint
      |=  [cof=cafe sut=type gen=twig]
      ^-  (bolt (pair type nock))
      %+  (clef %slim)  (fine cof sut gen) 
      |=  [cof=cafe sut=type gen=twig]
      =+  puz=(mule |.((~(mint ut sut) [%noun gen])))
      ?-  -.puz
        |  (flaw cof p.puz)
        &  (fine cof p.puz)
      ==
    ::
    ++  maim                                            ::  slap
      |=  [cof=cafe vax=vase gen=twig]
      ^-  (bolt vase)
      %+  cope  (mail cof p.vax gen)
      |=  [cof=cafe typ=type fol=nock]
      %+  (coup cof)  (mock [q.vax fol] (mole (slod save)))
      |=(val=* `vase`[typ val])
    ::
    ++  make                                            ::  reduce silk
      |=  [cof=cafe kas=silk]
      ^-  (bolt gage)
      :: ~&  [%make (,@tas -.kas)]
      ?-    -.kas
          ^
        %.  [cof p.kas q.kas]
        ;~  cope
          ;~  coax
            |=([cof=cafe p=silk q=silk] ^$(cof cof, kas p.kas))
            |=([cof=cafe p=silk q=silk] ^$(cof cof, kas q.kas))
          ==
        ::
          |=  [cof=cafe bor=gage heg=gage]  ^-  (bolt gage)
          [p=cof q=[%0 ~ [%$ (slop q.bor q.heg)]]]
        ==
      ::
          %bake
        ::  ~&  [%bake-start (tope q.kas)]
        %+  cool  |.(leaf/"ford: bake {<p.kas>} {<(tope q.kas)>}")
        %+  cope  (lima cof p.kas q.kas r.kas)
        |=  [cof=cafe vux=(unit vase)]
        ?~  vux
          (flaw cof leaf/"bake failed" (smyt (tope q.kas)) ~)
        (fine cof [p.kas u.vux])
      ::
          %boil
        ^-  (bolt gage)
        %+  cool  |.(leaf/"ford: boil {<p.kas>} {<(tope q.kas)>} {<r.kas>}")
        %+  cope  (lamp cof q.kas)
        |=  [cof=cafe bem=beam]
        %+  cope  (lime cof p.kas bem r.kas)
        |=  [cof=cafe vax=vase]
        (fine cof `gage`[p.kas vax])
      ::
          %bunt
        %+  cool  |.(leaf/"ford: bunt {<p.kas>}")
        ?:  ?=(?(%hoon %hook) p.kas)
          (fine cof p.kas [%atom %t] '')
        %+  cope  (fang cof p.kas)
        |=  [cof=cafe tux=vase]
        (fine cof [p.kas (slot 6 tux)])
      ::
          %call
        ::  %+  cool  |.(leaf/"ford: call {<`@p`(mug kas)>}")
        %.  [cof p.kas q.kas]
        ;~  cope
          ;~  coax
            |=([cof=cafe p=silk q=silk] ^$(cof cof, kas p))
            |=([cof=cafe p=silk q=silk] ^$(cof cof, kas q))
          ==
        ::
          |=  [cof=cafe gat=gage sam=gage]
          (maul cof q.gat q.sam)
        ::
          |=  [cof=cafe vax=vase]
          (fine cof %noun vax)
        ==
      ::
          %cast
        %+  cool  |.(leaf/"ford: cast {<p.kas>}")
        %+  cope  $(kas q.kas)
        |=  [cof=cafe cay=gage]
        ^-  (bolt gage)
        %+  cool  |.(leaf/"ford: casting {<p.cay>} to {<p.kas>}")
        ?.  ?=(@ p.cay)
          (flaw cof leaf/"bad cast marc" ~)
        %+  cope  (link cof p.kas p.cay q.cay)
        |=  [cof=cafe vax=vase]
        (fine cof [p.kas vax])
      ::
          %diff
        %+  cool  |.(leaf/"ford: diff {<`@p`(mug p.kas)>} {<`@p`(mug q.kas)>}")
        (diff cof p.kas q.kas)
      ::
          %done  [cof %0 p.kas q.kas]
          %dude  (cool |.(p.kas) $(kas q.kas))
          %dune
        ?~  q.kas  [cof [%2 p.kas [%leaf "no data"]~]]
        $(kas [%done p.kas u.q.kas])
      ::
          %file
        %+  cool  |.(leaf/"ford: file {<p.kas>}")
        (lear cof p.kas)
      ::
          %flag
        ?~  p.kas  $(kas q.kas)
        =+  dap=(~(get by deh.bay) p.kas)
        ?~  dap    ~&(flag-missed/p.kas $(kas q.kas))
        =+  dep=?-(-.u.dap %init p.u.dap, %sent q.u.dap, %done [[bek ~] ~ ~])
        =+  rez=$(kas q.kas)          ::  XX revisit ^ during dependency review
        ?:  ?=(%1 -.q.rez)  rez
        rez(p.q (~(uni in p.q.rez) `(set beam)`dep))
      ::
          %join
        %+  cool
          |.
          leaf/"ford: join {<p.kas>} {<`@p`(mug q.kas)>} {<`@p`(mug r.kas)>}"
        (join cof p.kas q.kas r.kas)
      ::
          %mash
        %+  cool
          |.
          leaf/"ford: mash {<p.kas>} {<`@p`(mug q.kas)>} {<`@p`(mug r.kas)>}"
        (mash cof p.kas q.kas r.kas)
      ::
          %mute  (kale cof p.kas q.kas)
          %pact
        %+  cool  |.(leaf/"ford: pact {<`@p`(mug p.kas)>} {<`@p`(mug q.kas)>}")
        (pact cof p.kas q.kas)
      ::
          %plan  
        %+  cope  (abut:(meow p.kas q.kas) cof r.kas)
        |=  [cof=cafe vax=vase]
        (fine cof %noun vax)
      ::
          %reef  (fine cof %noun pit)
          %ride
        %+  cool  |.(leaf/"ford: ride {<`@p`(mug kas)>}")
        %+  cope  $(kas q.kas)
        |=  [cof=cafe cay=gage]
        %+  cope  (maim cof q.cay p.kas)
        |=  [cof=cafe vax=vase]
        (fine cof %noun vax)
      ::
          %tabl
        %+  cope
          |-  ^-  (bolt (pair (list (pair marc marc)) vase))
          ?~  p.kas  (fine cof ~ *vase)
          %+  cope  (make cof p.i.p.kas)
          |=  [cof=cafe key=gage]
          %+  cope  (make cof q.i.p.kas)
          |=  [cof=cafe val=gage]
          %+  cope  ^^$(p.kas t.p.kas)
          |=  [cof=cafe rex=(list (pair marc marc)) rey=vase]
          (fine cof [[p.key p.val] rex] (slop (slop q.key q.val) rey))
        |=  [cof=cafe rex=(list (pair marc marc)) rey=vase]
        (fine cof [%tabl rex] rey)
      ::
          %vale  
        %+  cool  |.(leaf/"ford: vale {<p.kas>} {<q.kas>} {<`@p`(mug r.kas)>}")
        %+  cope  (lave cof p.kas q.kas r.kas)
        |=  [cof=cafe vax=vase]
        (fine cof `cage`[p.kas vax])
      ::
          %volt
        %+  cool  |.(leaf/"ford: volt {<p.q.kas>}")
        %+  cope  $(kas [%bunt p.q.kas])
        |=  [cof=cafe cay=gage]
        ^-  (bolt gage)
        [cof %0 p.kas p.q.kas p.q.cay q.q.kas]
      ==
    ::
    ++  malt                                            ::  cached slit
      |=  [cof=cafe gat=type sam=type]
      ^-  (bolt type)
      %+  (clef %slit)  (fine cof gat sam)
      |=  [cof=cafe gat=type sam=type]
      =+  top=(mule |.((slit gat sam)))
      ?-  -.top
        |  (flaw cof p.top)
        &  (fine cof p.top)
      ==
    ::
    ++  maul                                            ::  slam
      |=  [cof=cafe gat=vase sam=vase]
      ^-  (bolt vase)
      %+  cope  (malt cof p.gat p.sam)
      |=  [cof=cafe typ=type]
      %+  (coup cof)  (mong [q.gat q.sam] (mole (slod save)))
      |=(val=* `vase`[typ val])
    ::
    ++  meow                                            ::  assemble
      |=  [how=beam arg=heel] 
      =|  $:  rop=(map term (pair hoof twig))           ::  structure/complex
              bil=(map term (pair hoof twig))           ::  libraries known
              lot=(list term)                           ::  library stack
              zeg=(set term)                            ::  library guard
              boy=(list twig)                           ::  body stack
          ==
      ~%  %meow  ..meow
        ==
          %able  able
          %ably  ably
          %abut  abut
          %apex  apex
          %body  body
          %chad  chad
          %chai  chai
          %chap  chap
          %head  head
          %hone  hone
          %neck  neck
          %wilt  wilt
        ==
      |%
      ++  able                                          ::  assemble preamble
        ^-  twig
        :+  %tsgr
          ?:(=(~ rop) [%$ 1] [%brcn (~(run by rop) |=([* a=twig] [%ash a]))])
        [%tssg (turn (flop lot) |=(a=term q:(~(got by bil) a)))]
      ::
      ++  ably                                          ::  naked structure
        |=  [cof=cafe for=mark hyd=hood]                ::  XX unused
        ^-  (bolt twig)
        %+  cope  (apex cof hyd)
        |=  [cof=cafe sel=_..ably]
        =.  ..ably  sel
        %+  fine  cof
        ^-  twig
        :+  %tsgl  [%cnzy for]
        :-  %brcn
        ^-  (map term foot)
        %+  ~(put by `(map term foot)`(~(run by rop) |=([* a=twig] [%ash a])))
          for 
        [%ash [%tssg (flop boy)]]
      ::
      ++  abut                                          ::  generate
        |=  [cof=cafe hyd=hood]
        ^-  (bolt vase)
        %+  cope  (apex cof hyd)
        |=  [cof=cafe sel=_..abut]
        =.  ..abut  sel
        %+  cope  (maim cof pit able)
        |=  [cof=cafe bax=vase]
        %+  cope  (chap cof bax [%fan fan.hyd])
        |=  [cof=cafe gox=vase]
        %+  cope  (maim cof (slop gox bax) [%tssg (flop boy)])
        |=  [cof=cafe fin=vase]
        (fine cof fin) 
        ::  ~>  %slog.[0 ~(duck ut p.q.cay)]
      ::
      ++  apex                                          ::  build to body
        |=  [cof=cafe hyd=hood]
        ^-  (bolt ,_..apex)
        %+  cope  (body cof src.hyd)
        |=  [cof=cafe sel=_..apex]
        =.  ..apex  sel
        %+  cope  (neck cof lib.hyd)
        |=  [cof=cafe sel=_..apex]
        =.  ..apex  sel(boy boy)
        %+  cope  (head cof sur.hyd)
        |=  [cof=cafe sel=_..apex]
        (fine cof sel)
      ::
      ++  body                                          ::  produce functions
        |=  [cof=cafe src=(list hoop)]
        ^-  (bolt ,_..body)
        ?~  src  (fine cof ..body)
        %+  cope  (wilt cof i.src)
        |=  [cof=cafe sel=_..body]
        ^$(cof cof, src t.src, ..body sel)
      ::
      ++  chad                                          ::  atomic list
        |=  [cof=cafe bax=vase doe=term hon=horn]
        ^-  (bolt vase)
        %+  cope  ((lash (slat doe)) cof how)
        |=  [cof=cafe yep=(map ,@ span)]
        =+  ^=  poy  ^-  (list (pair ,@ span))
            %+  sort  (~(tap by yep) ~)
            |=([a=[@ *] b=[@ *]] (lth -.a -.b))
        %+  cope
          |-  ^-  (bolt (list (pair ,@ vase)))
          ?~  poy  (flue cof)
          %+  cope  $(poy t.poy)
          |=  [cof=cafe nex=(list (pair ,@ vase))]
          %+  cope  (chap(s.how [q.i.poy s.how]) cof bax hon)
          |=  [cof=cafe elt=vase]
          (fine cof [[p.i.poy elt] nex])
        |=  [cof=cafe yal=(list (pair ,@ vase))]
        %+  fine  cof
        |-  ^-  vase
        ?~  yal  [[%cube 0 [%atom %n]] 0]
        (slop (slop [[%atom doe] p.i.yal] q.i.yal) $(yal t.yal))
      ::
      ++  chai                                          ::  atomic map
        |=  [cof=cafe bax=vase hon=horn]
        ^-  (bolt vase)
        %+  cope  (lend cof how)
        |=  [cof=cafe arc=arch]
        %+  cope
          |-  ^-  (bolt (map ,@ vase))
          ?~  r.arc  (flue cof)
          %+  cope  $(r.arc l.r.arc)
          |=  [cof=cafe lef=(map ,@ vase)]
          %+  cope  `(bolt (map ,@ vase))`^$(cof cof, r.arc r.r.arc)
          |=  [cof=cafe rig=(map ,@ vase)]
          %+  cope  (chap(s.how [p.n.r.arc s.how]) cof bax hon)
          |=  [cof=cafe nod=vase]
          (fine cof [[p.n.r.arc nod] lef rig])
        |=  [cof=cafe doy=(map ,@ vase)]
        %+  fine  cof
        |-  ^-  vase
        ?~  doy  [[%cube 0 [%atom %n]] 0]
        %+  slop
          (slop [[%atom %ta] p.n.doy] q.n.doy)
        (slop $(doy l.doy) $(doy r.doy))
      ::
      ++  chap                                          ::  produce resources
        |=  [cof=cafe bax=vase hon=horn]
        ^-  (bolt vase)
        ?-    -.hon
            %ape  (maim cof bax p.hon)
            %arg  
          %+  cope  (maim cof bax p.hon)
          |=  [cof=cafe gat=vase]
          (maul cof gat !>([how arg]))
        ::
            %day  (chad cof bax %dr p.hon)
            %dub 
          %+  cope  $(hon q.hon)
          |=  [cof=cafe vax=vase]
          (fine cof [[%face p.hon p.vax] q.vax])
        ::
            %fan
          %+  cope
            |-  ^-  (bolt (list vase))
            ?~  p.hon  (flue cof)
            %+  cope  ^$(hon i.p.hon)
            |=  [cof=cafe vax=vase]
            %+  cope  ^$(cof cof, p.hon t.p.hon)
            |=  [cof=cafe tev=(list vase)]
            (fine cof [vax tev])
          |=  [cof=cafe tev=(list vase)]
          %+  fine  cof
          |-  ^-  vase
          ?~  tev  [[%cube 0 [%atom %n]] 0]
          (slop i.tev $(tev t.tev))
        ::
            %for  $(hon q.hon, s.how (weld (flop p.hon) s.how))
            %hel    
          %=  $  
            hon    q.hon
            arg    (scag p.hon arg)
            s.how  (weld (slag p.hon arg) s.how)
          ==
            %hub  (chad cof bax %ud p.hon)
            %man
          |-  ^-  (bolt vase)
          ?~  p.hon  (fine cof [[%cube 0 [%atom %n]] 0])
          %+  cope  $(p.hon l.p.hon)
          |=  [cof=cafe lef=vase]
          %+  cope  ^$(cof cof, p.hon r.p.hon)
          |=  [cof=cafe rig=vase]
          %+  cope  ^^^$(cof cof, hon q.n.p.hon)
          |=  [cof=cafe vax=vase]
          %+  fine  cof
          %+  slop
            (slop [[%atom %tas] p.n.p.hon] vax)
          (slop lef rig)
        ::
            %now  (chad cof bax %da p.hon)
            %nap  (chai cof bax p.hon)
            %see
          =.  r.p.hon  ?:(?=([%ud 0] r.p.hon) r.how r.p.hon)
          $(hon q.hon, how p.hon)
            %saw  
          %+  cope  $(hon q.hon)
          |=  [cof=cafe sam=vase]
          %+  cope  (maim cof bax p.hon)
          |=  [cof=cafe gat=vase]
          (maul cof gat sam)
        ::
            %sic
          %+  cope  $(hon q.hon)
          |=  [cof=cafe vax=vase]
          %+  cope  (maim cof bax [%bctr p.hon])
          |=  [cof=cafe tug=vase]
          ?.  (~(nest ut p.tug) | p.vax)
            (flaw cof [%leaf "type error: {<p.hon>} {<q.hon>}"]~)
          (fine cof [p.tug q.vax])
        ::
            %toy  (cope (make cof %boil p.hon how ~) feel)
        ==
      ::
      ++  head                                          ::  consume structures
        |=  [cof=cafe bir=(list hoot)]
        |-  ^-  (bolt ,_..head)
        ?~  bir  
          (fine cof ..head)
        =+  byf=(~(get by rop) p.q.i.bir)
        ?^  byf
          ?.  =(`hoof`q.i.bir `hoof`p.u.byf)
            (flaw cof [%leaf "structure mismatch: {<~[p.u.byf q.i.bir]>}"]~)
          $(bir t.bir)
        =+  bem=(hone ?:(p.i.bir %gate %core) %sur q.i.bir)
        %+  cope  (fade cof %hook bem)
        |=  [cof=cafe hyd=hood]
        %+  cope  (apex(boy ~) cof hyd)
        |=  [cof=cafe sel=_..head]
        =.  ..head
            %=  sel
              boy  ?:  p.i.bir  
                     boy
                   (welp boy [[[%cnzy p.q.i.bir] [%$ 1]] ~])
              zeg  zeg
              rop  %+  ~(put by (~(uni by rop) rop.sel))
                      p.q.i.bir 
                   [q.i.bir [%tssg (flop boy.sel)]]
            ==
        ^^$(cof cof, bir t.bir)
      ::
      ++  hone                                          ::  plant hoof
        |=  [for=@tas way=@tas huf=hoof]
        ^-  beam
        ?~  q.huf
          how(s ~[for p.huf way])
        [[q.u.q.huf q.how p.u.q.huf] ~[for p.huf way]]
      ::
      ++  neck                                          ::  consume libraries
        |=  [cof=cafe bir=(list hoof)]
        ^-  (bolt ,_..neck)
        ?~  bir  (fine cof ..neck)
        ?:  (~(has in zeg) p.i.bir)
          (flaw cof [%leaf "circular dependency: {<i.bir>}"]~)
        =+  goz=(~(put in zeg) p.i.bir)
        =+  byf=(~(get by bil) p.i.bir)
        ?^  byf
          ?.  =(`hoof`i.bir `hoof`p.u.byf)
            (flaw cof [%leaf "library mismatch: {<~[p.u.byf i.bir]>}"]~)
          $(bir t.bir)
        =+  bem=(hone %core %lib i.bir)
        %+  cope  (fade cof %hook bem)
        |=  [cof=cafe hyd=hood]
        %+  cope  (apex(zeg goz, boy ~) cof hyd)
        |=  [cof=cafe sel=_..neck]
        =.  ..neck  
            %=  sel
              zeg  zeg
              lot  [p.i.bir lot]
              bil  (~(put by bil) p.i.bir [i.bir [%tssg (flop boy.sel)]])
            ==
        ^^$(cof cof, bir t.bir)
      ::
      ++  wilt                                          ::  process body entry
        |=  [cof=cafe hop=hoop]
        ^-  (bolt ,_..wilt)
        ?-    -.hop
            %&  (fine cof ..wilt(boy [p.hop boy]))
            %| 
          =.  r.p.hop  ?:(?=([%ud 0] r.p.hop) r.how r.p.hop)
          %+  cool  |.(leaf/"ford: wilt {<[(tope p.hop)]>}")
          %+  cope  (lend cof p.hop)
          |=  [cof=cafe arc=arch]
          ?:  (~(has by r.arc) %hoon)
            %+  cope  (fade cof %hoon p.hop)
            |=  [cof=cafe hyd=hood]
            %+  cope  (apex(boy ~) cof hyd)
            |=  [cof=cafe sel=_..wilt]
            (fine cof sel(boy [[%tssg boy.sel] boy]))
          =+  [all=(lark (slat %tas) arc) sel=..wilt]
          %+  cope
            |-  ^-  (bolt (pair (map term foot) ,_..wilt))
            ?~  all  (fine cof ~ ..wilt)
            %+  cope  $(all l.all)
            |=  [cof=cafe lef=(map term foot) sel=_..wilt]
            %+  cope  ^$(all r.all, cof cof, sel sel)
            |=  [cof=cafe rig=(map term foot) sel=_..wilt]
            %+  cope  
              %=    ^^^^$
                  cof      cof
                  ..wilt   sel(boy ~)
                  s.p.hop  [p.n.all s.p.hop]
              ==
            |=  [cof=cafe sel=_..wilt]
            %+  fine  cof
            [`(map term foot)`[[p.n.all [%ash [%tssg boy.sel]]] lef rig] sel]
          |=  [cof=cafe mav=(map term foot) sel=_..wilt]
          ?~  mav
            (flaw cof [%leaf "source missing: {<(tope p.hop)>}"]~)
          (fine cof sel(boy [[%brcn mav] boy]))
        ==
      --
    ::
    ++  pact                                            ::  patch
      |=  [cof=cafe kas=silk kos=silk]
      ^-  (bolt gage)
      %.  [cof kas kos]
      ;~  cope
        ;~  coax
          |=([cof=cafe p=silk q=silk] (make cof p))
          |=([cof=cafe p=silk q=silk] (make cof q))
        ==
        |=  [cof=cafe cay=gage coy=gage]
        ?.  &(?=(@ p.cay) ?=(@ p.coy))
          (flaw cof leaf/"bad pact marc" ~)
        ?:  ?=(?(%hoon %hook) p.cay)
          ?.  ?=(%txt-diff p.coy)
            (flaw cof leaf/"{<p.cay>} mark with bad diff type: {<p.coy>}" ~)
          =+  txt=((soft ,@t) q.q.cay)
          ?~  txt
            (flaw cof leaf/"{<p.cay>} mark on bad data" ~)
          =+  dif=((soft (urge cord)) q.q.coy)
          ?~  dif
            =-  (flaw cof leaf/"{<p.cay>} data with bad diff" -)
            [>type=p.q.coy< >shouldbe=-:!>(*(urge cord))< ~]
          =+  pac=(role (lurk (lore (cat 3 u.txt '\0a')) u.dif))
          (fine cof p.cay [%atom %t] (end 3 (dec (met 3 pac)) pac))
        %+  cope  (fang cof p.cay)
        |=  [cof=cafe pro=vase]
        ?.  (slab %grad p.pro)
          (flaw cof leaf/"no ++grad" ~)
        =+  gar=(slap pro [%cnzy %grad])
        ?.  (slab %form p.gar)
          ?.  (slab %sted p.gar)
            (flaw cof leaf/"no ++form:grad nor ++sted:grad" ~)
          =+  for=((soft ,@tas) q:(slap gar [%cnzy %sted]))
          ?~  for
            (flaw cof leaf/"bad ++sted:grad" ~)
          (make cof %cast p.cay %pact [%cast u.for [%done ~ cay]] [%done ~ coy])
        =+  for=((soft ,@tas) q:(slap gar [%cnzy %form]))
        ?~  for
          (flaw cof leaf/"bad ++form:grad" ~)
        ?.  =(u.for p.coy)
          %+  flaw  cof  :_  ~
          =<  leaf/"pact on data with wrong form: {-} {+<} {+>}"
          [(trip p.cay) (trip u.for) (trip p.coy)]
        ?.  (slab %pact p.gar)
          (flaw cof leaf/"no ++pact:grad" ~)
        %+  cope  (keel cof pro [[%& 6]~ q.cay]~)
        |=  [cof=cafe pox=vase]
        %+  cope
          %^  maul  cof
            (slap (slap pox [%cnzy %grad]) [%cnzy %pact])
          q.coy
        |=  [cof=cafe pat=vase]
        (fine cof p.cay pat)
      ==
    ::
    ++  resp
      |=  [tik=@ud rot=riot]
      ^+  ..zo
      ?>  (~(has by q.kig) tik)
      =+  `[ren=care bem=beam]`(~(got by q.kig) tik)
      ?~  rot
        =^  dep  deh.bay  (daze ~)                      ::  dependencies?
        amok:(expo [%made dep %| (smyt ren (tope bem)) ~])
      =+  (cat 3 'c' ren)
      exec(q.kig (~(del by q.kig) tik), keg (~(put by keg) [- bem] r.u.rot))
    ::
    ++  save
      ^-  sled
      |=  [(unit (set monk)) tem=term bem=beam]
      ^-  (unit (unit cage))
      =+  (~(get by keg) tem bem)
      ?^  -
        ``u.-
      (ska +<.$)
    --
  --
::
--
.  ==
=|  axle
=*  lex  -
|=  [now=@da eny=@ ski=sled]                            ::  activate
^?                                                      ::  opaque core
~%  %ford-d  ..is  ~
|%                                                      ::
++  call                                                ::  request
  |=  [hen=duct hic=(hypo (hobo kiss))]
  ^-  [p=(list move) q=_..^$]
  =>  .(q.hic ?.(?=(%soft -.q.hic) q.hic ((hard kiss) p.q.hic)))
  ?:  ?=(%wegh -.q.hic)
    :_  ..^$  :_  ~
    :^  hen  %give  %mass
    :-  %ford
    :-  %|
    :~  lex/`lex
    ==
  =+  ^=  our  ^-  @p
      ?-  -.q.hic
        %exec  p.q.hic
        %wasp  p.q.hic
      ==
  =+  ^=  bay  ^-  baby
      =+  buy=(~(get by pol.lex) our)
      ?~(buy *baby u.buy)
  =^  mos  bay
    ?:  ?=(%wasp -.q.hic)
      abet:(~(awap za [[our *beak hen] [now eny ski] ~] bay) q.q.hic)
    =+  bek=-:(norm ski q.q.hic ~)
    abet:(~(apex za [[our bek hen] [now eny ski] ~] bay) r.q.hic)
  [mos ..^$(pol (~(put by pol) our bay))]
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load                                                ::  highly forgiving
  |=  old=axle
  ..^$(+>- old)
  ::=.  old  
  ::    ?.  ?=([%0 *] old)  old                           ::  remove at 1
  ::    :-  %1 
  ::    |-  ^-  *
  ::    ?~  +.old  ~
  ::    ?>  ?=([n=[p=* q=[tad=* dym=* deh=* jav=*]] l=* r=*] +.old)
  ::    :-  [p.n.+.old [tad.q.n.+.old dym.q.n.+.old deh.q.n.+.old ~]]
  ::    [$(+.old l.+.old) $(+.old r.+.old)]
::   =+  lox=((soft axle) old)
::   ^+  ..^$
::   ?~  lox
::     ~&  %ford-reset
::     ..^$
::   ..^$(+>- u.lox)
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit cage))
  [~ ~]
::
++  stay                                                ::  save w/o cache
  `axle`+>-.$(pol (~(run by pol) |=(a=baby [tad.a dym.a deh.a ~])))
::
++  take                                                ::  response
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^-  [p=(list move) q=_..^$]
  ?>  ?=([@ @ *] tea)
  =+  our=(slav %p i.tea)
  =+  bay=(~(got by pol.lex) our)
  =^  mos  bay
    =+  dep=(slaw %uv i.t.tea)
    ?^  dep
      =+  [bek sup]=(need (tome t.t.tea))
      abet:(~(axun za [[our bek hen] [now eny ski] ~] bay) tea u.dep sup q.hin)
    ?>  ?=([@ @ @ @ ~] t.t.tea)
    =+  :*  num=(slav %ud i.t.tea)
            tik=(slav %ud i.t.t.tea)
            bek=-:(need (tome t.t.t.tea)) 
        ==
    abet:(~(axon za [[our bek hen] [now eny ski] ~] bay) num tik q.hin)
  [mos ..^$(pol (~(put by pol) our bay))]
--
