!:::::  
::  ::  %ford, new execution control
!?  164
::::
|=  pit+vase   
=>  =~
::  structures
|%
++  gift  gift-ford                                     ::  out result <-$
++  heel  path                                          ::  functional ending
++  kiss  kiss-ford                                     ::  in request ->$
++  move  {p+duct q+(mold note gift)}                   ::  local move
++  note                                                ::  out request $->
          _%  _:  $c                                    ::  to %clay
          _%  {$warp p+sock q+riff}                     ::
          ==  ==  ==                                    ::
++  sign                                                ::  in result $<-
          _%  _:  $c                                    ::  by %clay
          _%  {$writ p+riot}                            ::
          ==  ==  ==                                    ::
--                                                      ::
|%                                                      ::  structures
++  axle                                                ::  all %ford state
  _:  $1                                                ::  version for update
      pol+(map ship baby)                               ::
  ==                                                    ::
++  baby                                                ::  state by ship
  _:  tad+{p+@ud q+(map @ud task)}                      ::  tasks by number
      dym+(map duct @ud)                                ::  duct to task number
      deh+(map @uvH deps)                               ::  depends by hash
      jav+(map * calx)                                  ::  cache
  ==                                                    ::
++  bolt                                                ::  gonadic edge
  |*  a+_+(* *)                                         ::  product clam
  _:  p+cafe                                            ::  cache
    _=  q                                               ::
      _%  {$0 p+(set beam) q+a}                         ::  depends/product
          {$1 p+(set {p+care q+beam r+tang})}           ::  blocks
          {$2 p+(set beam) q+tang}                      ::  depends/error
      ==                                                ::
  ==                                                    ::
::                                                      ::
++  burg                                                ::  gonadic rule
  |*  {a+_+(* *) b+_+(* *)}                             ::  from and to
  _+({c+cafe d+a} (bolt b))                             ::
::                                                      ::
++  cafe                                                ::  live cache
  _:  p+(set calx)                                      ::  used
      q+(map * calx)                                    ::  cache
  ==                                                    ::
::                                                      ::
++  calm                                                ::  cache metadata
  _:  laz+@da                                           ::  last accessed
      dep+(set beam)                                    ::  dependencies
  ==                                                    ::
++  calx                                                ::  concrete cache line
  _%  {$hood p+calm q+(pair beam cage) r+hood}          ::  compile
      {$bake p+calm q+(trel mark beam heel) r+(unit vase)}::  load
      {$lilt p+calm q+beak r+(jug mark mark)}           ::  translation graph
      {$slit p+calm q+{p+type q+type} r+type}           ::  slam type
      {$slim p+calm q+{p+type q+twig} r+(pair type nock)}::  mint
      {$slap p+calm q+{p+vase q+twig} r+vase}           ::  compute
      {$slam p+calm q+{p+vase q+vase} r+vase}           ::  compute
  ==                                                    ::
++  deps                                                ::  depend state
  _%  {$init p+(set beam)}                              ::  given out
      {$sent p+(set duct) q+(set beam)}                 ::  listener exists
      {$done $~}                                         ::  change seen
  ==                                                    ::
++  task                                                ::  problem in progress
  _:  nah+duct                                          ::  cause
      {bek+beak kas+silk}                               ::  problem
      keg+(map (pair term beam) cage)                   ::  block results
      kig+{p+@ud q+(map @ud {p+care q+beam})}           ::  blocks
  ==                                                    ::
++  gagl  (list (pair gage gage))                    
--                                                      ::
|%                                                      ::
++  calf                                                ::  reduce calx
  |*  sem+*                                             ::  a typesystem hack
  |=  cax+calx
  ?+  sem  !!
    $hood  ?>(?=($hood -.cax) r.cax)
    $bake  ?>(?=($bake -.cax) r.cax)
    $lilt  ?>(?=($lilt -.cax) r.cax)
    $slap  ?>(?=($slap -.cax) r.cax)
    $slam  ?>(?=($slam -.cax) r.cax)
    $slim  ?>(?=($slim -.cax) r.cax)
    $slit  ?>(?=($slit -.cax) r.cax)
  ==
::
++  calk                                                ::  cache lookup
  |=  a+cafe                                            ::
  |=  {b+@tas c+*}                                      ::
  ^-  {(unit calx) cafe}                                ::
  =+  d=(~(get by q.a) [b c])                           ::
  ?~  d  [~ a]                                          ::
  [d a(p (~(put in p.a) u.d))]                          ::
::                                                      ::
++  came                                                ::
  |=  {a+cafe b+calx}                                   ::  cache install
  ^-  cafe                                              ::
  a(q (~(put by q.a) [-.b q.b] b))                      ::
::                                                      ::
++  chub                                                ::  cache merge
  |=  {a+cafe b+cafe}                                   ::
  ^-  cafe                                              ::
  [(grom p.a p.b) (grum q.a q.b)]                       ::
::                                                      ::
++  faun  (flux |=(a+vase [%& %noun a]))                ::  vase to gage
++  feel  (flux |=(a+cage q.a))                         ::  cage to vase
++  furl                                                ::  unwrap gage to cage
  |=  {a+cafe b+gage}  ^-  (bolt cage)
  ?-  -.b
    $tabl  (flaw a >%bad-marc< ~)
    {$|}   (flaw a p.b)
    {$&}   (fine a p.b)
  ==
::
++  fret                                                ::  lift error
  |=  a+(bolt gage)  ^-  (bolt gage)
  ?.  ?=($2 -.q.a)  a
  [p.a [%0 p.q.a `gage`[%| q.q.a]]]
::
++  fine  |*  {a+cafe b+*}                              ::  bolt from data
          [p=`cafe`a q=[%0 p=*(set beam) q=b]]          ::
++  flaw  |=  {a+cafe b+tang}                           ::  bolt from error
          [p=a q=[%2 p=*(set beam) q=b]]                ::
++  flag                                                ::  beam into deps
  |*  {a+beam b+(bolt)}                                 ::
  ?:  ?=($1 -.q.b)  b
  =.  p.q.b  (~(put in p.q.b) a)
  b
::                                                      ::
++  flue  |=(a+cafe (fine a ~))                         ::  cafe to empty
++  flux  |*  a+__(*)                                   ::  bolt lift (fmap)
          |*({b+cafe c+__(+<.a)} (fine b (a c)))        ::
++  grom                                                ::  merge sets
  |*  {one+(set) two+(set)}
  ^+  one
  (~(gas in one) (~(tap in two) ~))                     ::  XX ugh
::
++  grum                                                ::  merge maps
  |*  {one+(map) two+(map)}
  ^+  one
  (~(gas by one) (~(tap by two) ~))                     ::  XX ugh
::
++  lark                                                ::  filter arch names
  |=  {wox+_+(span (unit @)) arc+arch}
  ^-  (map @ span)
  %-  ~(gas by *(map @ span))
  =|  rac+(list (pair @ span))
  |-  ^+  rac
  ?~  dir.arc  rac
  =.  rac  $(dir.arc l.dir.arc, rac $(dir.arc r.dir.arc))
  =+  gib=(wox p.n.dir.arc)
  ?~(gib rac [[u.gib p.n.dir.arc] rac])
::
++  tack                                                ::  fold path to term
  |=  a+{i+term t+(list term)}  ^-  term
  (rap 3 |-([i.a ?~(t.a ~ ['-' $(a t.a)])]))
::
++  tear                                                ::  split term
  =-  |=(a+term (rush a (most hep sym)))
  sym=(cook crip ;~(plug low (star ;~(pose low nud))))
::
++  norm                                                ::  normalize beam rev
  |=  {ska+sled bem+beam}
  %_  bem
    r  ?:  ?=($ud -.r.bem)  r.bem
       =+  num=(ska ~ %cw bem(s ~))
       ?.  ?=({$~ $~ * * @u} num)
         ~&  norm-lost/(tope bem(s ~))
         r.bem  ::  XX
       [%ud q.q.u.u.num]
  ==
::
++  za                                                  ::  per event
  =|  _:  _:  our+ship                                  ::  computation owner
              hen+duct                                  ::  event floor
              _:  now+@da                               ::  event date
                  eny+@                                 ::  unique entropy
                  ska+sled                              ::  system namespace
              ==                                        ::
              mow+(list move)                           ::  pending actions
          ==                                            ::
          bay+baby                                      ::  all owned state
      ==                                                ::
  |%
  ++  abet                                              ::  resolve
    ^-  {(list move) baby}
    [(flop mow) bay]
  ::
  ++  apax                                              ::  call
    ^+  ..apax
    =+  nym=(~(get by dym.bay) hen)
    ?~  nym                                             ::  XX should never
      ~&  [%ford-mystery hen]
      ..apax
    =+  tas=(need (~(get by q.tad.bay) u.nym))
    amok:~(camo zo [u.nym tas])
  ::
  ++  apex
    |=  kub+bilk
    ^+  +>
    =+  num=p.tad.bay
    ?<  (~(has by dym.bay) hen)
    =:  p.tad.bay  +(p.tad.bay)
        dym.bay    (~(put by dym.bay) hen num)
      ==
    ~(exec zo [num `task`[hen kub ~ 0 ~]])
  ::
  ++  axon                                              ::  take
    |=  {num+@ud tik+@ud sih+sign}
    ^+  +>
    ?-    -.+.sih
        $writ
      =+  tus=(~(get by q.tad.bay) num)
      ?~  tus
        ~&  [%ford-lost num]
        +>.$
      (~(resp zo [num u.tus]) tik p.+.sih)
    ==
  ::
  ++  axun                                              ::  take rev update
    |=  {tea+wire dep+@uvH bem+beam sih+sign}
    ^+  +>
    ?-    -.+.sih
        $writ
      ?~  p.sih  +>.$ 
      :: ~&  writ/tea
      =+  dap=(~(got by deh.bay) dep)
      =-  +>.$(mow mow, deh.bay ?~(dop deh.bay (~(put by deh.bay) dep dop)))
      ^-  {dop+_|($~ __(dap)) mow+__(mow)}
      ?-    -.dap
          $done  `mow                ::  writ redundant
          $init  ~|(never-subscribed/dep !!)
          $sent
        :-  [%done ~]
        ;:  weld
          (axap dep (~(del in q.dap) bem))              ::  cancel outstanding
          (turn (~(tap in p.dap)) |=(hen+duct [hen %give %news dep]))
          mow
    ==  ==
      ==
  ::
  ++  axap                                              ::  unsubscribe beams
    |=  {dep+@uvH dap+(set beam)}
    %+  turn  (~(tap in dap))
    |=  bem+beam
    :^  hen  %pass  [(scot %p our) (scot %uv dep) (tope bem)]
    [%c %warp [our p.bem] q.bem ~]
  ::
  ++  awap                                              ::  get next revision
    ~%  %ford-w  ..is  ~
    |=  {dep+@uvH ask+?}
    ?~  dep
      ~&(dep-empty/hen +>.$)
    ?:  =(dep 0vtest)                 ::  upstream testing
      +>.$(mow ?.(ask mow :_(mow [hen %give %news dep])))
    =+  dap=(~(get by deh.bay) dep)
    ?~  dap  ~&(dep-missed/dep +>.$)  ::  XX  ~|  !!
    :: ~&  awap/[dep u.dap]
    ?-  -.u.dap
      $done  +>.$(mow ?.(ask mow :_(mow [hen %give %news dep])))
      $sent
        =.  p.u.dap  
          ?:  ask  (~(put in p.u.dap) hen)
          (~(del in p.u.dap) hen)
        ?^  p.u.dap
          +>.$(deh.bay (~(put by deh.bay) dep u.dap))
        =.  mow  (weld (axap dep q.u.dap) mow)
        +>.$(deh.bay (~(put by deh.bay) dep [%init q.u.dap]))
    ::
      $init
        ?.  ask  ~&(awap-kill-empty/dep +>.$)  :: crash?
        %_    +>.$
            deh.bay 
          (~(put by deh.bay) dep [%sent [hen ~ ~] p.u.dap])
        ::
            mow
          =<  (welp :_(mow (turn (~(tap in p.u.dap)) .)))
          |=  bem+beam 
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
        %lion  lion
        %lily  lily
        %lope  lope
        %make  make
        %meow  meow
      ==
    |_  {num+@ud task}
    ++  abet  %_(..zo q.tad.bay (~(put by q.tad.bay) num +<+))
    ++  amok  
      %_  ..zo  
        q.tad.bay  (~(del by q.tad.bay) num)
        dym.bay    (~(del by dym.bay) nah)
      ==
    ++  camo                                            ::  stop requests
      ^+  .
      =+  kiz=(~(tap by q.kig) *(list {p+@ud q+{p+care q+beam}}))
      |-  ^+  +>
      ?~  kiz  +>
      %=    $
          kiz  t.kiz
          mow
        :_  mow
        :-  hen
        :^    %pass
            [(scot %p our) (scot %ud num) (scot %ud p.i.kiz) ~]
          %c
        [%warp [our p.q.q.i.kiz] q.q.q.i.kiz ~]
      ==
    ::
    ++  camp                                            ::  request a file
      |=  {ren+care bem+beam}
      ^+  +>
      %=    +>
          kig  [+(p.kig) (~(put by q.kig) p.kig [ren bem])]
          mow
        :_  mow
        :-  hen
        :^    %pass
            [(scot %p our) (scot %ud num) (scot %ud p.kig) ~]
          %c
        ~&  >>  [%camping ren bem]
        [%warp [our p.bem] q.bem [~ %sing ren r.bem (flop s.bem)]]
      ==
    ::
    ++  clef                                            ::  cache a result
      |*  sem+*
      |*  {hoc+(bolt) fun+(burg)}
      ?-    -.q.hoc
          $2  hoc
          $1  hoc
          $0
        =^  cux  p.hoc  ((calk p.hoc) sem q.q.hoc)
        ?^  cux
          [p=p.hoc q=[%0 p=dep.p.u.cux q=((calf sem) u.cux)]]
        =+  nuf=(cope hoc fun)
        ?-    -.q.nuf
            $2  nuf
            $1  nuf
            $0
          :: ~&  :-  %clef-new
          ::     ?+  sem  `@tas`sem
          ::       %hood  [%hood (tope &1.q.q.hoc)]
          ::       %bake  [%bake `mark`&1.q.q.hoc (tope &2.q.q.hoc)]
          ::     ==
          :-  p=(came p.nuf `calx`[sem `calm`[now p.q.nuf] q.q.hoc q.q.nuf])
          q=q.nuf
        ==
      ==
    ::
    ++  coax  !.                                        ::  bolt across
      |*  {hoc+(bolt) fun+(burg)}
      ?-  -.q.hoc
        $0  =+  nuf=$:fun(+<- p.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              $0  [%0 p=(grom p.q.hoc p.q.nuf) q=[q.q.hoc q.q.nuf]]
              $1  q.nuf
              $2  q.nuf
            ==
        $1  =+  nuf=$:fun(+<- p.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              $0  q.hoc
              $1  [%1 p=(grom p.q.nuf p.q.hoc)]
              $2  q.nuf
            ==
        $2  hoc
      ==
    ::
    ++  cool                                            ::  error caption
      |*  {cyt+_|(term (trap tank)) hoc+(bolt)}
      ?.  ?=($2 -.q.hoc)  hoc
      [p=p.hoc q=[%2 p=p.q.hoc q=[?^(cyt *cyt [>`@tas`cyt<]~) q.q.hoc]]]
    ::
    ++  cope                                            ::  bolt along
      |*  {hoc+(bolt) fun+(burg)}
      ?-  -.q.hoc
        $1  hoc
        $2  hoc
        $0  =+  nuf=(fun p.hoc q.q.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              $1  q.nuf
              $2  [%2 p=(grom `__(p.q.nuf)`p.q.hoc p.q.nuf) q=q.q.nuf]
              $0  [%0 p=(grom `__(p.q.nuf)`p.q.hoc p.q.nuf) q=q.q.nuf]
      ==    ==
    ::
    ++  coop                                            ::  bolt alter
      |*  {hoc+(bolt) fun+(burg)}
      ?-  -.q.hoc
        $1  hoc
        $0  hoc
        $2  =+  nuf=(fun(+<- p.hoc))
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              $1  q.nuf
              $0  [%0 p=(grom `__(p.q.nuf)`p.q.hoc p.q.nuf) q=q.q.nuf]
              $2  =.  q.q.nuf  (welp q.q.nuf q.q.hoc)
                  [%2 p=(grom `__(p.q.nuf)`p.q.hoc p.q.nuf) q=q.q.nuf]
      ==    ==
    ::
    ++  coup                                            ::  toon to bolt
      |=  cof+cafe
      |*  {ton+toon fun+_+(* *)}
      :-  p=cof
      ^=  q
      ?-  -.ton
        $2  [%2 p=*(set beam) q=p.ton]
        $0  [%0 p=*(set beam) q=(fun p.ton)]
        $1  ::  ~&  [%coup-need ((list path) p.ton)]
            =-  ?-  -.faw
                  {$&}  :-  %1
                        ^=  p
                        %-  sa
                        %+  turn  p.faw
                        |=(a+{care beam} [-.a +.a *tang])
                  {$|}  [%2 p=*(set beam) q=p.faw]
                ==
            ^=  faw
            |-  ^-  (each (list (pair care beam)) tang)
            ?~  p.ton  [%& ~]
            =+  nex=$(p.ton t.p.ton)
            =+  err=|=(a+tape [%| leaf/a ?:(?=($& -.nex) ~ p.nex)])
            =+  pax=(path i.p.ton)
            ?~  pax  (err "blocking empty")
            ?.  ?=($c (end 3 1 i.pax))
              (err "blocking not clay")
            =+  ren=((soft care) (rsh 3 1 i.pax))
            ?~  ren
              (err "blocking not care")
            =+  zis=(tome t.pax)
            ?~  zis
              (err "blocking not beam")
            ?-  -.nex
              {$&}  [%& [u.ren u.zis] p.nex]
              {$|}  nex
            ==
      ==
    ::
    ++  cowl                                            ::  each to bolt
      |=  cof+cafe
      |*  {tod+(each * tang) fun+_+(* *)}
      %+  (coup cof)
        ?-  -.tod
          {$&}  [%0 p=p.tod]
          {$|}  [%2 p=p.tod]
        ==
      fun
    ::
    ++  tabl-run                                        ::  apply to all elems
      |=  fun+(burg cage gage)
      |=  {cof+cafe gag+gage}
      ^-  (bolt gage)
      ?.  ?=($tabl -.gag)
        (cope (furl cof gag) fun)
      %-  cope  :_  (flux |=(rex+gagl [%tabl rex]))
      |-  ^-  (bolt (list (pair gage gage)))
      ?~  p.gag  (fine cof ~)
      %.  [cof p.gag]
      ;~  cope
        ;~  coax
          |=({cof+cafe {^ q+gage} t+gagl} (fret ^^$(cof cof, gag q)))
          |=({cof+cafe ^ t+gagl} ^$(cof cof, p.gag t))
        ==
        (flux |=({v+gage t+gagl} [[p.i.p.gag v] t]))
      ==
    ::
    ++  some-in-map
      |*  fun+(burg span (unit))
      =+  res=__((need [?+(-.q !! $0 q.q)]:*fun))
      =+  marv=(map span res)
      |=  {cof+cafe sud+(map span $~)}  ^-  (bolt marv)
      ?~  sud  (flue cof)
      %.  [cof sud]
      ;~  cope
        ;~  coax
          |=({cof+cafe __(sud)} ^$(cof cof, sud l))
          |=({cof+cafe __(sud)} ^$(cof cof, sud r))
          |=  {cof+cafe {dir+@ta $~} ^}
          %+  cope  (fun cof dir)
          (flux (lift |*(* [dir +<])))
        ==
        %-  flux
        |=  {lam+marv ram+marv nod+(unit {span res})}
        ?^(nod [u.nod lam ram] (~(uni by lam) ram))
      ==
    ++  dash                                          ::  process cache
      |=  cof+cafe
      ^+  +>
      :: ~?    |
      ::     :+  %dash  (lent (skim (~(tap in p.cof)) |=(a=calx ?=($bake -.a))))
      ::     =.  q.cof  (~(dif by q.cof) jav.bay)
      ::     =+  num=10
      ::     %.  |=  a+(list calx) 
      ::         =+  len=(lent a)
      ::         =-  [len (scag num (turn a f)) ?:((gth len num) %etc ~)]
      ::         =+  dewe=|=(beam +<(s ?+(s s {@ $web *} t.t.s)))
      ::         f=|=(b+calx [-.b ?+(-.b ~ %bake [p.q.b (tope (dewe q.q.b))])])
      ::     %~  run  by
      ::     =<  `(jar term calx)`(~(rep by q.cof) .)
      ::     |=  {{* a+calx} b+(jar term calx)}
      ::     (~(add ja b) -.a a)
      %_(+> jav.bay q.cof)
    ::
    ++  diff                                            ::  diff
      |=  {cof+cafe kas+silk kos+silk}
      ^-  (bolt gage)
      %.  [cof kas kos]
      ;~  cope
        ;~  coax
          |=({cof+cafe p+silk q+silk} (cope (make cof p) furl))
          |=({cof+cafe p+silk q+silk} (cope (make cof q) furl))
        ==
        |=  {cof+cafe cay+cage coy+cage}  ^-  (bolt gage)
        ?.  =(p.cay p.coy)
          %+  flaw  cof  :_  ~
          leaf/"diff on data of different marks: {(trip p.cay)} {(trip p.coy)}"
        ?:  =(q.q.cay q.q.coy)
          (fine cof %& %null [%atom %n] ~)
        ::
        %+  cope  (fang cof p.cay)
        |=  {cof+cafe pro+vase}
        ?.  (slab %grad p.pro)
          (flaw cof leaf/"no ++grad" ~)
        =+  gar=(slap pro [%cnzy %grad])
        ?@  q.gar
          =+  for=((sand %tas) q.gar)
          ?~  for  (flaw cof leaf/"bad mark ++grad" ~)
          %^  make  cof  %diff
          :-  [%cast u.for `cay]
          [%cast u.for `coy]
        ?.  (slab %form p.gar)
          (flaw cof leaf/"no ++form:grad" ~)
        ?.  (slab %diff p.gar)
          (flaw cof leaf/"no ++diff:grad" ~)
        %+  cope  (keel cof pro [[%& 6]~ q.cay]~)
        |=  {cof+cafe pox+vase}
        %+  cope
          %^  maul  cof
            (slap (slap pox [%cnzy %grad]) [%cnzy %diff])
          q.coy
        |=  {cof+cafe dif+vase}
        =+  for=((soft @tas) q:(slap gar [%cnzy %form]))
        ?~  for
          (flaw cof leaf/"bad ++form:grad" ~)
        (fine cof %& u.for dif)
      ==
    ::
    ++  daze                                            ::  remember depends
      |=  dep+(set beam)
      ^+  [*@uvH deh.bay]
      =.  dep
        =<  (sa (skip (~(tap in dep)) .))
        |=  dap+beam  ^-  ?
        ?~  s.dap  |
        =>(.(s.dap t.s.dap) |((~(has in dep) dap) $))
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
        $0  =^  dep  deh.bay  (daze p.q.bot)
            amok:(expo [%made dep q.q.bot])
        $2  =^  dep  deh.bay  (daze p.q.bot)
            amok:(expo [%made dep %| q.q.bot])
        $1  =+  zuk=(~(tap by p.q.bot) ~)
            =<  abet
            |-  ^+  ..exec
            ?~  zuk  ..exec
            $(zuk t.zuk, ..exec `__(..exec)`(camp p.i.zuk q.i.zuk))
      ==
    ::
    ++  expo                                            ::  return gift
      |=  gef+gift
      %_(+> mow :_(mow [hen %give gef]))
    ::
    ++  fade                                            ::  compile to hood
      |=  {cof+cafe for+mark bem+beam}
      ^-  (bolt hood)
      %+  cool  |.(leaf/"ford: fade {<[(tope bem)]>}")
      %+  cope  (cope (make cof [%bake for bem ~]) furl)
      |=  {cof+cafe cay+cage}
      %+  (clef %hood)  (fine cof bem(r [%ud 0]) cay)
      ^-  (burg (pair beam cage) hood)
      |=  {cof+cafe bum+beam cay+cage}
      ~|  fade/(tope bum)
      :: ~&  fade/clef-miss/bem
      =+  rul=(fair bum)
      ?.  ?=(@ q.q.cay)
        (flaw cof ~)
      =+  vex=((full rul) [[1 1] (trip q.q.cay)])
      ?~  q.vex
        (flaw cof [%leaf "syntax error: {<p.p.vex>} {<q.p.vex>}"] ~)
      (fine cof p.u.q.vex)
    ::
    ++  fame
      |=  {cof+cafe bem+beam}                          ::  beam with - as /
      ^-  (bolt beam)
      %-  cope  :_  (flux |=(a+(unit beam) (fall a bem)))
      ?~  s.bem  (flue cof)
      =+  opt=`(list term)`(fall (tear i.s.bem) ~)
      ?~  opt  (flue cof)
      |-  ^-  (bolt (unit beam))
      =.  i.s.bem  (tack opt)
      %+  cope  (lima cof %hoon bem ~)
      |=  {cof+cafe vax+(unit vase)}  ^-  (bolt (unit beam))
      ?^  vax  (fine cof `bem)
      ?~  t.opt  (flue cof)
      %+  cope  ^$(opt t.opt, t.s.bem :_(t.s.bem i.opt), cof cof)
      |=  {cof+cafe bem+(unit beam)}  ^-  (bolt (unit beam))
      ?^  bem  (fine cof bem)
      ^$(opt :_(t.t.opt (tack i.opt i.t.opt ~)), cof cof)
    ::
    ++  fang                                            ::  protocol door
      |=  {cof+cafe for+mark}  ^-  (bolt vase)
      (cope (lamp cof bek /[for]/mar) lear)
    ::
    ++  fair                                            ::  hood parsing rule
      |=  bem+beam
      ?>  ?=({$ud $0} r.bem)           ::  XX sentinel
      =+  vez=(vang & (tope bem))
      =<  hood
      |%  
      ++  case
        %-  sear  
        :_  nuck:so
        |=  a+coin
        ?.  ?=({$$ ?($da $ud $tas) *} a)  ~
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
            (ifix [;~(plug fas hep gap) gap] (most ;~(plug com gaw) hoof))
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
        %+  cook  |=(a+^hoof a)
        ;~  plug
          sym
          ;~  pose
            %+  stag  ~
            ;~(plug ;~(pfix fas case) ;~(pfix ;~(plug fas sig) fed:ag))
            (easy ~)
          ==
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
        =|  tol+?
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
          |*  {wid+rule tal+rule}
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
                |=  fan+(list ^horn)
                =|  naf+(list (pair term ^horn))
                |-  ^-  (unit (map term ^horn))
                ?~  fan  (some (~(gas by *(map term ^horn)) naf))
                ?.  ?=($dub -.i.fan)  ~
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
                  ;~(plug ;~(sfix wide:vez ket) apex(tol |))
                ;~(pfix gap ;~(plug tall:vez ;~(pfix gap apex)))
          --
        ::
        --
      --
    ::
    ++  join
      |=  {cof+cafe for+mark kas+silk kos+silk}
      ^-  (bolt gage)
      %.  [cof kas kos]
      ;~  cope
        ;~  coax
          |=({cof+cafe p+silk q+silk} (cope (make cof p) furl))
          |=({cof+cafe p+silk q+silk} (cope (make cof q) furl))
        ==
        |=  {cof+cafe cay+cage coy+cage}  ^-  (bolt gage)
        ::
        %+  cope  (fang cof for)
        |=  {cof+cafe pro+vase}
        ?.  (slab %grad p.pro)
          (flaw cof leaf/"no ++grad" ~)
        =+  gar=(slap pro [%cnzy %grad])
        ?@  q.gar
          =+  too=((sand %tas) q.gar)
          ?~  too  (flaw cof leaf/"bad mark ++grad" ~)
          (make cof %join u.too `cay `coy)
        ?.  (slab %form p.gar)
          (flaw cof leaf/"no ++form:grad" ~)
        =+  fom=((soft @tas) q:(slap gar [%cnzy %form]))
        ?~  fom
          (flaw cof leaf/"bad ++form:grad" ~)
        ?.  &(=(u.fom p.cay) =(u.fom p.coy))
          %+  flaw  cof  :_  :_  ~
            leaf/"join on data of bad marks: {(trip p.cay)} {(trip p.coy)}"
          leaf/"expected mark {(trip u.fom)}"
        ?:  =(q.q.cay q.q.coy)
          (fine cof %& cay)
        ?.  (slab %join p.gar)
          (flaw cof leaf/"no ++join:grad" ~)
        %+  cope
          %^  maul  cof
            (slap (slap pro [%cnzy %grad]) [%cnzy %join])
          (slop q.cay q.coy)
        |=  {cof+cafe dif+vase}
        ?@  q.dif
          (fine cof %& %null dif)
        (fine cof %& u.fom (slot 3 dif))
      ==
    ::
    ++  mash
      |=  {cof+cafe for+mark mas+milk mos+milk}
      ^-  (bolt gage)
      %.  [cof r.mas r.mos]
      ;~  cope
        ;~  coax
          |=({cof+cafe p+silk q+silk} (cope (make cof p) furl))
          |=({cof+cafe p+silk q+silk} (cope (make cof q) furl))
        ==
        |=  {cof+cafe cay+cage coy+cage}  ^-  (bolt gage)
        %+  cope  (fang cof for)
        |=  {cof+cafe pro+vase}
        ?.  (slab %grad p.pro)
          (flaw cof leaf/"no ++grad" ~)
        =+  gar=(slap pro [%cnzy %grad])
        ?@  q.gar
          =+  too=((sand %tas) q.gar)
          ?~  too  (flaw cof leaf/"bad mark ++grad" ~)
          %+  make  cof
          [%mash u.too [p.mas q.mas `cay] [p.mos q.mos `coy]]
        ?.  (slab %form p.gar)
          (flaw cof leaf/"no ++form:grad" ~)            
        =+  fom=((soft @tas) q:(slap gar [%cnzy %form]))
        ?~  fom
          (flaw cof leaf/"bad ++form:grad" ~)
        ?.  &(=(u.fom p.cay) =(u.fom p.coy))
          %+  flaw  cof  :_  :_  ~
            leaf/"mash on data of bad marks: {(trip p.cay)} {(trip p.coy)}"
          leaf/"expected mark {(trip u.fom)}"
        ?:  =(q.q.cay q.q.coy)
          (fine cof %& cay)
        ?.  (slab %mash p.gar)
          (fine cof %& %null [%atom %n] ~)
        %+  cope
          %^  maul  cof
            (slap (slap pro [%cnzy %grad]) [%cnzy %mash])
          %+  slop
            :(slop [[%atom %p] p.mas] [[%atom %tas] q.mas] q.cay)
          :(slop [[%atom %p] p.mos] [[%atom %tas] q.mos] q.coy)
        (flux |=(dif+vase [%& u.fom dif]))
      ==
    ::
    ++  kale                                            ::  mutate
      |=  {cof+cafe kas+silk muy+(list (pair wing silk))}
      ^-  (bolt gage)
      %+  cope
        |-  ^-  (bolt (list (pair wing vase)))
        ?~  muy  (flue cof)
        %+  cope  (cope (make cof q.i.muy) furl)
        |=  {cof+cafe cay+cage}
        %+  cope  ^$(muy t.muy)
        |=  {cof+cafe rex+(list (pair wing vase))}
        (fine cof [[p.i.muy q.cay] rex])
      |=  {cof+cafe yom+(list (pair wing vase))}
      %+  cope  (make cof kas)
      %-  tabl-run
      |=  {cof+cafe cay+cage}
      %+  cope  (keel cof q.cay yom)
      (flux |=(vax+vase [%& p.cay vax]))
    ::
    ++  keel                                            ::  apply mutations
      |=  {cof+cafe suh+vase yom+(list (pair wing vase))}
      ^-  (bolt vase)
      %+  cool  =<  |.(leaf/"ford: keel {<(murn yom +)>}")
                |=  {a+wing b+type *}
                =+  c=p:(slap suh cnzz/a)
                ?:  (~(nest ut c) | b)  ~
                (some [a c b])
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
    ++  lads                                            ::  possible children
      |=  {cof+cafe bem+beam arg+heel}
      ^-  (bolt (map span $~))
      =|  {res+(map span $~) new+(qeu beam)}
      =+  all=`(set beam)`[bem `~]
      |^  %+  with  (cope ?^(arg (flue cof) kids) return)
          |=(__(.) (with (cope marks look) next))
      ::
      ++  done  .
      ++  with
        |*  {a+(bolt __(done)) b+_+(__(done) (bolt))}  ^+  *b
        (cope a |=({cof+cafe c+__(done)} (b c(cof cof))))
      ::
      ++  return
        %-  flux
        |=(a+(map span $~) done(res (~(uni by res) a)))
      ::
      ++  next
        |=  __(.)  =>  +<
        ^-  (bolt (map span $~))
        ?^  s.bem
          $(s.bem t.s.bem, arg [i.s.bem arg])
        ?:  =(~ new)  (fine cof res)
        =^  bem  new  ~(get to new)
        ~|  next-beam/(tope bem)
        ?<  (gth (lent +.bem) 10)
        $(bem bem, arg ~)
      ::
      ++  kids
        %^  lash  cof  bem
        |=  {cof+cafe dir+span}
        %+  cope  (lend cof bem(s [dir s.bem]))
        (flux |=(a+arch ?~(dir.a ~ (some ~))))
      ::
      ++  marks
        =<  (cope (laze cof bem) (flux .))
        |=  a+(map mark ?)  ^-  (list mark)
        (turn :_(head (skip :_(tail (~(tap by a))))))   ::  | = %hook
      ::
      ++  look
        |=  {cof+cafe hok+(list mark)}  ^-  (bolt __(done))
        ?~  hok  (fine cof done)
        %+  with  $(hok t.hok)
        |=  __(done)  ^-  (bolt __(done))
        =+  bim=bem(s [i.hok s.bem])
        =+  hid=(fade cof %hook bim)
        ?:  ?=($2 -.q.hid)
          (fine cof done)
        =<  (cope hid (flux .))
        |=  hyd+hood
        =^  neu  all
          (chop:(meow bim (flop arg)) all %fan fan.hyd)
        done(new (~(gas to new) (~(tap in `(set beam)`neu))))
      --
    ::
    ++  laze                                            ::  find real or virtual
      |=  {cof+cafe bem+beam}
      %^  lash  cof  bem
      |=  {cof+cafe for+mark}
      ^-  (bolt (unit ?))
      ?.  ((sane %tas) for)  (flue cof)
      =.  s.bem  [for s.bem]
      %+  cope  (lend cof bem)
      |=  {cof+cafe arc+arch}
      ?^  fil.arc  (fine cof (some &))
      ?.  (~(has by dir.arc) %hook)
        (flue cof)
      %+  cope  (lend cof bem(s [%hook s.bem]))
      |=  {cof+cafe arc+arch}
      ?~  fil.arc
        (flue cof)
      (fine cof (some |))
    ::
    ++  lace                                            ::  load real or virtual
      |=  {cof+cafe for+mark bem+beam arg+heel lit+?}
      ^-  (bolt vase)
      =.  s.bem  [for s.bem]
      ?:  lit
        %+  cool  |.(leaf/"ford: load {<for>} {<(tope bem)>}")
        %+  cope  (liar cof bem)
        |=  {cof+cafe cay+cage}
        ?.  =(for p.cay)
          (flaw cof leaf/"unexpected mark {<p.cay>}" ~)
        ((lake for) cof q.cay)
      %+  cool  |.(leaf/"ford: hook {<for>} {<(tope bem)>}")
      %+  cope  (fade cof %hook bem)
      |=  {cof+cafe hyd+hood}
      (cope (abut:(meow bem arg) cof hyd) (lake for))
    ::
    ++  lake                                            ::  check/coerce
      |=  for+mark
      |=  {cof+cafe sam+vase}
      ^-  (bolt vase)
      %+  cool  |.(leaf/"ford: check {<[for bek `@p`(mug q.sam)]>}")
      ?:  ?=(?($hoon $hook) for)
        =+  mas=((soft @t) q.sam)
        ?~  mas
          (flaw cof [leaf/"ford: bad hoon or hook: {<[for bek]>}"]~)
        (fine cof [%atom %t] u.mas)
      %+  cope  (fang cof for)
      |=  {cof+cafe tux+vase}
      =+  typ=p:(slot 6 tux)
      =.  typ  ?+(-.typ typ $face q.typ)                ::  XX vulcanization
      ?:  (~(nest ut typ) | p.sam)
        (fine cof typ q.sam)
      ?.  (slob %grab p.tux)
        (flaw cof [%leaf "ford: no grab: {<[for bek]>}"]~)
      =+  gab=(slap tux [%cnzy %grab])
      ?.  (slob %noun p.gab)
        (flaw cof [%leaf "ford: no noun: {<[for bek]>}"]~)
      %+  cope  (maul cof (slap gab [%cnzy %noun]) [%noun q.sam])
      |=  {cof+cafe pro+vase}
      ?>  (~(nest ut typ) | p.pro)
      ?:  =(q.pro q.sam) 
        (fine cof typ q.pro)
      (flaw cof [%leaf "ford: invalid content: {<[for bek]>}"]~)
    ::
    ++  lamp                                            ::  normalize version
      |=  {cof+cafe bem+beam}
      ^-  (bolt beam)
      %+  cope
        ?:  ?=($ud -.r.bem)  (fine cof bem)
        =+  von=(save ~ %cw bem(s ~))
        ?~  von  [p=cof q=[%1 [%w bem ~] ~ ~]]
        (fine cof bem(r [%ud ((hard @) +.+:(need u.von))]))
      |=  {cof+cafe bem+{{ship desk $ud r+@u} s+spur}}
      ?:  =(0 r.bem)
        (flaw cof [leaf/"ford: no data: {<(tope bem(s ~))>}"]~)
      (fine cof bem)
    ::
    ++  lave                                            ::  validate
      |=  {cof+cafe for+mark som+*}
      ^-  (bolt vase)
      ((lake for) cof [%noun som])
    ::
    ++  lane                                            ::  type infer
      |=  {cof+cafe typ+type gen+twig}
      %+  (cowl cof)  (mule |.((~(play ut typ) gen)))
      |=(ref+type ref)
    ::
    ++  lash                                            ::  filter at beam
      |*  {cof+cafe bem+beam fun+(burg span (unit))}
      %+  cope  (lend cof bem)
      |=({cof+cafe arc+arch} ((some-in-map fun) cof dir.arc))
    ::
    ++  lear                                            ::  load core
      |=  {cof+cafe bem+beam}  ^-  (bolt vase)
      %+  cope  (fame cof bem)
      |=  {cof+cafe bem+beam}
      (cope (fade cof %hoon bem) abut:(meow bem ~))
    ::
    ++  lend                                            ::  load arch
      |=  {cof+cafe bem+beam}
      ^-  (bolt arch)
      =+  von=(save ~ %cy bem)
      ?~  von  [p=cof q=[%1 [%y bem ~] ~ ~]]
      ?>  ?=({$~ $arch ^} u.von)
      =+  arc=((hard arch) q.q.u.u.von)
      %+  cope  (lamp cof bem)
      |=  {cof+cafe bem+beam}
      (flag bem (fine cof arc))
    ::
    ++  liar                                            ::  load cage
      |=  {cof+cafe bem+beam}
      ^-  (bolt cage)
      =+  von=(save ~ %cx bem)
      ?~  von
        [p=*cafe q=[%1 [[%x bem ~] ~ ~]]]
      ?~  u.von
        (flaw cof leaf/"file not found" (smyt (tope bem)) ~)
      (fine cof u.u.von)
    ::
    ++  lily                                            ::  translation targets
      |=  {cof+cafe for+mark}  ^-  (bolt (set @tas))
      %+  cope  (lilt cof)
      |=  {cof+cafe lil+(jug mark mark)}
      (fine cof (~(get ju lil) for))
    ::
    ++  lilt
      |=  cof+cafe  ^-  (bolt (jug mark mark))
      %+  (clef %lilt)  (fine cof bek)
      ^-  (burg beak (jug mark mark))
      |=  {cof+cafe bek+beak}
      %+  cope  (lyle(bek bek) cof)
      %-  flux
      |=  mav+(map mark vase)
      =+  all=(~(tap by mav))
      |-  ^-  (jug mark mark)
      ?~  all  ~
      %-  ~(gas ju $(all t.all))
      =+  `{for+mark vax+vase}`i.all
      ~|  weg=(jam 3 p.vax)
      %+  weld
        ^-  (list {mark mark})
        ?.  (slob %grab p.vax)  ~
        =+  gab=(slap vax [%cnzy %grab])
        :: =+  opt=(skip (sloe p.gap) |=(fro+mark =(fro %noun)))
        (turn (sloe p.gab) |=(fro+mark [fro for]))
      ?.  (slob %grow p.vax)  ~
      =+  gow=(slap vax [%cnzy %grow])
      (turn (sloe p.gow) |=(too+mark [for too]))
    ::
    ++  lyle                                            ::  all mark doors
      |=  cof+cafe  ^-  (bolt (map mark vase))
      =|  {sup+path res+(map mark vase)}
      |^  `(bolt (map mark vase))`wide
      ++  here  [bek (welp sup /mar)]
      ++  wide
        %+  cope  (lend cof here)
        |=  {cof+cafe arc+arch}
        =+  all=(skim (turn (~(tap by dir.arc)) head) (sane %tas))
        =.  all  (sort all gth)                         ::  short-deepest
        |-  ^-  (bolt (map mark vase))
        ?~  all  (fine cof res)
        %+  cope  $(all t.all)
        |=  {cof+cafe res+(map mark vase)}
        =.  ^res  res
        =-  (cope - (tall i.all))
        ?.  (~(has by dir.arc) %hoon)  (flue cof)
        node(cof cof)
      ::
      ++  tall
        |=  for+term
        |=  {cof+cafe new+(unit {mark vase})}
        wide(cof cof, sup [for sup], res (~(gas by res) (drop new)))
      ::
      ++  node
        ^-  (bolt (unit {mark vase}))
        =+  pax=(flop sup)
        ?~  pax  (flue cof)
        =+  for=(tack pax)
        ?:  (~(has by res) for)  (flue cof)
        =+  raf=(lear cof here)
        ?.  ?=($2 -.q.raf)
          (cope raf (flux |=(vax+vase (some [for vax]))))
        =-  ((slog (flop `tang`-)) (flue cof))
        =+  (lent t.pax)
        ?:  ?~  -  |                      ::  error if level above built
            (~(has by res) (tack i.pax (scag (dec -) t.pax)))
          ~
        :_(q.q.raf leaf/"! {<`mark`for>} build failed, ignoring.")
      --
    ::
    ++  lima                                            ::  load at depth
      |=  {cof+cafe for+mark bem+beam arg+heel}
      %+  (clef %bake)  [p=cof q=[%0 p=[bem `~] q=[for bem arg]]]
      |=  {cof+cafe for+mark bem+beam arg+heel}
      ^-  (bolt (unit vase))
      %+  cope  (laze cof bem)
      |=  {cof+cafe mal+(map mark ?)}
      =+  lit=(~(get by mal) for)
      ?^  lit
        (cope (lace cof for bem arg u.lit) (flux some))
      =+  opt=(sa (turn (~(tap by mal)) head))          ::  XX asymptotics  
      %+  cope  (lion cof for opt)
      |=  {cof+cafe wuy+(list @tas)}
      ?~  wuy  (flue cof)
      %+  cope  
        (lace cof i.wuy bem arg (~(got by mal) i.wuy))
      |=  {cof+cafe hoc+vase}
      (cope (lope cof i.wuy t.wuy hoc) (flux some))
    ::
    ++  lime                                            ::  load beam
      |=  {cof+cafe for+mark bem+beam arg+heel}
      =+  [mob=bem mer=(flop arg)]
      |-  ^-  (bolt vase)
      %+  cope  (lima cof for mob (flop mer))
      |=  {cof+cafe vux+(unit vase)}
      ?^  vux  (fine cof u.vux)
      ?~  s.mob
        %+  flaw  cof
        ~[leaf/"build {<for>}" leaf/"no usable ancestors" (smyt (tope bem))]
      ^$(s.mob t.s.mob, mer [i.s.mob mer])
    ::
    ++  link                                            ::  translate
      |=  {cof+cafe too+mark for+mark vax+vase}
      ^-  (bolt vase)
      :: %+  cool   |.(leaf/"ford: link {<too>} {<for>} {<p.vax>}")
      ?:  =(too for)  (fine cof vax)
      ?:  |(=(%noun for) =(%$ for))
        ((lake too) cof vax)
      %+  cope  (fang cof for)
      |=  {cof+cafe pro+vase}  ^-  (bolt vase)
      ?:  &((slob %grow p.pro) (slob too p:(slap pro [%cnzy %grow])))
        %+  cope  (keel cof pro [[%& 6]~ vax]~)
        |=  {cof+cafe pox+vase}
        (maim cof pox [%tsgr [%cnzy %grow] [%cnzy too]])
      %+  cope  (fang cof too)
      |=  {cof+cafe pro+vase}
      =+  ^=  zat  ^-  (unit vase)
          ?.  (slob %grab p.pro)  ~
          =+  gab=(slap pro [%cnzy %grab])
          ?.  (slob for p.gab)  ~
          `(slap gab [%cnzy for])
      ?~  zat
        (flaw cof [%leaf "ford: no link: {<[for too]>}"]~)
      ~|  [%link-maul for too] 
      (maul cof u.zat vax)
    ::
    ++  lion                                            ::  translation search
      |=  {cof+cafe too+mark fro+(set mark)}
      :: ~&  lion/[too=too fro=(sa fro)]
      ^-  (bolt (list mark))
      =|  _:  war+(map mark (list mark))
              pax+(list mark)  
              won+{p+mark q+(qeu mark)}
          ==
      %.  [cof fro]
      |=  {cof+cafe fro+(set mark)}  ^-  (bolt (list mark))
      ?:  =(too p.won)
        (fine cof (flop pax))
      =+  for=(skip (~(tap by fro)) ~(has by war))
      =.  for  (sort for aor)         ::  XX useful?
      =:  q.won  (~(gas to q.won) for)
          war  (~(gas by war) (turn for |=(mark [+< pax])))
        ==
      ?:  =(~ q.won)
        (flue cof)
      =.  won  ~(get to q.won)
      %+  cope  (lily cof p.won)
      ..$(pax [p.won (~(got by war) p.won)])
    ::
    ++  lope                                            ::  translation pipe
      |=  {cof+cafe for+mark yaw+(list mark) vax+vase}
      ^-  (bolt vase)
      ?~  yaw  (fine cof vax)
      %+  cope  (link cof i.yaw for vax)
      |=  {cof+cafe yed+vase}
      ^$(cof cof, for i.yaw, yaw t.yaw, vax yed)
    ::
    ++  mail                                            ::  cached mint
      |=  {cof+cafe sut+type gen+twig}
      ^-  (bolt (pair type nock))
      %+  (clef %slim)  (fine cof sut gen)
      |=  {cof+cafe sut+type gen+twig}
      =+  puz=(mule |.((~(mint ut sut) [%noun gen])))
      ?-  -.puz
        {$|}  (flaw cof p.puz)
        {$&}  (fine cof p.puz)
      ==
    ::
    ++  maim                                            ::  slap
      |=  {cof+cafe vax+vase gen+twig}
      ^-  (bolt vase)
      %+  cope  (mail cof p.vax gen)
      |=  {cof+cafe typ+type fol+nock}
      %+  (coup cof)  (mock [q.vax fol] (mole (slod save)))
      |=(val+* `vase`[typ val])
    ::
    ++  make                                            ::  reduce silk
      |=  {cof+cafe kas+silk}
      :: =+  ^=  pre
      ::     ?+  -.kas  -.kas
      ::       ^  %cell
      ::       %boil  [-.kas p.kas (tope q.kas)]
      ::       %bake  [-.kas p.kas (tope q.kas)]
      ::       %core  [-.kas (tope p.kas)]
      ::     ==
      :: ~&  [dyv `term`(cat 3 %make (fil 3 dyv ' ')) pre]
      :: =-  ~&  [dyv `term`(cat 3 %made (fil 3 dyv ' ')) pre]  -
      :: =.  dyv  +(dyv)
      ^-  (bolt gage)
      ?-    -.kas
          ^
        %.  [cof p.kas q.kas]
        ;~  cope
          ;~  coax
            |=({cof+cafe p+silk q+silk} (cope ^$(cof cof, kas p.kas) furl))
            |=({cof+cafe p+silk q+silk} (cope ^$(cof cof, kas q.kas) furl)) 
          ==                          ::  XX merge %tabl
        ::
          |=  {cof+cafe bor+cage heg+cage}  ^-  (bolt gage)
          (faun cof (slop q.bor q.heg))
        ==
      ::
          $$  (fine cof %& p.kas)
          $bake
        ::  ~&  >  [p.kas (tope q.kas)]
        ::  %+  cool  |.(leaf/"ford: bake {<p.kas>} {<(tope q.kas)>}")
        %+  cope  (lima cof p.kas q.kas r.kas)
        |=  {cof+cafe vux+(unit vase)}
        ?~  vux
          (flaw cof leaf/"file not found" (smyt (tope q.kas)) ~)
        (fine cof [%& p.kas u.vux])
      ::
          $boil
        ^-  (bolt gage)
        %+  cool  |.(leaf/"ford: boil {<p.kas>} {<(tope q.kas)>} {<r.kas>}")
        %+  cope  (lamp cof q.kas)
        |=  {cof+cafe bem+beam}
        %+  cope  (lime cof p.kas bem r.kas)
        |=  {cof+cafe vax+vase}
        (fine cof `gage`[%& p.kas vax])
      ::
          $bunt
        %+  cool  |.(leaf/"ford: bunt {<p.kas>}")
        ?:  ?=(?($hoon $hook) p.kas)
          (fine cof %& p.kas [%atom %t] '')
        %+  cope  (fang cof p.kas)
        |=  {cof+cafe tux+vase}
        =+  [typ=p val=q]:(slot 6 tux)
        =.  typ  ?+(-.typ typ $face q.typ)              ::  XX vulcanization
        (fine cof [%& p.kas [typ val]])
      ::
          $call
        ::  %+  cool  |.(leaf/"ford: call {<`@p`(mug kas)>}")
        %.  [cof p.kas q.kas]
        ;~  cope
          ;~  coax
            |=({cof+cafe p+silk q+silk} (cope ^$(cof cof, kas p) furl))
            |=({cof+cafe p+silk q+silk} ^$(cof cof, kas q))
          ==
        ::
          |=  {cof+cafe gat+cage sam+gage}
          %.  [cof sam]
          %-  tabl-run
          |=  {cof+cafe sam+cage}
          (cope (maul cof q.gat q.sam) faun)
        ==
      ::
          $cast
        %+  cool  |.(leaf/"ford: cast {<p.kas>}")
        %+  cope  $(kas q.kas)
        %-  tabl-run
        |=  {cof+cafe cay+cage}
        ^-  (bolt gage)
        %+  cool  |.(leaf/"ford: casting {<p.cay>} to {<p.kas>}")
        %+  cope  (lion cof p.kas p.cay `~)
        |=  {cof+cafe wuy+(list @tas)}
        %+  cope
          ?~  wuy
            (link cof p.kas p.cay q.cay)
          (lope cof i.wuy t.wuy q.cay)
        (flux |=(vax+vase [%& p.kas vax]))
      ::
          $core
        %+  cool  |.(leaf/"ford: core {<(tope p.kas)>}")
        (cope (lear cof p.kas) (flux |=(a+vase [%& %core a])))
      ::
          $diff
        %+  cool  |.(leaf/"ford: diff {<`@p`(mug p.kas)>} {<`@p`(mug q.kas)>}")
        (diff cof p.kas q.kas)
      ::
          $dude  (cool p.kas $(kas q.kas))
          $file
        %+  cool  |.(leaf/"ford: file {<p.kas>}")
        %+  cope  (liar cof p.kas)
        (flux |=(cay+cage [%& cay]))
      ::
          $flag
        =+  rez=$(kas q.kas)
        ?:  ?=($1 -.q.rez)  rez
        =-  rez(p.q -)
        |-  ^-  (set beam)
        ?~  p.kas  p.q.rez
        =.  p.q.rez  $(p.kas l.p.kas)
        =.  p.q.rez  $(p.kas r.p.kas)
        ?^  n.p.kas
          (~(put in p.q.rez) n.p.kas)
        =+  dap=(~(get by deh.bay) n.p.kas)
        ?~  dap    ~&(flag-missed/n.p.kas p.q.rez)
        %-  ~(uni in p.q.rez)  ^-  (set beam)
        ?-(-.u.dap $init p.u.dap, $sent q.u.dap, $done [[bek ~] ~ ~])
      ::                              XX revisit ^ during dependency review
          $join
        %+  cool
          |.
          leaf/"ford: join {<p.kas>} {<`@p`(mug q.kas)>} {<`@p`(mug r.kas)>}"
        (join cof p.kas q.kas r.kas)
      ::
          $mash
        %+  cool
          |.
          leaf/"ford: mash {<p.kas>} {<`@p`(mug q.kas)>} {<`@p`(mug r.kas)>}"
        (mash cof p.kas q.kas r.kas)
      ::
          $mute  (kale cof p.kas q.kas)
          $pact
        %+  cool  |.(leaf/"ford: pact {<`@p`(mug p.kas)>} {<`@p`(mug q.kas)>}")
        (pact cof p.kas q.kas)
      ::
          $plan  (cope (abut:(meow p.kas q.kas) cof r.kas) faun)
          $reef  (faun cof pit)
          $ride
        %+  cool  |.(leaf/"ford: build failed")
        %+  cope  $(kas q.kas)
        %-  tabl-run
        |=  {cof+cafe cay+cage}
        %+  cope  (maim cof q.cay p.kas)
        |=  {cof+cafe vax+vase}
        (faun cof vax)
      ::
          $tabl
        %-  cope  :_  (flux |=(rex+(list (pair gage gage)) [%tabl rex]))
        !.
        |-  ^-  (bolt (list (pair gage gage)))
        ?~  p.kas  (fine cof ~)
        %.  [cof p.kas]
        ;~  cope
          ;~  coax
            |=({cof+cafe __(p.kas)} (fret ^^$(cof cof, kas p.i)))
            |=({cof+cafe __(p.kas)} (fret ^^$(cof cof, kas q.i)))
            |=({cof+cafe __(p.kas)} ^$(cof cof, p.kas t))
          ==
          (flux |=({k+gage v+gage t+(list {gage gage})} [[k v] t]))
        ==
      ::
          $vale  
        %+  cool  |.(leaf/"ford: vale {<p.kas>} {<`@p`(mug q.kas)>}")
        %+  cope  (lave cof p.kas q.kas)
        (flux |=(vax+vase `gage`[%& p.kas vax]))
      ::
          $volt
        %+  cool  |.(leaf/"ford: volt {<p.p.kas>}")
        %+  cope  $(kas [%bunt p.p.kas])
        %-  tabl-run
        |=  {cof+cafe cay+cage}
        ^-  (bolt gage)
        (fine cof [%& p.p.kas p.q.cay q.p.kas])
      ==
    ::
    ++  malt                                            ::  cached slit
      |=  {cof+cafe gat+type sam+type}
      ^-  (bolt type)
      %+  (clef %slit)  (fine cof gat sam)
      |=  {cof+cafe gat+type sam+type}
      %+  cool  |.(%.(%have ~(dunk ut sam)))
      %+  cool  |.(%.(%want ~(dunk ut (~(peek ut gat) %free 6))))
      =+  top=(mule |.((slit gat sam)))
      ?-  -.top
        {$|}  (flaw cof p.top)
        {$&}  (fine cof p.top)
      ==
    ::
    ++  maul                                            ::  slam
      |=  {cof+cafe gat+vase sam+vase}
      ^-  (bolt vase)
      %+  cope  (malt cof p.gat p.sam)
      |=  {cof+cafe typ+type}
      %+  (coup cof)  (mong [q.gat q.sam] (mole (slod save)))
      |=(val+* `vase`[typ val])
    ::
    ++  meow                                            ::  assemble
      |=  {how+beam arg+heel} 
      =|  _:  rop+(map term (pair hoof twig))           ::  structures
              bil+(map term (pair hoof twig))           ::  libraries
              boy+(list twig)                           ::  body stack
          ==
      ~%  %meow  ..meow
        ==
          %able  able
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
          ?:(=(~ rop) [%$ 1] [%brcn (~(run by rop) |=({^ a+twig} [%ash a]))])
        ?:(=(~ bil) [%$ 1] [%brcn (~(run by bil) |=({^ a+twig} [%ash a]))])
      ::
      ++  abut                                          ::  generate
        |=  {cof+cafe hyd+hood}
        ^-  (bolt vase)
        %+  cope  (apex cof hyd)
        |=  {cof+cafe sel+__(..abut)}
        =.  ..abut  sel
        %+  cope  (maim cof pit able)
        |=  {cof+cafe bax+vase}
        %+  cope  (chap cof bax [%fan fan.hyd])
        |=  {cof+cafe gox+vase}
        %+  cope  (maim cof (slop gox bax) [%tssg (flop boy)])
        |=  {cof+cafe fin+vase}
        (fine cof fin) 
        ::  ~>  %slog.[0 ~(duck ut p.q.cay)]
      ::
      ++  apex                                          ::  build to body
        |=  {cof+cafe hyd+hood}
        ^-  (bolt __(..apex))
        %+  cope  (body cof src.hyd)
        |=  {cof+cafe sel+__(..apex)}
        =.  ..apex  sel
        %+  cope  (neck cof lib.hyd)
        |=  {cof+cafe sel+__(..apex)}
        =.  ..apex  sel(boy boy)
        %+  cope  (head cof sur.hyd)
        |=  {cof+cafe sel+__(..apex)}
        (fine cof sel)
      ::
      ++  body                                          ::  produce functions
        |=  {cof+cafe src+(list hoop)}
        ^-  (bolt __(..body))
        ?~  src  (fine cof ..body)
        %+  cope  (wilt cof i.src)
        |=  {cof+cafe sel+__(..body)}
        ^$(src t.src, ..body sel, cof cof)
      ::
      ++  chad                                          ::  atomic list
        |=  {cof+cafe bax+vase doe+term hon+horn}
        ^-  (bolt vase)
        %+  cope  (lash cof how (flux (slat doe)))
        |=  {cof+cafe yep+(map span @)}
        =+  ^=  poy  ^-  (list (pair span @))
            %+  sort  (~(tap by yep) ~)
            |=({{* a+@} {* b+@}} (lth a b))
        %+  cope
          |-  ^-  (bolt (list (pair @ vase)))
          ?~  poy  (flue cof)
          %+  cope  $(poy t.poy)
          |=  {cof+cafe nex+(list (pair @ vase))}
          %+  cope  (chap(s.how [p.i.poy s.how]) cof bax hon)
          (flux |=(elt+vase [[q.i.poy elt] nex]))
        %-  flux
        |=  yal+(list (pair @ vase))  ^-  vase
        ?~  yal  [[%cube 0 [%atom %n]] 0]
        (slop (slop [[%atom doe] p.i.yal] q.i.yal) $(yal t.yal))
      ::
      ++  chai                                          ::  atomic map
        |=  {cof+cafe bax+vase hon+horn}
        ^-  (bolt vase)
        %+  cope
          %+  cope  (lads cof how ~)
          %-  some-in-map
          |=  {cof+cafe dir+span}
          =+  nod=(chap(s.how [dir s.how]) cof bax hon)
          ?:  ?=($2 -.q.nod)
            (flue cof)
          (cope nod (flux some))
        %-  flux
        |=  doy+(map @ vase)  ^-  vase
        ?~  doy  [[%cube 0 [%atom %n]] 0]
        %+  slop
          (slop [[%atom %ta] p.n.doy] q.n.doy)
        (slop $(doy l.doy) $(doy r.doy))
      ::
      ++  chap                                          ::  produce resources
        |=  {cof+cafe bax+vase hon+horn}
        ^-  (bolt vase)
        ?-    -.hon
            $ape  (maim cof bax p.hon)
            $arg  
          %+  cope  (maim cof bax p.hon)
          |=  {cof+cafe gat+vase}
          (maul cof gat !>([how arg]))
        ::
            $day  (chad cof bax %dr p.hon)
            $dub 
          %+  cope  $(hon q.hon)
          (flux |=(vax+vase [[%face p.hon p.vax] q.vax]))
        ::
            $fan
          %+  cope
            |-  ^-  (bolt (list vase))
            ?~  p.hon  (flue cof)
            %+  cope  ^$(cof cof, hon i.p.hon)
            |=  {cof+cafe vax+vase}
            %+  cope  ^$(cof cof, p.hon t.p.hon)
            (flux |=(tev+(list vase) [vax tev]))
          |=  {cof+cafe tev+(list vase)}
          %+  fine  cof
          |-  ^-  vase
          ?~  tev  [[%cube 0 [%atom %n]] 0]
          (slop i.tev $(tev t.tev))
        ::
            $for  $(hon q.hon, s.how (weld (flop p.hon) s.how))
            $hel    
          %=  $  
            hon    q.hon
            arg    (scag p.hon arg)
            s.how  (weld (slag p.hon arg) s.how)
          ==
        ::
            $hub  (chad cof bax %ud p.hon)
            $man
          |-  ^-  (bolt vase)
          ?~  p.hon  (fine cof [[%cube 0 [%atom %n]] 0])
          %+  cope  $(p.hon l.p.hon)
          |=  {cof+cafe lef+vase}
          %+  cope  ^$(cof cof, p.hon r.p.hon)
          |=  {cof+cafe rig+vase}
          %+  cope  ^^^$(cof cof, hon q.n.p.hon)
          |=  {cof+cafe vax+vase}
          %+  fine  cof
          %+  slop
            (slop [[%atom %tas] p.n.p.hon] vax)
          (slop lef rig)
        ::
            $now  (chad cof bax %da p.hon)
            $nap  (chai cof bax p.hon)
            $see
          =.  r.p.hon  ?:(?=({$ud $0} r.p.hon) r.how r.p.hon)
          $(hon q.hon, how p.hon)
        ::
            $saw  
          %+  cope  $(hon q.hon)
          |=  {cof+cafe sam+vase}
          %+  cope  (maim cof bax p.hon)
          |=  {cof+cafe gat+vase}
          (maul cof gat sam)
        ::
            $sic
          %+  cope  $(hon q.hon)
          |=  {cof+cafe vax+vase}
          %+  cope  (maim cof bax [%cnbc p.hon])
          |=  {cof+cafe tug+vase}
          ?.  (~(nest ut p.tug) | p.vax)
            (flaw cof [%leaf "type error: {<p.hon>} {<q.hon>}"]~)
          (fine cof [p.tug q.vax])
        ::
            $toy  (cope (cope (make cof %boil p.hon how ~) furl) feel)
        ==
      ::
      ++  chop                                          ::  possible subpaths
        |=  {old+(set beam) hon+horn}
        =+  acc=[new=*(set beam) old=old]
        |-  ^+  acc
        ?-    -.hon
            ?($ape $arg $toy)  acc
            ?($dub $sic $saw)  $(hon q.hon)
            ?($day $hub $nap $now)  acc                 ::  drop to avoid cycles
            $for  $(hon q.hon, s.how (weld (flop p.hon) s.how))
            $see
          =.  r.p.hon  ?:(?=({$ud $0} r.p.hon) r.how r.p.hon)
          $(hon q.hon, how p.hon)
        ::
            $hel
          =.  s.how  (weld (slag p.hon arg) s.how)
          ?:  (~(has in old.acc) how)  acc
          [(~(put in new.acc) how) (~(put in old.acc) how)]
        ::
            $fan
          |-  ^+  acc
          ?~  p.hon  acc
          ^$(hon i.p.hon, acc $(p.hon t.p.hon))
        ::
            $man
          |-  ^+  acc
          ?~  p.hon  acc
          =.  acc  $(p.hon l.p.hon, acc $(p.hon r.p.hon))
          ^$(hon q.n.p.hon)
        ==
      ::
      ++  head                                          ::  consume structures
        |=  {cof+cafe bir+(list hoof)}
        ^-  (bolt __(..head))
        ?~  bir
          (fine cof ..head)
        =+  byf=(~(get by rop) p.i.bir)
        ?^  byf
          ?.  =(`hoof`i.bir `hoof`p.u.byf)
            (flaw cof [%leaf "structure mismatch: {<~[p.u.byf i.bir]>}"]~)
          $(bir t.bir)
        %+  cope  (fame cof (hone %sur i.bir))
        |=  {cof+cafe bem+beam}
        %+  cope  (fade cof %hoon bem)
        |=  {cof+cafe hyd+hood}
        %+  cope  (apex(how bem, boy ~) cof hyd)
        |=  {cof+cafe sel+__(..head)}
        =.  ..head
            %=  sel
              boy  boy
              how  how
              rop  %+  ~(put by (~(uni by rop) rop.sel))
                      p.i.bir 
                   [i.bir [%tssg (flop boy.sel)]]
            ==
        ^^^$(cof cof, bir t.bir)
      ::
      ++  hone                                          ::  plant hoof
        |=  {way+@tas huf+hoof}
        ^-  beam
        ?~  q.huf
          how(s ~[p.huf way])
        [[q.u.q.huf q.how p.u.q.huf] ~[p.huf way]]
      ::
      ++  neck                                          ::  consume libraries
        |=  {cof+cafe bir+(list hoof)}
        ^-  (bolt __(..neck))
        ?~  bir  (fine cof ..neck)
        =+  byf=(~(get by bil) p.i.bir)
        ?^  byf
          ?.  =(`hoof`i.bir `hoof`p.u.byf)
            (flaw cof [%leaf "library mismatch: {<~[p.u.byf i.bir]>}"]~)
          $(bir t.bir)
        %+  cope  (fame cof (hone %lib i.bir))
        |=  {cof+cafe bem+beam}
        %+  cope  (fade cof %hoon bem)
        |=  {cof+cafe hyd+hood}
        %+  cope  (apex(how bem, boy ~) cof hyd)
        |=  {cof+cafe sel+__(..neck)}
        =.  ..neck  
            %=  sel
              how  how
              bil  (~(put by bil) p.i.bir [i.bir [%tssg (flop boy.sel)]])
            ==
        ^^^$(cof cof, bir t.bir)
      ::
      ++  wilt                                          ::  process body entry
        |=  {cof+cafe hop+hoop}
        ^-  (bolt __(..wilt))
        ?-    -.hop
            {$&}  (fine cof ..wilt(boy [p.hop boy]))
            {$|}
          =.  r.p.hop  ?:(?=({$ud $0} r.p.hop) r.how r.p.hop)
          %+  cool  |.(leaf/"ford: wilt {<[(tope p.hop)]>}")
          %+  cope  (lend cof p.hop)
          |=  {cof+cafe arc+arch}
          ?:  (~(has by dir.arc) %hoon)
            %+  cope  (fade cof %hoon p.hop)
            |=  {cof+cafe hyd+hood}
            %+  cope  (apex(boy ~) cof hyd)
            (flux |=(sel+__(..wilt) sel(boy [[%tssg boy.sel] boy])))
          =+  [all=(lark (slat %tas) arc) sel=..wilt]
          %+  cope
            |-  ^-  (bolt (pair (map term foot) __(..wilt)))
            ?~  all  (fine cof ~ ..wilt)
            %+  cope  $(all l.all)
            |=  {cof+cafe lef+(map term foot) sel+__(..wilt)}
            %+  cope  ^$(all r.all, cof cof, sel sel)
            |=  {cof+cafe rig+(map term foot) sel+__(..wilt)}
            %+  cope  
              %=    ^^^^$
                  cof      cof
                  ..wilt   sel(boy ~)
                  s.p.hop  [p.n.all s.p.hop]
              ==
            |=  {cof+cafe sel+__(..wilt)}
            %+  fine  cof
            [`(map term foot)`[[p.n.all [%ash [%tssg boy.sel]]] lef rig] sel]
          |=  {cof+cafe mav+(map term foot) sel+__(..wilt)}
          ?~  mav
            (flaw cof [%leaf "source missing: {<(tope p.hop)>}"]~)
          (fine cof sel(boy [[%brcn mav] boy]))
        ==
      --
    ::
    ++  pact                                            ::  patch
      |=  {cof+cafe kas+silk kos+silk}
      ^-  (bolt gage)
      %.  [cof kas kos]
      ;~  cope
        ;~  coax
          |=({cof+cafe p+silk q+silk} (cope (make cof p) furl))
          |=({cof+cafe p+silk q+silk} (cope (make cof q) furl))
        ==
        |=  {cof+cafe cay+cage coy+cage}  ^-  (bolt gage)
        ?:  ?=(?($hoon $hook) p.cay)
          ?.  ?=($txt-diff p.coy)
            (flaw cof leaf/"{<p.cay>} mark with bad diff type: {<p.coy>}" ~)
          =+  txt=((soft @t) q.q.cay)
          ?~  txt
            (flaw cof leaf/"{<p.cay>} mark on bad data" ~)
          =+  dif=((soft (urge cord)) q.q.coy)
          ?~  dif
            =-  (flaw cof leaf/"{<p.cay>} data with bad diff" -)
            [>type=p.q.coy< >want=p:!>(*(urge cord))< ~]
          =+  pac=(role (lurk (lore (cat 3 u.txt '\0a')) u.dif))
          (fine cof %& p.cay [%atom %t] (end 3 (dec (met 3 pac)) pac))
        ::
        %+  cope  (fang cof p.cay)
        |=  {cof+cafe pro+vase}
        ?.  (slab %grad p.pro)
          (flaw cof leaf/"no ++grad" ~)
        =+  gar=(slap pro [%cnzy %grad])
        ?@  q.gar
          =+  for=((sand %tas) q.gar)
          ?~  for  (flaw cof leaf/"bad mark ++grad" ~)
          (make cof %cast p.cay %pact [%cast u.for `cay] `coy)
        ?.  (slab %form p.gar)
          (flaw cof leaf/"no ++form:grad" ~)
        =+  for=((soft @tas) q:(slap gar [%cnzy %form]))
        ?~  for
          (flaw cof leaf/"bad ++form:grad" ~)
        ?.  =(u.for p.coy)
          %+  flaw  cof  :_  ~
          =<  leaf/"pact on data with wrong form: {-} {+<} {+>}"
          [(trip p.cay) (trip u.for) (trip p.coy)]
        ?.  (slab %pact p.gar)
          (flaw cof leaf/"no ++pact:grad" ~)
        %+  cope  (keel cof pro [[%& 6]~ q.cay]~)
        |=  {cof+cafe pox+vase}
        %+  cope
          %^  maul  cof
            (slap (slap pox [%cnzy %grad]) [%cnzy %pact])
          q.coy
        (flux |=(pat+vase [%& p.cay pat]))
      ==
    ::
    ++  resp
      |=  {tik+@ud rot+riot}
      ^+  ..zo
      ?>  (~(has by q.kig) tik)
      =+  `{ren+care bem+beam}`(~(got by q.kig) tik)
      ?~  rot
        =^  dep  deh.bay  (daze ~)                      ::  dependencies?
        amok:(expo [%made dep %| (smyt ren (tope bem)) ~])
      =+  (cat 3 'c' ren)
      exec(q.kig (~(del by q.kig) tik), keg (~(put by keg) [- bem] r.u.rot))
    ::
    ++  save
      ^-  sled
      |=  {(unit (set monk)) tem+term bem+beam}
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
|=  {now+@da eny+@ ski+sled}                            ::  activate
^?                                                      ::  opaque core
~%  %ford-d  ..is  ~
|%                                                      ::
++  call                                                ::  request
  |=  {hen+duct hic+(hypo (hobo kiss))}
  ^+  [p=*(list move) q=..^$]
  =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ((hard kiss) p.q.hic)))
  ?:  ?=($wegh -.q.hic)
    :_  ..^$  :_  ~
    :^  hen  %give  %mass
    :-  %ford
    :-  %|
    %-  |=  a+(list (list mass))  ^-  (list mass)       :: XX single-home
        =+  a2=a
        ?~  a  !!
        ?~  i.a  ~
        :_  $(a (turn a2 tail))
        :-  p.i.i.a
        ?~  -.q.i.i.a  
          [%& (turn (turn a2 head) |=(b+mass ?~(-.q.b p.q.b !!)))]
        [%| $(a (turn (turn a2 head) |=(b+mass ?~(-.q.b !! p.q.b))))]
    %+  turn  (~(tap by pol))
    |=  {@ baby}
    :~  =<  cache/[%| (turn `(list term)`/hood/bake/lilt/slit/slim/slap/slam .)]
        =-  |=(a+term [a `(~(get ja dep) a)])
        =<  `dep+(jar term *)`(~(rep by jav) .)
        |=({{* a+{term *}} b+(jar term *)} (~(add ja b) a))
    ::
        =<  depends/[%| (turn `(list term)`/init/sent/done .)]
        =-  |=(a+term [a `(~(get ja dep) a)])
        =<  `dep+(jar term *)`(~(rep by deh) .)
        |=({{@ a+{term *}} b+(jar term *)} (~(add ja b) a))
    ::
        tasks/`[dym tad]
    ==
  =+  our=p.q.hic
  =+  ^=  bay  ^-  baby
      =+  buy=(~(get by pol.lex) our)
      ?~(buy *baby u.buy)
  =^  mos  bay
    ?-    -.q.hic
        $wasp
      abet:(~(awap za [our hen [now eny ski] ~] bay) q.q.hic)
        $exec
      ?~  q.q.hic
        abet:~(apax za [our hen [now eny ski] ~] bay)
      =.  p.u.q.q.hic  -:(norm ski p.u.q.q.hic ~)
      abet:(~(apex za [our hen [now eny ski] ~] bay) u.q.q.hic)
    ==
  [mos ..^$(pol (~(put by pol) our bay))]
::
++  doze
  |=  {now+@da hen+duct}
  ^-  (unit @da)
  ~
::
++  load                                                ::  highly forgiving
  |=  old+axle  
  ..^$(+>- old)
  ::  ..^$
  ::=.  old  
  ::    ?.  ?=([%0 *] old)  old                           ::  remove at 1
  ::    :-  %1 
  ::    |-  ^-  *
  ::    ?~  +.old  ~
  ::    ?>  ?=([n=[p=* q=[tad=* dym=* deh=* jav=*]] l=* r=*] +.old)
  ::    :-  [p.n.+.old [tad.q.n.+.old dym.q.n.+.old deh.q.n.+.old ~]]
  ::    [$(+.old l.+.old) $(+.old r.+.old)]
  ::|=  old=*
  ::=+  lox=((soft axle) old)
  ::^+  ..^$
  ::?~  lox
  ::  ~&  %ford-reset
  ::  ..^$
  ::..^$(+>- u.lox)
::
++  scry
  |=  {fur+(unit (set monk)) ren+@tas who+ship syd+desk lot+coin tyl+path}
  ^-  (unit (unit cage))
  [~ ~]
::
++  stay                                                ::  save w/o cache
  `axle`+>-.$(pol (~(run by pol) |=(a+baby [tad.a dym.a deh.a ~])))
::
++  take                                                ::  response
  |=  {tea+wire hen+duct hin+(hypo sign)}
  ^+  [p=*(list move) q=..^$]
  ?>  ?=({@ @ *} tea)
  =+  our=(slav %p i.tea)
  =+  bay=(~(got by pol.lex) our)
  =^  mos  bay
    =+  dep=(slaw %uv i.t.tea)
    ?^  dep
      =+  bem=(need (tome t.t.tea))
      abet:(~(axun za [our hen [now eny ski] ~] bay) tea u.dep bem q.hin)
    ?>  ?=({@ $~} t.t.tea)
    =+  :*  num=(slav %ud i.t.tea)
            tik=(slav %ud i.t.t.tea)
        ==
    abet:(~(axon za [our hen [now eny ski] ~] bay) num tik q.hin)
  [mos ..^$(pol (~(put by pol) our bay))]
--
