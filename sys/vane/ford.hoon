!:::::
::  ::  %ford, new execution control
!?  164
::::
|=  pit/vase
=,  ford
=,  format
=>  =~
::  structures
|%
++  heel  path                                          ::  functional ending
++  move  {p/duct q/(wind note gift:able)}              ::  local move
++  note                                                ::  out request $->
          $%  $:  $c                                    ::  to %clay
          $%  {$warp p/sock q/riff:clay}               ::
          ==  ==                                        ::
              $:  $f                                    ::  to %ford
          $%  {$exec p/@p q/(unit bilk:ford)}          ::
          ==  ==                                        ::
              $:  $g                                    ::  to %gall
          $%  {$deal p/sock q/cush:gall}               ::
          ==  ==  ==                                    ::
++  sign                                                ::  in result $<-
          $%  $:  $c                                    ::  by %clay
          $%  {$writ p/riot:clay}                      ::
          ==  ==                                        ::
              $:  $f                                    ::  by %ford
          $%  {$made p/@uvH q/gage:ford}               ::
          ==  ==                                        ::
              $:  $g                                    ::  by %gall
          $%  {$unto p/cuft:gall}                      ::
          ==  ==  ==                                    ::
--                                                      ::
|%                                                      ::  structures
++  axle                                                ::  all %ford state
  $:  $2                                                ::  version for update
      pol/(map ship baby)                               ::
  ==                                                    ::
++  baby                                                ::  state by ship
  $:  tad/{p/@ud q/(map @ud task)}                      ::  tasks by number
      dym/(map duct @ud)                                ::  duct to task number
      deh/(map @uvH deps)                               ::  depends by hash
      jav/(map * calx)                                  ::  cache
  ==                                                    ::
++  bolt                                                ::  gonadic edge
  |*  a/mold                                            ::  product clam
  $:  p/cafe                                            ::  cache
    $=  q                                               ::
      $%  {$0 p/(set beam) q/a}                         ::  depends+product
          {$1 p/(set {van/vane ren/care:clay bem/beam tan/tang})}  ::  blocks
          {$2 p/(set beam) q/tang}                      ::  depends+error
      ==                                                ::
  ==                                                    ::
::                                                      ::
++  burg                                                ::  gonadic rule
  |*  {a/mold b/mold}                             ::  from and to
  $-({c/cafe d/a} (bolt b))                             ::
::                                                      ::
++  cafe                                                ::  live cache
  $:  p/(set calx)                                      ::  used
      q/(map * calx)                                    ::  cache
      r/(map @uvH deps)                                 ::  dependss
  ==                                                    ::
::                                                      ::
++  calm                                                ::  cache metadata
  $:  laz/@da                                           ::  last accessed
      dep/(set beam)                                    ::  dependencies
  ==                                                    ::
++  calx                                                ::  concrete cache line
  $%  {$hood p/calm q/(pair beam cage) r/hood}          ::  compile
      {$bake p/calm q/(pair mark beam) r/(unit vase)}   ::  load
      {$boil p/calm q/(trel coin beam beam) r/vase}     ::  execute
      {$path p/calm q/beam r/(unit beam)}               ::  -to/ transformation
      {$slit p/calm q/{p/span q/span} r/span}           ::  slam type
      {$slim p/calm q/{p/span q/twig} r/(pair span nock)}::  mint
      {$slap p/calm q/{p/vase q/twig} r/vase}           ::  compute
      {$slam p/calm q/{p/vase q/vase} r/vase}           ::  compute
  ==                                                    ::
++  deps                                                ::  depend state
  $%  {$init p/(set beam)}                              ::  given out
      {$sent p/(set duct) q/(set beam)}                 ::  listener exists
      {$done $~}                                         ::  change seen
  ==                                                    ::
++  task                                                ::  problem in progress
  $:  nah/duct                                          ::  cause
      {bek/beak kas/silk}                               ::  problem
      keg/(map (pair term beam) cage)                   ::  block results
      kig/(set (trel vane care:clay beam))              ::  blocks
  ==                                                    ::
++  gagl  (list (pair gage gage))                       ::
++  vane  ?($a $b $c $d $e $f $g)                       ::
--                                                      ::
|%                                                      ::
++  calf                                                ::  reduce calx
  |*  sem/*                                             ::  a spansystem hack
  |=  cax/calx
  ?+  sem  !!
    $hood  ?>(?=($hood -.cax) r.cax)
    $bake  ?>(?=($bake -.cax) r.cax)
    $boil  ?>(?=($boil -.cax) r.cax)
    $path  ?>(?=($path -.cax) r.cax)
    $slap  ?>(?=($slap -.cax) r.cax)
    $slam  ?>(?=($slam -.cax) r.cax)
    $slim  ?>(?=($slim -.cax) r.cax)
    $slit  ?>(?=($slit -.cax) r.cax)
  ==
::
++  calk                                                ::  cache lookup
  |=  a/cafe                                            ::
  |=  {b/@tas c/*}                                      ::
  ^-  {(unit calx) cafe}                                ::
  =+  d=(~(get by q.a) [b c])                           ::
  ?~  d  [~ a]                                          ::
  [d a(p (~(put in p.a) u.d))]                          ::
::                                                      ::
++  came                                                ::
  |=  {a/cafe b/calx}                                   ::  cache install
  ^-  cafe                                              ::
  a(q (~(put by q.a) [-.b q.b] b))                      ::
::                                                      ::
++  faun  (flux |=(a/vase [%& %noun a]))                ::  vase to gage
++  flay                                                ::  unwrap gage to cage
  |=  {a/cafe b/gage}  ^-  (bolt cage)
  ?-  -.b
    $tabl  (flaw a >%bad-marc< ~)
    $|     (flaw a p.b)
    $&     (fine a p.b)
  ==
::
++  fret                                                ::  lift error
  |=  a/(bolt gage)  ^-  (bolt gage)
  ?.  ?=($2 -.q.a)  a
  [p.a [%0 p.q.a `gage`[%| q.q.a]]]
::
++  fine  |*  {a/cafe b/*}                              ::  bolt from data
          [p=`cafe`a q=[%0 p=*(set beam) q=b]]          ::
++  flaw  |=  {a/cafe b/tang}                           ::  bolt from error
          [p=a q=[%2 p=*(set beam) q=b]]                ::
++  flag                                                ::  beam into deps
  |*  {a/beam b/(bolt)}                                 ::
  ?:  ?=($1 -.q.b)  b
  =.  p.q.b  (~(put in p.q.b) a)
  b
::                                                      ::
++  flue  |=(a/cafe (fine a ~))                         ::  cafe to empty
++  flux  |*  a/_*                                      ::  bolt lift (fmap)
          |*  {cafe _,.+<.a}
          (fine +<- (a +<+))
::
++  lark                                                ::  filter arch names
  |=  {wox/$-(knot (unit @)) arc/arch}
  ^-  (map @ knot)
  %-  ~(gas by *(map @ knot))
  =|  rac/(list (pair @ knot))
  |-  ^+  rac
  ?~  dir.arc  rac
  =.  rac  $(dir.arc l.dir.arc, rac $(dir.arc r.dir.arc))
  =+  gib=(wox p.n.dir.arc)
  ?~(gib rac [[u.gib p.n.dir.arc] rac])
::
++  tack                                                ::  fold path to term
  |=  a/{i/term t/(list term)}  ^-  term
  (rap 3 |-([i.a ?~(t.a ~ ['-' $(a t.a)])]))
::
++  tear                                                ::  split term
  =-  |=(a/term `(list term)`(fall (rush a (most hep sym)) /[a]))
  sym=(cook crip ;~(plug low (star ;~(pose low nud))))
::
++  za                                                  ::  per event
  =|  $:  $:  our/ship                                  ::  computation owner
              hen/duct                                  ::  event floor
              $:  now/@da                               ::  event date
                  eny/@                                 ::  unique entropy
                  ska/sley                              ::  system namespace
              ==                                        ::
              mow/(list move)                           ::  pending actions
          ==                                            ::
          bay/baby                                      ::  all owned state
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
    |=  kub/bilk
    ^+  +>
    =+  num=p.tad.bay
    ?<  (~(has by dym.bay) hen)
    =:  p.tad.bay  +(p.tad.bay)
        dym.bay    (~(put by dym.bay) hen num)
      ==
    ~(exec zo [num `task`[hen kub ~ ~]])
  ::
  ++  axon                                              ::  take
    |=  {num/@ud {van/vane ren/care:clay bem/beam} sih/sign}
    ^+  +>
    ?:  ?=({$unto $quit *} +.sih)
      +>.$
    =+  tus=(~(get by q.tad.bay) num)
    ?~  tus
      ~&  [%ford-lost van num]
      +>.$
    ?-    -.+.sih
        $writ  (~(resp zo [num u.tus]) [van ren bem] p.+.sih)
        $made  (~(resm zo [num u.tus]) [van ren bem] [p q]:+.sih)
        $unto
      ?+  -.p.+.sih  ~|(ford-strange-unto+-.p.+.sih !!)
        $diff  (~(resd zo [num u.tus]) [van ren bem] p.p.+.sih)
        $reap  ?~  p.p.+.sih  +>.$
               ((slog leaf+"ford-reap-fail" u.p.p.+.sih) +>.$)
      ==
    ==
  ::
  ++  axun                                              ::  take rev update
    |=  {tea/wire dep/@uvH bem/beam sih/sign}
    ^+  +>
    ?+    -.+.sih  ~|(%bad-axun !!)
        $writ
      ?~  p.sih  +>.$
      :: ~&  writ+tea
      =+  dap=(~(got by deh.bay) dep)
      =-  +>.$(mow mow, deh.bay ?~(dop deh.bay (~(put by deh.bay) dep dop)))
      ^-  {dop/$@($~ _dap) mow/_mow}
      ?-    -.dap
          $done  `mow                ::  writ redundant
          $init  ~|(never-subscribed+dep !!)
          $sent
        :-  [%done ~]
        ;:  weld
          (axap dep (~(del in q.dap) bem))              ::  cancel outstanding
          (turn (~(tap in p.dap)) |=(hen/duct [hen %give %news dep]))
          mow
    ==  ==
      ==
  ::
  ++  axap                                              ::  unsubscribe beams
    |=  {dep/@uvH dap/(set beam)}
    %+  turn  (~(tap in dap))
    |=  bem/beam
    :^  hen  %pass  [(scot %p our) (scot %uv dep) (en-beam bem)]
    [%c %warp [our p.bem] q.bem ~]
  ::
  ++  awap                                              ::  get next revision
    ~%  %ford-w  ..is  ~
    |=  {dep/@uvH ask/?}
    ?:  =(`@`0 dep)
      ~&(dep-empty+hen +>.$)
    ?:  =(dep 0vtest)                 ::  upstream testing
      +>.$(mow ?.(ask mow :_(mow [hen %give %news dep])))
    =+  dap=(~(get by deh.bay) dep)
    ?~  dap  ~&(dep-missed+dep +>.$)  ::  XX  ~|  !!
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
        ?.  ask  ~&(awap-kill-empty+dep +>.$)  :: crash?
        %_    +>.$
            deh.bay
          (~(put by deh.bay) dep [%sent [hen ~ ~] p.u.dap])
        ::
            mow
          =<  (welp :_(mow (turn (~(tap in p.u.dap)) .)))
          |=  bem/beam
          :^  hen  %pass  [(scot %p our) (scot %uv dep) (en-beam bem)]
          [%c [%warp [our p.bem] q.bem ~ [%next %z r.bem (flop s.bem)]]]
    ==  ==
  ::
  ++  zo
    ~%  %ford-z  ..is  ~
    =|  dyv/@
    |_  {num/@ud task}
    ++  abet  %_(..zo q.tad.bay (~(put by q.tad.bay) num +<+))
    ++  amok
      %_  ..zo
        q.tad.bay  (~(del by q.tad.bay) num)
        dym.bay    (~(del by dym.bay) nah)
      ==
    ++  camo                                            ::  stop requests
      ^+  .
      =+  kiz=(~(tap in kig))
      |-  ^+  +>
      ?~  kiz  +>
      $(kiz t.kiz, mow :_(mow [hen (cancel i.kiz)]))
    ::
    ++  cancel                                          ::  stop a request
      |=  {van/vane ren/care:clay bem/beam}  
      ^-  (wind note gift:able)
      ?+  van  ~|(stub-cancel+van !!)
        $c  [%pass (camp-wire +<) van [%warp [our p.bem] q.bem ~]]
        $g  [%pass (camp-wire +<) van [%deal [our p.bem] q.bem [%pull ~]]]
      ==
    ++  camp-wire                                       ::  encode block
      |=  {van/vane ren/care:clay bem/beam}  ^-  wire
      [(scot %p our) (scot %ud num) van ren (en-beam bem)]
    ::
    ++  camp                                            ::  request a file
      |=  {van/vane ren/care:clay bem/beam}
      ^+  +>
      ~&  >>  [%camping van ren bem]
      %_    +>.$
          kig  (~(put in kig) +<)
          mow
        :_  mow
        :-  hen
        ?+    van  ~&(%camp-stub !!)
            $g
          :+  %pass  (camp-wire +<)
          =+  ^=  tyl
              ?.  ?=($x ren)
                s.bem
              ?>  ?=(^ s.bem)
              t.s.bem

          [%g [%deal [our p.bem] q.bem [%peer %scry ren (flop tyl)]]]
        ::
            $c
          :+  %pass  (camp-wire +<)
          [%c [%warp [our p.bem] q.bem [~ %sing ren r.bem (flop s.bem)]]]
        ==
      ==
    ::
    ++  clad                                            ::  hash dependencies
      |*  hoc/(bolt)  ^+  [*@uvH hoc]
      ?:  ?=($1 -.q.hoc)  [*@uvH hoc]
      =^  dep  r.p.hoc  (daze [p.q r.p]:hoc)
      [dep hoc]
    ::
    ++  clef                                            ::  cache a result
      |*  sem/*
      |*  {hoc/(bolt) fun/(burg)}
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
          ::     ?+  sem  `term`sem
          ::       $hood  [%hood (en-beam &1.q.q.hoc)]
          ::       $bake  [%bake `mark`&1.q.q.hoc (en-beam |2.q.q.hoc)]
          ::     ==
          :-  p=(came p.nuf `calx`[sem `calm`[now p.q.nuf] q.q.hoc q.q.nuf])
          q=q.nuf
        ==
      ==
    ::
    ++  coax  !.                                        ::  bolt across
      |*  {hoc/(bolt) fun/(burg)}
      ?-  -.q.hoc
        $0  =+  nuf=$:fun(,.+<- p.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              $0  [%0 p=(~(uni in p.q.hoc) p.q.nuf) q=[q.q.hoc q.q.nuf]]
              $1  q.nuf
              $2  q.nuf
            ==
        $1  =+  nuf=$:fun(,.+<- p.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              $0  q.hoc
              $1  [%1 p=(~(uni in p.q.nuf) p.q.hoc)]
              $2  q.nuf
            ==
        $2  hoc
      ==
    ::
    ++  cool                                            ::  error caption
      |*  {cyt/$@(term (trap tank)) hoc/(bolt)}
      ?.  ?=($2 -.q.hoc)  hoc
      [p=p.hoc q=[%2 p=p.q.hoc q=[?^(cyt *cyt [>`@tas`cyt<]~) q.q.hoc]]]
    ::
    ++  cope                                            ::  bolt along
      |*  {hoc/(bolt) fun/(burg)}
      ?-  -.q.hoc
        $1  hoc
        $2  hoc
        $0  =+  nuf=(fun p.hoc q.q.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              $1  q.nuf
              $2  [%2 p=(~(uni in `_p.q.nuf`p.q.hoc) p.q.nuf) q=q.q.nuf]
              $0  [%0 p=(~(uni in `_p.q.nuf`p.q.hoc) p.q.nuf) q=q.q.nuf]
      ==    ==
    ::
    ++  coop                                            ::  bolt alter
      |*  {hoc/(bolt) fun/$-(cafe (bolt))}
      ?-  -.q.hoc
        $1  hoc
        $0  hoc
        $2  =+  nuf=(fun p.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              $1  q.nuf
              $0  [%0 p=(~(uni in `_p.q.nuf`p.q.hoc) p.q.nuf) q=q.q.nuf]
              $2  =.  q.q.nuf  (welp q.q.nuf q.q.hoc)
                  [%2 p=(~(uni in `_p.q.nuf`p.q.hoc) p.q.nuf) q=q.q.nuf]
      ==    ==
    ::
    ++  coup                                            ::  toon to bolt
      |=  cof/cafe
      |*  {ton/toon fun/gate}
      :-  p=cof
      ^=  q
      ?-  -.ton
        $2  [%2 p=*(set beam) q=p.ton]
        $0  [%0 p=*(set beam) q=(fun p.ton)]
        $1  ::  ~&  [%coup-need ((list path) p.ton)]
            =-  ?-  -.faw
                  $&  :-  %1
                      ^=  p
                      %-  silt
                      %+  turn  p.faw
                      |=(a/{vane care:clay beam} [-.a +<.a +>.a *tang])
                  $|  [%2 p=*(set beam) q=p.faw]
                ==
            ^=  faw
            |-  ^-  (each (list (trel vane care:clay beam)) tang)
            ?~  p.ton  [%& ~]
            =+  nex=$(p.ton t.p.ton)
            =+  err=|=(a/tape [%| leaf+a ?:(?=($& -.nex) ~ p.nex)])
            =+  pax=(path i.p.ton)
            ?~  pax  (err "blocking empty")
            =+  ren=((soft care:clay) (rsh 3 1 i.pax))
            ?~  ren
              (err "blocking not care: {<i.pax>}")
            =+  zis=(de-beam t.pax)
            ?~  zis
              (err "blocking not beam: {<t.pax>}")
            ?:  ?=($g (end 3 1 i.pax))
              ?-  -.nex
                $&  [%& [%g u.ren u.zis] p.nex]
                $|  nex
              ==
            ?:  ?=($c (end 3 1 i.pax))
              ?-  -.nex
                $&  [%& [%c u.ren u.zis] p.nex]
                $|  nex
              ==
            (err "blocking bad vane")
      ==
    ::
    ++  cowl                                            ::  each to bolt
      |=  cof/cafe
      |*  {tod/(each * tang) fun/gate}
      %+  (coup cof)
        ?-  -.tod
          $&  [%0 p=p.tod]
          $|  [%2 p=p.tod]
        ==
      fun
    ::
    ++  tabl-run                                        ::  apply to all elems
      |=  fun/(burg cage gage)
      |=  {cof/cafe gag/gage}
      ^-  (bolt gage)
      ?.  ?=($tabl -.gag)
        (cope (flay cof gag) fun)
      %+  cope
        |-  ^-  (bolt (list (pair gage gage)))
        ?~  p.gag  (fine cof ~)
        %.  [cof p.gag]
        ;~  cope
          ;~  coax
            |=({cof/cafe {^ q/gage} t/gagl} (fret ^^$(cof cof, gag q)))
            |=({cof/cafe ^ t/gagl} ^$(cof cof, p.gag t))
          ==
          (flux |=({v/gage t/gagl} [[p.i.p.gag v] t]))
        ==
      (flux |=(rex/gagl [%tabl rex]))
    ::
    ++  some-in-map
      |*  fun/(burg knot (unit))
      =+  res=_(need [?+(-.q !! $0 q.q)]:*fun)
      =+  marv=(map knot res)
      |=  {cof/cafe sud/(map knot $~)}  ^-  (bolt marv)
      ?~  sud  (flue cof)
      %.  [cof sud]
      ;~  cope
        ;~  coax
          |=({cof/cafe _sud} ^$(cof cof, sud l))
          |=({cof/cafe _sud} ^$(cof cof, sud r))
          |=  {cof/cafe {dir/@ta $~} ^}
          %+  cope  (fun cof dir)
          (flux (lift |*(* [dir +<])))
        ==
        %-  flux
        |=  {lam/marv ram/marv nod/(unit {knot res})}
        ?^(nod [u.nod lam ram] (~(uni by lam) ram))
      ==
    ++  dash                                          ::  process cache
      |=  cof/cafe
      ^+  +>
      %_(+> jav.bay q.cof, deh.bay r.cof)
    ::
    ++  diff                                            ::  diff
      |=  {cof/cafe kas/silk kos/silk}
      ^-  (bolt gage)
      %.  [cof kas kos]
      ;~  cope
        ;~  coax
          |=({cof/cafe p/silk q/silk} (cope (make cof p) flay))
          |=({cof/cafe p/silk q/silk} (cope (make cof q) flay))
        ==
        |=  {cof/cafe cay/cage coy/cage}  ^-  (bolt gage)
        ?.  =(p.cay p.coy)
          %+  flaw  cof  :_  ~
          leaf+"diff on data of different marks: {(trip p.cay)} {(trip p.coy)}"
        ?:  =(q.q.cay q.q.coy)
          (fine cof [%& %null [%atom %n ~] ~])
        ::
        %+  cope  (fang cof p.cay)
        |=  {cof/cafe pro/vase}
        ?.  (slab %grad p.pro)
          (flaw cof leaf+"no ++grad" ~)
        =+  gar=(slap pro [%limb %grad])
        ?@  q.gar
          =+  for=((sand %tas) q.gar)
          ?~  for  (flaw cof leaf+"bad mark ++grad" ~)
          %+  make  cof  ^-  silk
          :+  %diff
            [%cast u.for [%$ cay]]
          [%cast u.for [%$ coy]]
        ?.  (slab %form p.gar)
          (flaw cof leaf+"no ++form:grad" ~)
        ?.  (slab %diff p.gar)
          (flaw cof leaf+"no ++diff:grad" ~)
        %+  cope  (keel cof pro [[%& 6]~ q.cay]~)
        |=  {cof/cafe pox/vase}
        %+  cope
          %^  maul  cof
            (slap (slap pox [%limb %grad]) [%limb %diff])
          q.coy
        |=  {cof/cafe dif/vase}
        =+  for=((soft @tas) q:(slap gar [%limb %form]))
        ?~  for
          (flaw cof leaf+"bad ++form:grad" ~)
        (fine cof [%& u.for dif])
      ==
    ::
    ++  daze                                            ::  remember depends
      |=  {dep/(set beam) deh/(map @uvH deps)}
      ^+  [*@uvH deh.bay]
      =.  dep
        =<  (silt (skip (~(tap in dep)) .))
        |=  dap/beam  ^-  ?
        ?~  s.dap  |
        =>(.(s.dap t.s.dap) |((~(has in dep) dap) $))
      ?~  dep  [0v0 deh]
      =+  hap=(sham dep)
      ?:  (~(has by deh) hap)
        [hap deh]
      [hap (~(put by deh) hap [%init dep])]
    ::
    ++  exec                                            ::  execute app
      ^+  ..zo
      ?:  !=(~ kig)  ..zo
      =+  bot=(make-norm-bek [~ jav.bay deh.bay] kas)
      =^  dep  bot  (clad bot)
      =.  ..exec  (dash p.bot)
      ?-  -.q.bot
        $0  amok:(expo [%made dep q.q.bot])
        $2  amok:(expo [%made dep %| q.q.bot])
        $1  =+  zuk=(~(tap by p.q.bot) ~)
            =<  abet
            |-  ^+  ..exec
            ?~  zuk  ..exec
            $(zuk t.zuk, ..exec `_..exec`(camp van.i.zuk ren.i.zuk bem.i.zuk))
      ==
    ::
    ++  expo                                            ::  return gift
      |=  gef/gift:able
      %_(+> mow :_(mow [hen %give gef]))
    ::
    ++  fade                                            ::  compile to hood
      ~/  %fade
      |=  {cof/cafe bem/beam}
      :: ~&  fade+(en-beam bem)
      ^-  (bolt hood)
      %+  cool  |.(leaf+"ford: fade {<[(en-beam bem)]>}")
      %+  cope  (liar cof %*(. bem s [%hoon s.bem]))
      |=  {cof/cafe cay/cage}
      %+  (clef %hood)  (fine cof bem(r [%ud 0]) cay)
      ^-  (burg (pair beam cage) hood)
      ~%  %hood-miss  ..abet  ~
      |=  {cof/cafe bem/beam cay/cage}
      ?.  ?=(@ q.q.cay)
        (flaw cof ~)
      =+  vex=((full (fair bem)) [[1 1] (trip q.q.cay)])
      ?~  q.vex
        (flaw cof [%leaf "syntax error: {<p.p.vex>} {<q.p.vex>}"] ~)
      (fine cof p.u.q.vex)
    ::
    ++  fame                                            ::  beam with - as /
      ~/  %fame
      |=  {cof/cafe bem/beam}
      ^-  (bolt beam)
      =;  une/(bolt (unit beam))
        %+  cope  une
        |=  {cof/cafe bom/(unit beam)}  ^-  (bolt beam)
        ?^  bom  (fine cof u.bom)
        (flaw cof leaf+"fame: no {<(en-beam bem)>}" ~)
      %+  (clef %path)  (fine cof bem)
      |=  {cof/cafe bem/beam}
      =^  pax  bem  [(flop s.bem) bem(s ~)]
      |^  opts
      ++  opts                                          ::  search unless done
        ^-  (bolt (unit beam))
        ?^  pax  (wide(pax t.pax) (tear i.pax))
        %+  cope  (lima cof %hoon bem)
        (flux |=(a/(unit vase) ?~(a ~ `bem)))
      ::
      ++  wide                                          ::  match segments
        |=  sub/(list term)  ^-  (bolt (unit beam))
        ?~  sub  opts
        ?~  t.sub  opts(s.bem [i.sub s.bem])
        =>  .(sub `(list term)`sub)                     ::  TMI
        =-  (cope - flat)
        %^  lash  cof  bem
        |=  {cof/cafe dir/knot}  ^-  (bolt (unit beam))
        =+  sus=(tear dir)
        ?.  =(sus (scag (lent sus) sub))
          (flue cof)
        %_  ^$
          cof  cof
          sub   (slag (lent sus) sub)
          s.bem  [dir s.bem]
        ==
      ::
      ++  flat                                          ::  at most one
        |=  {cof/cafe opt/(map term beam)}  ^-  (bolt (unit beam))
        ?~  opt  (flue cof)
        ?:  ?=({^ $~ $~} opt)  (fine cof `q.n.opt)
        =+  all=(~(run by `(map term beam)`opt) en-beam)
        (flaw cof leaf+"fame: fork {<all>}" ~)
      --
    ::
    ++  fang                                            ::  protocol door
      |=  {cof/cafe for/mark}  ^-  (bolt vase)
      :: ~&  fang+for
      (lear cof bek /[for]/mar)
    ::
    ++  fair                                            ::  hood parsing rule
      |=  bem/beam
      ?>  ?=({$ud $0} r.bem)          ::  XX sentinel
      =+  vez=(vang & (en-beam bem))
      =<  hood
      |%
      ++  case
        %+  sear
          |=  a/coin
          ?.  ?=({$$ ?($da $ud $tas) *} a)  ~
          [~ u=(^case a)]
        nuck:so
      ::
      ++  mota  ;~(pfix pat mota:vez)                   ::  atom odor
      ++  hath  (sear plex (stag %conl poor)):vez       ::  hood path
      ++  have  (sear de-beam ;~(pfix fas hath))           ::  hood beam
      ++  hith                                          ::  static path
        =>  vez
        (sear plex (stag %conl (more fas hasp)))
      ::
      ++  hive                                          ::  late-bound path
        ;~  pfix  fas
          %+  cook  |=(a/hops a)
          =>  vez
          ;~  plug
            (stag ~ gash)
            ;~(pose (stag ~ ;~(pfix cen porc)) (easy ~))
          ==
        ==
      ::
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
      ++  hoot
        ;~  plug
          sym
          ;~  pose
            %+  stag  ~
            ;~(plug ;~(pfix fas case) ;~(pfix ;~(plug fas sig) fed:ag))
            (easy ~)
          ==
        ==
      ::
      ++  hoof
        %+  cook  |=(a/^hoof a)
        ;~  pose
          (stag %| ;~(pfix tar hoot))
          (stag %& hoot)
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
        =|  tol/?
        |%
        ++  apex
          %+  knee  *^horn  |.  ~+
          ;~  pfix  fas
            ;~  pose
              (stag %ape ;~(pfix sig ape:read))
              (stag %arg ;~(pfix buc ape:read))
              (stag %alt ;~(pfix bar alt:read))
              (stag %dep ;~(pfix hax day:read))
              (stag %dub ;~(pfix tis dub:read))
              (stag %fan ;~(pfix dot fan:read))
              (stag %for ;~(pfix com for:read))
              (stag %hel ;~(pfix cen day:read))
              (stag %lin ;~(pfix pam lin:read))
              (stag %man ;~(pfix tar man:read))
              (stag %nap ;~(pfix cab day:read))
              (stag %nod ;~(pfix cab now:read))
              (stag %saw ;~(pfix sem saw:read))
              (stag %see ;~(pfix col see:read))
              (stag %sic ;~(pfix ket sic:read))
              (stag %toy ;~(sfix toy:read fas))
            ==
          ==
        ::
        ++  rail
          |*  {wid/rule tal/rule}
          ?.  tol  wid
          ;~(pose wid tal)
        ::
        ++  read
          |%  ++  ape
                %+  rail
                  (ifix [sel ser] (stag %conp (most ace wide:vez)))
                ;~(pfix gap tall:vez)
          ::
              ++  alt
                %+  rail  (ifix [pel per] (most ace day))
                ;~(sfix (star day) gap duz)
          ::
              ++  day
                %+  rail
                  apex(tol |)
                ;~(pfix gap apex)
          ::
              ++  dub
                %+  rail
                  ;~(plug sym ;~(pfix tis day))
                ;~(pfix gap ;~(plug sym day))
          ::
              ++  fan
                %+  rail  fail
                ;~(sfix (star day) gap duz)
          ::
              ++  for
                %+  rail  fail
                =-  ;~(sfix (star -) gap duz)
                ;~(pfix gap fas ;~(plug hith day))
          ::
              ++  lin
                %+  rail
                  ;~(plug (plus ;~(sfix sym pam)) day)
                =+  (cook |=(a/term [a ~]) sym)
                ;~(pfix gap ;~(plug - day))
          ::
              ++  man
                %+  rail  fail
                %+  cook  ~(gas by *(map term ^horn))
                =<  ;~(sfix (star (sear . day)) gap duz)
                |=  a/^horn  ^-  (unit {term ^horn})
                ?+(-.a ~ $dub `[p.a q.a])
          ::
              ++  now
                %+  rail  ;~((glue cab) mota day)
                ;~(pfix gap ;~(plug mota day))
          ::
              ++  saw
                %+  rail
                  ;~(plug ;~(sfix wide:vez sem) day)
                ;~(pfix gap ;~(plug tall:vez day))
          ::
              ++  see
                %+  rail
                  ;~(plug ;~(sfix hive col) day)
                ;~(pfix gap ;~(plug hive day))
          ::
              ++  sic
                %+  rail
                  ;~(plug ;~(sfix wide:vez ket) day)
                ;~(pfix gap ;~(plug tall:vez day))
          ::
              ++  toy  ;~(plug ;~(pose (cold | zap) (easy &)) sym)
          --
        ::
        --
      --
    ::
    ++  join
      |=  {cof/cafe for/mark kas/silk kos/silk}
      ^-  (bolt gage)
      %.  [cof kas kos]
      ;~  cope
        ;~  coax
          |=({cof/cafe p/silk q/silk} (cope (make cof p) flay))
          |=({cof/cafe p/silk q/silk} (cope (make cof q) flay))
        ==
        |=  {cof/cafe cay/cage coy/cage}  ^-  (bolt gage)
        ::
        %+  cope  (fang cof for)
        |=  {cof/cafe pro/vase}
        ?.  (slab %grad p.pro)
          (flaw cof leaf+"no ++grad" ~)
        =+  gar=(slap pro [%limb %grad])
        ?@  q.gar
          =+  too=((sand %tas) q.gar)
          ?~  too  (flaw cof leaf+"bad mark ++grad" ~)
          (make cof %join u.too [%$ cay] [%$ coy])
        ?.  (slab %form p.gar)
          (flaw cof leaf+"no ++form:grad" ~)
        =+  fom=((soft @tas) q:(slap gar [%limb %form]))
        ?~  fom
          (flaw cof leaf+"bad ++form:grad" ~)
        ?.  &(=(u.fom p.cay) =(u.fom p.coy))
          %+  flaw  cof  :_  :_  ~
            leaf+"join on data of bad marks: {(trip p.cay)} {(trip p.coy)}"
          leaf+"expected mark {(trip u.fom)}"
        ?:  =(q.q.cay q.q.coy)
          (fine cof [%& cay])
        ?.  (slab %join p.gar)
          (flaw cof leaf+"no ++join:grad" ~)
        %+  cope
          %^  maul  cof
            (slap (slap pro [%limb %grad]) [%limb %join])
          (slop q.cay q.coy)
        |=  {cof/cafe dif/vase}
        ?@  q.dif
          (fine cof [%& %null dif])
        (fine cof [%& u.fom (slot 3 dif)])
      ==
    ::
    ++  mash
      |=  {cof/cafe for/mark mas/milk mos/milk}
      ^-  (bolt gage)
      %.  [cof r.mas r.mos]
      ;~  cope
        ;~  coax
          |=({cof/cafe p/silk q/silk} (cope (make cof p) flay))
          |=({cof/cafe p/silk q/silk} (cope (make cof q) flay))
        ==
        |=  {cof/cafe cay/cage coy/cage}  ^-  (bolt gage)
        %+  cope  (fang cof for)
        |=  {cof/cafe pro/vase}
        ?.  (slab %grad p.pro)
          (flaw cof leaf+"no ++grad" ~)
        =+  gar=(slap pro [%limb %grad])
        ?@  q.gar
          =+  too=((sand %tas) q.gar)
          ?~  too  (flaw cof leaf+"bad mark ++grad" ~)
          %+  make  cof
          `silk`[%mash u.too [p.mas q.mas [%$ cay]] [p.mos q.mos [%$ coy]]]
        ?.  (slab %form p.gar)
          (flaw cof leaf+"no ++form:grad" ~)
        =+  fom=((soft @tas) q:(slap gar [%limb %form]))
        ?~  fom
          (flaw cof leaf+"bad ++form:grad" ~)
        ?.  &(=(u.fom p.cay) =(u.fom p.coy))
          %+  flaw  cof  :_  :_  ~
            leaf+"mash on data of bad marks: {(trip p.cay)} {(trip p.coy)}"
          leaf+"expected mark {(trip u.fom)}"
        ?:  =(q.q.cay q.q.coy)
          (fine cof %& cay)
        ?.  (slab %mash p.gar)
          (fine cof %& %null [%atom %n ~] ~)
        %+  cope
          %^  maul  cof
            (slap (slap pro [%limb %grad]) [%limb %mash])
          %+  slop
            :(slop [[%atom %p ~] p.mas] [[%atom %tas ~] q.mas] q.cay)
          :(slop [[%atom %p ~] p.mos] [[%atom %tas ~] q.mos] q.coy)
        (flux |=(dif/vase [%& u.fom dif]))
      ==
    ::
    ++  kale                                            ::  mutate
      |=  {cof/cafe kas/silk muy/(list (pair wing silk))}
      ^-  (bolt gage)
      %+  cope
        |-  ^-  (bolt (list (pair wing vase)))
        ?~  muy  (flue cof)
        %+  cope  (cope (make cof q.i.muy) flay)
        |=  {cof/cafe cay/cage}
        %+  cope  ^$(muy t.muy)
        |=  {cof/cafe rex/(list (pair wing vase))}
        (fine cof [[p.i.muy q.cay] rex])
      |=  {cof/cafe yom/(list (pair wing vase))}
      %+  cope  (make cof kas)
      %-  tabl-run
      |=  {cof/cafe cay/cage}
      %+  cope  (keel cof q.cay yom)
      (flux |=(vax/vase [%& p.cay vax]))
    ::
    ++  keel                                            ::  apply mutations
      |=  {cof/cafe suh/vase yom/(list (pair wing vase))}
      ^-  (bolt vase)
      %+  cool
        =<  |.  ^-  tank
            :+  %palm  [" " ~ ~ ~]
            ~[leaf+"ford: keel" rose+[" " ~ ~]^(murn yom +)]
        |=  {a/wing b/span *}  ^-  (unit tank)
        =+  typ=(mule |.(p:(slap suh wing+a)))
        ?:  ?=($| -.typ)
          (some (show [%c %pull] %l a))
        ?:  (~(nest ut p.typ) | b)  ~
        %^  some  %palm  ["." ~ ~ ~]
        ~[(show [%c %mute] %l a) >[p.typ b]<]
      %^  maim  cof
        %+  slop  suh
        |-  ^-  vase
        ?~  yom  [[%atom %n ~] ~]
        (slop q.i.yom $(yom t.yom))
      ^-  twig
      :+  %keep  [%& 2]~
      =+  axe=3
      |-  ^-  (list (pair wing twig))
      ?~  yom  ~
      :-  [p.i.yom [%$ (peg axe 2)]]
      $(yom t.yom, axe (peg axe 3))
    ::
    ++  lads                                            ::  possible children
      |=  {cof/cafe bem/beam}
      ^-  (bolt (map knot $~))
      %^  lash  cof  bem
      |=  {cof/cafe dir/knot}
      %+  cope  (lend cof bem(s [dir s.bem]))
      (flux |=(a/arch ?~(dir.a ~ (some ~))))
    ::
    ++  laze                                            ::  find real or virtual
      |=  {cof/cafe bem/beam}
      %^  lash  cof  bem
      |=  {cof/cafe for/mark}
      ^-  (bolt (unit $~))
      ?.  ((sane %tas) for)  (flue cof)
      =.  s.bem  [for s.bem]
      %+  cope  (lend cof bem)
      |=  {cof/cafe arc/arch}
      (fine cof (bind fil.arc $~))
    ::
    ++  lace                                            ::  load file
      |=  {cof/cafe for/mark bem/beam}
      ^-  (bolt vase)
      %+  cool  |.(leaf+"ford: load {<for>} {<(en-beam bem)>}")
      =.  s.bem  [for s.bem]
      %+  cope  (liar cof bem)
      |=  {cof/cafe cay/cage}  ^-  (bolt vase)
      ?.  =(for p.cay)
        (flaw cof leaf+"unexpected mark {<p.cay>}" ~)
      (fine cof q.cay)
    ::
    ++  lake                                            ::  check+coerce
      |=  {fit/? for/mark}
      |=  {cof/cafe sam/vase}
      ^-  (bolt vase)
      %+  cool  |.(leaf+"ford: check {<[for bek `@p`(mug q.sam)]>}")
      %+  cope  (fang cof for)
      |=  {cof/cafe tux/vase}
      =+  typ=p:(slot 6 tux)
      =.  typ  ?+(-.typ typ $face q.typ)
      ?:  (~(nest ut typ) | p.sam)
        (fine cof typ q.sam)
      ?.  fit  (flaw cof [%leaf "ford: invalid type: {<p.sam>}"]~)
      ?.  (slob %grab p.tux)
        (flaw cof [%leaf "ford: no grab: {<[for bek]>}"]~)
      =+  gab=(slap tux [%limb %grab])
      ?.  (slob %noun p.gab)
        (flaw cof [%leaf "ford: no noun: {<[for bek]>}"]~)
      %+  cope  (maul cof (slap gab [%limb %noun]) [%noun q.sam])
      |=  {cof/cafe pro/vase}
      ?>  (~(nest ut typ) | p.pro)
      ?:  =(q.pro q.sam)
        (fine cof typ q.pro)
      (flaw cof [%leaf "ford: invalid content: {<[for bek]>}"]~)
    ::
    ++  lamp                                            ::  normalize version
      |=  {cof/cafe bem/beam}
      ^-  (bolt beam)
      ?:  ?=($ud -.r.bem)  (fine cof bem)
      =+  von=(syve [151 %noun] ~ %cw bem(s ~))
      ?~  von  [p=cof q=[%1 [%c %w bem ~] ~ ~]]
      (fine cof bem(r [%ud ((hard @) +.+:(need u.von))]))
    ::
    ++  lane                                            ::  span infer
      |=  {cof/cafe typ/span gen/twig}
      %+  (cowl cof)  (mule |.((~(play ut typ) gen)))
      |=(ref/span ref)
    ::
    ++  lash                                            ::  filter at beam
      |*  {cof/cafe bem/beam fun/(burg knot (unit))}
      %+  cope  (lend cof bem)
      |=({cof/cafe arc/arch} ((some-in-map fun) cof dir.arc))
    ::
    ++  lear                                            ::  load core
      |=  {cof/cafe bem/beam}  ^-  (bolt vase)
      %+  cope  (lamp cof bem)
      |=  {cof/cafe bem/beam}
      (leap cof many+~ bem bem)
    ::
    ++  leap                                            :: XX load with path
      ~/  %leap
      |=  {cof/cafe arg/coin bem/beam bom/beam}
      %+  cope  (lamp cof bem)
      |=  {cof/cafe bem/beam}
      %+  (clef %boil)  (fine cof arg bem bom)
      |=  {cof/cafe arg/coin bem/beam bom/beam}
      %+  cope  (fame cof bem)
      |=  {cof/cafe bem/beam}
      (cope (fade cof bem) abut:(meow bom arg))
    ::
    ++  lend                                            ::  load arch
      |=  {cof/cafe bem/beam}
      ^-  (bolt arch)
      =+  von=(syve [151 %noun] ~ %cy bem)
      ?~  von  [p=cof q=[%1 [%c %y bem ~] ~ ~]]
      ?>  ?=({$~ $arch ^} u.von)
      =+  arc=((hard arch) q.q.u.u.von)
      %+  cope  (lamp cof bem)
      |=  {cof/cafe bem/beam}
      (flag bem (fine cof arc))
    ::
    ++  liar                                            ::  load cage
      ~/  %liar
      |=  {cof/cafe bem/beam}
      ^-  (bolt cage)
      ?:  =([%ud 0] r.bem)
        (flaw cof [leaf+"ford: no data: {<(en-beam bem(s ~))>}"]~)
      =+  von=(syve [151 %noun] ~ %cx bem)
      ?~  von
        [p=cof q=[%1 [[%c %x bem ~] ~ ~]]]
      ?~  u.von
        (flaw cof leaf+"file not found" (smyt (en-beam bem)) ~)
      (fine cof u.u.von)
    ::
    ++  lily
      ~/  %lily
      |=  {cof/cafe for/mark}  ^-  (bolt (set @tas))
      %+  cope  (coop (fang cof for) |=(cof/cafe (fine cof %void ~)))
      %-  flux
      |=  vax/vase  ^-  (set mark)
      %-  =-  ~(gas in `(set mark)`-)
          ?.  (slob %grow p.vax)  ~
          (silt (sloe p:(slap vax [%limb %grow])))
      ?.  (slob %garb p.vax)  ~
      =+  (slap vax [%limb %garb])
      (fall ((soft (list mark)) q) ~)
    ::
    ++  lima                                            ::  load at depth
      ~/  %lima
      |=  {cof/cafe for/mark bem/beam}
      %+  (clef %bake)  (flag bem (fine cof for bem))
      |=  {cof/cafe for/mark bem/beam}
      ^-  (bolt (unit vase))
      %+  cope  (laze cof bem)
      |=  {cof/cafe mal/(map mark $~)}
      ?:  (~(has by mal) for)
        (cope (lace cof for bem) (flux some))
      =+  opt=(silt (turn (~(tap by mal)) head))        ::  XX asymptotics
      %+  cope  (lion cof for opt)
      |=  {cof/cafe wuy/(list @tas)}
      ?~  wuy  (flue cof)
      %+  cope
        (lace cof i.wuy bem)
      |=  {cof/cafe hoc/vase}
      (cope (lope cof i.wuy t.wuy hoc) (flux some))
    ::
    ++  lime                                            ::  load beam
      |=  {cof/cafe for/mark arg/coin bem/beam}
      ^-  (bolt vase)
      %+  coop  (leap cof arg [-.bem /[for]/ren] bem)
      |=  cof/cafe  ^-  (bolt vase)
      %+  cope  (lima cof for bem)
      |=  {cof/cafe vux/(unit vase)}
      ?^  vux  (fine cof u.vux)
      (flaw cof leaf+"ford: no {<for>} at {<(en-beam bem)>}" ~)
    ::
    ++  link                                            ::  translate
      ~/  %link
      |=  {cof/cafe too/mark for/mark vax/vase}
      =*  link-jet  .
      :: ~$  link
      ^-  (bolt vase)
      :: %+  cool   |.(leaf+"ford: link {<too>} {<for>} {<p.vax>}")
      ?:  =(too for)  (fine cof vax)
      ?:  |(=(%noun for) =(%$ for))
        ((lake & too) cof vax)
      %+  cope  (fang cof for)
      |=  {cof/cafe pro/vase}  ^-  (bolt vase)
      ?:  :: =<  $  ~%  %limb-grow  link-jet  ~  |.
          &((slob %grow p.pro) (slob too p:(slap pro [%limb %grow])))
        :: ~$  link-grow
        :: =<  $  ~%  %grow  link-jet  ~  |.
        %+  cope  (keel cof pro [[%& 6]~ vax]~)
        |=  {cof/cafe pox/vase}
        (maim cof pox [%per [%limb %grow] [%limb too]])
      %+  cope  (fang cof too)
      ~%  %grab  link-jet  ~
      |=  {cof/cafe pro/vase}
      =+  :: =<  $  ~%  %limb-grab  +  ~  |.
          ^=  zat  ^-  (unit vase)
          ?.  (slob %grab p.pro)  ~
          =+  gab=(slap pro [%limb %grab])
          ?.  (slob for p.gab)  ~
          `(slap gab [%limb for])
      ?~  zat
        :: ~$  link-miss
        (flaw cof [%leaf "ford: no link: {<[for too]>}"]~)
      :: ~$  link-grab
      ~|  [%link-maul for too]
      (maul cof u.zat vax)
    ::
    ++  lion                                            ::  translation search
      ~/  %lion
      |=  {cof/cafe too/mark fro/(set mark)}
      =*  lion-jet  .
      :: ~&  lion+[too=too fro=fro]
      :: =-  =+  (cope - (flux |=(a/(list mark) ~&(lioned+a ~))))
      ::     +<
      ^-  (bolt (list mark))
      =-  %+  coop  (gro cof too ~ ~)                    :: XX better grab layer
          ~%  %grab  lion-jet  ~
          |=  cof/cafe
          %+  cope  (fang cof too)
          |=  {cof/cafe vax/vase}  ^-  (bolt (list mark))
          ?.  (slob %grab p.vax)  (flue cof)
          %+  cope
            (gro cof (silt (sloe p:(slap vax [%limb %grab]))))
          (flux |=(a/path (welp a /[too])))
      ^=  gro
      |=  {cof/cafe tag/(set mark)}
      =|  $:  war/(map mark (list mark))
              pax/(list mark)
              won/{p/mark q/(qeu mark)}
          ==
      %.  [cof fro]
      |=  {cof/cafe fro/(set mark)}  ^-  (bolt (list mark))
      ?:  (~(has in tag) p.won)
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
      |=  {cof/cafe for/mark yaw/(list mark) vax/vase}
      ^-  (bolt vase)
      ?~  yaw  (fine cof vax)
      %+  cope  (link cof i.yaw for vax)
      |=  {cof/cafe yed/vase}
      ^$(cof cof, for i.yaw, yaw t.yaw, vax yed)
    ::
    ++  mail                                            ::  cached mint
      ~/  %mail
      |=  {cof/cafe sut/span gen/twig}
      ^-  (bolt (pair span nock))
      %+  (clef %slim)  (fine cof sut gen)
      |=  {cof/cafe sut/span gen/twig}
      =+  puz=(mule |.((~(mint ut sut) [%noun gen])))
      ?-  -.puz
        $|  (flaw cof p.puz)
        $&  (fine cof p.puz)
      ==
    ::
    ++  maim                                            ::  slap
      ~/  %maim
      |=  {cof/cafe vax/vase gen/twig}
      ^-  (bolt vase)
      %+  cope  (mail cof p.vax gen)
      |=  {cof/cafe typ/span fol/nock}
      %+  (coup cof)  (mock [q.vax fol] (sloy syve))
      |=(val/* `vase`[typ val])
    ::
    ++  make-norm-bek                                   ::  normalize root beak
      |=  {cof/cafe kas/silk}
      %+  cope  (lamp cof bek ~)
      |=({cof/cafe byk/beak *} (make(bek byk) cof kas))
    ::
    ++  abbrev                                          ::  shorten coin
      |=(a/coin ?-(-.a $$ a, $blob a(p (mug p.a)), $many a(p (turn p.a ..$))))
    ::
    ++  make                                            ::  reduce silk
      |=  {cof/cafe kas/silk}
      :: =+  ^=  pre
      ::     ?+  -.kas  `term`-.kas
      ::       ^  %cell
      ::       $bake  [-.kas p.kas (en-beam r.kas) ~(rent co (abbrev q.kas))]
      ::       $core  [-.kas (en-beam p.kas)]
      ::     ==
      :: ~?  !=(%$ pre)  [dyv `term`(cat 3 %make (fil 3 dyv ' ')) pre]
      :: =-  ~?  !=(%$ pre)  [dyv `term`(cat 3 %made (fil 3 dyv ' ')) pre]  -
      =.  dyv  +(dyv)
      ^-  (bolt gage)
      ?-    -.kas
          ^
        %.  [cof p.kas q.kas]
        ;~  cope
          ;~  coax
            |=({cof/cafe p/silk q/silk} (cope ^$(cof cof, kas p.kas) flay))
            |=({cof/cafe p/silk q/silk} (cope ^$(cof cof, kas q.kas) flay)) 
          ==                          ::  XX merge %tabl
        ::
          |=  {cof/cafe bor/cage heg/cage}  ^-  (bolt gage)
          (faun cof (slop q.bor q.heg))
        ==
      ::
          $$  (fine cof %& p.kas)
          $alts
        %.  cof
        |=  cof/cafe  ^-  (bolt gage)
        ?~  p.kas  (flaw cof leaf+"ford: out of options" ~)
        (coop ^$(cof cof, kas i.p.kas) ..$(p.kas t.p.kas))
      ::
          $bake
        ^-  (bolt gage)
        %+  cool
          |.(leaf+"ford: bake {<p.kas>} {<(en-beam r.kas)>} {~(rend co q.kas)}")
        %+  cope  (lamp cof r.kas)
        |=  {cof/cafe bem/beam}
        %+  cope  (lime cof p.kas q.kas bem)
        |=  {cof/cafe vax/vase}
        (fine cof `gage`[%& p.kas vax])
      ::
          $bunt
        %+  cool  |.(leaf+"ford: bunt {<p.kas>}")
        %+  cope  (fang cof p.kas)
        |=  {cof/cafe tux/vase}
        =+  [typ=p val=q]:(slot 6 tux)
        =.  typ  ?+(-.typ typ $face q.typ)
        (fine cof [%& p.kas [typ val]])
      ::
          $call
        ::  %+  cool  |.(leaf+"ford: call {<`@p`(mug kas)>}")
        %.  [cof p.kas q.kas]
        ;~  cope
          ;~  coax
            |=({cof/cafe p/silk q/silk} (cope ^$(cof cof, kas p) flay))
            |=({cof/cafe p/silk q/silk} ^$(cof cof, kas q))
          ==
        ::
          |=  {cof/cafe gat/cage sam/gage}
          %.  [cof sam]
          %-  tabl-run
          |=  {cof/cafe sam/cage}
          (cope (maul cof q.gat q.sam) faun)
        ==
      ::
          $cast
        %+  cool  |.(leaf+"ford: cast {<p.kas>}")
        %+  cope  $(kas q.kas)
        %-  tabl-run
        |=  {cof/cafe cay/cage}
        :: ~$  make-cast
        :: ~>  %live.  :: ~$(make-cast-{to}--{from} ~)
        ::   (rap 3 %make-cast- p.kas '--' p.cay ~)
        ^-  (bolt gage)
        %+  cool  |.(leaf+"ford: casting {<p.cay>} to {<p.kas>}")
        %+  cope  (lion cof p.kas p.cay `~)
        |=  {cof/cafe wuy/(list @tas)}
        %+  cope
          ?~  wuy
            (link cof p.kas p.cay q.cay)
          (lope cof i.wuy t.wuy q.cay)
        (flux |=(vax/vase [%& p.kas vax]))
      ::
          $core
        %+  cool  |.(leaf+"ford: core {<(en-beam p.kas)>}")
        ::  code runtime behaviour is frequently affected by marks
        ::  TODO: track this more formally
        %+  flag  [bek /mar]
        ::  until /? is in use, any hoon may implicitly depend on arvo types
        %+  flag  [bek /arvo/hoon]
        %+  flag  [bek /arvo/zuse]
        (cope (lear cof p.kas) (flux |=(a/vase [%& %core a])))
      ::
          $diff
        %+  cool  |.(leaf+"ford: diff {<`@p`(mug p.kas)>} {<`@p`(mug q.kas)>}")
        (diff cof p.kas q.kas)
      ::
          $dude  (cool p.kas $(kas q.kas))
          $file
        %+  cool  |.(leaf+"ford: file {<p.kas>}")
        %+  cope  (liar cof p.kas)
        (flux |=(cay/cage [%& cay]))
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
        ?~  dap    ~&(flag-missed+n.p.kas p.q.rez)
        %-  ~(uni in p.q.rez)  ^-  (set beam)
        ?-(-.u.dap $init p.u.dap, $sent q.u.dap, $done [[bek ~] ~ ~])
      ::                              XX revisit ^ during dependency review
          $join
        %+  cool
          |.
          leaf+"ford: join {<p.kas>} {<`@p`(mug q.kas)>} {<`@p`(mug r.kas)>}"
        (join cof p.kas q.kas r.kas)
      ::
          $mash
        %+  cool
          |.
          leaf+"ford: mash {<p.kas>} {<`@p`(mug q.kas)>} {<`@p`(mug r.kas)>}"
        (mash cof p.kas q.kas r.kas)
      ::
          $mute  (kale cof p.kas q.kas)
          $pact
        %+  cool  |.(leaf+"ford: pact {<`@p`(mug p.kas)>} {<`@p`(mug q.kas)>}")
        (pact cof p.kas q.kas)
      ::
          $plan  (cope (abut:(meow p.kas q.kas) cof r.kas) faun)
          $reef  (faun cof pit)
          $ride
        %+  cool  |.(leaf+"ford: build failed {<hen>}")
        %+  cope  $(kas q.kas)
        %-  tabl-run
        |=  {cof/cafe cay/cage}
        %+  cope  (maim cof q.cay p.kas)
        |=  {cof/cafe vax/vase}
        (faun cof vax)
      ::
          $tabl
        %+  cope
          |-  ^-  (bolt (list (pair gage gage)))
          ?~  p.kas  (fine cof ~)
          %.  [cof p.kas]
          ;~  cope
            ;~  coax
              |=({cof/cafe _p.kas} (fret ^^$(cof cof, kas p.i)))
              |=({cof/cafe _p.kas} (fret ^^$(cof cof, kas q.i)))
              |=({cof/cafe _p.kas} ^$(cof cof, p.kas t))
            ==
            (flux |=({k/gage v/gage t/(list {gage gage})} [[k v] t]))
          ==
        (flux |=(rex/(list (pair gage gage)) [%tabl rex]))
      ::
          $vale
        %+  cool  |.(leaf+"ford: vale {<p.kas>} {<`@p`(mug q.kas)>}")
        %+  cope  ((lake & p.kas) cof [%noun q.kas])
        (flux |=(vax/vase `gage`[%& p.kas vax]))
      ::
          $volt
        %+  cool  |.(leaf+"ford: volt {<p.p.kas>}")
        %+  cope  $(kas [%bunt p.p.kas])
        %-  tabl-run
        |=  {cof/cafe cay/cage}
        ^-  (bolt gage)
        (fine cof [%& p.p.kas p.q.cay q.p.kas])
      ==
    ::
    ++  malt                                            ::  cached slit
      ~/  %slit
      |=  {cof/cafe gat/span sam/span}
      ^-  (bolt span)
      %+  (clef %slit)  (fine cof gat sam)
      |=  {cof/cafe gat/span sam/span}
      %+  cool  |.(%.(%have ~(dunk ut sam)))
      %+  cool  |.(%.(%want ~(dunk ut (~(peek ut gat) %free 6))))
      =+  top=(mule |.((slit gat sam)))
      ?-  -.top
        $|  (flaw cof p.top)
        $&  (fine cof p.top)
      ==
    ::
    ++  maul                                            ::  slam
      ~/  %maul
      |=  {cof/cafe gat/vase sam/vase}
      ^-  (bolt vase)
      %+  cope  (malt cof p.gat p.sam)
      |=  {cof/cafe typ/span}
      %+  (coup cof)  (mong [q.gat q.sam] (sloy syve))
      |=(val/* `vase`[typ val])
    ::
    ++  meow                                            ::  assemble
      :: =+  dyv=0
      |=  {how/beam arg/coin}
      =|  $:  rop/(map term (pair hoof twig))           ::  structures
              bil/(map term (pair hoof twig))           ::  libraries
              boy/(list twig)                           ::  body stack
              lit/?                                     ::  drop arguments
          ==
      ~%  %meow  ..meow  ~
      |%
      ++  able                                          ::  assemble preamble
        ^-  twig
        :+  %per
          ?:(=(~ rop) [%$ 1] [%core (~(run by rop) |=({^ a/twig} [%ash a]))])
        ?:(=(~ bil) [%$ 1] [%core (~(run by bil) |=({^ a/twig} [%ash a]))])
      ::
      ++  abut                                          ::  generate
        |=  {cof/cafe hyd/hood}
        ^-  (bolt vase)
        %+  cope  (apex cof hyd)
        |=  {cof/cafe sel/_..abut}
        =.  ..abut  sel
        %+  cope  (maim cof pit able)
        |=  {cof/cafe bax/vase}
        %+  cope  (chap cof bax [%fan fan.hyd])
        |=  {cof/cafe mar/mark gox/vase}
        %+  cope  (maim cof (slop gox bax) [%tow (flop boy)])
        |=  {cof/cafe fin/vase}
        (fine cof fin)
        ::  ~>  %slog.[0 ~(duck ut p.q.cay)]
      ::
      ++  apex                                          ::  build to body
        |=  {cof/cafe hyd/hood}
        ^-  (bolt _..apex)
        %+  cope  (body cof src.hyd)
        ::=.  dyv  +(dyv)
        ::~&  [`term`(cat 3 %apex (fil 4 dyv '  ')) `path`(flop s.how) libs]
        ::=-  ~&  [`term`(cat 3 %xepa (fil 4 dyv '  ')) `path`(flop s.how)]  -
        |=  {cof/cafe sel/_..apex}
        =.  ..apex  sel
        %+  cope  (neck cof lib.hyd)
        |=  {cof/cafe sel/_..apex}
        =.  ..apex  sel(boy boy)
        %+  cope  (head cof sur.hyd)
        |=  {cof/cafe sel/_..apex}
        (fine cof sel)
      ::
      ++  body                                          ::  produce functions
        |=  {cof/cafe src/(list hoop)}
        ^-  (bolt _..body)
        ?~  src  (fine cof ..body)
        %+  cope  (wilt cof i.src)
        |=  {cof/cafe sel/_..body}
        ^$(src t.src, ..body sel, cof cof)
      ::
      :: ++  libs  `(set term)`(silt (turn (~(tap by bil)) head.is))
      ++  chad                                          ::  atomic list
        |=  {cof/cafe bax/vase doe/term hon/horn}
        ^-  (bolt vase)
        %+  cope  (lash cof how (flux (slat doe)))
        |=  {cof/cafe yep/(map knot @)}
        =+  ^=  poy  ^-  (list (pair knot @))
            %+  sort  (~(tap by yep) ~)
            |=({{* a/@} {* b/@}} (lth a b))
        %+  cope
          |-  ^-  (bolt (list (pair @ vase)))
          ?~  poy  (flue cof)
          %+  cope  $(poy t.poy)
          |=  {cof/cafe nex/(list (pair @ vase))}
          %+  cope  (chap(s.how [p.i.poy s.how]) cof bax hon)
          (flux |=({mar/mark elt/vase} [[q.i.poy elt] nex]))
        %-  flux
        |=  yal/(list (pair @ vase))  ^-  vase
        ?~  yal  [[%atom %n `~] 0]
        (slop (slop [[%atom doe ~] p.i.yal] q.i.yal) $(yal t.yal))
      ::
      ++  chai                                          ::  atomic map
        |=  {cof/cafe bax/vase hon/horn}
        ^-  (bolt vase)
        %+  cope
          %+  cope  (lads cof how)
          %-  some-in-map
          |=  {cof/cafe dir/knot}
          =+  nod=(chap(s.how [dir s.how]) cof bax hon)
          ?:  ?=($2 -.q.nod)
            (flue cof)
          (cope nod (flux some))
        %-  flux
        |=  doy/(map @ cage)  ^-  vase
        ?~  doy  [[%atom %n `0] 0]
        %+  slop
          (slop [[%atom %ta ~] p.n.doy] q.q.n.doy)
        (slop $(doy l.doy) $(doy r.doy))
      ::
      ++  chap                                          ::  produce resources
        |=  {cof/cafe bax/vase hon/horn}
        ^-  (bolt cage)
        ?-    -.hon
            $ape  (cope (maim cof bax p.hon) (flux |=(a/vase [%noun a])))
            $arg
          %+  cope  (maim cof bax p.hon)
          |=  {cof/cafe gat/vase}
          %+  cope  (maim cof !>(~) ((jock |) arg))
          |=  {cof/cafe val/vase}
          %+  cope  (maul cof gat (slop !>(how) val))
          (flux |=(a/vase noun+a))
        ::
            $alt
          %.  cof
          |=  cof/cafe  ^-  (bolt cage)
          ?~  p.hon  (flaw cof leaf+"ford: out of options" ~)
          (coop ^$(cof cof, hon i.p.hon) ..$(p.hon t.p.hon))
        ::
            $dep
          =+  [dep bot]=(clad $(hon p.hon))    :: XX review
          %+  cope  bot
          %-  flux
          |=  {mark vax/vase}
          [%noun (slop [atom+['uvH' ~] dep] vax)]
        ::
            $dub
          %+  cope  $(hon q.hon)
          %-  flux
          |=  {mar/mark vax/vase}
          [mar [%face p.hon p.vax] q.vax]
        ::
            $fan
          %+  cope
            %+  cope
              |-  ^-  (bolt (list vase))
              ?~  p.hon  (flue cof)
              %+  cope  ^$(cof cof, hon i.p.hon)
              |=  {cof/cafe mar/mark vax/vase}
              %+  cope  ^$(cof cof, p.hon t.p.hon)
              (flux |=(tev/(list vase) [vax tev]))
            |=  {cof/cafe tev/(list vase)}
            %+  fine  cof
            |-  ^-  vase
            ?~  tev  [[%atom %n `~] 0]
            (slop i.tev $(tev t.tev))
          (flux |=(a/vase noun+a))
        ::
            $for
          =+  opt=|.(>(turn p.hon |=({a/path ^} a))<)
          |-  ^-  (bolt cage)
          ?~  p.hon  (flaw cof leaf+"ford: no match" >(en-beam how)< *opt ~)
          ?:  =(p.i.p.hon (scag (lent p.i.p.hon) (flop s.how)))
            ^$(hon q.i.p.hon)
          $(p.hon t.p.hon)
        ::
            $hel  $(hon p.hon, lit |)
            $lin
          %+  cope  $(hon q.hon)
          |=  {cof/cafe cay/cage}  ^-  (bolt cage)
          ?~  p.hon  (fine cof cay)
          %+  cope  $(p.hon t.p.hon)
          |=  {cof/cafe cay/cage}
          (cope (make cof %cast i.p.hon $+cay) flay)
        ::
            $man
          %+  cope
            |-  ^-  (bolt vase)
            ?~  p.hon  (fine cof [[%atom %n `~] 0])
            %+  cope  $(p.hon l.p.hon)
            |=  {cof/cafe lef/vase}
            %+  cope  ^$(cof cof, p.hon r.p.hon)
            |=  {cof/cafe rig/vase}
            %+  cope  ^^^$(cof cof, hon q.n.p.hon)
            |=  {cof/cafe mar/mark vax/vase}
            %+  fine  cof
            %+  slop
              (slop [[%atom %tas ~] p.n.p.hon] vax)
            (slop lef rig)
          (flux |=(a/vase noun+a))
        ::
            $now
          %+  cope  (chad cof bax %da p.hon)
          (flux |=(a/vase noun+a))
        ::
            $nod
          %+  cope  (chad cof bax p.hon q.hon)
          (flux |=(a/vase noun+a))
        ::
            $nap
          %+  cope  (chai cof bax p.hon)
          (flux |=(a/vase noun+a))
        ::
            $saw
          %+  cope  $(hon q.hon)
          |=  {cof/cafe mar/mark sam/vase}
          %+  cope  (maim cof bax p.hon)
          |=  {cof/cafe gat/vase}
          %+  cope  (maul cof gat sam)
          (flux |=(a/vase noun+a))
        ::
            $see
          =+  vez=(vang & (en-beam how))
          =+  tuz=(posh:vez p.hon)
          ?~  tuz  (flaw cof leaf+"bad tusk: {<p.hon>}" ~)
          =+  pax=(plex:vez %conl u.tuz)
          ?~  pax  (flaw cof leaf+"bad path: {<u.tuz>}" ~)
          =+  bem=(de-beam u.pax)
          ?~  bem  (flaw cof leaf+"bad beam: {<u.pax>}" ~)
          $(hon q.hon, how u.bem)
        ::
            $sic
          %+  cope  $(hon q.hon)
          |=  {cof/cafe mar/mark vax/vase}
          %+  cope  (maim cof bax [%bunt p.hon])
          |=  {cof/cafe tug/vase}
          ?.  (~(nest ut p.tug) | p.vax)
            (flaw cof [%leaf "type error: {<p.hon>} {<q.hon>}"]~)
          (fine cof [mar p.tug q.vax])
        ::
            $toy
          ?:  p.hon
            =?  arg  lit  many+~
            (cope (make cof %bake q.hon arg how) flay)
          %+  cool  |.(leaf+"ford: hook {<q.hon>} {<(en-beam how)>}")
          %+  cope  (fade cof how)
          |=  {cof/cafe hyd/hood}
          %+  cope  (abut:(meow how arg) cof hyd)
          ;~(cope (lake | q.hon) (flux |=(a/vase [q.hon a])))
        ==
      ::
      ++  head                                          ::  consume structures
        |=  {cof/cafe bir/(list hoof)}
        ^-  (bolt _..head)
        ?~  bir
          (fine cof ..head)
        =.  boy
          ?:  p.i.bir  boy
          (welp boy [[%use [%limb q.i.bir] [%$ 1]] ~])
        =+  byf=(~(get by rop) q.i.bir)
        ?^  byf
          ?.  =(+:`hoof`i.bir +:`hoof`p.u.byf)
            (flaw cof [%leaf "structure mismatch: {<~[p.u.byf q.i.bir]>}"]~)
          $(bir t.bir)
        %+  cope  (fame cof (hone %sur i.bir))
        |=  {cof/cafe bem/beam}
        %+  cope  (fade cof bem)
        |=  {cof/cafe hyd/hood}
        %+  cope  (apex(how bem, boy ~) cof hyd)
        |=  {cof/cafe sel/_..head}
        =.  ..head
            %=  sel
              boy  boy
              how  how
              rop  %+  ~(put by (~(uni by rop) rop.sel))
                      q.i.bir
                   [i.bir [%tow (flop boy.sel)]]
            ==
        ^^^$(cof cof, bir t.bir)
      ::
      ++  hone                                          ::  plant hoof
        |=  {way/@tas huf/hoof}
        ^-  beam
        ?~  r.huf
          how(s ~[q.huf way])
        [[q.u.r.huf q.how p.u.r.huf] ~[q.huf way]]
      ::
      ++  neck                                          ::  consume libraries
        |=  {cof/cafe bir/(list hoof)}
        ^-  (bolt _..neck)
        ?~  bir  (fine cof ..neck)
        =.  boy
          ?:  p.i.bir  boy
::           ~&  ford+tscm+[q.i.bir boy]
          (welp boy [[%use [%limb q.i.bir] [%$ 1]] ~])
        =+  byf=(~(get by bil) q.i.bir)
        ?^  byf
          ?.  =(+:`hoof`i.bir +:`hoof`p.u.byf)
            (flaw cof [%leaf "library mismatch: {<~[p.u.byf i.bir]>}"]~)
          $(bir t.bir)
        %+  cope  (fame cof (hone %lib i.bir))
        |=  {cof/cafe bem/beam}
        %+  cope  (fade cof bem)
        |=  {cof/cafe hyd/hood}
        %+  cope  (apex(how bem, boy ~) cof hyd)
        |=  {cof/cafe sel/_..neck}
        =.  ..neck
            %=  sel
              how  how
              bil  %+  ~(put by (~(uni by bil) bil.sel))
                     q.i.bir
                   [i.bir [%tow (flop boy.sel)]]
            ==
        ^^^$(cof cof, bir t.bir)
      ::
      ++  wilt                                          ::  process body entry
        |=  {cof/cafe hop/hoop}
        ^-  (bolt _..wilt)
        ?-    -.hop
            $&  (fine cof ..wilt(boy [p.hop boy]))
            $|
          =.  r.p.hop  ?:(?=({$ud $0} r.p.hop) r.how r.p.hop)
          %+  cool  |.(leaf+"ford: wilt {<[(en-beam p.hop)]>}")
          %+  cope  (lend cof p.hop)
          |=  {cof/cafe arc/arch}
          ?:  (~(has by dir.arc) %hoon)
            %+  cope  (fade cof p.hop)
            |=  {cof/cafe hyd/hood}
            %+  cope  (apex(boy ~) cof hyd)
            (flux |=(sel/_..wilt sel(boy [[%tow boy.sel] boy])))
          =+  [all=(lark (slat %tas) arc) sel=..wilt]
          %+  cope
            |-  ^-  (bolt (pair (map term foot) _..wilt))
            ?~  all  (fine cof ~ ..wilt)
            %+  cope  $(all l.all)
            |=  {cof/cafe lef/(map term foot) sel/_..wilt}
            %+  cope  ^$(all r.all, cof cof, sel sel)
            |=  {cof/cafe rig/(map term foot) sel/_..wilt}
            %+  cope
              %=    ^^^^$
                  cof      cof
                  ..wilt   sel(boy ~)
                  s.p.hop  [p.n.all s.p.hop]
              ==
            |=  {cof/cafe sel/_..wilt}
            %+  fine  cof
            [`(map term foot)`[[p.n.all [%ash [%tow boy.sel]]] lef rig] sel]
          |=  {cof/cafe mav/(map term foot) sel/_..wilt}
          ?~  mav
            (flaw cof [%leaf "source missing: {<(en-beam p.hop)>}"]~)
          (fine cof sel(boy [[%core mav] boy]))
        ==
      --
    ::
    ++  pact-hoon                                       ::  .hoon special case
      |=  {a/@t b/(urge:clay cord)}  ^-  @t
      ~|  %lurk-hoon
      =,  format  =,  differ
      (of-wain (lurk (to-wain a) b))
    ::
    ++  pact                                            ::  patch
      |=  {cof/cafe kas/silk kos/silk}
      ^-  (bolt gage)
      %.  [cof kas kos]
      ;~  cope
        ;~  coax
          |=({cof/cafe p/silk q/silk} (cope (make cof p) flay))
          |=({cof/cafe p/silk q/silk} (cope (make cof q) flay))
        ==
        |=  {cof/cafe cay/cage coy/cage}  ^-  (bolt gage)
        %+  cope  (fang cof p.cay)
        |=  {cof/cafe pro/vase}
        ?.  (slab %grad p.pro)
          (flaw cof leaf+"no ++grad" ~)
        =+  gar=(slap pro [%limb %grad])
        ?@  q.gar
          =+  for=((sand %tas) q.gar)
          ?~  for  (flaw cof leaf+"bad mark ++grad" ~)
          (make cof `silk`[%cast p.cay %pact [%cast u.for %$ cay] %$ coy])
        ?.  (slab %form p.gar)
          (flaw cof leaf+"no ++form:grad" ~)
        =+  for=((soft @tas) q:(slap gar [%limb %form]))
        ?~  for
          (flaw cof leaf+"bad ++form:grad" ~)
        ?.  =(u.for p.coy)
          %+  flaw  cof  :_  ~
          =<  leaf+"pact on data with wrong form: {-} {+<} {+>}"
          [(trip p.cay) (trip u.for) (trip p.coy)]
        ?.  (slab %pact p.gar)
          (flaw cof leaf+"no ++pact:grad" ~)
        %+  cope  (keel cof pro [[%& 6]~ q.cay]~)
        |=  {cof/cafe pox/vase}
        %+  cope
          %^  maul  cof
            (slap (slap pox [%limb %grad]) [%limb %pact])
          q.coy
        (flux |=(pat/vase [%& p.cay pat]))
      ==
    ::
    ++  resp
      |=  {{van/vane ren/care:clay bem/beam} rot/riot:clay}
      ^+  ..zo
      ?>  ?=($c van)
      =.  kig  (~(del in kig) +<-.$)
      ?~  rot
        =^  dep  deh.bay  (daze ~ deh.bay)              ::  dependencies?
        amok:(expo [%made dep %| (smyt ren (en-beam bem)) ~])
      =+  (cat 3 van ren)
      exec(keg (~(put by keg) [- bem] r.u.rot))
    ::
    ++  resd                                            ::  take %diff
      |=  {{van/vane ren/care:clay bem/beam} cag/cage}
      ^+  ..zo
      ?>  ?=($g van)
      ?:  |(!?=($x ren) =(-.s.bem p.cag))
        =.  kig  (~(del in kig) +<-.$)
        =.  mow  :_(mow [hen (cancel van ren bem)])
        =+  (cat 3 van ren)
        exec(keg (~(put by keg) [- bem] cag))
      =.  mow
        :_  mow
        :^  hen  %pass  (camp-wire van ren bem)
        [%f %exec our ~ bek %cast ((hard mark) -.s.bem) %$ cag]
      ..zo
    ::
    ++  resm                                            ::  take %made
      |=  {{van/vane ren/care:clay bem/beam} dep/@uvH gag/gage}
      ^+  ..zo
      ?>  ?=($g van)
      =.  kig  (~(del in kig) +<-.$)
      =.  mow  :_(mow [hen (cancel van ren bem)])
      ?:  ?=($| -.gag)
        amok:(expo [%made dep %| leaf+"ford-scry-made-fail" p.gag])
      ?:  ?=($tabl -.gag)
        amok:(expo [%made dep %| leaf+"ford-scry-made-strange" ~])
      =+  (cat 3 van ren)
      exec(keg (~(put by keg) [- bem] p.gag))
    ::
    ++  syve
      ^-  sley
      |=  {ref/* sec/(unit (set monk)) tem/term bem/beam}
      ^-  (unit (unit cage))
      ?>  =(%151 -.ref)
      %-  %-  lift  |=  (unit cage)                     :: ignore block
          %+  biff  +<
          |=  cay/cage  ^-  (unit cage)
          ?.  -:(nets:wa +.ref `span`p.q.cay)           :: error if bad type
            ~&  :^  %ford-syve-lost  `path`[tem (en-beam bem)]
                  want=;;(span +.ref)
                have=p.q.cay
            ~
          `cay
      ^-  (unit (unit cage))
      =+  (~(get by keg) tem bem)
      ?^  -
        (some -)
      (ska +<.$)
    --
  --
::
--
.  ==
=|  axle
=*  lex  -
|=  {now/@da eny/@ ski/sley}                            ::  activate
^?                                                      ::  opaque core
~%  %ford-d  ..is  ~
|%                                                      ::
++  call                                                ::  request
  |=  {hen/duct hic/(hypo (hobo task:able))}
  ^+  [p=*(list move) q=..^$]
  =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ((hard task:able) p.q.hic)))
  ?:  ?=($wegh -.q.hic)
    :_  ..^$  :_  ~
    :^  hen  %give  %mass
    :-  %ford
    :-  %|
    %-  |=  a/(list (list mass))  ^-  (list mass)       :: XX single-home
        =+  a2=a
        ?~  a  !!
        ?~  i.a  ~
        :_  $(a (turn a2 tail))
        :-  p.i.i.a
        ?~  -.q.i.i.a
          [%& (turn (turn a2 head) |=(b/mass ?~(-.q.b p.q.b !!)))]
        [%| $(a (turn (turn a2 head) |=(b/mass ?~(-.q.b !! p.q.b))))]
    %+  turn  (~(tap by pol))
    |=  {@ baby}
    :~  =<  cache+[%| (turn `(list term)`/hood/bake/slit/slim/slap/slam .)]
        =-  |=(a/term [a %& (~(get ja dep) a)])
        =<  `dep/(jar term *)`(~(rep by jav) .)
        |=({{* a/{term *}} b/(jar term *)} (~(add ja b) -.a +.a))
    ::
        =<  depends+[%| (turn `(list term)`/init/sent/done .)]
        =-  |=(a/term [a %& (~(get ja dep) a)])
        =<  `dep/(jar term *)`(~(rep by deh) .)
        |=({{@ a/{term *}} b/(jar term *)} (~(add ja b) -.a +.a))
    ::
        tasks+[%& dym tad]
    ==
  =+  our=p.q.hic
  =+  ^=  bay  ^-  baby
      =+  buy=(~(get by pol.lex) our)
      ?~(buy *baby u.buy)
  =^  mos  bay
    ?-    -.q.hic
        $wipe  ~&(%ford-cache-wiped [~ bay(jav ~)])
        $wasp
      abet:(~(awap za [our hen [now eny ski] ~] bay) q.q.hic)
        $exec
      ?~  q.q.hic
        abet:~(apax za [our hen [now eny ski] ~] bay)
      abet:(~(apex za [our hen [now eny ski] ~] bay) u.q.q.hic)
    ==
  [mos ..^$(pol (~(put by pol) our bay))]
::
++  doze
  |=  {now/@da hen/duct}
  ^-  (unit @da)
  ~
::
++  load                                                ::  highly forgiving
  :: |=(old/axle ..^$(+>- old))
  ::=.  old
  ::    ?.  ?=([%0 *] old)  old                           ::  remove at 1
  ::    :-  %1
  ::    |-  ^-  *
  ::    ?~  +.old  ~
  ::    ?>  ?=([n=[p=* q=[tad=* dym=* deh=* jav=*]] l=* r=*] +.old)
  ::    :-  [p.n.+.old [tad.q.n.+.old dym.q.n.+.old deh.q.n.+.old ~]]
  ::    [$(+.old l.+.old) $(+.old r.+.old)]
  |=  old/*
  =+  lox=((soft axle) old)
  ^+  ..^$
  ?~  lox
   ~&  %ford-reset
   ..^$
  ..^$(+>- u.lox)
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  [~ ~]
::
++  stay                                                ::  save w+o cache
  `axle`+>-.$(pol (~(run by pol) |=(a/baby [tad.a dym.a deh.a ~])))
::
++  take                                                ::  response
  |=  {tea/wire hen/duct hin/(hypo sign)}
  ^+  [p=*(list move) q=..^$]
  ?>  ?=({@ @ *} tea)
  =+  our=(slav %p i.tea)
  =+  bay=(~(got by pol.lex) our)
  =^  mos  bay
    =+  dep=(slaw %uv i.t.tea)
    ?^  dep
      =+  bem=(need (de-beam t.t.tea))
      abet:(~(axun za [our hen [now eny ski] ~] bay) tea u.dep bem q.hin)
    ?>  ?=({@ @ ^} t.t.tea)
    =+  :*  num=(slav %ud i.t.tea)
            van=((hard vane) i.t.t.tea)
            ren=((hard care:clay) i.t.t.t.tea)
            bem=(need (de-beam t.t.t.t.tea))
        ==
    abet:(~(axon za [our hen [now eny ski] ~] bay) num [van ren bem] q.hin)
  [mos ..^$(pol (~(put by pol) our bay))]
--
