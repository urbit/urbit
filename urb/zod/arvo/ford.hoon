!:  ::  %ford, new execution control
!?  164
::::
|=  pit=vase
^-  vane
=>  =~
|%                                                      ::  structures
++  axle                                                ::  all %ford state
  $:  ven=%0                                            ::  version for update
      pol=(map ship baby)                               ::
  ==                                                    ::
++  baby                                                ::  state by ship
  $:  tad=[p=@ud q=(map ,@ud task)]                     ::  tasks by number
      dym=(map duct ,@ud)                               ::  duct to number
      tiz=(map cash twig)                               ::  file hash to twig
  ==                                                    ::
++  bolt                                                ::  gonadic edge
  |*  a=$+(* *)                                         ::  product clam
  $:  p=cafe                                            ::  cache
    $=  q                                               ::
      $%  [%0 p=(set beam) q=a]                         ::  depends/product
          [%1 p=(set ,[p=beam q=(list tank)])]          ::  blocks
          [%2 p=(list tank)]                            ::  error
      ==                                                ::
  ==                                                    ::
::                                                      ::
++  borg                                                ::  sub-gonadic calx
  |*  [a=$+(* *)]                                       ::  from and to
  $+(calx (bolt a))                                     ::
::                                                      ::
++  burg                                                ::  gonadic rule
  |*  [a=$+(* *) b=$+(* *)]                             ::  from and to
  $+(b (bolt a))                                        ::
::                                                      ::
++  cafe                                                ::  live cache
  $:  p=(set calx)                                      ::  used
      q=(map ,* calx)                                   ::  cache
  ==                                                    ::
::                                                      ::
++  calf                                                ::  abstract cache line
  |*  [a=@tas b=$+(* *) c=$+(* *)]                      ::  key, value
  ,[_a p=b q=@da r=c]                                   ::  key and last used
::                                                      ::
++  calm                                                ::  cache metadata
  $:  laz=@da                                           ::  last accessed
      dep=(set beam)                                    ::  dependencies
  ==                                                    ::
++  calx                                                ::  concrete cache line
  $%  [%comp p=calm q=@t r=twig]                        ::  compile by text
      ::  [%coop p=calm q=@uvI r=twig]                  ::  compile by hash
  ==                                                    ::
::                                                      ::
++  plan                                                ::  full construction
  $:  hov=@ud                                           ::  hoon version
      bek=beak                                          ::  load context
      kas=silk                                          ::  design
  ==                                                    ::
++  task                                                ::  problem in progress
  $:  nah=duct                                          ::  cause
      kas=silk                                          ::  problem
      kig=[p=@ud q=(map ,@ud beam)]                     ::  blocks
  ==                                                    ::
--                                                      ::
|%                                                      ::
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
++  colt                                                ::  reduce to save
  |=  lex=axle                                          ::
  lex
::
++  fine  |*(a=* [p=*cafe q=[%0 p=*(set beam) q=a]])
++  flaw  |=(a=(list tank) [p=*cafe q=[%2 p=a]])
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
++  za                                                  ::  per event
  =|  $:  $:  $:  our=ship                              ::  computation owner
                  tea=wire                              ::  event place
                  hen=duct                              ::  event floor
                  fav=card                              ::  event data
              ==                                        ::
              $:  now=@da                               ::  event date
                  eny=@                                 ::  unique entropy
                  ska=$+(* (unit (unit)))               ::  system namespace
              ==                                        ::
              mow=(list move)                           ::  pending actions
          ==                                            ::
          bay=baby                                      ::  all owned state
      ==                                                ::
  |%
  ++  abet
    ^-  [(list move) baby]
    [(flop mow) bay]
  ::
  ++  apex
    |-  ^+  +.$
    ?^  tea
      ?>  ?=([@ @ ~] tea)
      =+  num=(need (slaw %ud i.tea))
      ?>  ?=([%writ *] fav)
      =+  tus=(~(get by q.tad.bay) num)
      ?~  tus
        ~&  [%ford-lost num]
        +.$
      (~(resp zo [num u.tus]) (need (slaw %ud i.t.tea)) p.fav)
    ::
    ?+    -.fav  +.$
        %exec
      =+  num=p.tad.bay
      ?>  !(~(has by dym.bay) hen)
      =:  p.tad.bay  +(p.tad.bay)
          dym.bay    (~(put by dym.bay) hen num)
        ==
      ~(exec zo [num `task`[hen p.fav 0 ~]])
    ::
        %kill
      =+  num=(need (~(get by dym.bay) hen))
      =+  tas=(need (~(get by q.tad.bay) num))
      ::  ~(kill zo [num tas])
      !!
    ==
  ::
  ++  zo
    |_  [num=@ud task]
    ++  abet  %_(..zo q.tad.bay (~(put by q.tad.bay) num +<+))
    ++  amok  %_(..zo q.tad.bay (~(del by q.tad.bay) num))
    ++  camp                                            ::  request a file
      |=  [ren=care bem=beam]
      ^+  +>
      =+  tik=(scot %ud p.kig)
      =:  p.kig  +(p.kig)
          q.kig  (~(put by q.kig) p.kig bem)
        ==
      %=    $
          mow  :_  mow
        :+  [~ %iron our]
          [/c [%f (scot %ud num) (scot %ud tik) ~] hen]
        [%warp p.bem q.bem [~ %& %x r.bem s.bem]]
      ==
    ::
    ++  clef                                            ::  cache a result
      |*  sem=@tas
      |*  [hoc=(bolt) fun=(burg)]
      ?-    -.q.hoc
          %2  hoc
          %1  hoc
          %0
        =^  cux  p.hoc  ((calk p.hoc) sem q.q.hoc)
        ?~  cux
          =+  nuf=(cope hoc fun)
          ?-    -.q.nuf
              %2  nuf
              %1  nuf
              %0
            :-  p=(came p.nuf `calx`[sem `calm`[now p.q.nuf] q.q.hoc q.q.nuf])
            q=q.q.nuf
          ==
        :-  p=p.hoc
        ^=  q
        :+  %0  p.q.hoc
        ?+    sem  !!
            %comp  ?>(?=(%comp -.u.cux) r.u.cux)
        ==
      ==
    ::
    ++  coax                                            ::  bolt across
      |*  [hoc=(bolt) fun=(burg)]
      ?-  -.q.hoc
        %0  =+  nuf=(fun)
            :-  p=(chub p.hoc p.nuf)
            ^=  q
            ?-  -.q.nuf
              %0  [%0 p=(grom p.q.hoc p.q.nuf) q=[q.q.hoc q.q.nuf]]
              %1  q.nuf
              %2  q.nuf
            ==
        %1  =+  nuf=(fun)
            :-  p=(chub p.hoc p.nuf)
            ^=  q
            ?-  -.q.nuf
              %0  q.hoc
              %1  [%1 p=(grom p.q.nuf p.q.hoc)]
              %2  q.nuf
            ==
        %2  hoc
      ==
    ::
    ++  cope                                            ::  bolt along
      |*  [hoc=(bolt) fun=(burg)]
      ?-  -.q.hoc
        %2  hoc
        %1  hoc
        %0  =+  nuf=(fun q.q.hoc)
            :-  p=(chub p.hoc p.nuf)
            ^=  q
            ?-  -.q.nuf
              %2  q.nuf
              %1  q.nuf
              %0  [%0 p=(grom `_p.q.nuf`p.q.hoc p.q.nuf) q=q.q.nuf]
      ==   ==
    ::
    ++  coup                                            ::  toon to bolt
      |*  [ton=toon fun=(burg)]
      :-  p=*cafe
      ^=  q
      ?-  -.ton
        %2  [%2 p=p.ton]
        %0  [%0 p=~ q=(fun p.ton)]
        %1  =-  :-  p=*cafe
                ^=  q
                ?-  faw
                  &  [%1 p=(turn p.faw |=(a=beam [a *(list tank)]))]
                  |  [%2 p=p.faw]
                ==
            ^=  faw
            |-  ^-  (each (list beam) (list tank))
            ?~  p.ton  [%& ~]
            =+  nex=$(p.ton t.p.ton)
            =+  pax=(path i.p.ton)
            =+  zis=(tome (path i.p.ton))
            ?~  zis
              [%| (smyt pax) ?:(?=(& -.nex) ~ p.nex)]
            ?-  -.nex
              &  [%& u.zis p.nex]
              |  nex
            ==
      ==
    ::
    ++  exec                                            ::  execute app
      ^+  ..zo
      ?:  !=(~ q.kig)  ..zo
      |-  ^+  ..zo
      =+  bot=(make kas)
      ?-  -.q.bot
        %0  amok:(expo [%made %& p.q.bot q.q.bot])
        %2  amok:(expo [%made %| p.q.bot])
        %1  =+  zuk=(~(tap by p.q.bot) ~)
            =<  abet
            |-  ^+  ..exec
            ?~  zuk  ..exec
            =+  foo=`_..exec`(camp %x `beam`p.i.zuk)
            $(zuk t.zuk, ..exec foo)
      ==
    ::
    ++  expo                                            ::  return card
      |=  fav=card
      %_(+> mow :_(mow [[~ %iron our] hen fav]))
    ::
    ++  fade                                            ::  compile
      |=  cay=cage
      ^-  (bolt twig)
      ?.  ?=(@ q.q.cay)
        (flaw (smyt (tope bem)) ~)
      =+  vex=((full vest) [[1 1] (trip q.q.cay)])
      =+  vex=((full vest) [[1 1] (trip src)])
      ?~  q.vex
        (flaw [%leaf "syntax error: {<p.p.vex>} {<q.p.vex>}"] ~)
      (fine p.u.q.vex)
    ::
    ++  krab                                            ::  load to twig
      |=  [for=logo how=logo rem=spur bem=beam]
      ^-  (bolt vase)
      %+  cope  (make %bake how bem)
      |=  cay=cage
      ?.  ?=(@ q.q.cay)
        (flaw (smyt (tope bem)) ~)
      =+  vex=((full vest) [[1 1] (trip q.q.cay)])
      ?~  q.vex
        (flaw [%leaf "syntax error: {<p.p.vex>} {<q.p.vex>}"] ~)
      %+  cope  (maim pit p.u.q.vex)
      |=  gat=vase
      (maul gat !>([`beak`[p.bem q.bem r.bem] for +:s.bem rem]))
    ::
    ++  lace                                            ::  load and check
      |=  [for=logo rem=spur bem=beam]
      ^-  (bolt (unit vase))
      =+  bek=`beak`[p.bem q.bem r.bem]
      %+  cope  (lend bem)
      |=  arc=arch
      ?^  q.arc
        (cope (liar bem) (lake for bek))
      ?:  (~(has by r.arc) %hoon)
        %+  cope  (krab for %hoon rem bem)
        (lake for bek)
      ?:  (~(has by r.arc) %hook)
        %+  cope  (krab for %hook rem bem)
        |=  vax=vase
        %+  cope  ((lair for bem) vax)
        |=  vax=vase
        (fine ~ vax)
      (fine ~)
    ::
    ++  lake                                            ::  check/coerce
      |=  [for=logo bek=beak]
      |=  sam=vase
      ^-  (bolt (unit vase))
      ?:  ?=(?(%gate %core %hoon %hook) for)
        (fine ~ sam)
      %+  cope  (make %boil %gate p.bek q.bek r.bek /ref/[for]/sys)
      |=  cay=cage
      %+  cope  (lane p.q.cay [%cnzy %$])
      |=  ref=type
      ?:  (~(nest ut ref) | p.sam)
        (fine ~ sam)
      %+  cope  (maul q.cay sam)
      |=  pro=vase
      (fine ~ pro)
    ::
    ++  lair                                            ::  metaload
      |=  [for=logo bem=beam]
      |=  vax=vase
      ^-  (bolt vase)
      ?.  (~(nest ut -:!>(*silk)) | p.vax)
        (flaw (smyt (tope bem)) ~)
      %+  cope  (make ((hard silk) q.vax))
      |=  cay=cage
      =+  too=`logo`?@(p.cay p.cay %noun)
      (link for too [p.bem q.bem r.bem] q.cay)
    ::
    ++  lane                                            ::  type infer
      |=  [typ=type gen=twig]
      %+  coup  (mule |.((~(play ut typ) gen)))
      |=(ref=type ref)
    ::
    ++  lend                                            ::  load arch
      |=  bem=beam
      ^-  (bolt arch)
      =+  von=(ska %cy (tope bem))
      ?~  von  [p=*cafe q=[%1 [bem ~] ~ ~]]
      (fine ((hard arch) (need u.von)))
    ::
    ++  liar                                            ::  load vase
      |=  bem=beam
      ^-  (bolt vase)
      =+  von=(ska %cx (tope bem))
      ?~  von
        [p=*cafe q=[%1 [[bem ~] ~ ~]]]
      ?~  u.von
        (flaw (smyt (tope bem)) ~)
      (fine ?^(u.u.von [%cell %noun %noun] [%atom %$]) u.u.von)
    ::
    ++  lily                                            ::  translation targets
      |=  [for=logo bek=beak]
      ^-  (bolt (list ,@tas))
      %+  cope
        %+  cope  (lend p.bek q.bek r.bek `path`~[%tan for %sys])
        |=  arc=arch
        (fine (turn (~(tap by r.arc) ~) |=([a=@tas b=~] a)))
      |=  all=(list ,@tas)
      (fine ?.(=(%hoon for) all [%hoot all]))
    ::
    ++  lima                                            ::  load at depth
      |=  [for=logo rem=spur bem=beam]
      ^-  (bolt (unit vase))
      %+  cope  (lend bem)
      |=  arc=arch
      ^-  (bolt (unit vase))
      ?:  (~(has by r.arc) for)
        (lace for rem bem(s [for s.bem]))
      %+  cope
        %^  lion  for
          [p.bem q.bem r.bem]
        (turn (~(tap by r.arc) ~) |=([a=@tas b=~] a))
      |=  wuy=(unit (list ,@tas))
      ?~  wuy  (fine ~)
      ?>  ?=(^ u.wuy)
      %+  cope  (make %bake i.u.wuy bem)
      |=  hoc=cage
      %+  cope  (lope i.u.wuy t.u.wuy [p.bem q.bem r.bem] q.hoc)
      |=  vax=vase
      (fine ~ vax)
    ::
    ++  lime                                            ::  load beam
      |=  [for=logo bem=beam]
      =+  [mob=bem rem=*path]
      |-  ^-  (bolt vase)
      %+  cope  (lima for rem bem)
      |=  vux=(unit vase)
      ?^  vux  (fine u.vux)
      ?~  s.bem
        (flaw (smyt (tope mob)) ~)
      ^$(s.bem t.s.bem, rem [i.s.bem rem])
    ::
    ++  link                                            ::  translate
      |=  [too=logo for=logo bek=beak vax=vase]
      ^-  (bolt vase)
      ?:  =(too for)  (fine vax)
      ?:  &(=(%hoot too) =(%hoon for))
        (fine !>(ream))
      %+  cope  (make %boil %gate p.bek q.bek r.bek /[too]/tan/[for]/sys)
      |=  cay=cage
      (maul q.cay vax)
    ::
    ++  lion                                            ::  translation search
      |=  [too=@tas bek=beak fro=(list ,@tas)]
      ^-  (bolt (unit (list ,@tas)))
      =|  war=(set ,@tas)
      =<  -:(apex (fine fro))
      |%                                                ::  XX improve monads
      ++  apex
        |=  rof=(bolt (list ,@tas))
        ^-  [(bolt (unit (list ,@tas))) _+>]
        ?.  ?=(%0 -.q.rof)  [rof +>.$]
        ?~  q.q.rof
          [[p.rof [%0 p.q.rof ~]] +>.$]
        =^  orf  +>.$  (apse i.q.q.rof)
        ?.  ?=(%0 -.q.orf)
          [[(chub p.rof p.orf) q.orf] +>.$]
        ?~  q.q.orf
          $(q.q.rof t.q.q.rof)
        [[(chub p.rof p.orf) [%0 (grom p.q.rof p.q.orf) q.q.orf]] +>.$]
      ::
      ++  apse
        |=  for=@tas
        ^-  [(bolt (unit (list ,@tas))) _+>]
        ?:  =(for too)
          [(fine [~ too ~]) +>.$]
        ?:  (~(has in war) for)  [(fine ~) +>]
        =.  war  (~(put in war) for)
        =^  orf  +>.$  (apex (lily for bek))
        :_  +>.$
        %+  cope  orf
        |=  ked=(unit (list ,@tas))
        ?~  ked
          (fine ~)
        (fine ~ [for u.ked])
      --
    ::
    ++  lope
      |=  [for=logo yaw=(list logo) bek=beak vax=vase]
      ^-  (bolt vase)
      ?~  yaw  (fine vax)
      %+  cope  (link i.yaw for bek vax)
      |=  yed=vase
      ^$(yaw t.yaw, vax yed)
    ::
    ++  make                                            ::  reduce silk
      |=  kas=silk
      ^-  (bolt cage)
      ?-    -.kas
          ^
        %.  [p.kas q.kas]
        ;~  cope
          ;~  coax
            |=([p=silk q=silk] ^$(kas p.kas))
            |=([p=silk q=silk] ^$(kas q.kas))
          ==
        ::
          |=  [bor=cage heg=cage]  ^-  (bolt cage)
          :-  *cafe
          :+  %0  ~
          [[p.bor p.heg] (slop q.bor q.heg)]
        ==
      ::
          %bake
        %+  cope  (lima p.kas ~ q.kas)
        |=  vux=(unit vase)
        ?~  vux
          (flaw (smyt (tope q.kas)) ~)
        (fine [p.kas u.vux])
      ::
          %boil  (cope (lime p.kas q.kas) |=(a=vase (fine [p.kas a])))
          %call
        %.  [p.kas q.kas]
        ;~  cope
          ;~  coax
            |=([p=silk q=silk] ^$(kas p))
            |=([p=silk q=silk] ^$(kas q))
          ==
        ::
          |=  [gat=cage sam=cage]
          (maul q.gat q.sam)
        ::
          |=  vax=vase
          (fine %noun vax)
        ==
      ::
          %cast
        %+  cope  $(kas r.kas)
        |=  cay=cage
        =+  for=`logo`?@(p.cay p.cay %noun)
        %+  cope  (link p.kas ?@(p.cay p.cay %noun) q.kas q.cay)
        |=  vax=vase
        (fine [p.kas vax])
      ::
          %reef  (fine %noun pit)
      ==
    ::
    ++  maim                                            ::  slap
      |=  [vax=vase gen=twig]
      ^-  (bolt vase)
      =+  puz=(mule |.((~(mint ut p.vax) [%noun gen])))
      ?-  -.puz
        |  (flaw p.puz)
        &  %+  coup  (mock [q.vax q.p.puz] (mole ska))
           |=  val=*
           `vase`[p.p.puz val]
      ==
    ::
    ++  maul                                            ::  slam
      |=  [gat=vase sam=vase]
      ^-  (bolt vase)
      =+  top=(mule |.((slit p.gat p.sam)))
      ?-  -.top
        |  (flaw p.top)
        &  %+  coup  (mong [q.gat q.sam] (mole ska))
           |=  val=*
           `vase`[p.top val]
      ==
    ::
    ++  resp
      |=  [tik=@ud rot=riot]
      ^+  ..zo
      ?>  (~(has by q.kig) tik)
      ?~  rot
        amok:(expo [%made %| (smyt (tope (need (~(get by q.kig) tik)))) ~])
      exec(q.kig (~(del by q.kig) tik))
    --
  --
--
.  ==
=|  axle
=*  lex  -
|=  [now=@da eny=@ ska=$+(* (unit (unit)))]             ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  beat                                                ::  process move
  |=  [wru=(unit writ) tea=wire hen=duct fav=card]
  ^-  [p=(list move) q=vane]
  ?~  wru  ~|(%beat-whom !!)
  =+  ^=  bay  ^-  baby
      =+  buy=(~(get by pol.lex) q.u.wru)
      ?~(buy *baby u.buy)
  =^  mos  bay
    abet:~(apex za [[q.u.wru tea hen fav] [now eny ska] ~] bay)
  [mos ..^$(pol (~(put by pol) q.u.wru bay))]
::
++  come
  |=  [sam=? old=vase]
  ^-  vane
  (load old)
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load
  |=  old=vase
  ^-  vane
  ?.  (~(nest ut -:!>(`axle`+>-.^$)) | p.old)
    ~&  %ford-reset
    ..^$
  ..^$(+>- (axle q.old))
::
++  raze
  ^-  vane
  ..$(+>- *axle)
::
++  scry
  |=  [our=ship ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit))
  ~
::
++  stay
  `vase`!>((colt `axle`+>-.$))
++  vern  [164 0]
--
