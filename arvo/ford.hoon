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
      dym=(map duct ,@ud)                               ::  duct to task number
      jav=(map ,* calx)                                 ::  cache
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
  $%  [%comp p=calm q=cage r=twig]                      ::  compile by text
      [%slap p=calm q=[p=vase q=twig] r=vase]           ::  slap
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
::                                                      ::
++  calf                                                ::  reduce calx
  |*  sem=*                                             ::  a typesystem hack
  |=  cax=calx
  ?+  sem  !!
    %comp  ?>(?=(%comp -.cax) r.cax)
    %slap  ?>(?=(%slap -.cax) r.cax)
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
++  colt                                                ::  reduce to save
  |=  lex=axle                                          ::
  ^-  axle
  %=    lex
      pol
    %-  ~(run by pol.lex)
    |=  bay=baby
    bay(jav ~)
  ==
::
++  fine  |*  [a=cafe b=*]                              ::  bolt from data
          [p=`cafe`a q=[%0 p=*(set beam) q=b]]          ::
++  flaw  |=([a=cafe b=(list tank)] [p=a q=[%2 p=b]])   ::  bolt from error
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
  ++  apex                                              ::  advance
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
    ?>  ?=(%exec -.fav)
    ?~  q.fav
      =+  num=(need (~(get by dym.bay) hen))
      =+  tas=(need (~(get by q.tad.bay) num))
      amok:~(camo zo [num tas])
    =+  num=p.tad.bay
    ?>  !(~(has by dym.bay) hen)
    =:  p.tad.bay  +(p.tad.bay)
        dym.bay    (~(put by dym.bay) hen num)
      ==
    ~(exec zo [num `task`[hen u.q.fav 0 ~]])
  ::
  ++  zo
    |_  [num=@ud task]
    ++  abet  %_(..zo q.tad.bay (~(put by q.tad.bay) num +<+))
    ++  amok  %_(..zo q.tad.bay (~(del by q.tad.bay) num))
    ++  camo                                            ::  stop requests
      ^+  .
      =+  kiz=(~(tap by q.kig) *(list ,[p=@ud q=beam]))
      |-  ^+  +>
      ?~  kiz  +>
      %=    $
          kiz  t.kiz
          mow  :_  mow
        :-  hen
        :^  %toss  %c
          [(scot %ud num) (scot %ud p.i.kiz) ~]
        [%warp [our p.q.i.kiz] q.q.i.kiz ~]
      ==
    ::
    ++  camp                                            ::  request a file
      |=  [ren=care bem=beam]
      ^+  +>
      =+  tik=(scot %ud p.kig)
      =:  p.kig  +(p.kig)
          q.kig  (~(put by q.kig) p.kig bem)
        ==
      %=    $
          mow  :_  mow
        :-  hen
        :^  %toss  %c
          [(scot %ud num) (scot %ud tik) ~]
        [%warp [our p.bem] q.bem [~ %& %x r.bem s.bem]]
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
        ?~  cux
          =+  nuf=(cope hoc fun)
          ?-    -.q.nuf
              %2  nuf
              %1  nuf
              %0
            :-  p=(came p.nuf `calx`[sem `calm`[now p.q.nuf] q.q.hoc q.q.nuf])
            q=q.nuf
          ==
        :-  p=p.hoc
        ^=  q
        :+  %0  p.q.hoc
        ((calf sem) u.cux)
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
    ++  cope                                            ::  bolt along
      |*  [hoc=(bolt) fun=(burg)]
      ?-  -.q.hoc
        %2  hoc
        %1  hoc
        %0  =+  nuf=(fun p.hoc q.q.hoc)
            :-  p=p.nuf
            ^=  q
            ?-  -.q.nuf
              %2  q.nuf
              %1  q.nuf
              %0  [%0 p=(grom `_p.q.nuf`p.q.hoc p.q.nuf) q=q.q.nuf]
      ==   ==
    ::
    ++  coup                                            ::  toon to bolt
      |=  cof=cafe
      |*  [ton=toon fun=$+(* *)]
      :-  p=cof
      ^=  q
      ?-  -.ton
        %2  [%2 p=p.ton]
        %0  [%0 p=*(set beam) q=(fun p.ton)]
        %1  =-  ?-  faw
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
    ++  dash                                            ::  process cache
      |=  cof=cafe
      ^+  +>
      %_(+> jav.bay q.cof)
    ::
    ++  exec                                            ::  execute app
      ^+  ..zo
      ?:  !=(~ q.kig)  ..zo
      |-  ^+  ..zo
      =+  bot=(make [~ jav.bay] kas)
      =.  ..exec  (dash p.bot)
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
      %_(+> mow :_(mow [hen %give fav]))
    ::
    ++  fade                                            ::  compile
      |=  [cof=cafe kas=silk]
      ^-  (bolt twig)
      %+  (clef %comp)  (make cof kas)
      ^-  (burg cage twig)
      |=  [cof=cafe cay=cage]
      ?.  ?=(@ q.q.cay)
        (flaw cof ~)
      =+  vex=((full vest) [[1 1] (trip q.q.cay)])
      ?~  q.vex
        (flaw cof [%leaf "syntax error: {<p.p.vex>} {<q.p.vex>}"] ~)
      (fine cof p.u.q.vex)
    ::
    ++  krab                                            ::  load to twig
      |=  [cof=cafe for=logo how=logo rem=spur bem=beam]
      ^-  (bolt vase)
      %+  cope  (fade cof %bake how bem)
      |=  [cof=cafe gen=twig]
      %+  cope  (maim cof pit gen)
      |=  [cof=cafe gat=vase]
      (maul cof gat !>([`beak`[p.bem q.bem r.bem] for +:s.bem rem]))
    ::
    ++  lace                                            ::  load and check
      |=  [cof=cafe for=logo rem=spur bem=beam]
      ^-  (bolt (unit vase))
      =+  bek=`beak`[p.bem q.bem r.bem]
      %+  cope  (lend cof bem)
      |=  [cof=cafe arc=arch]
      ?^  q.arc
        (cope (liar cof bem) (lake for bek))
      ?:  (~(has by r.arc) %hoon)
        %+  cope  (krab cof for %hoon rem bem)
        (lake for bek)
      ?:  (~(has by r.arc) %hook)
        %+  cope  (krab cof for %hook rem bem)
        |=  [cof=cafe vax=vase]
        %+  cope  ((lair for bem) cof vax)
        |=  [cof=cafe vax=vase]
        (fine cof ~ vax)
      (fine cof ~)
    ::
    ++  lake                                            ::  check/coerce
      |=  [for=logo bek=beak]
      |=  [cof=cafe sam=vase]
      ^-  (bolt (unit vase))
      ?:  ?=(?(%gate %core %hoon %hook) for)
        (fine cof ~ sam)
      %+  cope  (make cof %boil %gate p.bek q.bek r.bek /ref/[for]/sys)
      |=  [cof=cafe cay=cage]
      %+  cope  (lane cof p.q.cay [%cnzy %$])
      |=  [cof=cafe ref=type]
      ?:  (~(nest ut ref) | p.sam)
        (fine cof ~ sam)
      %+  cope  (maul cof q.cay sam)
      |=  [cof=cafe pro=vase]
      (fine cof ~ pro)
    ::
    ++  lair                                            ::  metaload
      |=  [for=logo bem=beam]
      |=  [cof=cafe vax=vase]
      ^-  (bolt vase)
      ?.  (~(nest ut -:!>(*silk)) | p.vax)
        (flaw cof (smyt (tope bem)) ~)
      %+  cope  (make cof ((hard silk) q.vax))
      |=  [cof=cafe cay=cage]
      =+  too=`logo`?@(p.cay p.cay %noun)
      (link cof for too [p.bem q.bem r.bem] q.cay)
    ::
    ++  lane                                            ::  type infer
      |=  [cof=cafe typ=type gen=twig]
      %+  (coup cof)  (mule |.((~(play ut typ) gen)))
      |=(ref=type ref)
    ::
    ++  lend                                            ::  load arch
      |=  [cof=cafe bem=beam]
      ^-  (bolt arch)
      =+  von=(ska %cy (tope bem))
      ?~  von  [p=cof q=[%1 [bem ~] ~ ~]]
      (fine cof ((hard arch) (need u.von)))
    ::
    ++  liar                                            ::  load vase
      |=  [cof=cafe bem=beam]
      ^-  (bolt vase)
      =+  von=(ska %cx (tope bem))
      ?~  von
        [p=*cafe q=[%1 [[bem ~] ~ ~]]]
      ?~  u.von
        (flaw cof (smyt (tope bem)) ~)
      (fine cof ?^(u.u.von [%cell %noun %noun] [%atom %$]) u.u.von)
    ::
    ++  lily                                            ::  translation targets
      |=  [cof=cafe for=logo bek=beak]
      ^-  (bolt (list ,@tas))
      %+  cope
        %+  cope  (lend cof p.bek q.bek r.bek `path`~[%tan for %sys])
        |=  [cof=cafe arc=arch]
        (fine cof (turn (~(tap by r.arc) ~) |=([a=@tas b=~] a)))
      |=  [cof=cafe all=(list ,@tas)]
      (fine cof ?.(=(%hoon for) all [%hoot all]))
    ::
    ++  lima                                            ::  load at depth
      |=  [cof=cafe for=logo rem=spur bem=beam]
      ^-  (bolt (unit vase))
      %+  cope  (lend cof bem)
      |=  [cof=cafe arc=arch]
      ^-  (bolt (unit vase))
      ?:  (~(has by r.arc) for)
        (lace cof for rem bem(s [for s.bem]))
      %+  cope
        %-  lion
        :^    cof
            for
          [p.bem q.bem r.bem]
        (turn (~(tap by r.arc) ~) |=([a=@tas b=~] a))
      |=  [cof=cafe wuy=(unit (list ,@tas))]
      ?~  wuy  (fine cof ~)
      ?>  ?=(^ u.wuy)
      %+  cope  (make cof %bake i.u.wuy bem)
      |=  [cof=cafe hoc=cage]
      %+  cope  (lope cof i.u.wuy t.u.wuy [p.bem q.bem r.bem] q.hoc)
      |=  [cof=cafe vax=vase]
      (fine cof ~ vax)
    ::
    ++  lime                                            ::  load beam
      |=  [cof=cafe for=logo bem=beam]
      =+  [mob=bem rem=*path]
      |-  ^-  (bolt vase)
      %+  cope  (lima cof for rem bem)
      |=  [cof=cafe vux=(unit vase)]
      ?^  vux  (fine cof u.vux)
      ?~  s.bem
        (flaw cof (smyt (tope mob)) ~)
      ^$(s.bem t.s.bem, rem [i.s.bem rem])
    ::
    ++  link                                            ::  translate
      |=  [cof=cafe too=logo for=logo bek=beak vax=vase]
      ^-  (bolt vase)
      ?:  =(too for)  (fine cof vax)
      ?:  &(=(%hoot too) =(%hoon for))
        (fine cof !>(ream))
      %+  cope  (make cof %boil %gate p.bek q.bek r.bek /[too]/tan/[for]/sys)
      |=  [cof=cafe cay=cage]
      (maul cof q.cay vax)
    ::
    ++  lion                                            ::  translation search
      |=  [cof=cafe too=@tas bek=beak fro=(list ,@tas)]
      ^-  (bolt (unit (list ,@tas)))
      =|  war=(set ,@tas)
      =<  -:(apex (fine cof fro))
      |%                                                ::  XX improve monads
      ++  apex
        |=  rof=(bolt (list ,@tas))
        ^-  [(bolt (unit (list ,@tas))) _+>]
        ?.  ?=(%0 -.q.rof)  [rof +>.$]
        ?~  q.q.rof
          [[p.rof [%0 p.q.rof ~]] +>.$]
        =^  orf  +>.$  (apse cof i.q.q.rof)
        ?.  ?=(%0 -.q.orf)
          [orf +>.$]
        ?~  q.q.orf
          $(cof p.orf, q.q.rof t.q.q.rof)
        [[p.orf [%0 (grom p.q.rof p.q.orf) q.q.orf]] +>.$]
      ::
      ++  apse
        |=  [cof=cafe for=@tas]
        ^-  [(bolt (unit (list ,@tas))) _+>]
        ?:  =(for too)
          [(fine cof [~ too ~]) +>.$]
        ?:  (~(has in war) for)  [(fine cof ~) +>]
        =.  war  (~(put in war) for)
        =^  hoc  +>.$  (apex (lily cof for bek))
        :_  +>.$
        %+  cope  hoc
        |=  [cof=cafe ked=(unit (list ,@tas))]
        (fine cof ?~(ked ~ [~ for u.ked]))
      --
    ::
    ++  lope                                            ::  translation pipe
      |=  [cof=cafe for=logo yaw=(list logo) bek=beak vax=vase]
      ^-  (bolt vase)
      ?~  yaw  (fine cof vax)
      %+  cope  (link cof i.yaw for bek vax)
      |=  [cof=cafe yed=vase]
      ^$(cof cof, yaw t.yaw, vax yed)
    ::
    ++  make                                            ::  reduce silk
      |=  [cof=cafe kas=silk]
      ^-  (bolt cage)
      ?-    -.kas
          ^
        %.  [cof p.kas q.kas]
        ;~  cope
          ;~  coax
            |=([cof=cafe p=silk q=silk] ^$(cof cof, kas p.kas))
            |=([cof=cafe p=silk q=silk] ^$(cof cof, kas q.kas))
          ==
        ::
          |=  [cof=cafe bor=cage heg=cage]  ^-  (bolt cage)
          [p=cof q=[%0 ~ [[p.bor p.heg] (slop q.bor q.heg)]]]
        ==
      ::
          %bake
        %+  cope  (lima cof p.kas ~ q.kas)
        |=  [cof=cafe vux=(unit vase)]
        ?~  vux
          (flaw cof (smyt (tope q.kas)) ~)
        (fine cof [p.kas u.vux])
      ::
          %boil
        %+  cope  (lime cof p.kas q.kas)
        |=  [cof=cafe vax=vase]
        (fine cof `cage`[p.kas vax])
      ::
          %call
        %.  [cof p.kas q.kas]
        ;~  cope
          ;~  coax
            |=([cof=cafe p=silk q=silk] ^$(cof cof, kas p))
            |=([cof=cafe p=silk q=silk] ^$(cof cof, kas q))
          ==
        ::
          |=  [cof=cafe gat=cage sam=cage]
          (maul cof q.gat q.sam)
        ::
          |=  [cof=cafe vax=vase]
          (fine cof %noun vax)
        ==
      ::
          %cast
        %+  cope  $(kas r.kas)
        |=  [cof=cafe cay=cage]
        =+  for=`logo`?@(p.cay p.cay %noun)
        %+  cope  (link cof p.kas ?@(p.cay p.cay %noun) q.kas q.cay)
        |=  [cof=cafe vax=vase]
        (fine cof [p.kas vax])
      ::
          %reef  (fine cof %noun pit)
      ==
    ::
    ++  maim                                            ::  slap
      |=  [cof=cafe vax=vase gen=twig]
      ^-  (bolt vase)
      %+  (clef %slap)  (fine cof vax gen)
      |=  [cof=cafe vax=vase gen=twig]
      =+  puz=(mule |.((~(mint ut p.vax) [%noun gen])))
      ?-  -.puz
        |  (flaw cof p.puz)
        &  %+  (coup cof)  (mock [q.vax q.p.puz] (mole ska))
           |=  val=*
           `vase`[p.p.puz val]
      ==
    ::
    ++  maul                                            ::  slam
      |=  [cof=cafe gat=vase sam=vase]
      ^-  (bolt vase)
      =+  top=(mule |.((slit p.gat p.sam)))
      ?-  -.top
        |  (flaw cof p.top)
        &  %+  (coup cof)  (mong [q.gat q.sam] (mole ska))
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
++  take                                                ::  process move
  |=  [tea=wire hen=duct typ=type fav=card]
  ^-  [p=(list move) q=vane]
  ?:  ?=(%soft -.fav)
    $(fav ((hard card) p.fav))
  =+  ^=  our  ^-  @p
      ?+  -.fav  !!
        %exec  p.fav
      ==
  =+  ^=  bay  ^-  baby
      =+  buy=(~(get by pol.lex) our)
      ?~(buy *baby u.buy)
  =^  mos  bay
    abet:~(apex za [[our tea hen fav] [now eny ska] ~] bay)
  [mos ..^$(pol (~(put by pol) our bay))]
::
++  call                                                ::  process move
  |=  [hen=duct typ=type fav=card]
  (take ~ hen typ fav)
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
  |=  [fur=(unit (set monk)) ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit))
  ~
::
++  stay
  `vase`!>((colt `axle`+>-.$))
++  vern  [164 0]
--
