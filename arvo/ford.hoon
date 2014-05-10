!:  ::  %ford, new execution control
!?  164
::::
|=  pit=vase
^-  vane
=>  =~
|%                                                      ::  structures
++  axle                                                ::  all %ford state
  $:  ven=%0                                            ::  version for update
      tad=[p=@ud q=(map ,@ud task)]                     ::  tasks by number
      dym=(map duct ,@ud)                               ::  duct to number
      tiz=(map cash twig)                               ::  file hash to twig
  ==                                                    ::
++  bolt                                                ::  gonadic edge
  |*  a=$+(* *)                                         ::  product clam
  $%  [%0 p=(set beam) q=a]                             ::  depends/product
      [%1 p=(set ,[p=beam q=(list tank)])]              ::  blocks
      [%2 p=(list tank)]                                ::  error
  ==                                                    ::
::                                                      ::
++  burg                                                ::  gonadic rule
  |*  [a=$+(* *) b=$+(* *)]                             ::  from and to
  $+(silk (bolt a))                                     ::  
::                                                      ::
++  plan                                                ::  full construction
  $:  hov=@ud                                           ::  hoon version 
      bek=beak                                          ::  load context
      kas=silk                                          ::  design
  ==                                                    ::
++  task                                                ::  problem in progress
  $:  wor=writ                                          ::  rights and powers
      nah=duct                                          ::  cause
      kas=silk                                          ::  problem
      kig=[p=@ud q=(map ,@ud beam)]                     ::  blocks
  ==                                                    ::
--                                                      ::
|%                                                      ::
++  colt                                                ::  reduce to save
  |=  lex=axle                                          :: 
  lex
::
++  fine  |*(a=* [%0 p=*(set beam) q=a])
++  flew  |=(a=(set ,[p=beam q=(list tank)]) [%1 p=a])
++  flaw  |=(a=(list tank) [%2 p=a])
::
++  grim                                                ::  merge sets
  |=  [one=(set beam) two=(set beam)]
  ^-  (set beam)
  (~(gas in one) (~(tap in two) ~))                     ::  XX ugh
::
++  grum                                                ::  merge sets
  |=  [one=(set ,[p=beam q=(list tank)]) two=(set ,[p=beam q=(list tank)])]
  ^-  (set ,[p=beam q=(list tank)])
  (~(gas in one) (~(tap in two) ~))                     ::  XX ugh
::
++  za                                                  ::  per event
  =|  $:  $:  $:  wor=writ                              ::  event authority
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
          axle                                          ::  all vane state
      ==                                                ::
  =*  lex  ->
  |%
  ++  abet 
    ^-  [(list move) axle]
    [(flop mow) lex]
  ::
  ++  apex
    |-  ^+  +.$
    ?^  tea  
      ?>  ?=([@ @ ~] tea)
      =+  num=(need (slaw %ud i.tea))
      ?>  ?=([%writ *] fav)
      =+  tus=(~(get by q.tad) num)
      ?~  tus
        ~&  [%ford-lost num]
        +.$
      (~(resp zo [num u.tus]) (need (slaw %ud i.t.tea)) p.fav)
    ::
    ?+    -.fav  +.$
        %exec
      =+  num=p.tad
      ?>  !(~(has by dym) hen)
      =:  p.tad  +(p.tad)
          dym    (~(put by dym) hen num) 
        == 
      ~(exec zo [num `task`[wor hen p.fav 0 ~]])
    ::
        %kill
      =+  num=(need (~(get by dym) hen))
      =+  tas=(need (~(get by q.tad) num))
      ::  ~(kill zo [num tas])
      !!
    ==
  :: 
  ++  zo
    |_  [num=@ud task]
    ++  abet  %_(..zo q.tad (~(put by q.tad) num +<+))
    ++  amok  %_(..zo q.tad (~(del by q.tad) num))
    ++  camp                                            ::  request a file
      |=  [ren=care bem=beam]
      ^+  +>
      =+  tik=(scot %ud p.kig)
      =:  p.kig  +(p.kig)
          q.kig  (~(put by q.kig) p.kig bem)
        ==
      %=    $
          mow  :_  mow
        :+  [~ wor]
          [/c [%f (scot %ud num) (scot %ud tik) ~] hen]
        [%warp p.bem q.bem [~ %& %x r.bem s.bem]]
      ==
    ::
    ++  coax                                            ::  bolt across
      |*  [hoc=(bolt) fun=(burg)]
      ?-  -.hoc
        %0  =+  nuf=(fun)
            ?-  -.nuf
              %0  [%0 p=(grim p.hoc p.nuf) q=[q.hoc q.nuf]]
              %1  nuf
              %2  nuf
            ==
        %1  =+  nuf=(fun)
            ?-  -.nuf
              %0  hoc
              %1  ~!  p.nuf
                  ~!  p.hoc
                  ~!  *(set ,[p=beam q=(list tank)])
                  (flew `(set ,[p=beam q=(list tank)])`(grum p.nuf p.hoc))
              %2  nuf
            ==
        %2  hoc
      ==
    ::
    ++  cope                                            ::  bolt along
      |*  [hoc=(bolt) fun=(burg)]
      ?-  -.hoc
        %2  hoc
        %1  hoc
        %0  =+  nuf=(fun q.hoc)
            ?-  -.nuf
              %2  nuf
              %1  nuf
              %0  [%0 p=(grim p.hoc p.nuf) q=q.nuf]
      ==   == 
    ::
    ++  coup                                            ::  toon to bolt
      |*  [ton=toon fun=(burg)]
      ?-  -.ton
        %2  [%2 p=p.ton]
        %0  [%0 p=~ q=(fun p.ton)]
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
        %2  [%2 p=p.ton]
      ==
    ::
    ++  exec                                            ::  execute app
      ^+  ..zo
      ?:  !=(~ q.kig)  ..zo
      |-  ^+  ..zo
      =+  bot=(make kas)
      ?-  -.bot
        %0  amok:(expo [%made %& p.bot q.bot])
        %2  amok:(expo [%made %| p.bot])
        %1  =+  zuk=(~(tap by p.bot) ~)
            =<  abet
            |-  ^+  ..exec
            ?~  zuk  ..exec
            =+  foo=`_..exec`(camp %x `beam`p.i.zuk)
            $(zuk t.zuk, ..exec foo)
      == 
    ::
    ++  expo                                            ::  return card
      |=  fav=card
      %_(+> mow :_(mow [[~ wor] hen fav]))
    ::
    ++  krab                                            ::  load hoon to twig
      |=  [for=logo bem=beam]
      ^-  (bolt twig)
      %+  cope  (make %bake for bem)
      |=  cay=cage
      ?.  ?=(@ q.q.cay)  [%2 (smyt (tope bem)) ~]
      =+  vex=((full vest) [[1 1] (trip q.q.cay)])
      ?~  q.vex
        [%2 [%leaf "syntax error: {<p.p.vex>} {<q.p.vex>}"] ~]
      (fine p.u.q.vex)
    ::
    ++  lace                                            ::  load and check
      |=  [for=logo rem=spur bem=beam]
      ^-  (bolt (unit vase))
      %+  cope  (lend bem) 
      |=  arc=arch
      ?^  q.arc
        (cope (liar bem) (lake for [p.bem q.bem r.bem]))
      ?.  (~(has by r.arc) %hoon)  (fine ~)   
      %+  cope  (krab %hoon bem)
      |=  gen=twig
      %+  cope  (maim pit gen) 
      |=  gat=vase 
      %+  cope  (maul gat !>(rem))
      (lake for [p.bem q.bem r.bem])
    ::
    ++  lake                                            ::  check/coerce
      |=  [for=logo bek=beak]
      |=  sam=vase
      ^-  (bolt (unit vase))
      ?:  ?=(?(%gate %core) for)
        (fine ~ sam)
      %+  cope  (make %bake %gate p.bek q.bek r.bek /ref/[for]/sys)
      |=  cay=cage
      %+  cope  (lane p.q.cay [%cnzy %$])
      |=  ref=type
      ?:  (~(nest ut ref) | p.sam)
        (fine ~ sam)
      %+  cope  (maul q.cay sam)
      |=  pro=vase
      (fine ~ pro)
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
      ?~  von  [%1 [bem ~] ~ ~]
      (fine ((hard arch) (need u.von)))
    ::
    ++  liar                                            ::  load vase
      |=  bem=beam
      ^-  (bolt vase) 
      =+  von=(ska %cx (tope bem))
      ?~  von
        [%1 [[bem ~] ~ ~]]
      ?~  u.von
        [%2 (smyt (tope bem)) ~]
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
      =+  yaw=(flop u.wuy)
      ?>  ?=(^ yaw)
      %+  cope  (make %bake i.yaw bem)
      |=  hoc=cage
      %+  cope  (lope i.yaw t.yaw [p.bem q.bem r.bem] q.hoc)
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
        [%2 (smyt (tope mob)) ~]
      $(s.bem t.s.bem, rem [i.s.bem rem])
    ::
    ++  link                                            ::  translate 
      |=  [too=logo for=logo bek=beak vax=vase]
      ^-  (bolt vase)
      ?:  &(=(%hoot too) =(%hoon for))
        (fine !>(ream))
      %+  cope  (make %bake %gate p.bek q.bek r.bek /[too]/tan/[for]/sys)
      |=  cay=cage
      (maul q.cay vax)
    ::
    ++  lion                                            ::  translation search
      |=  [too=@tas bek=beak fro=(list ,@tas)]
      ^-  (bolt (unit (list ,@tas)))
      =|  war=(set ,@tas)
      =<  -:(apex (fine fro))
      |%
      ++  apex
        |=  rof=(bolt (list ,@tas))
        ^-  [(bolt (unit (list ,@tas))) _+>]
        ?.  ?=(%0 -.rof)  [rof +>.$]
        ?~  q.rof  
          [(fine ~) +>.$]
        =^  orf  +>.$  (apse i.q.rof)
        ?.  ?=(%0 -.orf)  [orf +>.$]
        ?~  q.orf  
          $(q.rof t.q.rof)
        [(fine q.orf) +>.$] 
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
        |  [%2 p.puz]
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
        |  [%2 p.top]
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
  =^  mos  lex
    abet:~(apex za [[u.wru tea hen fav] [now eny ska] ~] lex)
  [mos ..^$]
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
    ~&  %eyre-reset
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
