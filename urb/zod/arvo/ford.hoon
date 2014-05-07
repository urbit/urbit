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
      sik=silk                                          ::  design
  ==                                                    ::
++  task                                                ::  problem in progress
  $:  wor=writ                                          ::  rights and powers
      nah=duct                                          ::  cause
      bek=beak                                          ::  context
      sik=silk                                          ::  problem
      kig=[p=@ud q=(map ,@ud path)]                     ::  blocks
  ==                                                    ::
--                                                      ::
|%                                                      ::
++  colt                                                ::  reduce to save
  |=  lex=axle                                          :: 
  lex
::
++  fine  |*(a=* [%0 p=*(set beam) q=a])
++  flew  |=(a=(set ,[p=beam q=(list tank)]) a)
++  flaw  |=(a=(list tank) [%2 p=a])
::
++  grim                                                ::  merge sets
  |*  [one=(set) two=(set)]
  ^+  one
  (~(gas in one) (~(tap in two) ~))                     ::  XX ugh
::
++  tome                                                ::  parse path
  |=  pax=path
  ^-  (unit beam)
  ?.  ?=([* * * *] pax)  ~
  %+  bind  (slaw %p i.pax)
  |=  who=ship
  %+  bind  (slaw %tas i.t.pax)
  |=  dex=desk
  %+  bind  (slay i.t.t.pax)
  |=  cis=dime
  ?.  ?=(case cis)  ~
  [who dex cis (flop t.t.t.pax)]
::
++  tope                                                ::  beam to path
  |=  bem=beam
  ^-  path
  [(scot %p p.bem) q.bem (scot r.bem) (flop s.bem)]
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
    |-  ^+  +
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
    ?+    -.fav  +
        %exec
      =+  num=p.tad
      ?>  !(~(has by dym) hen)
      =:  p.tad  +(p.tad)
          dym    (~(put by dym) num hen) 
        ==
      abet:~(exec zo [num `task`[wor hen p.fav q.fav ~]])
    ::
        %kill
      =+  num=(need (~(get by dym) hen))
      =+  tas=(need (~(get by q.tad) num))
      ~(kill zo [num tas])
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
          q.kig  (~(put by q.kig) p.kig pax)
        ==
      %=    $
          mow  :_  mow
        :+  [~ wor]
          ~[/c [%f (scot %ud num) (scot %ud tik) ~] hen]
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
              %1  (flew (grim p.nuf p.hoc))
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
              %0  [%0 (grim p.hoc p.nuf) q.nuf]
      ==   == 
    ::
    ++  coup                                            ::  toon to bolt
      |*  [ton=toon fun=(burg)]
      ?-  -.ton
        %2  [%2 p.ton]
        %0  [%0 ~ (fun p.ton)]
        %1  =-  ?-  faw 
                  &  [%1 (turn p.faw |=(a=beam [a *(list tank)]))] 
                  |  [%2 p.faw]
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
              &  [%& zis p.nex]
              |  nex
            ==
        %2  [%2 p.ton]
      ==
    ::
    ++  exec                                            ::  execute app
      ^+  ..zo
      ?^  q.kig  ..zo
      |-  ^+  ..zo
      =^  bot  +.$  (make sik)
      ?-  -.bot
        %0  amok:+.$(mow [%made %& p.bot])
        %2  amok:+.$(mow [%made %| p.bot])
        %1  =<  abet
            |-  ^+  +.^$
            ?~  p.bot  +.^$
            $(p.bot t.p.bot, +.$ (camp %x i.p.bot))
      == 
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
      ^-  (bolt (unit cage))
      %+  cope  (lend bem) 
      |=  arc=arch
      ?^  q.arc
        (cope (liar bem) (lake for [p.bem q.bem r.bem]))
      ?.  (~(has by r.arc) %hoon)  (fine ~)   
      %+  cope  (krab bem)
      |=  gen=twig
      %+  cope  (maim pit gen) 
      |=  gat=vase 
      %+  cope  (maul gat !>(rem))
      (lake for [p.bem q.bem r.bem])
    ::
    ++  lake                                            ::  check/coerce
      |=  [for=logo bek=beak]
      |=  sam=vase
      ^-  (bolt (unit cage))
      ?:  ?=(?(%gate %core) for)
        (fine ~ for sam)
      %+  cope  (make %bake %gate p.bek q.bek r.bek /ref/[for]/sys)
      |=  gat=vase 
      %+  cope  (lane p.gat [%cnzy %$])
      |=  ref=type
      ?:  (~(nest ut ref) | p.sam)
        (fine ~ for sam)
      %+  cope  (maul gat sam)
      |=  pro=vase
      (fine ~ for pro)
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
      ?:  (~(has by r.arc) for)
        (lace for rem bem(s [for s.bem]))
      =+  ^=  wuy
          %^  lion  for 
            [p.bem q.bem r.bem]
          (turn (~(tap by r.arc ~) |=([a=@tas b=~] a)))
      ?~  wuy  [%0 ~ ~]
      %+  cope  `(bolt (list ,@tas))`u.wuy  
      |=  way=(list ,@tas)
      =+  yaw=(flop way)
      ?>  ?=(^ yaw)
      %+  cope  (make %bake i.yaw bem)
      |=  huc=(unit cage)
      ?~  huc  (fine ~)
      %+  cope  (lope i.yaw t.yaw [p.bem q.bem r.bem] q.hoc)
      |=  vax=vase
      (fine ~ vax)
    ::
    ++  lime                                            ::  load beam
      |=  [for=logo bem=beam]
      =+  mob=bem
      |-  ^-  (bolt vase)
      =+  buw=(lima for bem)
      ?^  buw  (fine u.buw)
      ?~  s.bem
        [%2 (smyt (tope mob)) ~]
      $(s.bem t.s.bem)
    ::
    ++  link                                            ::  translate 
      |=  [too=logo for=logo bek=beak vax=vase]
      ^-  (bolt vase)
      ?:  &(=(%hoot too) =(%hoon for))
        (fine !>(ream))
      %+  cope  (make %bake %gate p.bek q.bek r.bek /[too]/tan/[for]/sys)
      |=  gat=vase 
      (maul gat vax)
    ::
    ++  lion                                            ::  translation graph
      |=  [too=@tas bek=beak fro=(list ,@tas)]
      ^-  (unit (bolt (list ,@tas)))
      =|  war=(set ,@tas)
      =<  -:(apex [%0 ~ fro])
      |%
      ++  apex
        |=  rof=(bolt (list ,@tas))
        ^-  [(unit (bolt (list ,@tas))) _+>]
        ?.  ?=(%0 -.rof)
          [[~ rof] +>.$]
        ?~  q.rof
          [~ +>]
        =^  orf  +>  (apse i.q.rof)
        ?^(orf [orf +>.$] $(q.rof t.q.rof))
      ::
      ++  apse
        |=  for=@tas
        ^-  [(unit (bolt (list ,@tas))) _+>]
        ?:  =(for too)  [[~ [too ~]] +>]
        ?:  (~(has in war) for)  [~ +>]
        =.  war  (~(put in war) for)
        =^  orf  +>.$  (apex (lily for bek))
        :_  +>.$
        ?~(orf ~ [~ [for u.orf]])
      --
    ::
    ++  lope
      |=  [for=logo yaw=(list logo) bek=beak vax=vase]
      ^-  (bolt vase)
      ?~  yaw  [%0 ~ vax]
      %+  cope  (link i.yaw for bek vax)
      |=  yed=vase
      $(yaw t.yaw, vax yed)
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
          %call
        %.  [p.kas q.kas]
        ;~  cope
          ;~  coax
            |=([p=silk q=silk] ^$(kas p))
            |=([p=silk q=silk] ^$(kas q))
          ==
        ::
          |=  [gat=berm sam=berm]
          (maul q.q.gat q.q.sam)
        ::
          |=  vax=vase
          [%0 ~ %noun vax]
        ==
      ::
          %reef  [%0 ~ %noun pit]
          %sign  (cope (lime p.kas q.kas) |=(a=vase [p.kas a]))
      == 
    ::
    ++  maim                                            ::  slap
      |=  [vax=vase gen=twig]
      ^-  (bolt vase)
      =+  puz=(mule |.((~(mint ut p.vax) [%noun gen])))
      ?-  -.puz
        |  [%2 p.puz]
        &  =+  ton=(mock [q.vax q.p.puz] (mole ska))
           ?-  -.ton
             %0  [%0 p.top p.ton]
             %1  [%1 ((list path) p.ton)]
             %2  [%2 p.ton]
      ==   ==
    ::
    ++  maul                                            ::  slam
      |=  [gat=vase sam=vase]
      ^-  (bolt vase)
      =+  top=(mule |.((slit p.gat p.sam)))
      ?-  -.top 
        |  [%2 p.top]
        &  =+  ton=(mong [q.gat q.sam] (mole ska))
           ?-  -.ton
             %0  [%0 `type`p.top p.ton]
             %1  [%1 ((list path) p.ton)]
             %2  [%2 p.ton]
      ==   ==
    ::
    ++  resp
      |=  [tik=@ud rot=riot]
      ^+  ..zo
      ?>  (~(has by q.kig) tik)
      ?~  rot
        amok:+.$(mow [%made %| (smyt q:(need (~(get by q.kig)) tik))])
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
  |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
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
  |=  [our=ship ren=@tas who=ship syd=disc lot=coin tyl=path]
  ^-  (unit)
  ~
::
++  stay
  `vase`!>((colt `axle`+>-.$))
++  vern  [164 0]
--
