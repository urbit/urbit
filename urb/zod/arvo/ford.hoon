!:::::
::  ::  %ford, new execution control
!?  164
::::
|=  pit=vase
=>  =~
::  structures
|%
::
++  bead  ,[p=(set beam) q=cage]                        ::  computed result
++  gift                                                ::  out result <-$
          $%  [%made p=(each bead (list tank))]         ::  computed result
          ==                                            ::
++  heel  path                                          ::  functional ending
++  hock                                                ::  standard label
          $:  [%c p=@ud]                                ::  celsius version
              [%k p=@ud]                                ::  kelvin version
              [%s p=@ud q=@ud r=@ud]                    ::  semantic version
          ==                                            ::
++  hood                                                ::  assembly plan
          $:  zus=@ud                                   ::  zuse kelvin
              sur=(list hook)                           ::  structures
              lib=(list hoof)                           ::  libraries
              fan=(list horn)                           ::  resources
              src=(list hoop)                           ::  program
          ==                                            ::
++  hoof  (pair term (unit (pair case ship)))           ::  resource reference
++  hook  (pair bean hoof)                              ::  structure gate/core
++  hoop                                                ::  source in hood
          $%  [%& p=twig]                               ::  direct twig
              [%| p=beam]                               ::  resource location   
          ==                                            ::
++  horn                                                ::  resource tree
          $%  [%and p=twig]                             ::  /&  twig by hand
              [%arg p=twig]                             ::  /$  argument
              [%day p=horn]                             ::  /|  @dr map by @dr
              [%dub p=term q=horn]                      ::  /=  apply face
              [%fan p=(list horn)]                      ::  /:  list
              [%for p=path q=horn]                      ::  /,  descend
              [%hub p=horn]                             ::  /@  map by @ud
              [%man p=(map term horn)]                  ::  /*  map by hand
              [%nap p=horn]                             ::  /%  map by @tas
              [%now p=horn]                             ::  /&  map by @da 
              [%saw p=twig q=horn]                      ::  /;  operate on
              [%see p=beam q=horn]                      ::  /.  relative to
              [%sic p=tile q=horn]                      ::  /^  cast
              [%toy p=mark]                             ::  /mark/  static
          ==                                            ::
++  kiss                                                ::  in request ->$
          $%  [%exec p=@p q=(unit silk)]                ::  make / kill
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  note                                                ::  out request $->
          $%  $:  %c                                    ::  to %clay
          $%  [%warp p=sock q=riff]                     ::
          ==  ==  ==                                    ::
++  rave                                                ::  see %clay
          $%  [& p=mood]                                ::  single request
              [| p=moat]                                ::  change range
          ==                                            ::
++  riff  ,[p=desk q=(unit rave)]                       ::  see %clay
++  sign                                                ::  in result $<-
          $%  $:  %c                                    ::  by %clay
          $%  [%writ p=riot]                            ::
          ==  ==  ==                                    ::
++  silk                                                ::  construction layer
          $&  [p=silk q=silk]                           ::  cons
          $%  [%bake p=mark q=beam r=path]              ::  local synthesis
              [%boil p=mark q=beam r=path]              ::  general synthesis
              [%call p=silk q=silk]                     ::  slam
              [%cast p=mark q=silk]                     ::  translate
              [%done p=(set beam) q=cage]               ::  literal
              [%dude p=tank q=silk]                     ::  error wrap
              [%dune p=(set beam) q=(unit cage)]        ::  unit literal
              [%mute p=silk q=(list (pair wing silk))]  ::  mutant
              [%plan p=beam q=spur r=hood]              ::  structured assembly
              [%reef ~]                                 ::  kernel reef
              [%ride p=twig q=silk]                     ::  silk thru twig
              [%vale p=mark q=ship r=*]                 ::  validate [our his]
          ==                                            ::
--                                                      ::
|%                                                      ::  structures
++  axle                                                ::  all %ford state
  $:  %1                                                ::  version for update
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
  $%  [%hood p=calm q=(pair beam cage) r=hood]          ::  compile
      [%slap p=calm q=[p=vase q=twig] r=vase]           ::  compute
  ==                                                    ::
++  task                                                ::  problem in progress
  $:  nah=duct                                          ::  cause
      kas=silk                                          ::  problem
      kig=[p=@ud q=(map ,@ud beam)]                     ::  blocks
  ==                                                    ::
--                                                      ::
|%                                                      ::
++  calf                                                ::  reduce calx
  |*  sem=*                                             ::  a typesystem hack
  |=  cax=calx
  ?+  sem  !!
    %hood  ?>(?=(%hood -.cax) r.cax)
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
++  faun  |=([a=cafe b=vase] (fine a `cage`noun/b))     ::  vase to cage
++  feel  |=([a=cafe b=cage] (fine a q.b))              ::  cage to vase
++  fest  |*([a=cafe b=*] (fine a [~ u=b]))             ::  bolt to unit
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
++  za                                                  ::  per event
  =|  $:  $:  $:  our=ship                              ::  computation owner
                  tea=wire                              ::  event place
                  hen=duct                              ::  event floor
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
  ++  abet                                              ::  resolve
    ^-  [(list move) baby]
    [(flop mow) bay]
  ::
  ++  apex                                              ::  call
    |=  kus=(unit silk)
    ^+  +>
    ?~  kus
      =+  num=(need (~(get by dym.bay) hen))
      =+  tas=(need (~(get by q.tad.bay) num))
      amok:~(camo zo [num tas])
    =+  num=p.tad.bay
    ?>  !(~(has by dym.bay) hen)
    =:  p.tad.bay  +(p.tad.bay)
        dym.bay    (~(put by dym.bay) hen num)
      ==
    ~(exec zo [num `task`[hen u.kus 0 ~]])
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
  ++  zo
    |_  [num=@ud task]
    ++  abet  %_(..zo q.tad.bay (~(put by q.tad.bay) num +<+))
    ++  amok  
      %_  ..zo  
        q.tad.bay  (~(del by q.tad.bay) num)
        dym.bay    (~(del by dym.bay) nah)
      ==
    ++  camo                                            ::  stop requests
      ^+  .
      =+  kiz=(~(tap by q.kig) *(list ,[p=@ud q=beam]))
      |-  ^+  +>
      ?~  kiz  +>
      %=    $
          kiz  t.kiz
          mow  :_  mow
        :-  hen
        :^  %pass  [(scot %p our) (scot %ud num) (scot %ud p.i.kiz) ~]
          %c
        [%warp [our p.q.i.kiz] q.q.i.kiz ~]
      ==
    ::
    ++  camp                                            ::  request a file
      |=  [ren=care bem=beam]
      ^+  +>
      %=    +>
          kig  [+(p.kig) (~(put by q.kig) p.kig bem)]
          mow  :_  mow
        :-  hen
        :^  %pass  [(scot %p our) (scot %ud num) (scot %ud p.kig) ~]
          %c
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
        [p=p.hoc q=[%0 p=p.q.hoc q=((calf sem) u.cux)]]
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
      [p.hoc [%2 *cyt p.q.hoc]]
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
        %1  ~&  [%coup-need ((list path) p.ton)]
            =-  ?-  faw
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
    ++  cowl                                            ::  each to bolt
      |=  cof=cafe
      |*  [tod=(each ,* (list tank)) fun=$+(* *)]
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
    ++  expo                                            ::  return gift
      |=  gef=gift
      %_(+> mow :_(mow [hen %give gef]))
    ::
    ++  fade                                            ::  compile to hood
      |=  [cof=cafe bem=beam]
      ^-  (bolt hood)
      %+  cool  |.(leaf/"ford: fade {<[(tope bem)]>}")
      %+  cope  (make cof [%bake %hoon bem ~])
      |=  [cof=cafe cay=cage]
      %+  (clef %hood)  (fine cof bem(r [%ud 0]) cay)
      ^-  (burg (pair beam cage) hood)
      |=  [cof=cafe bum=beam cay=cage]
      =+  rul=(fair bem)
      ?.  ?=(@ q.q.cay)
        (flaw cof ~)
      =+  vex=((full rul) [[1 1] (trip q.q.cay)])
      ?~  q.vex
        (flaw cof [%leaf "syntax error: {<p.p.vex>} {<q.p.vex>}"] ~)
      (fine cof p.u.q.vex)
    ::
    ++  fang                                            ::  protocol door
      |=  [cof=cafe for=mark bek=beak]
      ^-  (bolt vase)
      =+  pax=/door/[for]/pro
      =+  ^=  bem  ^-  beam
          :_  pax
          ?:  =(p.bek our)  bek
          =+  oak=[our %main %da now]
          ?.  =(~ (ska %cy (tope [oak pax])))  oak
          bek
      (cope (fade cof bem) abut:(meow bem ~))
    ::
    ++  fair                                            ::  hood parsing rule
      |=  bem=beam
      =+  vez=(vang | (tope bem(r [%ud 0])))
      =<  hood
      |%  
      ++  case
        %-  sear  
        :_  nuck:so
        |=  a=coin
        ?.  ?=([%$ ?(%da %ud %tas) *] a)  ~
        [~ u=(^case a)]
      ::
      ++  hath  (cook plex:vez (stag %clsg poor:vez))   ::  hood path
      ++  have  (sear tome hath)                        ::  hood beam
      ++  hood
        %+  ifix  [gay gay]
        ;~  plug
          ;~  pose
            (ifix [;~(plug fas wut gap) gap] dem)
            (easy zuse)
          ==
        ::
          ;~  pose
            (ifix [;~(plug fas hep gap) gap] (most ;~(plug com gaw) hook))
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
      ++  hook
        ;~  pose
          (stag %| ;~(pfix tar hoof))
          (stag %& hoof)
        ==
      ::
      ++  hoop
        ;~  pose
          (stag %| ;~(pfix fas have))
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
              (stag %and ;~(pfix and:sign and:read))
              (stag %arg ;~(pfix arg:sign and:read))
              (stag %day ;~(pfix day:sign day:read))
              (stag %dub ;~(pfix dub:sign dub:read))
              (stag %fan ;~(pfix fan:sign fan:read))
              (stag %for ;~(pfix for:sign for:read))
              (stag %hub ;~(pfix hub:sign day:read))
              (stag %man ;~(pfix man:sign man:read))
              (stag %nap ;~(pfix nap:sign day:read))
              (stag %now ;~(pfix now:sign day:read))
              (stag %see ;~(pfix see:sign see:read))
              (stag %sic ;~(pfix sic:sign sic:read))
            ==
          ==
        ::
        ++  rail
          |*  [wid=_rule tal=_rule]
          ?.  tol  wid
          ;~(pose wid tal)
        ::
        ++  read
          |%  ++  and
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
              ++  see
                %+  rail  
                  ;~(plug (ifix [sel ser] have) apex(tol |))
                ;~(pfix gap ;~(plug have ;~(pfix gap apex)))
          ::
              ++  sic
                %+  rail  
                  ;~(plug (ifix [sel ser] toil:vez) apex(tol |))
                ;~(pfix gap ;~(plug howl:vez ;~(pfix gap apex)))
          --
        ::
        ++  sign
          |%  ++  and  ;~(pose pam (jest %and)) 
              ++  arg  ;~(pose buc (jest %arg))
              ++  day  ;~(pose bar (jest %day))
              ++  dub  ;~(pose tis (jest %dub))
              ++  fan  ;~(pose col (jest %fan))
              ++  for  ;~(pose com (jest %for))
              ++  hub  ;~(pose pat (jest %hub))
              ++  man  ;~(pose tar (jest %man))
              ++  nap  ;~(pose cen (jest %nap))
              ++  now  ;~(pose fas (jest %now))
              ++  saw  ;~(pose sem (jest %saw)) 
              ++  see  ;~(pose dot (jest %see))
              ++  sic  ;~(pose ket (jest %sic))
          -- 
        --
      --
    ::
    ++  kale                                            ::  mutate
      |=  [cof=cafe kas=silk muy=(list (pair wing silk))]
      ^-  (bolt cage)
      %+  cope
        |-  ^-  (bolt (list (pair wing vase)))
        ?~  muy  (fine cof ~)
        %+  cope  (make cof q.i.muy)
        |=  [cof=cafe cay=cage]
        %+  cope  ^$(muy t.muy)
        |=  [cof=cafe rex=(list (pair wing vase))]
        (fine cof [[p.i.muy q.cay] rex])
      |=  [cof=cafe yom=(list (pair wing vase))]
      %+  cope  (make cof kas)
      |=  [cof=cafe cay=cage]
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
      =+  bek=`beak`[p.bem q.bem r.bem]
      %+  cope  (lend cof bem)
      |=  [cof=cafe arc=arch]
      ?^  q.arc
        (cope (cope (liar cof bem) (lake for bek)) fest)
      ?:  (~(has by r.arc) %hoon)
        %+  cope  (fade cof bem)
        |=  [cof=cafe hyd=hood]
        (cope (cope (abut:(meow bem arg) cof hyd) (lake for bek)) fest)
      (fine cof ~)
    ::
    ++  lake                                            ::  check/coerce
      |=  [for=mark bek=beak]
      |=  [cof=cafe sam=vase]
      ^-  (bolt vase)
      %+  cool  |.(leaf/"ford: check {<[for bek `@p`(mug q.sam)]>}")
      ?:  ?=(?(%gate %core %door %hoon %hook %hood) for)
        ::  ~&  [%lake-easy for bek]
        (fine cof sam)
      %+  cope  (fang cof for bek)
      |=  [cof=cafe tux=vase]
      =+  bob=(slot 6 tux)
      ?:  (~(nest ut p.bob) | p.sam)
        (fine cof sam)
      ?.  (slab %grab p.tux)
        (flaw cof [%leaf "ford: no grab: {<[for bek]>}"]~)
      =+  gab=(slap tux [%cnzy %grab])
      ?.  (slab %noun p.gab)
        (flaw cof [%leaf "ford: no noun: {<[for bek]>}"]~)
      %+  cope  (maul cof (slap gab [%cnzy %noun]) [%noun q.sam])
      |=  [cof=cafe pro=vase]
      ?:  =(+<.q.pro q.sam) 
        (fine cof (slot 6 pro))
      (flaw cof [%leaf "ford: invalid content: {<[for bek]>}"]~)
    ::
    ++  lave                                            ::  validate
      |=  [cof=cafe for=mark his=ship som=*]
      ^-  (bolt vase)
      ((lake for [our %main [%da now]]) cof [%noun som])
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
      |=  [cof=cafe for=mark bek=beak]
      ^-  (bolt (list ,@tas))
      %+  cope  (fang cof for bek)
      |=  [cof=cafe vax=vase]
      %+  fine  cof
      %+  weld
        ^-  (list ,@tas)
        ?.  (slab %garb p.vax)  ~
        =+  gav=((soft (list ,@tas)) q:(slap vax [%cnzy %garb]))
        ?~(gav ~ u.gav)
      ?.  (slab %grow p.vax)  ~
      =+  gow=(slap vax [%cnzy %grow])
      (sloe p.gow)
    ::
    ++  lima                                            ::  load at depth
      |=  [cof=cafe for=mark bem=beam arg=heel]
      ^-  (bolt (unit vase))
      %+  cope  (lend cof bem)
      |=  [cof=cafe arc=arch]
      ^-  (bolt (unit vase))
      ?:  (~(has by r.arc) for)
        (lace cof for bem(s [for s.bem]) arg)
      =+  haz=(turn (~(tap by r.arc) ~) |=([a=@tas b=~] a))
      ?~  haz  (fine cof ~)
      %+  cope  (lion cof for -.bem haz)
      |=  [cof=cafe wuy=(unit (list ,@tas))]
      ?~  wuy  (fine cof ~)
      ?>  ?=(^ u.wuy)
      %+  cope  (make cof %bake i.u.wuy bem arg)
      |=  [cof=cafe hoc=cage]
      %+  cope  (lope cof i.u.wuy t.u.wuy -.bem q.hoc)
      |=  [cof=cafe vax=vase]
      (fine cof ~ vax)
    ::
    ++  lime                                            ::  load beam
      |=  [cof=cafe for=mark bem=beam arg=heel]
      =+  [mob=bem mer=(flop arg)]
      |-  ^-  (bolt vase)
      %+  cope  (lima cof for mob (flop mer))
      |=  [cof=cafe vux=(unit vase)]
      ?^  vux  (fine cof u.vux)
      ?~  s.mob
        (flaw cof (smyt (tope bem)) ~)
      ^$(s.mob t.s.mob, mer [i.s.mob mer])
    ::
    ++  link                                            ::  translate
      |=  [cof=cafe too=mark for=mark bek=beak vax=vase]
      ^-  (bolt vase)
      ?:  =(too for)  (fine cof vax)
      ?:  |(=(%noun for) =(%$ for))
        ((lake too bek) cof vax)
      %+  cope  (fang cof for bek)
      |=  [cof=cafe pro=vase]
      ?:  &((slab %grow p.pro) (slab too p:(slap pro [%cnzy %grow])))
        %+  cope  (keel cof pro [[%& 6]~ vax]~)
        |=  [cof=cafe pox=vase]
        (maim cof pox [%tsgr [%cnzy %grow] [%cnzy too]])
      %+  cope  (fang cof too bek)
      |=  [cof=cafe pro=vase]
      =+  ^=  zat  ^-  (unit vase)
          ?.  (slab %grab p.pro)  ~
          =+  gab=(slap pro [%cnzy %grab])
          ?.  (slab for p.gab)  ~
          `(slap gab [%cnzy for])
      ?~  zat
        (flaw cof [%leaf "ford: no link: {<[for too]>}"]~)
      (maul cof u.zat vax)
    ::
    ++  lion                                            ::  translation search
      |=  [cof=cafe too=@tas bek=beak fro=(list ,@tas)]
      ^-  (bolt (unit (list ,@tas)))
      =|  war=(set ,@tas)
      =<  -:(apex (fine cof fro))
      |%
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
      |=  [cof=cafe for=mark yaw=(list mark) bek=beak vax=vase]
      ^-  (bolt vase)
      ?~  yaw  (fine cof vax)
      %+  cope  (link cof i.yaw for bek vax)
      |=  [cof=cafe yed=vase]
      ^$(cof cof, for i.yaw, yaw t.yaw, vax yed)
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
    ++  make                                            ::  reduce silk
      |=  [cof=cafe kas=silk]
      ^-  (bolt cage)
      ::  ~&  [%make -.kas]
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
          [p=cof q=[%0 ~ [%$ (slop q.bor q.heg)]]]
        ==
      ::
          %bake
        %+  cool  |.(leaf/"ford: bake {<p.kas>} {<(tope q.kas)>}")
        %+  cope  (lima cof p.kas q.kas r.kas)
        |=  [cof=cafe vux=(unit vase)]
        ?~  vux
          (flaw cof (smyt (tope q.kas)) ~)
        (fine cof [p.kas u.vux])
      ::
          %boil
        %+  cool  |.(leaf/"ford: boil {<p.kas>} {<(tope q.kas)>} {<r.kas>}")
        %+  cope  (lime cof p.kas q.kas r.kas)
        |=  [cof=cafe vax=vase]
        (fine cof `cage`[p.kas vax])
      ::
          %call
        %+  cool  |.(leaf/"ford: call {<`@p`(mug kas)>}")
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
        %+  cool  |.(leaf/"ford: cast {<p.kas>}")
        %+  cope  $(kas q.kas)
        |=  [cof=cafe cay=cage]
        %+  cope  (link cof p.kas p.cay [our %main %da now] q.cay)
        |=  [cof=cafe vax=vase]
        (fine cof [p.kas vax])
      ::
          %done  [cof %0 p.kas q.kas]
          %dude  (cool |.(p.kas) $(kas q.kas))
          %dune
        ?~  q.kas  [cof [%2 [%leaf "no data"]~]]
        $(kas [%done p.kas u.q.kas])
      ::
          %mute  (kale cof p.kas q.kas)
          %plan  
        %+  cope  (abut:(meow p.kas q.kas) cof r.kas)
        |=  [cof=cafe vax=vase]
        (fine cof %noun vax)
      ::
          %reef  (fine cof %noun pit)
          %ride
        %+  cool  |.(leaf/"ford: ride {<`@p`(mug kas)>}")
        %+  cope  $(kas q.kas)
        |=  [cof=cafe cay=cage]
        %+  cope  (maim cof q.cay p.kas)
        |=  [cof=cafe vax=vase]
        (fine cof %noun vax)
      ::
          %vale  
        %+  cool  |.(leaf/"ford: vale {<p.kas>} {<q.kas>} {<`@p`(mug r.kas)>}")
        %+  cope  (lave cof p.kas q.kas r.kas)
        |=  [cof=cafe vax=vase]
        (fine cof `cage`[p.kas vax])
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
    ++  meow                                            ::  assemble
      |=  [how=beam arg=heel] 
      =|  $:  rop=(map term (pair hoof twig))           ::  structure/complex
              bil=(map term (pair hoof twig))           ::  libraries known
              lot=(list term)                           ::  library stack
              zeg=(set term)                            ::  library guard
              boy=(list twig)                           ::  body stack
          ==
      |%
      ++  able                                          ::  assemble preamble
        ^-  twig
        :+  %tsgr
          ?:(=(~ rop) [%$ 1] [%brcn (~(run by rop) |=([* a=twig] [%ash a]))])
        [%tssg (turn (flop lot) |=(a=term q:(need (~(get by bil) a))))]
      ::
      ++  ably                                          ::  naked structure
        |=  [cof=cafe for=mark hyd=hood]
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
        ^-  (bolt _..body)
        ?~  src  (fine cof ..body)
        %+  cope  (wilt cof i.src)
        |=  [cof=cafe sel=_..body]
        ^$(cof cof, src t.src, ..body sel)
      ::
      ++  chai                                          ::  atomic map
        |=  [cof=cafe bax=vase doe=term hon=horn]
        ^-  (bolt vase)
        %+  cope  ((lash (slat doe)) cof how)
        |=  [cof=cafe yep=(map ,@ span)]
        %+  cope
          |-  ^-  (bolt (map ,@ vase))
          ?~  yep  (fine cof ~)
          %+  cope  $(yep l.yep)
          |=  [cof=cafe lef=(map ,@ vase)]
          %+  cope  `(bolt (map ,@ vase))`^$(cof cof, yep r.yep)
          |=  [cof=cafe rig=(map ,@ vase)]
          %+  cope  (chap(s.how [q.n.yep s.how]) cof bax hon)
          |=  [cof=cafe nod=vase]
          (fine cof [[p.n.yep nod] lef rig])
        |=  [cof=cafe doy=(map ,@ vase)]
        %+  fine  cof
        |-  ^-  vase
        ?~  doy  [[%cube 0 [%atom %n]] 0]
        %+  slop
          (slop [[%atom doe] p.n.doy] q.n.doy)
        (slop $(doy l.doy) $(doy r.doy))
      ::
      ++  chap                                          ::  produce resources
        |=  [cof=cafe bax=vase hon=horn]
        ^-  (bolt vase)
        ?-    -.hon
            %and  (maim cof bax p.hon)
            %arg  
          %+  cope  (maim cof bax p.hon)
          |=  [cof=cafe gat=vase]
          (maul cof gat !>([how arg]))
        ::
            %day  (chai cof bax %dr p.hon)
            %dub 
          %+  cope  $(hon q.hon)
          |=  [cof=cafe vax=vase]
          (fine cof [[%face p.hon p.vax] q.vax])
        ::
            %fan
          %+  cope
            |-  ^-  (bolt (list vase))
            ?~  p.hon  (fine cof ~)
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
            %hub  (chai cof bax %ud p.hon)
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
            %now  (chai cof bax %da p.hon)
            %nap  (chai cof bax %tas p.hon)
        ::
            %see  $(hon q.hon, how p.hon)
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
          (fine cof vax)
        ::
            %toy  (cope (make cof %bake p.hon how ~) feel)
        ==
      ::
      ++  head                                          ::  consume structures
        |=  [cof=cafe bir=(list hook)]
        |-  ^-  (bolt ,_..head)
        ?~  bir  
          (fine cof ..head)
        =+  byf=(~(get by rop) p.q.i.bir)
        ?^  byf
          ?.  =(`hoof`q.i.bir `hoof`p.u.byf)
            (flaw cof [%leaf "structure mismatch: {<~[p.u.byf q.i.bir]>}"]~)
          $(bir t.bir)
        =+  bem=(hone ?:(p.i.bir %gate %core) %sur q.i.bir)
        %+  cope  (fade cof bem)
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
        [[q.u.q.huf %main p.u.q.huf] ~[for p.huf way]]
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
        %+  cope  (fade cof bem)
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
        ^-  (bolt _..wilt)
        ?-    -.hop
            %&  (fine cof ..wilt(boy [p.hop boy]))
            %| 
          %+  cope  (lend cof p.hop)
          |=  [cof=cafe arc=arch]
          ?:  (~(has by r.arc) %hoon)
            %+  cope  (fade cof p.hop)
            |=  [cof=cafe hyd=hood]
            %+  cope  (apex(boy ~) cof hyd)
            |=  [cof=cafe sel=_..wilt]
            (fine cof sel(boy [[%tssg boy.sel] boy]))
          =+  [all=(lark (slat %tas) arc) sel=..wilt]
          %+  cope
            |-  ^-  (bolt (pair (map term foot) _..wilt))
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
|=  [now=@da eny=@ ski=sled]                            ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  call                                                ::  request
  |=  [hen=duct hic=(hypo (hobo kiss))]
  ^-  [p=(list move) q=_..^$]
  =>  .(q.hic ?.(?=(%soft -.q.hic) q.hic ((hard kiss) p.q.hic)))
  =+  ska=(slod ski)
  =+  ^=  our  ^-  @p
      ?-  -.q.hic
        %exec  p.q.hic
      ==
  =+  ^=  bay  ^-  baby
      =+  buy=(~(get by pol.lex) our)
      ?~(buy *baby u.buy)
  =^  mos  bay
    abet:(~(apex za [[our ~ hen] [now eny ska] ~] bay) q.q.hic)
  [mos ..^$(pol (~(put by pol) our bay))]
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load                                                ::  highly forgiving
  |=  old=*
  =.  old  
      ?.  ?=([%0 *] old)  old                           ::  remove at 1
      :-  %1 
      |-  ^-  *
      ?~  +.old  ~
      ?>  ?=([n=[p=* q=[tad=* dym=* jav=*]] l=* r=*] +.old)
      :-  [p.n.+.old [tad.q.n.+.old dym.q.n.+.old ~]]
      [$(+.old l.+.old) $(+.old r.+.old)]
  =+  lox=((soft axle) old)
  ^+  ..^$
  ?~  lox
    ~&  %ford-reset
    ..^$
  ..^$(+>- u.lox)
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  ~
::
++  stay                                                ::  save w/o cache
  `axle`+>-.$(pol (~(run by pol) |=(a=baby [tad.a dym.a ~])))
::
++  take                                                ::  response
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^-  [p=(list move) q=_..^$]
  =+  ska=(slod ski)
  ?>  ?=([@ @ @ ~] tea)
  =+  :*  our=(need (slaw %p i.tea))
          num=(need (slaw %ud i.t.tea))
          tik=(need (slaw %ud i.t.t.tea))
      ==
  =+  bay=(need (~(get by pol.lex) our))
  =^  mos  bay
    abet:(~(axon za [[our tea hen] [now eny ska] ~] bay) num tik q.hin)
  [mos ..^$(pol (~(put by pol) our bay))]
--
