::  |pill: helper functions for making pills
::
/-  dice
^?
=<
|%
::
+$  pill
  $%  [%ivory p=(list)]
      $:  %pill
          nam=term
          boot-ova=(list)
          kernel-ova=(list unix-event)
          userspace-ova=(list unix-event)
      ==
      $:  %cash
          $:  nam=term
              boot-ova=(list)
              kernel-ova=(list unix-event)
              userspace-ova=(list unix-event)
          ==
          cache=(list)
  ==  ==
::
+$  unix-event
  %+  pair  wire
  $%  [%wack p=@]
      [%what p=(list (pair path (cask)))]
      [%whom p=ship]
      [%boot ? $%($>(%fake task:jael) $>(%dawn task:jael))]
      [%wyrd p=vere]
      [%verb p=(unit ?)]
      unix-task
  ==
::  +boot-ovum: boostrap kernel filesystem load
::
++  boot-ovum
  |=  [hoon=cord arvo=cord]
  :~  //arvo
      %what
      [/sys/hoon hoon/hoon]
      [/sys/arvo hoon/arvo]
  ==
::  +dirs: filter for userspace directories to include
::
++  dirs
  |=  [bas=path exc=(list spur)]
  ^-  (list spur)
  ?~  exc  ~(tap in dir:.^(arch %cy bas))
  %+  skim
    ~(tap in dir:.^(arch %cy bas))
  |=(=spur =(~ (find ~[spur] exc)))
::  +file-ovum: userspace filesystem load
::
::    bas: full path to / directory
::
++  file-ovum
  |=  [des=desk bas=path exc=(list spur)]
  ^-  unix-event
  %.  (dirs bas exc)
  |=  ::  sal: all spurs to load from
      ::
      sal=(list spur)
  ^-  unix-event
  ::
  ::  hav: all user files
  ::
  =;  hav  ~&  user-files+(lent hav)
           =/  =yuki:clay
             :-  *(list tako:clay)
             %-  ~(gas by *(map path (each page lobe:clay)))
             (turn hav |=([=path =page] [path &+page]))
           [/c/sync [%park des &+yuki *rang:clay]]
  (file-pages bas sal)
::
++  file-pages
  |=  [bas=path sal=(list spur)]
  =|  hav=(list [path page:clay])
  |-  ^+  hav
  ?~  sal  ~
  =.  hav  $(sal t.sal)
  ::
  ::  tyl: spur
  ::
  =/  tyl  i.sal
  |-  ^+  hav
  ::
  ::  pax: full path at `tyl`
  ::  lon: directory at `tyl`
  ::
  =/  lyt  (flop tyl)
  =/  pax  (weld bas lyt)
  =/  lon  .^(arch %cy pax)
  =?  hav  ?=(^ fil.lon)
    :_  hav
    :-  lyt
    ?.  ?=([%azimuth-snapshot *] tyl)
      [mark=;;(@tas (head tyl)) noun=.^(* %cx pax)]
    =;  convert
      mime/(convert .^(snap-state:dice %cx pax))
    .^($-(snap-state:dice mime) %cf (weld bas /azimuth-snapshot/mime))
  =/  all  ~(tap by dir.lon)
  |-  ^+  hav
  ?~  all  hav
  $(all t.all, hav ^$(tyl [p.i.all tyl]))
::
::  +file-ovum2: electric boogaloo
::
++  file-ovum2  |=(z=[path (list spur)] `unix-event`[//arvo what/(user-files z)])
::
++  get-wynn
  |=  [sys=path new-wynn=?]
  ^-  wynn
  ?.  new-wynn
    :~  zuse+zuse
        lull+lull
        arvo+arvo
        hoon+hoon-version
    ==
  ~>  %bout.[0 %get-wynn]
  =+  .^(zus=vase %ca (welp sys /zuse/hoon))
  =+  .^(lul=vase %ca (welp sys /lull/hoon))
  =+  .^(arv=vase %ca (welp sys /arvo/hoon))
  =+  .^(hun=vase %ca (welp sys /hoon/hoon))
  :~  [%zuse ;;(@ud q:(slap zus limb+%zuse))]
      [%lull ;;(@ud q:(slap lul limb+%lull))]
      [%arvo ;;(@ud q:(slap arv limb+%arvo))]
      [%hoon ;;(@ud q:(slap hun limb+%hoon-version))]
  ==
::
++  prep-ovum
  |=  dez=(list path)
  ^-  unix-event
  =-  ~&  clay-blobs+~(wyt by -)
      [/c/inflate [%prep -]]
  %+  roll  dez
  |=  [bas=path out=(map lobe:clay page:clay)]
  %-  ~(uni by out)
  .^(_out %cs (snoc bas %bloc))
::
::  +user-files: all userspace hoon files
::
++  user-files
  |=  [bas=path exc=(list spur)]
  %.  (dirs bas exc)
  |=  sal=(list spur)
  ^-  (list (pair path (cask)))
  ::
  ::  hav: all user files
  ::
  =|  hav=(list (pair path (cask)))
  |-  ^+   hav
  ?~  sal  ~
  =.  hav  $(sal t.sal)
  ::
  ::  tyl: spur
  ::
  =/  tyl  i.sal
  |-  ^+  hav
  ::
  ::  pax: full path at `tyl`
  ::  lon: directory at `tyl`
  ::
  =/  pax  (weld bas (flop tyl))
  =/  lon  .^(arch %cy pax)
  =?  hav  ?=(^ fil.lon)
      ::
      ::  install only hoon files for now
      ::
      ?.  ?=([%hoon *] tyl)
        hav
      :_  hav
      [(flop `path`t.tyl) hoon/.^(@t %cx pax)]
  ::
  =/  all  ~(tap by dir.lon)
  |-  ^+   hav
  ?~  all  hav
  $(all t.all, hav ^$(tyl [p.i.all tyl]))
::
::TODO  include %prep task in solid and brass?
++  solid
  ::  sys: root path to boot system, `/~me/[desk]/now/sys`
  ::  dez: secondary desks and their root paths
  ::  exc: list of desk folders to exclude
  ::
  |=  [sys=path dez=(list [desk path]) dub=? now=@da prime=? exc=(list spur)]
  ^-  pill
  =/  bas=path       (scag 3 sys)
  =/  compiler-path  (weld sys /hoon)
  =/  arvo-path      (weld sys /arvo)
  ~&  %solid-start
  =/  compiler-src  .^(@t %cx (weld compiler-path /hoon))
  =/  arvo-src      .^(@t %cx (weld arvo-path /hoon))
  =/  arvo-formula
    ~&  %solid-loaded
    =/  compiler-hoon  (rain compiler-path compiler-src)
    ?.  dub
      ::  compile arvo against hoon, with our current compiler
      ::
      =/  whole-hoon=hoon
        [%tsgr compiler-hoon [%tsgr [%$ 7] (rain arvo-path arvo-src)]]
      ~&  %solid-parsed
      =/  whole-formula  q:(~(mint ut %noun) %noun whole-hoon)
      ~&  %solid-arvo
      whole-formula
    ::  compile arvo against hoon, with a freshly compiled hoon (via +ride)
    ::
    ~&  %solid-parsed
    =/  compiler-formula  q:(~(mint ut %noun) %noun compiler-hoon)
    ~&  %solid-compiled
    =/  whole-src
      (rap 3 ['=>  ' compiler-src '=>  +7  ' arvo-src ~])
    ~&  %solid-double-loaded
    =/  whole-formula
      =<  +
      .*  [%noun whole-src]
      [%8 compiler-formula [%9 2 %10 [6 %0 3] [%0 2]]]
    ~&  %solid-double-compiled
    whole-formula
  ::
  ~&  [%solid-kernel `@ux`(mug arvo-formula)]
  ::
  ::  installed: Arvo gate (formal interface) with %zuse and vanes installed
  ::
  =/  installed
    =<  q
    %^    spin
        ^-  (list ovum)
        :-  (boot-ovum:pill compiler-src arvo-src)
        %+  turn
          (snoc (turn dez tail) bas)
        |=  =path
        (file-ovum2 [path exc])
      .*(0 arvo-formula)
    |=  [ovo=ovum ken=*]
    [~ (slum ken [now ovo])]
  ::
  ::  kernel-formula
  ::
  ::    We evaluate :arvo-formula (for jet registration),
  ::    then ignore the result and produce .installed
  ::
  =/  kernel-formula
    [%7 arvo-formula %1 installed]
  ::
  ::  boot-two: startup formula
  ::
  =/  boot-two
    =>  *[kernel-formula=^ tale=*]
    !=  [.*(0 kernel-formula) tale]
  ::
  ::  boot-ova
  ::
  =/  boot-ova=(list)
    [aeon:eden:part boot-two kernel-formula ~]
  ::
  ::  a pill is a 3-tuple of event-lists: [boot kernel userspace]
  ::
  ::    Our kernel event-list is ~, as we've already installed them.
  ::    Our userspace event-list is a list containing a full %clay
  ::    filesystem sync event.
  ::
  :+  %pill  %solid
  :+  boot-ova  ~
  =.  dez  (snoc dez [%base bas])
  ;:  weld
    %+  turn  dez
    |=  [dek=desk bas=path]
    (file-ovum [dek bas exc])
  ::
    ?.  prime  ~
    [(prep-ovum (turn dez tail))]~
  ::
    %+  turn  dez
    |=  [dek=desk *]
    ^-  unix-event
    [/c/essential/[dek] %esse dek %.y]
  ==
::
++  brass
  ::  sys: root path to boot system, `/~me/[desk]/now/sys`
  ::  dez: secondary desks and their root paths
  ::  exc: list of desk folders to exclude
  ::
  |=  [sys=path dez=(list [desk path]) cache=? prime=? new-wynn=? exc=(list spur)]
  ^-  pill
  =/  bas=path  (scag 3 sys)
  ::  compiler-source: hoon source file producing compiler, `sys/hoon`
  ::
  =+  compiler-source=.^(@t %cx (welp sys /hoon/hoon))
  ::
  ::  compiler-twig: compiler as hoon expression
  ::
  ~&  %brass-parsing
  =+  compiler-twig=(rain /sys/hoon/hoon compiler-source)
  ~&  %brass-parsed
  ::
  ::  compiler-formula: compiler as nock formula
  ::
  ~&  %brass-compiling
  =+  compiler-formula=q:(~(mint ut %noun) %noun compiler-twig)
  ~&  %brass-compiled
  ::
  ::  arvo-source: hoon source file producing arvo kernel, `sys/arvo`
  ::
  =+  arvo-source=.^(@t %cx (welp sys /arvo/hoon))
  ::
  ::  boot-ova: startup events
  ::
  =/  boot-ova=(list)
    :~  aeon:eden:part
        boot:eden:part
        compiler-formula
        compiler-source
        arvo-source
    ==
  ::
  =/  kernel-ova=(list unix-event)
    :~  (boot-ovum compiler-source arvo-source)
        (file-ovum2 [bas exc])
    ==
  ::
  =/  userspace-ova
    =.  dez  (snoc dez [%base bas])
    ;:  weld
      %+  turn  dez
      |=  [dek=desk bas=path]
      (file-ovum [dek bas exc])
    ::
      ?.  prime  ~
      [(prep-ovum (turn dez tail))]~
    ::
      %+  turn  dez
      |=  [dek=desk *]
      ^-  unix-event
      [/c/essential/[dek] %esse dek %.y]
    ==
  ::
  =/  =wynn  (get-wynn sys new-wynn)
  ::
  =/  cache=(list)
    ?.  cache  ~
    %~  tap  by
    =<  +
    %-  mice
    :_  [%2 [%0 3] %0 2]
    ;:  weld
      boot-ova
      ^-  (list)
      :~  [*@da //arvo %wack *@uvJ]
          [*@da //arvo %whom *@p]
          [*@da //arvo %wyrd [~.nonce /] wynn]
      ==
      `(list)`(turn kernel-ova (lead *@da))
      `(list)`[*@da [/d/term/1 %boot & %fake *@p]]^~
      `(list)`(turn userspace-ova (lead *@da))
      `(list)`[*@da [/c/init-zest %zeal (turn dez |=([d=desk *] [d %live]))]]^~
    ==
  ::
  ~>  %slog.[0 leaf+"brass: cache entries: {<(lent cache)>}"]
  ::
  :-  %cash
  :_  cache
  [%brass boot-ova kernel-ova userspace-ova]
::
++  ivory
  |=  sys=path
  ^-  pill
  =/  lib  (snoc (scag 3 sys) %lib)
  |^  =/  ver
        =/  sub  *(trap vase)
        =.  sub  (build-sys sub %hoon)
        =.  sub  (build-sys sub %arvo)
        =.  sub  (build-sys sub %lull)
        =.  sub  (build-sys sub %zuse)
        =.  sub  (build-lib sub & %ethereum)
        =.  sub  (build-lib sub & %azimuth)
        (build-lib sub | %vere)
      =/  nok  !.
        =>  *[ver=(trap vase) ~]
        !=  q:$:ver
      ivory/[nok ver ~]
  ::
  ++  build-sys
    |=  [sub=(trap vase) nam=term]  ^-  (trap vase)
    ~>  %slog.[0 leaf+"ivory: building /sys/{(trip nam)}"]
    (swat sub (rain /sys/[nam]/hoon .^(@t cx+(welp sys /[nam]/hoon))))
  ::
  ++  build-lib
    |=  [sub=(trap vase) imp=? nam=term]  ^-  (trap vase)
    ~>  %slog.[0 leaf+"ivory: building /lib/{(trip nam)}"]
    =/  hun=hoon
      %+  mist  /lib/[nam]/hoon
      .^(@t cx+(welp lib /[nam]/hoon))
    ?.  imp  (swat sub hun)
    (swel sub [%ktts nam hun])
  ::  +mist: +rain but skipping past ford runes
  ::
  ++  mist
    |=  [bon=path txt=@]
    ^-  hoon
    =+  vas=vast
    ~|  bon
    %+  scan  (trip txt)
    %-  full
    =;  fud
      (ifix [;~(plug gay fud) gay] tall:vas(wer bon))
    %-  star
    ;~  pose  vul
      %+  ifix  [fas (just `@`10)]
      (star ;~(less (just `@`10) next))
    ==
  ::  +swel: +swat but with +slop
  ::
  ++  swel
    |=  [tap=(trap vase) gen=hoon]
    ^-  (trap vase)
    =/  gun  (~(mint ut p:$:tap) %noun gen)
    =>  [tap=tap gun=gun]
    |.  ~+
    =/  pro  q:$:tap
    [[%cell p.gun p:$:tap] [.*(pro q.gun) pro]]
  --
::
++  events
  |%
  +$  prop  [%prop meta tier (list ovum)]
  +$  meta  term
  +$  tier  ?(%fore %hind)  ::  before or after userspace
  ::
  ++  install
    |=  [as=desk =beak pri=?]
    ^-  prop
    :^  %prop  (rap 3 %install '-' as ~)  %hind
    ::TODO  will exclude non-:directories files, such as /changelog/txt
    =-  (murn - same)
    ^-  (list (unit ovum))
    :~  `(file-ovum as (en-beam beak /) ~)
      ::
        ?.  pri  ~
        `(prep-ovum (en-beam beak /) ~)
      ::
        `[/d/install/[as] [%seat as]]
    ==
  --
--
::
~%  %non  ..part  ~
::  +mice: %memo hint caching version of mink
::
|%
++  mice  !.
  ~/  %mice
  |=  [subject=* formula=*]
  =|  trace=(list [@ta *])
  =|  memo=(map)
  =;  [ton=tone mem=_memo]
    ?.  ?=(%0 -.ton)
      [ton ~]
    [ton mem]
  |^  ^-  [tone _memo]
      ?+  formula  [[%2 trace] memo]
          [^ *]
        =^  head  memo  $(formula -.formula)
        ?.  ?=(%0 -.head)  [head memo]
        =^  tail  memo  $(formula +.formula)
        :_  memo
        ?.  ?=(%0 -.tail)  tail
        [%0 product.head product.tail]
      ::
          [%0 axis=@]
        :_  memo
        =/  part  (frag axis.formula subject)
        ?~  part  [%2 trace]
        [%0 u.part]
      ::
          [%1 constant=*]
        [[%0 constant.formula] memo]
      ::
          [%2 subject=* formula=*]
        =^  subject  memo  $(formula subject.formula)
        ?.  ?=(%0 -.subject)  [subject memo]
        =^  formula  memo  $(formula formula.formula)
        ?.  ?=(%0 -.formula)  [formula memo]
        %=  $
          subject  product.subject
          formula  product.formula
        ==
      ::
          [%3 argument=*]
        =^  argument  memo  $(formula argument.formula)
        :_  memo
        ?.  ?=(%0 -.argument)  argument
        [%0 .?(product.argument)]
      ::
          [%4 argument=*]
        =^  argument  memo  $(formula argument.formula)
        :_  memo
        ?.  ?=(%0 -.argument)  argument
        ?^  product.argument  [%2 trace]
        [%0 .+(product.argument)]
      ::
          [%5 a=* b=*]
        =^  a  memo  $(formula a.formula)
        ?.  ?=(%0 -.a)  [a memo]
        =^  b  memo  $(formula b.formula)
        :_  memo
        ?.  ?=(%0 -.b)  b
        [%0 =(product.a product.b)]
      ::
          [%6 test=* yes=* no=*]
        =^  result  memo  $(formula test.formula)
        ?.  ?=(%0 -.result)  [result memo]
        ?+  product.result  [[%2 trace] memo]
          %&  $(formula yes.formula)
          %|  $(formula no.formula)
        ==
      ::
          [%7 subject=* next=*]
        =^  subject  memo  $(formula subject.formula)
        ?.  ?=(%0 -.subject)  [subject memo]
        %=  $
          subject  product.subject
          formula  next.formula
        ==
      ::
          [%8 head=* next=*]
        =^  head  memo  $(formula head.formula)
        ?.  ?=(%0 -.head)  [head memo]
        %=  $
          subject  [product.head subject]
          formula  next.formula
        ==
      ::
          [%9 axis=@ core=*]
        =^  core  memo
          $(formula core.formula)
        ?.  ?=(%0 -.core)  [core memo]
        =/  arm  (frag axis.formula product.core)
        ?~  arm  [[%2 trace] memo]
        %=  $
          subject  product.core
          formula  u.arm
        ==
      ::
          [%10 [axis=@ value=*] target=*]
        ?:  =(0 axis.formula)  [[%2 trace] memo]
        =^  target  memo  $(formula target.formula)
        ?.  ?=(%0 -.target)  [target memo]
        =^  value  memo  $(formula value.formula)
        :_  memo
        ?.  ?=(%0 -.value)  value
        =/  mutant=(unit *)
          (edit axis.formula product.target product.value)
        ?~  mutant  [%2 trace]
        [%0 u.mutant]
      ::
          [%11 tag=@ next=*]
        =^  next  memo  $(formula next.formula)
        :_  memo
        ?.  ?=(%0 -.next)  next
        :-  %0
        .*  subject
        [11 tag.formula 1 product.next]
      ::
          [%11 [tag=@ clue=*] next=*]
        =^  clue  memo  $(formula clue.formula)
        ?.  ?=(%0 -.clue)  [clue memo]
        ?:  ?=(%memo tag.formula)
          ?^  hit=(~(get by memo) subject next.formula)
            [[%0 u.hit] memo]
          =^  ton  memo  $(formula next.formula)
          ?.  ?=(%0 -.ton)
            [ton memo]
          [ton (~(put by memo) [subject next.formula] product.ton)]
        =^  next  memo
          =?    trace
              ?=(?(%hunk %hand %lose %mean %spot) tag.formula)
            [[tag.formula product.clue] trace]
          $(formula next.formula)
        :_  memo
        ?.  ?=(%0 -.next)  next
        :-  %0
        .*  subject
        [11 [tag.formula 1 product.clue] 1 product.next]
      ==
  ::
  ++  frag
    |=  [axis=@ noun=*]
    ^-  (unit)
    ?:  =(0 axis)  ~
    |-  ^-  (unit)
    ?:  =(1 axis)  `noun
    ?@  noun  ~
    =/  pick  (cap axis)
    %=  $
      axis  (mas axis)
      noun  ?-(pick %2 -.noun, %3 +.noun)
    ==
  ::
  ++  edit
    |=  [axis=@ target=* value=*]
    ^-  (unit)
    ?:  =(1 axis)  `value
    ?@  target  ~
    =/  pick  (cap axis)
    =/  mutant
      %=  $
        axis    (mas axis)
        target  ?-(pick %2 -.target, %3 +.target)
      ==
    ?~  mutant  ~
    ?-  pick
      %2  `[u.mutant +.target]
      %3  `[-.target u.mutant]
    ==
  --
--
