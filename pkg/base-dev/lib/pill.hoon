::  |pill: helper functions for making pills
::
/-  dice
^?
|%
::
+$  pill
  $%  [%ivory p=(list)]
      $:  %pill
          nam=term
          boot-ova=(list)
          kernel-ova=(list unix-event)
          userspace-ova=(list unix-event)
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
  %+  weld
    %+  turn  dez 
    |=  [dek=desk bas=path] 
    (file-ovum [dek bas exc])
  ?.  prime  ~
  [(prep-ovum (turn dez tail))]~
::
++  brass
  ::  sys: root path to boot system, `/~me/[desk]/now/sys`
  ::  dez: secondary desks and their root paths
  ::  exc: list of desk folders to exclude
  ::
  |=  [sys=path dez=(list [desk path]) prime=? exc=(list spur)]
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
  ::  a pill is a 3-tuple of event-lists: [boot kernel userspace]
  ::
  :+  %pill  %brass
  :+  boot-ova
    :~  (boot-ovum compiler-source arvo-source)
        (file-ovum2 [bas exc])
    ==
  =.  dez  (snoc dez [%base bas])
  %+  weld
    %+  turn  dez 
    |=  [dek=desk bas=path] 
    (file-ovum [dek bas exc])
  ?.  prime  ~
  [(prep-ovum (turn dez tail))]~
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
