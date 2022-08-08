::  |pill: helper functions for making pills
::
/-  dice
/=  clay-raw  /sys/vane/clay
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
::  +fill-flow: construct cache
::
++  fill-flow
  |=  [des=desk =yoki:clay our=ship now=@da]
  ^-  flow:clay
  ::  pocket clay
  ::
  =/  clay-gate  (clay-raw our)
  =/  ruf  *raft:clay-gate
  =/  den  ((de:clay-gate now *roof *duct ruf) our des)
  ::  make flow
  ::
  ~&  [%filling-flow-for des]
  =^  moz  ruf  abet:(park:den & yoki *rang:clay)
  ~&  [%flow-filled-having-size ~(wyt by fad.ruf)]
  fad.ruf
::  +inject-flow: inject cache
::
++  inject-flow
  |=  [des=desk =yoki:clay our=ship now=@da]
  ^-  unix-event
  =/  =flow:clay  (fill-flow des yoki our now)
  [/c/feed %feed flow]
::  +file-yoki: assemble yoki from desk and path
::
::    bas: full path to / directory
::
++  file-yoki
  =/  directories=(list path)
    :~  /app    ::  %gall applications
        /gen    ::  :dojo generators
        /lib    ::  libraries
        /mar    ::  mark definitions
        /sur    ::  structures
        /sys    ::  system files
        /ted    ::  :spider strands
        /web    ::  %eyre web content
        /desk   ::  desk manifest
    ==
  |=  [des=desk bas=path]
  ^-  yoki:clay
  %.  directories
  |=  ::  sal: all spurs to load from
      ::
      sal=(list spur)
  ^-  yoki:clay
  ::
  ::  hav: all user files
  ::
  =;  hav  ~&  user-files+(lent hav)
           =/  =yuki:clay
             :-  *(list tako:clay)
             %-  ~(gas by *(map path (each page lobe:clay)))
             (turn hav |=([=path =page] [path &+page]))
           &+yuki
  =|  hav=(list [path page])
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
::  +file-ovum: userspace filesystem load
::
::    bas: full path to / directory
::
++  file-ovum
  |=  [des=desk bas=path]
  ^-  unix-event
  =-  (yoki-ovum des -)
  (file-yoki des bas)
::
::  +yoki-ovum: direct yoki load
::
++  yoki-ovum
  |=  [des=desk =yoki:clay]
  ^-  unix-event
  [/c/sync [%park des yoki *rang:clay]]
::
::  +file-ovum2: electric boogaloo
::
++  file-ovum2  |=(p=path `unix-event`[//arvo what/(user-files p)])
::
::  +user-files: all userspace hoon files
::
++  user-files
  |=  bas=path
  %.  directories:file-yoki
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
--
