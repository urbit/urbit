::  |pill: helper functions for making pills
::
/-  *bill
/$  weft-to-mime  %kelvin  %mime
/$  bill-to-mime  %bill    %mime
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
::  +file-ovum: userspace filesystem load
::
::    bas: full path to / directory
::
++  file-ovum
  =/  directories=(list path)
    :~  /app    ::  %gall applications
        /gen    ::  :dojo generators
        /lib    ::  libraries
        /mar    ::  mark definitions
        /sur    ::  structures
        /sys    ::  system files
        /ted    ::  :spider strands
        /tests  ::  unit tests
        /web    ::  %eyre web content
        /desk
    ==
  |=  bas=path
  ^-  unix-event
  %.  directories
  |=  ::  sal: all spurs to load from
      ::
      sal=(list spur)
  ^-  unix-event
  ::
  ::  hav: all user files
  ::
  =;  hav  ~&  user-files+(lent hav)
           [/c/sync [%into %$ & hav]]
  =|  hav=mode:clay
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
  =/  pax  (weld bas (flop tyl))
  =/  lon  .^(arch %cy pax)
  ::  XX this serialization should use marks
  ::
  =?  hav  ?=(^ fil.lon)
      ::  XX  this whitelist needs to be reviewed
      ::
      ?.  ?=  $?  %css  %hoon  %html  %js  %json  %md  %png  %txt
                  %udon  %umd  %kelvin  %bill  %woff2
              ==
          -.tyl
        ::
        ::  install only files with whitelisted marks
        ::
        ~&  ignoring+pax
        hav
      ::
      ::  cot: file as plain-text octet-stream
      ::
      =;  cot  [[(flop `path`tyl) `[/text/plain cot]] hav]
      ^-  octs
      ?-    tyl
          [%json *]
        =/  dat  .^(json %cx pax)
        (as-octt:mimes:html (en-json:html dat))
      ::
          [?(%md %txt) *]
        =/  dat  .^(wain %cx pax)
        (as-octs:mimes:html (of-wain:format dat))
      ::
          [%kelvin *]
        =/  weft  ,[lal=@tas num=@ud]  ::  TODO remove after merge
        =/  dat  .^(weft %cx pax)
        q:(weft-to-mime dat)
      ::
          [%bill *]
        =/  dat  .^(bill %cx pax)
        q:(bill-to-mime dat)
      ::
          [%woff2 *]
        =/  dat  .^(octs %cx pax)
        dat
      ::
          *
        =/  dat  .^(@t %cx pax)
        [(met 3 dat) dat]
      ==
  =/  all  ~(tap by dir.lon)
  |-  ^-  mode:clay
  ?~  all  hav
  $(all t.all, hav ^$(tyl [p.i.all tyl]))
::
::  +file-ovum2: electric boogaloo
::
++  file-ovum2  |=(p=path `unix-event`[//arvo what/(user-files p)])
::
::  +user-files: all userspace hoon files
::
++  user-files
  |=  bas=path
  %.  directories:file-ovum
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
