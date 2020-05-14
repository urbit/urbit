::  |pill: helper functions for making pills
::
^?
|%
::
+$  pill
  $:  boot-ova=*
      kernel-ova=(list unix-event)
      userspace-ova=(list unix-event)
  ==
::
+$  unix-event
  %+  pair  wire
  $%  [%wack p=@]
      [%whom p=ship]
      [%boot ? $%($>(%fake task:able:jael) $>(%dawn task:able:jael))]
      unix-task
  ==
::  +module-ova: vane load operations
::
::    sys: full path to /sys directory
::
++  module-ova
  |=  sys=path
  ^-  (list [wire [%veer term path cord]])
  %+  turn
    ^-  (list (pair term path))
    :~  [%$ /zuse]       ::  standard library
        [%a /vane/ames]  ::  network
        [%b /vane/behn]  ::  timer
        [%c /vane/clay]  ::  revision control
        [%d /vane/dill]  ::  console
        [%e /vane/eyre]  ::  http server
        [%g /vane/gall]  ::  applications
        [%i /vane/iris]  ::  http client
        [%j /vane/jael]  ::  identity and security
    ==
  |=  [=term =path]
  =/  pax  (weld sys path)
  =/  txt  .^(@ %cx (weld pax /hoon))
  [[%vane path] [%veer term pax txt]]
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
    ==
  |=  bas=path
  ^-  unix-event
  %.  directories
  |=  ::  sal: all spurs to load from
      ::
      sal/(list spur)
  ^-  unix-event
  ::
  ::  hav: all user files
  ::
  =;  hav  ~&  user-files+(lent hav)
           [[%$ %sync ~] [%into %$ & hav]]
  =|  hav/mode:clay
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
      ?.  ?=  ?($css $hoon $html $js $json $md $png $txt $udon $umd)
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
          {$json *}
        =/  dat  .^(json %cx pax)
        (as-octt:mimes:html (en-json:html dat))
      ::
          {$txt *}
        =/  dat  .^(wain %cx pax)
        (as-octs:mimes:html (of-wain:format dat))
      ::
          *
        =/  dat  .^(@t %cx pax)
        [(met 3 dat) dat]
      ==
  =/  all  ~(tap by dir.lon)
  |-  ^-  mode:clay
  ?~  all  hav
  $(all t.all, hav ^$(tyl [p.i.all tyl]))
--
