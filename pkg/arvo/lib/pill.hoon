::  |pill: helper functions for making pills
::
^?
|%
::  +module-ova: vane load operations.
::
::    sys: full path to /sys directory
::
+$  pill
  [boot-ova=* kernel-ova=(list unix-event) userspace-ova=(list unix-event)]
::
+$  unix-event
  %+  pair  wire
  $%  [%wack p=@]
      [%whom p=ship]
      [%boot ? $%($>(%fake task:able:jael) $>(%dawn task:able:jael))]
      unix-task
  ==
::
++  module-ova
  |=  sys=path
  ^-  (list [wire [%veer term path cord]])
  %+  turn
    ^-  (list (pair term path))
    :~  ::  sys/zuse: standard library
        ::
        [%$ /zuse]
        ::  sys/vane/ames: network
        ::
        [%a /vane/ames]
        ::  sys/vane/behn: timer
        ::
        [%b /vane/behn]
        ::  sys/vane/clay: revision control
        ::
        [%c /vane/clay]
        ::  sys/vane/dill: console
        ::
        [%d /vane/dill]
        ::  sys/vane/eyre: http server
        ::
        [%e /vane/eyre]
        ::  sys/vane/ford: build
        ::
        [%f /vane/ford]
        ::  sys/vane/gall: applications
        ::
        [%g /vane/gall]
        ::  sys/vane/iris: http client
        ::
        [%i /vane/iris]
        ::  sys/vane/kale: security
        ::
        [%j /vane/jael]
    ==
  |=  [=term =path]
  =/  pax  (weld sys path)
  =/  txt  .^(@ %cx (weld pax /hoon))
  [[%vane path] [%veer term pax txt]]
::  +file-ovum: userspace filesystem load
::
::     bas: full path to / directory
::
++  file-ovum
  =/  directories
    `(list path)`~[/app /ted /gen /lib /mar /ren /sec /sur /sys /tests /web]
  |=  bas=path
  ^-  unix-event
  ::
  ::    /app    %gall applications
  ::    /gen    :dojo generators
  ::    /lib    %ford libraries
  ::    /mar    %ford marks
  ::    /ren    %ford renderers
  ::    /sec    %eyre security drivers
  ::    /sur    %ford structures
  ::    /sys    system files
  ::    /tests  unit tests
  ::    /web    %eyre web content
  ::
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
