::  |pill: helper functions for making pills
::
^?
|%
::  +module-ova: vane load operations.
::
::    sys: full path to /sys directory
::
++  module-ova
  |=  sys=path
  ^-  (list [wire [%veer term path cord]])
  %+  turn
    ^-  (list (pair term path))
    :~  ::  sys/zuse: standard library
        ::
        [%$ /zuse]
        ::  TODO: I really want these to be named http-client and http-server,
        ::  but this interacts badly with every piece of the system which
        ::  assumes that vane names are one letter, and that one letter is the
        ::  first letter of the file. This assumption is threaded through way
        ::  too many places in the entire system to modify it while doing
        ::  something else.
        ::
        ::  sys/vane/lient: http client
        ::
        [%l /vane/lient]
        ::  sys/vane/rver: http server
        ::
        [%r /vane/rver]
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
        ::  sys/vane/eyre: web
        ::
        ::  [%e /vane/eyre]
        ::  sys/vane/ford: build
        ::
        [%f /vane/ford]
        ::  sys/vane/gall: applications
        ::
        [%g /vane/gall]
        ::  sys/vane/jael: security
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
    `(list path)`~[/app /gen /lib /mar /ren /sec /sur /sys /tests /web]
  |=  bas=path
  ^-  ovum
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
  ^-  ovum
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
      ?.  ?=  ?($css $hoon $js $json $md $txt $udon $umd)
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
