::  |pill: helper functions for making pills
::
|%
::  +module-ova: vane load operations.
::
::    sys: full path to /sys directory
::
++  module-ova
  |=  sys=path
  ^-  (list ovum)
  |^  :~  ::  sys/zuse: standard library
          ::
          (vent %$ /zuse)
          ::  sys/vane/ames: network
          ::
          (vent %a /vane/ames)
          ::  sys/vane/behn: timer
          ::
          (vent %b /vane/behn)
          ::  sys/vane/clay: revision control
          ::
          (vent %c /vane/clay)
          ::  sys/vane/dill: console
          ::
          (vent %d /vane/dill)
          ::  sys/vane/eyre: web
          ::
          (vent %e /vane/eyre)
          ::  sys/vane/ford: build
          ::
          (vent %f /vane/ford)
          ::  sys/vane/gall: applications
          ::
          (vent %g /vane/gall)
          ::  sys/vane/jael: security
          ::
          (vent %j /vane/jael)
      ==
  ++  vent
    |=  [abr=term den=path]
    ^-  ovum
    =/  pax  (weld sys den)
    =/  txt  .^(@ %cx (weld pax /hoon))
    [[%vane den] [%veer abr pax txt]]
  --
::  +file-ovum: userspace filesystem load
::
::     bas: full path to / directory
::
++  file-ovum
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
  %.  [/app /gen /lib /mar /ren /sec /sur /sys /tests /web ~]
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
