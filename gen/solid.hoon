::  Compile arvo as a pill noun, without compiler changes.
::  usage 
::
::    .urbit/pill +solid
::
::::  /hoon/solid/gen
  ::
/?    310
::
::::
  !:
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        arg=$@(~ [top=path ~])
        dub=_|
    ==
:-  %noun
::  sys: root path to boot system, `/~me/[desk]/now/sys`
::
=/  sys=path
  ?^  arg  top.arg
  /(scot %p p.bec)/[q.bec]/(scot %da now)/sys
::
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
      [%tsbn compiler-hoon [%tsbn [%$ 7] (rain arvo-path arvo-src)]]
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
    .*  0
    :+  %7
      compiler-formula
    [%9 2 %10 [6 %1 %noun whole-src] [%0 1]]
  ~&  %solid-double-compiled
  whole-formula
::
~&  [%solid-kernel `@ux`(mug arvo-formula)]
::  module-ova: vane load operations.
::
=/  module-ova=(list ovum)
    |^  :~  (vent %$ /zuse)
            (vent %a /vane/ames)
            (vent %b /vane/behn)
            (vent %c /vane/clay)
            (vent %d /vane/dill)
            (vent %e /vane/eyre)
            (vent %f /vane/ford)
            (vent %g /vane/gall)
            (vent %j /vane/jael)
        ==
    ++  vent
      |=  [abr=term den=path]
      ^-  ovum
      =/  pax  (weld sys den)
      =/  txt  .^(@ %cx (weld pax /hoon))
      [[%vane den] [%veer abr pax txt]]
    --
::  installed: Arvo gate (formal interface) with %zuse and vanes installed
::
=/  installed
  =<  q
  %^    spin
      module-ova
    .*(0 arvo-formula)
  |=  [ovo=ovum ken=*]
  [~ .*(ken [%9 2 %10 [6 %1 now ovo] %0 1])]
::
::  file-ovum: userspace filesystem load
::
::    XX use top.arg
::    XX deduplicate with +brass (lib/pill ?)
::
=/  file-ovum=ovum
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
    =/  pax  (en-beam:format bec tyl)
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
::
::  our boot-ova is a list containing one massive formula:
::
::    We evaluate :arvo-formula (for jet registration),
::    then ignore the result and produces :installed
::
=/  boot-ova=(list)
  [[%7 arvo-formula %1 installed] ~]
::
::  a pill is a 3-tuple of event-lists: [boot kernel userspace]
::
::    Our kernel event-list is ~, as we've already installed them.
::    Our userspace event-list is a list containing a full %clay
::    filesystem sync event.
::
[boot-ova ~ [file-ovum ~]]
