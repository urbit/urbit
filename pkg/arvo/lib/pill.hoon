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
::  +boot-ova: 
::
++  boot-ova
  |=  sys=path
  ^-  (list *)
  ::
  ::  boot-one: lifecycle formula
  ::
  =+  ^=  boot-one
      ::
      ::  event 1 is the lifecycle formula which computes the final
      ::  state from the full event sequence.
      ::
      ::  the formal urbit state is always just a gate (function)
      ::  which, passed the next event, produces the next state.
      ::
      =>  [boot-formula=* full-sequence=*]
      !=  ::
          ::  first we use the boot formula (event 1) to set up
          ::  the pair of state function and main sequence.  the boot
          ::  formula peels off the first 5 events
          ::  to set up the lifecycle loop.
          ::
          =+  [state-gate main-sequence]=.*(full-sequence boot-formula)
          ::
          ::  in this lifecycle loop, we replace the state function
          ::  with its product, called on the next event, until
          ::  we run out of events.
          ::
          |-  ?@  main-sequence
                state-gate
              %=  $
                main-sequence  +.main-sequence
                state-gate  .*(state-gate [%9 2 %10 [6 %1 -.main-sequence] %0 1])
              ==
  ::
  ::  boot-two: startup formula
  ::
  =+  ^=  boot-two
      ::
      ::  event 2 is the startup formula, which verifies the compiler
      ::  and starts the main lifecycle.
      ::
      =>  :*  ::  event 3: a formula producing the hoon compiler
              ::
              compiler-formula=**
              ::
              ::  event 4: hoon compiler source, compiling to event 2
              ::
              compiler-source=*@t
              ::
              ::  event 5: arvo kernel source
              ::
              arvo-source=*@t
              ::
              ::  events 6..n: main sequence with normal semantics
              ::
              main-sequence=**
          ==
      !=  :_  main-sequence
          ::
          ::  activate the compiler gate.  the product of this formula
          ::  is smaller than the formula.  so you might think we should
          ::  save the gate itself rather than the formula producing it.
          ::  but we have to run the formula at runtime, to register jets.
          ::
          ::  as always, we have to use raw nock as we have no type.
          ::  the gate is in fact ++ride.
          ::
          ~>  %slog.[0 leaf+"1-b"]
          =+  ^=  compiler-gate
              .*(0 compiler-formula)
          ::
          ::  compile the compiler source, producing (pair span nock).
          ::  the compiler ignores its input so we use a trivial span.
          ::
          ~>  %slog.[0 leaf+"1-c (compiling compiler, wait a few minutes)"]
          =+  ^=  compiler-tool
              .*(compiler-gate [%9 2 %10 [6 %1 [%noun compiler-source]] %0 1])
          ::
          ::  switch to the second-generation compiler.  we want to be
          ::  able to generate matching reflection nouns even if the
          ::  language changes -- the first-generation formula will
          ::  generate last-generation spans for `!>`, etc.
          ::
          ~>  %slog.[0 leaf+"1-d"]
          =.  compiler-gate  .*(0 +:compiler-tool)
          ::
          ::  get the span (type) of the kernel core, which is the context
          ::  of the compiler gate.  we just compiled the compiler,
          ::  so we know the span (type) of the compiler gate.  its
          ::  context is at tree address `+>` (ie, `+7` or Lisp `cddr`).
          ::  we use the compiler again to infer this trivial program.
          ::
          ~>  %slog.[0 leaf+"1-e"]
          =+  ^=  kernel-span
              -:.*(compiler-gate [%9 2 %10 [6 %1 [-.compiler-tool '+>']] %0 1])
          ::
          ::  compile the arvo source against the kernel core.
          ::
          ~>  %slog.[0 leaf+"1-f"]
          =+  ^=  kernel-tool
              .*(compiler-gate [%9 2 %10 [6 %1 [kernel-span arvo-source]] %0 1])
          ::
          ::  create the arvo kernel, whose subject is the kernel core.
          ::
          ~>  %slog.[0 leaf+"1-g"]
          .*(+>:compiler-gate +:kernel-tool)
  ::
  ::  compiler-source: hoon source file producing compiler, `sys/hoon`
  ::
  =+  compiler-source=.^(@t %cx (welp sys /hoon/hoon))
  ::
  ::  compiler-twig: compiler as hoon expression
  ::
  ~&  %brass-parsing
  =+  compiler-twig=(ream compiler-source)
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
  :~  boot-one
      boot-two
      compiler-formula
      compiler-source
      arvo-source
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
