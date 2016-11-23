::
::::  /hoon/metal/gen
  ::
/?    310
::
::::
  !:
:-  %say
|=  $:  {now/@da *}
        *
    ==
:-  %noun
=+  ^=  event-zero
    ::
    ::  event 0 is the lifecycle formula which computes the final
    ::  state from the full event sequence.
    ::
    ::  the formal urbit state is always just a gate (function)
    ::  which, passed the next event, produces the next state.
    ::
    =>  [boot-formula=* full-sequence=*]
    !=  ::
        ::  first we use the boot formula (event 1) to set up
        ::  the pair of state function and main sequence.  the boot
        ::  formula peels off the first n (currently 3) events
        ::  to set up the lifecycle loop.
        ::
        =+  [state-gate main-sequence]=.*(full-sequence boot-formula)
        ::
        ::  in this lifecycle loop, we replace the state function
        ::  with its product, called on the next event, until
        ::  we run out of events.
        ::
        ::  we have to use raw nock to "call" the function,
        ::  since these are untyped nouns.
        ::
        ::  in real life we don't actually run the lifecycle loop,
        ::  since 
        ::
        |-  ?@  main-sequence
              state-gate
            %=  $
              main-sequence  +.main-sequence
              state-gate     .*(state-gate(+< -.main-sequence) -.state-gate)
            ==
=+  ^=  event-one
    ::
    ::  event 1 is the boot formula, which verifies the compiler
    ::  and starts the main lifecycle.
    ::
    =>  :*  ::  event 2: a trap (hoon |.), producing the hoon compiler
            ::
            compiler-trap=**
            ::
            ::  event 3: hoon compiler source, compiling to event 2
            ::
            compiler-source=*@
            ::
            ::  event 4: arvo kernel source
            ::
            kernel-source=*@
            ::
            ::  events 5..n: main sequence with normal semantics
            ::
            main-sequence=**
        ==
    !=  :_  main-sequence
        ::
        ::  activate the compiler gate.  the reason we use a trap
        ::  here is to actually compose the gate live, activating
        ::  any jets as we build the cores.  the compiler trap is
        ::  the only true binary used in the boot sequence.
        ::
        ::  as always, we have to use raw nock as we have no type.
        ::  the gate is in fact ++ride.
        ::
        =+  ^=  compiler-gate
            .*(compiler-trap -:compiler-trap)
        ::
        ::  compile the compiler source, producing (pair span nock).
        ::  the compiler ignores its input so we use a trivial span.
        ::
        =+  ^=  compiler-tool
            .*(compiler-gate(+< [%noun compiler-source]) -.compiler-gate)
        ::
        ::  run the nock to produce a new copy of the compiler trap;
        ::  check that it equals the old one.
        ::
        ?>  =(compiler-trap -:.*(0 +:compiler-tool))
        ::
        ::  get the span (type) of the context of the compiler gate,
        ::  which is the compiler core.  this is at tree address 15
        ::  from the span produced by compiling the compiler source,
        ::  which is the span of the compiler trap.
        ::
        =+  ^=  compiler-span
            .*(compiler-gate(+< [-.compiler-gate '+15']) -.compiler-gate)
        ::
        ::  compile the arvo source against the compiler core.
        ::
        =+  ^=  kernel-tool
            .*(compiler-gate(+< [compiler-span kernel-source]) -.compiler-gate)
        ::
        ::  pass the compiler core to the arvo kernel, and we done
        ::
        .*(+>:compiler-gate +:kernel-tool)
42
::=+  ^=  
::::  
::::  load files.  ship and desk are in generator beak.  case is now.
::::  source files:
::::
::::        sys/hoon        compiler
::::        sys/arvo        kernel
::::        sys/zuse        standard library
::::        sys/vane/ames   network vane
::::        sys/vane/behn   timer vane
::::        sys/vane/clay   revision-control vane
::::        sys/vane/dill   console vane
::::        sys/vane/eyre   web/internet vane
::::        sys/vane/ford   build vane
::::        sys/vane/gall   app vane
::::        sys/vane/jael   security vane
::::
::=+  top=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/arvo
::=+  ^=  com
::=+  pax=`path`(weld top `path`[%hoon ~])
::~&  %plastic-start
::=+  gen=(reck (weld 
::~&  %plastic-parsed
::=+  ken=q:(~(mint ut %noun) %noun gen)
::~&  %plastic-compiled
:::-  ken
::=+  all=.*(0 ken)
::=+  ^=  vent
::    |=  {abr/term den/path}
::    =+  pax=(weld top den)
::    =+  txt=.^(@ %cx (weld pax `path`[%hoon ~]))
::    `ovum`[[%vane den] [%veer abr pax txt]]
::=+  ^=  evo  
::    ^-  (list ovum)
::    :~  (vent %$ /zuse)
::        [[%name (scot %p who) ~] [%veal who]]
::        (vent %c /vane/clay)
::        (vent %g /vane/gall)
::        (vent %f /vane/ford)
::        (vent %a /vane/ames)
::        (vent %b /vane/behn)
::        (vent %d /vane/dill)
::        (vent %e /vane/eyre)
::    ==
::|-  ^+  all
::?~  evo  all
::~&  [%plastic-step p.i.evo]
::=+  gat=.*(all .*(all [0 42]))
::=+  nex=+:.*([-.gat [[now i.evo] +>.gat]] -.gat)
::$(evo t.evo, all nex)
