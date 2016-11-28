::
::::  /hoon/metal/gen
  ::
/?    310
::
::::
  !:
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        {{who/@p $~} $~}
    ==
:-  %noun
::
::  we're creating an event series E whose lifecycle can be computed
::  with the standard lifecycle formula L, [2 [0 3] [0 2]].  that is:
::  if E is the list of events processed by a computer in its life,
::  its final state is S, where S is nock(E L).
::
::  in practice, the first five nouns in E are: two boot loaders,
::  a hoon compiler as a nock formula, the same compiler as source,
::  and the arvo kernel as source.  after the first five events,
::  we enter an iterative form in which the state is a function
::  that, passed the next event, produces the next state.
::
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
        ::  in real life we don't actually run the lifecycle loop,
        ::  since real life is updated incrementally and also cares
        ::  about things like output.  we couple to the internal
        ::  structure of the state machine and work directly with
        ::  the underlying arvo engine.
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
    =>  :*  ::  event 2: a formula producing the hoon compiler
            ::
            compiler-formula=**
            ::
            ::  event 3: hoon compiler source, compiling to event 2
            ::
            compiler-source=*@t
            ::
            ::  event 4: arvo kernel source
            ::
            arvo-source=*@t
            ::
            ::  events 5..n: main sequence with normal semantics
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
        ~>  %slog.[0 leaf+"1-c"]
        =+  ^=  compiler-tool
            .*(compiler-gate(+< [%noun compiler-source]) -.compiler-gate)
        ::
        ::  check that the new compiler formula equals the old formula.
        ::  this is not proof against thompson attacks but it doesn't hurt.
        ::
        ~>  %slog.[0 leaf+"1-d"]
        ?>  =(compiler-formula +:compiler-tool)
        ::
        ::  get the span (type) of the kernel core, which is the context
        ::  of the compiler gate.  we just compiled the compiler,
        ::  so we know the span (type) of the compiler gate.  its
        ::  context is at tree address `+>` (ie, `+7` or Lisp `cddr`).
        ::  we use the compiler again to infer this trivial program.
        ::
        ~>  %slog.[0 leaf+"1-e"]
        =+  ^=  kernel-span
            -:.*(compiler-gate(+< [-.compiler-tool '+>']) -.compiler-gate)
        ::
        ::  compile the arvo source against the kernel core.
        ::
        ~>  %slog.[0 leaf+"1-f"]
        =+  ^=  kernel-tool
            .*(compiler-gate(+< [kernel-span arvo-source]) -.compiler-gate)
        ::
        ::  create the arvo kernel, whose subject is the kernel core.
        ::
        ~>  %slog.[0 leaf+"1-g"]
        .*(+>:compiler-gate +:kernel-tool)
::  
::  load files.  ship and desk are in generator beak.  case is now.
::  source files:
::
::        sys/hoon        compiler
::        sys/arvo        kernel
::        sys/zuse        standard library
::        sys/vane/ames   network vane
::        sys/vane/behn   timer vane
::        sys/vane/clay   revision-control vane
::        sys/vane/dill   console vane
::        sys/vane/eyre   web/internet vane
::        sys/vane/ford   build vane
::        sys/vane/gall   app vane
::        sys/vane/jael   security vane
::
=+  sys=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/sys
=+  compiler-source=.^(@t %cx (welp sys /hoon/hoon))
~&  %metal-parsing
=+  compiler-twig=(ream compiler-source)
~&  %metal-parsed
=+  compiler-formula=q:(~(mint ut %noun) %noun compiler-twig)
~&  %metal-compiled
=+  arvo-source=.^(@t %cx (welp sys /arvo/hoon))
=+  ^=  vane-sequence
    |^  ^-  (list ovum)
        :~  (vent %$ /zuse)
            [[%name (scot %p who) ~] [%veal who]]
            (vent %c /vane/clay)
            (vent %g /vane/gall)
            (vent %f /vane/ford)
            (vent %a /vane/ames)
            (vent %b /vane/behn)
            (vent %d /vane/dill)
            (vent %e /vane/eyre)
            (vent %j /vane/jael)
        ==
    ::
    ++  vent
      |=  {abr/term den/path}
      =+  pax=(weld sys den)
      =+  txt=.^(@ %cx (welp pax /hoon))
      `ovum`[[%vane den] [%veer abr pax txt]]
    --
::
::  ~&  %metal-testing
::  =+  ^=  yop
::      ^-  @p
::      %-  mug
::      .*  :*  event-zero
::              event-one
::              compiler-formula
::              compiler-source
::              arvo-source
::              vane-sequence
::          ==
::      [2 [0 3] [0 2]]
::  ~&  [%metal-tested yop]
::
:*  event-zero
    event-one
    compiler-formula
    compiler-source
    arvo-source
    vane-sequence
==
