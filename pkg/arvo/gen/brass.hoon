::  Produce a brass pill
::
::::  /hoon/brass/gen
  ::
/?    310
/+  pill
::
::::
  !:
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        arg=$@(~ [top=path ~])
        ~
    ==
::
::  we're creating an event series E whose lifecycle can be computed
::  with the urbit lifecycle formula L, `[2 [0 3] [0 2]]`.  that is:
::  if E is the list of events processed by a computer in its life,
::  its final state is S, where S is nock(E L).
::
::  in practice, the first five nouns in E are: two boot formulas,
::  a hoon compiler as a nock formula, the same compiler as source,
::  and the arvo kernel as source.
::
::  after the first five special events, we enter an iterative
::  sequence of regular events which continues for the rest of the
::  computer's life.  during this sequence, each state is a function
::  that, passed the next event, produces the next state.
::
::  a regular event is a `[date wire type data]` tuple, where `date` is a
::  128-bit Urbit date; `wire` is an opaque path which output can
::  match to track causality; `type` is a symbol describing the type
::  of input; and `data` is input data specific to `type`.
::
::  in real life we don't actually run the lifecycle loop,
::  since real life is updated incrementally and also cares
::  about things like output.  we couple to the internal
::  structure of the state machine and work directly with
::  the underlying arvo engine.
::
::  this arvo core, which is at `+7` (Lisp `cddr`) of the state
::  function (see its public interface in `sys/arvo`), gives us
::  extra features, like output, which are relevant to running
::  a real-life urbit vm, but don't affect the formal definition.
::
::  so a real-life urbit interpreter is coupled to the shape of
::  the arvo core.  it becomes very hard to change this shape.
::  fortunately, it is not a very complex interface.
::
:-  %noun
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
::  sys: root path to boot system, `/~me/[desk]/now/sys`
::
=/  sys=path
  ?^  arg  top.arg
  /(scot %p p.bec)/[q.bec]/(scot %da now)/sys
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
=+  ^=  boot-ova  ^-  (list *)
    :~  boot-one
        boot-two
        compiler-formula
        compiler-source
        arvo-source
    ==
::  a pill is a 3-tuple of event-lists: [boot kernel userspace]
::
=/  bas=path  (flop (tail (flop sys)))
:+  boot-ova
  :~  :~  //arvo
          %what
          [/sys/hoon hoon/compiler-source]
          [/sys/arvo hoon/arvo-source]
      ==
      (file-ovum2:pill bas)
  ==
[(file-ovum:pill bas) ~]
