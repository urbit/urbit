::  Produce a zinc pill.
::
::  Brass is zinc plus copper. This pill corresponds to the normal Urbit boot
::  sequence with the current compiler, but no kernel module source or userland
::  files. The point of a zinc pill is so you can make a brass pill by taking
::  the zinc pill and adding the copper outside of Urbit from the git
::  repository.
::
::  While a zinc pill isn't bootable on its own, it still follows the three
::  list structure of all other pills of [boot-ova kernel-ova file-ova], where
::  kernel and file are null lists.
::
::::  /hoon/zinc/gen
  ::
/?    310
/+  pill
::
::::
  !:
:-  %say
|=  $:  {now/@da * bec/beak}
        {~ try/_| ~}
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
::  a pill is a 3-tuple of event-lists: [boot kernel userspace]
::
::  sys: root path to boot system, `/~me/[desk]/now/sys`
::
=+  sys=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/sys
[(boot-ova:pill sys) ~ ~]
