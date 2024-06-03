/@  number
/@  counter-diff
::
::  outer core of a shrub
^-  kook:neo
|%
::
::  the state of counter is a %number, just a @ud
::  curb:neo is a head-tagged constraint on the state
::  here the state is only ever one thing: a %number
++  state
  ^-  curb:neo
  [%only %number]
::
::  the set of pokes that counter
::  takes only contains %counter-diff
++  poke
  ^-  (set stud:neo)
  (sy %counter-diff ~)
::
::
::  counter does not constrain the type and behaviour of
::  its children; any shrub can be made below this shrub
::  in the tree, they can have any state, kids, or pokes
++  kids
  ^-  kids:neo
  *kids:neo
::
::  counter has no other shrubs as dependencies
++  deps
  ^-  deps:neo
  *deps:neo
::
::  inner core of a shrub
++  form
  ^-  form:neo
  ::  treat this door's sample as boilerplate
  |_  [=bowl:neo =aeon:neo =pail:neo]
    ::
    ::  +init, like +on-init
    ++  init
      ::
      ::  minimal +init, just returns the
      ::  initial state passed in on %make
      |=  old=(unit pail:neo)
      ^-  (quip card:neo pail:neo)
      [~ (need old)]
    ::
    ::  +poke, like +on-poke
    ++  poke
      ::
      ::  a stud (e.g. %number or %counter-diff) is kind
      ::  of like a mark, it only gets more complicated
      ::  than that with types from other desks/ships
      |=  [=stud:neo vaz=vase]
      ::
      ::  return a (list card:neo) and a
      ::  pail, which is a (pair stud vase)
      ^-  (quip card:neo pail:neo)
      ::
      ::  assert the stud of the pail (pair stud vase),
      ::  which is the shrub's state given in the sample
      ?>  =(p.pail %number)
      =/  state  !<(number q.pail)
      ::
      ::  assert that the poke's stud is %counter-diff,
      ::  which protects counter from evil vases
      ?>  =(%counter-diff stud)
      =/  act
        !<(counter-diff vaz)
      ?>  =(-.act %inc)
      ::
      ::  return no cards and a pail
      [~ [%number !>(+(state))]]
  --
--
