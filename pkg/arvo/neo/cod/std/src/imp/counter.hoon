/@  number
/@  counter-diff
::
::  outer core of a shrub
^-  kook:neo
|%
::
::  the state of counter is a %number, just a @ud
::  XX explain %pro / $curb?
++  state
  ^-  curb:neo
  [%pro %number]
::
::  the set of pokes that counter
::  takes only contains %counter-diff
++  poke
  ^-  (set stud:neo)
  (sy %counter-diff ~)
::
::  counter can have no children
::  beneath it in the tree
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
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
    ::
    ::  de-vase the state; we don't know what it is,
    ::  in most cases it will be counter's old state
    +*  this  !<(number state-vase)
    ::  +poke, like +on-poke
    ++  poke
      ::
      ::  XX explain stud
      |=  [=stud:neo vaz=vase]
      ::
      ::  return a (list card:neo) and a
      ::  pail, which is a (pair stud vase)
      ^-  (quip card:neo pail:neo)
      ::
      ::  assert that the poke's stud is %counter-diff,
      ::  which protects counter from evil vases
      ?>  =(%counter-diff stud)
      =/  act
        !<(counter-diff vaz)
      ?>  =(-.act %inc)
      ::
      ::  return no cards and a pail
      [~ [%number !>(+(this))]]
    ::
    ::  +init, like +on-init
    ++  init
      ::
      ::  minimal +init, just returns the
      ::  initial state passed in on %make
      |=  old=(unit pail:neo)
      ^-  (quip card:neo pail:neo)
      [~ (need old)]
  --
--
