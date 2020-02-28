/+  *test
=,  ford
|%
::  +expect-schematic: assert a +schematic:ford is what we expect
::
::    Since Ford requests contain types, we can't do simple
::    equality checking. This function handles all the different
::    kinds of +schematic:ford, dealing with types as necessary.
::
++  expect-schematic
  |=  [expected=schematic actual=schematic]
  ^-  tang
  ::
  ?^    -.expected
    ?.  ?=(^ -.actual)
      [%leaf "expected autocons, but got {<-.actual>}"]~
    ::
    %+  weld
      $(expected head.expected, actual head.actual)
    $(expected tail.expected, actual tail.actual)
  ::
  ?-    -.expected
      %$
    ?.  ?=(%$ -.actual)
      [%leaf "expected %$, but got {<-.actual>}"]~
    ::
    %+  weld
      (expect-eq !>(p.literal.expected) !>(p.literal.actual))
    (expect-eq q.literal.expected q.literal.actual)
  ::
      %pin
    ::
    ?.  ?=(%pin -.actual)
      [%leaf "expected %pin, but got {<-.actual>}"]~
    ::
    %+  weld
      (expect-eq !>(date.expected) !>(date.actual))
    $(expected schematic.expected, actual schematic.actual)
  ::
      %alts
    ::
    ?.  ?=(%alts -.actual)
      [%leaf "expected %alts, but got {<-.actual>}"]~
    ::
    |-  ^-  tang
    ?~  choices.expected
      ::  make sure there aren't any extra :choices in :actual
      ::
      ?~  choices.actual
        ~
      [%leaf "actual had more choices than expected"]~
    ::  :expected isn't empty yet; make sure :actual isn't either
    ::
    ?~  choices.actual
      [%leaf "expected had more choices than actual"]~
    ::  recurse on the first sub-schematic
    ::
    %+  weld
      ^$(expected i.choices.expected, actual i.choices.actual)
    $(choices.expected t.choices.expected, choices.actual t.choices.actual)
  ::
      %bake
    (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %bunt
    (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %call
    ::
    ?.  ?=(%call -.actual)
      [%leaf "expected %call, but got {<-.actual>}"]~
    ::
    %+  weld
      $(expected gate.expected, actual gate.actual)
    $(expected sample.expected, actual sample.actual)
  ::
      %cast
    ::
    ?.  ?=(%cast -.actual)
      [%leaf "expected %cast, but got {<-.actual>}"]~
    ::
    ;:  weld
      (expect-eq !>(disc.expected) !>(disc.actual))
      (expect-eq !>(mark.expected) !>(mark.actual))
      $(expected input.expected, actual input.actual)
    ==
  ::
      %core
    (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %diff
    ::
    ?.  ?=(%diff -.actual)
      [%leaf "expected %diff, but got {<-.actual>}"]~
    ::
    ;:  weld
      (expect-eq !>(disc.expected) !>(disc.actual))
      $(expected start.expected, actual start.actual)
      $(expected end.expected, actual end.actual)
    ==
  ::
      %dude
    ::
    ?.  ?=(%dude -.actual)
      [%leaf "expected %dude, but got {<-.actual>}"]~
    ::
    %+  weld
      (expect-eq !>(error.expected) !>(error.actual))
    $(expected attempt.expected, actual attempt.actual)
  ::
      %hood
    (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %join
    ::
    ?.  ?=(%join -.actual)
      [%leaf "expected %join, but got {<-.actual>}"]~
    ::
    ;:  weld
      (expect-eq !>(disc.expected) !>(disc.actual))
      (expect-eq !>(mark.expected) !>(mark.actual))
      $(expected first.expected, actual first.actual)
      $(expected second.expected, actual second.actual)
    ==
  ::
      %list
    ::
    ?.  ?=(%list -.actual)
      [%leaf "expected %list, but got {<-.actual>}"]~
    ::
    |-  ^-  tang
    ?~  schematics.expected
      ::  make sure there aren't any extra :schematics in :actual
      ::
      ?~  schematics.actual
        ~
      [%leaf "actual had more schematics than expected"]~
    ::  :expected isn't empty yet; make sure :actual isn't either
    ::
    ?~  schematics.actual
      [%leaf "expected had more schematics than actual"]~
    ::
    %+  weld
      ^$(expected i.schematics.expected, actual i.schematics.actual)
    ::
    %_  $
      schematics.expected  t.schematics.expected
      schematics.actual    t.schematics.actual
    ==
  ::
      %mash
    ::
    ?.  ?=(%mash -.actual)
      [%leaf "expected %mash, but got {<-.actual>}"]~
    ::
    ;:  weld
      (expect-eq !>(disc.expected) !>(disc.actual))
      (expect-eq !>(mark.expected) !>(mark.actual))
      (expect-eq !>(disc.first.expected) !>(disc.first.actual))
      (expect-eq !>(mark.first.expected) !>(mark.first.actual))
      (expect-eq !>(disc.second.expected) !>(disc.second.actual))
      (expect-eq !>(mark.second.expected) !>(mark.second.actual))
      $(expected schematic.first.expected, actual schematic.first.actual)
      $(expected schematic.second.expected, actual schematic.second.actual)
    ==
  ::
      %mute
    ::
    ?.  ?=(%mute -.actual)
      [%leaf "expected %mute, but got {<-.actual>}"]~
    ::
    %+  weld  $(expected subject.expected, actual subject.actual)
    ::
    |-  ^-  tang
    ?~  mutations.expected
      ::  make sure there aren't any extra :mutations in :actual
      ::
      ?~  mutations.actual
        ~
      [%leaf "actual had more mutations than expected"]~
    ::  :expected isn't empty yet; make sure :actual isn't either
    ::
    ?~  mutations.actual
      [%leaf "expected had more mutations than actual"]~
    ::
    ;:  weld
      (expect-eq !>(p.i.mutations.expected) !>(p.i.mutations.actual))
      ^$(expected q.i.mutations.expected, actual q.i.mutations.actual)
      %_  $
        mutations.expected  t.mutations.expected
        mutations.actual    t.mutations.actual
      ==
    ==
  ::
      %pact
    ::
    ?.  ?=(%pact -.actual)
      [%leaf "expected %pact, but got {<-.actual>}"]~
    ::
    ;:  weld
      (expect-eq !>(disc.expected) !>(disc.actual))
      $(expected start.expected, actual start.actual)
      $(expected diff.expected, actual diff.actual)
    ==
  ::
      %path
    (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %plan
    (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %reef
    (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %ride
    ::
    ?.  ?=(%ride -.actual)
      [%leaf "expected %ride, but got {<-.actual>}"]~
    ::
    %+  weld
      (expect-eq !>(formula.expected) !>(formula.actual))
    $(expected subject.expected, actual subject.actual)
  ::
      %same
    ::
    ?.  ?=(%same -.actual)
      [%leaf "expected %same, but got {<-.actual>}"]~
    ::
    $(expected schematic.expected, actual schematic.actual)
  ::
      %scry
    (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %slim
    ::
    ?.  ?=(%slim -.actual)
      [%leaf "expected %slim, but got {<-.actual>}"]~
    ::
    %+  weld
      (expect-eq !>(formula.expected) !>(formula.actual))
    ::
    %+  expect-eq
      !>(`?`%.y)
    ^-  vase
    :-  -:!>(*?)
    ^-  ?
    (~(nest ut subject-type.expected) | subject-type.actual)
  ::
      %slit
    ::
    ?.  ?=(%slit -.actual)
      [%leaf "expected %slit, but got {<-.actual>}"]~
    ::
    %+  weld
      (expect-eq gate.expected gate.actual)
    (expect-eq sample.expected sample.actual)
  ::
      ?(%vale %volt)
    (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %walk
    (expect-eq [schematic-type expected] [schematic-type actual])
  ==
::  +schematic-type: the +type for +schematic:ford
::
++  schematic-type  ^~  `type`-:!>(*schematic:ford)
--
