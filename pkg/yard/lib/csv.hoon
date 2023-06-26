::  Parse CSV files with a known schema, then perform queries on the
::  results.
::
|%
++  text
  %+  cook
    |=  =tape
    (crip tape)
  ;~  pose
    (cook tape soil:vast)
    non-quote-text
  ==
::
++  non-quote-text
  (star ;~(less com qit))
::
++  parse
  |*  cols=(list rule)
  %+  ifix
    :-  ;~(sfix ;~(less (just `@`10) (star prn)) (just `@`10))
    (just `@`10)
  (more (just `@`10) (parse-line cols))
::
++  parse-line
  |*  cols=(list rule)
  ?~  cols
    (easy ~)
  ?~  t.cols
    i.cols
  ;~  plug
      i.cols
      ;~(pfix com $(cols t.cols))
  ==
::
::  inner join
::
++  join
  =/  name-side  (ream '[left=- right=+]')
  |=  [left=(list vase) rite=(list vase) =hoon]
  ^-  (list vase)
  |-  ^-  (list vase)
  =*  left-loop  $
  ?~  left
    ~
  =/  rote  rite
  |-  ^-  (list vase)
  =*  rite-loop  $
  ?~  rite
    left-loop(left t.left, rite rote)
  =/  slopped-row  (slap (slop i.left i.rite) name-side)
  =/  val  (slap (slop slopped-row !>(..zuse)) hoon)
  ?.  =(%& q.val)
    rite-loop(rite t.rite)
  :-  slopped-row
  rite-loop(rite t.rite)
::
::  filter
::
++  where
  |=  [rows=(list vase) =hoon]
  ^-  (list vase)
  %+  skim  rows
  |=  =vase
  =/  val  (slap vase hoon)
  =(%& q.val)
::
::  select
::
++  select
  |=  [=hoon rows=(list vase)]
  ^-  (list vase)
  %+  turn  rows
  |=  =vase
  (slap (slop vase !>(..zuse)) hoon)
::
::  pretty-print rows
::
++  print-rows
  |=  rows=(list vase)
  (slog (turn rows sell))
--
