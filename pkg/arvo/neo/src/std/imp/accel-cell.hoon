/@  accel-cell
::
^-  firm:neo
=>
|%
++  render-hoon
  |=  [code=@t ref=vase]
  ^-  (unit (each vase tang))
  =/  code
    %-  crip
    :: (trip 10) = tape of newline
    :: to ensure that lack of newline at end
    :: of file doesn't break evaluation
    (welp (trip code) (trip 10))
  =/  mul
    %-  mule
    |.
    %+  slap
      (slop !>(..zuse) ref(p [%face %ref p.ref]))
    (ream code)
  ?-  -.mul
    %.y  (some [%.y p.mul])
    %.n  (some [%.n (tang p.mul)])
  ==
++  maybe-promote-vase
  ::  if the stud of the pail is %accel-cell,
  ::  'promote' the vase in its 'result'.
  ::  this avoids putting vases inside of vases
  ::  AND provides a better experience for the user
  ::  by no longer requiring them to unvase accel-cell
  ::  references themselves.
  |=  =pail:neo
  ^-  vase
  ?.  =(%accel-cell p.pail)
    q.pail
  =/  v  !<(accel-cell q.pail)
  ?~  result.v  !>(~)
  ?-  -.u.result.v
    %.n  !>(~)
    %.y  +.u.result.v
  ==
--
|%
++  state  %accel-cell
++  poke  (sy %rely ~)
++  kids  *kids:neo
++  deps
  %-  ~(gas by *deps:neo)
  :~  [%ref | [%pail %sig] ~]
  ==
::
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?.  =(stud %rely)  [~ state-vase]
    =+  !<([=term =stem:neo] vax)
    ?>  ?=(%x -.q.stem)
    =/  =pail:neo  pail.q.stem
    =/  cell  !<(accel-cell state-vase)
    =.  result.cell
      %+  render-hoon  code.cell
      (maybe-promote-vase pail.q.stem)
    [~ !>(cell)]
  ++  init
    |=  old=(unit vase)
    =/  cell  !<(accel-cell (need old))
    =/  ref=(unit (pair pith cane:neo))
      (~(get by deps.bowl) %ref)
    =.  result.cell
      %+  render-hoon  code.cell
      ?~  ref  !>(~)
      (maybe-promote-vase pail.q.u.ref)
    [~ !>(cell)]
  --
--
