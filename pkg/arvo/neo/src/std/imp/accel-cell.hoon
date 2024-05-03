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
    :: to ensure user-error doesn't break evaluation
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
    =/  vax  q.pail.q.stem
    =/  this  (accel-cell !<(accel-cell state-vase))
    =.  result.this  (render-hoon code.this vax)
    [~ !>(this)]
  ++  init
    |=  old=(unit vase)
    =/  cell  (accel-cell !<(accel-cell (need old)))
    =/  ref=(unit (pair pith cane:neo))
      (~(get by deps.bowl) %ref)
    :-  ~
    =/  vax
      ?~  ref  !>(~)
      q.pail.q.u.ref
    =.  result.cell  (render-hoon code.cell vax)
    !>(cell)
  --
--
