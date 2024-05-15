/@  accel-cell
::
^-  firm:neo
=>
|%
++  render-hoon
  |=  [code=@t a=vase b=vase]
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
      %+  slop
        %+  slop
          !>(..zuse) 
        a(p [%face %a p.a])
      b(p [%face %b p.b])
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
::
++  get-deps-vases
  |=  =bowl:neo
  ^-  [vase vase]
  =/  a=(unit (pair pith cane:neo))
    (~(get by deps.bowl) %a)
  =/  b=(unit (pair pith cane:neo))
    (~(get by deps.bowl) %b)
  =/  va
    ?~  a  !>(~)
    (maybe-promote-vase pail.q.u.a)
  =/  vb
    ?~  b  !>(~)
    (maybe-promote-vase pail.q.u.b)
  [va vb]
::
++  update
  |=  [cell=accel-cell =bowl:neo]
  ^-  (quip card:neo vase)
  =/  vavb=[vase vase]  (get-deps-vases bowl)
  =.  result.cell
    (render-hoon code.cell vavb)
  [~ !>(cell)]
--
|%
++  state  %accel-cell
++  poke  (sy %rely ~)
++  kids  *kids:neo
++  deps
  %-  ~(gas by *deps:neo)
  :~  [%a | [%pail %sig] ~]
      [%b | [%pail %sig] ~]
  ==
::
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?.  =(stud %rely)  
      [~ state-vase]
    =+  !<([=term =stem:neo] vax)
    ?>  ?=(%x -.q.stem)
    =/  cell  !<(accel-cell state-vase)
    (update cell bowl)
  ::
  ++  init
    |=  old=(unit vase)
    ^-  (quip card:neo vase)
    =/  cell  !<(accel-cell (need old))
    (update cell bowl) 
  --
--
