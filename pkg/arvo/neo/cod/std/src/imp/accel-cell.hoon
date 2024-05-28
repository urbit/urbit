/@  accel-cell
::
^-  kook:neo
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
  =/  a=(unit (pair pith lore:neo))
    (~(get by deps.bowl) %a)
  =/  b=(unit (pair pith lore:neo))
    (~(get by deps.bowl) %b)
  =/  va
    ?~  a  !>(~)
    =/  =idea:neo  (~(got of:neo q.u.a) /)
    (maybe-promote-vase pail.idea)
  =/  vb
    ?~  b  !>(~)
    =/  =idea:neo  (~(got of:neo q.u.b) /)
    (maybe-promote-vase pail.idea)
  [va vb]
::
++  update
  |=  [cell=accel-cell =bowl:neo]
  ^-  (quip card:neo pail:neo)
  =/  vavb=[vase vase]  (get-deps-vases bowl)
  =.  result.cell
    (render-hoon code.cell vavb)
  :_  accel-cell/!>(cell)
  ?~  target.cell  ~
  ?~  result.cell  ~
  ?-  -.u.result.cell
    %.n  ~
    %.y  [pith.u.target.cell %poke [stud.u.target.cell p.u.result.cell]]~
  ==
--
|%
++  state  pro/%accel-cell
++  poke  (sy %rely ~)
++  kids  *kids:neo
++  deps
  %-  ~(gas by *deps:neo)
  :~  [%a | [pro/%pail (sy %sig ~)] ~]
      [%b | [pro/%pail (sy %sig ~)] ~]
  ==
::
++  form
  ^-  form:neo
  ::|_  [=bowl:neo =ever:neo state-vase=vase *]
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?.  =(stud %rely)
      [~ pail]
    =+  !<([=term =stem:neo] vax)
    ?>  ?=(%x -.q.stem)
    =/  cell  !<(accel-cell q.pail)
    (update cell bowl)
  ::
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    ?~  old
      [~ accel-cell/!>(*accel-cell)]
    =/  cell  !<(accel-cell q.u.old)
    (update cell bowl)
  --
--
