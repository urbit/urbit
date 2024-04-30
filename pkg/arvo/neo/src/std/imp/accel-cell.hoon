/@  accel-cell
::
^-  firm:neo
=>
|%
::  from migrev:
++  render-udon
  |=  code=@t
  ^-  (unit (each manx tang))
  ::  this doesn't actually run the code you give it!
  ::  it just expects sail and then renders it.
  ::  you can't do arbitrary hoon
  =/  newline  (trip 10)
  =/  udon
    :: format as udon document
    %-  crip
    ;:  welp
      ";>"  newline  newline
      (trip code)  newline
    ==
  ~&  >  udon
  =/  mul
    %-  mule
    |.
    !<  manx
    %+  slap  !>(..zuse)
    (ream udon)
  ?-  -.mul
    %.y  (some [%.y (manx p.mul)])
    %.n  (some [%.n (tang p.mul)])
  ==
--
|%
++  state  %accel-cell
++  poke  ~
++  kids  *kids:neo
++  deps
  %-  ~(gas by *deps:neo)
  :~  [%ref | [%accel-cell %sig] ~]
  ==
::
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    [~ state-vase]
  ++  init
    |=  old=(unit vase)
    =/  text  ;;(@t !<(@t (need old)))
    =/  ref=(unit (pair pith cane:neo))  
      (~(get by deps.bowl) %ref)
    :-  ~
    !>([text (render-udon text)])
  --
--
