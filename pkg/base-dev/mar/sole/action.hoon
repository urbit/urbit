::
::::  /hoon/action/sole/mar
  ::
/?    310
/-    sole
::
::::
  ::
=,  sole
|_  sole-action
::
++  grad  %noun
++  grow
  |%
  ++  noun  +<.grad
  --
++  grab                                                ::  convert from
  |%
  ++  json
    |=  jon=^json  ^-  sole-action
    %-  need  %.  jon
    =>  [dejs-soft:format ..sole-action]
    |^  (ot id+id dat+(fo %ret (of det+change tab+ni ~)) ~)
    ++  id
      (ot who+(su ;~(pfix sig fed:ag)) ses+so ~)
    ++  fo
      |*  [a=term b=fist]
      |=(c=json ?.(=([%s a] c) (b c) (some [a ~])))
    ::
    ++  ra
      |*  [a=[term fist] b=fist]
      |=  c=json  %.  c
      ?.(=(%a -.c) b (pe -.a (ar +.a)))
    ::
    ++  ke                                              ::  callbacks
      |*  [gar=* sef=(trap fist)]
      |=  jon=json  ^-  (unit _gar)
      =-  ~!  gar  ~!  (need -)  -
      ((sef) jon)
    ::
    ++  change  (ot ler+(at ni ni ~) ted+(pe 0v0 edit) ~)
    ++  char  (cu taft so)
    ++  edit
      %+  ke  *sole-edit  |.  ~+
      %+  fo  %nop
      %+  ra  mor+edit
      (of del+ni set+(cu tuba sa) ins+(ot at+ni cha+char ~) ~)
    --
  ::
  ++  noun  sole-action                                   ::  clam from %noun
  --
--
