::
::::  /hoon/sole-action/mar
  ::
/?    314
/-    sole
!:
::::
  ::
[sole .]
|_  sole-action
::
++  grab                                                ::  convert from
  |%
  ++  json
    |=  jon+^json  ^-  sole-action
    %-  need  %.  jon
    =>  [jo ..sole-action]
    |^  (fo %ret (of det/change ~))
    ++  fo
      |*  {a+term b+fist}
      |=(c+json ?.(=([%s a] c) (b c) (some [a ~])))
    ::
    ++  ra
      |*  {a+{p+term q+fist} b+fist}
      |=  c+json  %.  c
      ?.(=(%a -.c) b (pe p.a (ar q.a)))
    ::
    ++  change  (ot ler/(at ni ni ~) ted/(cu |*(a+* [0v0 a]) edit) ~)
    ++  char  (cu turf so)
    ++  edit
      %+  fo  %nop
      %+  ra  mor/|=(json (edit +<))
      (of del/ni set/(cu tuba sa) ins/(ot at/ni cha/char ~) ~)
    --
  ::
  ++  noun  sole-action                                   ::  clam from %noun
  --
--
