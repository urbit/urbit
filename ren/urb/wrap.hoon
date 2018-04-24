::  Add tree chrome
::
::::  /hoon/wrap/urb/ren
  ::
/?    310
:: /=   hed    /#    /%   /tree-head/
:: /=   bod    /#    /%   /tree-body/
:: ^-  {hed/{@uvH marl} bod/{@uvH marl}}
:: [hed bod]
|=  [dep-bod=@uvH bod=manx]
^-  {hed/{@uvH marl} bod/{@uvH marl}}
:*  :: head
  :-  0v0
  [;(title:"Wrapped!")] ::REVIEW ugly
    ::  body
  :-  dep-bod
  ;=
    ;h1: Wrapping
    ;hr;
    ;+  bod
  ==
==
