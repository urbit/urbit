::  Add tree chrome
::
::::  /hoon/wrap/urb/ren
  ::
/?    310
/-    urb
/+    urb-split,    :: for single-page apps
      nutalk        ::FIXME write ren/urb/nutalk
/%    /^  $-(inr/{@uvH manx} {hed/{@uvH marl} bod/{@uvH marl}})
      /~  ::  XX don't have path information
      nutalk
::       /,      /web/pages/nutalk   /~  nutalk
::               /web/pages          /~  urb-split
::           :: put collections through the same .htm
::           :: routing structure as nutalk
::               /web/collections    /~  nutalk
::       ::
::               /
::           :: /urb-tree/
::           /~
::           |=  [@ manx]  ^-  urb
::           ~|(%tree-unimplemented !!)
::       ==
-.-

:: /=   hed    /#    /%   /tree-head/
:: /=   bod    /#    /%   /tree-body/
:: ^-  {hed/{@uvH marl} bod/{@uvH marl}}
:: [hed bod]
:: |=  [dep-bod=@uvH bod=manx]
:: ^-  {hed/{@uvH marl} bod/{@uvH marl}}
:: :*  :: head
::   :-  0v0
::   [;(title:"Wrapped!")] ::REVIEW ugly
::     ::  body
::   :-  dep-bod
::   ;=
::     ;h1: Wrapping
::     ;hr;
::     ;+  bod
::   ==
:: ==
