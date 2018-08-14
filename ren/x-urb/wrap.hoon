::  Add tree chrome
::
::::  /hoon/wrap/urb/ren
  ::
/?  309
/+  nutalk
/=  wrapped
    /^  $-(inr=manx out=manx)
    /~  nutalk
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
wrapped
