/+  dock=docket
|_  treaties=(list treaty:dock)
++  grow
  |%
  ++  noun  treaties
  ++  json  
    ^-  ^json
    %-  pairs:enjs:format
    %+  turn  treaties
    |=  =treaty:dock
    :-  (crip "{(scow %p ship.treaty)}/{(trip desk.treaty)}")
    (treaty:enjs:dock treaty)

  --
++  grab
  |%
  ++  noun  (list treaty:dock)
  --
++  grad  %noun
--
