/+  store=metadata-store
|_  =update:one:store
++  grad  %noun
++  grow
  |%
  ++  noun               update
  ++  metadata-update-2
    ^-  update:store
    update
  --
::
++  grab
  |%
  ++  noun  update:one:store
  ::  This is ok, we don't send %edit over the wire yet.
  ++  metadata-update-2
    |=  upd=update:store
    ^-  update:one:store
    ?<  ?=(%edit -.upd)
    upd
  --
--
