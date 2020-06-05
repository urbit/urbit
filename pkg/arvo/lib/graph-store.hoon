/-  sur=graph-store, pos=post, res=resource
^?
=<  [sur .]
=<  [pos .]
=<  [res .]
=,  sur
=,  pos
=,  res
|%
++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    ^-  json
    |^  (frond %graph-update (pairs ~[(encode upd)]))
    ::
    ++  encode
      |=  upd=^update
      ^-  [cord json]
      [*cord *json]
    --
  --
::
++  dejs
  =,  dejs:format
  |%
  ++  action
    |=  =json
    ^-  ^action
    !!
  --
--
