|%
++  base-hash
  |=  [our=@p now=@da]
  ^-  (list @uv)
  =+  .^  ota=(unit [=ship =desk =aeon:clay])
          %gx  /(scot %p our)/hood/(scot %da now)/kiln/ota/noun
      ==
  ?~  ota
    ~
  =/  parent  (scot %p ship.u.ota)
  =/  takos
    .^  (list tako:clay)  %cs
        /(scot %p our)/home/(scot %da now)/base/[parent]/[desk.u.ota]
    ==
  %+  turn  takos
  |=  =tako:clay
  .^(@uv %cs /(scot %p our)/home/(scot %da now)/hash/(scot %uv tako))
--
