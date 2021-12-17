/-  store=graph-store
/+  resource
|_  [=bowl:gall rid=resource count=@ud]
++  desk  %landscape
++  loc  /graph-store/(scot %p entity.rid)/[name.rid]
++  pag  (welp log /pages)
++  log  (welp loc /logs)
++  append-log
  |=  upd=logged-update:store
  ^-  [card:agent:gall @ud]
  =/  =miso:clay  [%ins noun+!>(upd)]
  =/  =path       (welp log /(scot %ud count)/graph-update-3)
  =/  =soba:clay  [path miso]~
  =/  =nori:clay  [%& soba]
  =/  =task:clay  [%info desk nori]
  :_(+(count) [%pass / %arvo %c task])
--
