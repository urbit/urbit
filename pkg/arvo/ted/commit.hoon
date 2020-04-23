::  Delete `deletes`, insert/change `changes`, and don't touch anything
::  else.
::
/-  spider
/+  strandio, clay-commit
=,  strand=strand:spider
=,  clay
^-  thread:spider
|=  arg=vase
=+  !<([=desk deletes=(set path) changes=(map path cage) ~] arg)
=/  m  (strand ,vase)
^-  form:m
::
::  Fetch current state
::
;<  our=@p  bind:m  get-our:strandio
;<  now=@da  bind:m  get-time:strandio
=+  .^(=dome %cv /(scot %p our)/[desk]/(scot %da now))
::
::  Apply changes to current state to create new yuki
::
=/  parent-tako=tako  (~(got by hit.dome) let.dome)
=/  data=(map path (each page lobe))
  =+  .^  =parent=yaki  %cs
          /(scot %p our)/[desk]/(scot %da now)/yaki/(scot %uv parent-tako)
      ==
  =/  after-deletes
    %-  ~(dif by q.parent-yaki)
    (malt (turn ~(tap in deletes) |=(=path [path *lobe])))
  =/  after=(map path (each page lobe))
    (~(run by after-deletes) |=(=lobe |+lobe))
  %-  ~(uni by after)
  ^-  (map path (each page lobe))
  (~(run by changes) |=(=cage &+[p q.q]:cage))
=/  =yuki  [~[parent-tako] data]
::
::  Send to clay
::
=/  args  [desk yuki *rang]
;<  ~  bind:m  (send-raw-card:strandio %pass /commit/[desk] %arvo %c %park args)
(pure:m !>(~))
