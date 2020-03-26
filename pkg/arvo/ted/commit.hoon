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
::  Cast to expected marks
::
;<  our=@p  bind:m  get-our:strandio
=/  cast-builds=(map path schematic:ford)
  %-  ~(urn by changes)
  |=  [=path =cage]
  [%cast [our desk] =>((flop path) ?~(. %$ i)) %$ cage]
;<  cast-results=(map path cage)  bind:m  (build-cages:strandio cast-builds)
::
::  Fetch current state
::
;<  now=@da  bind:m  get-time:strandio
=+  .^(=dome %cv /(scot %p our)/[desk]/(scot %da now))
::
::  Apply changes to current state to create new yaki
::
=/  new-blobs=(map path blob)
  %-  ~(run by cast-results)
  |=  =cage
  =/  =page  [p q.q]:cage
  [%direct (page-to-lobe page) page]
::
=/  parent-tako=tako  (~(got by hit.dome) let.dome)
=/  all-lobes=(map path lobe)
  =+  .^  =parent=yaki  %cs
          /(scot %p our)/[desk]/(scot %da now)/yaki/(scot %uv parent-tako)
      ==
  =/  after-deletes
    %-  ~(dif by q.parent-yaki)
    (malt (turn ~(tap in deletes) |=(=path [path *lobe])))
  %-  ~(uni by after-deletes)
  (~(run by new-blobs) |=(=blob p.blob))
::
::  XX should we be getting the time later, after all async?
;<  now=@da  bind:m  get-time:strandio
=/  new-yaki=yaki  (make-yaki ~[parent-tako] all-lobes now)
::
::  Apply new blobs and yaki to rang
::
=/  =rang
  :-  (~(put by hut:*rang) r.new-yaki new-yaki)
  (malt (turn ~(tap by new-blobs) |=([=path =blob] [p.blob blob])))
::
::  Checkout ankh and mime cache (derived state)
::
=/  =ankh  (checkout:clay-commit ank.dome deletes cast-results)
;<  mim=(map path (unit mime))  bind:m
  (checkout-cache:clay-commit desk deletes cast-results)
::  Send to clay
::
=/  args  [desk r.new-yaki rang ankh mim]
;<  ~  bind:m  (send-raw-card:strandio %pass /commit/[desk] %arvo %c %park args)
(pure:m !>(~))
