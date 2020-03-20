::  Delete `deletes`, insert/change `changes`, and don't touch anything
::  else.
::
/-  spider
/+  strandio
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
  =+  .^  parent-yaki=yaki  %cs
          /(scot %p our)/[desk]/(scot %da now)/(scot %uv parent-tako)
      ==
  =/  after-deletes
    %-  ~(dif by q.parent-yaki)
    (malt (turn ~(tap in deletes) |=(=path [path *lobe])))
  %-  ~(uni by after-deletes)
  (~(run by new-blobs) |=(=blob p.blob))
::
::  XX should we get getting the time later, after all async?
;<  now=@da  bind:m  get-time:strandio
=/  new-yaki=yaki  (make-yaki ~[parent-tako] all-lobes now)
::
::  Apply new blobs and yaki to rang
::
=/  =rang
  :-  (~(put by hut:*rang) r.new-yaki new-yaki)
  (malt (turn ~(tap by new-blobs) |=([=path =blob] [p.blob blob])))
::
::  Checkout ankh
::
=/  =ankh  ank.dome
=.  ankh
  =/  dels  ~(tap in deletes)
  |-  ^-  ^ankh
  =*  outer-loop  $
  ?~  dels
    ankh
  |-  ^-  ^ankh
  =*  inner-loop  $
  ?~  i.dels
    outer-loop(dels t.dels, fil.ankh ~)
  %=    ankh
      dir
    %+  ~(put by dir.ankh)  i.i.dels
    %=  inner-loop
      i.dels  t.i.dels
      ankh    (~(gut by dir.ankh) i.i.dels *^ankh)
    ==
  ==
=.  ankh
  =/  blobs=(list [=path =blob])  ~(tap by new-blobs)
  |-  ^-  ^ankh
  =*  outer-loop  $
  ?~  blobs
    ankh
  =/  orig-path  path.i.blobs
  |-  ^-  ^ankh
  =*  inner-loop  $
  ?~  path.i.blobs
    %=    outer-loop
        blobs     t.blobs
        fil.ankh
      ?>  ?=(%direct -.blob.i.blobs)
      :+  ~  p.blob.i.blobs
      (~(got by cast-results) orig-path)
    ==
  %=    ankh
      dir
    %+  ~(put by dir.ankh)  i.path.i.blobs
    %=  inner-loop
      path.i.blobs  t.path.i.blobs
      ankh          (~(gut by dir.ankh) i.path.i.blobs *^ankh)
    ==
  ==
::
::  Checkout cache
::
=/  mim-builds=(map path schematic:ford)
  %-  ~(run by cast-results)
  |=  =cage
  [%cast [our desk] %mime %$ cage]
;<  mim-results=(map path cage)  bind:m  (build-cages:strandio mim-builds)
=/  can-mim=(map path (unit mime))
  %-  ~(run by mim-results)
  |=  =cage
  ?>  ?=(%mime p.cage)
  `!<(mime q.cage)
=/  del-mim=(map path (unit mime))
  (malt (turn ~(tap in deletes) |=(=path [path ~])))
=/  new-mim=(map path (unit mime))
  (~(uni by del-mim) can-mim)
::
::  Send to clay
::
=/  args  [desk r.new-yaki rang ankh new-mim]
;<  ~  bind:m  (send-raw-card:strandio %pass /commit/[desk] %arvo %c %park args)
(pure:m !>(~))
