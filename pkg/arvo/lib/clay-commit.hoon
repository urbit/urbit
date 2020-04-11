/-  spider
/+  strandio
=,  strand=strand:spider
=,  clay
|%
::  Produce an ankh
::
++  checkout
  |=  [=ankh deletes=(set path) changes=(map path cage)]
  ^-  ^ankh
  ::  Delete
  ::
  =.  ankh
    =/  dels  ~(tap in deletes)
    |-  ^-  ^ankh
    =*  outer-loop  $
    ?~  dels
      ankh
    =.  ankh
      |-  ^-  ^ankh
      =*  inner-loop  $
      ?~  i.dels
        ankh(fil ~)
      %=    ankh
          dir
        %+  ~(put by dir.ankh)  i.i.dels
        %=  inner-loop
          i.dels  t.i.dels
          ankh    (~(gut by dir.ankh) i.i.dels *^ankh)
        ==
      ==
    outer-loop(dels t.dels)
  ::  Add/change
  ::
  =/  cans=(list [=path =cage])  ~(tap by changes)
  |-  ^-  ^ankh
  =*  outer-loop  $
  ?~  cans
    ankh
  =.  ankh
    =/  orig-path  path.i.cans
    |-  ^-  ^ankh
    =*  inner-loop  $
    ?~  path.i.cans
      %=    ankh
          fil
        `[(page-to-lobe [p q.q]:cage.i.cans) cage.i.cans]
      ==
    %=    ankh
        dir
      %+  ~(put by dir.ankh)  i.path.i.cans
      %=  inner-loop
        path.i.cans  t.path.i.cans
        ankh         (~(gut by dir.ankh) i.path.i.cans *^ankh)
      ==
    ==
  outer-loop(cans t.cans)
::  Produce a mime cache
::
++  checkout-cache
  |=  [=ship =desk deletes=(set path) changes=(map path cage)]
  =/  m  (strand ,(map path (unit mime)))
  ^-  form:m
  ;<  our=@p  bind:m  get-our:strandio
  =/  mim-builds=(map path schematic:ford)
    %-  ~(run by changes)
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
  (pure:m new-mim)
--
