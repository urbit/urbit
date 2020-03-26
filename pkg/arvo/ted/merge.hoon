::  Merge second desk into first
::
/-  spider
/+  strandio, clay-commit
=,  strand=strand:spider
=,  clay
^-  thread:spider
|=  arg=vase
=+  !<([=bob=desk =ali=ship =ali=desk =germ ~] arg)
=/  m  (strand ,vase)
^-  form:m
;<  our=@p          bind:m  get-our:strandio
;<  wen=@da         bind:m  get-time:strandio
|^
::
::  Fetch current states
::
=/  start-path  /(scot %p our)/[bob-desk]/(scot %da wen)
;<  =ali=riot:clay  bind:m
  (warp:strandio ali-ship ali-desk `[%sing %v da+wen /])
?>  ?=(^ ali-riot)
=+  !<(=ali=dome q.r.u.ali-riot)
=/  ali-tako=tako  (~(got by hit.ali-dome) let.ali-dome)
=+  .^(=ali=yaki %cs (weld start-path /yaki/(scot %uv ali-tako)))
=+  .^(=bob=dome %cv start-path)
=/  bob-tako=tako  (~(got by hit.bob-dome) let.bob-dome)
=+  .^(=bob=yaki %cs (weld start-path /yaki/(scot %uv bob-tako)))
::
;<  =merge-result   bind:m  (merge ali-yaki bob-yaki)
?~  merge-result
  (pure:m !>(~))
=/  =rang
  [(malt [r .]:new.u.merge-result ~) lat.u.merge-result]
;<  [=ankh changes=(map path cage)]  bind:m
  (checkout bob-dome new.u.merge-result [deletes changes]:u.merge-result)
;<  mim=(map path (unit mime))  bind:m
  (checkout-cache:clay-commit bob-desk deletes.u.merge-result changes)
=/  args  [bob-desk r.new.u.merge-result rang ankh mim]
;<  ~  bind:m
  (send-raw-card:strandio %pass /merg/[bob-desk]/[ali-desk] %arvo %c %park args)
(pure:m !>(~))
::
+$  merge-result
      %-  unit
      $:  conflicts=(set path)
          bop=(map path cage)
          new=yaki
          deletes=(set path)
          changes=(set path)
          lat=(map lobe blob)
      ==
++  merge
  |=  [=ali=yaki =bob=yaki]
  |^
  =/  m  (strand ,merge-result)
  ^-  form:m
  ?-    germ
  ::
  ::  If this is an %init merge, we set the ali's commit to be bob's.
  ::
      %init
    %:  pure:m
      ~
      conflicts=~
      bop=~
      new=ali-yaki
      deletes=~
      changes=~(key by q.ali-yaki)
      lat=~
    ==
  ::
  ::  If this is a %this merge, we check to see if ali's and bob's commits
  ::  are the same, in which case we're done.  Otherwise, we check to see
  ::  if ali's commit is in the ancestry of bob's, in which case we're
  ::  done.  Otherwise, we create a new commit with bob's data plus ali
  ::  and bob as parents.
  ::
      %this
    ?:  =(r.ali-yaki r.bob-yaki)
      (pure:m ~)
    ?:  (~(has in (reachable-takos r.bob-yaki)) r.ali-yaki)
      (pure:m ~)
    =/  new-yaki  (make-yaki [r.bob-yaki r.ali-yaki ~] q.bob-yaki wen)
    %:  pure:m
      ~
      conflicts=~
      bop=~
      new=new-yaki
      deletes=~
      changes=~
      lat=~
    ==
  ::
  ::  If this is a %that merge, we check to see if ali's and bob's commits
  ::  are the same, in which case we're done.  Otherwise, we create a new
  ::  commit with ali's data plus ali and bob as parents.
  ::
      %that
    ?:  =(r.ali-yaki r.bob-yaki)
      (pure:m ~)
    =/  new-yaki  (make-yaki [r.bob-yaki r.ali-yaki ~] q.ali-yaki wen)
    %:  pure:m
      ~
      conflicts=~
      bop=~
      new=new-yaki
      deletes=get-deletes
      changes=get-changes
      lat=~
    ==
  ::
  ::  If this is a %fine merge, we check to see if ali's and bob's commits
  ::  are the same, in which case we're done.  Otherwise, we check to see
  ::  if ali's commit is in the ancestry of bob's, in which case we're
  ::  done.  Otherwise, we check to see if bob's commit is in the ancestry
  ::  of ali's.  If not, this is not a fast-forward merge, so we error
  ::  out.  If it is, we add ali's commit to bob's desk and checkout.
  ::
  ::
      %fine
    ?:  =(r.ali-yaki r.bob-yaki)
      (pure:m ~)
    ?:  (~(has in (reachable-takos r.bob-yaki)) r.ali-yaki)
      (pure:m ~)
    ?.  (~(has in (reachable-takos r.ali-yaki)) r.bob-yaki)
      (strand-fail:strandio %bad-fine-merge ~)
    %:  pure:m
      ~
      conflicts=~
      bop=~
      new=ali-yaki
      deletes=get-deletes
      changes=get-changes
      lat=~
    ==
  ::
      ?(%meet %mate %meld)
    ~&  %merge-not-implemented
    !!
  ==
  ::
  ++  reachable-takos
    |=  tak=tako
    ^-  (set tako)
    =/  start-path  /(scot %p our)/[bob-desk]/(scot %da wen)
    |-  ^-  (set tako)
    =+  .^(=yaki %cs (weld start-path /yaki/(scot %uv tak)))
    %+  roll  p.yaki
    =<  .(takos (~(put in *(set tako)) tak))
    |=  [q=tako takos=(set tako)]
    ?:  (~(has in takos) q)                             ::  already done
      takos                                             ::  hence skip
    (~(uni in takos) ^$(tak q))                         ::  otherwise traverse
  ::
  ++  get-deletes
    %-  silt  ^-  (list path)
    %+  murn  ~(tap by (~(uni by q.bob-yaki) q.ali-yaki))
    |=  [=path =lobe]
    ^-  (unit ^path)
    =/  a  (~(get by q.ali-yaki) path)
    =/  b  (~(get by q.bob-yaki) path)
    ?:  |(=(a b) !=(~ a))
      ~
    `path
  ::
  ++  get-changes
    %-  silt  ^-  (list path)
    %+  murn  ~(tap by (~(uni by q.bob-yaki) q.ali-yaki))
    |=  [=path =lobe]
    ^-  (unit ^path)
    =/  a  (~(get by q.ali-yaki) path)
    =/  b  (~(get by q.bob-yaki) path)
    ?:  |(=(a b) =(~ a))
      ~
    `path
  --
::
++  checkout
  |^
  |=  [=bob=dome =yaki deletes=(set path) changes=(set path)]
  =/  m  (strand ,[ankh (map path cage)])
  ^-  form:m
  =/  start-path  /(scot %p our)/[bob-desk]/(scot %da wen)
  =/  builds
    %-  malt
    %+  turn  ~(tap in changes)
    |=  =path
    [path (lobe-to-schematic start-path (~(got by q.yaki) path))]
  ;<  results=(map path cage)  bind:m  (build-cages:strandio builds)
  (pure:m (checkout:clay-commit ank.bob-dome deletes results) results)
  ::
  ++  lobe-to-schematic
    |=  [=start=path =lobe]
    ^-  schematic:ford
    =+  .^(=blob %cs (weld start-path /blob/(scot %uv lobe)))
    =/  =disc:ford  [our bob-desk]
    ?-  -.blob
      %direct  (page-to-schematic disc q.blob)
      %delta   [%pact disc $(lobe q.q.blob) (page-to-schematic disc r.blob)]
    ==
  ::
  ++  page-to-schematic
    |=  [=disc:ford =page]
    ?.  ?=(%hoon p.page)
      [%volt disc page]
    [%$ p.page [%atom %t ~] q.page]
  --
--
