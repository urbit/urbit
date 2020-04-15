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
=/  bob-yaki=(unit yaki)
  ?~  let.bob-dome
    ~
  =/  bob-tako=tako  (~(got by hit.bob-dome) let.bob-dome)
  `.^(=bob=yaki %cs (weld start-path /yaki/(scot %uv bob-tako)))
::
;<  =merge-result   bind:m  (merge ali-yaki bob-yaki)
?~  merge-result
  (pure:m !>(~))
=/  =rang
  [(malt [r .]:new.u.merge-result ~) lat.u.merge-result]
;<  [=ankh changes=(map path cage)]  bind:m
  (checkout bob-dome new.u.merge-result [deletes changes]:u.merge-result)
;<  mim=(map path (unit mime))  bind:m
  (checkout-cache:clay-commit ali-ship ali-desk deletes.u.merge-result changes)
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
::
++  merge
  |=  [=ali=yaki bob-yaki=(unit yaki)]
  =/  m  (strand ,merge-result)
  ^-  form:m
  ?:  ?=(%init germ)
  ::
  ::  If this is an %init merge, we set the ali's commit to be bob's.
  ::
    %:  pure:m
      ~
      conflicts=~
      bop=~
      new=ali-yaki
      deletes=~
      changes=~(key by q.ali-yaki)
      lat=~
    ==
  =/  bob-yaki  (need bob-yaki)
  |^
  ?-    germ
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
      %^  error  %bad-fine-merge
        leaf+"tried fast-forward but is not ancestor or descendant"
      ~
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
    ?:  =(r.ali-yaki r.bob-yaki)
      (pure:m ~)
    ?:  (~(has in (reachable-takos r.bob-yaki)) r.ali-yaki)
      (pure:m ~)
    ?:  (~(has in (reachable-takos r.ali-yaki)) r.bob-yaki)
      $(germ %fine)
    =/  merge-points  find-merge-points
    ?~  merge-points
      %^  error  %merge-no-merge-base
        leaf+"consider a %this or %that merge to get a mergebase"
      ~
    =/  merge-point=yaki  n.merge-points
    ?.  ?=(%meet germ)
      ~&  %merge-not-implemented
      !!
    =/  ali-diffs=cane  (calc-diffs ali-yaki merge-point)
    =/  bob-diffs=cane  (calc-diffs bob-yaki merge-point)
    =/  both-diffs=(map path *)
      %-  %~  int  by
          %-  ~(uni by `(map path *)`new.ali-diffs)
          %-  ~(uni by `(map path *)`cal.ali-diffs)
          %-  ~(uni by `(map path *)`can.ali-diffs)
          `(map path *)`old.ali-diffs
      %-  ~(uni by `(map path *)`new.bob-diffs)
      %-  ~(uni by `(map path *)`cal.bob-diffs)
      %-  ~(uni by `(map path *)`can.bob-diffs)
      `(map path *)`old.bob-diffs
    ?.  =(~ both-diffs)
      %:  error  %meet-conflict  >~(key by both-diffs)<
        leaf+"consider a %mate merge"  ~
      ==
    =/  not-deleted=(map path lobe)
      %+  roll  ~(tap by (~(uni by old.ali-diffs) old.bob-diffs))
      =<  .(not-deleted q.merge-point)
      |=  [[pax=path ~] not-deleted=(map path lobe)]
      (~(del by not-deleted) pax)
    =/  hat=(map path lobe)
      %-  ~(uni by not-deleted)
      %-  ~(uni by new.ali-diffs)
      %-  ~(uni by new.bob-diffs)
      %-  ~(uni by cal.ali-diffs)
      cal.bob-diffs
    %:  pure:m
      ~
      conflicts=~
      bop=~
      new=(make-yaki [r.bob-yaki r.ali-yaki ~] hat wen)
      deletes=get-deletes
      changes=get-changes
      lat=~
    ==
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
  ::
  ::  Find the most recent common ancestor(s).
  ::
  ::    Pretty sure this could be a lot more efficient.
  ::
  ++  find-merge-points
    ^-  (set yaki)
    =/  start-path  /(scot %p our)/[bob-desk]/(scot %da wen)
    %-  reduce-merge-points
    =+  r=(reachable-takos r.ali-yaki)
    |-  ^-  (set yaki)
    ?:  (~(has in r) r.bob-yaki)  (~(put in *(set yaki)) bob-yaki)
    %+  roll  p.bob-yaki
    |=  [t=tako s=(set yaki)]
    ?:  (~(has in r) t)
      (~(put in s) .^(yaki %cs (weld start-path /yaki/(scot %uv t))))
    (~(uni in s) ^$(bob-yaki .^(yaki %cs (weld start-path /yaki/(scot %uv t)))))
  ::
  ::  Eliminate redundant merge-point candidates
  ::
  ++  reduce-merge-points
    |=  unk=(set yaki)
    =|  gud=(set yaki)
    =/  zar=(map tako (set tako))
      %+  roll  ~(tap in unk)
      |=  [yak=yaki qar=(map tako (set tako))]
      (~(put by qar) r.yak (reachable-takos r.yak))
    |-
    ^-  (set yaki)
    ?~  unk  gud
    =+  bun=(~(del in `(set yaki)`unk) n.unk)
    ?:  %+  levy  ~(tap by (~(uni in gud) bun))
        |=  yak=yaki
        !(~(has in (~(got by zar) r.yak)) r.n.unk)
      $(gud (~(put in gud) n.unk), unk bun)
    $(unk bun)
  ::
  ::  The set of changes between the mergebase and one of the desks being merged
  ::
  ::  --  `new` is the set of files in the new desk and not in the mergebase.
  ::  --  `cal` is the set of changes in the new desk from the mergebase except
  ::      for any that are also in the other new desk.
  ::  --  `can` is the set of changes in the new desk from the mergebase that
  ::      are also in the other new desk (potential conflicts).
  ::  --  `old` is the set of files in the mergebase and not in the new desk.
  ::
  +$  cane
    $:  new/(map path lobe)
        cal/(map path lobe)
        can/(map path cage)
        old/(map path ~)
    ==
  ::
  ::  Calculate cane knowing there are no files changed by both desks
  ::
  ++  calc-diffs
    |=  [hed=yaki bas=yaki]
    ^-  cane
    :*  %-  molt
        %+  skip  ~(tap by q.hed)
        |=  [pax=path lob=lobe]
        (~(has by q.bas) pax)
      ::
        %-  molt
        %+  skip  ~(tap by q.hed)
        |=  [pax=path lob=lobe]
        =+  (~(get by q.bas) pax)
        |(=(~ -) =([~ lob] -))
      ::
        ~
      ::
        %-  malt  ^-  (list [path ~])
        %+  murn  ~(tap by q.bas)
        |=  [pax=path lob=lobe]
        ^-  (unit (pair path ~))
        ?.  =(~ (~(get by q.hed) pax))
          ~
        `[pax ~]
    ==
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
    =/  =disc:ford  [ali-ship ali-desk]
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
::
++  error
  |=  [=term =tang]
  %:  strand-fail:strandio  term
    leaf+"failed merge from {<ali-ship>}/{<ali-desk>} to {<our>}/{<bob-desk>}"
    leaf+"with strategy germ"
    tang
  ==
--
