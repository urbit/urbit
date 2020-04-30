::  Merge second desk into first
::
/-  spider
/+  strandio
=,  strand=strand:spider
=,  clay
^-  thread:spider
|=  arg=vase
=+  !<([=bob=desk =ali=ship =ali=desk =germ ~] arg)
=/  m  (strand ,vase)
^-  form:m
;<  our=@p          bind:m  get-our:strandio
;<  now=@da         bind:m  get-time:strandio
::
::  Fetch current states
::
=/  start-path  /(scot %p our)/[bob-desk]/(scot %da now)
;<  =ali=riot:clay  bind:m
  (warp:strandio ali-ship ali-desk `[%sing %v da+now /])
::
;<  wen=@da         bind:m  get-time:strandio
|^
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
=/  =rang  [~ lat.u.merge-result]
=/  args  [bob-desk new.u.merge-result rang]
;<  ~  bind:m
  (send-raw-card:strandio %pass /merg/[bob-desk]/[ali-desk] %arvo %c %park args)
(pure:m !>(~))
::
+$  merge-result
  %-  unit
  $:  conflicts=(set path)
      new=yoki
      lat=(map lobe blob)
  ==
::
++  merge
  |=  [=ali=yaki bob-yaki=(unit yaki)]
  =/  m  (strand ,merge-result)
  ^-  form:m
  ::
  ::  If this is an %init merge, we set the ali's commit to be bob's.
  ::
  ?:  ?=(%init germ)
    (pure:m ~ conflicts=~ new=|+ali-yaki lat=~)
  ::
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
    %:  pure:m
      ~
      conflicts=~
      new=&+[[r.bob-yaki r.ali-yaki ~] (to-yuki q.bob-yaki)]
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
    %:  pure:m
      ~
      conflicts=~
      new=&+[[r.bob-yaki r.ali-yaki ~] (to-yuki q.ali-yaki)]
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
    (pure:m ~ conflicts=~ new=|+ali-yaki lat=~)
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
      new=&+[[r.bob-yaki r.ali-yaki ~] (to-yuki hat)]
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
  ::
  ++  to-yuki
    |=  m=(map path lobe)
    ^-  (map path (each page lobe))
    (~(run by m) |=(=lobe |+lobe))
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
