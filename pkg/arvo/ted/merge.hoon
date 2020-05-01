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
(pure:m !>(conflicts.u.merge-result))
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
      =/  ali-diffs=cane  (diff-base ali-yaki bob-yaki merge-point)
      =/  bob-diffs=cane  (diff-base bob-yaki ali-yaki merge-point)
      =/  bof=(map path (unit cage))
        (merge-conflicts can.ali-diffs can.bob-diffs)
      =/  [conflicts=(set path) new=yoki lat=(map lobe blob)]
        (build ali-yaki bob-yaki merge-point ali-diffs bob-diffs bof)
      (pure:m `[conflicts new lat])
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
  ::  The set of changes between the mergebase and one of the desks
  ::  being merged
  ::
  ::  --  `new` is the set of files in the new desk and not in the
  ::  mergebase.
  ::  --  `cal` is the set of changes in the new desk from the mergebase
  ::  except for any that are also in the other new desk.
  ::  --  `can` is the set of changes in the new desk from the mergebase
  ::  that are also in the other new desk (potential conflicts).
  ::  --  `old` is the set of files in the mergebase and not in the new
  ::  desk.
  ::
  +$  cane
    $:  new=(map path lobe)
        cal=(map path lobe)
        can=(map path cage)
        old=(map path ~)
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
  ::  Diff yak against bas where different from yuk
  ::
  ++  diff-base
    |=  [yak=yaki yuk=yaki bas=yaki]
    ^-  cane
    =/  new=(map path lobe)
      %-  malt
      %+  skip  ~(tap by q.yak)
      |=  [=path =lobe]
      (~(has by q.bas) path)
    ::
    =/  cal=(map path lobe)
      %-  malt  ^-  (list [path lobe])
      %+  murn  ~(tap by q.bas)
      |=  [pax=path lob=lobe]
      ^-  (unit (pair path lobe))
      =+  a=(~(get by q.yak) pax)
      =+  b=(~(get by q.yuk) pax)
      ?.  ?&  ?=(^ a)
              !=([~ lob] a)
              =([~ lob] b)
          ==
        ~
      `[pax +.a]
    ::
    =/  can=(map path cage)
      %-  malt
      %+  murn  ~(tap by q.bas)
      |=  [=path =lobe]
      ^-  (unit [^path cage])
      =/  in-yak  (~(get by q.yak) path)
      ?~  in-yak
        ~
      ?:  =(lobe u.in-yak)
        ~
      =/  in-yuk  (~(get by q.yuk) path)
      ?~  in-yuk
        ~
      ?:  =(lobe u.in-yuk)
        ~
      ?:  =(u.in-yak u.in-yuk)
        ~
      `[path (diff-lobes lobe u.in-yak)]
    ::
    =/  old=(map path ~)
        %-  malt  ^-  (list [path ~])
        %+  murn  ~(tap by q.bas)
        |=  [pax=path lob=lobe]
        ?.  =(~ (~(get by q.yak) pax))
          ~
        (some pax ~)
    ::
    [new cal can old]
  ::
  ++  start  ~+  `path`/(scot %p our)/[bob-desk]/(scot %da wen)
  ++  cb     ~+  |=(=mark .^(dais %cb (weld start /[mark])))
  ::
  ::  Diff two files on bob-desk
  ::
  ++  diff-lobes
    |=  [=a=lobe =b=lobe]
    ^-  cage
    =+  .^(=a=cage %cs (weld start /cage/(scot %uv a-lobe)))
    =+  .^(=b=cage %cs (weld start /cage/(scot %uv b-lobe)))
    ?>  =(p.a-cage p.b-cage)
    =/  =dais  (cb p.a-cage)
    [form:dais (~(diff dais q.a-cage) q.b-cage)]
  ::
  ::  Merge diffs that are on the same file.
  ::
  ++  merge-conflicts
    |=  [ali-conflicts=(map path cage) bob-conflicts=(map path cage)]
    ^-  (map path (unit cage))
    %-  ~(urn by (~(int by ali-conflicts) bob-conflicts))
    |=  [=path *]
    ^-  (unit cage)
    =/  cal=cage  (~(got by ali-conflicts) path)
    =/  cob=cage  (~(got by bob-conflicts) path)
    =/  =mark
      =+  (slag (dec (lent path)) path)
      ?~(- %$ i.-)
    =+  .^(=dais %cb (weld start /[mark]))
    =/  res=(unit (unit vase))  (~(join dais bunt:dais) q.cal q.cob)
    ?~  res
      ~
    ?~  u.res
      ~
    `[form:dais u.u.res]
  ::
  ::  Apply the patches in bof to get the new merged content.
  ::
  ::  Gather all the changes between ali's and bob's commits and the
  ::  mergebase.  This is similar to the %meet of ++merge, except
  ::  where they touch the same file, we use the merged versions.
  ::
  ++  build
    |=  $:  ali=yaki
            bob=yaki
            bas=yaki
            dal=cane
            dob=cane
            bof=(map path (unit cage))
        ==
    ^-  [conflicts=(set path) new=yoki lat=(map lobe blob)]
    =/  both-patched=(map path cage)
      %-  malt
      %+  murn  ~(tap by bof)
      |=  [=path cay=(unit cage)]
      ^-  (unit [^path cage])
      ?~  cay
        ~
      :+  ~  path
      =+  (~(get by q.bas) path)
      ?~  -
        ~|  %mate-strange-diff-no-base
        !!
      =+  .^(=a=cage %cs (weld start /cage/(scot %uv u.-)))
      =/  =dais  (cb p.a-cage)
      ?>  =(p.u.cay form.dais)
      :-  p.a-cage
      (~(pact dais q.a-cage) q.u.cay)
    =/  con=(map path *)                            ::  2-change conflict
      %-  molt
      %+  skim  ~(tap by bof)
      |=([pax=path cay=(unit cage)] ?=(~ cay))
    =/  cab=(map path lobe)                         ::  conflict base
      %-  ~(urn by con)
      |=  [pax=path *]
      (~(got by q.bas) pax)
    =.  con                                         ::  change+del conflict
      %-  ~(uni by con)
      %-  malt  ^-  (list [path *])
      %+  skim  ~(tap by old.dal)
      |=  [pax=path ~]
      ?:  (~(has by new.dob) pax)
        ~|  %strange-add-and-del
        !!
      (~(has by can.dob) pax)
    =.  con                                         ::  change+del conflict
      %-  ~(uni by con)
      %-  malt  ^-  (list [path *])
      %+  skim  ~(tap by old.dob)
      |=  [pax=path ~]
      ?:  (~(has by new.dal) pax)
        ~|  %strange-del-and-add
        !!
      (~(has by can.dal) pax)
    =.  con                                         ::  add+add conflict
      %-  ~(uni by con)
      %-  malt  ^-  (list [path *])
      %+  skip  ~(tap by (~(int by new.dal) new.dob))
      |=  [pax=path *]
      =((~(got by new.dal) pax) (~(got by new.dob) pax))
    =/  old=(map path lobe)                         ::  oldies but goodies
      %+  roll  ~(tap by (~(uni by old.dal) old.dob))
      =<  .(old q.bas)
      |=  [[pax=path ~] old=(map path lobe)]
      (~(del by old) pax)
    =/  [hot=(map path lobe) lat=(map lobe blob)]   ::  new content
      %+  roll  ~(tap by both-patched)
      |=  [[pax=path cay=cage] hat=(map path lobe) lat=(map lobe blob)]
      =/  =blob  [%direct (page-to-lobe [p q.q]:cay) [p q.q]:cay]
      :-  (~(put by hat) pax p.blob)
      ?:  (~(has by lat) p.blob)
        lat
      (~(put by lat) p.blob blob)
    =/  hat=(map path lobe)                         ::  all the content
      %-  ~(uni by old)
      %-  ~(uni by new.dal)
      %-  ~(uni by new.dob)
      %-  ~(uni by cal.dal)
      %-  ~(uni by cal.dob)
      %-  ~(uni by hot)
      cab
    =/  del=(map path ?)
        (~(run by (~(uni by old.dal) old.dob)) |=(~ %|))
    =/  new  &+[[r.bob r.ali ~] (~(run by hat) |=(=lobe |+lobe))]
    :*  (silt (turn ~(tap by con) head))
        new
        lat
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
