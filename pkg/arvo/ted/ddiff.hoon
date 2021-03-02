/-  spider
/+  strandio
=,  strand=strand:spider
=,  clay
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
|^
=+  !<([~ =a=path =b=path shallow=flag] arg)
=/  a-beam  (need (de-beam a-path))
=/  b-beam  (need (de-beam b-path))
;<  a-dome=dome  bind:m  (get-from-clay a-beam dome %v)
;<  b-dome=dome  bind:m  (get-from-clay b-beam dome %v)
;<  diff=vase  bind:m  (diff-beams a-beam ank.a-dome b-beam ank.b-dome)
=/  diffs=(list diff-type)  !<((list diff-type) diff)
%-  pure:m
!>  ^-  tang
?:  shallow
  (format-shallow diffs q.a-beam q.b-beam)
(format-deep diffs q.a-beam q.b-beam)
::
::  $diff-type: type for diffs produced.
::    1. %txt-diff is a standard diff (i.e. for hoon and txt files)
::    2. %directory-diff shows unique files between two directories
::    3. %other is for files that don't use txt-diff - we just take
::       the mug of the files
+$  diff-type
  :: path of the diffed beams
  $:  pax=path
      $%  [%txt-diff diff=(urge cord)]
          [%directory-diff p=(list path) q=(list path)]
          [%other p=@ q=@]
      ==
  ==
::  +diff-is-empty: check if a diff is empty (for two identical files)
::
++  diff-is-empty
  |=  d=diff-type
    ^-  flag
    ?:  ?=([%txt-diff *] +.d)
      :: levy produces & on empty lists
      %+  levy
        diff.d
      |=  u=(unce cord)
      ^-  flag
      -:u
    ?:  ?=([%directory-diff *] +.d)
      =(p.d q.d)
    =(p.d q.d)
::  +get-file: retrieve a cage of a file from clay
::
++  get-file
  |=  =beam
  =/  m  (strand ,cage)
  ^-  form:m
  ;<  =riot:clay  bind:m
    (warp:strandio p.beam q.beam ~ %sing %x r.beam s.beam)
  ?~  riot
    (strand-fail:strandio %file-not-found >s.beam< ~)
  (pure:m r.u.riot)
::  +get-from-clay: retrieve other data from clay based on care
::
++  get-from-clay
  |*  [=beam mol=mold =care]
  =/  m  (strand ,mol)
  ^-  form:m
  ;<  =riot:clay  bind:m
    (warp:strandio p.beam q.beam ~ %sing care r.beam s.beam)
  ?~  riot
    (strand-fail:strandio %file-not-found >s.beam< ~)
  (pure:m !<(mol q.r.u.riot))
::  +diff-beams: recursively diff two beams. produces a vase
::  of (list diff-type)
::
++  diff-beams
  =<
  |=  [a=beam a-ankh=ankh b=beam b-ankh=ankh]
  =/  m  (strand ,vase)
  ^-  form:m
  ?>  =(s.a s.b)
  ;<  hash-a=@  bind:m  (get-from-clay a @ %z)
  ;<  hash-b=@  bind:m  (get-from-clay b @ %z)
  ::  if the recursive hashes for each beam are the same we bail early
  ?:  =(hash-a hash-b)
    (pure:m !>(*(list diff-type)))
  ::  vase of a (unit diff-type)
  ;<  file-diff=vase  bind:m  (diff-file-contents a a-ankh b b-ankh)
  ::  get distinct files along with shared files
  =/  a-keys=(set @t)  ~(key by dir.a-ankh)
  =/  b-keys=(set @t)  ~(key by dir.b-ankh)
  ::  unique children
  =/  a-unique-children=(set @t)  (~(dif in a-keys) b-keys)
  =/  b-unique-children=(set @t)  (~(dif in b-keys) a-keys)
  =/  a-unique=(list path)  (format-unique-children a a-ankh a-unique-children)
  =/  b-unique=(list path)  (format-unique-children b b-ankh b-unique-children)
  =/  unique-diff=diff-type  [s.a %directory-diff a-unique b-unique]
  ::  shared children
  =/  find-common-diffs
    |.
    ^-  form:m
    =|  acc=(list diff-type)
    =/  common-children=(list @t)  ~(tap in (~(int in a-keys) b-keys))
    |-
    =*  loop  $
    ^-  form:m
    ?~  common-children
      (pure:m !>(acc))
    =/  child=@t  i.common-children
    =/  new-a=beam  a(s (snoc s.a child))
    =/  new-b=beam  b(s (snoc s.b child))
    ;<  res=vase  bind:m
      (diff-beams new-a (~(got by dir.a-ankh) child) new-b (~(got by dir.b-ankh) child))
    ::  ;< introduces another $ so we use "loop" instead.
    %=  loop
      acc  (weld !<((list diff-type) res) acc)
      common-children  t.common-children
    ==
  ;<  common-diffs=vase  bind:m  (find-common-diffs)
  %-  pure:m
  !>  ^-  (list diff-type)
  %+  skip
    ;:  weld
      (drop !<((unit diff-type) file-diff))
      [unique-diff ~]
      !<((list diff-type) common-diffs)
    ==
  diff-is-empty
  |%
  ::  +format-unique-children: produce list of paths representing
  ::  files that are unique within a directory.
  ::
  ++  format-unique-children
    =<
    |=  [bem=beam ank=ankh children=(set @t)]
    ^-  (list path)
    %-  ~(rep by dir.ank)
    |=  [[key=@t node=ankh] acc=(list path)]
    ^-  (list path)
    ?.  (~(has in children) key)
      acc
    (weld (get-all-paths node (snoc s.bem key)) acc)
    |%
    ++  get-all-paths
      |=  [ank=ankh cur-path=path]
      ^-  (list path)
      ::  check for terminal node
      ?:  =(~(wyt by dir.ank) 0)
        ~[cur-path]
      %-  ~(rep by dir.ank)
      |=  [[key=@t node=ankh] acc=(list path)]
      ^-  (list path)
      =/  new-path=path  (snoc cur-path key)
      =/  child-paths=(list path)  (get-all-paths node new-path)
      (weld child-paths acc)
    --
  ::  +diff-file-contents: diff two files at specified beams,
  ::  producing a vase of (unit diff-type)
  ++  diff-file-contents
    =<
    |=  [a=beam a-ankh=ankh b=beam b-ankh=ankh]
    =/  m  (strand ,vase)
    ^-  form:m
    ::  the files must be at the same path
    ?>  =(s.a s.b)
    ?:  =(fil.a-ankh fil.b-ankh)
      (pure:m !>(*(unit diff-type)))
    ?~  fil.a-ankh
      :: only b has contents
      %-  pure:m
      !>  ^-  (unit diff-type)
      %-  some
      :+  s.a
        %txt-diff
      :_  ~
      ^-  (unce cord)
      :+  %|
        ~
      ~[(format-file-content-missing s.b q.b)]
    ?~  fil.b-ankh
      :: only a has contents
      %-  pure:m
      !>  ^-  (unit diff-type)
      %-  some
      :+  s.a
        %txt-diff
      :_  ~
      ^-  (unce cord)
      :+  %|
        ~[(format-file-content-missing s.a q.a)]
      ~
    ::  have two file contents - diff them
    =/  mar=mark  -:(flop s.a)
    ;<  =a=cage     bind:m  (get-file a)
    ;<  =b=cage     bind:m  (get-file b)
    ;<  =dais:clay  bind:m  (build-mark:strandio -.a mar)
    ::  for txt-diff we produce an actual diff with type (urge cord).
    ::  for all other marks we just take the mug)
    %-  pure:m
    ?:  =(form:dais %txt-diff)
      !>  ^-  (unit diff-type)
      %-  some
      :+  s.a
        %txt-diff
      !<((urge cord) (~(diff dais q.a-cage) q.b-cage))
    !>  ^-  (unit diff-type)
    %-  some
    :+  s.a
      %other
    ::  For some reason, vases for identical files on different desks
    ::  can sometimes have different types. for this reason, we only
    ::  take the mug of the data.
    [(mug q.q.a-cage) (mug q.q.b-cage)]
    |%
    ++  format-file-content-missing
      |=  [p=path d=desk]
      ^-  cord
      %-  crip
      ;:  weld
        "only "
        <p>
        " in desk "
        <d>
        " has file contents"
      ==
    --
  --
::  +format-directory-diff: helper for producing a tank based on
::  a %directory-diff
::
++  format-directory-diff
  |=  [paths=(list path) =desk]
  ^-  tang
  =/  prefix=tape  ;:(weld "only in " (trip desk) ": ")
  (turn paths |=(p=path >(weld prefix <p>)<))
::  +format-shallow: converts a list of diff-type generated
::  between desks a and b into a tang in a shallow manner (just
::  listing files that differ).
::
++  format-shallow
|=  [diffs=(list diff-type) a=desk b=desk]
  ^-  tang
  %+  reel
    diffs
  |=  [d=diff-type acc=tang]
  ^-  tang
  ?:  ?=([%txt-diff *] +.d)
    [`tank`>pax.d< acc]
  ?:  ?=([%other *] +.d)
    [`tank`>pax.d< acc]
  ?:  ?=([%directory-diff *] +.d)
    ;:  weld
      (format-directory-diff p.d a)
      (format-directory-diff q.d b)
      acc
    ==
  !!
::  +format-deep: converts a list of diff-type generated
::  between desks a and b into a tang in a deep manner (preserving
::  diff information for files)
++  format-deep
|=  [diffs=(list diff-type) a=desk b=desk]
  ^-  tang
  %+  reel
    diffs
  |=  [d=diff-type acc=tang]
  ^-  tang
  ?:  ?=([%txt-diff *] +.d)
    ::  TODO come up with some more readable formatting
    ::  super ugly when printed.
    :-  >pax.d<
    %+  weld
      (turn diff.d |=(u=(unce cord) >u<))
    acc
  ?:  ?=([%directory-diff *] +.d)
    ;:  weld
      (format-directory-diff p.d a)
      (format-directory-diff q.d b)
      acc
    ==
  ?:  ?=([%other *] +.d)
    :_  acc
    :-  %leaf
    ;:  weld
      "file "
      <pax.d>
      " has mug "
      <p.d>
      " on desk "
      <a>
      " but mug "
      <q.d>
      " on desk "
      <b>
    ==
  !!
--
