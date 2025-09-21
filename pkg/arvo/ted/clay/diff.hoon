/-  spider
/+  strandio
=,  strand=strand:spider
=,  clay
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
|^
::  workaround to make the shallow flag optional. if it's specified we
::  do require a non-empty path - however this shouldn't be called with
::  empty paths to begin with.
=+  !<([~ =a=path b=$~([/hi &] $^([(lest @ta) flag] path))] arg)
=/  [b-path=path shallow=flag]  ?:(?=([^ *] b) b [`path`b |])
=/  a-beam  (need (de-beam a-path))
=/  b-beam  (need (de-beam b-path))
;<  a-domo=domo  bind:m  (get-from-clay a-beam domo %v)
;<  b-domo=domo  bind:m  (get-from-clay b-beam domo %v)
;<  diffs=(list diff-type)  bind:m  (diff-beams a-beam b-beam)
%-  pure:m
!>  ^-  tang
::  our tang is built in reverse order
%-  flop
?:  shallow
  (format-shallow diffs a-beam b-beam)
(format-deep diffs a-beam b-beam)
::
::  $diff-type: type for diffs produced.
::    1. %txt-diff is a standard diff (i.e. for hoon and txt files)
::    2. %directory-diff shows unique files between two directories
::    3. %other is for files that don't use txt-diff - we just take
::       the mug of the files
+$  diff-type
  :: paths of the diffed beams
  $:  a=path
      b=path
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
  ?:  ?=([%txt-diff *] +.+.d)
    :: levy produces & on empty lists
    %+  levy
      diff.d
    |=  u=(unce cord)
    ^-  flag
    -:u
  ?:  ?=([%directory-diff *] +.+.d)
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
  |=  [a=beam b=beam]
  =/  m  (strand ,(list diff-type))
  ^-  form:m
  ;<  hash-a=@  bind:m  (get-from-clay a @ %z)
  ;<  hash-b=@  bind:m  (get-from-clay b @ %z)
  ::  if the recursive hashes for each beam are the same we bail early
  ?:  =(hash-a hash-b)
    (pure:m *(list diff-type))
  ;<  a-arch=arch  bind:m  (get-from-clay a arch %y)
  ;<  b-arch=arch  bind:m  (get-from-clay b arch %y)
  ;<  file-diff=(unit diff-type)  bind:m  (diff-file-contents a a-arch b b-arch)
  ::  get distinct files along with shared files
  =/  a-keys=(set @t)  ~(key by dir.a-arch)
  =/  b-keys=(set @t)  ~(key by dir.b-arch)
  ::  unique children
  =/  a-unique-children=(set @t)  (~(dif in a-keys) b-keys)
  =/  b-unique-children=(set @t)  (~(dif in b-keys) a-keys)
  ;<  a-unique=(list path)  bind:m  (format-unique-children a a-arch a-unique-children)
  ;<  b-unique=(list path)  bind:m  (format-unique-children b b-arch b-unique-children)
  =/  unique-diff=diff-type  [s.a s.b %directory-diff a-unique b-unique]
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
      (pure:m acc)
    =/  child=@t  i.common-children
    =/  new-a=beam  a(s (snoc s.a child))
    =/  new-b=beam  b(s (snoc s.b child))
    ;<  diffs=(list diff-type)  bind:m
      (diff-beams new-a new-b)
    ::  ;< introduces another $ so we use "loop" instead.
    %=  loop
      acc  (weld diffs acc)
      common-children  t.common-children
    ==
  ;<  common-diffs=(list diff-type)  bind:m  (find-common-diffs)
  %-  pure:m
  ^-  (list diff-type)
  %+  skip
    ;:  weld
      (drop file-diff)
      [unique-diff ~]
      common-diffs
    ==
  diff-is-empty
  |%
  ::  +format-unique-children: produce list of paths representing
  ::  files that are unique within a directory.
  ::
  ++  format-unique-children
    |=  [bem=beam ark=arch children=(set @t)]
    =/  m  (strand ,(list path))
    ^-  form:m
    =/  children=(list @t)  ~(tap in children)
    =|  acc=(list path)
    |-
    =*  loop  $
    ^-  form:m
    ?~  children
      (pure:m acc)
    ::  the %t care gives all paths with the specified prefix
    ;<  res=(list path)  bind:m  (get-from-clay bem(s (snoc s.bem i.children)) (list path) %t)
    %=  loop
      acc  (weld res acc)
      children  t.children
    ==
  ::  +diff-file-contents: diff two files at specified beams,
  ::  producing a vase of (unit diff-type)
  ++  diff-file-contents
    =<
    |=  [a=beam a-arch=arch b=beam b-arch=arch]
    =/  m  (strand ,(unit diff-type))
    ^-  form:m
    ?:  =(fil.a-arch fil.b-arch)
      (pure:m *(unit diff-type))
    ?~  fil.a-arch
      :: only b has contents
      %-  pure:m
      ^-  (unit diff-type)
      %-  some
      :^    s.a
          s.b
        %txt-diff
      :_  ~
      ^-  (unce cord)
      :+  %|
        ~
      ~[(format-file-content-missing s.b q.b)]
    ?~  fil.b-arch
      :: only a has contents
      %-  pure:m
      ^-  (unit diff-type)
      %-  some
      :^    s.a
          s.b
        %txt-diff
      :_  ~
      ^-  (unce cord)
      :+  %|
        ~[(format-file-content-missing s.a q.a)]
      ~
    ::  have two file contents - check that they have
    ::  the same mark.
    =/  mar=mark  -:(flop s.a)
    ?:  !=(mar -:(flop s.b))
      (strand-fail:strandio %files-not-same-type >s.a< >s.b< ~)
    ;<  =a=cage     bind:m  (get-file a)
    ;<  =b=cage     bind:m  (get-file b)
    ;<  =dais:clay  bind:m  (build-mark:strandio -.a mar)
    ::  for txt-diff we produce an actual diff with type (urge cord).
    ::  for all other marks we just take the mug)
    %-  pure:m
    ?:  =(form:dais %txt-diff)
      ^-  (unit diff-type)
      %-  some
      :^    s.a
          s.b
        %txt-diff
      !<((urge cord) (~(diff dais q.a-cage) q.b-cage))
    ^-  (unit diff-type)
    %-  some
    :^    s.a
        s.b
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
::  +format-beams: helper to combine two beams into a tank
::
++  format-beams
  |=  [a=beam b=beam]
  ^-  tank
  [%rose [" " ~ ~] ~[(smyt (en-beam a)) (smyt (en-beam b))]]
::  +format-directory-diff: helper for producing a tank based on
::  a %directory-diff
::
++  format-directory-diff
  |=  [paths=(list path) =beam]
  ^-  tang
  =/  prefix=tape  (weld "only in " <(en-beam beam)>)
  %+  turn
    paths
  |=  p=path
  ^-  tank
  [%rose [": " ~ ~] [leaf+prefix (smyt p) ~]]
::  +format-shallow: converts a list of diff-type generated
::  between desks a and b into a tang in a shallow manner (just
::  listing files that differ).
::
++  format-shallow
|=  [diffs=(list diff-type) a=beam b=beam]
  ^-  tang
  %+  reel
    diffs
  |=  [d=diff-type acc=tang]
  ^-  tang
  ?:  ?=([%txt-diff *] +.+.d)
    [(format-beams a(s a.d) b(s b.d)) acc]
  ?:  ?=([%other *] +.+.d)
    [(format-beams a(s a.d) b(s b.d)) acc]
  ?:  ?=([%directory-diff *] +.+.d)
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
|=  [diffs=(list diff-type) a=beam b=beam]
  ^-  tang
  %+  reel
    diffs
  |=  [d=diff-type acc=tang]
  ^-  tang
  ?:  ?=([%txt-diff *] +.+.d)
    :+  (format-beams a(s a.d) b(s b.d))
      >diff.d<
    acc
  ?:  ?=([%directory-diff *] +.+.d)
    ;:  weld
      (format-directory-diff p.d a)
      (format-directory-diff q.d b)
      acc
    ==
  ?:  ?=([%other *] +.+.d)
    =/  a-tank=tank  (smyt (en-beam a(s a.d)))
    =/  b-tank=tank  (smyt (en-beam b(s b.d)))
    :+  [%rose [" " "files " ~] ~[a-tank b-tank]]
      [%rose [" and " "have mugs: " ~] ~[leaf+<p.d> leaf+<q.d>]]
    acc
  !!
--
