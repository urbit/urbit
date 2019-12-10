::
::  # Type Analysis
::
::  This does analysis on types to produce an `ximage` value, which can
::  be used to print the type (with `ximage-to-spec`) or to print values
::  of that type (using the `libpprint` library).  You should understand
::  the `ximage` type before digging further.
::
::  `xray-type` is the main gate of interest here. It's implemented as a
::  series of passes:
::
::  - `analyze-type`: This takes a `type`, which is a lazily-evaluated,
::     recursive xdat structure, and converts it into an explicit
::     graph. It also collect the information from `%hint` types and
::     decorates the graph nodes with that.
::
::  - `cleanup`: Removes `%pntr` nodes, replacing references to them
::    with references to what they resolve to.
::
::  - `decorate-ximage-with-loops`: Determines which nodes reference
::    themselves recursively.
::
::  - `decorate-ximage-with-xpats`: Adds printing heuristics to types:
::    "Should this be printed as a list?"
::
::  - `decorate-ximage-with-xshapes`: Determines the loose shape of each
::    type. This overlaps with, and is used by, the next pass. Doing
::    this as a separate pass removes a lot of difficult edge-cases when
::    determining the `xrole` of cell-types.
::
::  - `decorate-ximage-with-xroles`: Restructures forks to make them
::    coherent. This is important both for printing types (we want to use
::    `$@` `$%` `$%`, etc) and for printing xdat (we need an efficient
::    way to determine which branch of a fork matches a value.
::
::  # Todos
::
::  - XX It seems (have'nt verified this) that there's a lot of things
::    that are forks that, once void types have been factored out,
::    only actually refer to one thing. It would be nice to discover
::    things of this kind and replace such fork node with the thing the
::    actualy resolve to.
::
::    The reason I think this is what's happening is that I see lots
::    of %unexpected-fork-xrole messages when converting the kernel type
::    to a spec, and those xroles have things like %tall and %atom.
::    However! The `combine` function never produces anything with
::    those xroles.
::
::  - XX Create xpats and matchers %map %set.
::
::  - XX Create xpats and matchers for tuples. There's no need to
::    recreate this structure in the printing code, and that's what we're
::    doing now.
::
::  - XX The xpat of an xray could be computed on demand instead of
::    up-front. Possibly a lot faster!
::
::  - XX The loop-detection of an xray could be done on demand instead
::    of up-front. Possibly a lot faster!
::
::  - XX The xpat matching code is basically brute-force.
::
::    If it turns out to be a performance bottleneck, there's lots of
::    low-hanging fruit there. For example:
::
::    - Faces repeat the work done for the type they reference.
::    - When detecting whether a cell is part of an "informal" list,
::      we recurse into the tail repeatedly. For example, the following
::      example will do the "formal-list" test 3 times:
::
::      - `[1 2 `(list @)`~]`
::
::  - XX Try to find a way to drop the `%pntr` constructor from
::    `%xdat`. The consumer of an `xray` does not care.
::
::  - XX Actually, it would also be really nice to produce another
::    version of this structure that doesn't have the (unit *) wrapper around
::    everything interesting. This would make the on-demand computation
::    of various things hard, though.
::
::  - XX Simply lying about the type of deep arms is not robust. I am just
::    claiming that they are nouns, but if another thing in the xray
::    actually needs it, it will think it's a noun too.
::
::  - XX There are probably remaining bugs. Test the shit out of this.
::
::  - XX What should the `xrole` of a cell with a %noun head be? I
::    think the current design can't handle this case coherently.
::
|^  ^-  $:  ximage-to-spec=$-(=ximage =spec)
            xray-type=$-([@ type] ximage)
            focus-on=$-([xtable xkey] xray)
        ==
    [ximage-to-spec xray-type focus-on]
::
+|  %helpers
::
++  batt-of  |$  [arm]  (map term (pair what (map term arm)))
++  chap-of  |$  [arm]  [doc=what arms=(map term arm)]
::
::  Traverse over a chapter in a battery.
::
++  traverse-chapter
  |*  [state=mold in=mold out=mold]
  |=  [[st=state chap=(chap-of in)] f=$-([state term in] [out state])]
  ^-  [(chap-of out) state]
  =^  arms  st  ((traverse-map state term in out) [st arms.chap] f)
  [chap(arms arms) st]
::
::  Traverse over a battery.
::
++  traverse-battery
  |*  [state=mold in=mold out=mold]
  |=  [[st=state batt=(batt-of in)] f=$-([state term in] [out state])]
  ^-  [(batt-of out) state]
  %+  (traverse-map state term (chap-of in) (chap-of out))
    [st batt]
  |=  [st=state chapter-name=term chap=(chap-of in)]
  ^-  [(chap-of out) state]
  ((traverse-chapter state in out) [st chap] f)
::
::  Map a function over all the arms in a battery.
::
++  turn-battery
  |*  arm=mold
  |=  [b=(batt-of arm) f=$-(arm arm)]
  ^-  (batt-of arm)
  %-  ~(run by b)
  |=  [w=what chap=(map term arm)]
  ^-  [what (map term arm)]
  :-  w
  %-  ~(run by chap)
  |=  i=arm
  ^-  arm
  (f i)
::
::  Create a new xray with `xdat` set to `d`. If the xray is already in
::  the table, do nothing.
::
++  post-xray
  |=  [tbl=xtable ty=type d=(unit xdat)]
  ^-  [xkey xtable]
  ::
  =/  old  (~(get by type-map.tbl) ty)
  ?^  old  [u.old tbl]
  ::
  =/  i=xkey  next.tbl
  =/  x=xray  [i ty d ~ ~ ~ ~ ~ ~ ~]
  ::
  =.  next.tbl      +(next.tbl)
  =.  xrays.tbl     (~(put by xrays.tbl) i x)
  =.  type-map.tbl  (~(put by type-map.tbl) ty i)
  [i tbl]
::
::  Create an new xray and put it in the xray table. If there's already
::  a stub xray under this type, replace it.  Otherwise, allocate a
::  new index and put it there.
::
++  replace-xray
  |=  [img=xtable x=xray]
  ^-  xtable
  img(xrays (~(put by xrays.img) xkey.x x))
::
::  Get an xray, update its xdat, and put it back in.
::
++  set-xray-xdat
  |=  [img=xtable i=xkey d=xdat]
  ^-  xtable
  =/  x=xray  (focus-on img i)
  (replace-xray img x(xdat `d))
::
::  Get an xray from an `xtable`, given its `xkey`.
::
++  focus-on
  |=  [img=xtable i=xkey]
  ^-  xray
  =/  res=(unit xray)  (~(get by xrays.img) i)
  ?~  res  ~&  ['internal error: invalid xray reference' i]  !!
  u.res
::
::  Return a list of xrays referenced by an xrayed battery. (the context
::  type and the type of each arm).
::
++  battery-refs
  |=  b=xbat
  ^-  (list xkey)
  %-  zing
  %+  turn  ~(val by b)
  |=  [=what =(map term xkey)]
  ^-  (list xkey)
  ~(val by map)
::
::  Just for debugging: print an ximage and then return it.
::
++  trace-ximage
  |=  img=ximage
  ^-  ximage
  ~&  ['root=' root.img]
  ~&  %+  sort  ~(tap by xrays.xtable.img)
      |=  [[xi=xkey x=xray] [yi=xkey y=xray]]
      (lth xi yi)
  img
::
::  All non-fork xrays referenced by a fork xray. This will recurse
::  into forks-of-forks (and so on) and can handle infinite forks.
::
::  If this is called on a non-fork node, it will return a set with just
::  that one node in it.
::
::  Separating this out really simplifies things, without this handling
::  infinite forks is quite error-prone.
::
::  XX Should we collect face nodes instead of recursing into them (feels
::  like yes, but why did I do it the other way before)?
::
::  XX This is turning out to be useful. Should we add a field to cache
::  the result of this?
::
++  xray-branches
  |=  [img=xtable i=xkey]
  ^-  (set xkey)
  ::
  =/  acc=(set xkey)  ~
  =/  stk=(set xkey)  ~
  ::
  |-  ^-  (set xkey)
  ::
  ?:  (~(has in acc) i)  acc
  ?:  (~(has in stk) i)  acc
  ::
  =.  stk  (~(put in stk) i)
  ::
  =/  x=xray  (focus-on img i)
  =/  d=xdat  (need xdat.x)
  ::
  ?-  d
    %noun      (~(put in acc) i)
    %void      (~(put in acc) i)
    [%atom *]  (~(put in acc) i)
    [%cell *]  (~(put in acc) i)
    [%core *]  (~(put in acc) i)
    [%face *]  $(i xray.d)
    [%pntr *]  $(i xray.d)
    [%fork *]  %+  (fold (set xkey) xkey)
                 [acc ~(tap in set.d)]
               |=  [=(set xkey) =xkey]
               ^$(acc set, i xkey)
  ==
::
+|  %entry-point
::
::  The top-level routine: Takes a type, and xrays it to produce an
::  ximage.
::
::  When we analyze a core, we also analyze its context. `core-depth`
::  controls how deeply we will dig into the context. With `core-depth`
::  at 0, we just pretend that all cores have a context of type `*`.
::
++  xray-type
  |=  [core-depth=@ =type]
  ^-  ximage
  ::  ~&  %analyze-type
  =/  =ximage  (analyze-type core-depth type)
  ::  ~&  %cleanup
  =.  ximage  (cleanup ximage)
  ::  ~&  %decorate-ximage-with-loops
  =.  ximage  (decorate-ximage-with-loops ximage)
  ::  ~&  %decorate-ximage-with-xpats
  =.  ximage  (decorate-ximage-with-xpats ximage)
  ::  ~&  %decorate-ximage-with-xshapes
  =.  ximage  (decorate-ximage-with-xshapes ximage)
  ::  ~&  %trace-ximage
  ::  =.  ximage  (trace-ximage ximage)
  ::  ~&  %decorate-ximage-with-xroles
  (decorate-ximage-with-xroles ximage)
  ::  ~&  %trace-ximage
  ::  (trace-ximage ximage)
::
+|  %analysis-passes
::
::  The main analysis code.
::
::  For every type we encounter,
::
::  - First check if an xray for this has already been created. This
::    could either be a recursive reference or just something we've
::    already processed. At this point we don't care.
::
::  - Next, allocate a new xray for this type with empty xdat. If
::    we encounter this type again recursively, that's fine, that will
::    just produce a reference to this xray and it will eventually
::    have xdat.
::
::  - Next, recurse into all referenced types and build out graph
::    nodes for those.
::
::  - Finally, create `xdat` based on the above, and update the xray
::    to have that xdat.
::
::  - The two edge-cases here are %hint and %hold. For those, we simply
::    do everything in exactly the same way except that `xdat`
::    will be set to `[%pntr *]`. We will resolve all of these
::    references in the first analysis pass (`cleanup`).
::
++  analyze-type
  |=  [core-depth=@ud =top=type]
  ^-  ximage
  ::
  |^  (main [0 ~ ~] top-type)
  ::
  ++  main
    |=  [st=xtable ty=type]
    ^-  [xkey xtable]
    ::
    =/  old  (~(get by type-map.st) ty)             ::  already done
    ?^  old  [u.old st]
    ::
    =^  res=xkey  st  (post-xray st ty ~)
    ::
    :-  res
    ?-  ty
      %void      (set-xray-xdat st res %void)
      %noun      (set-xray-xdat st res %noun)
      [%atom *]  (set-xray-xdat st res ty)
      [%cell *]  =^  hed=xkey  st  (main st p.ty)
                 =^  tyl=xkey  st  (main st q.ty)
                 (set-xray-xdat st res [%cell hed tyl])
      [%core *]  =^  d=xdat   st  (xray-core [p.ty q.ty] st)
                 (set-xray-xdat st res d)
      [%face *]  =^  i=xkey  st  (main st q.ty)
                 (set-xray-xdat st res [%face p.ty i])
      [%fork *]   =^  br  st  ((traverse-set xtable type xkey) [st p.ty] main)
                 (set-xray-xdat st res [%fork br])
      [%hint *]  =^  ref      st  (main st q.ty)
                 =^  updated  st  (hint st p.ty (focus-on st res))
                 (set-xray-xdat (replace-xray st updated) res [%pntr ref])
      [%hold *]  =^  ref  st  (main st ~(repo ut ty))
                 (set-xray-xdat st res [%pntr ref])
    ==
  ::
  ::  Analyze a %hint type.
  ::
  ::  This updates the `helps`, `studs`, and/or `recipe` fields of the
  ::  given xray.
  ::
  ++  hint
    |=  [st=xtable [subject-of-note=type =note] x=xray]
    ^-  [xray xtable]
    ?-  -.note
      %help  :_  st  x(helps (~(put in helps.x) p.note))
      %know  :_  st  x(studs (~(put in studs.x) p.note))
      %made  =^  recipe  st
               ?~  q.note  [[%direct p.note] st]
               =^  params=(list xkey)  st
                 |-  ^-  [(list xkey) xtable]
                 ?~  u.q.note  [~ st]
                 =/  tsld  [%tsld [%limb %$] [%wing i.u.q.note]]
                 =/  part  (~(play ut subject-of-note) tsld)
                 =^  this  st  (main st part)
                 =^  more  st  $(u.q.note t.u.q.note)
                 [[this more] st]
               [[%synthetic p.note params] st]
             :_  st  x(recipes (~(put in recipes.x) recipe))
    ==
  ::
  ::  Analyze a core.
  ::
  ::  When we analyze the context, we decrement `core-depth`. If that
  ::  ever hits zero, we substitute `%noun` for the type of the context.
  ::
  ::  The reason that we switch the variance to %gold is because the
  ::  core we're creating isn't an actual core, we're just using the arms
  ::  of this core as a namespace in which to evaluate each arm.
  ::
  ::  Also, in general, there's no way to determine the type of an arm
  ::  of a wet core, so we just assign all wet arms the type `%noun`.
  ::
  ::  This seems to work in practice, but I don't think it's actually
  ::  sound.
  ::
  ++  xray-core
    |=  [[=payload=type =coil] st=xtable]
    ^-  [xdat xtable]
    ::
    =^  payload-xkey  st  (main st payload-type)
    =/  ctx=type  [%core payload-type coil(r.p %gold)]
    ::
    =^  batt  st
      %+  (traverse-battery xtable hoon xkey)
        [st q.r.coil]
      |=  [st=xtable nm=term =hoon]
      ^-  [xkey xtable]
      ?:  =(%wet q.p.coil)  (post-xray st %noun `%noun)
      ?:  =(0 core-depth)   (post-xray st %noun `%noun)
      =.  core-depth        (dec core-depth)
      (main st [%hold ctx hoon])
    ::
    [[%core p.coil payload-xkey batt] st]
  ::
  --
::
::  Remove `%pntr` nodes, replacing references to them with references
::  to what they resolve to.
::
::  1. Build a list of reachable, non-reference nodes.
::  2. Build a table of references mapped to the node they resolve to.
::  3. If the root node is a pointer, replace it with what it references.
::  4. Map over `type-map`, and replace every value using the table from #2.
::  5. Map over the xrays, drop pointer nodes, replace every reference
::     using the table from #2.
::
++  cleanup
  |=  xt=ximage
  ^-  ximage
  ::
  =/  img=xtable  xtable.xt
  ::
  |^  =/  =xkey          root.xt
      ::  ~&  %build-table
      =/  tbl           (build-table xkey)
      ::  ~&  %fix-xkey
      =.  xkey           (fix-xkey tbl xkey)
      ::  ~&  %fix-type-map
      =.  type-map.img  (fix-type-map tbl type-map.img)
      ::  ~&  %fix-xrays
      =.  xrays.img     (fix-xrays tbl xrays.img)
      ::  ~&  :*  %gc-results
      ::          %before  ~(wyt by xrays.xtable.xt)
      ::          %after   ~(wyt by xrays.img)
      ::      ==
      [xkey img]
  ::
  +$  table
    [live=(set xkey) refs=(map xkey xkey) refs-to=(map xkey (set xkey))]
  ::
  ::  Given a node that may be a pointer, follow the chain of pointers
  ::  until we find a non-pointer node.
  ::
  ++  deref
    |=  [img=xtable k=xkey]
    ^-  xkey
    |-
    =/  x=xray  (focus-on img k)
    =/  d=xdat  (need xdat.x)
    ?.  ?=([%pntr *] d)  xkey.x
    $(k xray.d)
  ::
  ::  Walks the graph starting at the root, everything that's a %pntr
  ::  node becomes a xkey in the `refs` table and one of the values in the
  ::  `refs-to` table.
  ::
  ++  build-table
    |^  |=  k=xkey
        ^-  table
        =/  t=table    [~ ~ ~]
        =.  t          (recur t k)
        =.  refs-to.t  ((reverse-map xkey xkey) refs.t)
        t
    ::
    ++  recur
      |=  [acc=table k=xkey]
      ^-  table
      ::
      ?:  (~(has in live.acc) k)  acc                 ::  already processed
      ?:  (~(has by refs.acc) k)  acc                 ::  already processed
      ::
      =/  x=xray  (focus-on img k)
      =/  d=xdat  (need xdat.x)
      ::
      =.  acc  ?.  ?=([%pntr *] d)
                 acc(live (~(put in live.acc) k))
               acc(refs (~(put by refs.acc) k (deref img k)))
      ::
      ((fold table xkey) [acc (xray-refs k)] recur)
    --
  ::
  ::  Rebuild `type-map`:
  ::
  ::  - If a type points to a pointer xray, update it to point to what
  ::    that pointer resolves to
  ::  - If the type isn't referenced from the root node, ignore it.
  ::  - Otherwise, just copy it into the resulting table as-is.
  ::
  ++  fix-type-map
    |=  [tbl=table =(map type xkey)]
    ^-  _map
    %+  (fold _map (pair type xkey))
      [*_map ~(tap by map)]
    |=  [acc=_map [ty=type k=xkey]]
    =/  dest  (~(get by refs.tbl) k)
    ?^  dest  (~(put by acc) ty u.dest)
    ?.  (~(has in live.tbl) k)  acc
    (~(put in acc) ty k)
  ::
  ::  Rebuild the `xrays` table.
  ::
  ::  - If the xray isn't in the `live` set (it wont be there if it's
  ::    a pointer node or if it's inaccessible from the root node),
  ::    then ignore it.
  ::  - Otherwise, copy the xray into the result map while updating
  ::    all its references.
  ::
  ++  fix-xrays
    |=  [tbl=table xrays=(map xkey xray)]
    ^-  _xrays
    %+  (fold (map xkey xray) (pair xkey xray))
      [*(map xkey xray) ~(tap by xrays)]
    |=  [acc=(map xkey xray) [i=xkey x=xray]]
    ?.  (~(has in live.tbl) i)  acc                   ::  Drop unused xrays
    (~(put by acc) i (fix-xray tbl x))
  ::
  ::  All the xrays which are simply references to `i`.
  ::
  ++  all-refs-to
    |=  [tbl=table i=xkey]
    ^-  (set xkey)
    =/  res  (~(get by refs-to.tbl) i)
    ?~(res ~ u.res)
  ::
  ::  There may be `%hint` xdat on the `%pntr` xrays. Find all pointer
  ::  nodes that reference this one, and put all of their hint-xdat onto
  ::  this xray.
  ::
  ++  collect-hints
    |=  [tbl=table target=xray]
    ^-  xray
    %+  (fold xray xkey)
      [target ~(tap in (all-refs-to tbl xkey.target))]
    |=  [acc=xray ref=xkey]
    =/  ref-xray=xray  (focus-on img ref)
    =/  helps    ^-  (set help)    (~(uni in helps.acc) helps.ref-xray)
    =/  recipes  ^-  (set recipe)  (~(uni in recipes.acc) recipes.ref-xray)
    ::
    =/  studs    ^-  (set stud)                       ::  Type system hack
                 %+  (fold (set stud) stud)
                   [studs.acc ~(tap in studs.ref-xray)]
                 |=  [acc=(set stud) new=stud]
                 (~(put in acc) new)
    ::
    acc(helps helps, studs studs, recipes recipes)
  ::
  ::  Note that the `xroles` and `pats` fields may contain references
  ::  to other xrays as well. We don't bother to update those, because this
  ::  pass runs before those fields are populated.
  ::
  ++  fix-xray
    |=  [tbl=table x=xray]
    ^-  xray
    =.  x  (collect-hints tbl x)
    %=  x
      xdat     `(fix-xdat tbl (need xdat.x))
      recipes  %-  ~(gas in *(set recipe))
               %+  turn  ~(tap in recipes.x)
               |=  r=recipe  (fix-recipe tbl r)
    ==
  ::
  ::  Update all the references in the `xdat` field.
  ::
  ++  fix-xdat
    |=  [tbl=table d=xdat]
    ^-  xdat
    ::
    =/  fix  |=(i=xkey (fix-xkey tbl i))
    ::
    ?-  d
      %noun      d
      %void      d
      [%atom *]  d
      [%cell *]  d(head (fix head.d), tail (fix tail.d))
      [%core *]  d(xray (fix xray.d), batt (fix-battery tbl batt.d))
      [%face *]  d(xray (fix xray.d))
      [%fork *]  d(set (~(gas in *(set xkey)) (turn ~(tap in set.d) fix)))
      [%pntr *]  d(xray (fix xray.d))
    ==
  ::
  ++  fix-battery
    |=  [tbl=table b=xbat]
    ^-  xbat
    %+  (turn-battery xkey)  b
    |=  i=xkey  (fix-xkey tbl i)
  ::
  ++  fix-xkey
    |=  [tbl=table i=xkey]
    ^-  xkey
    =/  res=(unit xkey)  (~(get by refs.tbl) i)
    ?^  res  u.res
    i
  ::
  ++  fix-recipe
    |=  [tbl=table r=recipe]
    ^-  recipe
    ?-  r
      [%direct *]     r
      [%synthetic *]  r(list (turn list.r |=(i=xkey (fix-xkey tbl i))))
    ==
  ::
  ++  xray-refs
    |=  i=xkey
    ^-  (list xkey)
    =/  x=xray  (focus-on img i)
    %-  zing
    ^-  (list (list xkey))
    :~  ?~(xdat.x ~ (xdat-refs u.xdat.x))
        (zing (turn ~(tap in recipes.x) recipe-refs))
        ?~(xrole.x ~ (xrole-refs u.xrole.x))
    ==
  ::
  ++  recipe-refs
    |=  r=recipe
    ^-  (list xkey)
    ?-  r
      [%direct *]     ~
      [%synthetic *]  list.r
    ==
  ::
  ++  xrole-refs
    |=  s=xrole
    ^-  (list xkey)
    ?@  s  ~
    ?-  -.s
      %constant     ~
      %instance     ~
      %option       ~(val by map.s)
      %union        ~(val by map.s)
      %junction     ~[flat.s deep.s]
      %conjunction  ~[wide.s tall.s]
      %misjunction  ~[one.s two.s]
    ==
  ::
  ++  xdat-refs
    |=  d=xdat
    ^-  (list xkey)
    ?-  d
      %noun      ~
      %void      ~
      [%atom *]  ~
      [%cell *]  ~[head.d tail.d]
      [%core *]  [xray.d (battery-refs batt.d)]
      [%face *]  ~[xray.d]
      [%pntr *]  ~[xray.d]
      [%fork *]  ~(tap in set.d)
    ==
  --
::
::  Detect loops.
::
::  This works by simply recursing through all the references within an
::  xray while keeping an explicit recursion stack: If we hit a node
::  that's in the stack, that's a loop. If we touch everything without
::  hitting a recursive reference, then it's not a loop.
::
::  Is the short-circuiting sound? I'm not sure now.
::
::    - When could it go wrong?
::    - This graph, for example:
::
::      ```
::      x -> y
::      y -> z
::      y -> y
::      z -> x
::      ```
::
::    - Let's say we process this starting with y, we will see that `y`
::      is a loop, and then when we go to process x, recursing into y will be
::      short-circuited since its `loop` field is already set.
::
::    - Well, maybe `x` will have been recognized as a loop during the
::      processing of `x`? I think it depends on whether we continue
::      to trace through all references from `y` even after we've found
::      a loop, and I think we do.
::
::    - Put another way, this will recurse into everything referenced
::      by a type, and only mark loops once it's encountered them:
::      After processing a type, every type that it references
::      (transitive closure) will have been processed correctly.
::
++  decorate-ximage-with-loops
  |=  xt=ximage
  ^-  ximage
  |^  xt(xtable decorated)
  ::
  ++  decorated
    ^-  xtable
    =/  all-indicies  ~(tap in ~(key by xrays.xtable.xt))
    ((fold xtable xkey) [xtable.xt all-indicies] decorate)
  ::
  ++  decorate
    |=  [img=xtable i=xkey]
    ^-  xtable
    ::
    =/  trace=(set xkey)  ~
    |-  ^-  xtable
    ::
    =/  x    (focus-on img i)
    =/  dat  (need xdat.x)
    ::
    ?.  =(~ loop.x)          img                      ::  already done
    ?:  (~(has in trace) i)  (replace-xray img x(loop `%.y))
    ::
    =.  trace  (~(put in trace) i)
    ::
    =.  img
      ?-  dat
        %noun      img
        %void      img
        [%atom *]  img
        [%cell *]  =.  img  $(i head.dat)
                   $(i tail.dat)
        [%core *]  =.  img  $(i xray.dat)
                   %+  (fold xtable xkey)
                     [img (battery-refs batt.dat)]
                   |=  [img=xtable i=xkey]
                   ^$(img img, i i)
        [%face *]  $(i xray.dat)
        [%pntr *]  $(i xray.dat)
        [%fork *]  %+  (fold xtable xkey)
                     [img ~(tap in set.dat)]
                   |=  [img=xtable i=xkey]
                   ^$(img img, i i)
        ==
    ::
    =.  x  (focus-on img i)                           ::  get updated xray
    ?^  loop.x  img                                   ::  loop found
    (replace-xray img x(loop `%.n))                   ::  no loop found
  --
::
::  Fills in the `xpats` fields in each xray (where possible).
::
::  This has a list of xpat "matchers", and, for each xray in the
::  ximage, it tries each matcher until one of them succeeds.
::
++  decorate-ximage-with-xpats
  |=  xt=ximage
  ^-  ximage
  ::
  =/  img=xtable  xtable.xt
  ::
  |^  =/  pairs  %+  turn  ~(tap by xrays.xtable.xt)
                 |=  [i=xkey x=xray]
                 ^-  [xkey xray]
                 [i x(pats (xray-pats x))]
      xt(xrays.xtable (~(gas by *(map xkey xray)) pairs))
  ::
  ++  xpats
    ^-  (list $-(xray (unit xpat)))
    :~  tree-xpat
        list-xpat
        unit-xpat
        core-xpat
        spec-xpat
        type-xpat
        manx-xpat
        vase-xpat
        hoon-xpat
        json-xpat
        nock-xpat
        plum-xpat
        skin-xpat
    ==
  ::
  ++  xray-pats
    |=  x=xray
    ^-  (unit xpat)
    ::
    =/  i=xkey  xkey.x
    =/  t=type  type.x
    =/  d=xdat  (need xdat.x)
    ::
    ::  Atom printing works just fine using the xdat field.
    ?:  ?=([%atom *] d)  ~
    ::
    =/  match  xpats
    ::
    |-  ^-  (unit xpat)
    ?~  match  ~
    =/  pat  (i.match x)
    ?^  pat  pat
    $(match t.match)
  ::
  ++  simple-nest-xpat
    |=  [ty=type pat=xpat]
    ^-  $-(xray (unit xpat))
    |=  x=xray
    ^-  (unit xpat)
    =/  subtype  (~(nest ut ty) | type.x)
    ?:(subtype `pat ~)
  ::
  ++  type-xpat  (simple-nest-xpat -:!>(*type) %type)
  ++  spec-xpat  (simple-nest-xpat -:!>(*spec) %spec)
  ++  manx-xpat  (simple-nest-xpat -:!>(*manx) %manx)
  ++  vase-xpat  (simple-nest-xpat -:!>(*vase) %vase)
  ++  hoon-xpat  (simple-nest-xpat -:!>(*hoon) %hoon)
  ++  json-xpat  (simple-nest-xpat -:!>(*json) %json)
  ++  nock-xpat  (simple-nest-xpat -:!>(*nock) %nock)
  ++  plum-xpat  (simple-nest-xpat -:!>(*plum) %plum)
  ++  skin-xpat  (simple-nest-xpat -:!>(*skin) %skin)
  ::
  ++  focus
    |=  i=xkey
    ^-  xray
    (focus-on img i)
  ::
  ++  is-nil
    |=  i=xkey
    ^-  ?
    =/  d=xdat  (need xdat:(focus i))
    ?+  d  %.n
      [%atom *]  =(d [%atom ~.n `0])
      [%face *]  $(i xray.d)
    ==
  ::
  ::  Is `ref`, after dereferencing faces, a loop-reference to `target`?
  ::
  ++  is-ref-to
    |=  [target=xkey ref=xkey]
    ^-  ?
    ?:  =(target ref)  %.y
    =/  =xdat  (need xdat:(focus ref))
    ?:  ?=([%face *] xdat)  $(ref xray.xdat)
    %.n
  ::
  ::  Is an xray an atom with the specified aura?
  ::
  ++  is-atom-with-aura
    |=  [c=cord i=xkey]
    ^-  ?
    =/  =xdat  (need xdat:(focus i))
    ?+  xdat  %.n
      [%atom *]  =(xdat [%atom aura=c constant-unit=~])
      [%face *]  $(i xray.xdat)
    ==
  ::
  ::  If the xray is a exactly two things, nil and a cell type, then
  ::  yield the xray for the cell type.
  ::
  ++  fork-of-nil-and-cell
    |=  x=xray
    ^-  (unit xkey)
    ::
    =/  d=xdat  (need xdat.x)
    ::
    ?.  ?=([%fork *] d)  ~
    ::
    =/  branches  ~(tap in set.d)
    ?.  ?=([* * ~] branches)  ~
    ::
    =/  nil   i.branches
    =/  node  i.t.branches
    |-
    ::
    ?:  (is-nil node)  $(node nil, nil node)
    ?.  (is-nil nil)   ~
    ::
    `node
  ::
  ::  Is this xray a unit? (the %unit xpat)
  ::
  ::  This matches strictly. For example `[~ %a]` doesn't match, but
  ::  `^-((unit @) [~ %a])` does.
  ::
  ++  unit-xpat
    |^  |=  x=xray
        ^-  (unit xpat)
        =/  elem  (match-unit-type-strict (focus xkey.x))
        ?~  elem  ~
        `[%unit u.elem]
    ::
    ++  match-unit-type-strict
      |=  =input=xray
      ^-  (unit xkey)
      ::
      =/  node=(unit xkey)  (fork-of-nil-and-cell input-xray)
      ?~  node  ~
      ::
      =/  node-xdat=xdat  (need xdat:(focus u.node))
      ::
      ?.  ?=([%cell *] node-xdat)  ~
      ?.  (is-nil head.node-xdat)  ~
      =/  elem-xkey                 tail.node-xdat
      =/  elem-xdat                (need xdat:(focus elem-xkey))
      ?.  ?=([%face *] elem-xdat)  ~
      ::
      `xray.elem-xdat
    --
  ::
  ::  Is this xray a tree? (the %tree xpat)
  ::
  ++  tree-xpat
    |^  |=  =input=xray
        ^-  (unit xpat)
        =/  input-xkey=xkey  xkey.input-xray
        =/  inxdat=xdat      (need xdat.input-xray)
        ?.  ?=([%fork *] inxdat)  ~
        =/  branches  ~(tap in set.inxdat)
        ?.  ?=([* * ~] branches)  ~
        =/  nil   i.branches
        =/  node  i.t.branches
        |-
        ?:  (is-nil node)  $(node nil, nil node)
        ?.  (is-nil nil)  ~
        =/  node-xdat=xdat  (need xdat:(focus node))
        ?.  ?=([%cell *] node-xdat)  ~
        ?.  (is-pair-of-refs-to input-xkey tail.node-xdat)
          ~
        =/  elem-xdat  (need xdat:(focus head.node-xdat))
        ?.  ?=([%face *] elem-xdat)  ~
        `[%tree xray.elem-xdat]
    ::
    ++  is-pair-of-refs-to
      |=  [target=xkey cell=xkey]
      ^-  ?
      =/  =xdat  (need xdat:(focus cell))
      ?:  ?=([%face *] xdat)  $(cell xray.xdat)
      ?.  ?=([%cell *] xdat)  %.n
      ?.  (is-ref-to target head.xdat)  %.n
      ?.  (is-ref-to target tail.xdat)  %.n
      %.y
    --
  ::
  ::
  ::  Is this xray a list? (a %list, %tape, %path, or %tour xpat)
  ::
  ::  This handles the special case of path literals not having a
  ::  list type:  `/a/b` is just a macro for `[%a %b ~]`, but doesn't
  ::  accept this for other lists: We don't want ['a' %n ~] to be printed
  ::  as `['a' ~[%n]]`. However, we WILL print ['a' ~['b' 'c']] as ~['a'
  ::  'b' 'c']. And that's what `match-list` matches on.
  ::
  ::  `match-list` checks is a type is informally a list: Is it a
  ::  cell with a (formal or informal) list in its tail?
  ::
  ::  `match-list-type-strict` checks if a list literally has the shape
  ::  of a `list type`. It must be a loop reference and fork of two
  ::  types, one of which is the nil type and the other is a cell with a
  ::  face in its head and loop reference as its tail.
  ::
  ++  list-xpat
    |^  |=  x=xray
        ^-  (unit xpat)
        =/  elem  (match-list x)
        ?~  elem  ~
        ?:  (is-atom-with-aura 'tD' u.elem)   [~ %tape]
        ?:  (is-atom-with-aura 'ta' u.elem)   [~ %path]
        ?:  (is-atom-with-aura 'c' u.elem)    [~ %tour]
        ?:  (is-atom-with-aura 'tas' u.elem)  [~ %path]
        `[%list u.elem]
    ::
    ++  match-list
      |=  =input=xray
      ^-  (unit xkey)
      =/  d=xdat  (need xdat.input-xray)
      ?+  d        ~
        [%face *]  (match-list (focus xray.d))
        [%fork *]  (match-list-type-strict input-xray)
        [%cell *]  =/  elem-xkey=(unit xkey)
                     ?:  ?&((is-nil tail.d) (is-atom-with-aura 'tas' head.d))
                       `head.d
                     (match-list (focus tail.d))
                   ?~  elem-xkey                       ~
                   ?.  (is-ref-to u.elem-xkey head.d)  ~
                   `u.elem-xkey
      ==
    ::
    ++  match-list-type-strict
      |=  =input=xray
      ^-  (unit xkey)
      ::
      =/  node=(unit xkey)  (fork-of-nil-and-cell input-xray)
      ?~  node  ~
      ::
      =/  node-xdat=xdat                   (need xdat:(focus u.node))
      ?.  ?=([%cell *] node-xdat)          ~
      ?.  (is-ref-to xkey.input-xray tail.node-xdat)  ~
      ::
      =/  elem-xdat                        (need xdat:(focus head.node-xdat))
      ?.  ?=([%face *] elem-xdat)          ~
      ::
      `xray.elem-xdat
    --
  ::
  ::  A %gear is any core with a cell context.
  ::
  ::  A %gate is a gear with one chapter ('') with one arm ('').
  ::
  ++  core-xpat
    |^  |=  x=xray
        ^-  (unit xpat)
        =.  x  (focus xkey.x)
        =/  gear  (match-gear x)
        ?~  gear  ~
        =/  gate  (match-gate x sample.u.gear batt.u.gear)
        ?^  gate  gate
        ~  ::  XX  gear
    ::
    ++  match-gear
      |=  =input=xray
      ^-  (unit [%gear sample=xkey context=xkey batt=xbat])
      ::
      =/  input-xdat  (need xdat.input-xray)
      ?.  ?=([%core *] input-xdat)  ~
      =/  context-xkey=xkey  xray.input-xdat
      ::
      =/  context-xdat=xdat  (need xdat:(focus context-xkey))
      ?.  ?=([%cell *] context-xdat)  ~
      ::
      =/  sample-xkey=xkey  head.context-xdat
      =.  context-xkey     tail.context-xdat
      `[%gear sample-xkey context-xkey batt.input-xdat]
    ::
    ++  match-gate
      |=  [=input=xray sample=xkey batt=xbat]
      ^-  (unit [%gate xkey xkey])
      ::
      =/  input-xdat  (need xdat.input-xray)
      ?.  ?=([%core *] input-xdat)  ~
      =/  chapters  ~(tap by batt)
      ::
      ?~  chapters            ~
      ?^  t.chapters          ~
      ?.  =(p.i.chapters '')  ~
      ::
      =/  arms=(list (pair term xkey))  ~(tap by q.q.i.chapters)
      ::
      ?~  arms            ~
      ?^  t.arms          ~
      ?.  =(p.i.arms '')  ~
      ::
      =/  product=xkey  q.i.arms
      ::
      `[%gate sample product]
    --
  ::
  --
::
::  Determines the loose shape of each node in an ximage.
::
::  This is trival for everything besides forks, and for forks, we just
::  find all the non-fork branches with `xray-branches` and then calculate
::  the union type with `combine`.
::
::  Here's some pseudocode for the essence of the logic that we're
::  trying to implement here:
::
::      xdat Data = Noun | Void
::                | Atom | Cnst
::                | Cell Data Data
::                | Fork Data Data
::
::      xdat Shape = Noun | Void | Atom | Cnst | Cell | Junc
::
::      shape :: Data -> Shape
::      shape Noun       = Noun
::      shape Void       = Void
::      shape Atom       = Atom
::      shape Cnst       = Atom
::      shape (Cell a b) = Cell
::      shape (Fork x y) = forkShape (shape x) (shape y)
::
::      forkShape :: Shape -> Shape -> Shape
::      forkShape Void x           = x
::      forkShape Noun _           = Noun
::      forkShape Junc _           = Junc
::      forkShape Atom Cell        = Junc
::      forkShape x    y    | x==y = x
::      forkShape x    y           = forkShape y x
::
++  decorate-ximage-with-xshapes
  |^  |=  xt=ximage
      ^-  ximage
      =/  keys  ~(tap in ~(key by xrays.xtable.xt))
      %=  xt  xtable
        %+  (fold xtable xkey)
          [xtable.xt keys]
        |=  [st=xtable i=xkey]
        xtable:(xray-xshape st i)
      ==
  ::
  ::  Calculate the xray
  ::
  ++  xray-xshape
    |=  [st=xtable i=xkey]
    ^-  [xshape =xtable]
    ::
    =/  x=xray  (focus-on st i)
    =/  dat  (need xdat.x)
    ::
    ?^  xshape.x  [u.xshape.x st]                     ::  already processed
    ::
    =^  res=xshape  st
      ?-  dat
        %noun      [%noun st]
        %void      [%void st]
        [%atom *]  [%atom st]
        [%cell *]  [%cell st]
        [%core *]  [%cell st]
        [%fork *]  (fork-xshape st (xray-branches st xkey.x))
        [%face *]  (xray-xshape st xray.dat)
        [%pntr *]  !!                                 ::  run `cleanup` first
      ==
    ::
    =/  y=xray    x                                   ::  type system hack
    =.  xshape.y   `res
    =.  xrays.st  (~(put by xrays.st) xkey.y y)
    [res st]
  ::
  ::  Because `branches` comes from `xray-branches`, none of the xrays
  ::  we're folding over will be forks, therefore, we none of our calls
  ::  to `xray-xshape` will recurse: we won't get stuck in a loop.
  ::
  ++  fork-xshape
    |=  [st=xtable branches=(set xkey)]
    ^-  [xshape xtable]
    %+  (fold (pair xshape xtable) xkey)
      [[%void st] ~(tap in branches)]
    |=  [acc=(pair xshape xtable) i=xkey]
    ^-  [xshape xtable]
    =^  res  st  (xray-xshape q.acc i)
    [(combine p.acc res) st]
  ::
  ::  Given the xshapes of two types, determine the xshape of their union.
  ::
  ++  combine
    |=  [x=xshape y=xshape]
    ^-  xshape
    ?:  =(x y)      x
    ?:  =(x %void)  y
    ?:  =(y %void)  x
    ?:  =(x %noun)  %noun
    ?:  =(y %noun)  %noun
    %junc
  --
::
::  Determine the `xrole` of each xray node, restructuring forks to make
::  them coherent.
::
::  This is fairly simple for non-role types, and we handle forks the
::  same way we do with `xshape` detection. The basic move is to get all
::  of the non-fork branches using `xray-branches`, make a list of them,
::  and fold a function over that. However, the function we're folding with
::  is MUCH more complicated.
::
::  One of the big sources of complexity is that we need to restructure
::  the shape of forks, so we will be creating a bunch of new graph
::  nodes, and rearranging them. For example, if we want to merge a
::  junction (a fork of an atom and a cell) with an atom type, we create
::  a new junction xray that is a fork of the old cell type and the
::  union of the two cell types. The function we fold with is `merge`,
::  but the bulk of the logic lives in `combine`.
::
::  Here's some pseudocode for the essence of the logic that we're
::  trying to implement here. Note that the code is actually shaped
::  quite differently than this and is much more detailed. So, try
::  to wrap your head around WHY this makes sense instead of just
::  trying to use this a map for the actual code.
::
::      xdat Data = Noun | Void
::                | Atom | Cnst
::                | Cell Data Data
::                | Fork Data Data
::
::      xdat Shape = Noun | Void | Atom | Cnst | Cell | Junc
::
::      xdat Role = Void | Noun
::                | Atom | Cnst
::                | Tall | Wide | Instance
::                | Option | Union | Conjunc | Junc
::                | Misjunc
::
::      role :: Data -> Unit Role
::      role Noun        = ~
::      role Void        = ~
::      role Atom        = ~
::      role Cnst        = ~
::      role (Cell hd _) = `(cellRoleByHead (shape hd))
::      role (Fork x y)  = `(forkRole (shape x, role x) (shape y, role y))
::
::      cellRoleByHead :: Shape -> Unit Role
::      cellRoleByHead Cell = `Wide
::      cellRoleByHead Cnst = `Instance
::      cellRoleByHead Atom = `Tall
::      cellRoleByHead _    = ~
::
::      forkRole :: (Shape,Role) + (Shape,Role) -> Role
::      forkRole
::          Option  <- option + option
::          Union   <- union  + union
::          Conjunc <- tall   + wide
::          Junc    <- atom   + cell
::          Misjunc <- otherwise
::        where
::          option = role==Option || role==Instance
::          union  = shape==Cnst  || role==Union
::          atom   = shape==Atom  || shape==Cnst
::          cell   = shape==Cell
::          tall   = role==Tall
::          wide   = role==Wide
::          cell   = shape==Cell
::
++  decorate-ximage-with-xroles
  |^  |=  xt=ximage
      ^-  ximage
      ::
      =/  keys=(list xkey)  ~(tap in ~(key by xrays.xtable.xt))
      ::
      %=  xt  xtable
        %+  (fold xtable xkey)  [xtable.xt keys]
        |=  [st=xtable i=xkey]
        ^-  xtable
        xtable:(xray-xrole st i)
      ==
  ::
  ::  Given a type and xdat, either find the xray corresponding to that
  ::  type, or create a new one.
  ::
  ::  These xrays are for internal types that we create in order to
  ::  restructure forks, therefore they will never be loops.
  ::
  ++  alloc-fork-xray
    |=  [st=xtable ty=type d=xdat]
    ^-  [xkey xtable]
    =/  old=(unit xkey)  (~(get by type-map.st) ty)
    ?^  old  [u.old st]
    =/  xkey          next.st
    =/  res=xray     [xkey ty `d ~ ~ ~ ~ ~ ~ `%.n]
    =.  next.st      +(xkey)
    =.  xrays.st     (~(put by xrays.st) xkey.res res)
    =.  type-map.st  (~(put by type-map.st) type.res xkey.res)
    [xkey st]
  ::
  ::  Produces an xtable updated to have xrole information for a certain
  ::  node. For convenience, it also returns the xrole itself.
  ::
  ::  Note that the xrole of a core is always %wide, since the head of
  ::  a core is a battery, which is always a cell.
  ::
  ++  xray-xrole
    |=  [st=xtable i=xkey]
    ^-  [=xrole =xtable]
    =/  x=xray  (focus-on st i)
    ::
    =/  old  xrole.x
    ?^  old  [u.old st]
    ::
    =/  dat=xdat  (need xdat.x)
    ::
    =^  res=xrole  st
      ?:  ?=([~ %void] xshape.x)  [%void st]          ::  optimization
      ?:  ?=([~ %noun] xshape.x)  [%noun st]          ::  optimization
      ?-  dat
        %noun      :_  st  %noun
        %void      :_  st  %void
        [%atom *]  :_  st  (atom-xrole dat)
        [%cell *]  :_  st  (cell-xrole-by-head (focus-on st head.dat))
        [%core *]  :_  st  %wide
        [%face *]  (xray-xrole st xray.dat)
        [%pntr *]  !!                                 ::  Run `cleanup` first.
        [%fork *]  (fork-xrole st (xray-branches st xkey.x))
      ==
    ::
    =.  xrays.st  (~(put by xrays.st) xkey.x x(xrole `res))
    [res st]
  ::
  ::  Determines the xrole of an atom xray.
  ::
  ++  atom-xrole
    |=  [%atom =aura =constant=(unit @)]
    ^-  xrole
    ?~  constant-unit  %atom
    [%constant u.constant-unit]
  ::
  ::  Calculate the xrole of a %cell xray.
  ::
  ::  XX I'm not sure this is correct. Should a cell with a noun head
  ::  be %tall? How about a %void head?
  ::
  ::    - A %void head should probably be %void.
  ::    - A %noun head should probably just be %cell, a xrole separate from
  ::      (%wide and %tall) to make the ambiguity explicit. For example,
  ::      the union of `[* @] + [@ @]` should be a misjunction, which isn't
  ::      what's happening now.
  ::
  ::  XX Also! A cell with a junction in its head should be a
  ::  conjunction, right?
  ::
  ++  cell-xrole-by-head
    |=  head=xray
    ^-  xrole
   ::
    =/  =xshape  (need xshape.head)
    =/  =xdat   (need xdat.head)
   ::
    =/  const  ?.  ?=([%atom *] xdat)  ~
               constant.xdat
    ::
    ?:  =(xshape %cell)  %wide
    ?^  const           [%instance u.const]
    %tall
  ::
  ::  Determine the xrole of %fork type.
  ::
  ::  Fold over all the branches off a fork using the `merge` function,
  ::  and then grab its `xrole` using `xray-xrole`.
  ::
  ::  In any non-trivial cases, the xray returned from `merge` will
  ::  already have its `xrole` set, so recursing into `xray-xrole`
  ::  shouldn't be dangerous.
  ::
  ::  XX This is probably an important part of the control-flow, and it
  ::  might be helpful to make this invariant more prominent.
  ::
  ++  fork-xrole
    |=  [st=xtable fork=(set xkey)]
    ^-  [xrole xtable]
    ::
    =^  void  st  (post-xray st %void `%void)
    ::
    =^  i=xkey  st
      ^-  [xkey xtable]
      %+  (fold {xkey xtable} xkey)
        [[void st] ~(tap in fork)]
      |=  [[k=xkey tbl=xtable] branch=xkey]
      ^-  [xkey xtable]
      (merge tbl k branch)
    ::
    (xray-xrole st i)
  ::
  ::  Return an xray of the union of two xrays.
  ::
  ++  merge
    |=  [st=xtable this=xkey that=xkey]
    ^-  [xkey xtable]
    =/  this-xray=xray  (focus-on st this)
    =/  that-xray=xray  (focus-on st that)
    ?:  =(%void type.this-xray)  [that st]
    ?:  =(%void type.that-xray)  [this st]
    (combine st this that)
  ::
  ::  =collate-union: merge union maps
  ::
  ++  collate-union
    |^  |=  [st=xtable thick=(map atom xkey) thin=(map atom xkey)]
        ^-  [(map atom xkey) xtable]
        ::
        =/  list=(list (pair atom xkey))  ~(tap by thin)
        ::
        |-  ^-  [(map atom xkey) xtable]
        ::
        ?~  list  [thick st]
        =/  item=(unit xkey)  (~(get by thick) p.i.list)
        =^  merged=xkey  st  ?~  item  [q.i.list st]
                            (merge-instances st p.i.list u.item q.i.list)
        =/  new-thick  (~(put by thick) p.i.list merged)
        $(list t.list, thick new-thick)
    ::
    ::  We want to merge two cell-types that have the same head; gross.
    ::
    ::  First, get both tail types, merge them, produce a new cell type
    ::  with the merged tail.
    ::
    ++  merge-instances
      |=  [st=xtable =atom =x=xkey =y=xkey]
      ^-  [xkey xtable]
      ::
      =/  x-xray=xray    (focus-on st x-xkey)
      =/  x-xdat=xdat    (need xdat.x-xray)
      |-  ^-  [xkey xtable]
      ::
      ?:  ?=([%face *] x-xdat)
        $(x-xdat (need xdat:(focus-on st xray.x-xdat)))
      ?>  ?=([%cell *] x-xdat)
      =/  x-tail=xkey      tail.x-xdat
      =/  head-xray=xray  (focus-on st head.x-xdat)
      ::
      =/  y-xray=xray     (focus-on st y-xkey)
      =/  y-xdat=xdat     (need xdat.y-xray)
      |-  ^-  [xkey xtable]
      ::
      ?:  ?=([%face *] y-xdat)
        $(y-xdat (need xdat:(focus-on st xray.y-xdat)))
      ?>  ?=([%cell *] y-xdat)
      =/  y-tail=xkey      tail.y-xdat
      ::
      =^  merged-tail  st  (merge st x-tail y-tail)
      =/  tail-xray=xray   (focus-on st merged-tail)
      ::
      =/  res-ty=type    [%cell type.head-xray type.tail-xray]
      =/  res-xdat=xdat  [%cell xkey.head-xray xkey.tail-xray]
      =^  res-xkey  st   (alloc-fork-xray st res-ty res-xdat)
      ::
      =/  res-xray=xray    (focus-on st res-xkey)
      =.  xshape.res-xray  `%cell
      =.  xrole.res-xray   `[%instance atom]
      =.  xrays.st         (~(put by xrays.st) res-xkey res-xray)
      ::
      [xkey.res-xray st]
    --
  ::
  ::  =collate-option: merge option maps
  ::
  ++  collate-option
    |=  [st=xtable thick=(map atom xkey) thin=(map atom xkey)]
    ^-  [(map atom xkey) xtable]
    =/  list=(list (pair atom xkey))  ~(tap by thin)
    |-
    ^-  [(map atom xkey) xtable]
    ?~  list  [thick st]
    =/  item=(unit xkey)  (~(get by thick) p.i.list)
    =^  merged=xkey  st  ?~  item  [q.i.list st]
                        (merge st u.item q.i.list)
    =/  new-thick  (~(put by thick) p.i.list merged)
    $(list t.list, thick new-thick)
  ::
  ::  Create a new xray that is the union of two xrays, but with a
  ::  coherent `xrole` (where possible, otherwise a %misjunction).
  ::
  ::  This often needs to restructure things. For example, if we are
  ::  combining `{{~ ~} {%a ~}}` and `{{~ ~} {%b ~}}`, we should produce
  ::  `{{~ ~} ?%({%a ~} {%b ~})}`.
  ::
  ::  This is a massive switch on the xroles of the two arguments. This
  ::  is *very* easy to get wrong, so I structured things this in a
  ::  verbose and explicit way, so that you should be able to easily go
  ::  through each case and verify that it's doing the right thing.
  ::
  ++  combine
    |^  |=  [st=xtable =this=xkey =that=xkey]
        ^-  [xkey xtable]
        ::
        ?:  =(this-xkey that-xkey)  [this-xkey st]
        ::
        =^  this-xrole=xrole  st  (xray-xrole st this-xkey)
        =^  that-xrole=xrole  st  (xray-xrole st that-xkey)
        ::
        =/  this=[=xkey =xrole]  [this-xkey this-xrole]
        =/  that=[=xkey =xrole]  [that-xkey that-xrole]
        ::
        ?:  ?=(%void xrole.this)             [that-xkey st]
        ?:  ?=(%void xrole.that)             [this-xkey st]
        ?:  ?=(%noun xrole.this)             (noun-noun st this that)
        ?:  ?=(%noun xrole.that)             (noun-noun st that this)
        ?:  ?=([%misjunction *] xrole.this)  (misjunkin st this that)
        ?:  ?=([%misjunction *] xrole.that)  (misjunkin st this that)
        ::
        ?-  xrole.that
          %atom
            ?-  xrole.this
              %atom             (atom-atom st that this)
              %tall             (atom-cell st that this)
              %wide             (atom-cell st that this)
              [%constant *]     (atom-atom st that this)
              [%instance *]     (atom-cell st that this)
              [%option *]       (atom-optn st that this)
              [%union *]        (atom-cell st that this)
              [%junction *]     (atom-junc st that this)
              [%conjunction *]  (atom-cell st that this)
            ==
          %tall
            ?-  xrole.this
              %atom             (atom-cell st this that)
              %tall             (tall-tall st this that)
              %wide             (wide-tall st this that)
              [%constant *]     (atom-cell st this that)
              [%instance *]     (tall-tall st this that)
              [%option *]       (atom-cell st this that)
              [%union *]        (tall-tall st this that)
              [%junction *]     (cell-junc st that this)
              [%conjunction *]  (tall-conj st that this)
            ==
          %wide
            ?-  xrole.this
              %atom             (atom-cell st this that)
              %tall             (wide-tall st that this)
              %wide             (wide-wide st this that)
              [%constant *]     (atom-cell st this that)
              [%instance *]     (wide-tall st this that)
              [%option *]       (atom-cell st this that)
              [%union *]        (wide-tall st that this)
              [%junction *]     (cell-junc st that this)
              [%conjunction *]  (wide-conj st that this)
            ==
          [%constant *]
            ?-  xrole.this
              %atom             (atom-atom st that this)
              %tall             (atom-cell st that this)
              %wide             (atom-cell st that this)
              [%constant *]     (cnst-cnst st that this)
              [%instance *]     (atom-cell st that this)
              [%option *]       (cnst-optn st that this)
              [%union *]        (atom-cell st that this)
              [%junction *]     (atom-junc st that this)
              [%conjunction *]  (atom-cell st that this)
            ==
          [%instance *]
            ?-  xrole.this
              %atom             (atom-cell st this that)
              %tall             (tall-tall st this that)
              %wide             (wide-tall st this that)
              [%constant *]     (atom-cell st this that)
              [%instance *]     (inst-inst st this that)
              [%option *]       (atom-cell st this that)
              [%union *]        (inst-unin st that this)
              [%junction *]     (cell-junc st that this)
              [%conjunction *]  (tall-conj st that this)
            ==
          [%option *]
            ?-  xrole.this
              %atom             (atom-optn st this that)
              %tall             (atom-cell st that this)
              %wide             (atom-cell st that this)
              [%constant *]     (cnst-optn st this that)
              [%instance *]     (atom-cell st that this)
              [%option *]       (optn-optn st this that)
              [%union *]        (atom-cell st that this)
              [%junction *]     (atom-junc st that this)
              [%conjunction *]  (atom-cell st that this)
            ==
          [%union *]
            ?-  xrole.this
              %atom             (atom-cell st this that)
              %tall             (tall-tall st this that)
              %wide             (wide-tall st this that)
              [%constant *]     (atom-cell st this that)
              [%instance *]     (inst-unin st this that)
              [%option *]       (atom-cell st this that)
              [%union *]        (unin-unin st this that)
              [%junction *]     (cell-junc st that this)
              [%conjunction *]  (tall-conj st that this)
            ==
          [%junction *]
            ?-  xrole.this
              %atom             (atom-junc st this that)
              %tall             (cell-junc st this that)
              %wide             (cell-junc st this that)
              [%constant *]     (atom-junc st this that)
              [%instance *]     (cell-junc st this that)
              [%option *]       (atom-junc st this that)
              [%union *]        (cell-junc st this that)
              [%junction *]     (junc-junc st this that)
              [%conjunction *]  (cell-junc st this that)
            ==
          [%conjunction *]
            ?-  xrole.this
              %atom             (atom-cell st this that)
              %tall             (tall-conj st this that)
              %wide             (wide-conj st this that)
              [%constant *]     (atom-cell st this that)
              [%instance *]     (tall-conj st this that)
              [%option *]       (atom-cell st this that)
              [%union *]        (tall-conj st this that)
              [%junction *]     (cell-junc st that this)
              [%conjunction *]  (conj-conj st this that)
            ==
        ==
    ::
    ::  This guy ACTUALLY constructs the union type by calling `fork`
    ::  from `hoon.hoon`. To populate the `xdat` field, we just call
    ::  `xray-branches` on both of the input xrays and union the result.
    ::
    ::  Node that `xray-branches` produces a singleton set when called on
    ::  a node that isn't a fork, so this works correctly both for
    ::  joining fork node and non-fork nodes.
    ::
    ++  join
      |=  [st=xtable this=xkey that=xkey]
      ^-  [xkey xtable]
      ::
      ?:  =(this that)  [this st]
      ::
      =/  this-xray=xray  (focus-on st this)
      =/  that-xray=xray  (focus-on st that)
      ::
      =/  union-type=type  (fork ~[type.this-xray type.that-xray])
      ::
      =/  this-fork  (xray-branches st this)
      =/  that-fork  (xray-branches st that)
      =/  branches   (~(uni in this-fork) that-fork)
      ::
      (alloc-fork-xray st union-type [%fork branches])
    ::
    ::  Create the join of two xrays with the specified `xrole`.
    ::
    ++  joint
      |=  [st=xtable x=xkey y=xkey =xrole]
      ^-  [xkey xtable]
      ::
      =^  joined=xkey  st  (join st x y)
      =/  jray            (focus-on st joined)
      =.  st              (replace-xray st jray(xrole `xrole))
      [xkey.jray st]
    ::
    ++  atom-atom                                     ::  Can't discriminate
      |=  [st=xtable [x=xkey xrole] [y=xkey xrole]]
      (joint st x y [%misjunction x y])
    ::
    ++  atom-cell
      |=  [st=xtable [a=xkey xrole] [c=xkey xrole]]
      (joint st a c [%junction a c])
    ::
    ++  wide-tall
      |=  [st=xtable [w=xkey xrole] [t=xkey xrole]]
      (joint st w t [%conjunction w t])
    ::
    ++  noun-noun                                     ::  Can't discriminate
      |=  [st=xtable [x=xkey xrole] [y=xkey xrole]]
      (joint st x y [%misjunction x y])
    ::
    ++  misjunkin
      |=  [st=xtable [x=xkey xrole] [y=xkey xrole]]
      (joint st x y [%misjunction x y])
    ::
    ++  atom-optn                                     ::  Can't discriminate
      |=  [st=xtable [x=xkey xrole] [y=xkey [%option *]]]
      (joint st x y [%misjunction x y])
    ::
    ++  cnst-optn
      |=  $:  st=xtable
              [x=xkey [%constant xv=atom]]
              [y=xkey [%option ym=(map atom xkey)]]
          ==
      =^  res  st  (collate-option st [[xv x] ~ ~] ym)
      (joint st x y [%option res])
    ::
    ::  XX If the have the same xkey, produce a new instance who's tail
    ::  is the union of both tails.
    ::
    ++  inst-inst
      |=  $:  st=xtable
              [x=xkey [%instance xv=atom]]
              [y=xkey [%instance yv=atom]]
          ==
      =^  res  st  (collate-union st [[xv x] ~ ~] [[yv y] ~ ~])
      (joint st x y [%union res])
    ::
    ++  inst-unin
      |=  $:  st=xtable
              [x=xkey [%instance xv=atom]]
              [y=xkey [%union ym=(map atom xkey)]]
          ==
      =^  res  st   (collate-union st [[xv x] ~ ~] ym)
      (joint st x y [%union res])
    ::
    ++  junc-junc
      |=  $:  st=xtable
              [x=xkey [%junction xflat=xkey xdeep=xkey]]
              [y=xkey [%junction yflat=xkey ydeep=xkey]]
          ==
      =^  flat  st  (merge st xflat yflat)
      =^  deep  st  (merge st xdeep ydeep)
      (joint st x y [%junction flat deep])
    ::
    ::  XX Justify why this is always a misjunction. What if they have
    ::  the same head? Wouldn't producing a wide with that head and the
    ::  union of the two tails be coherent?
    ::
    ::  I *can* get the head and the tail of both and merge them,
    ::  why would this never make sense?
    ::
    ++  tall-tall
      |=  [st=xtable [x=xkey xrole] [y=xkey xrole]]
      (joint st x y [%misjunction x y])
    ::
    ++  unin-unin
      |=  $:  st=xtable
              [x=xkey [%union xm=(map atom xkey)]]
              [y=xkey [%union ym=(map atom xkey)]]
          ==
      =^  res  st  (collate-union st xm ym)
      (joint st x y [%union res])
    ::
    ::  XX  Can this ever produce a coherent result? If it can't, should
    ::  the result be a misjunction, or should the misjunction instead
    ::  exist in the wide part of the resulting conjunction (what this
    ::  code will do)?
    ::
    ++  wide-conj
      |=  $:  st=xtable
              [x=xkey xrole]
              [y=xkey [%conjunction ywide=xkey ytall=xkey]]
          ==
      =^  new-wide  st  (merge st x ywide)
      (joint st x y [%conjunction new-wide ytall])
    ::
    ::  XX Justify why this is always a misjunction. What if they have
    ::  the same head? Wouldn't producing a wide with that head and the
    ::  union of the two tails be coherent?
    ::
    ::  I *can* get the head and the tail and merge
    ::  them, why would this never make sense?
    ::
    ++  wide-wide
      |=  [st=xtable [x=xkey xrole] [y=xkey xrole]]
      (joint st x y [%misjunction x y])
    ::
    ++  cnst-cnst
      |=  $:  st=xtable
              [x=xkey [%constant xv=atom]]
              [y=xkey [%constant yv=atom]]
          ==
      =^  res  st  (collate-option st [[xv x] ~ ~] [[yv y] ~ ~])
      (joint st x y [%option res])
    ::
    ++  optn-optn
      |=  $:  st=xtable
              [x=xkey [%option xm=(map atom xkey)]]
              [y=xkey [%option ym=(map atom xkey)]]
          ==
      =^  res  st  (collate-option st xm ym)
      (joint st x y [%option res])
    ::
    ++  tall-conj
      |=  $:  st=xtable
              [x=xkey xrole]
              [y=xkey [%conjunction ywide=xkey ytall=xkey]]
              ==
      =^  new-tall  st  (merge st x ytall)
      (joint st ywide new-tall [%conjunction ywide new-tall])
    ::
    ++  atom-junc
      |=  $:  st=xtable
              [x=xkey xrole]
              [y=xkey [%junction yflat=xkey ydeep=xkey]]
          ==
      =^  flat-merged  st  (merge st x yflat)
      (joint st flat-merged ydeep [%junction flat-merged ydeep])
    ::
    ++  cell-junc
      |=  $:  st=xtable
              [x=xkey xrole]
              [y=xkey [%junction yflat=xkey ydeep=xkey]]
          ==
      =^  deep-merged  st  (merge st x ydeep)
      (joint st yflat deep-merged [%junction yflat deep-merged])
    ::
    ++  conj-conj
      |=  $:  st=xtable
              [x=xkey [%conjunction xwide=xkey xtall=xkey]]
              [y=xkey [%conjunction ywide=xkey ytall=xkey]]
          ==
      =^  new-wide  st  (merge st xwide ywide)
      =^  new-tall  st  (merge st xtall ytall)
      (joint st new-wide new-tall [%conjunction new-wide new-tall])
    ::
    --
  --
::
::  Convert an `ximage` to a spec for printing.
::
++  ximage-to-spec
  |=  [=top=xkey img=xtable]
  ^-  spec
  ::
  |^  (xray-to-spec ~ top-xkey)
  ::
  +$  trace  (set xkey)
  ::
  ++  xray-to-spec
    |=  [tr=trace i=xkey]
    ^-  spec
    =/  x=xray  (focus-on img i)
    =/  d=xdat  (need xdat.x)
    ?:  (~(has in tr) i)  [%loop (synthetic i)]
    ?^  recipes.x  (recipe-to-spec tr n.recipes.x)
    %+  wrap-with-loop-binding  x
    =.  tr  (~(put in tr) i)
    ^-  spec
    ?@  d  [%base d]
    ?-  -.d
      %atom  ?~  constant.d  [%base %atom aura.d]
             ?:  &(=(%n aura.d) =(`@`0 u.constant.d))  [%base %null]
             [%leaf aura.d u.constant.d]
      %cell  =/  hd  `spec`$(i head.d)
             =/  tl  `spec`$(i tail.d)
             =/  both-basic  &(=([%base %noun] hd) =([%base %noun] tl))
             ?:  both-basic      [%base %cell]
             ?:  ?=(%bscl -.tl)  [%bscl hd +.tl]
             [%bscl hd tl ~]
      %core  =/  payld  $(i xray.d)
             =/  batt   ^-  (map term spec)
                        %-  ~(run by (flatten-battery batt.d))
                        |=  =xkey  ^$(i xkey)
             ?-  r.garb.d
               %lead  [%bszp payld batt]
               %gold  [%bsdt payld batt]
               %zinc  [%bstc payld batt]
               %iron  [%bsnt payld batt]
             ==
      %pntr  !!
      %face  =/  =spec  $(i xray.d)
             ?^(face.d spec [%bsts face.d spec])
      %fork  =/  =xrole  (need xrole.x)
             |^  ?+  xrole
                     ~&  [%unexpected-fork-xrole xkey.x d xrole choices]
                     [%bswt choices]
                   %noun             [%base %noun]
                   %void             [%base %void]
                   [%option *]       [%bswt choices]
                   [%union *]        [%bscn choices]
                   [%misjunction *]  [%bswt choices]
                   [%junction *]     :+  %bsvt
                                       ^$(i flat.xrole)
                                     ^$(i deep.xrole)
                   [%conjunction *]  :+  %bskt
                                       ^$(i wide.xrole)
                                     ^$(i tall.xrole)
                 ==
             ::
             ++  choices
               ^-  [i=spec t=(list spec)]
               =-  ?>(?=(^ -) -)
               (turn ~(tap in set.d) |=(=xkey ^^$(i xkey)))
             --
    ==
  ::
  ::  If this xray references itself, generate a $$ binding in the output
  ::  spec, and then we can just reference ourselves by name.
  ::
  ++  wrap-with-loop-binding
    |=  [xr=xray sp=spec]
    ^-  spec
    ?.  (need loop.xr)  sp
    =/  nm  (synthetic xkey.xr)
    [%bsbs [%loop nm] [[nm sp] ~ ~]]
  ::
  ::  If we have a `recipe`, we can generate much nicer output.
  ::
  ++  recipe-to-spec
    |=  [tr=trace r=recipe]
    ^-  spec
    ?-  -.r
      %direct     [%like [term.r ~] ~]
      %synthetic  =/  subs  %+  turn  list.r
                            |=  =xkey  (xray-to-spec tr xkey)
                  [%make [%limb term.r] subs]
    ==
  ::
  ::  Generate symbols to be used for loop references.
  ::
  ::  given a small atom (:number), construct a coresponding symbol
  ::  using the Hebrew alphabet.
  ::
  ++  synthetic
    |=  number=@ud
    ^-  @tas
    =/  alf/(list term)
        ^~  :~  %alf  %bet  %gim  %dal  %hej  %vav  %zay  %het
                %tet  %yod  %kaf  %lam  %mem  %nun  %sam  %ayn
                %pej  %sad  %qof  %res  %sin  %tav
            ==
    ?:  (lth number 22)
      (snag number alf)
    (cat 3 (snag (mod number 22) alf) $(number (div number 22)))
  ::
  ::  Batteries in a `spec` do not have chapters, so we just ignore
  ::  the chapters and flatten the whole battery down to `(map term xkey)`.
  ::
  ++  flatten-battery
    |=  batt=(batt-of xkey)
    ^-  (map term xkey)
    =/  chapters  ~(tap by batt)
    |-  ^-  (map term xkey)
    ?~  chapters  ~
    (~(uni by q.q.i.chapters) $(chapters t.chapters))
  ::
  --
--
