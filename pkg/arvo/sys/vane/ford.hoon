::  ford: build system vane
!:
::    Ford is a functional reactive build system.
::
::    A Ford build is a function of the Urbit namespace and a date that
::    produces marked, typed data or an error.
::
::    The function in the definition of a build is called a "schematic,"
::    and it's represented by a Hoon data structure with twenty-five sub-types.
::    A schematic is a (possibly trivial) DAG of sub-builds to be performed.
::    The different schematic sub-types transform the results of their
::    sub-builds in different ways.
::
::    We call the date in the definition of a build the "formal date" to
::    distinguish it from the time at which the build was performed.
::
::    Each build is referentially transparent with respect to its formal date:
::    ask to run that function on the namespace and a particular formal date,
::    and Ford will always produce the same result.
::
::    We can now say Ford is a functional build system, since each build is a
::    function. We have not yet explained how it's a functional reactive build
::    system. With Ford, you can subscribe to results of a build. Ford tracks
::    the result of a "live" build consisting of a static schematic and the
::    ever-changing current date. Whenever this live build's result changes,
::    Ford sends you the new result and the formal date of the build (the date
::    which would cause the same result if you asked Ford to build that
::    schematic again). This is a push-based FRP paradigm.
::
::    The implementation is event-driven, like the rest of Urbit. While
::    performing a build, Ford registers each namespace access as a dependency
::    and also notes whether the dependency is "live," meaning the path within
::    the namespace updates with time. For example a live Clay dependency would
::    update the +case within the +beam over time.
::
::    A request to perform a build without subscribing to its future changes is
::    called a "once build."
::
::    After finishing a build, Ford subscribes to updates on the build's
::    dependencies. For now, this just means it subscribes to Clay for file
::    changes. Whenever any of the files in the subscription have new contents,
::    Clay will notify Ford, which will then rerun any live builds that depend
::    on any of the changed files and send its subscribers the new results.
::
::    This matches the semantics of live builds defined above. If someone had
::    asked for a build of the schematic with a formal date d2 just before the
::    changed Clay files, Ford would respond with the result of the previous
::    build with formal date d1, which would still be an accurate
::    representation of the schematic's result at d2, since Ford knows none of
::    its dependencies changed between d1 and d2.
::
::    Note that Ford can only calculate dependencies after running a build,
::    not before. This is because Ford can be thought of as an interpreter for
::    schematics, rather than a compiler, in the sense that it can't have a
::    dependency-gathering step followed by a build step. The dependencies of
::    some schematics must be calculated based on results, e.g. the %alts
::    schematic, which tries a sequence of sub-builds until one succeeds. If
::    the first sub-build succeeds, the build depends only on that first
::    sub-build, but if the first fails and the second succeeds, the build
::    depends on both.
::
::    This dynamicity implies we don't know what we depend on until we depend
::    on it. Most build systems have this property, but this part of Ford's
::    job is easier than for most Unix-based build systems: Ford draws all
::    resources from an immutable namespace, and it can track every access of
::    that namespace.
::
::    Ford might produce a build's result asynchronously, in a subsequent Arvo
::    event. This happens when accessing the namespace doesn't complete
::    synchronously, such as when grabbing a file from another ship. Ford
::    guarantees it will respond with build results in chronological order
::    using the formal date, not the order in which the builds completed.
::
::    Ford does not guarantee it will notify a subscriber of a changed build
::    only once per change. In common usage it will not send duplicate
::    notifications, but it might if its cache was recently wiped.
::
::    Ford uses dependency tracking, caching, and results of previous builds
::    to eliminate excess work. When rerunning a live build, Ford "promotes"
::    previous results to the new time if the build's dependencies hvaen't
::    changed since the previous build's formal date. Ford does this check
::    for each build in a tree of sub-builds under the "root build," which
::    is the build that was requested directly.
::
::    In addition to the main %build +task sub-type, Ford also supports
::    four other commands:
::
::    %kill: cancel a build
::
::      A once build in progress will be canceled, including all of its
::      sub-builds that aren't part of any other builds.
::
::      A live build's subscriptions will be canceled, its completed results
::      will be deleted, and its dependency tracking information will be
::      deleted. If a rebuild is in progress, it will be canceled.
::
::    %keep: resize caches
::
::      Ford maintains two caches: a :compiler-cache that stores
::      content-addressed compiler operations, such as parsing, compiling,
::      and type inference; and a :build-cache that stores previously
::      completed build trees along with their results and dependency tracking.
::
::      The %keep command resets the maximum sizes of these caches, deleting
::      entries if necessary.
::
::    %wipe: decimate storage
::
::      The %wipe command removes build results from storage to free memory.
::      It deletes the specified percentage of build results, in LRU
::      (Least Recently Used) order. It also removes entries from the compiler
::      cache. It does not remove dependency tracking information.
::
::    %wegh: report memory usage
::
::      Like all vanes, Ford can also be asked to produce a human-readable
::      report of its memory usage. Nock cannot calculate its own memory use
::      directly, so instead we produce the nouns themselves, which the runtime
::      "weighs" based on its memory model.
::
::    For details on Ford's implementation, consult Ford's vane interface core
::    near the bottom of the file.
::
::  pit: a +vase of the hoon+zuse kernel, which is a deeply nested core
::
|=  pit=vase
::
=,  contain
=,  ford
::  ford internal data structures
::
=>  =~
|%
::  +move: arvo moves that ford can emit
::
+=  move
  ::
  $:  ::  duct: request identifier
      ::
      =duct
      ::  card: move contents; either a +note or a +gift:able
      ::
      card=(wind note gift:able)
  ==
::  +note: private request from ford to another vane
::
+$  note
  $~  [%c %warp *@p *riff:clay]
  $%  ::  %c: to clay
      ::
      $:  %c
          ::  %warp: internal (intra-ship) file request
          ::
          $>(%warp task:able:clay)
  ==  ==
::  +sign: private response from another vane to ford
::
+$  sign
  $~  [%c %writ *riot:clay]
  $?  ::  %c: from clay
      ::
      ::    XX also from behn due to %slip asynchronicity
      ::
      $:  ?(%b %c)
          $>  $?  ::  %writ: internal (intra-ship) file response
                  ::
                  %writ
                  ::  %wris: response to %mult; many changed files
                  ::
                  %wris
              ==
          gift:able:clay
  ==  ==
--
|%
::  +axle: overall ford state, tagged by version
::
+=  axle
  $%  [%~2018.12.13 state=ford-state]
      [%~2020.2.21 state=ford-state]
  ==
::  +ford-state: all state that ford maintains
::
+=  ford-state
  $:  ::  builds: per-build state machine for all builds
      ::
      ::    Ford holds onto all in-progress builds that were either directly
      ::    requested by a duct (root builds) or that are dependencies
      ::    (sub-builds) of a directly requested build.
      ::
      ::    It also stores the last completed version of each live build tree
      ::    (root build and sub-builds), and any cached builds.
      ::
      builds=(map build build-status)
      ::  ducts: per-duct state machine for all incoming ducts (build requests)
      ::
      ::    Ford tracks every duct that has requested a build until it has
      ::    finished dealing with that request.
      ::
      ::    For live ducts, we store the duct while we repeatedly run new
      ::    versions of the live build it requested until it is explicitly
      ::    canceled by the requester.
      ::
      ::    A once (non-live) duct, on the other hand, will be removed
      ::    as soon as the requested build has been completed.
      ::
      ducts=(map duct duct-status)
      ::  builds-by-schematic: all attempted builds, sorted by time
      ::
      ::    For each schematic we've attempted to build at any time,
      ::    list the formal dates of all build attempts, sorted newest first.
      ::
      builds-by-schematic=(map schematic (list @da))
      ::  pending-scrys: outgoing requests for static resources
      ::
      pending-scrys=(request-tracker scry-request)
      ::  pending-subscriptions: outgoing subscriptions on live resources
      ::
      pending-subscriptions=(request-tracker subscription)
      ::  build-cache: fifo queue of completed root builds
      ::
      $=  build-cache
      $:  ::  next-anchor-id: incrementing identifier for cache anchors
          ::
          next-anchor-id=@ud
          ::  queue: fifo queue of root builds identified by anchor id
          ::
          queue=(capped-queue build-cache-key)
      ==
      ::  compiler-cache: clock based cache of build results
      ::
      compiler-cache=(clock compiler-cache-key build-result)
  ==
::  +anchor: something which holds on to builds
::
::    An anchor is a reference which keeps builds. This is either a %duct, in
::    which case the build is live because a duct is waiting for a response, or
::    a %cache, in which case the anchor is a cached build.
::
::    When a duct would be removed from a build, the %duct anchor is replaced
::    with a %cache anchor. This %cache anchor refers to a FIFO queue of cached
::    builds.
::
+=  anchor
  $%  ::  %duct: this is anchored on a duct
      ::
      [%duct =duct]
      ::  %cache: this is anchored to a cache entry
      ::
      [%cache id=@ud]
  ==
::  +build-status: current data for a build, including construction status
::
::    +build-status stores the construction status of a build as a finite state
::    machine (:state). It stores links to dependent sub-builds in :subs, and
::    per-duct client builds in :clients.
::
+=  build-status
  $:  ::  requesters: ducts for whom this build is the root build
      ::
      requesters=(set anchor)
      ::  clients: per duct information for this build
      ::
      clients=(jug anchor build)
      ::  subs: sub-builds of this build, for whom this build is a client
      ::
      subs=(map build build-relation)
      ::  state: a state machine for tracking the build's progress
      ::
      $=  state
      $%  $:  ::  %untried: build has not been started yet
              ::
              %untried  ~
          ==
          $:  ::  %blocked: build blocked on either sub-builds or resource
              ::
              ::    If we're in this state and there are no blocks in :subs,
              ::    then we're blocked on a resource.
              ::
              %blocked  ~
          ==
          $:  ::  %unblocked: we were blocked but now we aren't
              ::
              %unblocked  ~
          ==
          $:  ::  %complete: build has finished running and has a result
              ::
              %complete
              ::  build-record: the product of the build, possibly tombstoned
              ::
              =build-record
  ==  ==  ==
::  +duct-status: information relating a build to a duct
::
+=  duct-status
  $:  ::  live: whether this duct is being run live
      ::
      $=  live
      $%  [%once in-progress=@da]
          $:  %live
              ::
              ::
              in-progress=(unit @da)
              ::  the last subscription we made
              ::
              ::    This can possibly have an empty set of resources, in which
              ::    we never sent a move.
              ::
              ::    NOTE: This implies that a single live build can only depend
              ::    on live resources from a single disc. We don't have a
              ::    working plan for fixing this and will need to think very
              ::    hard about the future.
              ::
              last-sent=(unit [date=@da subscription=(unit subscription)])
      ==  ==
      ::  root-schematic: the requested build for this duct
      ::
      root-schematic=schematic
  ==
::  +build-relation: how do two builds relate to each other?
::
::    A +build-relation can be either :verified or not, and :blocked or not.
::    It is a symmetric relation between two builds, in the sense that both
::    the client and the sub will store the same relation, just pointing to
::    the other build.
::
::    If it's not :verified, then the relation is a guess based on previous
::    builds. These guesses are used to ensure that we hold onto builds we
::    expect to be used in future builds. Each time we run +make on a build,
::    it might produce new :verified sub-builds, which may have been unverified
::    until then. Once a build completes, any unverified sub-builds must be
::    cleaned up, since it turned out they weren't used by the build after all.
::
::    :blocked is used to note that a build can't be completed until that
::    sub-build has been completed. A relation can be :blocked but not :verified
::    if we're trying to promote a build, but we haven't run all its sub-builds
::    yet. In that case, we'll try to promote or run the sub-build in order to
::    determine whether we can promote the client. Until the sub-build has been
::    completed, the client is provisionally blocked on the sub-build.
::
+=  build-relation
  $:  ::  verified: do we know this relation is real, or is it only a guess?
      ::
      verified=?
      ::  is this build blocked on this other build?
      ::
      blocked=?
  ==
::  +build-record: information associated with the result of a completed +build
::
+=  build-record
  $%  $:  ::  %tombstone: the build's result has been wiped
          ::
          %tombstone  ~
      ==
      $:  ::  %value: we have the +build-result
          ::
          %value
          ::  last-accessed: last time we looked at the result
          ::
          ::    This is used for LRU cache reclamation.
          ::
          last-accessed=@da
          ::  build-result: the stored value of the build's product
          ::
          =build-result
  ==  ==
::  +build: a referentially transparent request for a build
::
::    Each unique +build will always produce the same +build-result
::    when run (if it completes). A live build consists of a sequence of
::    instances of +build with the same :schematic and increasing :date.
::
+=  build
  $:  ::  date: the formal date of this build; unrelated to time of execution
      ::
      date=@da
      ::  schematic: the schematic that determines how to run this build
      ::
      =schematic
  ==
::  +request-tracker: generic tracker and multiplexer for pending requests
::
++  request-tracker
  |*  request-type=mold
  %+  map  request-type
  $:  ::  waiting: ducts blocked on this request
      ::
      waiting=(set duct)
      ::  originator: the duct that kicked off the request
      ::
      originator=duct
  ==
::  +subscription: a single subscription to changes on a set of resources
::
+=  subscription
  $:  ::  date: date this was made
      ::
      date=@da
      ::  disc: ship and desk for all :resources
      ::
      =disc
      ::  resources: we will be notified if any of these resources change
      ::
      resources=(set resource)
  ==
::  +scry-request: parsed arguments to a scry operation
::
+=  scry-request
  $:  ::  vane: the vane from which to make the request
      ::
      ::    If we add other vanes in the future, this will become a fork type.
      ::    For now, though, Ford only knows how to make asynchronous scry
      ::    requests to Clay.
      ::
      vane=%c
      ::  care: type of request
      ::
      care=care:clay
      ::  beam: request path
      ::
      =beam
  ==
::  +compiler-cache-key: content addressable build definitions
::
+=  compiler-cache-key
  $%  [%call gate=vase sample=vase]
      [%hood =beam txt=@t]
      [%ride formula=hoon subject=vase]
      [%slim subject-type=type formula=hoon]
      [%slit gate=type sample=type]
  ==
::  +build-cache-key: key for the fifo cache of completed build trees
::
+=  build-cache-key
  $:  ::  id: incrementing identifier for an +anchor
      ::
      id=@ud
      ::  root-build: the root build associated with this anchor
      ::
      root-build=build
  ==
::  +build-receipt: result of running +make
::
::    A +build-receipt contains all information necessary to perform the
::    effects and state mutations indicated by a call to +make. If :build
::    succeeded, :result will be %build-result; otherwise, it will be %blocks.
::
::    After +make runs on a batch of builds, the resulting +build-receipt's are
::    applied one at a time.
::
+=  build-receipt
  $:  ::  build: the build we worked on
      ::
      =build
      ::  result: the outcome of this build
      ::
      $=  result
      $%  ::  %build-result: the build produced a result
          ::
          $:  %build-result
              =build-result
          ==
          ::  %blocks: the build blocked on the following builds or resource
          ::
          $:  %blocks
              ::  builds: builds that :build blocked on
              ::
              builds=(list build)
          ==
      ==
      ::  sub-builds: subbuilds of :build
      ::
      ::    While running +make on :build, we need to keep track of any
      ::    sub-builds that we try to access so we can keep track of
      ::    component linkages and cache access times.
      ::
      sub-builds=(list build)
      ::  cache-access: if not ~, cache this result as :compiler-cache-key.
      ::
      cache-access=(unit [=compiler-cache-key new=?])
  ==
--
=,  format
|%
::  +tear: split a +term into segments delimited by `-`
::
::  Example:
::  ```
::  dojo> (tear 'foo-bar-baz')
::  ['foo' 'bar' 'baz']
::  ```
::
++  tear
  |=  a=term
  ^-  (list term)
  ::  sym-no-heps: a parser for terms with no heps and a leading letter
  ::
  =/  sym-no-heps  (cook crip ;~(plug low (star ;~(pose low nud))))
  ::
  (fall (rush a (most hep sym-no-heps)) /[a])
::  +segments: compute all paths from :path-part, replacing some `/`s with `-`s
::
::    For example, when passed a :path-part of 'foo-bar-baz',
::    the product will contain:
::    ```
::    dojo> (segments 'foo-bar-baz')
::    [/foo/bar/baz /foo/bar-baz /foo-bar/baz /foo-bar-baz]
::    ```
::
++  segments
  |=  path-part=@tas
  ^-  (list path)
  ::
  =/  join  |=([a=@tas b=@tas] (crip "{(trip a)}-{(trip b)}"))
  ::
  =/  torn=(list @tas)  (tear path-part)
  ::
  |-  ^-  (list (list @tas))
  ::
  ?<  ?=(~ torn)
  ::
  ?:  ?=([@ ~] torn)
    ~[torn]
  ::
  %-  zing
  %+  turn  $(torn t.torn)
  |=  s=(list @tas)
  ^-  (list (list @tas))
  ::
  ?>  ?=(^ s)
  ~[[i.torn s] [(join i.torn i.s) t.s]]
::  +build-to-tape: convert :build to a printable format
::
::    Builds often contain the standard library and large types, so
::    this function should always be called when trying to print a +build.
::
++  build-to-tape
  |=  =build
  ^-  tape
  ~+
  ::
  =/  enclose  |=(tape "[{+<}]")
  =/  date=@da  date.build
  =/  =schematic  schematic.build
  ::
  %-  enclose
  %+  welp  (trip (scot %da date))
  %+  welp  " "
  ::
  ?+      -.schematic
        :(welp "[" (trip -.schematic) " {<`@uvI`(mug schematic)>}]")
      ::
      %$
    "literal"
  ::
      ^
    %-  enclose
    ;:(welp $(build [date head.schematic]) " " $(build [date tail.schematic]))
  ::
      %alts
    ;:  welp
      %+  roll  choices.schematic
      |=  [choice=^schematic txt=_"[alts"]
      :(welp txt " " ^$(schematic.build choice))
    ::
      "]"
    ==
  ::
      %core
    :(welp "[core " (spud (en-beam (rail-to-beam source-path.schematic))) "]")
  ::
      %hood
    :(welp "[hood " (spud (en-beam (rail-to-beam source-path.schematic))) "]")
  ::
      %plan
    ;:  welp
      "[plan "
      (spud (en-beam (rail-to-beam path-to-render.schematic)))
      "]"
    ==
  ::
      %scry
    (spud (en-beam (extract-beam resource.schematic ~)))
  ::
    ::    %slim
    ::  "slim {<subject-type.schematic>} {<formula.schematic>}"
  ::
      %vale
    ;:  welp
      "[vale ["
      (trip (scot %p ship.disc.schematic))
      " "
      (trip desk.disc.schematic)
      "] "
      (trip mark.schematic)
      "]"
    ==
  ==
::  +rail-to-beam: convert :rail to a +beam, filling in the case with `[%ud 0]`
::
++  rail-to-beam
  |=  =rail
  ^-  beam
  [[ship.disc.rail desk.disc.rail [%ud 0]] spur.rail]
::  +rail-to-path: pretty-printable rail
::
++  rail-to-path
  |=  =rail
  ^-  path
  (en-beam (rail-to-beam rail))
::  +unify-jugs: make a new jug, unifying sets for all keys
::
::    Example:
::    ```
::    dojo> %+  unify-jugs
::            (~(gas by *(jug @tas @ud)) ~[[%a (sy 1 2 ~)] [%b (sy 4 5 ~)]])
::          (~(gas by *(jug @tas @ud)) ~[[%b (sy 5 6 ~)] [%c (sy 7 8 ~)]])
::
::    {[p=%a q={1 2 3}] [p=%b q={4 5 6}] [p=%c q={7 8}]}
::    ```
::
++  unify-jugs
  |*  [a=(jug) b=(jug)]
  ^+  a
  ::
  =/  tapped  ~(tap by b)
  ::
  |-  ^+  a
  ?~  tapped  a
  ::
  =/  key  p.i.tapped
  =/  vals  ~(tap in q.i.tapped)
  ::
  =.  a
    |-  ^+  a
    ?~  vals  a
    ::
    $(vals t.vals, a (~(put ju a) key i.vals))
  ::
  $(tapped t.tapped)
::  +path-to-resource: decode a +resource from a +wire
::
++  path-to-resource
  |=  =path
  ^-  (unit resource)
  ::
  =/  scry-request=(unit scry-request)  (path-to-scry-request path)
  ?~  scry-request
    ~
  =+  [vane care bem]=u.scry-request
  =/  =beam  bem
  =/  =rail  [disc=[p.beam q.beam] spur=s.beam]
  `[vane care rail]
::  +scry-request-to-path: encode a +scry-request in a +wire
::
::    Example:
::    ```
::    dojo> %-  scry-request-to-path
::          [%c %x [[~zod %home [%da ~2018.1.1]] /hoon/bar]])
::
::    /cx/~zod/home/~2018.1.1/bar/hoon
::    ```
::
++  scry-request-to-path
  |=  =scry-request
  ^-  path
  =/  =term  (cat 3 [vane care]:scry-request)
  [term (en-beam beam.scry-request)]
::  +path-to-scry-request: parse :path's components into :vane, :care, and :rail
::
++  path-to-scry-request
  |=  =path
  ^-  (unit scry-request)
  ::
  ?~  path
    ~
  ?~  vane=((soft ,%c) (end 3 1 i.path))
    ~
  ?~  care=((soft care:clay) (rsh 3 1 i.path))
    ~
  ?~  beam=(de-beam t.path)
    ~
  ?.  ?=(%da -.r.u.beam)
    ~
  `[u.vane u.care u.beam]
::  +scry-request-to-build: convert a +scry-request to a %scry build
::
++  scry-request-to-build
  |=  =scry-request
  ^-  build
  ::  we only operate on dates, not other kinds of +case:clay
  ::
  ?>  ?=(%da -.r.beam.scry-request)
  ::
  =,  scry-request
  [p.r.beam [%scry [vane care `rail`[[p q] s]:beam]]]
::  +extract-beam: obtain a +beam from a +resource
::
::    Fills case with [%ud 0] for live resources if :date is `~`.
::    For once resources, ignore :date.
::
++  extract-beam
  |=  [=resource date=(unit @da)]  ^-  beam
  ::
  =/  =case  ?~(date [%ud 0] [%da u.date])
  ::
  =,  rail.resource
  [[ship.disc desk.disc case] spur]
::  +extract-disc: obtain a +disc from a +resource
::
++  extract-disc
  |=  =resource  ^-  disc
  disc.rail.resource
::  +get-sub-schematics: find any schematics contained within :schematic
::
++  get-sub-schematics
  |=  =schematic
  ^-  (list ^schematic)
  ?-    -.schematic
      ^      ~[head.schematic tail.schematic]
      %$     ~
      %pin   ~[schematic.schematic]
      %alts  choices.schematic
      %bake  ~
      %bunt  ~
      %call  ~[gate.schematic sample.schematic]
      %cast  ~[input.schematic]
      %core  ~
      %diff  ~[start.schematic end.schematic]
      %dude  ~[attempt.schematic]
      %hood  ~
      %join  ~[first.schematic second.schematic]
      %list  schematics.schematic
      %mash  ~[schematic.first.schematic schematic.second.schematic]
      %mute  [subject.schematic (turn mutations.schematic tail)]
      %pact  ~[start.schematic diff.schematic]
      %path  ~
      %plan  ~
      %reef  ~
      %ride  ~[subject.schematic]
      %same  ~[schematic.schematic]
      %scry  ~
      %slim  ~
      %slit  ~
      %vale  ~
      %volt  ~
      %walk  ~
  ==
::  +by-schematic: door for manipulating :by-schematic.builds.ford-state
::
::    The :dates list for each key in :builds is sorted in reverse
::    chronological order. These operations access and mutate keys and values
::    of :builds and maintain that sort order.
::
++  by-schematic
  |_  builds=(map schematic (list @da))
  ::  +put: add a +build to :builds
  ::
  ::    If :build already exists in :builds, this is a no-op.
  ::    Otherwise, replace the value at the key :schematic.build
  ::    with a new :dates list that contains :date.build.
  ::
  ++  put
    |=  =build
    ^+  builds
    %+  ~(put by builds)  schematic.build
    ::
    =/  dates  (~(gut by builds) schematic.build ~)
    |-
    ^+  dates
    ?~  dates
      [date.build ~]
    ?:  =(i.dates date.build)
      dates
    ?:  (gth date.build i.dates)
      [date.build dates]
    [i.dates $(dates t.dates)]
  ::  +del: remove a +build from :builds
  ::
  ::    Removes :build from :builds by replacing the value at
  ::    the key :schematic.build with a new :dates list with
  ::    :date.build omitted. If the resulting :dates list is
  ::    empty, then remove the key-value pair from :builds.
  ::
  ++  del
    |=  =build
    ^+  builds
    =.  builds
      %+  ~(jab by builds)  schematic.build
      |=  dates=(list @da)
      ~|  build+build
      =/  date-index  (need (find [date.build]~ dates))
      (oust [date-index 1] dates)
    ::  if :builds has an empty entry for :build, delete it
    ::
    =?    builds
        =(~ (~(got by builds) schematic.build))
      (~(del by builds) schematic.build)
    ::
    builds
  ::  +find-previous: find the most recent older build with :schematic.build
  ::
  ++  find-previous
    |=  =build
    ^-  (unit ^build)
    ::
    =/  dates=(list @da)  (~(gut by builds) schematic.build ~)
    ::
    |-  ^-  (unit ^build)
    ?~  dates  ~
    ::
    ?:  (lth i.dates date.build)
      `[i.dates schematic.build]
    $(dates t.dates)
  ::  +find-next: find the earliest build of :schematic.build later than :build
  ::
  ++  find-next
    |=  =build
    ^-  (unit ^build)
    ::
    =/  dates=(list @da)  (flop (~(gut by builds) schematic.build ~))
    ::
    |-  ^-  (unit ^build)
    ?~  dates  ~
    ::
    ?:  (gth i.dates date.build)
      `[i.dates schematic.build]
    $(dates t.dates)
  --
::  +get-request-ducts: all ducts waiting on this request
::
++  get-request-ducts
  |*  [tracker=(request-tracker) request=*]
  ^-  (list duct)
  ::
  ?~  val=(~(get by tracker) request)
    ~
  ~(tap in waiting.u.val)
::  +put-request: associates a +duct with a request
::
++  put-request
  |*  [tracker=(request-tracker) request=* =duct]
  ::
  %+  ~(put by tracker)  request
  ?~  existing=(~(get by tracker) request)
    [(sy duct ~) duct]
  u.existing(waiting (~(put in waiting.u.existing) duct))
::  +del-request: remove a duct and produce the originating duct if empty
::
++  del-request
  |*  [tracker=(request-tracker) request=* =duct]
  ^-  [(unit ^duct) _tracker]
  ::  remove :duct from the existing :record of this :request
  ::
  =/  record  (~(got by tracker) request)
  =.  waiting.record  (~(del in waiting.record) duct)
  ::  if no more ducts wait on :request, delete it
  ::
  ?^  waiting.record
    [~ (~(put by tracker) request record)]
  [`originator.record (~(del by tracker) request)]
::  +parse-scaffold: produces a parser for a hoon file with +crane instances
::
::    Ford parses a superset of hoon which contains additional runes to
::    represent +crane s. This parses to a +scaffold.
::
::    src-beam: +beam of the source file we're parsing
::
++  parse-scaffold
  |=  src-beam=beam
  ::
  =/  hoon-parser  (vang & (en-beam src-beam))
  |^  ::
      %+  cook
        |=  a=[@ud (list ^cable) (list ^cable) (list ^crane) (list hoon)]
        ^-  scaffold
        [[[p q] s]:src-beam a]
      ::
      %+  ifix  [gay gay]
      ;~  plug
      ::  parses the zuse version, eg "/?  309"
      ::
        ;~  pose
          (ifix [;~(plug net wut gap) gap] dem)
          (easy zuse)
        ==
      ::  pareses the structures, eg "/-  types"
      ::
        ;~  pose
          (ifix [;~(plug net hep gap) gap] (most ;~(plug com gaw) cable))
          (easy ~)
        ==
      ::  parses the libraries, eg "/+  lib1, lib2"
      ::
        ;~  pose
          (ifix [;~(plug net lus gap) gap] (most ;~(plug com gaw) cable))
          (easy ~)
        ==
      ::
        (star ;~(sfix crane gap))
      ::
        (most gap tall:hoon-parser)
      ==
  ::  +beam: parses a hood path and converts it to a beam
  ::
  ++  beam
    %+  sear  de-beam
    ;~  pfix
      net
      (sear plex (stag %clsg poor)):hoon-parser
    ==
  ::  +cable: parses a +^cable, a reference to something on the filesystem
  ::
  ::    This parses:
  ::
  ::      `library`       ->  wraps `library` around the library `library`
  ::      `face=library`  ->  wraps `face` around the library `library`
  ::      `*library`      ->  exposes `library` directly to the subject
  ::
  ++  cable
    %+  cook  |=(a=^cable a)
    ;~  pose
      (stag ~ ;~(pfix tar sym))
      (cook |=([face=term tis=@ file=term] [`face file]) ;~(plug sym tis sym))
      (cook |=(a=term [`a a]) sym)
    ==
  ::  +crane: all runes that start with / which aren't /?, /-, /+ or //.
  ::
  ++  crane
    =<  apex
    ::  whether we allow tall form
    =|  allow-tall-form=?
    ::
    |%
    ++  apex
      %+  knee  *^crane  |.  ~+
      ;~  pfix  net
        ;~  pose
          ::  `/~`  hoon literal
          ::
          (stag %fssg ;~(pfix sig hoon))
          ::  `/$`  process query string
          ::
          (stag %fsbc ;~(pfix bus hoon))
          ::  `/|`  first of many options that succeeds
          ::
          (stag %fsbr ;~(pfix bar parse-alts))
          ::  `/=`  wrap a face around a crane
          ::
          (stag %fsts ;~(pfix tis parse-face))
          ::  `/.`  null terminated list
          ::
          (stag %fsdt ;~(pfix dot parse-list))
          ::  `/,`  switch by path
          ::
          (stag %fscm ;~(pfix com parse-switch))
          ::  `/&`  pass through a series of mark
          ::
          (stag %fspm ;~(pfix pad parse-pipe))
          ::  `/_`  run a crane on each file in the current directory
          ::
          (stag %fscb ;~(pfix cab subcrane))
          ::  `/;`  passes date through a gate
          ::
          (stag %fssm ;~(pfix mic parse-gate))
          ::  `/:`  evaluate at path
          ::
          (stag %fscl ;~(pfix col parse-at-path))
          ::  `/^`  cast
          ::
          (stag %fskt ;~(pfix ket parse-cast))
          ::  `/*`  run a crane on each file with current path as prefix
          ::
          (stag %fstr ;~(pfix tar subcrane))
          ::  `/!mark/ evaluate as hoon, then pass through mark
          ::
          (stag %fszp ;~(pfix zap ;~(sfix sym net)))
          ::  `/mark/` passes current path through :mark
          ::
          (stag %fszy ;~(sfix sym net))
        ==
      ==
    ::  +parse-alts: parse a set of alternatives
    ::
    ++  parse-alts
      %+  wide-or-tall
        (ifix [lit rit] (most ace subcrane))
      ;~(sfix (star subcrane) gap duz)
    ::  +parse-face: parse a face around a subcrane
    ::
    ++  parse-face
      %+  wide-or-tall
        ;~(plug sym ;~(pfix tis subcrane))
      ;~(pfix gap ;~(plug sym subcrane))
    ::  +parse-list: parse a null terminated list of cranes
    ::
    ++  parse-list
      %+  wide-or-tall
        fail
      ;~(sfix (star subcrane) gap duz)
    ::  +parse-switch: parses a list of [path crane]
    ::
    ++  parse-switch
      %+  wide-or-tall
        fail
      =-  ;~(sfix (star -) gap duz)
      ;~(pfix gap net ;~(plug static-path subcrane))
    ::  +parse-pipe: parses a pipe of mark conversions
    ::
    ++  parse-pipe
      %+  wide-or-tall
        ;~(plug (plus ;~(sfix sym pad)) subcrane)
      =+  (cook |=(a=term [a ~]) sym)
      ;~(pfix gap ;~(plug - subcrane))
    ::  +parse-gate: parses a gate applied to a crane
    ::
    ++  parse-gate
      %+  wide-or-tall
        ;~(plug ;~(sfix wide:hoon-parser mic) subcrane)
      ;~(pfix gap ;~(plug tall:hoon-parser subcrane))
    ::  +parse-at-path: parses a late bound bath
    ::
    ++  parse-at-path
      %+  wide-or-tall
        ;~(plug ;~(sfix late-bound-path col) subcrane)
      ;~(pfix gap ;~(plug late-bound-path subcrane))
    ::  +parse-cast: parses a mold and then the subcrane to apply that mold to
    ::
    ++  parse-cast
      %+  wide-or-tall
        ;~(plug ;~(sfix wyde:hoon-parser ket) subcrane)
      ;~(pfix gap ;~(plug till:hoon-parser subcrane))
    ::  +subcrane: parses a subcrane
    ::
    ++  subcrane
      %+  wide-or-tall
        apex(allow-tall-form |)
      ;~(pfix gap apex)
    ::  +wide-or-tall: parses tall form hoon if :allow-tall-form is %.y
    ::
    ++  wide-or-tall
      |*  [wide=rule tall=rule]
      ?.  allow-tall-form  wide
      ;~(pose wide tall)
    ::  +hoon: parses hoon as an argument to a crane
    ::
    ++  hoon
      %+  wide-or-tall
        (ifix [lac rac] (stag %cltr (most ace wide:hoon-parser)))
      ;~(pfix gap tall:hoon-parser)
    --
  ::  +static-path: parses a path
  ::
  ++  static-path
    (sear plex (stag %clsg (more net hasp))):hoon-parser
  ::  +late-bound-path: a path whose time varies
  ::
  ++  late-bound-path
    ;~  pfix  net
      %+  cook  |=(a=truss a)
      =>  hoon-parser
      ;~  plug
        (stag ~ gash)
        ;~(pose (stag ~ ;~(pfix cen porc)) (easy ~))
      ==
    ==
  --
::  +per-event: per-event core; main build engine
::
::    This arm produces a gate that when called with state and event
::    information produces the core of Ford's main build engine.
::
::    The main build engine core has the following entry points:
::
::      +start-build  start performing a build
::      +rebuild      rerun a live build at a new date
::      +unblock      continue a build that was waiting on a resource
::      +cancel       stop trying to run a build and delete its tracking info
::      +wipe         wipe the build storage to free memory
::      +keep         resize caches, deleting entries if necessary
::
::    The main internal arm is +execute-loop, which is called from +start-build,
::    +rebuild, and +unblock. +execute defines Ford's build loop.
::
++  per-event
  ::  moves: the moves to be sent out at the end of this event, reversed
  ::
  =|  moves=(list move)
  ::  scry-results: responses to scry's to handle in this event
  ::
  ::    If a value is `~`, the requested resource is not available.
  ::    Otherwise, the value will contain a +cage.
  ::
  =|  scry-results=(map scry-request (unit cage))
  ::  next-builds: builds to perform in the next iteration
  ::
  =|  next-builds=(set build)
  ::  candidate-builds: builds which might go into next-builds
  ::
  =|  candidate-builds=(set build)
  ::  gate that produces the +per-event core from event information
  ::
  ::    Produces a core containing Ford's main build engine.
  ::
  ~%  %f  ..is  ~
  |=  [[our=@p =duct now=@da scry=sley] state=ford-state]
  ::
  ~%  %per-event  +  ~
  |%
  ::  +finalize: extract moves and state from the +per-event core
  ::
  ::    Run once at the end of processing an event.
  ::
  ++  finalize
    ^-  [(list move) ford-state]
    [(flop moves) state]
  ::  |entry-points: externally fired arms
  ::
  ::+|  entry-points
  ::
  ::  +start-build: perform a fresh +build, either live or once
  ::
  ::    This might complete the build, or the build might block on one or more
  ::    requests for resources. Calls +execute-loop.
  ::
  ++  start-build
    ~/  %start-build
    |=  [=build live=?]
    ^-  [(list move) ford-state]
    ::
    =<  finalize
    ::  associate :duct with :build in :ducts.state
    ::
    =.  ducts.state
      %+  ~(put by ducts.state)  duct
      :_  schematic.build
      ?:  live
        [%live in-progress=`date.build last-sent=~]
      [%once in-progress=date.build]
    ::  register a state machine for :build in :builds.state
    ::
    =.  state  (add-build build)
    ::  :anchor: the reason we hold onto the root of this build tree
    ::
    =/  =anchor  [%duct duct]
    ::  register :duct as an anchor in :requesters.build-status
    ::
    ::    This establishes :build as the root build for :duct.
    ::
    =.  builds.state
      %+  ~(jab by builds.state)  build
      |=  =build-status
      build-status(requesters (~(put in requesters.build-status) anchor))
    ::  copy :anchor into any preexisting descendants
    ::
    ::    Sub-builds will reference :build in their :clients.build-status,
    ::    using `[%duct duct]` as the key. Some sub-builds might already
    ::    exist if we've already started running :build, so make sure they
    ::    know who their daddy is.
    ::
    =.  builds.state  (add-anchor-to-subs anchor build)
    ::  run +execute on :build in a loop until it completes or blocks
    ::
    (execute-loop (sy [build ~]))
  ::  +rebuild: rebuild a live build based on +resource updates
  ::
  ::    For every changed resource, run the %scry build for that
  ::    for that resource. Then rebuild upward using the main +execute-loop
  ::    until all relevant builds either complete or block on external
  ::    resources. Use dependency tracking information from the previous
  ::    run of this live build to inform the dependency tracking for this
  ::    new rebuild.
  ::
  ++  rebuild
    ~/  %rebuild
    |=  $:  =subscription
            new-date=@da
            =disc
            care-paths=(set [care=care:clay =path])
        ==
    ^-  [(list move) ford-state]
    ::
    ~|  [%rebuilding new-date disc]
    ::
    =<  finalize
    ::  mark this subscription as complete now that we've heard a response
    ::
    =.  pending-subscriptions.state
      +:(del-request pending-subscriptions.state subscription duct)
    ::  for every changed resource, create a %scry build
    ::
    =/  builds=(list build)
      %+  turn  ~(tap in care-paths)
      |=  [care=care:clay =path]
      ^-  build
      ::
      [new-date [%scry [%c care rail=[disc spur=(flop path)]]]]
    ::  sanity check; only rebuild live builds, not once builds
    ::
    =/  duct-status  (~(got by ducts.state) duct)
    ?>  ?=(%live -.live.duct-status)
    ::  sanity check; only rebuild once we've completed the previous one
    ::
    ?>  ?=(~ in-progress.live.duct-status)
    ?>  ?=(^ last-sent.live.duct-status)
    ::  set the in-progress date for this new build
    ::
    =.  ducts.state
      %+  ~(put by ducts.state)  duct
      duct-status(in-progress.live `new-date)
    ::  copy the previous build's tree as provisional sub-builds
    ::
    ::    This provides an upward rebuild path from leaves to root,
    ::    so that once the %scry builds complete, we'll know to rebuild
    ::    their clients. This process will continue up through rebuilding
    ::    the root build.
    ::
    ::    If the build at this new date ends up with a different set of
    ::    dependencies from its previous incarnation, provisional sub-builds
    ::    that weren't actually used will be removed in
    ::    +cleanup-orphaned-provisional-builds.
    ::
    =/  old-root=build
      [date.u.last-sent.live.duct-status root-schematic.duct-status]
    ::
    =.  state
      ::
      ~|  [%duct-doesnt-refer-to-real-build live.duct-status]
      ~|  [%missing-build (build-to-tape old-root)]
      ~|  [%dates (~(get by builds-by-schematic.state) root-schematic.duct-status)]
      ?>  (~(has by builds.state) old-root)
      ::
      (copy-build-tree-as-provisional old-root new-date=new-date)
    ::  gather all the :builds, forcing reruns
    ::
    ::    The normal +gather logic would promote the previous results
    ::    for these %scry builds, since we have subscriptions on them.
    ::    We pass `force=%.y` to ensure the builds get enqueued instead
    ::    of promoted.
    ::
    =.  ..execute  (gather (sy builds) force=%.y)
    ::  rebuild resource builds at the new date
    ::
    ::    This kicks off the main build loop, which will first build
    ::    :builds, then rebuild upward toward the root. If the whole
    ::    build tree completes synchronously, then this will produce
    ::    %made moves at the end of this event. Otherwise, it will
    ::    block on resources and complete during a later event.
    ::
    (execute-loop ~)
  ::  +unblock: continue builds that had blocked on :resource
  ::
  ::    A build can be stymied temporarily if it depends on a resource
  ::    that must be fetched asynchronously. +unblock is called when
  ::    we receive a response to a resource request that blocked a build.
  ::
  ::    We pick up the build from where we left off, starting with the
  ::    %scry build that blocked on this resource last time we tried it.
  ::
  ++  unblock
    ~/  %unblock
    |=  [=scry-request scry-result=(unit cage)]
    ^-  [(list move) ford-state]
    ::
    =<  finalize
    ::  place :scry-result in :scry-results.per-event
    ::
    ::    We don't want to call the actual +scry function again,
    ::    because we already tried that in a previous event and it
    ::    had no synchronous answer. This +unblock call is a result
    ::    of the response to the asynchronous request we made to
    ::    retrieve that resource from another vane.
    ::
    ::    Instead, we'll intercept any calls to +scry by looking up
    ::    the arguments in :scry-results.per-event. This is ok because
    ::    in this function we attempt to run every +build that had
    ::    blocked on the resource, so the information is guaranteed
    ::    to be used during this event before it goes out of scope.
    ::
    =.  scry-results  (~(put by scry-results) scry-request scry-result)
    ::  mark this +scry-request as complete now that we have a response
    ::
    =.  pending-scrys.state
      +:(del-request pending-scrys.state scry-request duct)
    ::  update :unblocked-build's state machine to reflect its new status
    ::
    =/  unblocked-build=build  (scry-request-to-build scry-request)
    =.  builds.state
      %+  ~(jab by builds.state)  unblocked-build
      |=  =build-status
      build-status(state [%unblocked ~])
    ::  jump into the main build loop, starting with :unblocked-build
    ::
    (execute-loop (sy unblocked-build ~))
  ::  +wipe: forcibly decimate build results from the state
  ::
  ::    +wipe decimates both the :compiler-cache and the results in
  ::    :builds.state. It removes the specified percentage of build results
  ::    from the state. For simplicity, it considers the weight of each
  ::    compiler cache line to be equal to the weight of a build result.
  ::
  ::    It deletes cache entries before dipping into :builds.state; it only
  ::    converts entries in :builds.state to %tombstone's if there aren't
  ::    enough entries in the compiler cache to sate the request's bloodlust.
  ::
  ::    When deleting results from :builds.state, it first sorts them by
  ::    their :last-accessed date so that the stalest builds are deleted first.
  ::    We do not touch the :build-cache directly, but because the results
  ::    of the builds in :build-cache live in :builds.state, the results of
  ::    both FIFO-cached builds and active builds are all sorted and trimmed.
  ::
  ++  wipe
    ~/  %wipe
    |=  percent-to-remove=@ud
    ^+  state
    ::  removing 0% is the same as doing nothing, so do nothing
    ::
    ?:  =(0 percent-to-remove)
      ~&  %wipe-no-op
      state
    ::
    ~|  [%wipe percent-to-remove=percent-to-remove]
    ?>  (lte percent-to-remove 100)
    ::  find all completed builds, sorted by :last-accessed date
    ::
    =/  completed-builds=(list build)
      =-  (turn - head)
      %+  sort
        ::  filter for builds with a stored +build-result
        ::
        %+  skim  ~(tap by builds.state)
        |=  [=build =build-status]
        ^-  ?
        ::
        ?=([%complete %value *] state.build-status)
      ::  sort by :last-accessed date
      ::
      |=  [[* a=build-status] [* b=build-status]]
      ^-  ?
      ::
      ?>  ?=([%complete %value *] state.a)
      ?>  ?=([%complete %value *] state.b)
      ::
      %+  lte
        last-accessed.build-record.state.a
      last-accessed.build-record.state.b
    ::  determine how many builds should remain after decimation
    ::
    ::    This formula has the property that repeated applications
    ::    of +wipe with anything other than 100% retention rate will
    ::    always eventually remove every build.
    ::
    =/  num-completed-builds=@ud
      (add (lent completed-builds) size.compiler-cache.state)
    =/  percent-to-keep=@ud  (sub 100 percent-to-remove)
    =/  num-to-keep=@ud  (div (mul percent-to-keep num-completed-builds) 100)
    =/  num-to-remove=@ud  (sub num-completed-builds num-to-keep)
    ::
    |^  ^+  state
        ::
        =+  cache-size=size.compiler-cache.state
        ?:  (lte num-to-remove cache-size)
          (remove-from-cache num-to-remove)
        =.  compiler-cache.state
          %~  purge
            (by-clock compiler-cache-key build-result)
          compiler-cache.state
        (tombstone-builds (sub num-to-remove cache-size))
    ::
    ++  remove-from-cache
      |=  count=@ud
      %_    state
          compiler-cache
        %-  %~  trim
                (by-clock compiler-cache-key build-result)
                compiler-cache.state
        count
      ==
    ::
    ++  tombstone-builds
      |=  num-to-remove=@ud
      ::
      ~|  [%wipe num-to-remove=num-to-remove]
      ::  the oldest :num-to-remove builds are considered stale
      ::
      =/  stale-builds  (scag num-to-remove completed-builds)
      ::  iterate over :stale-builds, replacing with %tombstone's
      ::
      |-  ^+  state
      ?~  stale-builds  state
      ::  replace the build's entry in :builds.state with a %tombstone
      ::
      =.  builds.state
        =<  builds
        %+  update-build-status  i.stale-builds
        |=  =build-status
        build-status(state [%complete %tombstone ~])
      ::
      $(stale-builds t.stale-builds)
    --
  ::  +keep: resize caches
  ::
  ::    Ford maintains two caches: a :build-cache for caching previously
  ::    completed build trees, and a :compiler-cache for caching various
  ::    compiler operations that tend to be shared among multiple builds.
  ::
  ::    To handle this command, we reset the maximum sizes of both of
  ::    these caches, removing entries from the caches if necessary.
  ::
  ++  keep
    ~/  %keep
    |=  [compiler-cache-size=@ud build-cache-size=@ud]
    ^+  state
    ::  pop old builds out of :build-cache and remove their cache anchors
    ::
    =^  pops  queue.build-cache.state
      %.  build-cache-size
      ~(resize (to-capped-queue build-cache-key) queue.build-cache.state)
    ::
    =.  state
      |-  ^+  state
      ?~  pops  state
      ::
      =.  state  (remove-anchor-from-root root-build.i.pops [%cache id.i.pops])
      ::
      $(pops t.pops)
    ::  resize the :compiler-cache
    ::
    %_    state
        compiler-cache
      %-  %~  resize
              (by-clock compiler-cache-key build-result)
              compiler-cache.state
      compiler-cache-size
    ==
  ::  +cancel: cancel a build
  ::
  ::    When called on a live build, removes all tracking related to the live
  ::    build, and no more %made moves will be sent for that build.
  ::
  ::    When called on a once build, removes all tracking related to the once
  ::    build, and that build will never be completed or have a %made sent.
  ::
  ::    When called on a build that isn't registered in :state, such as a
  ::    completed once build, or a build that has already been canceled,
  ::    prints and no-ops.
  ::
  ++  cancel  ^+  [moves state]
    ::
    =<  finalize
    ::
    ?~  duct-status=(~(get by ducts.state) duct)
      ~&  [%no-build-for-duct duct]
      ..execute
    ::  :duct is being canceled, so remove it unconditionally
    ::
    =.  ducts.state  (~(del by ducts.state) duct)
    ::  if the duct was not live, cancel any in-progress builds
    ::
    ?:  ?=(%once -.live.u.duct-status)
      ::
      =/  root-build=build  [in-progress.live root-schematic]:u.duct-status
      ::
      =.  ..execute  (cancel-scrys root-build)
      =.  state  (remove-anchor-from-root root-build [%duct duct])
      ..execute
    ::  if the duct was live and has an unfinished build, cancel it
    ::
    =?  ..execute  ?=(^ in-progress.live.u.duct-status)
      ::
      =/  root-build=build  [u.in-progress.live root-schematic]:u.duct-status
      ::
      =.  ..execute  (cancel-scrys root-build)
      =.  state  (remove-anchor-from-root root-build [%duct duct])
      ..execute
    ::  if there is no completed build for the live duct, we're done
    ::
    ?~  last-sent=last-sent.live.u.duct-status
      ..execute
    ::  there is a completed build for the live duct, so delete it
    ::
    =/  root-build=build  [date.u.last-sent root-schematic.u.duct-status]
    ::
    =.  state  (remove-anchor-from-root root-build [%duct duct])
    ::
    ?~  subscription.u.last-sent
      ..execute
    (cancel-clay-subscription u.subscription.u.last-sent)
  ::  +cancel-scrys: cancel all blocked %scry sub-builds of :root-builds
  ::
  ++  cancel-scrys
    |=  root-build=build
    ^+  ..execute
    ::
    =/  blocked-sub-scrys  ~(tap in (collect-blocked-sub-scrys root-build))
    ::
    |-  ^+  ..execute
    ?~  blocked-sub-scrys  ..execute
    ::
    =.  ..execute  (cancel-scry-request i.blocked-sub-scrys)
    ::
    $(blocked-sub-scrys t.blocked-sub-scrys)
  ::  +move-root-to-cache: replace :duct with a %cache anchor in :build's tree
  ::
  ++  move-root-to-cache
    ~/  %move-root-to-cache
    |=  =build
    ^+  state
    ::  obtain the new cache id and increment the :next-anchor-id in the state
    ::
    =^  new-id  next-anchor-id.build-cache.state
      =/  id=@ud  next-anchor-id.build-cache.state
      [id +(id)]
    ::  replace the requester in the root build
    ::
    =.  builds.state
      %+  ~(jab by builds.state)  build
      |=  =build-status
      %_    build-status
          requesters
        =-  (~(del in -) [%duct duct])
        =-  (~(put in -) [%cache new-id])
        requesters.build-status
      ==
    ::  enqueue :build into cache, possibly popping and deleting a stale build
    ::
    =^  oldest  queue.build-cache.state
      %.  [new-id build]
      ~(put (to-capped-queue build-cache-key) queue.build-cache.state)
    ::
    =?    state
        ?=(^ oldest)
      (remove-anchor-from-root root-build.u.oldest [%cache id.u.oldest])
    ::  recursively replace :clients in :build and descendants
    ::
    |-  ^+  state
    ::
    =/  client-status=build-status  (got-build build)
    =/  subs=(list ^build)  ~(tap in ~(key by subs.client-status))
    ::
    |-  ^+  state
    ?~  subs  state
    ::
    =.  builds.state
      %+  ~(jab by builds.state)  i.subs
      |=  =build-status
      %_    build-status
          clients
        ::  if we've already encountered :i.subs, don't overwrite
        ::
        ?:  (~(has by clients.build-status) [%cache new-id])
          clients.build-status
        ::
        =/  old-clients-on-duct  (~(get ju clients.build-status) [%duct duct])
        ::
        =-  (~(del by -) [%duct duct])
        =-  (~(put by -) [%cache new-id] old-clients-on-duct)
        clients.build-status
      ==
    ::
    =.  state  ^$(build i.subs)
    ::
    $(subs t.subs)
  ::  +remove-anchor-from-root: remove :anchor from :build's tree
  ::
  ++  remove-anchor-from-root
    ~/  %remove-anchor-from-root
    |=  [=build =anchor]
    ^+  state
    ::
    =.  builds.state
      %+  ~(jab by builds.state)  build
      |=  =build-status
      build-status(requesters (~(del in requesters.build-status) anchor))
    ::
    =.  builds.state  (remove-anchor-from-subs build anchor)
    ::
    (cleanup build)
  ::  +remove-anchor-from-subs: recursively remove :anchor from sub-builds
  ::
  ++  remove-anchor-from-subs
    ~/  %remove-anchor-from-subs
    |=  [=build =anchor]
    ^+  builds.state
    ::
    =/  =build-status  (got-build build)
    =/  subs=(list ^build)  ~(tap in ~(key by subs.build-status))
    =/  client=^build  build
    ::
    |-  ^+  builds.state
    ?~  subs  builds.state
    ::
    =/  sub-status=^build-status  (got-build i.subs)
    ::
    =.  clients.sub-status
      (~(del ju clients.sub-status) anchor client)
    ::
    =.  builds.state  (~(put by builds.state) i.subs sub-status)
    ::
    =?  builds.state  !(~(has by clients.sub-status) anchor)
      ::
      ^$(build i.subs)
    ::
    $(subs t.subs)
  ::  +add-anchors-to-build-subs: for each sub, add all of :build's anchors
  ::
  ++  add-anchors-to-build-subs
    ~/  %add-anchors-to-build-subs
    |=  =build
    ^+  state
    ::
    =/  =build-status  (got-build build)
    =/  new-anchors
      ~(tap in (~(put in ~(key by clients.build-status)) [%duct duct]))
    =/  subs  ~(tap in ~(key by subs.build-status))
    ::
    =.  state
      |-
      ^+  state
      ?~  subs  state
      ::
      =.  state  (add-build i.subs)
      ::
      $(subs t.subs)
    ::
    =.  builds.state
      |-  ^+  builds.state
      ?~  new-anchors  builds.state
      ::
      =.  builds.state  (add-anchor-to-subs i.new-anchors build)
      ::
      $(new-anchors t.new-anchors)
    ::
    state
  ::  +add-anchor-to-subs: attach :duct to :build's descendants
  ::
  ++  add-anchor-to-subs
    ~/  %add-anchor-to-subs
    |=  [=anchor =build]
    ^+  builds.state
    ::
    =/  =build-status  (got-build build)
    =/  subs=(list ^build)  ~(tap in ~(key by subs.build-status))
    =/  client=^build  build
    ::
    |-  ^+  builds.state
    ?~  subs  builds.state
    ::
    =/  sub-status=^build-status  (got-build i.subs)
    ::
    =/  already-had-anchor=?  (~(has by clients.sub-status) anchor)
    ::
    =.  clients.sub-status
      (~(put ju clients.sub-status) anchor client)
    ::
    =.  builds.state  (~(put by builds.state) i.subs sub-status)
    ::
    =?  builds.state  !already-had-anchor  ^$(build i.subs)
    ::
    $(subs t.subs)
  ::  +copy-build-tree-as-provisional: prepopulate new live build
  ::
  ::    Make a provisional copy of the completed old root build tree at the
  ::    :new time.
  ::
  ++  copy-build-tree-as-provisional
    ~/  %copy-build-tree-as-provisional
    |=  [old-root=build new-date=@da]
    ^+  state
    ~|  [old-root=(build-to-tape old-root) new-date=new-date]
    ::
    =/  old-client=build  old-root
    =/  new-client=build  old-client(date new-date)
    =.  state  (add-build new-client)
    ::
    =.  builds.state
      %+  ~(jab by builds.state)  new-client
      |=  =build-status
      build-status(requesters (~(put in requesters.build-status) [%duct duct]))
    ::
    =<  copy-node
    ::
    |%
    ++  copy-node
      ^+  state
      ::
      =/  old-build-status=build-status  (got-build old-client)
      ::
      =/  old-subs=(list build)  ~(tap in ~(key by subs.old-build-status))
      =/  new-subs=(list build)  (turn old-subs |=(a=build a(date new-date)))
      ::
      =.  builds.state
        (add-subs-to-client new-client new-subs [verified=%.n blocked=%.y])
      ::
      |-
      ^+  state
      ?~  old-subs
        state
      ::
      =.  state  (add-client-to-sub i.old-subs)
      =.  state
        copy-node(old-client i.old-subs, new-client i.old-subs(date new-date))
      ::
      $(old-subs t.old-subs)
    ::
    ++  add-client-to-sub
      |=  old-sub=build
      ^+  state
      ::
      =/  new-sub  old-sub(date new-date)
      =.  state  (add-build new-sub)
      ::
      =.  builds.state
        %+  ~(jab by builds.state)  new-sub
        |=  =build-status
        %_  build-status
          clients  (~(put ju clients.build-status) [%duct duct] new-client)
        ==
      ::
      state
    --
  ::  +add-subs-to-client: register :new-subs as subs of :new-client
  ::
  ++  add-subs-to-client
    ~/  %add-subs-to-client
    |=  [new-client=build new-subs=(list build) =build-relation]
    ^+  builds.state
    ::
    %+  ~(jab by builds.state)  new-client
    |=  =build-status
    %_    build-status
        subs
      %-  ~(gas by subs.build-status)
      %+  murn  new-subs
      |=  sub=build
      ^-  (unit (pair build ^build-relation))
      ::
      ?^  (~(get by subs.build-status) sub)
        ~
      `[sub build-relation]
    ==
  ::  |construction: arms for performing builds
  ::
  ::+|  construction
  ::
  ::  +execute-loop: +execute repeatedly until there's no more work to do
  ::
  ::    Keep running +execute until all relevant builds either complete or
  ::    block on external resource requests. See +execute for details of each
  ::    loop execution.
  ::
  ::    This implementation is for simplicity. In the longer term, we'd
  ::    like to just perform a single run through +execute and set a Behn timer
  ::    to wake us up immediately. This has the advantage that Ford stops hard
  ::    blocking the main Urbit event loop, letting other work be done.
  ::
  ++  execute-loop  !.
    ~/  %execute-loop
    |=  builds=(set build)
    ^+  ..execute
    ::
    =.  ..execute  (execute builds)
    ::
    ?:  ?&  ?=(~ next-builds)
            ?=(~ candidate-builds)
        ==
      ..execute
    ::
    $(builds ~)
  ::  +execute: main recursive construction algorithm
  ::
  ::    Performs the three step build process: First, figure out which builds
  ::    we're going to run this loop through the ford algorithm. Second, run
  ::    the gathered builds, possibly in parallel. Third, apply the
  ::    +build-receipt algorithms to the ford state.
  ::
  ++  execute
    ~/  %execute
    |=  builds=(set build)
    ^+  ..execute
    ::
    =.  ..execute  (gather builds force=%.n)
    ::
    =^  build-receipts  ..execute  run-builds
    ::
    (reduce build-receipts)
  ::  +gather: collect builds to be run in a batch
  ::
  ::    The +gather phase is the first of the three parts of +execute. In
  ::    +gather, we look through each item in :candidate-builds.  If we
  ::    should run the candidate build this cycle through the +execute loop, we
  ::    place it in :next-builds. +gather runs until it has no more candidates.
  ::
  ++  gather  !.
    ~/  %gather
    |=  [builds=(set build) force=?]
    ^+  ..execute
    ::  add builds that were triggered by incoming event to the candidate list
    ::
    =.  candidate-builds  (~(uni in candidate-builds) builds)
    ::
    |^  ^+  ..execute
        ::
        ?:  =(~ candidate-builds)
          ..execute
        ::
        =/  next=build
          ?<  ?=(~ candidate-builds)
          n.candidate-builds
        =.  candidate-builds  (~(del in candidate-builds) next)
        ::
        $(..execute (gather-build next))
    ::  +gather-build: looks at a single candidate build
    ::
    ::    This gate inspects a single build. It might move it to :next-builds,
    ::    or promote it using an old build. It also might add this build's
    ::    sub-builds to :candidate-builds.
    ::
    ++  gather-build
      |=  =build
      ^+  ..execute
      ~|  [%duct duct]
      =/  duct-status  (~(got by ducts.state) duct)
      ::  if we already have a result for this build, don't rerun the build
      ::
      =^  current-result  builds.state  (access-build-record build)
      ::
      ?:  ?=([~ %value *] current-result)
        (on-build-complete build)
      ::  place :build in :builds.state if it isn't already there
      ::
      =.  state  (add-build build)
      ::  ignore blocked builds
      ::
      =/  =build-status  (got-build build)
      ?:  ?=(%blocked -.state.build-status)
        =.  state  (add-anchors-to-build-subs build)
        ::
        =/  sub-scrys=(list scry-request)
          ~(tap in (collect-blocked-sub-scrys build))
        ::
        =.  pending-scrys.state
          |-  ^+  pending-scrys.state
          ?~  sub-scrys  pending-scrys.state
          ::
          =.  pending-scrys.state
            (put-request pending-scrys.state i.sub-scrys duct)
          ::
          $(sub-scrys t.sub-scrys)
        ::
        ..execute
      ::  old-build: most recent previous build with :schematic.build
      ::
      =/  old-build=(unit ^build)
        ?:  ?&  ?=(%live -.live.duct-status)
                ?=(^ last-sent.live.duct-status)
            ==
          ::  check whether :build was run as part of the last live build tree
          ::
          ::    If we had build this schematic as part of the build tree
          ::    during the last run of this live build, then we can compare
          ::    our result to that build. It might not be the most recent,
          ::    but if our sub-builds have the same results as they did then,
          ::    we can promote them. This is especially helpful for a %scry
          ::    build, because we don't have to make a new request for the
          ::    resource if the last live build subscribed to it.
          ::
          ::    Otherwise, default to looking up the most recent build of this
          ::    schematic in :builds-by-schematic.state. We'll have to rerun
          ::    any %scry sub-builds, but other than that, we should still be
          ::    able to promote its result if its sub-builds have the same
          ::    results as ours.
          ::
          =/  possible-build=^build
            [date.u.last-sent.live.duct-status schematic.build]
          ?:  (~(has by builds.state) possible-build)
            `possible-build
          (~(find-previous by-schematic builds-by-schematic.state) build)
        (~(find-previous by-schematic builds-by-schematic.state) build)
      ::  if no previous builds exist, we need to run :build
      ::
      ?~  old-build
        (add-build-to-next build)
      ::
      =/  old-build-status=^build-status  (got-build u.old-build)
      ::  selectively promote scry builds
      ::
      ::    We can only promote a scry if it's not forced and we ran the same
      ::    scry schematic as a descendant of the root build schematic at the
      ::    last sent time for this duct.
      ::
      ?:  ?&  ?=(%scry -.schematic.build)
              ?|  force
                  ?!
                  ?&  ?=(%live -.live.duct-status)
                      ?=(^ last-sent.live.duct-status)
                  ::
                      =/  subscription=(unit subscription)
                        subscription.u.last-sent.live.duct-status
                      ::
                      ?~  subscription
                        %.n
                      %-  ~(has in resources.u.subscription)
                      resource.schematic.build
          ==  ==  ==
        (add-build-to-next build)
      ::  if we don't have :u.old-build's result cached, we need to run :build
      ::
      =^  old-build-record  builds.state  (access-build-record u.old-build)
      ?.  ?=([~ %value *] old-build-record)
        (add-build-to-next build)
      ::
      =.  old-build-status  (got-build u.old-build)
      ::
      =/  old-subs=(list ^build)  ~(tap in ~(key by subs.old-build-status))
      =/  new-subs=(list ^build)
        (turn old-subs |=(^build +<(date date.build)))
      ::  link sub-builds provisionally, blocking on incomplete
      ::
      ::    We don't know that :build will end up depending on :new-subs,
      ::    so they're not :verified.
      ::
      =/  split-new-subs
        %+  skid  new-subs
        |=  sub=^build
        ^-  ?
        ::
        ?~  maybe-build-status=(~(get by builds.state) sub)
          %.n
        ::
        ?&  ?=(%complete -.state.u.maybe-build-status)
            ?=(%value -.build-record.state.u.maybe-build-status)
        ==
      ::
      =/  stored-new-subs=(list ^build)     -.split-new-subs
      =/  un-stored-new-subs=(list ^build)  +.split-new-subs
      ::
      =.  builds.state
        (add-subs-to-client build stored-new-subs [verified=%.n blocked=%.n])
      =.  builds.state
        (add-subs-to-client build un-stored-new-subs [verified=%.n blocked=%.y])
      ::
      =.  state  (add-anchors-to-build-subs build)
      ::
      ?^  un-stored-new-subs
        ::  enqueue incomplete sub-builds to be promoted or run
        ::
        ::    When not all our sub builds have results, we can't add :build to
        ::    :next-builds.state. Instead, put all the remaining uncached new
        ::    subs into :candidate-builds.
        ::
        ::    If all of our sub-builds finish immediately (i.e. promoted) when
        ::    they pass through +gather-internal, they will add :build back to
        ::    :candidate-builds and we will run again before +execute runs
        ::    +make.
        ::
        %_    ..execute
            candidate-builds
          (~(gas in candidate-builds) un-stored-new-subs)
        ==
      ::
      =^  promotable  builds.state  (are-subs-unchanged old-subs new-subs)
      ?.  promotable
        (add-build-to-next build)
      ::
      ?>  =(schematic.build schematic.u.old-build)
      ?>  (~(has by builds.state) build)
      (promote-build u.old-build date.build new-subs)
    ::  +are-subs-unchanged: checks sub-build equivalence, updating access time
    ::
    ++  are-subs-unchanged
      |=  [old-subs=(list build) new-subs=(list build)]
      ^-  [? _builds.state]
      ::
      ?~  old-subs
        [%.y builds.state]
      ?>  ?=(^ new-subs)
      ::
      =^  old-build-record  builds.state  (access-build-record i.old-subs)
      ?.  ?=([~ %value *] old-build-record)
        [%.n builds.state]
      ::
      =^  new-build-record  builds.state  (access-build-record i.new-subs)
      ?.  ?=([~ %value *] new-build-record)
        [%.n builds.state]
      ::
      ?.  =(build-result.u.old-build-record build-result.u.new-build-record)
        [%.n builds.state]
      $(new-subs t.new-subs, old-subs t.old-subs)
    ::  +add-build-to-next: run this build during the +make phase
    ::
    ++  add-build-to-next
      |=  =build
      ..execute(next-builds (~(put in next-builds) build))
    ::  +promote-build: promote result of :build to newer :date
    ::
    ::    Also performs relevant accounting, and possibly sends %made moves.
    ::
    ++  promote-build
      |=  [old-build=build new-date=@da new-subs=(list build)]
      ^+  ..execute
      ::  grab the previous result, freshening the cache
      ::
      =^  old-build-record  builds.state  (access-build-record old-build)
      ::  we can only promote a cached result, not missing or a %tombstone
      ::
      ?>  ?=([~ %value *] old-build-record)
      =/  =build-result  build-result.u.old-build-record
      ::  :new-build is :old-build at :date; promotion destination
      ::
      =/  new-build=build  old-build(date new-date)
      ::
      =.  builds.state
        %+  ~(jab by builds.state)  new-build
        |=  =build-status
        ^+  build-status
        ::
        %_    build-status
        ::  verify linkages between :new-build and subs
        ::
            subs
          ::
          ^-  (map build build-relation)
          %-  my
          ^-  (list (pair build build-relation))
          %+  turn  new-subs
          |=  sub=build
          ::
          [sub [verified=& blocked=|]]
        ::  copy the old result to :new-build
        ::
            state
          [%complete [%value last-accessed=now build-result=build-result]]
        ==
      ::
      (on-build-complete new-build)
    --
  ::  +run-builds: run the builds and produce +build-receipts
  ::
  ::    Runs the builds and cleans up the build lists afterwards.
  ::
  ::    When the vere interpreter has a parallel variant of +turn, use
  ::    that as each build might take a while and there are no data
  ::    dependencies between builds here. For now, though, run them serially.
  ::
  ++  run-builds
    =<  $
    ~%  %run-builds  +  ~
    |.
    ^-  [(list build-receipt) _..execute]
    ::
    =/  build-receipts=(list build-receipt)
      (turn ~(tap in next-builds) make)
    ::
    =.  next-builds  ~
    [build-receipts ..execute]
  ::  reduce: apply +build-receipts produce from the +make phase.
  ::
  ::    +gather produces builds to run make on. +make produces
  ::    +build-receipts. It is in +reduce where we take these +build-receipts
  ::    and apply them to ..execute.
  ::
  ++  reduce  !.
    ~/  %reduce
    |=  build-receipts=(list build-receipt)
    ^+  ..execute
    ::  sort :build-receipts so blocks are processed before completions
    ::
    ::    It's possible for a build to block on a sub-build that was run
    ::    in the same batch. If that's the case, make sure we register
    ::    that the build blocked on the sub-build before registering the
    ::    completion of the sub-build. This way, when we do register the
    ::    completion of the sub-build, we will know which builds are blocked
    ::    on the sub-build, so we can enqueue those blocked clients to be
    ::    rerun.
    ::
    =.  build-receipts
      %+  sort  build-receipts
      |=  [a=build-receipt b=build-receipt]
      ^-  ?
      ?=(%blocks -.result.a)
    ::
    |^  ^+  ..execute
        ?~  build-receipts  ..execute
        ::
        =.  ..execute  (apply-build-receipt i.build-receipts)
        $(build-receipts t.build-receipts)
    ::  +apply-build-receipt: applies a single state diff to ..execute
    ::
    ++  apply-build-receipt
      |=  made=build-receipt
      ^+  ..execute
      ::  process :sub-builds.made
      ::
      =.  state  (track-sub-builds build.made sub-builds.made)
      ::
      ?-    -.result.made
          %build-result
        (apply-build-result [build build-result.result cache-access]:made)
      ::
          %blocks
        (apply-blocks [build builds.result]:made)
      ==
    ::  +track-sub-builds:
    ::
    ::    For every sub-build discovered while running :build, we have to make
    ::    sure that we track that sub-build and that it is associated with the
    ::    right ducts.
    ::
    ++  track-sub-builds
      |=  [client=build sub-builds=(list build)]
      ^+  state
      ::  mark :sub-builds as :subs in :build's +build-status
      ::
      =^  build-status  builds.state
        %+  update-build-status  client
        |=  =build-status
        %_    build-status
            subs
          %-  ~(gas by subs.build-status)
          %+  turn  sub-builds
          |=  sub=build
          ::
          =/  blocked=?
            ?~  sub-status=(~(get by builds.state) sub)
              %.y
            !?=([%complete %value *] state.u.sub-status)
          ::
          [sub [verified=& blocked]]
        ==
      ::
      =.  state  (add-anchors-to-build-subs client)
      ::
      |-  ^+  state
      ?~  sub-builds  state
      ::
      =.  builds.state
        %+  ~(jab by builds.state)  i.sub-builds
        |=  build-status=^build-status
        %_    build-status
        ::  freshen :last-accessed date
        ::
            state
          ::
          ?.  ?=([%complete %value *] state.build-status)
            state.build-status
          state.build-status(last-accessed.build-record now)
        ==
      ::
      $(sub-builds t.sub-builds)
    ::  +apply-build-result: apply a %build-result +build-receipt to ..execute
    ::
    ::    Our build produced an actual result.
    ::
    ++  apply-build-result
      |=  [=build =build-result cache-access=(unit [=compiler-cache-key new=?])]
      ^+  ..execute
      ::
      =?  compiler-cache.state  ?=(^ cache-access)
        =+  by-clock=(by-clock compiler-cache-key ^build-result)
        ?.  new.u.cache-access
          =^  ignored  compiler-cache.state
            (~(get by-clock compiler-cache.state) compiler-cache-key.u.cache-access)
          compiler-cache.state
        ::
        %+  ~(put by-clock compiler-cache.state)
          compiler-cache-key.u.cache-access
        build-result
      ::
      =.  builds.state
        %+  ~(jab by builds.state)  build
        |=  =build-status
        build-status(state [%complete [%value last-accessed=now build-result]])
      ::
      (on-build-complete build)
    ::  +apply-blocks: apply a %blocks +build-receipt to ..execute
    ::
    ::    :build blocked. Record information about what builds it blocked on
    ::    and try those blocked builds as candidates in the next pass.
    ::
    ++  apply-blocks
      |=  [=build blocks=(list build)]
      ^+  ..execute
      ::  if a %scry blocked, register it and maybe send an async request
      ::
      =?    ..execute
          ?=(~ blocks)
        ?>  ?=(%scry -.schematic.build)
        =,  resource.schematic.build
        %-  start-scry-request
        [vane care [[ship.disc.rail desk.disc.rail [%da date.build]] spur.rail]]
      ::  we must run +apply-build-receipt on :build.made before :block
      ::
      ?<  %+  lien  blocks
          |=  block=^build
          ?~  maybe-build-status=(~(get by builds.state) block)
            %.n
          ?=(%complete -.state.u.maybe-build-status)
      ::  transition :build's state machine to the %blocked state
      ::
      =.  builds.state
        %+  ~(jab by builds.state)  build
        |=  =build-status
        build-status(state [%blocked ~])
      ::  enqueue :blocks to be run next
      ::
      =.  candidate-builds  (~(gas in candidate-builds) blocks)
      ::
      ..execute
    --
  ::  +make: attempt to perform :build, non-recursively
  ::
  ::    Registers component linkages between :build and its sub-builds.
  ::    Attempts to perform +scry if necessary. Does not directly enqueue
  ::    any moves.
  ::
  ++  make
    ~/  %make
    |=  =build
    ^-  build-receipt
    ::  out: receipt to return to caller
    ::
    =|  out=build-receipt
    ::  ~&  [%turbo-make (build-to-tape build)]
    ::  dispatch based on the kind of +schematic in :build
    ::
    |^  =,  schematic.build
        ::
        =.  build.out  build
        ::
        ?-    -.schematic.build
        ::
            ^  (make-autocons [head tail])
        ::
            %$  (make-literal literal)
        ::
            %pin   (make-pin date schematic)
            %alts  (make-alts choices ~)
            %bake  (make-bake renderer query-string path-to-render)
            %bunt  (make-bunt disc mark)
            %call  (make-call gate sample)
            %cast  (make-cast disc mark input)
            %core  (make-core source-path)
            %diff  (make-diff disc start end)
            %dude  (make-dude error attempt)
            %hood  (make-hood source-path)
            %join  (make-join disc mark first second)
            %list  (make-list schematics)
            %mash  (make-mash disc mark first second)
            %mute  (make-mute subject mutations)
            %pact  (make-pact disc start diff)
            %path  (make-path disc prefix raw-path)
            %plan  (make-plan path-to-render query-string scaffold)
            %reef  (make-reef disc)
            %ride  (make-ride formula subject)
            %same  (make-same schematic)
            %scry  (make-scry resource)
            %slim  (make-slim subject-type formula)
            %slit  (make-slit gate sample)
            %vale  (make-vale disc mark input)
            %volt  (make-volt disc mark input)
            %walk  (make-walk disc source target)
        ==
    ::  |schematic-handlers:make: implementation of the schematics
    ::
    ::    All of these produce a value of the same type as +make itself.
    ::
    ::  +|  schematic-handlers
    ::
    ++  make-autocons
      ~%  %make-autocons  ..^^$  ~
      |=  [head=schematic tail=schematic]
      ^-  build-receipt
      ::
      =/  head-build=^build  [date.build head]
      =/  tail-build=^build  [date.build tail]
      =^  head-result  out  (depend-on head-build)
      =^  tail-result  out  (depend-on tail-build)
      ::
      =|  blocks=(list ^build)
      =?  blocks  ?=(~ head-result)  [head-build blocks]
      =?  blocks  ?=(~ tail-result)  [tail-build blocks]
      ::  if either build blocked, we're not done
      ::
      ?^  blocks
        ::
        (return-blocks blocks)
      ::
      ?<  ?=(~ head-result)
      ?<  ?=(~ tail-result)
      ::
      (return-result %success u.head-result u.tail-result)
    ::
    ++  make-literal
      ~%  %make-literal  ..^^$  ~
      |=  =cage
      ^-  build-receipt
      (return-result %success %$ cage)
    ::
    ++  make-pin
      ~%  %make-pin  ..^^$  ~
      |=  [date=@da =schematic]
      ^-  build-receipt
      ::  pinned-sub: sub-build with the %pin date as formal date
      ::
      =/  pinned-sub=^build  [date schematic]
      ::
      =^  result  out  (depend-on pinned-sub)
      ::
      ?~  result
        (return-blocks ~[pinned-sub])
      ::
      (return-result u.result)
    ::
    ++  make-alts
      ~%  %make-alts  ..^^$  ~
      |=  [choices=(list schematic) errors=(list tank)]
      ^-  build-receipt
      ::
      ?~  choices
        (return-error [[%leaf "%alts: all options failed"] errors])
      ::
      =/  choice=^build  [date.build i.choices]
      ::
      =^  result  out  (depend-on choice)
      ?~  result
        (return-blocks ~[choice])
      ::
      ?:  ?=([%error *] u.result)
        ::
        =/  braces  [[' ' ' ' ~] ['{' ~] ['}' ~]]
        =/  wrapped-error=tank
          [%rose braces `(list tank)`message.u.result]
        =.  errors
          (weld errors `(list tank)`[[%leaf "option"] wrapped-error ~])
        $(choices t.choices)
      ::
      (return-result %success %alts u.result)
    ::
    ++  make-bake
      ~%  %make-bake  ..^^$  ~
      |=  [renderer=term query-string=coin path-to-render=rail]
      ^-  build-receipt
      ::  path-build: find the file path for the renderer source
      ::
      =/  path-build=^build
        [date.build [%path disc.path-to-render %ren renderer]]
      ::
      =^  path-result  out  (depend-on path-build)
      ?~  path-result
        (return-blocks [path-build]~)
      ::
      |^  ^-  build-receipt
          ::  if there's a renderer called :renderer, use it on :path-to-render
          ::
          ::    Otherwise, fall back to running the contents of :path-to-render
          ::    through a mark that has the same name as :renderer.
          ::
          ?:  ?=([~ %success %path *] path-result)
            (try-renderer-then-mark rail.u.path-result)
          (try-mark ~)
      ::  +try-renderer-then-mark: try to render a path, then fall back to mark
      ::
      ++  try-renderer-then-mark
        |=  =rail
        ^-  build-receipt
        ::  build a +scaffold from the renderer source
        ::
        =/  hood-build=^build  [date.build [%hood rail]]
        ::
        =^  hood-result  out  (depend-on hood-build)
        ?~  hood-result
          (return-blocks [hood-build]~)
        ::  if we can't find and parse the renderer, try the mark instead
        ::
        ?:  ?=([~ %error *] hood-result)
          (try-mark message.u.hood-result)
        ?>  ?=([~ %success %hood *] hood-result)
        ::  link the renderer, passing through :path-to-render and :query-string
        ::
        =/  plan-build=^build
          :-  date.build
          [%plan path-to-render query-string scaffold.u.hood-result]
        ::
        =^  plan-result  out  (depend-on plan-build)
        ?~  plan-result
          (return-blocks [plan-build]~)
        ::  if compiling the renderer errors out, try the mark instead
        ::
        ?:  ?=([~ %error *] plan-result)
          (try-mark message.u.plan-result)
        ?>  ?=([~ %success %plan *] plan-result)
        ::  renderers return their name as the mark
        ::
        ::    We should rethink whether we want this to be the case going
        ::    forward, but for now, Eyre depends on this detail to work.
        ::
        (return-result [%success %bake renderer vase.u.plan-result])
      ::   +try-mark: try to cast a file's contents through a mark
      ::
      ::     :errors contains any error messages from our previous attempt to
      ::     run a renderer, if we made one. This way if both the renderer and
      ::     mark fail, the requester will see the errors of both attempts.
      ::
      ++  try-mark
        |=  errors=(list tank)
        ^-  build-receipt
        ::  no renderer, try mark; retrieve directory listing of :path-to-render
        ::
        ::    There might be multiple files of different marks stored at
        ::    :path-to-render. Retrieve the directory listing for
        ::    :path-to-render, then check which of the path segments in
        ::    that directory are files (not just folders), then for each
        ::    file try to %cast its mark to the desired mark (:renderer).
        ::
        ::    Start by retrieving the directory listing, using :toplevel-build.
        ::
        =/  toplevel-build=^build
          [date.build [%scry %c %y path-to-render]]
        ::
        =^  toplevel-result  out  (depend-on toplevel-build)
        ?~  toplevel-result
          (return-blocks [toplevel-build]~)
        ::
        ?:  ?=([~ %error *] toplevel-result)
          ::
          =/  =path  (rail-to-path path-to-render)
          ?~  errors
            %-  return-error
            :-  [%leaf "ford: %bake {<renderer>} on {<path>} failed:"]
            message.u.toplevel-result
          ::
          =/  braces  [[' ' ' ' ~] ['{' ~] ['}' ~]]
          %-  return-error  :~
            [%leaf "ford: %bake {<renderer>} on {<path>} failed:"]
            [%leaf "as-renderer"]
            [%rose braces errors]
            [%leaf "as-mark"]
            [%rose braces message.u.toplevel-result]
          ==
        ?>  ?=([~ %success %scry *] toplevel-result)
        ::
        =/  toplevel-arch=arch  ;;(arch q.q.cage.u.toplevel-result)
        ::  find the :sub-path-segments that could be files
        ::
        ::    Filter out path segments that aren't a +term,
        ::    since those aren't valid marks and therefore can't
        ::    be the last segment of a filepath in Clay.
        ::
        =/  sub-path-segments=(list @ta)
          (skim (turn ~(tap by dir.toplevel-arch) head) (sane %tas))
        ::
        =/  sub-schematics=(list [sub-path=@ta =schematic])
          %+  turn  sub-path-segments
          |=  sub=@ta
          :-  sub
          [%scry %c %y path-to-render(spur [sub spur.path-to-render])]
        ::
        =^  maybe-schematic-results  out
          %-  perform-schematics  :*
            ;:  weld
              "ford: %bake "  (trip renderer)  " on "
              (spud (rail-to-path path-to-render))  " contained failures:"
            ==
            sub-schematics
            %fail-on-errors
            *@ta
          ==
        ?~  maybe-schematic-results
          out
        ::  marks: list of the marks of the files at :path-to-render
        ::
        =/  marks=(list @tas)
          %+  murn  u.maybe-schematic-results
          |=  [sub-path=@ta result=build-result]
          ^-  (unit @tas)
          ::
          ?>  ?=([%success %scry *] result)
          ::
          =/  =arch  ;;(arch q.q.cage.result)
          ::  if it's a directory, not a file, we can't load it
          ::
          ?~  fil.arch
            ~
          [~ `@tas`sub-path]
        ::  sort marks in alphabetical order
        ::
        =.  marks  (sort marks lte)
        ::  try to convert files to the destination mark, in order
        ::
        =/  alts-build=^build
          ::
          :+  date.build  %alts
          ^=  choices  ^-  (list schematic)
          ::
          %+  turn  marks
          |=  mark=term
          ^-  schematic
          ::
          =/  file=rail  path-to-render(spur [mark spur.path-to-render])
          ::
          [%cast disc.file renderer [%scry %c %x file]]
        ::
        =^  alts-result  out  (depend-on alts-build)
        ?~  alts-result
          (return-blocks [alts-build]~)
        ::
        ?:  ?=([~ %error *] alts-result)
          =/  =path  (rail-to-path path-to-render)
          ?~  errors
            %-  return-error
            :-  [%leaf "ford: %bake {<renderer>} on {<path>} failed:"]
            message.u.alts-result
          ::
          =/  braces  [[' ' ' ' ~] ['{' ~] ['}' ~]]
          %-  return-error  :~
            [%leaf "ford: %bake {<renderer>} on {<path>} failed:"]
            [%leaf "as-renderer"]
            [%rose braces errors]
            [%leaf "as-mark"]
            [%rose braces message.u.alts-result]
          ==
        ::
        ?>  ?=([~ %success %alts *] alts-result)
        ::
        =/  =build-result
          [%success %bake (result-to-cage u.alts-result)]
        ::
        (return-result build-result)
      --
    ::
    ++  make-bunt
      ~%  %make-bunt  ..^^$  ~
      |=  [=disc mark=term]
      ^-  build-receipt
      ::  resolve path of the mark definition file
      ::
      =/  path-build=^build  [date.build [%path disc %mar mark]]
      ::
      =^  path-result  out  (depend-on path-build)
      ?~  path-result
        (return-blocks [path-build]~)
      ::
      ?:  ?=([~ %error *] path-result)
        %-  return-error
        :_  message.u.path-result
        :-  %leaf
        "ford: %bunt resolving path for {<mark>} on {<disc>} failed:"
      ::
      ?>  ?=([~ %success %path *] path-result)
      ::  build the mark core from source
      ::
      =/  core-build=^build  [date.build [%core rail.u.path-result]]
      ::
      =^  core-result  out  (depend-on core-build)
      ?~  core-result
        (return-blocks [core-build]~)
      ::
      ?:  ?=([~ %error *] core-result)
        %-  return-error
        :_  message.u.core-result
        :-  %leaf
        "ford: %bunt compiling mark {<mark>} on {<disc>} failed:"
      ::
      ?>  ?=([~ %success %core *] core-result)
      ::  extract the sample from the mark core
      ::
      =/  mark-vase=vase    vase.u.core-result
      ~|  %mark-vase
      =+  [sample-type=p sample-value=q]:(slot 6 mark-vase)
      ::  if sample is wrapped in a face, unwrap it
      ::
      =?  sample-type  ?=(%face -.sample-type)  q.sample-type
      ::
      =/  =cage  [mark sample-type sample-value]
      (return-result %success %bunt cage)
    ::
    ++  make-call
      ~%  %make-call  ..^^$  ~
      |=  [gate=schematic sample=schematic]
      ^-  build-receipt
      ::
      =/  gate-build=^build  [date.build gate]
      =^  gate-result    out  (depend-on gate-build)
      ::
      =/  sample-build=^build  [date.build sample]
      =^  sample-result  out  (depend-on sample-build)
      ::
      =|  blocks=(list ^build)
      =?  blocks  ?=(~ gate-result)    [[date.build gate] blocks]
      =?  blocks  ?=(~ sample-result)  [[date.build sample] blocks]
      ?^  blocks
        (return-blocks blocks)
      ::
      ?<  ?=(~ gate-result)
      ?:  ?=([~ %error *] gate-result)
        %-  return-error
        :-  [%leaf "ford: %call failed to build gate:"]
        message.u.gate-result
      ::
      ?<  ?=(~ sample-result)
      ?:  ?=([~ %error *] sample-result)
        %-  return-error
        :-  [%leaf "ford: %call failed to build sample:"]
        message.u.sample-result
      ::
      =/  gate-vase=vase    q:(result-to-cage u.gate-result)
      =/  sample-vase=vase  q:(result-to-cage u.sample-result)
      ::  run %slit to get the resulting type of calculating the gate
      ::
      =/  slit-schematic=schematic  [%slit gate-vase sample-vase]
      =/  slit-build=^build  [date.build slit-schematic]
      =^  slit-result  out  (depend-on slit-build)
      ?~  slit-result
        (return-blocks [date.build slit-schematic]~)
      ::
      ?:  ?=([~ %error *] slit-result)
        %-  return-error
        :-  [%leaf "ford: %call failed type calculation"]
        message.u.slit-result
      ::
      ?>  ?=([~ %success %slit *] slit-result)
      ::
      =/  =compiler-cache-key  [%call gate-vase sample-vase]
      =^  cached-result  out  (access-cache compiler-cache-key)
      ?^  cached-result
        (return-result u.cached-result)
      ::
      ?>  &(?=(^ q.gate-vase) ?=(^ +.q.gate-vase))
      =/  val
        (mong [q.gate-vase q.sample-vase] intercepted-scry)
      ::
      ?-    -.val
          %0
        (return-result %success %call [type.u.slit-result p.val])
      ::
          %1
        =/  blocked-paths=(list path)  ;;((list path) p.val)
        (blocked-paths-to-receipt %call blocked-paths)
      ::
          %2
        (return-error [[%leaf "ford: %call execution failed:"] p.val])
      ==
    ::
    ++  make-cast
      ~%  %make-cast  ..^^$  ~
      |=  [=disc mark=term input=schematic]
      ^-  build-receipt
      ::
      =/  input-build=^build  [date.build input]
      ::
      =^  input-result  out  (depend-on input-build)
      ?~  input-result
        (return-blocks [input-build]~)
      ::
      ?:  ?=([~ %error *] input-result)
        %-  return-error
        :_  message.u.input-result
        :-  %leaf
        ;:  weld
          "ford: %cast "  (trip mark)  " on ["  (trip (scot %p ship.disc))
          " "  (trip desk.disc)  "] failed on input:"
        ==
      ::
      ?>  ?=([~ %success *] input-result)
      ::
      =/  result-cage=cage  (result-to-cage u.input-result)
      ::
      =/  translation-path-build=^build
        [date.build [%walk disc p.result-cage mark]]
      =^  translation-path-result  out
        (depend-on translation-path-build)
      ::
      ?~  translation-path-result
        (return-blocks [translation-path-build]~)
      ::
      ?:  ?=([~ %error *] translation-path-result)
        %-  return-error
        :_  message.u.translation-path-result
        :-  %leaf
        ;:  weld
          "ford: %cast "  (trip mark)  " on ["  (trip (scot %p ship.disc))
          " "  (trip desk.disc)  "] failed:"
        ==
      ::
      ?>  ?=([~ %success %walk *] translation-path-result)
      ::
      =/  translation-path=(list mark-action)
        results.u.translation-path-result
      ::
      |^  ^-  build-receipt
          ?~  translation-path
            (return-result %success %cast result-cage)
          ::
          =^  action-result  out
            =,  i.translation-path
            ?-  -.i.translation-path
              %grow  (run-grow source target result-cage)
              %grab  (run-grab source target result-cage)
            ==
          ::
          ?-    -.action-result
              %success
            %_  $
              translation-path  t.translation-path
              result-cage  cage.action-result
            ==
          ::
              %blocks
            (return-blocks blocks.action-result)
          ::
              %error
            (return-error [leaf+"ford: failed to %cast" tang.action-result])
        ==
      ::
      +=  action-result
        $%  ::  translation was successful and here's a cage for you
            [%success =cage]
            ::  it was an error. sorry.
            [%error =tang]
            ::  we block on a build
            [%blocks blocks=(list ^build)]
        ==
      ::
      ++  run-grab
        |=  [source-mark=term target-mark=term input-cage=cage]
        ^-  [action-result _out]
        ::
        =/  mark-path-build=^build
          [date.build [%path disc %mar target-mark]]
        ::
        =^  mark-path-result  out
          (depend-on mark-path-build)
        ?~  mark-path-result
          [[%blocks [mark-path-build]~] out]
        ::
        ?.  ?=([~ %success %path *] mark-path-result)
          %-  cast-wrap-error  :*
            source-mark
            target-mark
            ;:  weld
              "ford: %cast failed to find path for mark "  (trip source-mark)
              " during +grab:"
            ==
            mark-path-result
          ==
        ::
        =/  mark-core-build=^build  [date.build [%core rail.u.mark-path-result]]
        ::
        =^  mark-core-result  out  (depend-on mark-core-build)
        ?~  mark-core-result
          [[%blocks ~[mark-core-build]] out]
        ::  find +grab within the destination mark core
        ::
        =/  grab-build=^build
          :-  date.build
          [%ride [%limb %grab] [%$ (result-to-cage u.mark-core-result)]]
        ::
        =^  grab-result  out  (depend-on grab-build)
        ?~  grab-result
          [[%blocks [grab-build]~] out]
        ::
        ?.  ?=([~ %success %ride *] grab-result)
          =/  =path  (rail-to-path rail.u.mark-path-result)
          %-  cast-wrap-error  :*
            source-mark
            target-mark
            :(weld "ford: %cast failed to ride " (spud path) " during +grab:")
            grab-result
          ==
        ::  find an arm for the input's mark within the +grab core
        ::
        =/  grab-mark-build=^build
          :-  date.build
          [%ride [%limb source-mark] [%$ %noun vase.u.grab-result]]
        ::
        =^  grab-mark-result  out  (depend-on grab-mark-build)
        ?~  grab-mark-result
          [[%blocks [grab-mark-build]~] out]
        ::
        ?.  ?=([~ %success %ride *] grab-mark-result)
          =/  =path  (rail-to-path rail.u.mark-path-result)
          %-  cast-wrap-error  :*
            source-mark
            target-mark
            :(weld "ford: %cast failed to ride " (spud path) " during +grab:")
            grab-mark-result
          ==
        ::  slam the +mark-name:grab gate on the result of running :input
        ::
        =/  call-build=^build
          :-  date.build
          [%call gate=[%$ %noun vase.u.grab-mark-result] sample=[%$ input-cage]]
        ::
        =^  call-result  out  (depend-on call-build)
        ?~  call-result
          [[%blocks [call-build]~] out]
        ::
        ?.  ?=([~ %success %call *] call-result)
          =/  =path  (rail-to-path rail.u.mark-path-result)
          %-  cast-wrap-error  :*
            source-mark
            target-mark
            :(weld "ford: %cast failed to call +grab arm in " (spud path) ":")
            call-result
          ==
        ::
        [[%success [mark vase.u.call-result]] out]
      ::  +grow: grow from the input mark to the destination mark
      ::
      ++  run-grow
        |=  [source-mark=term target-mark=term input-cage=cage]
        ^-  [action-result _out]
        ::
        =/  starting-mark-path-build=^build
          [date.build [%path disc %mar source-mark]]
        ::
        =^  starting-mark-path-result  out
          (depend-on starting-mark-path-build)
        ?~  starting-mark-path-result
          [[%blocks [starting-mark-path-build]~] out]
        ::
        ?.  ?=([~ %success %path *] starting-mark-path-result)
          %-  cast-wrap-error  :*
            source-mark
            target-mark
            ;:  weld
              "ford: %cast failed to find path for mark "  (trip source-mark)
              " during +grow:"
            ==
            starting-mark-path-result
          ==
        ::  grow the value from the initial mark to the final mark
        ::
        ::  Replace the input mark's sample with the input's result,
        ::  then fire the mark-name:grow arm to produce a result.
        ::
        =/  grow-build=^build
          :-  date.build
          :+  %ride
            formula=`hoon`[%tsld [%wing ~[target-mark]] [%wing ~[%grow]]]
          ^=  subject
          ^-  schematic
          :*  %mute
              ^-  schematic
              [%core rail.u.starting-mark-path-result]
              ^=  mutations
              ^-  (list [wing schematic])
              [[%& 6]~ [%$ input-cage]]~
          ==
        ::
        =^  grow-result  out  (depend-on grow-build)
        ?~  grow-result
          [[%blocks [grow-build]~] out]
        ::
        ?.  ?=([~ %success %ride *] grow-result)
          =/  =path  (rail-to-path rail.u.starting-mark-path-result)
          %-  cast-wrap-error  :*
            source-mark
            target-mark
            :(weld "ford: %cast failed to ride " (spud path) " during +grow:")
            grow-result
          ==
        ::  make sure the product nests in the sample of the destination mark
        ::
        =/  bunt-build=^build  [date.build [%bunt disc target-mark]]
        ::
        =^  bunt-result  out  (depend-on bunt-build)
        ?~  bunt-result
          [[%blocks [bunt-build]~] out]
        ::
        ?.  ?=([~ %success %bunt *] bunt-result)
          %-  cast-wrap-error  :*
            source-mark
            target-mark
            :(weld "ford: %cast failed to bunt " (trip target-mark) ":")
            bunt-result
          ==
        ::
        ?.  (~(nest ut p.q.cage.u.bunt-result) | p.vase.u.grow-result)
          =*  src  source-mark
          =*  dst  target-mark
          :_  out
          :-  %error
          :_  ~
          :-  %leaf
          ;:  weld
            "ford: %cast from "  (trip src)  " to "  (trip dst)
            " failed: nest fail"
          ==
        ::
        [[%success mark vase.u.grow-result] out]
      ::
      ++  cast-wrap-error
        |=  $:  source-mark=term
                target-mark=term
                description=tape
                result=(unit build-result)
            ==
        ^-  [action-result _out]
        ::
        ?>  ?=([~ %error *] result)
        ::
        :_  out
        :-  %error
        :*  :-  %leaf
            ;:  weld
              "ford: %cast failed while trying to cast from "
              (trip source-mark)  " to "  (trip target-mark)  ":"
            ==
            [%leaf description]
            message.u.result
        ==
      --
    ::
    ++  make-core
      ~%  %make-core  ..^^$  ~
      |=  source-path=rail
      ^-  build-receipt
      ::  convert file at :source-path to a +scaffold
      ::
      =/  hood-build=^build  [date.build [%hood source-path]]
      ::
      =^  hood-result  out  (depend-on hood-build)
      ?~  hood-result
        (return-blocks [hood-build]~)
      ::
      ?:  ?=(%error -.u.hood-result)
        %-  return-error
        :-  [%leaf "ford: %core on {<(rail-to-path source-path)>} failed:"]
        message.u.hood-result
      ::  build the +scaffold into a program
      ::
      ?>  ?=([%success %hood *] u.hood-result)
      ::
      =/  plan-build=^build
        [date.build [%plan source-path `coin`[%many ~] scaffold.u.hood-result]]
      ::
      =^  plan-result  out  (depend-on plan-build)
      ?~  plan-result
        (return-blocks [plan-build]~)
      ::
      ?:  ?=(%error -.u.plan-result)
        %-  return-error
        :-  [%leaf "ford: %core on {<(rail-to-path source-path)>} failed:"]
        message.u.plan-result
      ::
      ?>  ?=([%success %plan *] u.plan-result)
      (return-result %success %core vase.u.plan-result)
    ::
    ++  make-diff
      ~%  %make-diff  ..^^$  ~
      |=  [=disc start=schematic end=schematic]
      ^-  build-receipt
      ::  run both input schematics as an autocons build
      ::
      =/  sub-build=^build  [date.build [start end]]
      ::
      =^  sub-result  out  (depend-on sub-build)
      ?~  sub-result
        (return-blocks [sub-build]~)
      ::
      ?.  ?=([~ %success ^ ^] sub-result)
        (wrap-error sub-result)
      ?.  ?=([%success *] head.u.sub-result)
        (wrap-error `head.u.sub-result)
      ?.  ?=([%success *] tail.u.sub-result)
        (wrap-error `tail.u.sub-result)
      ::
      =/  start-cage=cage  (result-to-cage head.u.sub-result)
      =/  end-cage=cage    (result-to-cage tail.u.sub-result)
      ::  if the marks aren't the same, we can't diff them
      ::
      ?.  =(p.start-cage p.end-cage)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %diff failed: mark mismatch: %{<p.start-cage>} / %{<p.end-cage>}"
      ::  if the values are the same, the diff is null
      ::
      ?:  =(q.q.start-cage q.q.end-cage)
        =/  =build-result
          [%success %diff [%null [%atom %n ~] ~]]
        ::
        (return-result build-result)
      ::
      =/  mark-path-build=^build  [date.build [%path disc %mar p.start-cage]]
      ::
      =^  mark-path-result  out  (depend-on mark-path-build)
      ?~  mark-path-result
        (return-blocks [mark-path-build]~)
      ::
      ?:  ?=([~ %error *] mark-path-result)
        %-  return-error
        :-  [%leaf "ford: %diff failed on {<disc>}:"]
        message.u.mark-path-result
      ::
      ?>  ?=([~ %success %path *] mark-path-result)
      ::
      =/  mark-build=^build  [date.build [%core rail.u.mark-path-result]]
      ::
      =^  mark-result  out  (depend-on mark-build)
      ?~  mark-result
        (return-blocks [mark-build]~)
      ::
      ?:  ?=([~ %error *] mark-result)
        %-  return-error
        :-  [%leaf "ford: %diff failed on {<disc>}:"]
        message.u.mark-result
      ::
      ?>  ?=([~ %success %core *] mark-result)
      ::
      ?.  (slab %grad p.vase.u.mark-result)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %diff failed: %{<p.start-cage>} mark has no +grad arm"
      ::
      =/  grad-build=^build
        [date.build [%ride [%limb %grad] [%$ %noun vase.u.mark-result]]]
      ::
      =^  grad-result  out  (depend-on grad-build)
      ?~  grad-result
        (return-blocks [grad-build]~)
      ::
      ?:  ?=([~ %error *] grad-result)
        %-  return-error
        :-  [%leaf "ford: %diff failed on {<disc>}:"]
        message.u.grad-result
      ::
      ?>  ?=([~ %success %ride *] grad-result)
      ::  if +grad produced a @tas, convert to that mark and diff those
      ::
      ?@  q.vase.u.grad-result
        =/  mark=(unit @tas)  ((sand %tas) q.vase.u.grad-result)
        ?~  mark
          %-  return-error  :_  ~  :-  %leaf
          "ford: %diff failed: %{<p.start-cage>} mark has invalid +grad arm"
        ::
        =/  diff-build=^build
          :-  date.build
          :^    %diff
              disc
            [%cast disc u.mark [%$ start-cage]]
          [%cast disc u.mark [%$ end-cage]]
        ::
        =^  diff-result  out  (depend-on diff-build)
        ?~  diff-result
          (return-blocks [diff-build]~)
        ::
        ?.  ?=([~ %success %diff *] diff-result)
          (wrap-error diff-result)
        ::
        =/  =build-result
          [%success %diff cage.u.diff-result]
        ::
        (return-result build-result)
      ::  +grad produced a cell, which should be a core with a +form arm
      ::
      ?.  (slab %form p.vase.u.grad-result)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %diff failed: %{<p.start-cage>} mark has no +form:grab arm"
      ::  the +grab core should also contain a +diff arm
      ::
      ?.  (slab %diff p.vase.u.grad-result)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %diff failed: %{<p.start-cage>} mark has no +diff:grab arm"
      ::
      =/  diff-build=^build
        :-  date.build
        :+  %call
          ::
          ^=  gate
          :+  %ride
            ::
            formula=`hoon`[%tsld [%wing ~[%diff]] [%wing ~[%grad]]]
          ::
          ^=  subject
          :+  %mute
            ::
            subject=`schematic`[%$ %noun vase.u.mark-result]
          ::
          ^=  mutations
          ^-  (list [wing schematic])
          [[%& 6]~ [%$ start-cage]]~
        ::
        sample=`schematic`[%$ end-cage]
      ::
      =^  diff-result  out  (depend-on diff-build)
      ?~  diff-result
        (return-blocks [diff-build]~)
      ::
      ?.  ?=([~ %success %call *] diff-result)
        (wrap-error diff-result)
      ::
      =/  form-build=^build
        [date.build [%ride [%limb %form] [%$ %noun vase.u.grad-result]]]
      ::
      =^  form-result  out  (depend-on form-build)
      ?~  form-result
        (return-blocks [form-build]~)
      ::
      ?.  ?=([~ %success %ride *] form-result)
        (wrap-error form-result)
      ::
      =/  mark=(unit @tas)  ((soft @tas) q.vase.u.form-result)
      ?~  mark
        %-  return-error  :_  ~  :-  %leaf
        "ford: %diff failed: invalid +form result: {(text vase.u.form-result)}"
      ::
      =/  =build-result
        [%success %diff [u.mark vase.u.diff-result]]
      ::
      (return-result build-result)
    ::
    ++  make-dude
      ~%  %make-dude  ..^^$  ~
      |=  [error=tank attempt=schematic]
      ^-  build-receipt
      ::
      =/  attempt-build=^build  [date.build attempt]
      =^  attempt-result  out  (depend-on attempt-build)
      ?~  attempt-result
        ::
        (return-blocks ~[[date.build attempt]])
      ::
      ?.  ?=([%error *] u.attempt-result)
        (return-result u.attempt-result)
      ::
      (return-error [error message.u.attempt-result])
    ::
    ++  make-hood
      ~%  %make-hood  ..^^$  ~
      |=  source-rail=rail
      ^-  build-receipt
      ::
      =/  scry-build=^build  [date.build [%scry [%c %x source-rail]]]
      =^  scry-result  out  (depend-on scry-build)
      ?~  scry-result
        ::
        (return-blocks ~[scry-build])
      ::
      ?:  ?=([~ %error *] scry-result)
        =/  =path  (rail-to-path source-rail)
        %-  return-error
        :-  [%leaf "ford: %hood failed for {<path>}:"]
        message.u.scry-result
      =+  as-cage=(result-to-cage u.scry-result)
      ::  hoon files must be atoms to parse
      ::
      ?.  ?=(@ q.q.as-cage)
        =/  =path  (rail-to-path source-rail)
        %-  return-error
        :_  ~
        :-  %leaf
        "ford: %hood: path {<path>} not an atom"
      ::
      =/  src-beam=beam  [[ship.disc desk.disc [%ud 0]] spur]:source-rail
      ::
      =/  =compiler-cache-key  [%hood src-beam q.q.as-cage]
      =^  cached-result  out  (access-cache compiler-cache-key)
      ?^  cached-result
        (return-result u.cached-result)
      ::
      =/  parsed
        ((full (parse-scaffold src-beam)) [1 1] (trip q.q.as-cage))
      ::
      ?~  q.parsed
        =/  =path  (rail-to-path source-rail)
        %-  return-error
        :-  :-  %leaf
            %+  weld  "ford: %hood: syntax error at "
            "[{<p.p.parsed>} {<q.p.parsed>}] in {<path>}"
        ~
      ::
      (return-result %success %hood p.u.q.parsed)
    ::
    ++  make-join
      ~%  %make-join  ..^^$  ~
      |=  [disc=disc mark=term first=schematic second=schematic]
      ^-  build-receipt
      ::
      =/  initial-build=^build
        [date.build [first second] [%path disc %mar mark]]
      ::
      =^  initial-result  out  (depend-on initial-build)
      ?~  initial-result
        (return-blocks [initial-build]~)
      ::
      ?.  ?=([~ %success [%success ^ ^] %success %path *] initial-result)
        (wrap-error initial-result)
      ?.  ?=([%success *] head.head.u.initial-result)
        (wrap-error `head.head.u.initial-result)
      ?.  ?=([%success *] tail.head.u.initial-result)
        (wrap-error `tail.head.u.initial-result)
      ::
      =/  first-cage=cage   (result-to-cage head.head.u.initial-result)
      =/  second-cage=cage  (result-to-cage tail.head.u.initial-result)
      =/  mark-path=rail    rail.tail.u.initial-result
      ::  TODO: duplicate logic with +make-pact and others
      ::
      =/  mark-build=^build  [date.build [%core mark-path]]
      ::
      =^  mark-result  out  (depend-on mark-build)
      ?~  mark-result
        (return-blocks [mark-build]~)
      ::
      ?:  ?=([~ %error *] mark-result)
        %-  return-error
        :-  [%leaf "ford: %join to {<mark>} on {<disc>} failed:"]
        message.u.mark-result
      ::
      ?>  ?=([~ %success %core *] mark-result)
      ::
      =/  mark-vase=vase  vase.u.mark-result
      ::
      ?.  (slab %grad p.mark-vase)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %join failed: %{<mark>} mark has no +grad arm"
      ::
      =/  grad-build=^build
        [date.build [%ride [%limb %grad] [%$ %noun mark-vase]]]
      ::
      =^  grad-result  out  (depend-on grad-build)
      ?~  grad-result
        (return-blocks [grad-build]~)
      ::
      ?:  ?=([~ %error *] grad-result)
        %-  return-error
        :-  [%leaf "ford: %join to {<mark>} on {<disc>} failed:"]
        message.u.grad-result
      ::
      ?>  ?=([~ %success %ride *] grad-result)
      ::
      =/  grad-vase=vase  vase.u.grad-result
      ::  if +grad produced a mark, delegate %join behavior to that mark
      ::
      ?@  q.grad-vase
        ::  if +grad produced a term, make sure it's a valid mark
        ::
        =/  grad-mark=(unit term)  ((sand %tas) q.grad-vase)
        ?~  grad-mark
          %-  return-error  :_  ~  :-  %leaf
          "ford: %join failed: %{<mark>} mark invalid +grad"
        ::  todo: doesn't catch full cycles of +grad arms, only simple cases
        ::
        ?:  =(u.grad-mark mark)
          %-  return-error  :_  ~  :-  %leaf
          "ford: %join failed: %{<mark>} mark +grad arm refers to self"
        ::
        =/  join-build=^build
          [date.build [%join disc u.grad-mark [%$ first-cage] [%$ second-cage]]]
        ::
        =^  join-result  out  (depend-on join-build)
        ?~  join-result
          (return-blocks [join-build]~)
        ::
        ?:  ?=([~ %error *] join-result)
          %-  return-error
          :-  [%leaf "ford: %join to {<mark>} on {<disc>} failed:"]
          message.u.join-result
        ::
        ?>  ?=([~ %success %join *] join-result)
        ::
        (return-result u.join-result)
      ::  make sure the +grad core has a +form arm
      ::
      ?.  (slab %form p.grad-vase)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %join failed: no +form:grad in %{<mark>} mark"
      ::  make sure the +grad core has a +join arm
      ::
      ?.  (slab %join p.grad-vase)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %join failed: no +join:grad in %{<mark>} mark"
      ::  fire the +form:grad arm, which should produce a mark
      ::
      =/  form-build=^build
        [date.build [%ride [%limb %form] [%$ %noun grad-vase]]]
      ::
      =^  form-result  out  (depend-on form-build)
      ?~  form-result
        (return-blocks [form-build]~)
      ::
      ?.  ?=([~ %success %ride *] form-result)
        (wrap-error form-result)
      ::
      =/  form-mark=(unit term)  ((soft @tas) q.vase.u.form-result)
      ?~  form-mark
        %-  return-error  :_  ~  :-  %leaf
        "ford: %join failed: %{<mark>} mark invalid +form:grad"
      ::  the mark produced by +form:grad should match both diffs
      ::
      ?.  &(=(u.form-mark p.first-cage) =(u.form-mark p.second-cage))
        %-  return-error  :_  ~  :-  %leaf
        "ford: %join failed: mark mismatch"
      ::  if the diffs are identical, just produce the first
      ::
      ?:  =(q.q.first-cage q.q.second-cage)
        (return-result %success %join first-cage)
      ::  call the +join:grad gate on the two diffs
      ::
      =/  diff-build=^build
        :-  date.build
        :+  %call
          :+  %ride
            [%limb %join]
          [%$ %noun grad-vase]
        [%$ %noun (slop q.first-cage q.second-cage)]
      ::
      =^  diff-result  out  (depend-on diff-build)
      ?~  diff-result
        (return-blocks [diff-build]~)
      ::
      ?:  ?=([~ %error *] diff-result)
        %-  return-error
        :-  [%leaf "ford: %join to {<mark>} on {<disc>} failed:"]
        message.u.diff-result
      ::
      ?>  ?=([~ %success %call *] diff-result)
      ::  the result was a unit; if `~`, use %null mark; otherwise grab tail
      ::
      =/  =build-result
        :+  %success  %join
        ?@  q.vase.u.diff-result
          [%null vase.u.diff-result]
        [u.form-mark (slot 3 vase.u.diff-result)]
      ::
      (return-result build-result)
    ::
    ++  make-list
      ~%  %make-list  ..^^$  ~
      |=  schematics=(list schematic)
      ^-  build-receipt
      ::
      =/  key-and-schematics
        (turn schematics |=(=schematic [~ schematic]))
      ::  depend on builds of each schematic
      ::
      =^  maybe-schematic-results  out
        (perform-schematics "" key-and-schematics %ignore-errors *~)
      ?~  maybe-schematic-results
        out
      ::  return all builds
      ::
      =/  =build-result
        :+  %success  %list
        ::  the roll above implicitly flopped the results
        ::
        (flop (turn u.maybe-schematic-results tail))
      (return-result build-result)
    ::
    ++  make-mash
      ~%  %make-mash  ..^^$  ~
      |=  $:  disc=disc
              mark=term
              first=[disc=disc mark=term =schematic]
              second=[disc=disc mark=term =schematic]
          ==
      ^-  build-receipt
      ::
      =/  initial-build=^build
        [date.build [schematic.first schematic.second] [%path disc %mar mark]]
      ::
      =^  initial-result  out  (depend-on initial-build)
      ?~  initial-result
        (return-blocks [initial-build]~)
      ::  TODO: duplicate logic with +make-join
      ::
      ?.  ?=([~ %success [%success ^ ^] %success %path *] initial-result)
        (wrap-error initial-result)
      ?.  ?=([%success *] head.head.u.initial-result)
        (wrap-error `head.head.u.initial-result)
      ?.  ?=([%success *] tail.head.u.initial-result)
        (wrap-error `tail.head.u.initial-result)
      ::
      =/  first-cage=cage   (result-to-cage head.head.u.initial-result)
      =/  second-cage=cage  (result-to-cage tail.head.u.initial-result)
      =/  mark-path=rail    rail.tail.u.initial-result
      ::  TODO: duplicate logic with +make-pact and others
      ::
      =/  mark-build=^build  [date.build [%core mark-path]]
      ::
      =^  mark-result  out  (depend-on mark-build)
      ?~  mark-result
        (return-blocks [mark-build]~)
      ::
      ?.  ?=([~ %success %core *] mark-result)
        (wrap-error mark-result)
      ::
      =/  mark-vase=vase  vase.u.mark-result
      ::
      ?.  (slab %grad p.mark-vase)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %mash failed: %{<mark>} mark has no +grad arm"
      ::
      =/  grad-build=^build
        [date.build [%ride [%limb %grad] [%$ %noun mark-vase]]]
      ::
      =^  grad-result  out  (depend-on grad-build)
      ?~  grad-result
        (return-blocks [grad-build]~)
      ::
      ?.  ?=([~ %success %ride *] grad-result)
        (wrap-error grad-result)
      ::
      =/  grad-vase=vase  vase.u.grad-result
      ::  if +grad produced a mark, delegate %mash behavior to that mark
      ::
      ?@  q.grad-vase
        ::  if +grad produced a term, make sure it's a valid mark
        ::
        =/  grad-mark=(unit term)  ((sand %tas) q.grad-vase)
        ?~  grad-mark
          %-  return-error  :_  ~  :-  %leaf
          "ford: %mash failed: %{<mark>} mark invalid +grad"
        ::
        =/  mash-build=^build
          :-  date.build
          :-  %mash
          :^  disc  u.grad-mark
            [disc.first mark.first [%$ first-cage]]
          [disc.second mark.second [%$ second-cage]]
        ::
        =^  mash-result  out  (depend-on mash-build)
        ?~  mash-result
          (return-blocks [mash-build]~)
        ::
        ?.  ?=([~ %success %mash *] mash-result)
          (wrap-error mash-result)
        ::
        =/  =build-result
          [%success %mash cage.u.mash-result]
        ::
        (return-result build-result)
      ::
      ?.  (slab %form p.grad-vase)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %mash failed: %{<mark>} mark has no +form:grad"
      ::
      ?.  (slab %mash p.grad-vase)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %mash failed: %{<mark>} mark has no +mash:grad"
      ::
      =/  form-build=^build
        [date.build [%ride [%limb %form] [%$ %noun grad-vase]]]
      ::
      =^  form-result  out  (depend-on form-build)
      ?~  form-result
        (return-blocks [form-build]~)
      ::
      ?.  ?=([~ %success %ride *] form-result)
        (wrap-error form-result)
      ::
      =/  form-mark=(unit term)  ((soft @tas) q.vase.u.form-result)
      ?~  form-mark
        %-  return-error  :_  ~  :-  %leaf
        "ford: %mash failed: %{<mark>} mark invalid +form:grad"
      ::
      ?.  &(=(u.form-mark p.first-cage) =(u.form-mark p.second-cage))
        %-  return-error  :_  ~  :-  %leaf
        "ford: %mash failed: mark mismatch"
      ::
      ?:  =(q.q.first-cage q.q.second-cage)
        =/  =build-result
          [%success %mash [%null [%atom %n ~] ~]]
        ::
        (return-result build-result)
      ::  call the +mash:grad gate on two [ship desk diff] triples
      ::
      =/  mash-build=^build
        :-  date.build
        :+  %call
          :+  %ride
            [%limb %mash]
          [%$ %noun grad-vase]
        :+  %$  %noun
        %+  slop
          ;:  slop
            [[%atom %p ~] ship.disc.first]
            [[%atom %tas ~] desk.disc.first]
            q.first-cage
          ==
        ;:  slop
          [[%atom %p ~] ship.disc.second]
          [[%atom %tas ~] desk.disc.second]
          q.second-cage
        ==
      ::
      =^  mash-result  out  (depend-on mash-build)
      ?~  mash-result
        (return-blocks [mash-build]~)
      ::
      ?.  ?=([~ %success %call *] mash-result)
        (wrap-error mash-result)
      ::
      =/  =build-result
        [%success %mash [u.form-mark vase.u.mash-result]]
      ::
      (return-result build-result)
    ::
    ++  make-mute
      ~%  %make-mute  ..^^$  ~
      |=  [subject=schematic mutations=(list [=wing =schematic])]
      ^-  build-receipt
      ::  run the subject build to produce the noun to be mutated
      ::
      =/  subject-build=^build  [date.build subject]
      =^  subject-result  out  (depend-on subject-build)
      ?~  subject-result
        (return-blocks [subject-build]~)
      ::
      ?.  ?=([~ %success *] subject-result)
        (wrap-error subject-result)
      ::
      =/  subject-cage=cage  (result-to-cage u.subject-result)
      ::
      =/  subject-vase=vase  q.subject-cage
      ::
      =^  maybe-schematic-results  out
        %-  perform-schematics  :*
          "ford: %mute contained failures:"
          mutations
          %fail-on-errors
          *wing
        ==
      ?~  maybe-schematic-results
        out
      ::  all builds succeeded; retrieve vases from results
      ::
      =/  successes=(list [=wing =vase])
        %+  turn  u.maybe-schematic-results
        |=  [=wing result=build-result]
        ^-  [^wing vase]
        ::
        ?>  ?=([%success *] result)
        ::
        [wing q:(result-to-cage result)]
      ::  create and run a +build to apply all mutations in order
      ::
      =/  ride-build=^build
        :-  date.build
        :+  %ride
          ::  formula: a `%_` +hoon that applies a list of mutations
          ::
          ::    The hoon ends up looking like:
          ::    ```
          ::    %_  +2
          ::      wing-1  +6
          ::      wing-2  +14
          ::      ...
          ::    ==
          ::    ```
          ::
          ^=  formula
          ^-  hoon
          :+  %cncb  [%& 2]~
          =/  axis  3
          ::
          |-  ^-  (list [wing hoon])
          ?~  successes  ~
          ::
          :-  [wing.i.successes [%$ (peg axis 2)]]
          $(successes t.successes, axis (peg axis 3))
        ::  subject: list of :subject-vase and mutations, as literal schematic
        ::
        ::    The subject ends up as a vase of something like this:
        ::    ```
        ::    :~  original-subject
        ::        mutant-1
        ::        mutant-2
        ::        ...
        ::    ==
        ::    ```
        ::
        ^=  subject  ^-  schematic
        :+  %$  %noun
        ^-  vase
        %+  slop  subject-vase
        |-  ^-  vase
        ?~  successes  [[%atom %n ~] ~]
        ::
        (slop vase.i.successes $(successes t.successes))
      ::
      =^  ride-result  out  (depend-on ride-build)
      ?~  ride-result
        (return-blocks [ride-build]~)
      ::
      ?.  ?=([~ %success %ride *] ride-result)
        (wrap-error ride-result)
      ::
      =/  =build-result
        [%success %mute p.subject-cage vase.u.ride-result]
      ::
      (return-result build-result)
    ::
    ++  make-pact
      ~%  %make-pact  ..^^$  ~
      |=  [disc=disc start=schematic diff=schematic]
      ^-  build-receipt
      ::  first, build the inputs
      ::
      =/  initial-build=^build  [date.build start diff]
      ::
      =^  initial-result  out  (depend-on initial-build)
      ?~  initial-result
        (return-blocks [initial-build]~)
      ::
      ?>  ?=([~ %success ^ ^] initial-result)
      =/  start-result=build-result  head.u.initial-result
      =/  diff-result=build-result    tail.u.initial-result
      ::
      ?.  ?=(%success -.start-result)
        (wrap-error `start-result)
      ?.  ?=(%success -.diff-result)
        (wrap-error `diff-result)
      ::
      =/  start-cage=cage  (result-to-cage start-result)
      =/  diff-cage=cage    (result-to-cage diff-result)
      ::
      =/  start-mark=term  p.start-cage
      =/  diff-mark=term    p.diff-cage
      ::  load the starting mark from the filesystem
      ::
      =/  mark-path-build=^build  [date.build [%path disc %mar start-mark]]
      ::
      =^  mark-path-result  out
        (depend-on mark-path-build)
      ::
      ?~  mark-path-result
        (return-blocks [mark-path-build]~)
      ::
      ?.  ?=([~ %success %path *] mark-path-result)
        (wrap-error mark-path-result)
      ::
      =/  mark-build=^build  [date.build [%core rail.u.mark-path-result]]
      ::
      =^  mark-result  out  (depend-on mark-build)
      ?~  mark-result
        (return-blocks [mark-build]~)
      ::
      ?.  ?=([~ %success %core *] mark-result)
        (wrap-error mark-result)
      ::
      =/  mark-vase=vase  vase.u.mark-result
      ::  fire the +grad arm of the mark core
      ::
      ?.  (slab %grad p.mark-vase)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %pact failed: %{<start-mark>} mark has no +grad arm"
      ::
      =/  grad-build=^build
        [date.build [%ride [%limb %grad] [%$ %noun mark-vase]]]
      ::
      =^  grad-result  out  (depend-on grad-build)
      ?~  grad-result
        (return-blocks [grad-build]~)
      ::
      ?.  ?=([~ %success %ride *] grad-result)
        (wrap-error grad-result)
      ::
      =/  grad-vase=vase  vase.u.grad-result
      ::  +grad can produce a term or a core
      ::
      ::    If a mark's +grad arm produces a mark (as a +term),
      ::    it means we should use that mark's machinery to run %pact.
      ::    In this way, a mark can delegate its patching machinery to
      ::    another mark.
      ::
      ::    First we cast :start-cage to the +grad mark, then we run
      ::    a new %pact build on the result of that, which will use the
      ::    +grad mark's +grad arm. Finally we cast the %pact result back to
      ::    :start-mark, since we're trying to produce a patched version of
      ::    the initial marked value (:start-cage).
      ::
      ?@  q.grad-vase
        ::  if +grad produced a term, make sure it's a valid mark
        ::
        =/  grad-mark=(unit term)  ((sand %tas) q.grad-vase)
        ?~  grad-mark
          %-  return-error  :_  ~  :-  %leaf
          "ford: %pact failed: %{<start-mark>} mark invalid +grad"
        ::  cast :start-cage to :grad-mark, %pact that, then cast back to start
        ::
        =/  cast-build=^build
          :-  date.build
          :^  %cast  disc  start-mark
          :^  %pact  disc
            :^  %cast  disc  u.grad-mark
            [%$ start-cage]
          [%$ diff-cage]
        ::
        =^  cast-result  out  (depend-on cast-build)
        ?~  cast-result
          (return-blocks [cast-build]~)
        ::
        ?.  ?=([~ %success %cast *] cast-result)
          (wrap-error cast-result)
        ::
        =/  =build-result
          [%success %pact cage.u.cast-result]
        ::
        (return-result build-result)
      ::  +grad produced a core; make sure it has a +form arm
      ::
      ::    +grad can produce a core containing +pact and +form
      ::    arms. +form:grad, which produces a mark (as a term), is used
      ::    to verify that the diff is of the correct mark.
      ::
      ::    +pact:grad produces a gate that gets slammed with the diff
      ::    as its sample and produces a mutant version of :start-cage
      ::    by applying the diff.
      ::
      ?.  (slab %form p.grad-vase)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %pact failed: no +form:grad in %{<start-mark>} mark"
      ::  we also need a +pact arm in the +grad core
      ::
      ?.  (slab %pact p.grad-vase)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %pact failed: no +pact:grad in %{<start-mark>} mark"
      ::  fire the +form arm in the core produced by +grad
      ::
      =/  form-build=^build
        [date.build [%ride [%limb %form] [%$ %noun grad-vase]]]
      ::
      =^  form-result  out  (depend-on form-build)
      ?~  form-result
        (return-blocks [form-build]~)
      ::
      ?.  ?=([~ %success %ride *] form-result)
        (wrap-error form-result)
      ::  +form:grad should produce a mark
      ::
      =/  form-mark=(unit @tas)  ((soft @tas) q.vase.u.form-result)
      ?~  form-mark
        %-  return-error  :_  ~  :-  %leaf
        "ford: %pact failed: %{<start-mark>} mark invalid +form:grad"
      ::  mark produced by +form:grad needs to match the mark of the diff
      ::
      ?.  =(u.form-mark diff-mark)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %pact failed: %{<start-mark>} mark invalid +form:grad"
      ::  call +pact:grad on the diff
      ::
      =/  pact-build=^build
        :-  date.build
        :+  %call
          ^-  schematic
          :+  %ride
            [%tsld [%limb %pact] [%limb %grad]]
          ^-  schematic
          :+  %mute
            ^-  schematic
            [%$ %noun mark-vase]
          ^-  (list [wing schematic])
          [[%& 6]~ [%$ start-cage]]~
        ^-  schematic
        [%$ diff-cage]
      ::
      =^  pact-result  out  (depend-on pact-build)
      ?~  pact-result
        (return-blocks [pact-build]~)
      ::
      ?.  ?=([~ %success %call *] pact-result)
        (wrap-error pact-result)
      ::
      =/  =build-result
        [%success %pact start-mark vase.u.pact-result]
      ::
      (return-result build-result)
    ::
    ++  make-path
      ~%  %make-path  ..^^$  ~
      |=  [disc=disc prefix=@tas raw-path=@tas]
      ^-  build-receipt
      ::  possible-spurs: flopped paths to which :raw-path could resolve
      ::
      =/  possible-spurs=(list spur)  (turn (segments raw-path) flop)
      ::  rails-and-schematics: scrys to check each path in :possible-paths
      ::
      =/  rails-and-schematics=(list [=rail =schematic])
        %+  turn  possible-spurs
        |=  possible-spur=spur
        ^-  [rail schematic]
        ::  full-spur: wrap :possible-spur with :prefix and /hoon suffix
        ::
        =/  full-spur=spur  :(welp /hoon possible-spur /[prefix])
        ::
        :-  [disc full-spur]
        [%scry %c %x `rail`[disc full-spur]]
      ::  depend on builds of each schematic
      ::
      =^  maybe-schematic-results  out
        %-  perform-schematics  :*
          ;:  weld
            "ford: %path resolution of "  (trip raw-path)  "at prefix "
            (trip prefix)  " contained failures:"
          ==
          rails-and-schematics
          %filter-errors
          *rail
        ==
      ?~  maybe-schematic-results
        out
      ::  matches: builds that completed with a successful result
      ::
      =/  matches  u.maybe-schematic-results
      ::  if no matches, error out
      ::
      ?~  matches
        =/  =beam
          [[ship.disc desk.disc [%da date.build]] /hoon/[raw-path]/[prefix]]
        ::
        %-  return-error
        :_  ~
        :-  %leaf
        (weld "%path: no matches for " (spud (en-beam beam)))
      ::  if exactly one path matches, succeed with the matching path
      ::
      ?:  ?=([* ~] matches)
        (return-result %success %path key.i.matches)
      ::  multiple paths matched; error out
      ::
      %-  return-error
      ::
      :-  [%leaf "multiple matches for %path: "]
      ::  tmi; cast :matches back to +list
      ::
      %+  roll  `_u.maybe-schematic-results`matches
      |=  [[key=rail result=build-result] message=tang]
      ^-  tang
      ::  beam: reconstruct request from :kid's schematic and date
      ::
      =/  =beam  [[ship.disc desk.disc [%da date.build]] spur.key]
      ::
      [[%leaf (spud (en-beam beam))] message]
    ::
    ++  make-plan
      ~%  %make-plan  ..^^$  ~
      |=  [path-to-render=rail query-string=coin =scaffold]
      ^-  build-receipt
      ::  blocks: accumulator for blocked sub-builds
      ::
      =|  blocks=(list ^build)
      ::  error-message: accumulator for failed sub-builds
      ::
      =|  error-message=tang
      ::
      |^  ::  imports: structure and library +cables, with %sur/%lib prefixes
          ::
          =/  imports=(list [prefix=?(%sur %lib) =cable])
            %+  welp
              (turn structures.scaffold |=(cable [%sur +<]))
            (turn libraries.scaffold |=(cable [%lib +<]))
          ::  path-builds: %path sub-builds to resolve import paths
          ::
          =/  path-builds  (gather-path-builds imports)
          ::
          =^  path-results  ..$  (resolve-builds path-builds)
          ?^  blocks
            (return-blocks blocks)
          ::
          ?^  error-message
            (return-error error-message)
          ::  tmi; remove type specializations
          ::
          =>  .(blocks *(list ^build), error-message *tang)
          ::  core-builds: %core sub-builds to produce library vases
          ::
          =/  core-builds  (gather-core-builds path-results)
          ::
          =^  core-results  ..$  (resolve-builds core-builds)
          ?^  blocks
            (return-blocks blocks)
          ::
          ?^  error-message
            (return-error error-message)
          ::  reef-build: %reef build to produce standard library
          ::
          =/  reef-build=^build  [date.build [%reef disc.path-to-render]]
          ::
          =^  reef-result  out  (depend-on reef-build)
          ?~  reef-result
            (return-blocks [reef-build]~)
          ::
          ?.  ?=([~ %success %reef *] reef-result)
            (wrap-error reef-result)
          ::  subject: tuple of imports and standard library
          ::
          =/  subject=vase
            (link-imports imports vase.u.reef-result core-results)
          ::  tmi; remove type specializations
          ::
          =>  .(blocks *(list ^build), error-message *tang)
          ::  iterate over each crane
          ::
          =^  crane-result  ..$
            (compose-cranes [%noun subject] cranes.scaffold)
          ?:  ?=(%error -.crane-result)
            (return-error message.crane-result)
          ?:  ?=(%block -.crane-result)
            (return-blocks builds.crane-result)
          ::  combined-hoon: source hoons condensed into a single +hoon
          ::
          =/  combined-hoon=hoon  [%tssg sources.scaffold]
          ::  compile :combined-hoon against :subject
          ::
          =/  compile=^build
            [date.build [%ride combined-hoon [%$ subject.crane-result]]]
          ::
          =^  compiled  out  (depend-on compile)
          ::  compilation blocked; produce block on sub-build
          ::
          ?~  compiled
            (return-blocks ~[compile])
          ::  compilation failed; error out
          ::
          ?.  ?=([~ %success %ride *] compiled)
            (wrap-error compiled)
          ::  compilation succeeded: produce resulting +vase
          ::
          (return-result %success %plan vase.u.compiled)
      ::  +compose-result: the result of a single composition
      ::
      +=  compose-result
        $%  [%subject subject=cage]
            [%block builds=(list ^build)]
            [%error message=tang]
        ==
      ::  +compose-cranes: runs each crane and composes the results
      ::
      ::    For each crane in :cranes, runs it and composes its result into a
      ::    new subject, which is returned if there are no errors or blocks.
      ::
      ++  compose-cranes
        |=  [subject=cage cranes=(list crane)]
        ^-  $:  compose-result
                _..compose-cranes
            ==
        ::
        ?~  cranes
          [[%subject subject] ..compose-cranes]
        ::
        =^  result  ..compose-cranes  (run-crane subject i.cranes)
        ?+    -.result  [result ..compose-cranes]
        ::
            %subject
          $(cranes t.cranes, subject [%noun (slop q.subject.result q.subject)])
        ==
      ::  +run-crane: runs an individual :crane against :subject
      ::
      ++  run-crane
        |=  [subject=cage =crane]
        ^-  compose-cranes
        ::
        |^  ?-  -.crane
              %fssg  (run-fssg +.crane)
              %fsbc  (run-fsbc +.crane)
              %fsbr  (run-fsbr +.crane)
              %fsts  (run-fsts +.crane)
              %fscm  (run-fscm +.crane)
              %fspm  (run-fspm +.crane)
              %fscb  (run-fscb +.crane)
              %fsdt  (run-fsdt +.crane)
              %fssm  (run-fssm +.crane)
              %fscl  (run-fscl +.crane)
              %fskt  (run-fskt +.crane)
              %fstr  (run-fstr +.crane)
              %fszp  (run-fszp +.crane)
              %fszy  (run-fszy +.crane)
            ==
        ::  +run-fssg: runs the `/~` rune
        ::
        ++  run-fssg
          |=  =hoon
          ^-  compose-cranes
          ::
          =/  ride-build=^build
            [date.build [%ride hoon [%$ subject]]]
          =^  ride-result  out  (depend-on ride-build)
          ?~  ride-result
            [[%block [ride-build]~] ..run-crane]
          ?:  ?=([~ %error *] ride-result)
            [[%error [leaf+"/~ failed: " message.u.ride-result]] ..run-crane]
          ?>  ?=([~ %success %ride *] ride-result)
          [[%subject %noun vase.u.ride-result] ..run-crane]
        ::  +run-fsbc: runs the `/$` rune
        ::
        ++  run-fsbc
          |=  =hoon
          ^-  compose-cranes
          ::
          =/  query-compile-build=^build
            [date.build [%ride ((jock |) query-string) [%$ %noun !>(~)]]]
          =^  query-compile-result  out  (depend-on query-compile-build)
          ?~  query-compile-result
            [[%block [query-compile-build]~] ..run-crane]
          ?:  ?=([~ %error *] query-compile-result)
            :-  [%error [leaf+"/; failed: " message.u.query-compile-result]]
            ..run-crane
          ?>  ?=([~ %success %ride *] query-compile-result)
          ::
          =/  =beam
            =,  path-to-render
            [[ship.disc desk.disc [%da date.build]] spur]
          =+  arguments=(slop !>(beam) vase.u.query-compile-result)
          ::
          =/  call-build=^build
            [date.build [%call [%ride hoon [%$ subject]] [%$ %noun arguments]]]
          =^  call-result  out  (depend-on call-build)
          ?~  call-result
            [[%block [call-build]~] ..run-crane]
          ?:  ?=([~ %error *] call-result)
            [[%error [leaf+"/; failed: " message.u.call-result]] ..run-crane]
          ?>  ?=([~ %success %call *] call-result)
          ::
          [[%subject %noun vase.u.call-result] ..run-crane]
        ::  +run-fsbr: runs the `/|` rune
        ::
        ++  run-fsbr
          |=  choices=(list ^crane)
          ^-  compose-cranes
          ::
          ?~  choices
            [[%error [leaf+"/| failed: out of options"]~] ..run-crane]
          ::
          =^  child  ..run-crane  (run-crane subject i.choices)
          ?.  ?=([%error *] child)
            [child ..run-crane]
          $(choices t.choices)
        ::  +run-fsts: runs the `/=` rune
        ::
        ++  run-fsts
          |=  [face=term sub-crane=^crane]
          ^-  compose-cranes
          ::
          =^  child  ..run-crane  (run-crane subject sub-crane)
          ?.  ?=([%subject *] child)
            [child ..run-crane]
          :_  ..run-crane
          :*  %subject
              p.subject.child
              [[%face face p.q.subject.child] q.q.subject.child]
          ==
        ::  +run-fscm: runs the `/,` rune
        ::
        ++  run-fscm
          |=  cases=(list [=spur crane=^crane])
          ^-  compose-cranes
          ::
          ?~  cases
            [[%error [leaf+"/, failed: no match"]~] ..run-crane]
          ::
          ?.  .=  spur.i.cases
              (scag (lent spur.i.cases) (flop spur.path-to-render))
            $(cases t.cases)
          ::
          (run-crane subject crane.i.cases)
        ::  +run-fspm: runs the `/&` rune
        ::
        ++  run-fspm
          |=  [marks=(list mark) sub-crane=^crane]
          ^-  compose-cranes
          ::
          =^  child  ..run-crane  (run-crane subject sub-crane)
          ?.  ?=([%subject *] child)
            [child ..run-crane]
          ::
          =/  cast-build=^build
            :-  date.build
            |-
            ^-  schematic
            ?~  marks
              ::  TODO: If we were keeping track of the mark across runes, this
              ::  wouldn't have %noun here. This is a case where it might matter.
              ::
              [%$ subject.child]
            [%cast disc.source-rail.scaffold i.marks $(marks t.marks)]
          =^  cast-result  out  (depend-on cast-build)
          ?~  cast-result
            [[%block [cast-build]~] ..run-crane]
          ::
          ?:  ?=([~ %error *] cast-result)
            [[%error [leaf+"/& failed: " message.u.cast-result]] ..run-crane]
          ?>  ?=([~ %success %cast *] cast-result)
          ::
          [[%subject cage.u.cast-result] ..run-crane]
        ::  +run-fscb: runs the `/_` rune
        ::
        ++  run-fscb
          |=  sub-crane=^crane
          ^-  compose-cranes
          ::  perform a scry to get the contents of +path-to-render
          ::
          =/  toplevel-build=^build
            [date.build [%scry [%c %y path-to-render]]]
          ::
          =^  toplevel-result  out  (depend-on toplevel-build)
          ?~  toplevel-result
            [[%block ~[toplevel-build]] ..run-crane]
          ::
          ?:  ?=([~ %error *] toplevel-result)
            :-  [%error [leaf+"/_ failed: " message.u.toplevel-result]]
            ..run-crane
          ?>  ?=([~ %success %scry *] toplevel-result)
          ::
          =/  toplevel-arch=arch  ;;(arch q.q.cage.u.toplevel-result)
          ::  sub-path: each possible sub-directory to check
          ::
          =/  sub-paths=(list @ta)
            (turn ~(tap by dir.toplevel-arch) head)
          ::  for each directory in :toplevel-arch, issue a sub-build
          ::
          =/  sub-builds=(list ^build)
            %+  turn  sub-paths
            |=  sub=@ta
            ^-  ^build
            :-  date.build
            [%scry [%c %y path-to-render(spur [sub spur.path-to-render])]]
          ::  results: accumulator for results of sub-builds
          ::
          =|  $=  results
              (list [kid=^build sub-path=@ta results=(unit build-result)])
          ::  resolve all the :sub-builds
          ::
          =/  subs-results
            |-  ^+  [results out]
            ?~  sub-builds  [results out]
            ?>  ?=(^ sub-paths)
            ::
            =/  kid=^build  i.sub-builds
            =/  sub-path=@ta  i.sub-paths
            ::
            =^  result  out  (depend-on kid)
            =.  results  [[kid sub-path result] results]
            ::
            $(sub-builds t.sub-builds, sub-paths t.sub-paths)
          ::  apply mutations from depending on sub-builds
          ::
          =:  results  -.subs-results
              out      +.subs-results
          ==
          ::  split :results into completed :mades and incomplete :blocks
          ::
          =+  ^=  split-results
              (skid results |=([* * r=(unit build-result)] ?=(^ r)))
          ::
          =/  mades=_results   -.split-results
          =/  blocks=_results  +.split-results
          ::  if any builds blocked, produce them all in %blocks
          ::
          ?^  blocks
            [[%block (turn `_results`blocks head)] ..run-crane]
          ::  find the first error and return it if exists
          ::
          =/  errors=_results
            %+  skim  results
            |=  [* * r=(unit build-result)]
            ?=([~ %error *] r)
          ?^  errors
            ?>  ?=([~ %error *] results.i.errors)
            [[%error message.u.results.i.errors] ..run-crane]
          ::  get a list of valid sub-paths
          ::
          ::    :results is now a list of the :build-result of %cy on each path
          ::    in :toplevel-arch. What we want is to now filter this list so
          ::    that we filter files out.
          ::
          =/  sub-paths=(list [=rail sub-path=@ta])
            %+  murn  results
            |=  [build=^build sub-path=@ta result=(unit build-result)]
            ^-  (unit [rail @ta])
            ::
            ?>  ?=([@da %scry %c %y *] build)
            ?>  ?=([~ %success %scry *] result)
            =/  =arch  ;;(arch q.q.cage.u.result)
            ::
            ?~  dir.arch
              ~
            `[rail.resource.schematic.build sub-path]
          ::  keep track of the original value so we can reset it
          ::
          =/  old-path-to-render  path-to-render
          ::  apply each of the filtered :sub-paths to the :sub-crane.
          ::
          =^  crane-results  ..run-crane
            %+  roll  sub-paths
            |=  $:  [=rail sub-path=@ta]
                    $=  accumulator
                    [(list [sub-path=@ta =compose-result]) _..run-crane]
                ==
            =.  ..run-crane  +.accumulator
            =.  path-to-render  rail
            =^  result  ..run-crane  (run-crane subject sub-crane)
            [[[sub-path result] -.accumulator] ..run-crane]
          ::  set :path-to-render back
          ::
          =.  path-to-render  old-path-to-render
          ::  if any sub-cranes error, return the first error
          ::
          =/  error-list=(list [@ta =compose-result])
            %+  skim  crane-results
            |=  [@ta =compose-result]
            =(%error -.compose-result)
          ::
          ?^  error-list
            [compose-result.i.error-list ..run-crane]
          ::  if any sub-cranes block, return all blocks
          ::
          =/  block-list=(list ^build)
            =|  block-list=(list ^build)
            |-
            ^+  block-list
            ?~  crane-results
              block-list
            ?.  ?=(%block -.compose-result.i.crane-results)
              $(crane-results t.crane-results)
            =.  block-list
              (weld builds.compose-result.i.crane-results block-list)
            $(crane-results t.crane-results)
          ::
          ?^  block-list
            [[%block block-list] ..run-crane]
          ::  put the data in map order
          ::
          =/  result-map=(map @ta vase)
            %-  my
            %+  turn  crane-results
            |=  [path=@ta =compose-result]
            ^-  (pair @ta vase)
            ::
            ?>  ?=([%subject *] compose-result)
            [path q.subject.compose-result]
          ::  convert the map into a flat format for return
          ::
          ::    This step flattens the values out of the map for return. Let's
          ::    say we're doing a /_ over a directory of files that just have a
          ::    single @ud in them. We want the return value of /_ to have the
          ::    nest in (map @ta @ud) instead of returning a (map @ta vase).
          ::
          =/  as-vase=vase
            |-
            ^-  vase
            ::
            ?~  result-map
              [[%atom %n `0] 0]
            ::
            %+  slop
              (slop [[%atom %ta ~] p.n.result-map] q.n.result-map)
            (slop $(result-map l.result-map) $(result-map r.result-map))
          ::
          [[%subject %noun as-vase] ..run-crane]
        ::  +run-fsdt: runs the `/.` rune
        ::
        ++  run-fsdt
          |=  sub-cranes=(list ^crane)
          ^-  compose-cranes
          ::
          =^  list-results  ..run-crane
            %+  roll  sub-cranes
            |=  $:  sub-crane=^crane
                    accumulator=[(list compose-result) _..run-crane]
                ==
            =.  ..run-crane  +.accumulator
            =^  result  ..run-crane  (run-crane subject sub-crane)
            [[result -.accumulator] ..run-crane]
          ::  if any sub-cranes error, return the first error
          ::
          =/  error-list=(list compose-result)
            %+  skim  list-results
            |=  =compose-result
            =(%error -.compose-result)
          ::
          ?^  error-list
            [i.error-list ..run-crane]
          ::  if any sub-cranes block, return all blocks
          ::
          =/  block-list=(list ^build)
            =|  block-list=(list ^build)
            |-
            ^+  block-list
            ?~  list-results
              block-list
            ?.  ?=(%block -.i.list-results)
              $(list-results t.list-results)
            =.  block-list  (weld builds.i.list-results block-list)
            $(list-results t.list-results)
          ::
          ?^  block-list
            [[%block block-list] ..run-crane]
          ::  concatenate all the results together with null termination
          ::
          =.  list-results  (flop list-results)
          ::
          =/  final-result=vase
            |-
            ^-  vase
            ?~  list-results
              [[%atom %n `~] 0]
            ?>  ?=(%subject -.i.list-results)
            (slop q.subject.i.list-results $(list-results t.list-results))
          ::
          [[%subject %noun final-result] ..run-crane]
        ::  +run-fssm: runs the `/;` rune
        ::
        ++  run-fssm
          |=  [=hoon sub-crane=^crane]
          ^-  compose-cranes
          ::
          =^  child  ..run-crane  (run-crane subject sub-crane)
          ?.  ?=([%subject *] child)
            [child ..run-crane]
          ::
          =/  call-build=^build
            [date.build [%call [%ride hoon [%$ subject]] [%$ subject.child]]]
          =^  call-result  out  (depend-on call-build)
          ?~  call-result
            [[%block [call-build]~] ..run-crane]
          ?:  ?=([~ %error *] call-result)
            [[%error [leaf+"/; failed: " message.u.call-result]] ..run-crane]
          ?>  ?=([~ %success %call *] call-result)
          ::
          [[%subject %noun vase.u.call-result] ..run-crane]
        ::  +run-fscl: runs the `/:` rune
        ::
        ++  run-fscl
          |=  [=truss sub-crane=^crane]
          ^-  compose-cranes
          ::
          =/  beam-to-render=beam
            [[ship.disc desk.disc %ud 0] spur]:path-to-render
          ::
          =/  hoon-parser  (vang & (en-beam beam-to-render))
          ::
          =+  tuz=(posh:hoon-parser truss)
          ?~  tuz
            [[%error [leaf+"/: failed: bad tusk: {<truss>}"]~] ..run-crane]
          =+  pax=(plex:hoon-parser %clsg u.tuz)
          ?~  pax
            [[%error [leaf+"/: failed: bad path: {<u.tuz>}"]~] ..run-crane]
          =+  bem=(de-beam u.pax)
          ?~  bem
            [[%error [leaf+"/: failed: bad beam: {<u.pax>}"]~] ..run-crane]
          ::
          =.  path-to-render  [[p q] s]:u.bem
          (run-crane subject sub-crane)
        ::  +run-fskt: runs the `/^` rune
        ::
        ++  run-fskt
          |=  [=spec sub-crane=^crane]
          ^-  compose-cranes
          ::
          =^  child  ..run-crane  (run-crane subject sub-crane)
          ?.  ?=([%subject *] child)
            [child ..run-crane]
          ::
          =/  bunt-build=^build
            [date.build [%ride [%kttr spec] [%$ subject]]]
          =^  bunt-result  out  (depend-on bunt-build)
          ?~  bunt-result
            [[%block [bunt-build]~] ..run-crane]
          ?:  ?=([~ %error *] bunt-result)
            [[%error [leaf+"/^ failed: " message.u.bunt-result]] ..run-crane]
          ?>  ?=([~ %success %ride *] bunt-result)
          ::
          ?.  (~(nest ut p.vase.u.bunt-result) | p.q.subject.child)
            [[%error [leaf+"/^ failed: nest-fail"]~] ..run-crane]
          :_  ..run-crane
          [%subject %noun [p.vase.u.bunt-result q.q.subject.child]]
        ::  +run-fstr: runs the `/*` rune
        ::
        ::    TODO: some duplicate code with +run-fscb
        ::
        ++  run-fstr
          |=  sub-crane=^crane
          ^-  compose-cranes
          ::
          =/  tree-build=^build
            [date.build [%scry [%c %t path-to-render]]]
          ::
          =^  tree-result  out  (depend-on tree-build)
          ?~  tree-result
            [[%block ~[tree-build]] ..run-crane]
          ::
          ?:  ?=([~ %error *] tree-result)
            :-  [%error [%leaf "/* failed: "] message.u.tree-result]
            ..run-crane
          ?>  ?=([~ %success %scry *] tree-result)
          ::
          =/  file-list=(list path)  ;;((list path) q.q.cage.u.tree-result)
          ::  trim file extensions off the file paths
          ::
          ::    This is pretty ugly, but Ford expects :path-to-render not to
          ::    have a file extension, so we need to trim it off each path.
          ::
          =.  file-list
            ::  deduplicate since multiple files could share a trimmed path
            ::
            =-  ~(tap in (~(gas in *(set path)) `(list path)`-))
            %+  turn  file-list
            |=  =path
            ^+  path
            (scag (sub (lent path) 1) path)
          ::
          =/  old-path-to-render  path-to-render
          ::  apply each of the paths in :file-list to the :sub-crane
          ::
          =^  crane-results  ..run-crane
            %+  roll  file-list
            |=  $:  =path
                    $=  accumulator
                    [(list [=path =compose-result]) _..run-crane]
                ==
            =.  ..run-crane  +.accumulator
            =.  spur.path-to-render  (flop path)
            ::
            =^  result  ..run-crane  (run-crane subject sub-crane)
            [[[path result] -.accumulator] ..run-crane]
          ::
          =.  path-to-render  old-path-to-render
          ::  if any sub-cranes error, return the first error
          ::
          =/  error-list=(list [=path =compose-result])
            %+  skim  crane-results
            |=  [=path =compose-result]
            =(%error -.compose-result)
          ::
          ?^  error-list
            [compose-result.i.error-list ..run-crane]
          ::  if any sub-cranes block, return all blocks
          ::
          =/  block-list=(list ^build)
            =|  block-list=(list ^build)
            |-  ^+  block-list
            ?~  crane-results  block-list
            ::
            ?.  ?=(%block -.compose-result.i.crane-results)
              $(crane-results t.crane-results)
            =.  block-list
              (weld builds.compose-result.i.crane-results block-list)
            ::
            $(crane-results t.crane-results)
          ::
          ?^  block-list
            [[%block block-list] ..run-crane]
          ::
          =/  result-map=(map path vase)
            %-  my
            %+  turn  crane-results
            |=  [=path =compose-result]
            ^-  (pair ^path vase)
            ::
            ?>  ?=(%subject -.compose-result)
            [path q.subject.compose-result]
          ::
          =/  as-vase
            =/  path-type  -:!>(*path)
            |-  ^-  vase
            ?~  result-map  [[%atom %n `0] 0]
            ::
            %+  slop
              (slop [path-type p.n.result-map] q.n.result-map)
            (slop $(result-map l.result-map) $(result-map r.result-map))
          ::
          [[%subject %noun as-vase] ..run-crane]
        ::  +run-fszp: runs the `/!mark/` "rune"
        ::
        ++  run-fszp
          |=  =mark
          ^-  compose-cranes
          ::
          =/  hoon-path=rail
            =,  path-to-render
            [disc [%hoon spur]]
          ::
          =/  hood-build=^build  [date.build [%hood hoon-path]]
          =^  hood-result  out  (depend-on hood-build)
          ?~  hood-result
            [[%block [hood-build]~] ..run-crane]
          ?:  ?=([~ %error *] hood-result)
            [[%error [leaf+"/! failed: " message.u.hood-result]] ..run-crane]
          ?>  ?=([~ %success %hood *] hood-result)
          ::
          =/  plan-build=^build
            :-  date.build
            [%plan path-to-render query-string scaffold.u.hood-result]
          =^  plan-result  out  (depend-on plan-build)
          ?~  plan-result
            [[%block [plan-build]~] ..run-crane]
          ?:  ?=([~ %error *] plan-result)
            [[%error [leaf+"/! failed: " message.u.plan-result]] ..run-crane]
          ?>  ?=([~ %success %plan *] plan-result)
          ::  if :mark is %noun, don't perform mark translation; just return
          ::
          ::    If we were to verify the product type with %noun, this would
          ::    cast to *, which would overwrite :vase.u.plan-result's actual
          ::    product type
          ::
          ?:  =(%noun mark)
            [[%subject %noun vase.u.plan-result] ..run-crane]
          ::
          =/  vale-build=^build
            :-  date.build
            [%vale disc.source-rail.scaffold mark q.vase.u.plan-result]
          =^  vale-result  out  (depend-on vale-build)
          ?~  vale-result
            [[%block [vale-build]~] ..run-crane]
          ?:  ?=([~ %error *] vale-result)
            [[%error [leaf+"/! failed: " message.u.vale-result]] ..run-crane]
          ?>  ?=([~ %success %vale *] vale-result)
          ::
          [[%subject cage.u.vale-result] ..run-crane]
        ::  +run-fszy: runs the `/mark/` "rune"
        ::
        ++  run-fszy
          |=  =mark
          ^-  compose-cranes
          ::
          =/  bake-build=^build
            :-  date.build
            [%bake mark query-string path-to-render]
          =^  bake-result  out  (depend-on bake-build)
          ?~  bake-result
            [[%block [bake-build]~] ..run-crane]
          ?:  ?=([~ %error *] bake-result)
            :_  ..run-crane
            [%error [leaf+"/{(trip mark)}/ failed: " message.u.bake-result]]
          ?>  ?=([~ %success %bake *] bake-result)
          ::
          [[%subject cage.u.bake-result] ..run-crane]
        --
      ::  +gather-path-builds: produce %path builds to resolve import paths
      ::
      ++  gather-path-builds
        |=  imports=(list [prefix=?(%sur %lib) =cable])
        ^-  (list ^build)
        ::
        %+  turn  imports
        |=  [prefix=?(%sur %lib) =cable]
        ^-  ^build
        [date.build [%path disc.source-rail.scaffold prefix file-path.cable]]
      ::  +resolve-builds: run a list of builds and collect results
      ::
      ::    If a build blocks, put its +tang in :error-message and stop.
      ::    All builds that block get put in :blocks. Results of
      ::    successful builds are produced in :results.
      ::
      ++  resolve-builds
        =|  results=(list build-result)
        |=  builds=(list ^build)
        ^+  [results ..^$]
        ::
        ?~  builds
          [results ..^$]
        ::
        =^  result  out  (depend-on i.builds)
        ?~  result
          =.  blocks  [i.builds blocks]
          $(builds t.builds)
        ::
        ?.  ?=(%success -.u.result)
          =.  error-message  [[%leaf "%plan failed: "] message.u.result]
          [results ..^$]
        ::
        =.  results  [u.result results]
        $(builds t.builds)
      ::  +gather-core-builds: produce %core builds from resolved paths
      ::
      ++  gather-core-builds
        |=  path-results=(list build-result)
        ^-  (list ^build)
        %+  turn  path-results
        |=  result=build-result
        ^-  ^build
        ::
        ?>  ?=([%success %path *] result)
        ::
        [date.build [%core rail.result]]
      ::  +link-imports: link libraries and structures with standard library
      ::
      ::    Prepends each library vase onto the standard library vase.
      ::    Wraps a face around each library to prevent namespace leakage
      ::    unless imported as *lib-name.
      ::
      ++  link-imports
        |=  $:  imports=(list [?(%lib %sur) =cable])
                reef=vase
                core-results=(list build-result)
            ==
        ^-  vase
        ::
        =/  subject=vase  reef
        ::
        =/  core-vases=(list vase)
          %+  turn  core-results
          |=  result=build-result
          ^-  vase
          ?>  ?=([%success %core *] result)
          vase.result
        ::  link structures and libraries into a subject for compilation
        ::
        |-  ^+  subject
        ?~  core-vases  subject
        ?<  ?=(~ imports)
        ::  cons this vase onto the head of the subject
        ::
        =.  subject
          %-  slop  :_  subject
          ::  check if the programmer named the library
          ::
          ?~  face.cable.i.imports
            ::  no face assigned to this library, so use vase as-is
            ::
            i.core-vases
          ::  use the library name as a face to prevent namespace leakage
          ::
          ^-  vase
          [[%face u.face.cable.i.imports p.i.core-vases] q.i.core-vases]
        ::
        $(core-vases t.core-vases, imports t.imports)
      --
    ::
    ++  make-reef
      ~%  %make-reef  ..^^$  ~
      |=  =disc
      ^-  build-receipt
      ::
      =/  hoon-scry
        [date.build [%scry %c %x [disc /hoon/hoon/sys]]]
      ::
      =^  hoon-scry-result  out  (depend-on hoon-scry)
      ::
      =/  arvo-scry
        [date.build [%scry %c %x [disc /hoon/arvo/sys]]]
      ::
      =^  arvo-scry-result  out  (depend-on arvo-scry)
      ::
      =/  zuse-scry
        [date.build [%scry %c %x [disc /hoon/zuse/sys]]]
      ::
      =^  zuse-scry-result  out  (depend-on zuse-scry)
      ::
      =|  blocks=(list ^build)
      =?  blocks  ?=(~ hoon-scry-result)  [hoon-scry blocks]
      =?  blocks  ?=(~ arvo-scry-result)  [arvo-scry blocks]
      =?  blocks  ?=(~ zuse-scry-result)  [zuse-scry blocks]
      ::
      ?^  blocks
        (return-blocks blocks)
      ::
      ?.  ?=([~ %success %scry *] hoon-scry-result)
        (wrap-error hoon-scry-result)
      ::
      ?.  ?=([~ %success %scry *] arvo-scry-result)
        (wrap-error arvo-scry-result)
      ::
      ?.  ?=([~ %success %scry *] zuse-scry-result)
        (wrap-error zuse-scry-result)
      ::
      ::  short-circuit to .pit during boot
      ::
      ::    This avoids needing to recompile the kernel if we're asked
      ::    to build %hoon one the home desk, at revision 1 or 2.
      ::
      ?:  ?&  =(our ship.disc)
              ?=(?(%base %home) desk.disc)
          ::
              =/  =beam
                [[ship.disc desk.disc [%da date.build]] /hoon/hoon/sys]
              =/  cass
                (scry [%141 %noun] [~ %cw beam])
              ?=([~ ~ %cass * ?(%1 %2) *] cass)
          ==
        ::
        (return-result %success %reef pit)
      ::  omit case from path to prevent cache misses
      ::
      =/  hoon-path=path
        /(scot %p ship.disc)/(scot %tas desk.disc)/hoon/hoon/sys
      =/  hoon-hoon=(each hoon tang)
        %-  mule  |.
        (rain hoon-path ;;(@t q.q.cage.u.hoon-scry-result))
      ?:  ?=(%| -.hoon-hoon)
        (return-error leaf+"ford: %reef failed to compile hoon" p.hoon-hoon)
      ::
      =/  arvo-path=path
        /(scot %p ship.disc)/(scot %tas desk.disc)/hoon/arvo/sys
      =/  arvo-hoon=(each hoon tang)
        %-  mule  |.
        (rain arvo-path ;;(@t q.q.cage.u.arvo-scry-result))
      ?:  ?=(%| -.arvo-hoon)
        (return-error leaf+"ford: %reef failed to compile arvo" p.arvo-hoon)
      ::
      =/  zuse-path=path
        /(scot %p ship.disc)/(scot %tas desk.disc)/hoon/zuse/sys
      =/  zuse-hoon=(each hoon tang)
        %-  mule  |.
        (rain zuse-path ;;(@t q.q.cage.u.zuse-scry-result))
      ?:  ?=(%| -.zuse-hoon)
        (return-error leaf+"ford: %reef failed to compile zuse" p.zuse-hoon)
      ::
      =/  zuse-build=^build
        :*  date.build
            %ride  p.zuse-hoon
            ::  hoon for `..is` to grab the :pit out of the arvo core
            ::
            %ride  [%cnts ~[[%& 1] %is] ~]
            %ride  p.arvo-hoon
            %ride  [%$ 7]
            %ride  p.hoon-hoon
            [%$ %noun !>(~)]
        ==
      ::
      =^  zuse-build-result  out  (depend-on zuse-build)
      ?~  zuse-build-result
        (return-blocks [zuse-build]~)
      ::
      ?.  ?=([~ %success %ride *] zuse-build-result)
        (wrap-error zuse-build-result)
      ::
      (return-result %success %reef vase.u.zuse-build-result)
    ::
    ++  make-ride
      ~%  %make-ride  ..^^$  ~
      |=  [formula=hoon =schematic]
      ^-  build-receipt
      ::
      =^  result  out  (depend-on [date.build schematic])
      ?~  result
        (return-blocks [date.build schematic]~)
      ::
      =*  subject-vase  q:(result-to-cage u.result)
      =/  slim-schematic=^schematic  [%slim p.subject-vase formula]
      =^  slim-result  out  (depend-on [date.build slim-schematic])
      ?~  slim-result
        (return-blocks [date.build slim-schematic]~)
      ::
      ?:  ?=([~ %error *] slim-result)
        %-  return-error
        :*  [%leaf "ford: %ride failed to compute type:"]
            message.u.slim-result
        ==
      ::
      ?>  ?=([~ %success %slim *] slim-result)
      ::
      =/  =compiler-cache-key  [%ride formula subject-vase]
      =^  cached-result  out  (access-cache compiler-cache-key)
      ?^  cached-result
        (return-result u.cached-result)
      ::
      =/  val
        (mock [q.subject-vase nock.u.slim-result] intercepted-scry)
      ::  val is a toon, which might be a list of blocks.
      ::
      ?-    -.val
      ::
          %0
        (return-result %success %ride [type.u.slim-result p.val])
      ::
          %1
        =/  blocked-paths=(list path)  ;;((list path) p.val)
        (blocked-paths-to-receipt %ride blocked-paths)
      ::
          %2
        (return-error [[%leaf "ford: %ride failed to execute:"] p.val])
      ==
    ::
    ++  make-same
      ~%  %make-same  ..^^$  ~
      |=  =schematic
      ^-  build-receipt
      ::
      =^  result  out  (depend-on [date.build schematic])
      ::
      ?~  result
        (return-blocks [date.build schematic]~)
      (return-result u.result)
    ::
    ++  make-scry
      ~%  %make-scry  ..^^$  ~
      |=  =resource
      ^-  build-receipt
      ::  construct a full +beam to make the scry request
      ::
      =/  =beam          (extract-beam resource `date.build)
      =/  =scry-request  [vane.resource care.resource beam]
      ::  perform scry operation if we don't already know the result
      ::
      ::    Look up :scry-request in :scry-results.per-event to avoid
      ::    rerunning a previously blocked +scry.
      ::
      =/  scry-response
        ?:  (~(has by scry-results) scry-request)
          (~(get by scry-results) scry-request)
        (scry [%141 %noun] ~ `@tas`(cat 3 [vane care]:resource) beam)
      ::  scry blocked
      ::
      ?~  scry-response
        (return-blocks ~)
      ::  scry failed
      ::
      ?~  u.scry-response
        %-  return-error
        :~  leaf+"scry failed for"
            leaf+:(weld "%c" (trip care.resource) " " (spud (en-beam beam)))
        ==
      ::  scry succeeded
      ::
      (return-result %success %scry u.u.scry-response)
    ::
    ++  make-slim
      ~%  %make-slim  ..^^$  ~
      |=  [subject-type=type formula=hoon]
      ^-  build-receipt
      ::
      =/  =compiler-cache-key  [%slim subject-type formula]
      =^  cached-result  out  (access-cache compiler-cache-key)
      ?^  cached-result
        (return-result u.cached-result)
      ::
      =/  compiled=(each (pair type nock) tang)
        (mule |.((~(mint ut subject-type) [%noun formula])))
      ::
      %_    out
          result
        ?-  -.compiled
          %|  [%build-result %error [leaf+"ford: %slim failed: " p.compiled]]
          %&  [%build-result %success %slim p.compiled]
        ==
      ==
    ::  TODO: Take in +type instead of +vase?
    ::
    ++  make-slit
      ~%  %make-slit  ..^^$  ~
      |=  [gate=vase sample=vase]
      ^-  build-receipt
      ::
      =/  =compiler-cache-key  [%slit p.gate p.sample]
      =^  cached-result  out  (access-cache compiler-cache-key)
      ?^  cached-result
        (return-result u.cached-result)
      ::
      =/  product=(each type tang)
        (mule |.((slit p.gate p.sample)))
      ::
      %_    out
          result
        ?-  -.product
          %|  :*  %build-result   %error
                  :*  (~(dunk ut p.sample) %have)
                      (~(dunk ut (~(peek ut p.gate) %free 6)) %want)
                      leaf+"ford: %slit failed:"
                      p.product
                  ==
              ==
          %&  [%build-result %success %slit p.product]
        ==
      ==
    ::
    ++  make-volt
      ~%  %make-volt  ..^^$  ~
      |=  [=disc mark=term input=*]
      ^-  build-receipt
      ::
      =/  bunt-build=^build  [date.build [%bunt disc mark]]
      ::
      =^  bunt-result  out  (depend-on bunt-build)
      ?~  bunt-result
        (return-blocks [bunt-build]~)
      ::
      ?:  ?=([~ %error *] bunt-result)
        %-  return-error
        :-  [%leaf "ford: %volt {<mark>} on {<disc>} failed:"]
        message.u.bunt-result
      ::
      ?>  ?=([~ %success %bunt *] bunt-result)
      ::
      =/  =build-result
        [%success %volt [mark p.q.cage.u.bunt-result input]]
      ::
      (return-result build-result)
    ::
    ++  make-vale
      ~%  %make-vale  ..^^$  ~
      ::  TODO: better docs
      ::
      |=  [=disc mark=term input=*]
      ^-  build-receipt
      ::  don't validate for the %noun mark
      ::
      ?:  =(%noun mark)
        =/  =build-result  [%success %vale [%noun %noun input]]
        ::
        (return-result build-result)
      ::
      =/  path-build  [date.build [%path disc %mar mark]]
      ::
      =^  path-result  out  (depend-on path-build)
      ?~  path-result
        (return-blocks [path-build]~)
      ::
      ?:  ?=([~ %error *] path-result)
        %-  return-error
        :-  leaf+"ford: %vale failed while searching for {<mark>}:"
        message.u.path-result
      ::
      ?>  ?=([~ %success %path *] path-result)
      ::
      =/  bunt-build=^build  [date.build [%bunt disc mark]]
      ::
      =^  bunt-result  out  (depend-on bunt-build)
      ?~  bunt-result
        (return-blocks [bunt-build]~)
      ::
      ?.  ?=([~ %success %bunt *] bunt-result)
        (wrap-error bunt-result)
      ::
      =/  mark-sample=vase  q.cage.u.bunt-result
      ::
      =/  call-build=^build
        :^    date.build
            %call
          ^=  gate
          :*  %ride
              ::  (ream 'noun:grab')
              formula=`hoon`[%tsld [%wing ~[%noun]] [%wing ~[%grab]]]
              subject=`schematic`[%core rail.u.path-result]
          ==
        sample=[%$ %noun %noun input]
      ::
      =^  call-result  out  (depend-on call-build)
      ?~  call-result
        (return-blocks [call-build]~)
      ::
      ?:  ?=([~ %error *] call-result)
        ::
        %-  return-error
        =/  =beam
          [[ship.disc desk.disc %da date.build] spur.rail.u.path-result]
        :*  :-  %leaf
            "ford: %vale failed: invalid input for mark: {<(en-beam beam)>}"
            message.u.call-result
        ==
      ::
      ?>  ?=([~ %success %call *] call-result)
      =/  product=vase  vase.u.call-result
      ::  +grab might produce the wrong type
      ::
      ?.  (~(nest ut p.mark-sample) | p.product)
        %-  return-error
        :~  leaf+"ford: %vale failed"
            leaf+"+grab has wrong type in mark {<mark>} on disc {<disc>}"
        ==
      ::
      =/  =build-result
        [%success %vale [mark p.mark-sample q.product]]
      ::
      (return-result build-result)
    ::
    ++  make-walk
      ~%  %make-walk  ..^^$  ~
      |=  [=disc source=term target=term]
      ^-  build-receipt
      ::  define some types used in this gate
      ::
      =>  |%
          ::  +load-node: a queued arm to run from a mark core
          ::
          +=  load-node  [type=?(%grab %grow) mark=term]
          ::  edge-jug: directed graph from :source mark to :target marks
          ::
          ::    :source can be converted to :target either by running
          ::    its own +grow arm, or by running the target's +grab arm.
          ::
          +=  edge-jug  (jug source=term [target=term arm=?(%grow %grab)])
          ::  mark-path: a path through the mark graph
          ::
          ::    +mark-path represents a series of mark translation
          ::    operations to be performed to 'walk' from one mark to another.
          ::
          ::    +mark-action is defined in Zuse. It represents a conversion
          ::    from a source mark to a target mark, and it specifies
          ::    whether it will use +grow or +grab.
          ::
          +=  mark-path  (list mark-action)
          --
      ::
      |^  ^-  build-receipt
          ?:  =(source target)
            (return-result %success %walk ~)
          ::  load all marks.
          ::
          =^  marks-result  out
            (load-marks-reachable-from [[%grow source] [%grab target] ~])
          ?~  -.marks-result
            out
          ::  find a path through the graph
          ::
          ::    Make a list of individual mark translation actions which will
          ::    take us from :source to :term.
          ::
          =/  path  (find-path-through u.-.marks-result)
          ::  if there is no path between these marks, give an error message
          ::
          ?~  path
            ::  we failed; surface errors from +load-marks-reachable-from
            ::
            =/  braces  [[' ' ' ' ~] ['{' ~] ['}' ~]]
            =/  errors=(list tank)
              %-  zing
              %+  turn  ~(tap in +.marks-result)
              |=  [mark=term err=tang]
              ^-  tang
              :~  [%leaf :(weld "while compiling " (trip mark) ":")]
                  [%rose braces err]
              ==
            ::
            %_    out
                result
              :*  %build-result  %error
                  :*  :-  %leaf
                      ;:  weld
                        "ford: no mark path from "  (trip source)  " to "
                        (trip target)
                      ==
                      errors
              ==  ==
            ==
          ::
          (return-result %success %walk path)
      ::  +load-marks-reachable-from: partial mark graph loading
      ::
      ::    While we can just load all marks in the %/mar directory, this is
      ::    rather slow. What we do instead is traverse forwards and backwards
      ::    from the source and target marks: we start at the source mark,
      ::    check all the grow arms, and then check their grow arms. At the
      ::    same time, we start from the target mark, check all the grab arms,
      ::    and then check their grab arms. This gives us a much smaller
      ::    dependency set than loading the entire %/mar directory.
      ::
      ++  load-marks-reachable-from
        |=  queued-nodes=(list load-node)
        ::  list of nodes in the graph that we've already checked
        ::
        =|  visited=(set load-node)
        ::  graph of the available edges
        ::
        =|  =edge-jug
        ::  compile-failures: mark files which didn't compile
        ::
        =|  compile-failures=(map term tang)
        ::
        |-
        ^-  [[(unit ^edge-jug) _compile-failures] _out]
        ::  no ?~ to prevent tmi
        ::
        ?:  =(~ queued-nodes)
          [[`edge-jug compile-failures] out]
        ::
        =/  nodes-and-schematics
          %+  turn  queued-nodes
          |=  =load-node
          ^-  [^load-node schematic]
          :-  load-node
          [%path disc %mar mark.load-node]
        ::  get the path for each mark name
        ::
        ::    For %path builds, any ambiguous path is just filtered out.
        ::
        =^  maybe-path-results  out
          %-  perform-schematics  :*
            ;:  weld
              "ford: %walk from "  (trip source)  " to "  (trip target)
              " contained failures:"
            ==
            nodes-and-schematics
            %filter-errors
            *load-node
          ==
        ?~  maybe-path-results
          [[~ ~] out]
        ::
        =/  nodes-and-cores
          %+  turn  u.maybe-path-results
          |=  [=load-node =build-result]
          ^-  [^load-node schematic]
          ::
          ?>  ?=([%success %path *] build-result)
          ::
          :-  load-node
          [%core rail.build-result]
        ::
        =^  maybe-core-results  out
          %-  perform-schematics  :*
            ;:  weld
              "ford: %walk from "  (trip source)  " to "  (trip target)
              " contained failures:"
            ==
            nodes-and-cores
            %ignore-errors
            *load-node
          ==
        ?~  maybe-core-results
          [[~ ~] out]
        ::  clear the queue before we process the new results
        ::
        =.  queued-nodes  ~
        ::
        =/  cores  u.maybe-core-results
        ::
        |-
        ?~  cores
          ^$
        ::  mark this node as visited
        ::
        =.  visited  (~(put in visited) key.i.cores)
        ::  add core errors to compile failures
        ::
        =?  compile-failures  ?=([%error *] result.i.cores)
          %+  ~(put by compile-failures)  mark.key.i.cores
          message.result.i.cores
        ::
        =/  target-arms=(list load-node)
          ?.  ?=([%success %core *] result.i.cores)
            ~
          ?:  =(%grow type.key.i.cores)
            (get-arms-of-type %grow vase.result.i.cores)
          (get-arms-of-type %grab vase.result.i.cores)
        ::  filter places we know we've already been.
        ::
        =.  target-arms
          %+  skip  target-arms  ~(has in visited)
        =.  queued-nodes  (weld target-arms queued-nodes)
        ::
        =.  edge-jug
          |-
          ?~  target-arms
            edge-jug
          ::
          =.  edge-jug
            ?-    type.i.target-arms
            ::
                %grab
              (~(put ju edge-jug) mark.i.target-arms [mark.key.i.cores %grab])
            ::
                %grow
              (~(put ju edge-jug) mark.key.i.cores [mark.i.target-arms %grow])
            ==
          $(target-arms t.target-arms)
        ::
        $(cores t.cores)
      ::
      ++  get-arms-of-type
        |=  [type=?(%grab %grow) =vase]
        ^-  (list load-node)
        ::  it is valid for this node to not have a +grow arm.
        ::
        ?.  (slob type p.vase)
          ~
        ::
        %+  turn
          (sloe p:(slap vase [%limb type]))
        |=  arm=term
        [type arm]
      ::  +find-path-through: breadth first search over the mark graph
      ::
      ++  find-path-through
        |=  edges=edge-jug
        ^-  mark-path
        ::  the source node starts out visited
        =/  visited-nodes=(set mark)  [source ~ ~]
        ::  these paths are flopped so we're always inserting to the front.
        =|  path-queue=(qeu mark-path)
        ::  start the queue with all the edges which start at the source mark
        ::
        =.  path-queue
          =/  start-links  (find-links-in-edges edges source)
          ::
          |-
          ^+  path-queue
          ?~  start-links
            path-queue
          ::
          =.  path-queue  (~(put to path-queue) [i.start-links]~)
          ::
          $(start-links t.start-links)
        ::
        |-
        ^-  mark-path
        ::
        ?:  =(~ path-queue)
          ::  no path found
          ~
        =^  current  path-queue  [p q]:~(get to path-queue)
        ?>  ?=(^ current)
        ::
        ?:  =(target target.i.current)
          ::  we have a completed path. paths in the queue are backwards
          (flop current)
        ::
        =+  next-steps=(find-links-in-edges edges target.i.current)
        ::  filter out already visited nodes
        ::
        =.  next-steps
          %+  skip  next-steps
          |=  link=mark-action
          (~(has in visited-nodes) source.link)
        ::  then add the new ones to the set of already visited nodes
        ::
        =.  visited-nodes
          (~(gas in visited-nodes) (turn next-steps |=(mark-action source)))
        ::  now all next steps go in the queue
        ::
        =.  path-queue
          %-  ~(gas to path-queue)
          %+  turn  next-steps
          |=  new-link=mark-action
          [new-link current]
        ::
        $
      ::  +find-links-in-edges: gets edges usable by +find-path-through
      ::
      ::    This deals with disambiguating between %grab and %grow so we always
      ::    pick %grab over %grow.
      ::
      ++  find-links-in-edges
        |=  [edges=edge-jug source=term]
        ^-  (list mark-action)
        ::
        =+  links=~(tap in (~(get ju edges) source))
        ::
        =|  results=(set mark-action)
        |-
        ^-  (list mark-action)
        ?~  links
          ~(tap in results)
        ::
        ?-    arm.i.links
            %grab
          ::  if :results has a %grow entry, remove it before adding our %grab
          =/  grow-entry=mark-action  [%grow source target.i.links]
          =?  results  (~(has in results) grow-entry)
            (~(del in results) grow-entry)
          ::
          =.  results  (~(put in results) [%grab source target.i.links])
          $(links t.links)
        ::
            %grow
          ::  if :results has a %grab entry, don't add a %grow entry
          ?:  (~(has in results) [%grab source target.i.links])
            $(links t.links)
          ::
          =.  results  (~(put in results) [%grow source target.i.links])
          $(links t.links)
        ==
      --
    ::  |utilities:make: helper arms
    ::
    ::+|  utilities
    ::
    ::  +perform-schematics: helper function that performs a list of builds
    ::
    ::    We often need to run a list of builds. This helper method will
    ::    depend on all :builds, will return a +build-receipt of either the
    ::    blocks or the first error, or a list of all completed results.
    ::
    ::    This is a wet gate so individual callers can associate their own
    ::    key types with schematics.
    ::
    ++  perform-schematics
      |*  $:  failure=tape
              builds=(list [key=* =schematic])
              on-error=?(%fail-on-errors %filter-errors %ignore-errors)
              key-bunt=*
          ==
      ^-  $:  (unit (list [key=_key-bunt result=build-result]))
              _out
          ==
      ::
      |^  =^  results  out
            =|  results=(list [_key-bunt ^build (unit build-result)])
            |-
            ^+  [results out]
            ::
            ?~  builds
              [results out]
            ::
            =/  sub-build=^build  [date.build schematic.i.builds]
            =^  result  out  (depend-on sub-build)
            =.  results  [[key.i.builds sub-build result] results]
            ::
            $(builds t.builds)
          ?:  =(%fail-on-errors on-error)
            (check-errors results)
          ?:  =(%filter-errors on-error)
            (filter-errors results)
          (handle-rest results)
      ::
      ++  check-errors
        |=  results=(list [_key-bunt ^build (unit build-result)])
        ::
        =/  braces  [[' ' ' ' ~] ['{' ~] ['}' ~]]
        =/  errors=(list tank)
          %+  murn  results
          |=  [* * result=(unit build-result)]
          ^-  (unit tank)
          ?.  ?=([~ %error *] result)
            ~
          `[%rose braces message.u.result]
        ::
        ?^  errors
          :-  ~
          %-  return-error
          :-  [%leaf failure]
          errors
        ::
        (handle-rest results)
      ::
      ++  filter-errors
        |=  results=(list [_key-bunt ^build (unit build-result)])
        =.  results
          %+  skip  results
          |=  [* * r=(unit build-result)]
          ?=([~ %error *] r)
        (handle-rest results)
      ::
      ++  handle-rest
        |=  results=(list [_key-bunt ^build (unit build-result)])
        ::  if any sub-builds blocked, produce all blocked sub-builds
        ::
        =/  blocks=(list ^build)
          %+  murn  `(list [* ^build (unit build-result)])`results
          |=  [* sub=^build result=(unit build-result)]
          ^-  (unit ^build)
          ?^  result
            ~
          `sub
        ::
        ?^  blocks
          [~ (return-blocks blocks)]
        ::
        :_  out
        :-  ~
        %+  turn  results
        |*  [key=_key-bunt ^build result=(unit build-result)]
        ^-  [_key-bunt build-result]
        [key (need result)]
      --
    ::  +wrap-error: wrap an error message around a failed sub-build
    ::
    ++  wrap-error
      |=  result=(unit build-result)
      ^-  build-receipt
      ::
      ?>  ?=([~ %error *] result)
      =/  message=tang
        [[%leaf "ford: {<-.schematic.build>} failed: "] message.u.result]
      ::
      (return-error message)
    ::  +return-blocks: exit +make as a blocked build
    ::
    ++  return-blocks
      |=  builds=(list ^build)
      ^-  build-receipt
      out(result [%blocks builds])
    ::  +return-error: exit +make with a specific failure message
    ::
    ++  return-error
      |=  =tang
      ^-  build-receipt
      out(result [%build-result %error tang])
    ::  +return-result: exit +make with a completed build
    ::
    ++  return-result
      |=  =build-result
      ^-  build-receipt
      out(result [%build-result build-result])
    ::
    ++  access-cache
      |=  =compiler-cache-key
      ^-  [(unit build-result) _out]
      ::
      ?~  entry=(~(get by lookup.compiler-cache.state) compiler-cache-key)
        [~ out(cache-access `[compiler-cache-key new=%.y])]
      ::
      [`val.u.entry out(cache-access `[compiler-cache-key new=%.n])]
    ::
    ++  depend-on
      |=  kid=^build
      ^-  [(unit build-result) _out]
      ::
      ?:  =(kid build)
        ~|  [%depend-on-self (build-to-tape kid)]
        !!
      ::
      =.  sub-builds.out  [kid sub-builds.out]
      ::  +access-build-record will mutate :results.state
      ::
      ::    It's okay to ignore this because the accessed-builds get gathered
      ::    and merged during the +reduce step.
      ::
      =/  maybe-build-record  -:(access-build-record kid)
      ?~  maybe-build-record
        [~ out]
      ::
      =*  build-record  u.maybe-build-record
      ?:  ?=(%tombstone -.build-record)
        [~ out]
      ::
      [`build-result.build-record out]
    ::  +blocked-paths-to-receipt: handle the %2 case for mock
    ::
    ::    Multiple schematics handle +toon instances. This handles the %2 case
    ::    for a +toon and transforms it into a +build-receipt so we depend on
    ::    the blocked paths correctly.
    ::
    ++  blocked-paths-to-receipt
      |=  [name=term blocked-paths=(list path)]
      ^-  build-receipt
      ::
      =/  blocks-or-failures=(list (each ^build tank))
        %+  turn  blocked-paths
        |=  =path
        ::
        =/  scry-request=(unit scry-request)  (path-to-scry-request path)
        ?~  scry-request
          [%| [%leaf "ford: {<name>}: invalid scry path: {<path>}"]]
        ::
        =*  case  r.beam.u.scry-request
        ::
        ?.  ?=(%da -.case)
          [%| [%leaf "ford: {<name>}: invalid case in scry path: {<path>}"]]
        ::
        =/  date=@da  p.case
        ::
        =/  resource=(unit resource)  (path-to-resource path)
        ?~  resource
          :-  %|
          [%leaf "ford: {<name>}: invalid resource in scry path: {<path>}"]
        ::
        =/  sub-schematic=schematic  [%pin date %scry u.resource]
        ::
        [%& `^build`[date sub-schematic]]
      ::
      =/  failed=tang
        %+  murn  blocks-or-failures
        |=  block=(each ^build tank)
        ^-  (unit tank)
        ?-  -.block
          %&  ~
          %|  `p.block
        ==
      ::
      ?^  failed
        ::  some failed
        ::
        out(result [%build-result %error failed])
      ::  no failures
      ::
      =/  blocks=(list ^build)
        %+  turn  blocks-or-failures
        |=  block=(each ^build tank)
        ?>  ?=(%& -.block)
        ::
        p.block
      ::
      =.  out
        %+  roll  blocks
        |=  [block=^build accumulator=_out]
        =.  out  accumulator
        +:(depend-on [date.block schematic.block])
      ::
      (return-blocks blocks)
    --
  ::  |utilities:per-event: helper arms
  ::
  ::+|  utilities
  ::
  ::  +got-build: lookup :build in state, asserting presence
  ::
  ++  got-build
    |=  =build
    ^-  build-status
    ~|  [%ford-missing-build build=(build-to-tape build) duct=duct]
    (~(got by builds.state) build)
  ::  +add-build: store a fresh, unstarted build in the state
  ::
  ++  add-build
    ~/  %add-build
    |=  =build
    ^+  state
    ::  don't overwrite an existing entry
    ::
    ?:  (~(has by builds.state) build)
      state
    ::
    %_    state
        builds-by-schematic
      (~(put by-schematic builds-by-schematic.state) build)
    ::
        builds
      %+  ~(put by builds.state)  build
      =|  =build-status
      build-status(state [%untried ~])
    ==
  ::  +remove-builds: remove builds and their sub-builds
  ::
  ++  remove-builds
    ~/  %remove-builds
    |=  builds=(list build)
    ::
    |^  ^+  state
        ::
        ?~  builds
          state
        ::
        ?~  maybe-build-status=(~(get by builds.state) i.builds)
          $(builds t.builds)
        =/  subs  ~(tap in ~(key by subs.u.maybe-build-status))
        ::
        =^  removed  state  (remove-single-build i.builds u.maybe-build-status)
        ?.  removed
          $(builds t.builds)
        ::
        $(builds (welp t.builds subs))
    ::  +remove-build: stop storing :build in the state
    ::
    ::    Removes all linkages to and from sub-builds
    ::
    ++  remove-single-build
      |=  [=build =build-status]
      ^+  [removed=| state]
      ::  never delete a build that something depends on
      ::
      ?^  clients.build-status
        [removed=| state]
      ?^  requesters.build-status
        [removed=| state]
      ::  nothing depends on :build, so we'll remove it
      ::
      :-  removed=&
      ::
      %_    state
          builds-by-schematic
        (~(del by-schematic builds-by-schematic.state) build)
      ::
          builds
        (~(del by builds.state) build)
      ==
    --
  ::  +update-build-status: replace :build's +build-status by running a function
  ::
  ++  update-build-status
    ~/  %update-build-status
    |=  [=build update-func=$-(build-status build-status)]
    ^-  [build-status builds=_builds.state]
    ::
    =/  original=build-status  (got-build build)
    =/  mutant=build-status  (update-func original)
    ::
    [mutant (~(put by builds.state) build mutant)]
  ::  +intercepted-scry: augment real scry with local %scry build results
  ::
  ::    Try to deduplicate requests for possibly remote resources by looking up
  ::    the result in local state if the real scry has no synchronous
  ::    answer (it produced `~`).
  ::
  ++  intercepted-scry
    %-  sloy  ^-  slyd
    ~/  %intercepted-scry
    |=  [ref=* (unit (set monk)) =term =beam]
    ^-  (unit (unit (cask meta)))
    ::  if the actual scry produces a value, use that value; otherwise use local
    ::
    =/  scry-response  (scry +<.$)
    ::
    ?^  scry-response
      scry-response
    ::
    =/  vane=(unit %c)  ((soft ,%c) (end 3 1 term))
    ?~  vane
      ~
    =/  care=(unit care:clay)  ((soft care:clay) (rsh 3 1 term))
    ?~  care
      ~
    ?.  ?=(%da -.r.beam)
      ~
    =/  =resource  [u.vane u.care rail=[[p.beam q.beam] s.beam]]
    =/  =build     [date=p.r.beam %scry resource]
    ::  look up the scry result from our permanent state
    ::
    ::    Note: we can't freshen :build's :last-accessed date because
    ::    we can't mutate :state from this gate. %scry results might get
    ::    deleted during %wipe more quickly than they should because of this.
    ::
    =/  local-result  -:(access-build-record build)
    ?~  local-result
      ~
    ?:  ?=(%tombstone -.u.local-result)
      ~
    ::
    =/  local-cage=cage  (result-to-cage build-result.u.local-result)
    ::  if :local-result does not nest in :type, produce an error
    ::
    ?.  -:(nets:wa +.ref `type`p.q.local-cage)
      [~ ~]
    ::
    [~ ~ local-cage]
  ::  +unblock-clients-on-duct: unblock and produce clients blocked on :build
  ::
  ++  unblock-clients-on-duct
    =|  unblocked=(list build)
    ~%  %unblock-clients-on-duct  +>+  ~
    |=  =build
    ^+  [unblocked builds.state]
    ::
    =/  =build-status  (got-build build)
    ::
    =/  clients=(list ^build)  ~(tap in (~(get ju clients.build-status) [%duct duct]))
    ::
    |-
    ^+  [unblocked builds.state]
    ?~  clients
      [unblocked builds.state]
    ::
    =^  client-status  builds.state
      %+  update-build-status  i.clients
      |=  client-status=^build-status
      ::
      =.  subs.client-status
        %+  ~(jab by subs.client-status)  build
        |=  original=build-relation
        original(blocked |)
      ::
      =?    state.client-status
          ?&  ?=(%blocked -.state.client-status)
          ::
              ?!
              %-  ~(any by subs.client-status)
              |=(build-relation &(blocked verified))
          ==
        ::
        [%unblocked ~]
      client-status
    ::
    =?  unblocked  !?=(%blocked -.state.client-status)
      [i.clients unblocked]
    ::
    $(clients t.clients)
  ::  +on-build-complete: handles completion of any build
  ::
  ++  on-build-complete
    ~/  %on-build-complete
    |=  =build
    ^+  ..execute
    ::
    =.  ..execute  (cleanup-orphaned-provisional-builds build)
    ::
    =/  duct-status  (~(got by ducts.state) duct)
    ::
    =/  =build-status  (got-build build)
    ?:  (~(has in requesters.build-status) [%duct duct])
      (on-root-build-complete build)
    ::
    =^  unblocked-clients  builds.state  (unblock-clients-on-duct build)
    =.  candidate-builds  (~(gas in candidate-builds) unblocked-clients)
    ::
    ..execute
  ::  +on-root-build-complete: handle completion or promotion of a root build
  ::
  ::    When a build completes for a duct, we might have to send a %made move
  ::    on the requesting duct and also do duct and build book-keeping.
  ::
  ++  on-root-build-complete
    ~/  %on-root-build-complete
    |=  =build
    ^+  ..execute
    ::
    =;  res=_..execute
        =/  duct-status=(unit duct-status)
          (~(get by ducts.state.res) duct)
        ?~  duct-status  res
        ::  debugging assertions to try to track down failure in
        ::  +copy-build-tree-as-provisional
        ::
        ~|  [%failed-to-preserve-live-build (build-to-tape build)]
        ?>  ?=(%live -.live.u.duct-status)
        ~|  %failed-2
        ?>  ?=(^ last-sent.live.u.duct-status)
        ~|  %failed-3
        ?>  .=  build
            [date.u.last-sent.live.u.duct-status root-schematic.u.duct-status]
        ~|  %failed-4
        ?>  (~(has by builds.state.res) build)
        ::
        res
    ::
    =/  =build-status  (got-build build)
    =/  =duct-status  (~(got by ducts.state) duct)
    ::  make sure we have something to send
    ::
    ?>  ?=([%complete %value *] state.build-status)
    ::  send a %made move unless it's an unchanged live build
    ::
    =?    moves
        ?!
        ?&  ?=(%live -.live.duct-status)
            ?=(^ last-sent.live.duct-status)
            ::
            =/  last-build-status
              %-  ~(got by builds.state)
              [date.u.last-sent.live.duct-status schematic.build]
            ::
            ?>  ?=(%complete -.state.last-build-status)
            ?&  ?=(%value -.build-record.state.last-build-status)
            ::
                .=  build-result.build-record.state.last-build-status
                    build-result.build-record.state.build-status
        ==  ==
      :_  moves
      ^-  move
      ::
      :*  duct  %give  %made  date.build  %complete
          build-result.build-record.state.build-status
      ==
    ::
    ?-    -.live.duct-status
        %once
      =.  ducts.state  (~(del by ducts.state) duct)
      =.  state  (move-root-to-cache build)
      ::
      ..execute
    ::
        %live
      ::  clean up previous build
      ::
      =?  state  ?=(^ last-sent.live.duct-status)
        =/  old-build=^build  build(date date.u.last-sent.live.duct-status)
        ~?  =(date.build date.old-build)
          :+  "old and new builds have same date, will probably crash!"
            (build-to-tape build)
          (build-to-tape old-build)
        ::
        (remove-anchor-from-root old-build [%duct duct])
      ::
      =/  resource-list=(list [=disc resources=(set resource)])
        ~(tap by (collect-live-resources build))
      ::  we can only handle a single subscription
      ::
      ::    In the long term, we need Clay's interface to change so we can
      ::    subscribe to multiple desks at the same time.
      ::
      ?:  (lth 1 (lent resource-list))
        =.  ..execute
          %+  send-incomplete  build  :~
            [%leaf "root build {(build-to-tape build)}"]
            [%leaf "on duct:"]
            [%leaf "{<duct>}"]
            [%leaf "tried to subscribe to multiple discs:"]
            [%leaf "{<resource-list>}"]
          ==
        ::  delete this instead of caching it, since it wasn't right
        ::
        =.  ducts.state  (~(del by ducts.state) duct)
        =.  state  (remove-anchor-from-root build [%duct duct])
        ..execute
      ::
      =/  subscription=(unit subscription)
        ?~  resource-list
          ~
        `[date.build disc.i.resource-list resources.i.resource-list]
      ::
      =?  ..execute  ?=(^ subscription)
        (start-clay-subscription u.subscription)
      ::
      =.  ducts.state
        %+  ~(put by ducts.state)  duct
        %_    duct-status
            live
          [%live in-progress=~ last-sent=`[date.build subscription]]
        ==
      ::
      ..execute
    ==
  ::  +send-incomplete: emit a move indicating we can't complete :build
  ::
  ++  send-incomplete
    |=  [=build message=tang]
    ^+  ..execute
    ::
    =.  moves
      :_  moves
      `move`[duct %give %made date.build %incomplete message]
    ::
    ..execute
  ::  +cleanup-orphaned-provisional-builds: delete extraneous sub-builds
  ::
  ::    Remove unverified linkages to sub builds. If a sub-build has no other
  ::    clients on this duct, then it is orphaned and we remove the duct from
  ::    its subs and call +cleanup on it.
  ::
  ++  cleanup-orphaned-provisional-builds
    ~/  %cleanup-orphaned-provisional-builds
    |=  =build
    ^+  ..execute
    ::
    =/  =build-status  (got-build build)
    ::
    =/  orphans=(list ^build)
      %+  murn  ~(tap by subs.build-status)
      |=  [sub=^build =build-relation]
      ^-  (unit ^build)
      ::
      ?:  verified.build-relation
        ~
      `sub
    ::  dequeue orphans in case we were about to run them
    ::
    =/  orphan-set        (~(gas in *(set ^build)) orphans)
    =.  next-builds       (~(dif in next-builds) orphan-set)
    =.  candidate-builds  (~(dif in candidate-builds) orphan-set)
    ::  remove links to orphans in :build's +build-status
    ::
    =^  build-status  builds.state
      %+  update-build-status  build
      |=  build-status=^build-status
      %_    build-status
          subs
        ::
        |-  ^+  subs.build-status
        ?~  orphans  subs.build-status
        ::
        =.  subs.build-status  (~(del by subs.build-status) i.orphans)
        ::
        $(orphans t.orphans)
      ==
    ::
    =/  =anchor  [%duct duct]
    ::
    |-  ^+  ..execute
    ?~  orphans  ..execute
    ::  remove link to :build in :i.orphan's +build-status
    ::
    =^  orphan-status  builds.state
      %+  update-build-status  i.orphans
      |=  orphan-status=_build-status
      %_  orphan-status
        clients  (~(del ju clients.orphan-status) anchor build)
      ==
    ::
    ?:  (~(has by clients.orphan-status) anchor)
      $(orphans t.orphans)
    ::  :build was the last client on this duct so remove it
    ::
    =.  builds.state  (remove-anchor-from-subs i.orphans anchor)
    =.  state  (cleanup i.orphans)
    $(orphans t.orphans)
  ::  +access-build-record: access a +build-record, updating :last-accessed
  ::
  ::    Usage:
  ::    ```
  ::    =^  maybe-build-record  builds.state  (access-build-record build)
  ::    ```
  ::
  ++  access-build-record
    ~/  %access-build-record
    |=  =build
    ^-  [(unit build-record) _builds.state]
    ::
    ?~  maybe-build-status=(~(get by builds.state) build)
      [~ builds.state]
    ::
    =/  =build-status  u.maybe-build-status
    ::
    ?.  ?=(%complete -.state.build-status)
      [~ builds.state]
    ::
    ?:  ?=(%tombstone -.build-record.state.build-status)
      [`build-record.state.build-status builds.state]
    ::
    =.  last-accessed.build-record.state.build-status  now
    ::
    :-  `build-record.state.build-status
    (~(put by builds.state) build build-status)
  ::  +cleanup: try to clean up a build and its sub-builds
  ::
  ++  cleanup
    ~/  %cleanup
    |=  =build
    ^+  state
    ::   does this build even exist?!
    ::
    ?~  maybe-build-status=(~(get by builds.state) build)
      state
    ::
    =/  =build-status  u.maybe-build-status
    ::  never delete a build that something depends on
    ::
    ?^  clients.build-status
      state
    ?^  requesters.build-status
      state
    ::
    (remove-builds ~[build])
  ::  +collect-live-resources: produces all live resources from sub-scrys
  ::
  ++  collect-live-resources
    ~/  %collect-live-resources
    |=  =build
    ^-  (jug disc resource)
    ::
    ?:  ?=(%scry -.schematic.build)
      =*  resource  resource.schematic.build
      (my [(extract-disc resource) (sy [resource]~)]~)
    ::
    ?:  ?=(%pin -.schematic.build)
      ~
    ::
    =/  subs  ~(tap in ~(key by subs:(got-build build)))
    =|  resources=(jug disc resource)
    |-
    ?~  subs
      resources
    ::
    =/  sub-resources=(jug disc resource)  ^$(build i.subs)
    =.  resources  (unify-jugs resources sub-resources)
    $(subs t.subs)
  ::  +collect-blocked-resources: produces all blocked resources from sub-scrys
  ::
  ++  collect-blocked-sub-scrys
    ~/  %collect-blocked-sub-scrys
    |=  =build
    ^-  (set scry-request)
    ::
    ?:  ?=(%scry -.schematic.build)
      =,  resource.schematic.build
      =/  =scry-request
        :+  vane  care
        ^-  beam
        [[ship.disc.rail desk.disc.rail [%da date.build]] spur.rail]
      (sy [scry-request ~])
    ::  only recurse on blocked sub-builds
    ::
    =/  subs=(list ^build)
      %+  murn  ~(tap by subs:(got-build build))
      |=  [sub=^build =build-relation]
      ^-  (unit ^build)
      ::
      ?.  blocked.build-relation
        ~
      `sub
    ::
    =|  scrys=(set scry-request)
    |-
    ^+  scrys
    ?~  subs
      scrys
    ::
    =.  scrys  (~(uni in scrys) ^$(build i.subs))
    $(subs t.subs)
  ::  +start-clay-subscription: listen for changes in the filesystem
  ::
  ++  start-clay-subscription
    ~/  %start-clay-subscription
    |=  =subscription
    ^+  ..execute
    ::
    =/  already-subscribed=?
      (~(has by pending-subscriptions.state) subscription)
    ::
    =.  pending-subscriptions.state
      (put-request pending-subscriptions.state subscription duct)
    ::  don't send a duplicate move if we're already subscribed
    ::
    ?:  already-subscribed
      ..execute
    ::
    =/  =wire  (clay-subscription-wire [date disc]:subscription)
    ::
    =/  =note
      ::  request-contents: the set of [care path]s to subscribe to in clay
      ::
      =/  request-contents=(set [care:clay path])
        %-  sy  ^-  (list [care:clay path])
        %+  murn  ~(tap in `(set resource)`resources.subscription)
        |=  =resource  ^-  (unit [care:clay path])
        ::
        `[care.resource (flop spur.rail.resource)]
      ::  if :request-contents is `~`, this code is incorrect
      ::
      ?<  ?=(~ request-contents)
      ::  their: requestee +ship
      ::
      =+  [their desk]=disc.subscription
      ::
      :^  %c  %warp  ship=their
      ^-  riff:clay
      [desk `[%mult `case`[%da date.subscription] request-contents]]
    ::
    =.  moves  [`move`[duct [%pass wire note]] moves]
    ::
    ..execute
  ::  +cancel-clay-subscription: remove a subscription on :duct
  ::
  ++  cancel-clay-subscription
    ~/  %cancel-clay-subscription
    |=  =subscription
    ^+  ..execute
    ::
    =^  originator  pending-subscriptions.state
      (del-request pending-subscriptions.state subscription duct)
    ::  if there are still other ducts on this subscription, don't send a move
    ::
    ?~  originator
      ..execute
    ::
    =/  =wire  (clay-subscription-wire [date disc]:subscription)
    ::
    =/  =note
      =+  [their desk]=disc.subscription
      [%c %warp ship=their `riff:clay`[desk ~]]
    ::
    =.  moves  [`move`[u.originator [%pass wire note]] moves]
    ::
    ..execute
  ::  +clay-sub-wire: the wire to use for a clay subscription
  ::
  ::    While it is possible for two different root builds to make
  ::    subscriptions with the same wire, those wires will always be associated
  ::    with different ducts, so there's no risk of duplicates.
  ::
  ++  clay-subscription-wire
    |=  [date=@da =disc]
    ^-  wire
    ::
    =+  [their desk]=disc
    ::
    /clay-sub/(scot %p their)/[desk]/(scot %da date)
  ::  +start-scry-request: kick off an asynchronous request for a resource
  ::
  ++  start-scry-request
    |=  =scry-request
    ^+  ..execute
    ::  if we are the first block depending on this scry, send a move
    ::
    =/  already-started=?  (~(has by pending-scrys.state) scry-request)
    ::
    =.  pending-scrys.state
      (put-request pending-scrys.state scry-request duct)
    ::  don't send a duplicate move if we've already sent one
    ::
    ?:  already-started
      ..execute
    ::
    =/  =wire  (scry-request-wire scry-request)
    ::
    =/  =note
      =,  scry-request
      =/  =disc  [p q]:beam
      :*  %c  %warp  their=ship.disc  desk.disc
          `[%sing care case=r.beam (flop s.beam)]
      ==
    ::
    =.  moves  [`move`[duct [%pass wire note]] moves]
    ::
    ..execute
  ::  +cancel-scry-request: cancel a pending asynchronous scry request
  ::
  ++  cancel-scry-request
    |=  =scry-request
    ^+  ..execute
    ::
    =^  originator  pending-scrys.state
      (del-request pending-scrys.state scry-request duct)
    ::  if there are still other ducts on this subscription, don't send a move
    ::
    ?~  originator
      ..execute
    ::
    =/  =wire  (scry-request-wire scry-request)
    ::
    =/  =note
      =+  [their desk]=[p q]:beam.scry-request
      [%c %warp ship=their `riff:clay`[desk ~]]
    ::
    =.  moves  [`move`[u.originator [%pass wire note]] moves]
    ::
    ..execute
  ::  +scry-request-wire
  ::
  ++  scry-request-wire
    |=  =scry-request
    ^-  wire
    (welp /scry-request (scry-request-to-path scry-request))
  --
--
::
::  end the =~
::
.  ==
::
::::  vane interface
  ::
::  begin with a default +axle as a blank slate
::
=|  ax=axle
::  a vane is activated with identity, the current date, entropy,
::  and a namespace function
::
|=  [our=ship now=@da eny=@uvJ scry-gate=sley]
=*  ford-gate  .
::  allow jets to be registered within this core
::
~%  %ford  ..is  ~
|%
::  +call: handle a +task:able from arvo
::
::    Ford can be tasked with:
::
::      %build: perform a build
::      %keep: resize caches
::      %kill: cancel a build
::      %wipe: clear memory
::
::    Most requests get converted into operations to be performed inside
::    the +per-event core, which is Ford's main build engine.
::
++  call
  |=  [=duct dud=(unit goof) type=* wrapped-task=(hobo task:able)]
  ^-  [(list move) _ford-gate]
  ?^  dud
    ~|(%ford-call-dud (mean tang.u.dud))
  ::
  =/  task=task:able  ((harden task:able) wrapped-task)
  ::  we wrap +per-event with a call that binds our event args
  ::
  =*  this-event  (per-event [our duct now scry-gate] state.ax)
  ::
  ?-    -.task
      ::  %build: request to perform a build
      ::
      %build
    ::  perform the build indicated by :task
    ::
    ::    We call :start-build on :this-event, which is the |per-event core
    ::    with the our event-args already bound. :start-build performs the
    ::    build and produces a pair of :moves and a mutant :state.
    ::    We update our :state and produce it along with :moves.
    ::
    =/  =build  [now schematic.task]
    =^  moves  state.ax  (start-build:this-event build live.task)
    ::
    [moves ford-gate]
  ::
      ::  %keep: keep :count cache entries
      ::
      %keep
    ::
    =.  state.ax  (keep:this-event [compiler-cache build-cache]:task)
    ::
    [~ ford-gate]
  ::
      ::  %kill: cancel a %build
      ::
      %kill
    ::
    =^  moves  state.ax  cancel:this-event
    ::
    [moves ford-gate]
  ::
      ::  %trim: in response to memory pressure
      ::
      %trim
    ::
    ?.  =(0 p.task)
      ::  low-priority: remove 50% of cache/stored-builds
      ::
      ~>  %slog.[0 leaf+"ford: trim: pruning caches"]
      =.  state.ax  (wipe:this-event 50)
      [~ ford-gate]
    ::
    ::  high-priority: remove 100% of cache/stored-builds
    ::
    ::    We use %keep to ensure that cache-keys are also purged,
    ::    then restore original limits to allow future caching.
    ::
    ::    XX cancel in-progress builds?
    ::
    ~>  %slog.[0 leaf+"ford: trim: clearing caches"]
    =/  b-max  max-size.queue.build-cache.state.ax
    =/  c-max  max-size.compiler-cache.state.ax
    =.  state.ax  (keep:this-event 0 0)
    =.  state.ax  (keep:this-event c-max b-max)
    [~ ford-gate]
  ::
      ::  %vega: learn of kernel upgrade
      ::
      ::    XX clear cache, rebuild live builds
      ::
      %vega
    ::
    [~ ford-gate]
  ::
      ::  %wipe: wipe stored builds, clearing :percent-to-remove of the entries
      ::
      %wipe
    ::
    =.  state.ax  (wipe:this-event percent-to-remove.task)
    ::
    [~ ford-gate]
  ::
      %wegh
    :_  ford-gate
    :_  ~
    :^  duct  %give  %mass
    ^-  mass
    :+  %ford  %|
    :~  builds+&+builds.state.ax
        compiler-cache+&+compiler-cache.state.ax
        dot+&+ax
    ==
  ==
::  +take: receive a response from another vane
::
::    A +take is a response to a request that Ford made of another vane.
::
::    Ford decodes the type of response based on the +wire in the +take.
::    The possibilities are:
::
::      %clay-sub: Clay notification of an update to a subscription
::
::        If Ford receives this, it will rebuild one or more live builds,
::        taking into account the new date and changed resources.
::
::      %scry-request: Clay response to a request for a resource
::
::        If Ford receives this, it will continue building one or more builds
::        that were blocked on this resource.
::
::    The +sign gets converted into operations to be performed inside
::    the +per-event core, which is Ford's main build engine.
::
++  take
  |=  [=wire =duct dud=(unit goof) wrapped-sign=(hypo sign)]
  ^-  [(list move) _ford-gate]
  ?^  dud
    ~|(%ford-take-dud (mean tang.u.dud))
  ::  unwrap :sign, ignoring unneeded +type in :p.wrapped-sign
  ::
  =/  =sign  q.wrapped-sign
  ::  :wire must at least contain a tag for dispatching
  ::
  ?>  ?=([@ *] wire)
  ::
  |^  ^-  [(list move) _ford-gate]
      ::
      =^  moves  state.ax
        ?+  i.wire     ~|([%bad-take-wire wire] !!)
          %clay-sub      take-rebuilds
          %scry-request  take-unblocks
        ==
      ::
      [moves ford-gate]
    ::  +take-rebuilds: rebuild all live builds affected by the Clay changes
    ::
    ++  take-rebuilds
      ^-  [(list move) ford-state]
      ::
      ~|  [%ford-take-rebuilds wire=wire duct=duct]
      ?>  ?=([@tas %wris *] sign)
      =*  case-sign  p.sign
      =*  care-paths-sign  q.sign
      =+  [ship desk date]=(raid:wired t.wire ~[%p %tas %da])
      =/  disc  [ship desk]
      ::  ignore spurious clay updates
      ::
      ::    Due to asynchronicity of Clay notifications, we might get a
      ::    subscription update on an already-canceled duct.  This is
      ::    normal; no-op.
      ::
      ?~  duct-status=(~(get by ducts.state.ax) duct)
        [~ state.ax]
      ::
      =/  =subscription
        ?>  ?=(%live -.live.u.duct-status)
        (need subscription:(need last-sent.live.u.duct-status))
      ::
      =/  ducts=(list ^duct)
        ::  sanity check; there must be at least one duct per subscription
        ::
        =-  ?<(=(~ -) -)
        (get-request-ducts pending-subscriptions.state.ax subscription)
      ::
      =|  moves=(list move)
      |-  ^+  [moves state.ax]
      ?~  ducts  [moves state.ax]
      ::
      =*  event-args  [[our i.ducts now scry-gate] state.ax]
      =*  rebuild  rebuild:(per-event event-args)
      =^  duct-moves  state.ax
        (rebuild subscription p.case-sign disc care-paths-sign)
      ::
      $(ducts t.ducts, moves (weld moves duct-moves))
    ::  +take-unblocks: unblock all builds waiting on this scry request
    ::
    ++  take-unblocks
      ^-  [(list move) ford-state]
      ::
      ~|  [%ford-take-unblocks wire=wire duct=duct]
      ?>  ?=([@tas %writ *] sign)
      =*  riot-sign  p.sign
      ::  scry-request: the +scry-request we had previously blocked on
      ::
      =/  =scry-request  (need (path-to-scry-request t.wire))
      ::  scry-result: parse a (unit cage) from :sign
      ::
      ::    If the result is `~`, the requested resource was not available.
      ::
      =/  scry-result=(unit cage)
        ?~  riot-sign
          ~
        `r.u.riot-sign
      ::  if spurious Clay response, :ducts will be empty, causing no-op
      ::
      =/  ducts=(list ^duct)
        (get-request-ducts pending-scrys.state.ax scry-request)
      ::
      =|  moves=(list move)
      |-  ^+  [moves state.ax]
      ?~  ducts  [moves state.ax]
      ::
      =*  event-args  [[our i.ducts now scry-gate] state.ax]
      ::  unblock the builds that had blocked on :resource
      ::
      =*  unblock  unblock:(per-event event-args)
      =^  duct-moves  state.ax  (unblock scry-request scry-result)
      ::
      $(ducts t.ducts, moves (weld moves duct-moves))
  --
::  +load: either flush or migrate old state (called on vane reload)
::
::    If it has the old state version, flush the ford state. Otherwise trim
::    build results in case a change to our code invalidated an old build
::    result.
::
::    Flushing state of the old version is a temporary measure for the OS1
::    %publish update. We can flush all build state like this because only gall
::    and %publish use ford live builds currently. :goad will handle remaking
::    builds for gall, and the new %publish does not use ford.
::
++  load
  |=  old=axle
  ^+  ford-gate
  ?:  =(%~2018.12.13 -.old)
    =.  -.ax  %~2020.2.21
    ford-gate
  =.  ax  [%~2020.2.21 state.old]
  =.  ford-gate  +:(call ~[/ford-load-self] ~ *type %trim 0)
  ford-gate
::  +stay: produce current state
::
++  stay  `axle`ax
::  +scry: request a path in the urbit namespace
::
++  scry
  |=  *
  [~ ~]
--
