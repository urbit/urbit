::  ford: build system vane
!:
::    Ford is a functional reactive build system.
::
::    A Ford build is a function of the Urbit namespace and a date that
::    produces marked, typed data or an error.
::
::    The function in the definition of a build is called a "plan,"
::    and it's represented by a Hoon data structure with twenty-five sub-types.
::    A plan is a (possibly trivial) DAG of sub-builds to be performed.
::    The different plan sub-types transform the results of their
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
::    the result of a "live" build consisting of a static plan and the
::    ever-changing current date. Whenever this live build's result changes,
::    Ford sends you the new result and the formal date of the build (the date
::    which would cause the same result if you asked Ford to build that
::    plan again). This is a push-based FRP paradigm.
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
::    asked for a build of the plan with a formal date d2 just before the
::    changed Clay files, Ford would respond with the result of the previous
::    build with formal date d1, which would still be an accurate
::    representation of the plan's result at d2, since Ford knows none of
::    its dependencies changed between d1 and d2.
::
::    Note that Ford can only calculate dependencies after running a build,
::    not before. This is because Ford can be thought of as an interpreter for
::    plans, rather than a compiler, in the sense that it can't have a
::    dependency-gathering step followed by a build step. The dependencies of
::    some plans must be calculated based on results, e.g. the %alts
::    plan, which tries a sequence of sub-builds until one succeeds. If
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
::      Ford maintains two caches: a .hoon-cache that stores
::      content-addressed compiler operations, such as parsing, compiling,
::      and type inference; and a .build-cache that stores previously
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
::  $move: arvo moves that ford can emit
::
+$  move  [=duct card=(wind note gift:able)]
::  $note: private request from ford to another vane
::
+$  note
  $~  [%c %warp *@p *riff:clay]
  $%  [%c $>(%warp task:able:clay)]
  ==
::  $sign: private response from another vane to ford
::
+$  sign
  $~  [%c %writ *riot:clay]
  [?(%b %c) $>($?(%writ %wris) gift:able:clay)]
--
|%
::  $axle: versioned ford state
::
+$  axle  [%0 state=ford-state]
::  $ford-state: all state that ford maintains
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
::    ducts: per-duct state machine for all incoming ducts (build requests)
::    pending-scrys: outgoing requests for static resources
::    pending-subscriptions: outgoing subscriptions on live resources
::    hoon-cache: cache of compiler operations and results
::
+$  ford-state
  $:  ducts=(map duct duct-status)
      pending-scrys=(request-tracker scry-request)
      pending-subscriptions=(request-tracker subscription)
      =hoon-cache
  ==
::  $duct-status: information relating a build to a duct
::
::    live: whether this duct is being run live
::    date: date of most recent build request
::    plan: root plan for build request
::    cur: state of scrys related to this build
::    pre: scry requests and results from previous build, if any
::
+$  duct-status
  $:  live=?
      date=@da
      =plan
      cur=scry-results
      pre=(unit subscription)
  ==
::  $build: a referentially transparent request for a build
::
::    Each unique $build will always produce the same $build-result
::    when run (if it completes). A live build consists of a sequence of
::    instances of $build with the same .plan and increasing .date.
::
::    date: formal date of this build; unrelated to time of execution
::    plan: the plan for how to run this build
::
+$  build  [date=@da =plan]
::  +request-tracker: generic tracker and multiplexer for pending requests
::
::    waiting: ducts blocked on a request
::    originator: the duct that kicked off the request
::
+$  request-tracker
  |$  request-type=mold
  (map request-type [waiting=(set duct) originator=duct])
::
+$  scry-results  (map scry-request (unit (unit cage)))
::  +subscription: a single subscription to changes on a set of resources
::
::    date: subscription formal start date
::    disc: ship and desk for all .resources
::    resources: we will be notified if any of these resources change
::
+$  subscription  [date=@da =disc resources=(set resource)]
::  +scry-request: parsed arguments to a scry operation
::
::    If we add other vanes in the future, this will become a fork type.
::    For now, though, Ford only knows how to make asynchronous scry
::    requests to Clay.
::
::    vane: the vane from which to make the request
::    care: type of request
::    beam: request path
::
+$  scry-request  [vane=%c =care:clay =beam]
::  $product: result of running a $build
::
::    ~         block; no referentially transparent answer     
::    `&+cage:  success
::    `|+tang:  failure, with $tang error message
::
+$  product  (unit (each cage tang))
::  $progress: state of an in-progress $build
::
+$  progress
  $:  blocks=(set scry-request)
      live-resources=(set resource)
      =hoon-cache
  ==
::  $hoon-cache: cache for compiler operations
::
+$  hoon-cache  (clock hoon-cache-key (each cage tang))
::  +hoon-cache-key: cache key for a compiler operation
::
::    %call: +slam (vase of) gate on (vase of) sample
::    %hood: parse file into $scaffold
::    %ride: +slap $hoon against $vase
::    %slim: infer +slap product type
::    %slit: infer +slam product type
::
+$  hoon-cache-key
  $%  [%call gate=vase sample=vase]
      [%hood =beam txt=@t]
      [%ride formula=hoon subject=vase]
      [%slim subject-type=type formula=hoon]
      [%slit gate=type sample=type]
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
::  +segments: compute all paths from .path-part, replacing some `/`s with `-`s
::
::    For example, when passed a .path-part of 'foo-bar-baz',
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
::  +build-to-tape: convert .build to a printable format
::
::    Builds often contain the standard library and large types, so
::    this function should always be called when trying to print a $build.
::
++  build-to-tape
  |=  =build
  ^-  tape
  ~+
  ::
  =/  enclose  |=(tape "[{+<}]")
  =/  date=@da  date.build
  =/  =plan  plan.build
  ::
  %-  enclose
  %+  welp  (trip (scot %da date))
  %+  welp  " "
  ::
  ?+      -.plan
        :(welp "[" (trip -.plan) " {<`@uvI`(mug plan)>}]")
      ::
      %$
    "literal"
  ::
      ^
    %-  enclose
    ;:(welp $(build [date head.plan]) " " $(build [date tail.plan]))
  ::
      %alts
    ;:  welp
      %+  roll  choices.plan
      |=  [choice=^plan txt=_"[alts"]
      :(welp txt " " ^$(plan.build choice))
    ::
      "]"
    ==
  ::
      %core
    :(welp "[core " (spud (en-beam (rail-to-beam source-path.plan))) "]")
  ::
      %hood
    :(welp "[hood " (spud (en-beam (rail-to-beam source-path.plan))) "]")
  ::
      %plan
    ;:  welp
      "[plan "
      (spud (en-beam (rail-to-beam path-to-render.plan)))
      "]"
    ==
  ::
      %scry
    (spud (en-beam (extract-beam resource.plan ~)))
  ::
    ::    %slim
    ::  "slim {<subject-type.plan>} {<formula.plan>}"
  ::
      %vale
    ;:  welp
      "[vale ["
      (trip (scot %p ship.disc.plan))
      " "
      (trip desk.disc.plan)
      "] "
      (trip mark.plan)
      "]"
    ==
  ==
::  +rail-to-beam: convert .rail to a +beam, filling in the case with `[%ud 0]`
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
::  +path-to-scry-request: parse .path's components into .vane, .care, and .rail
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
::    Fills case with [%ud 0] for live resources if .date is `~`.
::    For once resources, ignore .date.
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
::  +get-sub-plans: find any plans contained within .plan
::
++  get-sub-plans
  |=  =plan
  ^-  (list ^plan)
  ?-    -.plan
      ^      ~[head.plan tail.plan]
      %$     ~
      %pin   ~[plan.plan]
      %alts  choices.plan
      %bake  ~
      %bunt  ~
      %call  ~[gate.plan sample.plan]
      %cast  ~[input.plan]
      %core  ~
      %diff  ~[start.plan end.plan]
      %dude  ~[attempt.plan]
      %hood  ~
      %join  ~[first.plan second.plan]
      %list  plans.plan
      %mash  ~[plan.first.plan plan.second.plan]
      %mute  [subject.plan (turn mutations.plan tail)]
      %pact  ~[start.plan diff.plan]
      %path  ~
      %plan  ~
      %reef  ~
      %ride  ~[subject.plan]
      %same  ~[plan.plan]
      %scry  ~
      %slim  ~
      %slit  ~
      %vale  ~
      %volt  ~
      %walk  ~
  ==
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
  ::  remove .duct from the existing .record of this .request
  ::
  =/  record  (~(got by tracker) request)
  =.  waiting.record  (~(del in waiting.record) duct)
  ::  if no more ducts wait on .request, delete it
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
          ::  `/mark/` passes current path through .mark
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
    ::  +wide-or-tall: parses tall form hoon if .allow-tall-form is %.y
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
::    The main internal arm is +run-root-build, which is called from
::    +start-build, +rebuild, and +unblock. +run-root-build defines
::    Ford's build loop.
::
::    TODO: extend with |build-core
::
++  per-event
  ::  moves: the moves to be sent out at the end of this event, reversed
  ::
  =|  moves=(list move)
  ::  gate that produces the +per-event core from event information
  ::
  ~%  %f  ..is  ~
  |=  [[our=@p =duct now=@da scry=sley] state=ford-state]
  ~%  %per-event  +  ~
  |%
  +*  event-core  .
  ::  +finalize: extract moves and state from the +per-event core
  ::
  ++  finalize
    ^-  [(list move) ford-state]
    [(flop moves) state]
  ::  |entry-points: externally fired arms
  ::
  ::+|  entry-points
  ::
  ::  +start-build: perform a fresh $build, either live or once
  ::
  ++  start-build
    ~/  %start-build
    |=  [=build live=?]
    ^-  [(list move) ford-state]
    =<  finalize
    ::  associate .duct with .build in .ducts.state
    ::
    =.  ducts.state
      %+  ~(jab by ducts.state)  duct
      :_  plan.build
      ?:  live
        [%live in-progress=`date.build last-sent=~]
      [%once in-progress=date.build]
    ::
    (run-root-build build live)
  ::  +rebuild: rebuild a live build based on $resource updates
  ::
  ++  rebuild
    ~/  %rebuild
    |=  $:  =subscription
            new-date=@da
            =disc
            care-paths=(set [care=care:clay =path])
        ==
    ^-  [(list move) ford-state]
    ~|  [%rebuilding new-date duct]
    =<  finalize
    ::  mark this subscription as complete now that we've heard a response
    ::
    =.  pending-subscriptions.state
      +:(del-request pending-subscriptions.state subscription duct)
    ::  update build date; asserts .live, not in progress, and new date
    ::
    =.  ducts.state
      %+  ~(jab by ducts.state)  duct
      |=  =duct-status
      ?>  =([& ~] [live cur]:duct-status)
      ?<  =(new-date date.duct-status)
      duct-status(date new-date)
    ::
    (run-root-build new-root-build live=&)
  ::  +unblock: continue builds that had blocked on .resource
  ::
  ::    A build can be stymied temporarily if it depends on a resource
  ::    that must be fetched asynchronously. +unblock is called when
  ::    we receive a response to a resource request that blocked a build.
  ::
  ::    We rebuild the root build from the beginning, leaning on the
  ::    cache so that parts that have already been build don't take as
  ::    long the second time.
  ::
  ++  unblock
    ~/  %unblock
    |=  [=scry-request scry-result=(unit cage)]
    ^-  [(list move) ford-state]
    =<  finalize
    ::  mark this $scry-request as complete now that we have a response
    ::
    =.  pending-scrys.state
      +:(del-request pending-scrys.state scry-request duct)
    ::  place .scry-result in cur.duct-status so repeated scry will complete
    ::
    =/  =duct-status  (got-duct-status duct)
    =.  cur.duct-status  (~(put by cur.duct-status) scry-request scry-result)
    =.  ducts.state  (~(put by ducts.state) duct duct-status)
    ::
    (run-root-build [[date plan] live]:duct-status)
  ::  +wipe: decimate .hoon-cache.state
  ::
  ::    Repeated calls will eventually clear the whole cache.
  ::
  ++  wipe
    ~/  %wipe
    |=  percent-to-remove=@ud
    ^+  state
    ?:  =(0 percent-to-remove)
      ~&  %ford-wipe-no-op
      state
    ::
    =/  num-to-remove=@ud
      ~|  [%wipe percent-to-remove=percent-to-remove]
      ?>  (lte percent-to-remove 100)
      =/  num-total=@ud        size.hoon-cache.state
      =/  percent-to-keep=@ud  (sub 100 percent-to-remove)
      =/  num-to-keep=@ud      (div (mul percent-to-keep num-total) 100)
      (sub num-completed-builds num-to-keep)
    ::
    =/  clock  (by-clock hoon-cache-key build-result)
    =.  hoon-cache.state
      ?:  (lte num-to-remove size.hoon-cache.state)
        (~(trim clock hoon-cache.state) count)
      ~(purge clock hoon-cache.state)
    state
  ::  +keep: resize caches
  ::
  ++  keep
    ~/  %keep
    |=  hoon-cache-size=@ud
    ^+  state
    =-  state(hoon-cache -)
    %.  hoon-cache-size
    ~(resize (by-clock hoon-cache-key build-result) hoon-cache.state)
  ::  +cancel: cancel a build
  ::
  ::    When called on a live build, removes all tracking related to the live
  ::    build, and no more %made moves will be sent for that build.
  ::
  ::    When called on a once build, removes all tracking related to the once
  ::    build, and that build will never be completed or have a %made sent.
  ::
  ::    When called on a build that isn't registered in .state, such as a
  ::    completed once build, or a build that has already been canceled,
  ::    prints and no-ops.
  ::
  ++  cancel  ^+  [moves state]
    ::
    =<  finalize
    ::
    ?~  duct-status=(~(get by ducts.state) duct)
      ~&  [%no-build-for-duct duct]
      event-core
    ::  .duct is being canceled, so remove it unconditionally
    ::
    =.  ducts.state  (~(del by ducts.state) duct)
    ::  if the duct was not live, cancel any in-progress builds
    ::
    ?:  ?=(%once -.live.u.duct-status)
      ::
      =/  root-build=build  [in-progress.live root-plan]:u.duct-status
      ::
      =.  event-core  (cancel-scrys root-build)
      =.  state  (remove-anchor-from-root root-build [%duct duct])
      event-core
    ::  if the duct was live and has an unfinished build, cancel it
    ::
    =?  event-core  ?=(^ in-progress.live.u.duct-status)
      ::
      =/  root-build=build  [u.in-progress.live root-plan]:u.duct-status
      ::
      =.  event-core  (cancel-scrys root-build)
      =.  state  (remove-anchor-from-root root-build [%duct duct])
      event-core
    ::  if there is no completed build for the live duct, we're done
    ::
    ?~  last-sent=last-sent.live.u.duct-status
      event-core
    ::  there is a completed build for the live duct, so delete it
    ::
    =/  root-build=build  [date.u.last-sent root-plan.u.duct-status]
    ::
    =.  state  (remove-anchor-from-root root-build [%duct duct])
    ::
    ?~  subscription.u.last-sent
      event-core
    (cancel-clay-subscription u.subscription.u.last-sent)
  ::  +cancel-scrys: cancel all blocked %scry sub-builds of .root-builds
  ::
  ++  cancel-scrys
    |=  root-build=build
    ^+  event-core
    ::
    =/  blocked-sub-scrys  ~(tap in (collect-blocked-sub-scrys root-build))
    ::
    |-  ^+  event-core
    ?~  blocked-sub-scrys  event-core
    ::
    =.  event-core  (cancel-scry-request i.blocked-sub-scrys)
    ::
    $(blocked-sub-scrys t.blocked-sub-scrys)
  ::  |construction: arms for performing builds
  ::
  ::+|  construction
  ::
  ++  run-root-build
    |=  [=build live=?]
    ^+  event-core
    ::
    =/  [=product =progress]  (make build live)
    =.  hoon-cache.state  hoon-cache.progress
    ?~  product
      (on-build-blocked build live-resources.progress)
    ?:  live
      (on-live-build-done build u.product live-resources.progress)
    (on-once-build-done build u.product)
  ::
  ++  on-build-blocked
    |=  [=build =blocks]
    ^+  event-core
    ::
    %+  roll  (tap in blocks)
    |=  [=scry-request core=_event-core]
    (start-scry-request:core scry-request duct)
  ::
  ++  on-once-build-done
    |=  [=build result=(each cage tang)]
    ^+  event-core
    ::
    =.  ducts.state  (~(del by ducts.state) duct)
    (send-complete date.build result)
  ::
  ++  on-live-build-done
    |=  [=build result=(each cage tang) live-resouces=(set resource)]
    ^+  event-core
    ::  group .live-resources by $disc
    ::
    =.  resources-by-disc=(jug disc resource)
      %+  roll  ~(tap in live-resources)
      |=  [=resource acc=(jug disc resource)]
      (~(put ju acc) disc.rail.resource resource)
    ::  if :build depends on multiple discs, send an %incomplete and cancel
    ::
    ?:  (lth 1 ~(wyt by resources-by-disc))
      =.  ducts.state  (~(del by ducts.state) duct)
      =/  reason=tang  :~
        [%leaf "root build"]
        ::  TODO: [%leaf (build-to-tape build)]
        [%leaf "on duct:"]
        [%leaf "{<duct>}"]
        [%leaf "tried to subscribe to multiple discs:"]
        [%leaf "{<resources-by-disc>}"]
      ==
      (send-incomplete date.build reason)
    ::
    =.  event-core  (send-complete date.build result duct)
    ::
    =/  sub=subscription
      [date.build ?~(resources-by-disc ~ q.n.resources.by-disc)]
    ::
    =?  event-core  ?=(^ resources.sub)  (start-clay-subscription sub)
    ::
    =.  ducts.state
      %+  ~(jab by ducts.state)  duct
      |=  =duct-status
      ?>  live.duct-status
      duct-status(cur ~, pre `sub)
    ::
    event-core
  ::  +make: attempt to perform .build
  ::  TODO: reinstate jet hint
  ::  TODO: redo for ford-pinto
  ::
  ++  make
    =/  =progress  [blocks=~ live-resources=~ hoon-cache.state]
    |=  =build
    ^-  build-receipt
    ::  ~&  [%turbo-make (build-to-tape build)]
    ::  dispatch based on the kind of +plan in .build
    ::
    |^  =,  plan.build
        ::
        =.  build.out  build
        ::
        ?-    -.plan.build
        ::
            ^  (make-autocons [head tail])
        ::
            %$  (make-literal literal)
        ::
            %pin   (make-pin date plan)
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
            %list  (make-list plans)
            %mash  (make-mash disc mark first second)
            %mute  (make-mute subject mutations)
            %pact  (make-pact disc start diff)
            %path  (make-path disc prefix raw-path)
            %dais  (make-dais path-to-render query-string scaffold)
            %reef  (make-reef disc)
            %ride  (make-ride formula subject)
            %same  (make-same plan)
            %scry  (make-scry resource)
            %slim  (make-slim subject-type formula)
            %slit  (make-slit gate sample)
            %vale  (make-vale disc mark input)
            %volt  (make-volt disc mark input)
            %walk  (make-walk disc source target)
        ==
    ::  |plan-handlers:make: implementation of the plans
    ::
    ::    All of these produce a value of the same type as +make itself.
    ::
    ::  +|  plan-handlers
    ::
    ++  make-autocons
      ~%  %make-autocons  ..^^$  ~
      |=  [head=plan tail=plan]
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
        (return-blocks blocks)
      ?<  ?=(~ head-result)
      ?<  ?=(~ tail-result)
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
      |=  [date=@da =plan]
      ^-  build-receipt
      ::  pinned-sub: sub-build with the %pin date as formal date
      ::
      =/  pinned-sub=^build  [date plan]
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
      |=  [choices=(list plan) errors=(list tank)]
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
          ::  if there's a renderer called .renderer, use it on .path-to-render
          ::
          ::    Otherwise, fall back to running the contents of .path-to-render
          ::    through a mark that has the same name as .renderer.
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
        ::  link the renderer, passing through .path-to-render and .query-string
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
      ::     .errors contains any error messages from our previous attempt to
      ::     run a renderer, if we made one. This way if both the renderer and
      ::     mark fail, the requester will see the errors of both attempts.
      ::
      ++  try-mark
        |=  errors=(list tank)
        ^-  build-receipt
        ::  no renderer, try mark; retrieve directory listing of .path-to-render
        ::
        ::    There might be multiple files of different marks stored at
        ::    .path-to-render. Retrieve the directory listing for
        ::    .path-to-render, then check which of the path segments in
        ::    that directory are files (not just folders), then for each
        ::    file try to %cast its mark to the desired mark (:renderer).
        ::
        ::    Start by retrieving the directory listing, using .toplevel-build.
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
        ::  find the .sub-path-segments that could be files
        ::
        ::    Filter out path segments that aren't a +term,
        ::    since those aren't valid marks and therefore can't
        ::    be the last segment of a filepath in Clay.
        ::
        =/  sub-path-segments=(list @ta)
          (skim (turn ~(tap by dir.toplevel-arch) head) (sane %tas))
        ::
        =/  sub-plans=(list [sub-path=@ta =plan])
          %+  turn  sub-path-segments
          |=  sub=@ta
          :-  sub
          [%scry %c %y path-to-render(spur [sub spur.path-to-render])]
        ::
        =^  maybe-plan-results  out
          %-  perform-plans  :*
            ;:  weld
              "ford: %bake "  (trip renderer)  " on "
              (spud (rail-to-path path-to-render))  " contained failures:"
            ==
            sub-plans
            %fail-on-errors
            *@ta
          ==
        ?~  maybe-plan-results
          out
        ::  marks: list of the marks of the files at .path-to-render
        ::
        =/  marks=(list @tas)
          %+  murn  u.maybe-plan-results
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
          ^=  choices  ^-  (list plan)
          ::
          %+  turn  marks
          |=  mark=term
          ^-  plan
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
      |=  [gate=plan sample=plan]
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
      =/  slit-plan=plan  [%slit gate-vase sample-vase]
      =/  slit-build=^build  [date.build slit-plan]
      =^  slit-result  out  (depend-on slit-build)
      ?~  slit-result
        (return-blocks [date.build slit-plan]~)
      ::
      ?:  ?=([~ %error *] slit-result)
        %-  return-error
        :-  [%leaf "ford: %call failed type calculation"]
        message.u.slit-result
      ::
      ?>  ?=([~ %success %slit *] slit-result)
      ::
      =/  =hoon-cache-key  [%call gate-vase sample-vase]
      =^  cached-result  out  (access-cache hoon-cache-key)
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
      |=  [=disc mark=term input=plan]
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
      +$  action-result
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
        ::  slam the +mark-name:grab gate on the result of running .input
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
          ^-  plan
          :*  %mute
              ^-  plan
              [%core rail.u.starting-mark-path-result]
              ^=  mutations
              ^-  (list [wing plan])
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
      ::  convert file at .source-path to a +scaffold
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
      |=  [=disc start=plan end=plan]
      ^-  build-receipt
      ::  run both input plans as an autocons build
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
            subject=`plan`[%$ %noun vase.u.mark-result]
          ::
          ^=  mutations
          ^-  (list [wing plan])
          [[%& 6]~ [%$ start-cage]]~
        ::
        sample=`plan`[%$ end-cage]
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
      |=  [error=tank attempt=plan]
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
      =/  =hoon-cache-key  [%hood src-beam q.q.as-cage]
      =^  cached-result  out  (access-cache hoon-cache-key)
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
      |=  [disc=disc mark=term first=plan second=plan]
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
      |=  plans=(list plan)
      ^-  build-receipt
      ::
      =/  key-and-plans
        (turn plans |=(=plan [~ plan]))
      ::  depend on builds of each plan
      ::
      =^  maybe-plan-results  out
        (perform-plans "" key-and-plans %ignore-errors *~)
      ?~  maybe-plan-results
        out
      ::  return all builds
      ::
      =/  =build-result
        :+  %success  %list
        ::  the roll above implicitly flopped the results
        ::
        (flop (turn u.maybe-plan-results tail))
      (return-result build-result)
    ::
    ++  make-mash
      ~%  %make-mash  ..^^$  ~
      |=  $:  disc=disc
              mark=term
              first=[disc=disc mark=term =plan]
              second=[disc=disc mark=term =plan]
          ==
      ^-  build-receipt
      ::
      =/  initial-build=^build
        [date.build [plan.first plan.second] [%path disc %mar mark]]
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
      |=  [subject=plan mutations=(list [=wing =plan])]
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
      =^  maybe-plan-results  out
        %-  perform-plans  :*
          "ford: %mute contained failures:"
          mutations
          %fail-on-errors
          *wing
        ==
      ?~  maybe-plan-results
        out
      ::  all builds succeeded; retrieve vases from results
      ::
      =/  successes=(list [=wing =vase])
        %+  turn  u.maybe-plan-results
        |=  [=wing result=build-result]
        ^-  [^wing vase]
        ::
        ?>  ?=([%success *] result)
        ::
        [wing q:(result-to-cage result)]
      ::  create and run a $build to apply all mutations in order
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
        ::  subject: list of .subject-vase and mutations, as literal plan
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
        ^=  subject  ^-  plan
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
      |=  [disc=disc start=plan diff=plan]
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
      ::    First we cast .start-cage to the +grad mark, then we run
      ::    a new %pact build on the result of that, which will use the
      ::    +grad mark's +grad arm. Finally we cast the %pact result back to
      ::    .start-mark, since we're trying to produce a patched version of
      ::    the initial marked value (.start-cage).
      ::
      ?@  q.grad-vase
        ::  if +grad produced a term, make sure it's a valid mark
        ::
        =/  grad-mark=(unit term)  ((sand %tas) q.grad-vase)
        ?~  grad-mark
          %-  return-error  :_  ~  :-  %leaf
          "ford: %pact failed: %{<start-mark>} mark invalid +grad"
        ::  cast .start-cage to .grad-mark, %pact that, then cast back to start
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
      ::    as its sample and produces a mutant version of .start-cage
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
          ^-  plan
          :+  %ride
            [%tsld [%limb %pact] [%limb %grad]]
          ^-  plan
          :+  %mute
            ^-  plan
            [%$ %noun mark-vase]
          ^-  (list [wing plan])
          [[%& 6]~ [%$ start-cage]]~
        ^-  plan
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
      ::  possible-spurs: flopped paths to which .raw-path could resolve
      ::
      =/  possible-spurs=(list spur)  (turn (segments raw-path) flop)
      ::  rails-and-plans: scrys to check each path in .possible-paths
      ::
      =/  rails-and-plans=(list [=rail =plan])
        %+  turn  possible-spurs
        |=  possible-spur=spur
        ^-  [rail plan]
        ::  full-spur: wrap .possible-spur with .prefix and /hoon suffix
        ::
        =/  full-spur=spur  :(welp /hoon possible-spur /[prefix])
        ::
        :-  [disc full-spur]
        [%scry %c %x `rail`[disc full-spur]]
      ::  depend on builds of each plan
      ::
      =^  maybe-plan-results  out
        %-  perform-plans  :*
          ;:  weld
            "ford: %path resolution of "  (trip raw-path)  "at prefix "
            (trip prefix)  " contained failures:"
          ==
          rails-and-plans
          %filter-errors
          *rail
        ==
      ?~  maybe-plan-results
        out
      ::  matches: builds that completed with a successful result
      ::
      =/  matches  u.maybe-plan-results
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
      ::  tmi; cast .matches back to +list
      ::
      %+  roll  `_u.maybe-plan-results`matches
      |=  [[key=rail result=build-result] message=tang]
      ^-  tang
      ::  beam: reconstruct request from .kid's plan and date
      ::
      =/  =beam  [[ship.disc desk.disc [%da date.build]] spur.key]
      ::
      [[%leaf (spud (en-beam beam))] message]
    ::
    ++  make-dais
      ~%  %make-dais  ..^^$  ~
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
          ::  compile .combined-hoon against .subject
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
      +$  compose-result
        $%  [%subject subject=cage]
            [%block builds=(list ^build)]
            [%error message=tang]
        ==
      ::  +compose-cranes: runs each crane and composes the results
      ::
      ::    For each crane in .cranes, runs it and composes its result into a
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
      ::  +run-crane: runs an individual .crane against .subject
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
            ^-  plan
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
          ::  for each directory in .toplevel-arch, issue a sub-build
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
          ::  resolve all the .sub-builds
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
          ::  split .results into completed .mades and incomplete .blocks
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
          ::    .results is now a list of the .build-result of %cy on each path
          ::    in .toplevel-arch. What we want is to now filter this list so
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
            `[rail.resource.plan.build sub-path]
          ::  keep track of the original value so we can reset it
          ::
          =/  old-path-to-render  path-to-render
          ::  apply each of the filtered .sub-paths to the .sub-crane.
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
          ::  set .path-to-render back
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
          ::    This is pretty ugly, but Ford expects .path-to-render not to
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
          ::  apply each of the paths in .file-list to the .sub-crane
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
          ::  if .mark is %noun, don't perform mark translation; just return
          ::
          ::    If we were to verify the product type with %noun, this would
          ::    cast to *, which would overwrite .vase.u.plan-result's actual
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
      ::    If a build blocks, put its +tang in .error-message and stop.
      ::    All builds that block get put in .blocks. Results of
      ::    successful builds are produced in .results.
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
      ::  short-circuit to .pit if asked for current %home desk
      ::
      ::    This avoids needing to recompile the kernel if we're asked
      ::    for the kernel we're already running. Note that this fails
      ::    referential transparency if |autoload is turned off.
      ::
      ?:  ?&  |(=(disc [our %home]) =(disc [our %base]))
              ::  is .date.build the latest commit on the %home desk?
              ::
              ?|  =(now date.build)
                  ::
                  =/  =beam  [[our %home [%da date.build]] /hoon/hoon/sys]
                  ::
                  .=  (scry [%141 %noun] ~ %cw beam)
                  (scry [%141 %noun] ~ %cw beam(r [%da now]))
          ==  ==
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
            ::  hoon for `..is` to grab the .pit out of the arvo core
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
      |=  [formula=hoon =plan]
      ^-  build-receipt
      ::
      =^  result  out  (depend-on [date.build plan])
      ?~  result
        (return-blocks [date.build plan]~)
      ::
      =*  subject-vase  q:(result-to-cage u.result)
      =/  slim-plan=^plan  [%slim p.subject-vase formula]
      =^  slim-result  out  (depend-on [date.build slim-plan])
      ?~  slim-result
        (return-blocks [date.build slim-plan]~)
      ::
      ?:  ?=([~ %error *] slim-result)
        %-  return-error
        :*  [%leaf "ford: %ride failed to compute type:"]
            message.u.slim-result
        ==
      ::
      ?>  ?=([~ %success %slim *] slim-result)
      ::
      =/  =hoon-cache-key  [%ride formula subject-vase]
      =^  cached-result  out  (access-cache hoon-cache-key)
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
      |=  =plan
      ^-  build-receipt
      ::
      =^  result  out  (depend-on [date.build plan])
      ::
      ?~  result
        (return-blocks [date.build plan]~)
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
      ::    Look up .scry-request in .scry-results.per-event to avoid
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
      =/  =hoon-cache-key  [%slim subject-type formula]
      =^  cached-result  out  (access-cache hoon-cache-key)
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
      =/  =hoon-cache-key  [%slit p.gate p.sample]
      =^  cached-result  out  (access-cache hoon-cache-key)
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
              subject=`plan`[%core rail.u.path-result]
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
          +$  load-node  [type=?(%grab %grow) mark=term]
          ::  edge-jug: directed graph from .source mark to .target marks
          ::
          ::    .source can be converted to .target either by running
          ::    its own +grow arm, or by running the target's +grab arm.
          ::
          +$  edge-jug  (jug source=term [target=term arm=?(%grow %grab)])
          ::  mark-path: a path through the mark graph
          ::
          ::    +mark-path represents a series of mark translation
          ::    operations to be performed to 'walk' from one mark to another.
          ::
          ::    +mark-action is defined in Zuse. It represents a conversion
          ::    from a source mark to a target mark, and it specifies
          ::    whether it will use +grow or +grab.
          ::
          +$  mark-path  (list mark-action)
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
          ::    take us from .source to .term.
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
        =/  nodes-and-plans
          %+  turn  queued-nodes
          |=  =load-node
          ^-  [^load-node plan]
          :-  load-node
          [%path disc %mar mark.load-node]
        ::  get the path for each mark name
        ::
        ::    For %path builds, any ambiguous path is just filtered out.
        ::
        =^  maybe-path-results  out
          %-  perform-plans  :*
            ;:  weld
              "ford: %walk from "  (trip source)  " to "  (trip target)
              " contained failures:"
            ==
            nodes-and-plans
            %filter-errors
            *load-node
          ==
        ?~  maybe-path-results
          [[~ ~] out]
        ::
        =/  nodes-and-cores
          %+  turn  u.maybe-path-results
          |=  [=load-node =build-result]
          ^-  [^load-node plan]
          ::
          ?>  ?=([%success %path *] build-result)
          ::
          :-  load-node
          [%core rail.build-result]
        ::
        =^  maybe-core-results  out
          %-  perform-plans  :*
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
          ::  if .results has a %grow entry, remove it before adding our %grab
          =/  grow-entry=mark-action  [%grow source target.i.links]
          =?  results  (~(has in results) grow-entry)
            (~(del in results) grow-entry)
          ::
          =.  results  (~(put in results) [%grab source target.i.links])
          $(links t.links)
        ::
            %grow
          ::  if .results has a %grab entry, don't add a %grow entry
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
    ::  +perform-plans: helper function that performs a list of builds
    ::
    ::    We often need to run a list of builds. This helper method will
    ::    depend on all .builds, will return a $build-receipt of either the
    ::    blocks or the first error, or a list of all completed results.
    ::
    ::    This is a wet gate so individual callers can associate their own
    ::    key types with plans.
    ::
    ++  perform-plans
      |*  $:  failure=tape
              builds=(list [key=* =plan])
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
            =/  sub-build=^build  [date.build plan.i.builds]
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
        [[%leaf "ford: {<-.plan.build>} failed: "] message.u.result]
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
      |=  =hoon-cache-key
      ^-  [(unit build-result) _out]
      ::
      ?~  entry=(~(get by lookup.hoon-cache.state) hoon-cache-key)
        [~ out(cache-access `[hoon-cache-key new=%.y])]
      ::
      [`val.u.entry out(cache-access `[hoon-cache-key new=%.n])]
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
      ::  +access-build-record will mutate .results.state
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
    ::    Multiple plans handle +toon instances. This handles the %2 case
    ::    for a +toon and transforms it into a $build-receipt so we depend on
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
        =/  sub-plan=plan  [%pin date %scry u.resource]
        ::
        [%& `^build`[date sub-plan]]
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
        +:(depend-on [date.block plan.block])
      ::
      (return-blocks blocks)
    --
  ::  |utilities:per-event: helper arms
  ::
  ::+|  utilities
  ::
  ::  +got-duct-status: retrieve a +duct-status from the state
  ::
  ++  got-duct-status
    |=  duct=^duct
    ^-  duct-status
    ::
    ~|  [%missing-duct duct]
    (~(got by ducts.state) duct)
  ::
  ::  +intercepted-scry: augment real scry with local %scry build results
  ::
  ::    Try to deduplicate requests for possibly remote resources by looking up
  ::    the result in local state if the real scry has no synchronous
  ::    answer (it produced `~`).
  ::
  ::    TODO: convert to ford-pinto
  ::
  ++  intercepted-scry
    %-  sloy  ^-  slyd
    ~/  %intercepted-scry
    |=  [ref=* (unit (set monk)) =term =beam]
    ^-  (unit (unit (cask milt)))
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
    ::    Note: we can't freshen .build's .last-accessed date because
    ::    we can't mutate .state from this gate. %scry results might get
    ::    deleted during %wipe more quickly than they should because of this.
    ::
    =/  local-result  -:(access-build-record build)
    ?~  local-result
      ~
    ?:  ?=(%tombstone -.u.local-result)
      ~
    ::
    =/  local-cage=cage  (result-to-cage build-result.u.local-result)
    ::  if .local-result does not nest in .type, produce an error
    ::
    ?.  -:(nets:wa +.ref `type`p.q.local-cage)
      [~ ~]
    ::
    [~ ~ local-cage]
  ::  +send-incomplete: emit a move indicating we can't complete .build
  ::
  ++  send-incomplete
    |=  [date=@da message=tang]
    ^+  event-core
    =.  moves  :_(moves [duct %give %made date %incomplete message])
    event-core
  ::  +send-complete: send a move to respond with a completed build
  ::
  ++  send-complete
    |=  [date=@da result=(each cage tang)]
    ^+  event-core
    =.  moves  :_(moves [duct %give %made date %complete result])
    event-core
  ::  +collect-live-resources: produces all live resources from sub-scrys
  ::
  ++  collect-live-resources
    ~/  %collect-live-resources
    |=  =build
    ^-  (jug disc resource)
    ::
    ?:  ?=(%scry -.plan.build)
      =*  resource  resource.plan.build
      (my [(extract-disc resource) (sy [resource]~)]~)
    ::
    ?:  ?=(%pin -.plan.build)
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
  ::  +start-clay-subscription: listen for changes in the filesystem
  ::
  ++  start-clay-subscription
    ~/  %start-clay-subscription
    |=  =subscription
    ^+  event-core
    ::
    =/  already-subscribed=?
      (~(has by pending-subscriptions.state) subscription)
    ::
    =.  pending-subscriptions.state
      (put-request pending-subscriptions.state subscription duct)
    ::  don't send a duplicate move if we're already subscribed
    ::
    ?:  already-subscribed
      event-core
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
      ::  if .request-contents is `~`, this code is incorrect
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
    event-core
  ::  +cancel-clay-subscription: remove a subscription on .duct
  ::
  ++  cancel-clay-subscription
    ~/  %cancel-clay-subscription
    |=  =subscription
    ^+  event-core
    ::
    =^  originator  pending-subscriptions.state
      (del-request pending-subscriptions.state subscription duct)
    ::  if there are still other ducts on this subscription, don't send a move
    ::
    ?~  originator
      event-core
    ::
    =/  =wire  (clay-subscription-wire [date disc]:subscription)
    ::
    =/  =note
      =+  [their desk]=disc.subscription
      [%c %warp ship=their `riff:clay`[desk ~]]
    ::
    =.  moves  [`move`[u.originator [%pass wire note]] moves]
    ::
    event-core
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
    ^+  event-core
    ::  if we are the first block depending on this scry, send a move
    ::
    =/  already-started=?  (~(has by pending-scrys.state) scry-request)
    ::
    =.  pending-scrys.state
      (put-request pending-scrys.state scry-request duct)
    ::  don't send a duplicate move if we've already sent one
    ::
    ?:  already-started
      event-core
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
    event-core
  ::  +cancel-scry-request: cancel a pending asynchronous scry request
  ::
  ++  cancel-scry-request
    |=  =scry-request
    ^+  event-core
    ::
    =^  originator  pending-scrys.state
      (del-request pending-scrys.state scry-request duct)
    ::  if there are still other ducts on this subscription, don't send a move
    ::
    ?~  originator
      event-core
    ::
    =/  =wire  (scry-request-wire scry-request)
    ::
    =/  =note
      =+  [their desk]=[p q]:beam.scry-request
      [%c %warp ship=their `riff:clay`[desk ~]]
    ::
    =.  moves  [`move`[u.originator [%pass wire note]] moves]
    ::
    event-core
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
  |=  [=duct type=* wrapped-task=(hobo task:able)]
  ^-  [(list move) _ford-gate]
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
    ::  perform the build indicated by .task
    ::
    ::    We call .start-build on .this-event, which is the |per-event core
    ::    with the our event-args already bound. .start-build performs the
    ::    build and produces a pair of .moves and a mutant .state.
    ::    We update our .state and produce it along with .moves.
    ::
    =/  =build  [now plan.task]
    =^  moves  state.ax  (start-build:this-event build live.task)
    ::
    [moves ford-gate]
  ::
      ::  %keep: keep .count cache entries
      ::
      %keep
    ::
    =.  state.ax  (keep:this-event [hoon-cache build-cache]:task)
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
    =/  c-max  max-size.hoon-cache.state.ax
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
      ::  %wipe: wipe stored builds, clearing .percent-to-remove of the entries
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
        hoon-cache+&+hoon-cache.state.ax
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
  |=  [=wire =duct wrapped-sign=(hypo sign)]
  ^-  [(list move) _ford-gate]
  ::  unwrap .sign, ignoring unneeded +type in .p.wrapped-sign
  ::
  =/  =sign  q.wrapped-sign
  ::  .wire must at least contain a tag for dispatching
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
      ::  scry-result: parse a (unit cage) from .sign
      ::
      ::    If the result is `~`, the requested resource was not available.
      ::
      =/  scry-result=(unit cage)
        ?~  riot-sign
          ~
        `r.u.riot-sign
      ::  if spurious Clay response, .ducts will be empty, causing no-op
      ::
      =/  ducts=(list ^duct)
        (get-request-ducts pending-scrys.state.ax scry-request)
      ::
      =|  moves=(list move)
      |-  ^+  [moves state.ax]
      ?~  ducts  [moves state.ax]
      ::
      =*  event-args  [[our i.ducts now scry-gate] state.ax]
      ::  unblock the builds that had blocked on .resource
      ::
      =*  unblock  unblock:(per-event event-args)
      =^  duct-moves  state.ax  (unblock scry-request scry-result)
      ::
      $(ducts t.ducts, moves (weld moves duct-moves))
  --
::  +load: migrate old state to new state (called on vane reload)
::
::    Trim builds completely in case a change to our code invalidated an
::    old build result.
::
++  load
  |=  old=axle
  ^+  ford-gate
  ::
  =.  ax  old
  =.  ford-gate  +:(call ~[/ford-load-self] *type %trim 0)
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
