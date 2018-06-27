!:
::  pit: a +vase of the hoon+zuse kernel, which is a deeply nested core
::
|=  pit=vase
::
=,  ford-api
::  ford internal data structures
::
=>  =~
=,  ford-api  ::  TODO remove once in vane
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
+=  note
  $%  ::  %c: to clay
      ::
      $:  %c
      ::  %warp: internal (intra-ship) file request
      ::
      $%  $:  %warp
              ::  sock: pair of requesting ship, requestee ship
              ::
              =sock
              ::  riff: clay request contents
              ::
              riff=riff:clay
      ==  ==  ==
      ::  %f: to ford itself
      ::
      $:  %f
      ::  %build: perform a build
      ::
      $%  $:  %build
              ::  schematic: the schematic to build
              ::
              =schematic
      ==  ==  ==
      ::  %g: to gall
      ::
      $:  %g
      ::  %unto: full transmission
      ::
      ::    TODO: document more fully
      ::
      $%  $:  %deal
          ::  sock: pair of requesting ship, requestee ship
          ::
          =sock
          ::  cush: gall request contents
          ::
          cush=cush:gall
  ==  ==  ==  ==
::  +sign: private response from another vane to ford
::
+=  sign
  $%  ::  %c: from clay
      ::
      $:  %c
      $%  ::  %writ: internal (intra-ship) file response
          ::
          $:  %writ
              ::  riot: response contents
              ::
              riot=riot:clay
          ==
          ::  %wris: response to %mult; many changed files
          ::
          $:  %wris
              ::  case: case of the new files
              ::
              ::    %wris can only return dates to us.
              ::
              case=[%da p=@da]
              ::  care-paths: the +care:clay and +path of each file
              ::
              care-paths=(set [care=care:clay =path])
  ==  ==  ==  ==
--
::
=,  ford-api  ::  TODO remove once in vane
::
|%
::
::  +axle: overall ford state
::
+=  axle
  $:  ::  date: date at which ford's state was updated to this data structure
      ::
      date=%~2018.3.14
      ::  state-by-ship: storage for all the @p's this ford has been
      ::
      ::    Once the cc-release boot sequence lands, we can remove this
      ::    mapping, since an arvo will not change @p identities. until
      ::    then, we need to support a ship booting as a comet before
      ::    becoming its adult identity.
      ::
      state-by-ship=(map ship ford-state)
  ==
::  +ford-state: all state that ford maintains for a @p ship identity
::
+=  ford-state
  $:  ::  results: all stored build results
      ::
      ::    Ford generally stores the result for all the most recently
      ::    completed live builds, unless it's been asked to wipe its cache.
      ::
      results=(map build cache-line)
      ::  builds: registry of all attempted builds
      ::
      builds=build-registry
      ::  components: bidirectional linkages between sub-builds and clients
      ::
      ::    The first of the two jugs maps from a build to its sub-builds.
      ::    The second of the two jugs maps from a build to its client builds.
      ::
      components=build-dag
      ::  provisional-components: expected linkage we can't prove yet
      ::
      ::    During the +gather step, we promote builds, but our promotion
      ::    decisions may be wrong. We record our predictions here so we
      ::    can undo them.
      ::
      provisional-components=build-dag
      ::  rebuilds: bidirectional links between old and new identical builds
      ::
      ::    Old and new build must have the same schematic and result.
      ::    This can form a chain, like build<-->build<-->build.
      ::
      $=  rebuilds
      $:  ::  new: map from old build to new build
          ::
          new=(map build build)
          ::  old: map from new build to old build
          ::
          old=(map build build)
      ==
      ::  blocks: map from +resource to all builds waiting for its retrieval
      ::
      blocks=(jug scry-request build)
      ::  next-builds: builds to perform in the next iteration
      ::
      next-builds=(set build)
      ::  candidate-builds: builds which might go into next-builds
      ::
      candidate-builds=(list build)
      ::  blocked builds: mappings between blocked and blocking builds
      ::
      blocked-builds=build-dag
  ::
  ::  build request tracking
  ::
      ::  listeners: external requests for a build
      ::
      ::    Listeners get copied from the root build to every sub-build to
      ::    facilitate quickly checking which listeners are attached to a leaf
      ::    build.
      ::
      listeners=(jug build listener)
      ::  root-listeners: listeners attached only to root builds
      ::
      root-listeners=(jug build listener)
      ::  builds-by-listener: reverse lookup for :root-listeners
      ::
      ::    A duct can only be attached to one root build, and it is either
      ::    live or once. :builds-by-listener can be used to look up a +build
      ::    for a +duct, or to look up whether a duct is live or once.
      ::
      builds-by-listener=(map duct [=build live=?])
  ::
  ::  update tracking
  ::
      ::  resources-by-disc: live clay resources
      ::
      ::    Used for looking up which +resource's rely on a particular
      ::    +disc, so that we can send a new Clay subscription with all
      ::    the resources we care about within that disc.
      ::
      resources-by-disc=(jug disc resource)
      ::  latest-by-disc: latest formal date of a completed live build on disc
      ::
      ::    Updated each time we complete a build of a +resource,
      ::    if the build's formal date is later than the stored value.
      ::
      latest-by-disc=(map disc @da)
      ::  clay-subscriptions: ducts we'll use to cancel existing clay requests
      ::
      clay-subscriptions=(map disc duct)
      ::  resource-updates: all clay updates we need to know about
      ::
      ::    resource-updates stores all Clay changes at dates that
      ::    Ford needs to track because Ford is tracking attempted builds with
      ::    that formal date.
      ::
      resource-updates=(jug @da resource)
  ==
::  +build-registry: a registry of all attempted builds
::
+=  build-registry
  $:  ::  builds-by-schematic: all attempted builds, sorted by time
      ::
      ::    For each schematic we've attempted to build at any time,
      ::    list the formal dates of all build attempts, sorted newest first.
      ::
      by-schematic=(map schematic (list @da))
      ::  builds-by-date: all attempted builds, grouped by time
      ::
      by-date=(jug @da schematic)
  ==
::  +build-dag: a directed acyclic graph of builds
::
+=  build-dag
  $:  ::  sub-builds: jug from a build to its sub-builds
      ::
      sub-builds=(jug build build)
      ::  client-builds: jug from a build to its client builds
      ::
      client-builds=(jug build build)
  ==
::  +build: a referentially transparent request for a build.
::
::    Each unique +build will always produce the same +build-result
::    when run (if it completes). A live build consists of a sequence of
::    instances of +build with the same :plan and increasing :date.
::
+=  build
  $:  ::  date: the formal date of this build; unrelated to time of execution
      ::
      date=@da
      ::  schematic: the schematic that determines how to run this build
      ::
      =schematic
  ==
::  +cache-line: a record of our result of running a +build
::
::    Proof that a build has been run. Might include the result if Ford is
::    caching it. If Ford wiped the result from its cache, the result will
::    be replaced with a tombstone so Ford still knows the build has been
::    run before. Otherwise, contains the last accessed time of the result,
::    for use in cache reclamation.
::
+=  cache-line
  $%  ::  %value: the result of running a +build, and its last access time
      ::
      $:  %value
          ::  last-accessed: the last time this result was accessed
          ::
          ::    Updated every time this result is used in another build or
          ::    requested in a build request.
          ::
          last-accessed=@da
          ::  build-result: the referentially transparent result of a +build
          ::
          =build-result
      ==
      ::  %tombstone: marker that this build has been run and its result wiped
      ::
      [%tombstone ~]
  ==
::  +listener: either a :live :duct or a once :duct
::
+=  listener
  $:  ::  duct: where to send a response
      ::
      =duct
      ::  live: whether :duct had requested a live build
      ::
      live=?
  ==
::  +scry-request: parsed arguments to a scry operation
::
+=  scry-request
  $:  ::  vane: the vane from which to make the request
      ::
      ::    TODO: use +vane here
      ::
      vane=?(%c %g)
      ::  care: type of request
      ::
      care=care:clay
      ::  beam: request path
      ::
      =beam
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
              ::  scry-blocked: namespace request that :build blocked on
              ::
              scry-blocked=(unit scry-request)
          ==
      ==
      ::  sub-builds: subbuilds of :build
      ::
      ::    While running +make on :build, we need to keep track of any
      ::    sub-builds that we try to access so we can keep track of
      ::    component linkages and cache access times.
      ::
      sub-builds=(list build)
  ==
::  +vane: short names for vanes
::
::    TODO: move to zuse
::
+=  vane  ?(%a %b %c %d %e %f %g)
--
=,  format
|%
::  +tear: split a +term into segments delimited by `-`
::
++  tear
  |=  a=term
  ^-  (list term)
  ::  sym-no-heps: a parser for terms with no heps and a leading letter
  ::
  =/  sym-no-heps  (cook crip ;~(plug low (star ;~(pose low nud))))
  ::
  (fall (rush a (most hep sym-no-heps)) /[a])
::  +segments: TODO rename
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
++  build-to-tape
  |=  =build
  ^-  tape
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
      %core
    :(welp "[core " (spud (en-beam (rail-to-beam source-path.schematic))) "]")
  ::
      %hood
    :(welp "[hood " (spud (en-beam (rail-to-beam source-path.schematic))) "]")
  ::
      %plan
    :(welp "[plan " (spud (en-beam (rail-to-beam path-to-render.schematic))) "]")
  ::
      %scry
    (spud (en-beam (extract-beam resource.schematic ~)))
  ::
    ::    %slim
    ::  "slim {<subject-type.schematic>} {<formula.schematic>}"
  ==
::  +rail-to-beam
::
++  rail-to-beam
  |=  =rail
  ^-  beam
  [[ship.disc.rail desk.disc.rail [%ud 0]] spur.rail]
::  +unify-jugs: make a new jug, unifying sets for all keys
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
::  +scry-request-to-path: encode a +scry-request in a +wire
::
++  scry-request-to-path
  |=  =scry-request
  ^-  path
  =/  =term  (cat 3 [vane care]:scry-request)
  [term (en-beam beam.scry-request)]
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
::  +path-to-scry-request: parse :path to a :scry-request
::
++  path-to-scry-request
  |=  =path
  ^-  (unit scry-request)
  ::
  ?.  ?=([@ @ *] path)
    ~
  ::  parse :path's components into :vane, :care, and :rail
  ::
  =/  vane=(unit ?(%c %g))  ((soft ?(%c %g)) (end 3 1 i.path))
  ?~  vane
    ~
  =/  care=(unit care:clay)  ((soft care:clay) (rsh 3 1 i.path))
  ?~  care
    ~
  =/  rest=(unit ^path)  ((soft ^path) t.path)
  ?~  rest
    ~
  =/  beam  (de-beam u.rest)
  ?~  beam
    ~
  ::
  `[u.vane u.care u.beam]
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
::  +date-from-schematic: finds the latest pin date from this schematic tree.
::
++  date-from-schematic
  |=  =schematic
  ^-  @da
  =+  children=(get-sub-schematics schematic)
  =/  dates  (turn children date-from-schematic)
  =+  children-latest=(roll dates max)
  ?.  ?=(%pin -.schematic)
    children-latest
  (max date.schematic children-latest)
::  +is-schematic-live:
::
::    A schematic is live if it is not pinned.
::
++  is-schematic-live
  |=  =schematic
  ^-  ?
  !?=(%pin -.schematic)
::  +is-listener-live: helper function for loops
::
++  is-listener-live  |=(=listener live.listener)
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
    =/  dates  (fall (~(get by builds) schematic.build) ~)
    ?^  (find [date.build]~ dates)
      dates
    (sort [date.build dates] gte)
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
    =.  builds  %+  ~(put by builds)  schematic.build
      ::
      ~|  build+build
      =/  dates  (~(got by builds) schematic.build)
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
    =/  dates=(list @da)  (fall (~(get by builds) schematic.build) ~)
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
    =/  dates=(list @da)  (flop (fall (~(get by builds) schematic.build) ~))
    ::
    |-  ^-  (unit ^build)
    ?~  dates  ~
    ::
    ?:  (gth i.dates date.build)
      `[i.dates schematic.build]
    $(dates t.dates)
  --
::  +by-builds: door for manipulating :builds.state
::
++  by-builds
  |_  builds=build-registry
  ::  +put: add a +build
  ::
  ++  put
    |=  =build
    ^+  builds
    ::
    %_    builds
         by-date
      (~(put ju by-date.builds) date.build schematic.build)
    ::
        by-schematic
      (~(put by-schematic by-schematic.builds) build)
    ==
  ::  +del: remove a build
  ::
  ++  del
    |=  =build
    ^+  builds
    ::
    %_    builds
        by-date
      (~(del ju by-date.builds) date.build schematic.build)
    ::
        by-schematic
      (~(del by-schematic by-schematic.builds) build)
    ==
  --
::  +by-build-dag: door for manipulating a :build-dag
::
++  by-build-dag
  |_  dag=build-dag
  ::  +get-subs: produce a list of sub-builds.
  ::
  ++  get-subs
    |=  =build
    ^-  (list ^build)
    =-  ~(tap in (fall - ~))
    (~(get by sub-builds.dag) build)
  ::  +get-clients: produce a list of client-builds.
  ::
  ++  get-clients
    |=  =build
    ^-  (list ^build)
    =-  ~(tap in (fall - ~))
    (~(get by client-builds.dag) build)
  ::
  ::  +put: add a linkage between a :client and a :sub +build
  ::
  ++  put
    |=  [client=build sub=build]
    ^+  dag
    %_  dag
      sub-builds     (~(put ju sub-builds.dag) client sub)
      client-builds  (~(put ju client-builds.dag) sub client)
    ==
  ::  +del: delete a linkage between a :client and a :sub +build
  ::
  ++  del
    |=  [client=build sub=build]
    ^+  dag
    %_  dag
      sub-builds     (~(del ju sub-builds.dag) client sub)
      client-builds  (~(del ju client-builds.dag) sub client)
    ==
  ::  +del-build: remove all linkages containing :build
  ::
  ++  del-build
    |=  =build
    ^+  dag
    ::
    %_    dag
      ::  remove the mapping from :build to its sub-builds
      ::
          sub-builds
        (~(del by sub-builds.dag) build)
      ::  for each +build in :kids, remove :build from its clients
      ::
          client-builds
        %+  roll  ~(tap in (~(get ju sub-builds.dag) build))
        |=  [kid=^build clients=_client-builds.dag]
        (~(del ju clients) kid build)
      ==
  --
::  +parse-scaffold: produces a parser for a hoon file with +crane instances
::
::    Ford parses a superset of hoon which contains additional runes to
::    represent +crane s. This parses to a +scaffold.
::
::    src-beam: +beam of the source file we're parsing
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
      ::
        ::  parses the zuse version, eg "/?  309"
        ;~  pose
          (ifix [;~(plug fas wut gap) gap] dem)
          (easy zuse)
        ==
      ::
        ::  pareses the structures, eg "/-  types"
        ;~  pose
          (ifix [;~(plug fas hep gap) gap] (most ;~(plug com gaw) cable))
          (easy ~)
        ==
      ::
        ::  parses the libraries, eg "/+  lib1, lib2"
        ;~  pose
          (ifix [;~(plug fas lus gap) gap] (most ;~(plug com gaw) cable))
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
      fas
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
      ;~  pfix  fas
        ;~  pose
          ::  `/~`  hoon literal
          (stag %fssg ;~(pfix sig hoon))
          ::  `/$`  process query string
          (stag %fsbc ;~(pfix buc hoon))
          ::  `/|`  first of many options that succeeds
          (stag %fsbr ;~(pfix bar parse-alts))
          ::  `/=`  wrap a face around a crane
          (stag %fsts ;~(pfix tis parse-face))
          ::  `/.`  null terminated list
          (stag %fsdt ;~(pfix dot parse-list))
          ::  `/,`  switch by path
          (stag %fscm ;~(pfix com parse-switch))
          ::  `/&`  pass through a series of mark
          (stag %fspm ;~(pfix pam parse-pipe))
          ::  `/_`  run a crane on each file in the current directory
          (stag %fscb ;~(pfix cab subcrane))
          ::  `/;`  passes date through a gate
          (stag %fssm ;~(pfix sem parse-gate))
          ::  `/:`  evaluate at path
          (stag %fscl ;~(pfix col parse-at-path))
          ::  `/^` cast
          (stag %fskt ;~(pfix ket parse-cast))
          ::  `/!mark/ evaluate as hoon, then pass through mark
          (stag %fszp ;~(pfix zap ;~(sfix sym fas)))
          ::  `/mark/` passes current path through :mark
          (stag %fszy ;~(sfix sym fas))
        ==
      ==
    ::  +parse-alts: parse a set of alternatives
    ::
    ++  parse-alts
      %+  wide-or-tall
        (ifix [pel per] (most ace subcrane))
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
      ;~(pfix gap fas ;~(plug static-path subcrane))
    ::  +parse-pipe: parses a pipe of mark conversions
    ::
    ++  parse-pipe
      %+  wide-or-tall
        ;~(plug (plus ;~(sfix sym pam)) subcrane)
      =+  (cook |=(a=term [a ~]) sym)
      ;~(pfix gap ;~(plug - subcrane))
    ::  +parse-gate: parses a gate applied to a crane
    ::
    ++  parse-gate
      %+  wide-or-tall
        ;~(plug ;~(sfix wide:hoon-parser sem) subcrane)
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
        ;~(plug ;~(sfix wide:hoon-parser ket) subcrane)
      ;~(pfix gap ;~(plug tall:hoon-parser subcrane))
    ::  +crane: parses a subcrane
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
        (ifix [sel ser] (stag %cltr (most ace wide:hoon-parser)))
      ;~(pfix gap tall:hoon-parser)
    --
  ::  +static-path: parses a path
  ::
  ++  static-path
    (sear plex (stag %clsg (more fas hasp))):hoon-parser
  ::  +late-bound-path: a path whose time varies
  ::
  ++  late-bound-path
    ;~  pfix  fas
      %+  cook  |=(a=truss a)
      =>  hoon-parser
      ;~  plug
        (stag ~ gash)
        ;~(pose (stag ~ ;~(pfix cen porc)) (easy ~))
      ==
    ==
  --
::  +per-event: per-event core
::
++  per-event
  ::  moves: the moves to be sent out at the end of this event, reversed
  ::
  =|  moves=(list move)
  ::  dirty-discs: discs whose resources have changed during this event
  ::
  =|  dirty-discs=(set disc)
  ::  scry-results: responses to scry's to handle in this event
  ::
  ::    If a value is `~`, the requested resource is not available.
  ::    Otherwise, the value will contain a +cage.
  ::
  =|  scry-results=(map scry-request (unit cage))
  ::  the +per-event gate; each event will have a different sample
  ::
  ::    Not a `|_` because of the `=/`s at the beginning.
  ::    Produces a core containing four public arms:
  ::    +start-build, +rebuild, +unblock, and +cancel.
  ::
  |=  [[our=@p =duct now=@da scry=sley] state=ford-state]
  ::  original-clay-subscriptions: outstanding clay subscriptions at event start
  ::
  =/  original-clay-subscriptions  clay-subscriptions.state
  ::  original-resources-by-disc: :resources-by-disc.state at event start
  ::
  =/  original-resources-by-disc  resources-by-disc.state
  ::
  |%
  ::  |entry-points: externally fired arms
  ::
  ::+|  entry-points
  ::
  ::  +start-build: perform a fresh +build, either live or once
  ::
  ++  start-build
    |=  =schematic
    ^-  [(list move) ford-state]
    ::
    =<  finalize
    ::
    |^  =+  live=(is-schematic-live schematic)
        ?:  live
          start-live-build
        start-once-build
    ::
    ++  start-live-build
      ^+  this
      =/  =build  [now schematic]
      ::
      =.  state  (associate-build build duct %.y)
      ::
      (execute-loop (sy build ~))
    ::
    ++  start-once-build
      ^+  this
      =/  pin-date=@da  (date-from-schematic schematic)
      =/  =build  [pin-date schematic]
      ::
      =.  state  (associate-build build duct %.n)
      ::
      (execute-loop (sy build ~))
    ::  +associate-build: associate +listener with :build in :state
    ::
    ++  associate-build
      |=  [=build duct=^duct live=?]
      ^+  state
      ::
      %_    state
          listeners
        (~(put ju listeners.state) build [duct live])
      ::
          builds-by-listener
        (~(put by builds-by-listener.state) duct [build live])
      ::
          root-listeners
        (~(put ju root-listeners.state) build [duct live])
      ==
    ::
    --
  ::  +rebuild: rebuild any live builds based on +resource updates
  ::
  ++  rebuild
    |=  [ship=@p desk=@tas case=[%da p=@da] care-paths=(set [care=care:clay =path])]
    ^-  [(list move) ford-state]
    ::
    =<  finalize
    ::
    =/  date=@da  p.case
    ::
    =/  =disc  [ship desk]
    ::  delete the now-dead clay subscription
    ::
    =.  clay-subscriptions.state  (~(del by clay-subscriptions.state) disc)
    ::
    =/  resources=(list resource)
      %+  turn  ~(tap in care-paths)
      |=  [care=care:clay =path]  ^-  resource
      ::
      [%c care rail=[disc spur=(flop path)]]
    ::  store changed resources persistently in case rebuilds finish later
    ::
    =.  resource-updates.state
      %+  roll  resources
      |=  [=resource resource-updates=_resource-updates.state]
      ::
      (~(put ju resource-updates) date resource)
    ::  rebuild resource builds at the new date
    ::
    %-  execute-loop
    %-  sy
    %+  turn  resources
    |=(=resource `build`[date [%scry resource]])
  ::  +unblock: continue builds that had blocked on :resource
  ::
  ++  unblock
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
    ::  find all the :blocked-builds to continue
    ::
    =/  blocked-builds  (~(get ju blocks.state) scry-request)
    ::
    (execute-loop blocked-builds)
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
    =/  build-and-live  (~(get by builds-by-listener.state) duct)
    ::
    ?~  build-and-live
      ~&(no-build-for-duct+duct this)
    ::
    =+  [build live]=u.build-and-live
    ::  old-rebuilds and listeners don't interact well.
    ::
    ::    Have two ducts listening to the same build, causing a promotion.
    ::    The new build has an old build. Both ducts by :builds-by-listener
    ::    point to th new build. The new build has an old build and thus
    ::    never gets cleaned up.
    ::
    =.  state  (remove-listener-from-build [duct live] build)
    ::
    (cleanup build)
  ::  +remove-listener-from-build: recursively remove listener from (sub)builds
  ::
  ++  remove-listener-from-build
    |=  [=listener =build]
    ^+  state
    ::
    =?  state  (~(has ju root-listeners.state) build listener)
      %_    state
          builds-by-listener
        (~(del by builds-by-listener.state) duct.listener)
      ::
          root-listeners
        (~(del ju root-listeners.state) build listener)
      ==
    ::
    =/  original-build  build
    =/  builds=(list ^build)  ~[build]
    ::
    |-  ^+  state
    ?~  builds
      state
    ::
    =.  build  i.builds
    ::  are there any clients with this listener?
    ::
    =/  clients-with-listener=?
      %+  lien
        %~  tap  in
        =/  clients=(set ^build)
          (fall (~(get by client-builds.components.state) build) ~)
        %-  ~(uni in clients)
        (fall (~(get by client-builds.provisional-components.state) build) ~)
      ::
      |=  client=^build
      (~(has ju listeners.state) client listener)
    ::  when there are clients, don't remove the listener from this build
    ::
    ?:  clients-with-listener
      $(builds t.builds)
    ::
    =.  listeners.state
      (~(del ju listeners.state) build listener)
    ::
    =/  sub-builds  (~(get-subs by-build-dag components.state) build)
    ::
    =/  provisional-sub-builds
      (~(get-subs by-build-dag provisional-components.state) build)
    ::
    =/  new-builds=(list ^build)
      ?:  =(build original-build)  ~
      (drop (~(find-next by-schematic by-schematic.builds.state) build))
    ::
    $(builds :(welp t.builds sub-builds provisional-sub-builds new-builds))
  ::  |construction: arms for performing builds
  ::
  ::+|  construction
  ::
  ::  +execute-loop: +execute repeatedly until there's no more work to do
  ::
  ::    TODO: This implementation is for simplicity. In the longer term, we'd
  ::    like to just perform a single run through +execute and set a Behn timer
  ::    to wake us up immediately. This has the advantage that Ford stops hard
  ::    blocking the main Urbit event loop, letting other work be done.
  ::
  ++  execute-loop
    |=  builds=(set build)
    ^+  ..execute
    ::
    =.  ..execute  (execute builds)
    ::
    ?:  ?&  ?=(~ next-builds.state)
            ?=(~ candidate-builds.state)
        ==
      ..execute
    ::
    $(builds ~)
  ::  +execute: main recursive construction algorithm
  ::
  ::    Performs the three step build process: First, figure out which builds
  ::    we're going to run this loop through the ford algorithm. Second, run
  ::    the gathered builds, possibly in parallel. Third, apply the +build-receipt
  ::    algorithms to the ford state.
  ::
  ++  execute
    |=  builds=(set build)
    ^+  ..execute
    ::
    |^  ^+  ..execute
        ::
        =.  ..execute  (gather builds)
        ::
        =^  build-receipts  ..execute  run-builds
        ::
        (reduce build-receipts)
    ::  +gather: collect builds to be run in a batch
    ::
    ::    The +gather phase is the first of the three parts of +execute. In
    ::    +gather, we look through each item in :candidate-builds.state.  If we
    ::    should run the candidate build this cycle through the +execute loop,
    ::    we place it in :next-builds.state. +gather runs until it has no more
    ::    candidates.
    ::
    ++  gather
      |=  builds=(set build)
      ^+  ..execute
      ::  add builds that were triggered by incoming event to the candidate list
      ::
      =.  candidate-builds.state
        (weld candidate-builds.state ~(tap in builds))
      ::
      |^  ::
          ?~  candidate-builds.state
            ..execute
          ::
          =/  next  i.candidate-builds.state
          =>  .(candidate-builds.state t.candidate-builds.state)
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
        ::  if we already have a result for this build, don't rerun the build
        ::
        =^  current-result  results.state  (access-cache build)
        ?:  ?=([~ %value *] current-result)
          ..execute
        ::  place :build in :builds.state if it isn't already there
        ::
        =.  builds.state  (~(put by-builds builds.state) build)
        ::  old-build: most recent previous build with :schematic.build
        ::
        =/  old-build=(unit ^build)
          (~(find-previous by-schematic by-schematic.builds.state) build)
        ::  if no previous builds exist, we need to run :build
        ::
        ?~  old-build
          (add-build-to-next build)
        ::  copy :old-build's live listeners
        ::
        =.  state  (copy-old-live-listeners u.old-build build)
        ::  if any resources have changed, we need to rebuild :build
        ::
        ?:  (resources-changed build)
          (add-build-to-next build)
        ::  if we don't have :u.old-build's result cached, we need to run :build
        ::
        =^  old-cache-line  results.state  (access-cache u.old-build)
        ?~  old-cache-line
          (add-build-to-next build)
        ::  if :u.old-build's result has been wiped, we need to run :build
        ::
        ?:  ?=(%tombstone -.u.old-cache-line)
          (add-build-to-next build)
        ::  if any ancestors are pinned, we must rerun
        ::
        ::    We can't cleanly promote a once build to a live build because we
        ::    didn't register its resources in the live tracking system.
        ::
        ?:  (has-pinned-client u.old-build)
          (add-build-to-next build)
        ::  old-subs: sub-builds of :u.old-build
        ::
        =/  old-subs  (~(get-subs by-build-dag components.state) u.old-build)
        ::
        =/  new-subs  (turn old-subs |=(^build +<(date date.build)))
        ::  if all subs are in old.rebuilds.state, promote ourselves
        ::
        ?:  (levy new-subs ~(has by old.rebuilds.state))
          (on-all-subs-are-rebuilds u.old-build build new-subs)
        ::
        =.  state  (record-sub-builds-as-provisional build new-subs)
        ::  all new-subs have results, some are not rebuilds
        ::
        ::    We rerun :build because these non-rebuild results might be different,
        ::    possibly giving :build a different result.
        ::
        =/  uncached-new-subs  (skip new-subs is-build-cached)
        ?~  uncached-new-subs
          (add-build-to-next build)
        ::  otherwise, not all new subs have results and we shouldn't be run
        ::
        (on-not-all-subs-have-results build uncached-new-subs)
      ::  +add-build-to-next: run this build during the +make phase
      ::
      ++  add-build-to-next
        |=  =build
        ..execute(next-builds.state (~(put in next-builds.state) build))
      ::  +on-all-subs-are-rebuilds: promote when all sub-builds are rebuilds
      ::
      ::    When all subs are rebuilds, we promote :old and add builds
      ::    unblocked by this promotion to our :candidate-builds.
      ::
      ++  on-all-subs-are-rebuilds
        |=  [old=build new=build new-subs=(list build)]
        ^+  ..execute
        ::  link all :new-subs to :build in :components.state
        ::
        =.  state
          %+  roll  new-subs
          ::
          |=  [new-sub=build state=_state]
          ::
          state(components (~(put by-build-dag components.state) new new-sub))
        ::
        =^  wiped-rebuild  ..execute  (promote-build old date.new)
        =?    next-builds.state
            ?=(^ wiped-rebuild)
          (~(put in next-builds.state) u.wiped-rebuild)
        ::
        =^  unblocked-clients  state  (mark-as-done new)
        =.  candidate-builds.state
          (welp unblocked-clients candidate-builds.state)
        ::
        ..execute
      ::  +on-not-all-subs-have-results: this build can't be run at this time
      ::
      ::    When all our sub builds don't have results, we can't add :build to
      ::    :next-builds.state. Instead, put all the remaining uncached new
      ::    subs into :candidate-builds.state.
      ::
      ::    If all of our sub-builds finish immediately (i.e. promoted) when
      ::    they pass through +gather-internal, they will add :build back to
      ::    :candidate-builds.state and we will run again before +execute runs
      ::    +make.
      ::
      ++  on-not-all-subs-have-results
        |=  [=build uncached-new-subs=(list build)]
        ^+  ..execute
        ::
        =.  blocked-builds.state
          %+  roll  uncached-new-subs
          |=  [new-sub=^build blocked-builds=_blocked-builds.state]
          ::
          (~(put by-build-dag blocked-builds) build new-sub)
        ::
        %_    ..execute
            candidate-builds.state
          :(welp uncached-new-subs candidate-builds.state)
        ==
      ::  +copy-old-live-listeners: copies each live listener from :old to :new
      ::
      ++  copy-old-live-listeners
        |=  [old=build new=build]
        ^+  state
        ::
        =/  old-live-listeners=(list listener)
          =-  (skim - is-listener-live)
          =-  ~(tap in `(set listener)`(fall - ~))
          (~(get by listeners.state) old)
        ::
        %+  roll  old-live-listeners
        |=  [=listener state=_state]
        ::
        state(listeners (~(put ju listeners.state) new listener))
      ::  +record-sub-builds-as-provisional:
      ::
      ::    When we can't directly promote ourselves, we're going to rerun
      ::    our build. It's possible that the sub-builds are different, in
      ::    which case we'll need to clean up the current sub-build dependency.
      ::
      ++  record-sub-builds-as-provisional
        |=  [=build new-subs=(list build)]
        ^+  state
        ::
        %_    state
            provisional-components
          %+  roll  new-subs
          |=  [new-sub=^build provisional-components=_provisional-components.state]
          ::
          (~(put by-build-dag provisional-components) build new-sub)
        ==
      --
    ::  +promote-build: promote result of :build to newer :date
    ::
    ::    Also performs relevant accounting, and possibly sends %made moves.
    ::
    ++  promote-build
      |=  [old-build=build date=@da]
      ^-  [(unit build) _..execute]
      ::  grab the previous result, freshening the cache
      ::
      =^  old-cache-line  results.state  (access-cache old-build)
      ::  we can only promote a cached result, not missing or a %tombstone
      ::
      ?>  ?=([~ %value *] old-cache-line)
      ::  :new-build is :old-build at :date; promotion destination
      ::
      =/  new-build=build  old-build(date date)
      ::  copy the old result to :new-build
      ::
      =.  results.state  (~(put by results.state) new-build u.old-cache-line)
      ::  link :old-build and :new-build persistently
      ::
      ::    We store identical rebuilds persistently so that we know we don't
      ::    have to promote or rerun clients of the new rebuild.
      ::
      =.  rebuilds.state  (link-rebuilds old-build new-build)
      ::  if this is the newest %scry on :disc, update :latest-by-disc.state
      ::
      ::    :latest-by-disc.state is used to create Clay subscriptions. This
      ::    promoted build may now be the latest time for this :disc.
      ::
      =?    latest-by-disc.state
          ?&  ?=(%scry -.schematic.old-build)
              =/  disc  (extract-disc resource.schematic.old-build)
              ~|  [disc+disc latest-by-disc+latest-by-disc.state]
              (gth date (~(got by latest-by-disc.state) disc))
          ==
        =/  disc  (extract-disc resource.schematic.old-build)
        (~(put by latest-by-disc.state) disc date)
      ::  sanity check that +promote-build was called on a registered build
      ::
      ?>  (~(has ju by-date.builds.state) date.new-build schematic.new-build)
      ::  mirror linkages between :old-build and subs to :new-build and subs
      ::
      =.  components.state
        %+  roll  (~(get-subs by-build-dag components.state) old-build)
        ::
        |=  [old-sub=build components=_components.state]
        ::
        =/  new-sub=build  old-sub(date date)
        (~(put by-build-dag components) new-build new-sub)
      ::  promoted builds are no longer provisional
      ::
      =.  provisional-components.state
        %+  roll  (~(get-subs by-build-dag provisional-components.state) new-build)
        ::
        |=  [old-sub=build provisional-components=_provisional-components.state]
        ::
        =/  new-sub=build  old-sub(date date)
        (~(del by-build-dag provisional-components) new-build new-sub)
      ::  send %made moves for the previously established live listeners
      ::
      ::    We only want to send %made moves for live listeners which were
      ::    already on :new-build. We don't want to send %made moves for
      ::    listeners that we copy from :old-build because :new-build has the
      ::    same result as :old-build; therefore, we would be sending a
      ::    duplicate %made.
      ::
      =.  ..execute  (send-mades new-build (root-live-listeners new-build))
      ::  move live listeners from :old-build to :new-build
      ::
      ::    When we promote a build, we advance the live listeners from
      ::    :old-build to :new-build. Live listeners should be attached to the
      ::    most recent completed build for a given schematic.
      ::
      =.  state  (advance-live-listeners old-build new-build)
      ::  send %made moves for once listeners and delete them
      ::
      ::    If we have once listeners, we can send %made moves for them and
      ::    then no longer track them.
      ::
      =.  ..execute  (send-mades new-build (root-once-listeners new-build))
      =.  state  (delete-root-once-listeners new-build)
      ::  send %made moves for future builds
      ::
      ::    We may have future results queued, waiting on this build to send a
      ::    %made. Now that we've sent current %made moves, we can send future
      ::    ones, as we need to send these in chronological order by formal
      ::    date.
      ::
      =^  wiped-rebuild  ..execute  (send-future-mades new-build)
      ::  :old-build might no longer be tracked by anything
      ::
      =.  ..execute  (cleanup old-build)
      ::
      [wiped-rebuild ..execute]
    ::  +send-future-mades: send %made moves for future rebuilds
    ::
    ::    If a future rebuild has been wiped, then produce it along with
    ::    a modified `..execute` core.
    ::
    ++  send-future-mades
      |=  =build
      ^-  [(unit ^build) _..execute]
      ::
      =^  result  results.state  (access-cache build)
      ::
      =/  next  (~(find-next by-schematic by-schematic.builds.state) build)
      ?~  next
        ::  no future build
        ::
        [~ ..execute]
      ::
      =^  next-result  results.state  (access-cache u.next)
      ?~  next-result
        ::  unfinished future build
        ::
        [~ ..execute]
      ::  if :next's result hasn't been wiped
      ::
      ?:  ?&  ?=(%value -.u.next-result)
              !(has-pinned-client u.next)
          ==
        ::
        =.  state  (advance-live-listeners build u.next)
        =.  ..execute  (cleanup build)
        ::  if the result has changed, send %made moves for live listeners
        ::
        =?    ..execute
            ?&  ?=([~ %value *] result)
                !=(build-result.u.result build-result.u.next-result)
            ==
          (send-mades u.next (root-live-listeners u.next))
        ::
        $(build u.next)
      ::  if :next has been wiped, produce it
      ::
      [`u.next ..execute]
    ::  +run-builds: run the builds and produce +build-receipts
    ::
    ::    Runs the builds and cleans up the build lists afterwards.
    ::
    ::    TODO: When the vere interpreter has a parallel variant of +turn, use
    ::    that as each build might take a while and there are no data
    ::    dependencies between builds here.
    ::
    ++  run-builds
      ^-  [(list build-receipt) _..execute]
      ::
      =/  build-receipts=(list build-receipt)
        (turn ~(tap in next-builds.state) make)
      ::
      =.  next-builds.state  ~
      [build-receipts ..execute]
    ::  reduce: apply +build-receipts produce from the +make phase.
    ::
    ::    +gather produces builds to run make on. +make produces
    ::    +build-receipts. It is in +reduce where we take these +build-receipts and
    ::    apply them to ..execute.
    ::
    ++  reduce
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
        ::  update live resource tracking if the build is a live %scry
        ::
        =?    ..execute
            ?&  ?=(%scry -.schematic.build.made)
                (is-build-live build.made)
            ==
          ::
          (do-live-scry-accounting build.made resource.schematic.build.made)
        ::  process :sub-builds.made
        ::
        =.  state  (track-sub-builds build.made sub-builds.made)
        ::
        ?-    -.result.made
            %build-result
          (apply-build-result made)
        ::
            %blocks
          (apply-blocks build.made result.made sub-builds.made)
        ==
      ::  +do-live-scry-accounting: updates tracking for a live %scry build
      ::
      ++  do-live-scry-accounting
        |=  [=build =resource]
        ^+  ..execute
        =/  =disc  (extract-disc resource)
        ::
        %_    ..execute
        ::  link :disc to :resource
        ::
            resources-by-disc.state
          (~(put ju resources-by-disc.state) [disc resource])
        ::  mark :disc as dirty
        ::
            dirty-discs
          (~(put in dirty-discs) disc)
        ::  update :latest-by-disc.state if :date.build is later
        ::
            latest-by-disc.state
          =/  latest-date  (~(get by latest-by-disc.state) disc)
          ::
          ?:  ?&  ?=(^ latest-date)
                  (lte date.build u.latest-date)
              ==
            latest-by-disc.state
          ::
          (~(put by latest-by-disc.state) disc date.build)
        ==
      ::  +track-sub-builds:
      ::
      ::    For every sub-build discovered while running :build, we have to make
      ::    sure that we track that sub-build and that it is associated with the
      ::    right listeners.
      ::
      ++  track-sub-builds
        |=  [=build sub-builds=(list build)]
        ^+  state
        %+  roll  sub-builds
        |=  [sub-build=^build accumulator=_state]
        =.  state  accumulator
        ::  freshen cache for sub-build
        ::
        =.  results.state  +:(access-cache sub-build)
        ::
        %_    state
            builds
          (~(put by-builds builds.state) sub-build)
        ::
            components
          (~(put by-build-dag components.state) build sub-build)
        ::
            listeners
          ::
          =/  unified-listeners
            %-  ~(uni in (fall (~(get by listeners.state) sub-build) ~))
            (fall (~(get by listeners.state) build) ~)
          ::  don't put a key with an empty value
          ::
          ?~  unified-listeners
            listeners.state
          ::
          (~(put by listeners.state) sub-build unified-listeners)
        ==
      ::
      ::  +|  apply-build-result
      ::
      ::  +apply-build-result: apply a %build-result +build-receipt to ..execute
      ::
      ::    Our build produced an actual result.
      ::
      ++  apply-build-result
        |=  $:  =build
                $:  %build-result
                    =build-result
                ==
                sub-builds=(list build)
            ==
        ^+  ..execute
        ::
        ?>  (~(has ju by-date.builds.state) date.build schematic.build)
        ::  record the result returned from the build
        ::
        =.  results.state
          %+  ~(put by results.state)  build
          [%value last-accessed=now build-result]
        ::  queue clients we can run now that we have this build result
        ::
        =^  unblocked-clients  state  (mark-as-done build)
        =.  next-builds.state  (~(gas in next-builds.state) unblocked-clients)
        ::  :previous-build: last build of :schematic.build before :build if any
        ::
        =/  previous-build
          (~(find-previous by-schematic by-schematic.builds.state) build)
        ::  :previous-result: result of :previous-build if any
        ::
        =^  previous-result  results.state
          ?~  previous-build
            [~ results.state]
          (access-cache u.previous-build)
        ::  promote live listeners if we can
        ::
        ::    When we have a :previous-build with a :previous-result, and the
        ::    previous-build isn't a descendant of a %pin schematic, we need to
        ::    advance live listeners because this is now the most recent build.
        ::
        =?    state
            ?&  ?=(^ previous-build)
                ?=(^ previous-result)
                ::  TODO: double check on the tests for this. it seems wrong,
                ::  as a build could have an unpinned client and a pinned
                ::  client
                !(has-pinned-client u.previous-build)
            ==
          (advance-live-listeners u.previous-build build)
        ::  send results to once listeners and delete them
        ::
        ::    Once listeners are deleted as soon as their %made has been sent
        ::    because they don't maintain a subscription to the build.
        ::
        =.  ..execute  (send-mades build (root-once-listeners build))
        =.  state  (delete-root-once-listeners build)
        ::  does :build have the same result as :u.previous-build?
        ::
        ::    We consider a result the same if we have a :previous-build which
        ::    has a real %value, the current :build-result is the same, and
        ::    the :previous-build doesn't have a pinned client. We can't
        ::    promote pinned builds, so we always consider the result to be
        ::    different.
        ::
        =/  same-result=?
          ?&  ?=(^ previous-build)
              !(has-pinned-client u.previous-build)
              ?=([~ %value *] previous-result)
              =(build-result build-result.u.previous-result)
          ==
        ::  if we have the same result, link the rebuilds
        ::
        ::    We store identical rebuilds persistently so that we know we don't
        ::    have to promote or rerun clients of the new rebuild.
        ::
        =?    rebuilds.state
            same-result
          ::
          ?>  ?=(^ previous-build)
          (link-rebuilds u.previous-build build)
        ::  if the result has changed, inform all live listeners
        ::
        =?    ..execute
            !same-result
          (send-mades build (root-live-listeners build))
        ::  if the result has changed, rerun all old clients
        ::
        ::    When we have a previous result which isn't the same, we need to
        ::    rerun old clients at the current time. Since those clients have
        ::    sub-builds with new results, the results of clients might also be
        ::    different.
        ::
        =?    state
            &(!same-result ?=(^ previous-build))
          (enqueue-client-rebuilds build u.previous-build)
        ::  clean up provisional builds
        ::
        =.  state      (unlink-used-provisional-builds build sub-builds)
        =.  ..execute  (cleanup-orphaned-provisional-builds build)
        ::  if we had a previous build, clean it up
        ::
        =?    ..execute
            ?=(^ previous-build)
          (cleanup u.previous-build)
        ::  clean up our current build
        ::
        ::    If :build was a once build, now that we've sent its %made moves, we
        ::    can delete it.
        ::
        =.  ..execute  (cleanup build)
        ::  now that we've handled :build, check any future builds
        ::
        ::    We may have future results queued, waiting on this build to send
        ::    a %made. Now that we've sent current %made moves, we can send
        ::    future ones, as we need to send these in chronological order by
        ::    formal date.
        ::
        =^  wiped-rebuild  ..execute  (send-future-mades build)
        ?~  wiped-rebuild
          ..execute
        ::  if a future-build's result was wiped from the cache, rebuild it.
        ::
        =.  next-builds.state  (~(put in next-builds.state) u.wiped-rebuild)
        ::
        ..execute
      ::  +enqueue-client-rebuilds: rerun old clients, updated to current time
      ::
      ++  enqueue-client-rebuilds
        |=  [=build previous-build=build]
        ^+  state
        ::
        =/  clients-to-rebuild=(list ^build)
          %+  turn  (find-old-clients previous-build)
          ::
          |=  old-client=^build
          old-client(date date.build)
        ::
        %+  roll  clients-to-rebuild
        |=  [client=^build state=_state]
        ::
        %_    state
        ::
            next-builds
          (~(put in next-builds.state) client)
        ::
            provisional-components
          (~(put by-build-dag provisional-components.state) client build)
        ::
            builds
          (~(put by-builds builds.state) client)
        ==
      ::  +find-old-clients: find all previous clients of :build
      ::
      ::    Walks :old.rebuilds.state recursively to find all previous clients
      ::    of :build that produced the same result.
      ::
      ++  find-old-clients
        |=  =build
        ^-  (list ^build)
        ::
        %+  weld
          (~(get-clients by-build-dag components.state) build)
        ::
        =/  older-build  (~(get by old.rebuilds.state) build)
        ?~  older-build
          ~
        $(build u.older-build)
      ::  +unlink-used-provisional-builds:
      ::
      ::    The first step in provisional build cleanup is to remove
      ::    sub-builds which were actually depended on from the provisional
      ::    build set because they're no longer provisional.
      ::
      ++  unlink-used-provisional-builds
        |=  [=build sub-builds=(list build)]
        ^+  state
        ::
        %_    state
            provisional-components
          %+  roll  sub-builds
          |=  $:  sub-build=^build
                  provisional-components=_provisional-components.state
              ==
          ::
          (~(del by-build-dag provisional-components) build sub-build)
        ==
      ::  +cleanup-orphaned-provisional-builds: delete extraneous sub-builds
      ::
      ::    Any builds left in :provisional-components.state for our build
      ::    are orphaned builds. However, these builds may have other
      ::    listeners and we don't want to delete those.
      ::
      ++  cleanup-orphaned-provisional-builds
        |=  =build
        ^+  ..execute
        ::
        %+  roll
          (~(get-subs by-build-dag provisional-components.state) build)
        ::
        |=  [sub-build=^build accumulator=_..execute]
        =.  ..execute  accumulator
        ::  calculate the listeners to remove
        ::
        ::    Orphaned sub-builds have a set of listeners attached to them.
        ::    We want to find the listeners which shouldn't be there and
        ::    remove them.
        ::
        =/  provisional-client-listeners=(set listener)
          (fall (~(get by listeners.state) build) ~)
        ::  unify listener sets of all provisional client builds of :sub-build
        ::
        =/  all-other-client-listeners=(set listener)
          %+  roll
            %~  tap  in
            ::  omit :build; it's all *other* client listeners
            ::
            =-  (~(del in -) build)
            =-  (fall - ~)
            (~(get by client-builds.provisional-components.state) sub-build)
          |=  [build=^build listeners=(set listener)]
          ::
          %-  ~(uni in listeners)
          (fall (~(get by listeners.state) build) ~)
        ::  remove the orphaned build from provisional builds
        ::
        =.  provisional-components.state
          (~(del by-build-dag provisional-components.state) build sub-build)
        ::  orphaned-listeners: the clients we actually have to remove
        ::
        ::    The clients that are actually orphaned are the ones which are
        ::    in :provisional-client-listeners, but not
        ::    :all-other-client-listeners.
        ::
        =/  orphaned-listeners
          (~(dif in provisional-client-listeners) all-other-client-listeners)
        ::  remove orphaned listeners from :sub-build
        ::
        ::    We need to do this after we've removed :sub-build from
        ::    :provisional-components.state because otherwise that provisional
        ::    client link will prevent the listener from being removed.
        ::
        =.  state
          %+  roll  ~(tap in orphaned-listeners)
          |=  [=listener accumulator=_state]
          =.  state  accumulator
          ::
          (remove-listener-from-build listener sub-build)
        ::
        (cleanup sub-build)
      ::
      ::  +|  apply-blocks
      ::
      ::  +apply-blocks: apply a %blocks +build-receipt to ..execute
      ::
      ::    :build blocked. Record information about what builds it blocked on
      ::    and try those blocked builds as candidates in the next pass.
      ::
      ++  apply-blocks
        |=  $:  =build
                $:  %blocks
                    blocks=(list build)
                    scry-blocked=(unit scry-request)
                ==
                sub-builds=(list build)
            ==
        ^+  ..execute
        ::  if we scryed, send clay a request for the path we blocked on reading
        ::
        =?    moves
            ?=(^ scry-blocked)
          ::  TODO: handle other vanes
          ::
          ?>  ?=(%c vane.u.scry-blocked)
          ::
          [(clay-request-for-scry-request date.build u.scry-blocked) moves]
        ::  register resource block in :blocks.state
        ::
        =?    blocks.state
            ?=(^ scry-blocked)
          (~(put ju blocks.state) u.scry-blocked build)
        ::  register blocks on sub-builds in :blocked-builds.state
        ::
        =.  state  (register-sub-build-blocks build blocks)
        ::
        ..execute
      ::  +clay-request-for-scry-request: new move to request blocked resource
      ::
      ++  clay-request-for-scry-request
        |=  [date=@da =scry-request]
        ^-  move
        ::
        =/  =wire
          (welp /(scot %p our)/scry-request (scry-request-to-path scry-request))
        ::
        =/  =note
          =/  =disc  [p q]:beam.scry-request
          :*  %c  %warp  sock=[our their=ship.disc]  desk.disc
              `[%sing care.scry-request case=[%da date] (flop s.beam.scry-request)]
          ==
        ::
        [duct [%pass wire note]]
      ::  +register-sub-build-blocks: book-keeping on blocked builds
      ::
      ::    When we receive a %blocks +build-receipt, we need to register that
      ::    :build is blocked on each item in :blocks, along with queuing
      ::    each block as a candidate build.
      ::
      ++  register-sub-build-blocks
        |=  [=build blocks=(list build)]
        ^+  state
        ::
        %+  roll  blocks
        |=  [block=^build state=_state]
        ::  we must run +apply-build-receipt on :build.made before :block
        ::
        ?<  (~(has by results.state) block)
        ::
        %_    state
            blocked-builds
          (~(put by-build-dag blocked-builds.state) build block)
        ::
            candidate-builds
          [block candidate-builds.state]
        ==
      --
    ::  +resources-changed: did resources change since :previous-build?
    ::
    ++  resources-changed
      |=  =build
      ^-  ?
      ?.  ?=(%scry -.schematic.build)
        |
      ::
      =/  =resource  resource.schematic.build
      ::
      ?.  ?=(%c -.resource)
        |
      ::
      =/  updates  (fall (~(get by resource-updates.state) date.build) ~)
      ::
      (~(has in updates) resource)
    ::  +link-rebuilds: link old and new same build in :rebuilds.state
    ::
    ++  link-rebuilds
      |=  [old-build=build new-build=build]
      ^+  rebuilds.state
      ::
      %_  rebuilds.state
        old  (~(put by old.rebuilds.state) new-build old-build)
        new  (~(put by new.rebuilds.state) old-build new-build)
      ==
    ::  +delete-root-once-listeners: remove once listeners on :build from :state
    ::
    ++  delete-root-once-listeners
      |=  =build
      ^+  state
      ::
      %+  roll  (root-once-listeners build)
      |=  [=listener accumulator=_state]
      =.  state  accumulator
      (remove-listener-from-build listener build)
    --
  ::  +make: attempt to perform :build, non-recursively
  ::
  ::    Registers component linkages between :build and its sub-builds.
  ::    Attempts to perform +scry if necessary. Does not directly enqueue
  ::    any moves.
  ::
  ++  make
    |=  =build
    ^-  build-receipt
    ::  accessed-builds: builds accessed/depended on during this run.
    ::
    =|  accessed-builds=(list ^build)
    ::  ~&  [%turbo-make (build-to-tape build)]
    ::  dispatch based on the kind of +schematic in :build
    ::
    ::
    |^  =,  schematic.build
        ::
        ?-    -.schematic.build
        ::
            ^  (make-autocons [head tail])
        ::
            %$  (make-literal literal)
        ::
            %pin   (make-pin date schematic)
            %alts  (make-alts choices)
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
      |=  [head=schematic tail=schematic]
      ^-  build-receipt
      ::
      =/  head-build=^build  [date.build head]
      =/  tail-build=^build  [date.build tail]
      =^  head-result  accessed-builds  (depend-on head-build)
      =^  tail-result  accessed-builds  (depend-on tail-build)
      ::
      =|  blocks=(list ^build)
      =?  blocks  ?=(~ head-result)  [head-build blocks]
      =?  blocks  ?=(~ tail-result)  [tail-build blocks]
      ::  if either build blocked, we're not done
      ::
      ?^  blocks
        ::
        [build [%blocks blocks ~] accessed-builds]
      ::
      ?<  ?=(~ head-result)
      ?<  ?=(~ tail-result)
      ::
      =-  [build [%build-result -] accessed-builds]
      `build-result`[%success u.head-result u.tail-result]
    ::
    ++  make-literal
      |=  =cage
      ^-  build-receipt
      [build [%build-result %success %$ cage] accessed-builds]
    ::
    ++  make-pin
      |=  [date=@da =schematic]
      ^-  build-receipt
      ::  pinned-sub: sub-build with the %pin date as formal date
      ::
      =/  pinned-sub=^build  [date schematic]
      ::
      =^  result  accessed-builds  (depend-on pinned-sub)
      ::
      ?~  result
        [build [%blocks ~[pinned-sub] ~] accessed-builds]
      ::
      ?:  ?=([%error *] u.result)
        [build [%build-result %error message.u.result] accessed-builds]
      ::
      [build [%build-result %success %pin date u.result] accessed-builds]
    ::
    ++  make-alts
      |=  choices=(list schematic)
      ^-  build-receipt
      ::
      ?~  choices
        (return-error [leaf+"%alts: all options failed"]~)
      ::
      =/  choice=^build  [date.build i.choices]
      ::
      =^  result  accessed-builds  (depend-on choice)
      ?~  result
        [build [%blocks ~[choice] ~] accessed-builds]
      ::
      ?:  ?=([%error *] u.result)
        $(choices t.choices)
      ::
      [build [%build-result %success %alts u.result] accessed-builds]
    ::
    ++  make-bake
      |=  [renderer=term query-string=coin path-to-render=rail]
      ^-  build-receipt
      ::  path-build: find the file path for the renderer source
      ::
      =/  path-build=^build
        [date.build [%path disc.path-to-render %ren renderer]]
      ::
      =^  path-result  accessed-builds  (depend-on path-build)
      ?~  path-result
        [build [%blocks [path-build]~ ~] accessed-builds]
      ::  if there's a renderer called :renderer, use it on :path-to-render
      ::
      ?:  ?=([~ %success %path *] path-result)
        ::  build a +scaffold from the renderer source
        ::
        =/  hood-build=^build  [date.build [%hood rail.u.path-result]]
        ::
        =^  hood-result  accessed-builds  (depend-on hood-build)
        ?~  hood-result
          [build [%blocks [hood-build]~ ~] accessed-builds]
        ::
        ?.  ?=([~ %success %hood *] hood-result)
          (wrap-error hood-result)
        ::  link the renderer, passing through :path-to-render and :query-string
        ::
        =/  plan-build=^build
          :-  date.build
          [%plan path-to-render query-string scaffold.u.hood-result]
        ::
        =^  plan-result  accessed-builds  (depend-on plan-build)
        ?~  plan-result
          [build [%blocks [plan-build]~ ~] accessed-builds]
        ::
        ?.  ?=([~ %success %plan *] plan-result)
          (wrap-error plan-result)
        ::
        =/  =build-result
          [%success %bake %noun vase.u.plan-result]
        ::
        [build [%build-result build-result] accessed-builds]
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
      =^  toplevel-result  accessed-builds  (depend-on toplevel-build)
      ?~  toplevel-result
        [build [%blocks [toplevel-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %scry *] toplevel-result)
        (wrap-error toplevel-result)
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
      =^  schematic-results  accessed-builds
        (perform-schematics sub-schematics %fail-on-errors *@ta)
      ?:  ?=([%| *] schematic-results)
        ::  block or error
        p.schematic-results
      ::  marks: list of the marks of the files at :path-to-render
      ::
      =/  marks=(list @tas)
        %+  murn  p.schematic-results
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
      =^  alts-result  accessed-builds  (depend-on alts-build)
      ?~  alts-result
        [build [%blocks [alts-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %alts *] alts-result)
        (wrap-error alts-result)
      ::
      =/  =build-result
        [%success %bake (result-to-cage u.alts-result)]
      ::
      [build [%build-result build-result] accessed-builds]
    ::
    ++  make-bunt
      |=  [=disc mark=term]
      ^-  build-receipt
      ::  resolve path of the mark definition file
      ::
      =/  path-build=^build  [date.build [%path disc %mar mark]]
      ::
      =^  path-result  accessed-builds  (depend-on path-build)
      ?~  path-result
        [build [%blocks [path-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %path *] path-result)
        (wrap-error path-result)
      ::  build the mark core from source
      ::
      =/  core-build=^build  [date.build [%core rail.u.path-result]]
      ::
      =^  core-result  accessed-builds  (depend-on core-build)
      ?~  core-result
        [build [%blocks [core-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %core *] core-result)
        (wrap-error core-result)
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
      [build [%build-result %success %bunt cage] accessed-builds]
    ::
    ++  make-call
      |=  [gate=schematic sample=schematic]
      ^-  build-receipt
      ::
      =/  gate-build=^build  [date.build gate]
      =^  gate-result    accessed-builds  (depend-on gate-build)
      ::
      =/  sample-build=^build  [date.build sample]
      =^  sample-result  accessed-builds  (depend-on sample-build)
      ::
      =|  blocks=(list ^build)
      =?  blocks  ?=(~ gate-result)    [[date.build gate] blocks]
      =?  blocks  ?=(~ sample-result)  [[date.build sample] blocks]
      ?^  blocks
        ::
        [build [%blocks blocks ~] accessed-builds]
      ::
      ?<  ?=(~ gate-result)
      ?<  ?=(~ sample-result)
      ::
      =/  gate-vase=vase    q:(result-to-cage u.gate-result)
      =/  sample-vase=vase  q:(result-to-cage u.sample-result)
      ::
      ::  run %slit to get the resulting type of calculating the gate
      ::
      =/  slit-schematic=schematic  [%slit gate-vase sample-vase]
      =/  slit-build=^build  [date.build slit-schematic]
      =^  slit-result  accessed-builds  (depend-on slit-build)
      ?~  slit-result
        [build [%blocks [date.build slit-schematic]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %slit *] slit-result)
        (wrap-error slit-result)
      ::
      ::  How much duplication is there going to be here between +call and
      ::  +ride? Right now, we're just !! on scrys, but for reals we want it to
      ::  do the same handling.
      ?>  &(?=(^ q.gate-vase) ?=(^ +.q.gate-vase))
      =/  val
        (mong [q.gate-vase q.sample-vase] intercepted-scry)
      ::
      ?-    -.val
          %0
        :*  build
            [%build-result %success %call [type.u.slit-result p.val]]
            accessed-builds
        ==
      ::
          %1
        =/  blocked-paths=(list path)  ((hard (list path)) p.val)
        (blocked-paths-to-receipt %call blocked-paths)
      ::
          %2
        (return-error [[%leaf "ford: %call failed:"] p.val])
      ==
    ::
    ++  make-cast
      |=  [=disc mark=term input=schematic]
      ^-  build-receipt
      ::  find the path of the destination mark source
      ::
      =/  final-mark-path-build=^build  [date.build [%path disc %mar mark]]
      ::
      =^  final-mark-path-result  accessed-builds
        (depend-on final-mark-path-build)
      ::
      ?~  final-mark-path-result
        [build [%blocks [final-mark-path-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %path *] final-mark-path-result)
        (wrap-error final-mark-path-result)
      ::
      =/  final-mark-path=rail  rail.u.final-mark-path-result
      ::  build the destination mark source into a +vase of the mark core
      ::
      =/  final-mark-build=^build  [date.build [%core final-mark-path]]
      ::
      =^  final-mark-result  accessed-builds  (depend-on final-mark-build)
      ?~  final-mark-result
        [build [%blocks [final-mark-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %core *] final-mark-result)
        (wrap-error final-mark-result)
      ::
      =/  final-mark=vase  vase.u.final-mark-result
      ::  run the :input schematic to obtain the mark and value of the input
      ::
      =/  input-build=^build  [date.build input]
      ::
      =^  input-result  accessed-builds  (depend-on input-build)
      ?~  input-result
        [build [%blocks [input-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success *] input-result)
        (wrap-error input-result)
      ::
      =/  input-result-cage=cage  (result-to-cage u.input-result)
      ::
      |^  ::  if :final-mark has no +grab arm, grow from the input mark
          ::
          ?.  (slob %grab p.final-mark)
            grow
          ::  find +grab within the destination mark core
          ::
          =/  grab-build=^build
            [date.build [%ride [%limb %grab] [%$ %noun final-mark]]]
          ::
          =^  grab-result  accessed-builds  (depend-on grab-build)
          ?~  grab-result
            [build [%blocks [grab-build]~ ~] accessed-builds]
          ::
          ?.  ?=([~ %success %ride *] grab-result)
            (wrap-error grab-result)
          ::  if the +grab core has no arm for the input mark, grow from input
          ::
          ?.  (slob p.input-result-cage p.vase.u.grab-result)
            grow
          ::  find an arm for the input's mark within the +grab core
          ::
          =/  grab-mark-build=^build
            :-  date.build
            [%ride [%limb p.input-result-cage] [%$ %noun vase.u.grab-result]]
          ::
          =^  grab-mark-result  accessed-builds  (depend-on grab-mark-build)
          ?~  grab-mark-result
            [build [%blocks [grab-mark-build]~ ~] accessed-builds]
          ::
          ?.  ?=([~ %success %ride *] grab-mark-result)
            (wrap-error grab-mark-result)
          ::  slam the +mark-name:grab gate on the result of running :input
          ::
          =/  call-build=^build
            :-  date.build
            [%call gate=[%$ %noun vase.u.grab-mark-result] sample=input]
          ::
          =^  call-result  accessed-builds  (depend-on call-build)
          ?~  call-result
            [build [%blocks [call-build]~ ~] accessed-builds]
          ::
          ?.  ?=([~ %success %call *] call-result)
            (wrap-error call-result)
          ::
          =/  =build-result
            [%success %cast [mark vase.u.call-result]]
          ::
          [build [%build-result build-result] accessed-builds]
      ::  +grow: grow from the input mark to the destination mark
      ::
      ++  grow
        ^-  build-receipt
        ::  we couldn't grab; try to +grow from the input mark
        ::
        =/  starting-mark-path-build=^build
          [date.build [%path disc %mar p.input-result-cage]]
        ::
        =^  starting-mark-path-result  accessed-builds
          (depend-on starting-mark-path-build)
        ?~  starting-mark-path-result
          [build [%blocks [starting-mark-path-build]~ ~] accessed-builds]
        ::
        ?.  ?=([~ %success %path *] starting-mark-path-result)
          (wrap-error starting-mark-path-result)
        ::  grow the value from the initial mark to the final mark
        ::
        ::  Replace the input mark's sample with the input's result,
        ::  then fire the mark-name:grow arm to produce a result.
        ::
        =/  grow-build=^build
          :-  date.build
          :+  %ride
            formula=`hoon`[%tsgl [%wing ~[mark]] [%wing ~[%grow]]]
          ^=  subject
          ^-  schematic
          :*  %mute
              ^-  schematic
              [%core rail.u.starting-mark-path-result]
              ^=  mutations
              ^-  (list [wing schematic])
              [[%& 6]~ [%$ input-result-cage]]~
          ==
        ::
        =^  grow-result  accessed-builds  (depend-on grow-build)
        ?~  grow-result
          [build [%blocks [grow-build]~ ~] accessed-builds]
        ::
        ?.  ?=([~ %success %ride *] grow-result)
          (wrap-error grow-result)
        ::  make sure the product nests in the sample of the destination mark
        ::
        =/  bunt-build=^build  [date.build [%bunt disc mark]]
        ::
        =^  bunt-result  accessed-builds  (depend-on bunt-build)
        ?~  bunt-result
          [build [%blocks [bunt-build]~ ~] accessed-builds]
        ::
        ?.  ?=([~ %success %bunt *] bunt-result)
          (wrap-error bunt-result)
        ::
        ?.  (~(nest ut p.q.cage.u.bunt-result) | p.vase.u.grow-result)
          (return-error [leaf+"ford: %cast failed: nest fail"]~)
        ::
        =/  =build-result
          [%success %cast mark vase.u.grow-result]
        ::
        [build [%build-result build-result] accessed-builds]
      --
    ::
    ++  make-core
      |=  source-path=rail
      ^-  build-receipt
      ::  convert file at :source-path to a +scaffold
      ::
      =/  hood-build=^build  [date.build [%hood source-path]]
      ::
      =^  hood-result  accessed-builds  (depend-on hood-build)
      ?~  hood-result
        [build [%blocks [hood-build]~ ~] accessed-builds]
      ::
      ?:  ?=(%error -.u.hood-result)
        (wrap-error hood-result)
      ::  build the +scaffold into a program
      ::
      ?>  ?=([%success %hood *] u.hood-result)
      ::
      =/  plan-build=^build
        [date.build [%plan source-path `coin`[%many ~] scaffold.u.hood-result]]
      ::
      =^  plan-result  accessed-builds  (depend-on plan-build)
      ?~  plan-result
        [build [%blocks [plan-build]~ ~] accessed-builds]
      ::
      ?:  ?=(%error -.u.plan-result)
        (wrap-error plan-result)
      ::
      ?>  ?=([%success %plan *] u.plan-result)
      [build [%build-result %success %core vase.u.plan-result] accessed-builds]
    ::
    ++  make-diff
      |=  [=disc start=schematic end=schematic]
      ^-  build-receipt
      ::  run both input schematics as an autocons build
      ::
      =/  sub-build=^build  [date.build [start end]]
      ::
      =^  sub-result  accessed-builds  (depend-on sub-build)
      ?~  sub-result
        [build [%blocks [sub-build]~ ~] accessed-builds]
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
        [build [%build-result build-result] accessed-builds]
      ::
      =/  mark-path-build=^build  [date.build [%path disc %mar p.start-cage]]
      ::
      =^  mark-path-result  accessed-builds  (depend-on mark-path-build)
      ?~  mark-path-result
        [build [%blocks [mark-path-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %path *] mark-path-result)
        (wrap-error mark-path-result)
      ::
      =/  mark-build=^build  [date.build [%core rail.u.mark-path-result]]
      ::
      =^  mark-result  accessed-builds  (depend-on mark-build)
      ?~  mark-result
        [build [%blocks [mark-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %core *] mark-result)
        (wrap-error mark-result)
      ::
      ?.  (slab %grad p.vase.u.mark-result)
        %-  return-error  :_  ~  :-  %leaf
        "ford: %diff failed: %{<p.start-cage>} mark has no +grad arm"
      ::
      =/  grad-build=^build
        [date.build [%ride [%limb %grad] [%$ %noun vase.u.mark-result]]]
      ::
      =^  grad-result  accessed-builds  (depend-on grad-build)
      ?~  grad-result
        [build [%blocks [grad-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %ride *] grad-result)
        (wrap-error grad-result)
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
        =^  diff-result  accessed-builds  (depend-on diff-build)
        ?~  diff-result
          [build [%blocks [diff-build]~ ~] accessed-builds]
        ::
        ?.  ?=([~ %success %diff *] diff-result)
          (wrap-error diff-result)
        ::
        =/  =build-result
          [%success %diff cage.u.diff-result]
        ::
        [build [%build-result build-result] accessed-builds]
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
            formula=`hoon`[%tsgl [%wing ~[%diff]] [%wing ~[%grad]]]
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
      =^  diff-result  accessed-builds  (depend-on diff-build)
      ?~  diff-result
        [build [%blocks [diff-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %call *] diff-result)
        (wrap-error diff-result)
      ::
      =/  form-build=^build
        [date.build [%ride [%limb %form] [%$ %noun vase.u.grad-result]]]
      ::
      =^  form-result  accessed-builds  (depend-on form-build)
      ?~  form-result
        [build [%blocks [form-build]~ ~] accessed-builds]
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
      [build [%build-result build-result] accessed-builds]
    ::
    ++  make-dude
      |=  [error=(trap tank) attempt=schematic]
      ^-  build-receipt
      ::
      =/  attempt-build=^build  [date.build attempt]
      =^  attempt-result  accessed-builds  (depend-on attempt-build)
      ?~  attempt-result
        ::
        [build [%blocks ~[[date.build attempt]] ~] accessed-builds]
      ::
      ?.  ?=([%error *] u.attempt-result)
        [build [%build-result %success %dude u.attempt-result] accessed-builds]
      ::
      (return-error [$:error message.u.attempt-result])
    ::
    ++  make-hood
      |=  source-path=rail
      ^-  build-receipt
      ::
      =/  scry-build=^build  [date.build [%scry [%c %x source-path]]]
      =^  scry-result  accessed-builds  (depend-on scry-build)
      ?~  scry-result
        ::
        [build [%blocks ~[scry-build] ~] accessed-builds]
      ::
      ?:  ?=([~ %error *] scry-result)
        (wrap-error scry-result)
      =+  as-cage=(result-to-cage u.scry-result)
      ::  hoon files must be atoms to parse
      ::
      ?.  ?=(@ q.q.as-cage)
        (return-error [%leaf "ford: %hood: file not an atom"]~)
      ::
      =*  src-beam  [[ship.disc desk.disc [%ud 0]] spur]:source-path
      =/  parsed
        ((full (parse-scaffold src-beam)) [1 1] (trip q.q.as-cage))
      ::
      ?~  q.parsed
        (return-error [%leaf "syntax error: {<p.p.parsed>} {<q.p.parsed>}"]~)
      ::
      [build [%build-result %success %hood p.u.q.parsed] accessed-builds]
    ::
    ++  make-join
      |=  [disc=^disc mark=term first=schematic second=schematic]
      ^-  build-receipt
      ::
      =/  initial-build=^build
        [date.build [first second] [%path disc %mar mark]]
      ::
      =^  initial-result  accessed-builds  (depend-on initial-build)
      ?~  initial-result
        [build [%blocks [initial-build]~ ~] accessed-builds]
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
      =^  mark-result  accessed-builds  (depend-on mark-build)
      ?~  mark-result
        [build [%blocks [mark-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %core *] mark-result)
        (wrap-error mark-result)
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
      =^  grad-result  accessed-builds  (depend-on grad-build)
      ?~  grad-result
        [build [%blocks [grad-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %ride *] grad-result)
        (wrap-error grad-result)
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
          "ford: %pact failed: %{<mark>} mark invalid +grad"
        ::
        =/  join-build=^build
          [date.build [%join disc mark [%$ first-cage] [%$ second-cage]]]
        ::
        =^  join-result  accessed-builds  (depend-on join-build)
        ?~  join-result
          [build [%blocks [join-build]~ ~] accessed-builds]
        ::
        ?.  ?=([~ %success %join *] join-result)
          (wrap-error join-result)
        ::
        [build [%build-result u.join-result] accessed-builds]
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
      =^  form-result  accessed-builds  (depend-on form-build)
      ?~  form-result
        [build [%blocks [form-build]~ ~] accessed-builds]
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
        [build [%build-result %success %join first-cage] accessed-builds]
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
      =^  diff-result  accessed-builds  (depend-on diff-build)
      ?~  diff-result
        [build [%blocks [diff-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %call *] diff-result)
        (wrap-error diff-result)
      ::  the result was a unit; if `~`, use %null mark; otherwise grab tail
      ::
      =/  =build-result
        :+  %success  %join
        ?@  q.vase.u.diff-result
          [%null vase.u.diff-result]
        [u.form-mark (slot 3 vase.u.diff-result)]
      ::
      [build [%build-result build-result] accessed-builds]
    ::
    ++  make-list
      |=  schematics=(list schematic)
      ^-  build-receipt
      ::
      =/  key-and-schematics
        (turn schematics |=(=schematic [~ schematic]))
      ::  depend on builds of each schematic
      ::
      =^  schematic-results  accessed-builds
        (perform-schematics key-and-schematics %ignore-errors *~)
      ?:  ?=([%| *] schematic-results)
        ::  block or error
        p.schematic-results
      ::  return all builds
      ::
      =/  =build-result
        :+  %success  %list
        ::  the roll above implicitly flopped the results
        ::
        (flop (turn p.schematic-results tail))
      [build [%build-result build-result] accessed-builds]
    ::
    ++  make-mash
      |=  $:  disc=^disc
              mark=term
              first=[disc=^disc mark=term =schematic]
              second=[disc=^disc mark=term =schematic]
          ==
      ^-  build-receipt
      ::
      =/  initial-build=^build
        [date.build [schematic.first schematic.second] [%path disc %mar mark]]
      ::
      =^  initial-result  accessed-builds  (depend-on initial-build)
      ?~  initial-result
        [build [%blocks [initial-build]~ ~] accessed-builds]
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
      =^  mark-result  accessed-builds  (depend-on mark-build)
      ?~  mark-result
        [build [%blocks [mark-build]~ ~] accessed-builds]
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
      =^  grad-result  accessed-builds  (depend-on grad-build)
      ?~  grad-result
        [build [%blocks [grad-build]~ ~] accessed-builds]
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
        =^  mash-result  accessed-builds  (depend-on mash-build)
        ?~  mash-result
          [build [%blocks [mash-build]~ ~] accessed-builds]
        ::
        ?.  ?=([~ %success %mash *] mash-result)
          (wrap-error mash-result)
        ::
        =/  =build-result
          [%success %mash cage.u.mash-result]
        ::
        [build [%build-result build-result] accessed-builds]
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
      =^  form-result  accessed-builds  (depend-on form-build)
      ?~  form-result
        [build [%blocks [form-build]~ ~] accessed-builds]
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
        [build [%build-result build-result] accessed-builds]
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
      =^  mash-result  accessed-builds  (depend-on mash-build)
      ?~  mash-result
        [build [%blocks [mash-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %call *] mash-result)
        (wrap-error mash-result)
      ::
      =/  =build-result
        [%success %mash [u.form-mark vase.u.mash-result]]
      ::
      [build [%build-result build-result] accessed-builds]
    ::
    ++  make-mute
      |=  [subject=schematic mutations=(list [=wing =schematic])]
      ^-  build-receipt
      ::  run the subject build to produce the noun to be mutated
      ::
      =/  subject-build=^build  [date.build subject]
      =^  subject-result  accessed-builds  (depend-on subject-build)
      ?~  subject-result
        [build [%blocks [subject-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success *] subject-result)
        (wrap-error subject-result)
      ::
      =/  subject-cage=cage  (result-to-cage u.subject-result)
      ::
      =/  subject-vase=vase  q.subject-cage
      ::
      =^  schematic-results  accessed-builds
        (perform-schematics mutations %fail-on-errors *wing)
      ?:  ?=([%| *] schematic-results)
        ::  block or error
        p.schematic-results
      ::  all builds succeeded; retrieve vases from results
      ::
      =/  successes=(list [=wing =vase])
        %+  turn  p.schematic-results
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
      =^  ride-result  accessed-builds  (depend-on ride-build)
      ?~  ride-result
        [build [%blocks [ride-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %ride *] ride-result)
        (wrap-error ride-result)
      ::
      =/  =build-result
        [%success %mute p.subject-cage vase.u.ride-result]
      ::
      [build [%build-result build-result] accessed-builds]
    ::
    ++  make-pact
      |=  [disc=^disc start=schematic diff=schematic]
      ^-  build-receipt
      ::  first, build the inputs
      ::
      =/  initial-build=^build  [date.build start diff]
      ::
      =^  initial-result  accessed-builds  (depend-on initial-build)
      ?~  initial-result
        [build [%blocks [initial-build]~ ~] accessed-builds]
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
      =^  mark-path-result  accessed-builds
        (depend-on mark-path-build)
      ::
      ?~  mark-path-result
        [build [%blocks [mark-path-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %path *] mark-path-result)
        (wrap-error mark-path-result)
      ::
      =/  mark-build=^build  [date.build [%core rail.u.mark-path-result]]
      ::
      =^  mark-result  accessed-builds  (depend-on mark-build)
      ?~  mark-result
        [build [%blocks [mark-build]~ ~] accessed-builds]
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
      =^  grad-result  accessed-builds  (depend-on grad-build)
      ?~  grad-result
        [build [%blocks [grad-build]~ ~] accessed-builds]
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
        =^  cast-result  accessed-builds  (depend-on cast-build)
        ?~  cast-result
          [build [%blocks [cast-build]~ ~] accessed-builds]
        ::
        ?.  ?=([~ %success %cast *] cast-result)
          (wrap-error cast-result)
        ::
        =/  =build-result
          [%success %pact cage.u.cast-result]
        ::
        [build [%build-result build-result] accessed-builds]
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
      =^  form-result  accessed-builds  (depend-on form-build)
      ?~  form-result
        [build [%blocks [form-build]~ ~] accessed-builds]
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
            [%tsgl [%limb %pact] [%limb %grad]]
          ^-  schematic
          :+  %mute
            ^-  schematic
            [%$ %noun mark-vase]
          ^-  (list [wing schematic])
          [[%& 6]~ [%$ start-cage]]~
        ^-  schematic
        [%$ diff-cage]
      ::
      =^  pact-result  accessed-builds  (depend-on pact-build)
      ?~  pact-result
        [build [%blocks [pact-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %call *] pact-result)
        (wrap-error pact-result)
      ::
      =/  =build-result
        [%success %pact start-mark vase.u.pact-result]
      ::
      [build [%build-result build-result] accessed-builds]
    ::
    ++  make-path
      |=  [disc=^disc prefix=@tas raw-path=@tas]
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
      =^  schematic-results  accessed-builds
        (perform-schematics rails-and-schematics %ignore-errors *rail)
      ?:  ?=([%| *] schematic-results)
        ::  block or error
        p.schematic-results
      ::  matches: builds that completed with a successful result
      ::
      =/  matches=_p.schematic-results
        ~!  p.schematic-results
        %+  skim  p.schematic-results
        |=  [* r=build-result]
        ::
        ?=([%success *] r)
      ::  if no matches, error out
      ::
      ?~  matches
        =/  =beam
          [[ship.disc desk.disc [%da date.build]] /hoon/[raw-path]/[prefix]]
        ::
        (return-error [%leaf "%path: no matches for {<(en-beam beam)>}"]~)
      ::  if exactly one path matches, succeed with the matching path
      ::
      ?:  ?=([* ~] matches)
        [build [%build-result %success %path key.i.matches] accessed-builds]
      ::  multiple paths matched; error out
      ::
      %-  return-error
      ::
      :-  [%leaf "multiple matches for %path: "]
      ::  tmi; cast :matches back to +list
      ::
      %+  roll  `_p.schematic-results`matches
      |=  [[key=rail result=build-result] message=tang]
      ^-  tang
      ::  beam: reconstruct request from :kid's schematic and date
      ::
      =/  =beam  [[ship.disc desk.disc [%da date.build]] spur.key]
      ::
      [[%leaf "{<(en-beam beam)>}"] message]
    ::
    ++  make-plan
      |=  [path-to-render=rail query-string=coin =scaffold]
      ^-  build-receipt
      ::  TODO: support query-string
      ::
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
            [build [%blocks blocks ~] accessed-builds]
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
            [build [%blocks blocks ~] accessed-builds]
          ::
          ?^  error-message
            (return-error error-message)
          ::  reef-build: %reef build to produce standard library
          ::
          =/  reef-build=^build  [date.build [%reef disc.path-to-render]]
          ::
          =^  reef-result  accessed-builds  (depend-on reef-build)
          ?~  reef-result
            [build [%blocks [reef-build]~ ~] accessed-builds]
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
            [build [%blocks builds.crane-result ~] accessed-builds]
          ::  combined-hoon: source hoons condensed into a single +hoon
          ::
          =/  combined-hoon=hoon  [%tssg sources.scaffold]
          ::  compile :combined-hoon against :subject
          ::
          =/  compile=^build
            [date.build [%ride combined-hoon [%$ subject.crane-result]]]
          ::
          =^  compiled  accessed-builds  (depend-on compile)
          ::  compilation blocked; produce block on sub-build
          ::
          ?~  compiled
            [build [%blocks ~[compile] ~] accessed-builds]
          ::  compilation failed; error out
          ::
          ?.  ?=([~ %success %ride *] compiled)
            (wrap-error compiled)
          ::  compilation succeeded: produce resulting +vase
          ::
          [build [%build-result %success %plan vase.u.compiled] accessed-builds]
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
          =^  ride-result  accessed-builds  (depend-on ride-build)
          ?~  ride-result
            [[%block [ride-build]~] ..run-crane]
          ?:  ?=([~ %error *] ride-result)
            [[%error [leaf+"/~ failed: " message.u.ride-result]] ..run-crane]
          ?>  ?=([~ %success %ride *] ride-result)
          [[%subject %noun vase.u.ride-result] ..run-crane]
        ::  +run-fsbc: runes the `/$` rune
        ::
        ++  run-fsbc
          |=  =hoon
          ^-  compose-cranes
          ::
          =/  query-compile-build=^build
            [date.build [%ride ((jock |) query-string) [%$ %noun !>(~)]]]
          =^  query-compile-result  accessed-builds  (depend-on query-compile-build)
          ?~  query-compile-result
            [[%block [query-compile-build]~] ..run-crane]
          ?:  ?=([~ %error *] query-compile-result)
            [[%error [leaf+"/; failed: " message.u.query-compile-result]] ..run-crane]
          ?>  ?=([~ %success %ride *] query-compile-result)
          ::  TODO: if we had a slop build type, everything could be crammed
          ::  into one sub-build.
          ::
          =/  =beam
            =,  path-to-render
            [[ship.disc desk.disc [%da date.build]] spur]
          =+  arguments=(slop !>(beam) vase.u.query-compile-result)
          ::
          =/  call-build=^build
            [date.build [%call [%ride hoon [%$ subject]] [%$ %noun arguments]]]
          =^  call-result  accessed-builds  (depend-on call-build)
          ?~  call-result
            [[%block [call-build]~] ..run-crane]
          ?:  ?=([~ %error *] call-result)
            [[%error [leaf+"/; failed: " message.u.call-result]] ..run-crane]
          ?>  ?=([~ %success %call *] call-result)
          ::
          [[%subject %noun vase.u.call-result] ..run-crane]
        ::  +run-fsbr: runes the `/|` rune
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
              [[%face [~ face] p.q.subject.child] q.q.subject.child]
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
              ::  wouldn't have %noun here. This is case where it might matter.
              ::
              [%$ subject.child]
            [%cast disc.source-rail.scaffold i.marks $(marks t.marks)]
          =^  cast-result  accessed-builds  (depend-on cast-build)
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
          =^  toplevel-result  accessed-builds  (depend-on toplevel-build)
          ?~  toplevel-result
            [[%block ~[toplevel-build]] ..run-crane]
          ::
          ?:  ?=([~ %error *] toplevel-result)
            [[%error [leaf+"/_ failed: " message.u.toplevel-result]] ..run-crane]
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
            [date.build [%scry [%c %y path-to-render(spur [sub spur.path-to-render])]]]
          ::  results: accumulator for results of sub-builds
          ::
          =|  results=(list [kid=^build sub-path=@ta results=(unit build-result)])
          ::  resolve all the :sub-builds
          ::
          ::    TODO: It feels like this running sub build and filtering
          ::    results could be generalized.
          ::
          =/  subs-results
            |-  ^+  [results accessed-builds]
            ?~  sub-builds  [results accessed-builds]
            ?>  ?=(^ sub-paths)
            ::
            =/  kid=^build  i.sub-builds
            =/  sub-path=@ta  i.sub-paths
            ::
            =^  result  accessed-builds  (depend-on kid)
            =.  results  [[kid sub-path result] results]
            ::
            $(sub-builds t.sub-builds, sub-paths t.sub-paths)
          ::  apply mutations from depending on sub-builds
          ::
          =:  results          -.subs-results
              accessed-builds  +.subs-results
          ==
          ::  split :results into completed :mades and incomplete :blocks
          ::
          =+  split-results=(skid results |=([* * r=(unit build-result)] ?=(^ r)))
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
                    accumulator=[(list [sub-path=@ta =compose-result]) _..run-crane]
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
          =^  call-result  accessed-builds  (depend-on call-build)
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
          =/  =beam
            =,  source-rail.scaffold
            [[ship.disc desk.disc [%ud 0]] spur]
          =/  hoon-parser  (vang & (en-beam beam))
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
          |=  [mold=hoon sub-crane=^crane]
          ^-  compose-cranes
          ::
          =^  child  ..run-crane  (run-crane subject sub-crane)
          ?.  ?=([%subject *] child)
            [child ..run-crane]
          ::
          =/  bunt-build=^build
            [date.build [%ride [%bunt mold] [%$ subject]]]
          =^  bunt-result  accessed-builds  (depend-on bunt-build)
          ?~  bunt-result
            [[%block [bunt-build]~] ..run-crane]
          ?:  ?=([~ %error *] bunt-result)
            [[%error [leaf+"/^ failed: " message.u.bunt-result]] ..run-crane]
          ?>  ?=([~ %success %ride *] bunt-result)
          ::
          ?.  (~(nest ut p.vase.u.bunt-result) | p.q.subject.child)
            [[%error [leaf+"/^ failed: nest-fail"]~] ..run-crane]
          [[%subject %noun [p.vase.u.bunt-result q.q.subject.child]] ..run-crane]
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
          =^  hood-result  accessed-builds  (depend-on hood-build)
          ?~  hood-result
            [[%block [hood-build]~] ..run-crane]
          ?:  ?=([~ %error *] hood-result)
            [[%error [leaf+"/! failed: " message.u.hood-result]] ..run-crane]
          ?>  ?=([~ %success %hood *] hood-result)
          ::
          =/  plan-build=^build
            :-  date.build
            [%plan path-to-render query-string scaffold.u.hood-result]
          =^  plan-result  accessed-builds  (depend-on plan-build)
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
          =^  vale-result  accessed-builds  (depend-on vale-build)
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
          =^  bake-result  accessed-builds  (depend-on bake-build)
          ?~  bake-result
            [[%block [bake-build]~] ..run-crane]
          ?:  ?=([~ %error *] bake-result)
            [[%error [leaf+"/mark/ failed: " message.u.bake-result]] ..run-crane]
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
        =^  result  accessed-builds  (depend-on i.builds)
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
          [[%face face.cable.i.imports p.i.core-vases] q.i.core-vases]
        ::
        $(core-vases t.core-vases, imports t.imports)
      --
    ::
    ++  make-reef
      |=  =disc
      ^-  build-receipt
      ::  short-circuit to :pit if asked for current %home desk
      ::
      ::    This avoids needing to recompile the kernel if we're asked
      ::    for the kernel we're already running. Note that this fails
      ::    referential transparency if |autoload is turned off.
      ::
      ?:  ?&  =(disc [our %home])
              ::  is :date.build the latest commit on the %home desk?
              ::
              ?|  =(now date.build)
                  ::
                  =/  =beam  [[our %home [%da date.build]] /hoon/hoon/sys]
                  ::
                  .=  (scry [%143 %noun] ~ %cw beam)
                  (scry [%143 %noun] ~ %cw beam(r [%da now]))
          ==  ==
        ::
        [build [%build-result %success %reef pit] accessed-builds]
      ::
      =/  hoon-scry
        [date.build [%scry %c %x [disc /hoon/hoon/sys]]]
      ::
      =^  hoon-scry-result  accessed-builds  (depend-on hoon-scry)
      ::
      =/  arvo-scry
        [date.build [%scry %c %x [disc /hoon/arvo/sys]]]
      ::
      =^  arvo-scry-result  accessed-builds  (depend-on arvo-scry)
      ::
      =/  zuse-scry
        [date.build [%scry %c %x [disc /hoon/zuse/sys]]]
      ::
      =^  zuse-scry-result  accessed-builds  (depend-on zuse-scry)
      ::
      =|  blocks=(list ^build)
      =?  blocks  ?=(~ hoon-scry-result)  [hoon-scry blocks]
      =?  blocks  ?=(~ arvo-scry-result)  [arvo-scry blocks]
      =?  blocks  ?=(~ zuse-scry-result)  [zuse-scry blocks]
      ::
      ?^  blocks
        [build [%blocks blocks ~] accessed-builds]
      ::
      ?.  ?=([~ %success %scry *] hoon-scry-result)
        (wrap-error hoon-scry-result)
      ::
      ?.  ?=([~ %success %scry *] arvo-scry-result)
        (wrap-error arvo-scry-result)
      ::
      ?.  ?=([~ %success %scry *] zuse-scry-result)
        (wrap-error zuse-scry-result)
      ::  omit case from path to prevent cache misses
      ::
      =/  hoon-path=path
        /(scot %p ship.disc)/(scot %tas desk.disc)/hoon/hoon/sys
      =/  hoon-hoon=hoon  (rain hoon-path ;;(@t q.q.cage.u.hoon-scry-result))
      ::
      =/  arvo-path=path
        /(scot %p ship.disc)/(scot %tas desk.disc)/hoon/arvo/sys
      =/  arvo-hoon=hoon  (rain arvo-path ;;(@t q.q.cage.u.arvo-scry-result))
      ::
      =/  zuse-path=path
        /(scot %p ship.disc)/(scot %tas desk.disc)/hoon/zuse/sys
      =/  zuse-hoon=hoon  (rain zuse-path ;;(@t q.q.cage.u.zuse-scry-result))
      ::
      =/  zuse-build=^build
        :*  date.build
            %ride  zuse-hoon
            %ride  arvo-hoon
            %ride  hoon-hoon
            [%$ %noun !>(~)]
        ==
      ::
      =^  zuse-build-result  accessed-builds  (depend-on zuse-build)
      ?~  zuse-build-result
        [build [%blocks [zuse-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %ride *] zuse-build-result)
        (wrap-error zuse-build-result)
      ::
      :+  build
        [%build-result %success %reef vase.u.zuse-build-result]
      accessed-builds
    ::
    ++  make-ride
      |=  [formula=hoon =schematic]
      ^-  build-receipt
      ::
      =^  result  accessed-builds  (depend-on [date.build schematic])
      ?~  result
        [build [%blocks [date.build schematic]~ ~] accessed-builds]
      ::
      =*  subject  u.result
      =*  subject-cage  (result-to-cage subject)
      =/  slim-schematic=^schematic  [%slim p.q.subject-cage formula]
      =^  slim-result  accessed-builds  (depend-on [date.build slim-schematic])
      ?~  slim-result
        [build [%blocks [date.build slim-schematic]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %slim *] slim-result)
        (wrap-error slim-result)
      ::
      =/  val
        (mock [q.q.subject-cage nock.u.slim-result] intercepted-scry)
      ::  val is a toon, which might be a list of blocks.
      ::
      ?-    -.val
      ::
          %0
        :*  build
            [%build-result %success %ride [type.u.slim-result p.val]]
            accessed-builds
        ==
      ::
          %1
        =/  blocked-paths=(list path)  ((hard (list path)) p.val)
        (blocked-paths-to-receipt %ride blocked-paths)
      ::
          %2
        (return-error [[%leaf "ford: %ride failed:"] p.val])
      ==
    ::
    ++  make-same
      |=  =schematic
      ^-  build-receipt
      ::
      =^  result  accessed-builds  (depend-on [date.build schematic])
      ::
      ?~  result
        [build [%blocks [date.build schematic]~ ~] accessed-builds]
      [build [%build-result %success %same u.result] accessed-builds]
    ::
    ++  make-scry
      ::  TODO: All accesses to :state which matter happens in this function;
      ::  those calculations need to be lifted out of +make into +execute.
      ::
      |=  =resource
      ^-  build-receipt
      ::  construct a full +beam to make the scry request
      ::
      =/  =beam  (extract-beam resource `date.build)
      ::
      =/  =scry-request  [vane.resource care.resource beam]
      ::  perform scry operation if we don't already know the result
      ::
      ::    Look up :scry-request in :scry-results.per-event to avoid
      ::    rerunning a previously blocked +scry.
      ::
      =/  scry-response
        ?:  (~(has by scry-results) scry-request)
          (~(get by scry-results) scry-request)
        (scry [%143 %noun] ~ `@tas`(cat 3 [vane care]:resource) beam)
      ::  scry blocked
      ::
      ?~  scry-response
        ::  :build blocked on :scry-request
        ::
        ::    Enqueue a request +move to fetch the blocked resource.
        ::    Link :block and :build in :blocks.state so we know
        ::    which build to rerun in a later event when we +unblock
        ::    on that +resource.
        ::
        =/  already-blocked=?  (~(has by blocks.state) scry-request)
        ::  store :scry-request in persistent state
        ::
        =.  blocks.state  (~(put ju blocks.state) scry-request build)
        ::
        ?:  already-blocked
          ::  this resource was already blocked, so don't duplicate move
          ::
          [build [%blocks ~ ~] accessed-builds]
        ::
        [build [%blocks ~ `scry-request] accessed-builds]
      ::  scry failed
      ::
      ?~  u.scry-response
        %-  return-error
        :~  leaf+"scry failed for"
            leaf+"%c{(trip care.resource)} {<(en-beam beam)>}"
        ==
      ::  scry succeeded
      ::
      [build [%build-result %success %scry u.u.scry-response] accessed-builds]
    ::
    ++  make-slim
      |=  [subject-type=type formula=hoon]
      ^-  build-receipt
      ::
      =/  compiled=(each (pair type nock) tang)
        (mule |.((~(mint ut subject-type) [%noun formula])))
      ::
      :*  build
          ?-  -.compiled
            %|  [%build-result %error [leaf+"%slim failed: " p.compiled]]
            %&  [%build-result %success %slim p.compiled]
          ==
          accessed-builds
      ==
    ::
    ++  make-slit
      |=  [gate=vase sample=vase]
      ^-  build-receipt
      ::
      =/  product=(each type tang)
        (mule |.((slit p.gate p.sample)))
      ::
      :*  build
          ?-  -.product
            %|  :*  %build-result   %error
                    :*  (~(dunk ut p.sample) %have)
                        (~(dunk ut (~(peek ut p.gate) %free 6)) %want)
                        leaf+"%slit failed: "
                        p.product
                    ==
                ==
            %&  [%build-result %success %slit p.product]
          ==
          accessed-builds
      ==
    ::
    ++  make-volt
      |=  [=disc mark=term input=*]
      ^-  build-receipt
      ::
      =/  bunt-build=^build  [date.build [%bunt disc mark]]
      ::
      =^  bunt-result  accessed-builds  (depend-on bunt-build)
      ?~  bunt-result
        [build [%blocks [bunt-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %bunt *] bunt-result)
        (wrap-error bunt-result)
      ::
      =/  =build-result
        [%success %volt [mark p.q.cage.u.bunt-result input]]
      ::
      [build [%build-result build-result] accessed-builds]
    ::
    ++  make-vale
      ::  TODO: better docs
      ::
      |=  [=disc mark=term input=*]
      ^-  build-receipt
      ::  don't validate for the %noun mark
      ::
      ?:  =(%noun mark)
        =/  =build-result  [%success %vale [%noun %noun input]]
        ::
        [build [%build-result build-result] accessed-builds]
      ::
      =/  path-build  [date.build [%path disc %mar mark]]
      ::
      =^  path-result  accessed-builds  (depend-on path-build)
      ?~  path-result
        [build [%blocks [path-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %path *] path-result)
        (wrap-error path-result)
      ::
      =/  bunt-build=^build  [date.build [%bunt disc mark]]
      ::
      =^  bunt-result  accessed-builds  (depend-on bunt-build)
      ?~  bunt-result
        [build [%blocks [bunt-build]~ ~] accessed-builds]
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
              formula=`hoon`[%tsgl [%wing ~[%noun]] [%wing ~[%grab]]]
              subject=`schematic`[%core rail.u.path-result]
          ==
        sample=[%$ %noun %noun input]
      ::
      =^  call-result  accessed-builds  (depend-on call-build)
      ?~  call-result
        [build [%blocks [call-build]~ ~] accessed-builds]
      ::
      ?.  ?=([~ %success %call *] call-result)
        (wrap-error call-result)
      ::
      =/  product=vase  vase.u.call-result
      ::  TODO: why do we check nesting here?
      ::
      ?>  (~(nest ut p.mark-sample) | p.product)
      ::  check mold idempotence; if different, nest fail
      ::
      ?:  =(q.product input)
        =/  =build-result
          [%success %vale [mark p.mark-sample q.product]]
        ::
        [build [%build-result build-result] accessed-builds]
      ::
      %-  return-error
      =/  =beam  [[ship.disc desk.disc %da date.build] spur.rail.u.path-result]
      [leaf+"ford: %vale failed: invalid input for mark: {<(en-beam beam)>}"]~
    ::
    ++  make-walk
      |=  [=disc source=term target=term]
      ^-  build-receipt
      ::
      |^  ^-  build-receipt
          ::  load all marks.
          ::
          ::    Any random mark can implement a +grab arm, so we must load all
          ::    the marks if we want the mark graph to be complete.
          ::
          =^  load-directory-result  accessed-builds
            (load-directory /mar "" "")
          ?:  ?=([%error *] load-directory-result)
            :*  build
                [%build-result %error message.load-directory-result]
                accessed-builds
            ==
          ?:  ?=([%block *] load-directory-result)
            :*  build
                [%blocks builds.load-directory-result ~]
                accessed-builds
            ==
          ::  build an edge graph
          ::
          ::    :load-directory-result is a raw set of mark names and compiled
          ::    cores. +build-edges reads that data into a consistent build
          ::    graph.
          ::
          =/  edges  (build-edges ~(tap in results.load-directory-result))
          ::  find a path through the graph
          ::
          ::    Make a list of individual mark translation actions which will
          ::    take us from :source to :term.
          ::
          =/  path  (find-path-through edges)
          ::  if there is no path between these marks, give a nice error message.
          ::
          ?~  path
            :*  build
                :*  %build-result  %error
                    [leaf+"ford: no mark path from {<source>} to {<target>}"]~
                ==
                accessed-builds
            ==
          ::
          :*  build
              [%build-result %success %walk path]
              accessed-builds
          ==
      ::  +load-result: either a complete map, some blocks, or an error
      ::
      +=  load-result
        $%  [%result results=(map tape vase)]
            [%block builds=(list ^build)]
            [%error message=tang]
        ==
      ::  +load-directory: recursively walks the directory tree, loading marks
      ::
      ::    This either returns a map of all the mark cores on the specified
      ::    :disc, a list of blocked builds or a fatal error.
      ::
      ::    We must load all the marks on :disc because any random mark may
      ::    have a +grab arm which we must consider when doing the mark graph
      ::    traversal.
      ::
      ++  load-directory
        |=  [=path prev-name=tape name=tape]
        ^-  [load-result _accessed-builds]
        ::  load the directory directly
        ::
        =/  dir-build=^build  [date.build [%scry %c %y [disc path]]]
        =^  dir-result  accessed-builds  (depend-on dir-build)
        ?~  dir-result
          [[%block [dir-build]~] accessed-builds]
        ::
        ?.  ?=([~ %success %scry *] dir-result)
          ::  when directory scrys fail, this is a hard error.
          ?>  ?=([~ %error *] dir-result)
          [[%error message.u.dir-result] accessed-builds]
        ::
        =/  dir-arch=arch  ;;(arch q.q.cage.u.dir-result)
        ::  try loading the file here
        ::
        =^  fil-result=load-result  accessed-builds
          ?~  fil.dir-arch
            ::  we have no file at this node. ignore it.
            ::
            [[%result ~] accessed-builds]
          ?.  =(name "hoon")
            ::  this is a non-hoon file in the mark directory. ignore it.
            ::
            [[%result ~] accessed-builds]
          ::  load the file and return it
          ::
          =/  core-build=^build  [date.build [%core [disc path]]]
          =^  core-result  accessed-builds  (depend-on core-build)
          ?~  core-result
            [[%block [core-build]~] accessed-builds]
          ::
          ?.  ?=([~ %success %core *] core-result)
            ::  it is not a hard error if there's a hoon file which failed to compile.
            ::
            [[%result ~] accessed-builds]
          ::
          [[%result (my [prev-name vase.u.core-result]~)] accessed-builds]
        ::
        =/  valid-marks=(list @ta)
          (skim (turn ~(tap by dir.dir-arch) head) (sane %tas))
        ::  block on recursing into each directory
        ::
        =^  sub-results  accessed-builds
          =|  results=(list load-result)
          |-  ^+  [results accessed-builds]
          ?~  valid-marks  [results accessed-builds]
          ::
          =*  current-name  i.valid-marks
          =/  new-prev-name
            ?~  prev-name
              name
            :(weld prev-name "-" name)
          =^  sub-result  accessed-builds
            (load-directory [current-name path] new-prev-name (trip current-name))
          ::
          =.  results  [sub-result results]
          ::
          $(valid-marks t.valid-marks)
        ::  our results are the file node and the directory results
        ::
        =.  sub-results  [fil-result sub-results]
        ::  return the first error, if exists
        ::
        =/  errors=(list tang)
          %+  murn  sub-results
          |=  result=load-result
          ^-  (unit tang)
          ?.  ?=([%error *] result)
            ~
          `message.result
        ?^  errors
          [[%error i.errors] accessed-builds]
        ::  return all blocks, if exists
        ::
        =/  blocks=(list ^build)
          %+  roll  sub-results
          |=  [result=load-result blocks=(list ^build)]
          ^-  (list ^build)
          ?.  ?=([%block *] result)
            blocks
          (welp builds.result blocks)
        ?^  blocks
          [[%block blocks] accessed-builds]
        ::  our list only has results. merge them.
        ::
        =/  merged=(map tape vase)
          %+  roll  sub-results
          |=  [result=load-result merged=(map tape vase)]
          ^-  (map tape vase)
          ?>  ?=([%result *] result)
          (~(uni by merged) results.result)
        [[%result merged] accessed-builds]
      ::  edge-jug: type of our graph representation
      ::
      +=  edge-jug  (jug source=term [target=term arm=?(%grow %grab)])
      ::  +build-edges: build an edge database out of raw mark vases
      ::
      ++  build-edges
        |=  marks=(list [name=tape =vase])
        ^-  edge-jug
        ::
        =|  edges=edge-jug
        |-
        ^+  edges
        ?~  marks  edges
        ::
        =/  mark-name  (crip name.i.marks)
        =/  mark-vase  vase.i.marks
        ::
        =?  edges  (slob %grab p.mark-vase)
          =/  grab-arms  (sloe p:(slap mark-vase [%limb %grab]))
          |-
          ^+  edges
          ?~  grab-arms  edges
          =.  edges  (~(put ju edges) i.grab-arms [mark-name %grab])
          $(grab-arms t.grab-arms)
        ::
        =?  edges  (slob %grow p.mark-vase)
          =/  grow-arms  (sloe p:(slap mark-vase [%limb %grow]))
          |-
          ^+  edges
          ?~  grow-arms  edges
          =.  edges  (~(put ju edges) mark-name [i.grow-arms %grow])
          $(grow-arms t.grow-arms)
        ::
        $(marks t.marks)
      ::  mark-path: a path through the mark graph
      ::
      +=  mark-path  (list mark-action)
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
      |*  $:  builds=(list [key=* =schematic])
              on-error=?(%fail-on-errors %ignore-errors)
              key-bunt=*
          ==
      ^-  $:  (each (list [key=_key-bunt result=build-result]) build-receipt)
              _accessed-builds
          ==
      ::
      |^  =^  results  accessed-builds
            =|  results=(list [_key-bunt ^build (unit build-result)])
            |-
            ^+  [results accessed-builds]
            ::
            ?~  builds
              [results accessed-builds]
            ::
            =/  sub-build=^build  [date.build schematic.i.builds]
            =^  result  accessed-builds  (depend-on sub-build)
            =.  results  [[key.i.builds sub-build result] results]
            ::
            $(builds t.builds)
          ?:  =(%fail-on-errors on-error)
            (check-errors results)
          (handle-rest results)
      ::
      ++  check-errors
        |=  results=(list [_key-bunt ^build (unit build-result)])
        ::
        =/  error=tang
          %-  zing  ^-  (list tang)
          %+  murn  results
          |=  [* * result=(unit build-result)]
          ^-  (unit tang)
          ?.  ?=([~ %error *] result)
            ~
          `message.u.result
        ::  only produce the first error, as is tradition
        ::
        ?^  error
          =.  error  [leaf+"ford: %mute failed: " error]
          [[%| [build [%build-result %error error] accessed-builds]] accessed-builds]
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
          [[%| [build [%blocks blocks ~] accessed-builds]] accessed-builds]
        ::
        :_  accessed-builds
        :-  %&
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
      [build [%build-result %error message] accessed-builds]
    ::  +return-error: returns a specific failure message
    ::
    ++  return-error
      |=  =tang
      ^-  build-receipt
      [build [%build-result %error tang] accessed-builds]
    ::
    ++  depend-on
      |=  kid=^build
      ^-  [(unit build-result) _accessed-builds]
      ::
      =.  accessed-builds  [kid accessed-builds]
      ::  +access-cache will mutate :results.state
      ::
      ::    It's okay to ignore this because the accessed-builds get gathered
      ::    and merged during the +reduce step.
      ::
      =/  maybe-cache-line  -:(access-cache kid)
      ?~  maybe-cache-line
        [~ accessed-builds]
      ::
      =*  cache-line  u.maybe-cache-line
      ?:  ?=(%tombstone -.cache-line)
        [~ accessed-builds]
      ::
      [`build-result.cache-line accessed-builds]
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
        =/  sub-schematic=^schematic  [%pin date %scry u.resource]
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
        [build [%build-result %error failed] accessed-builds]
      ::  no failures
      ::
      =/  blocks=(list ^build)
        %+  turn  blocks-or-failures
        |=  block=(each ^build tank)
        ?>  ?=(%& -.block)
        ::
        p.block
      ::
      =.  accessed-builds
        %+  roll  blocks
        |=  [block=^build accumulator=_accessed-builds]
        =.  accessed-builds  accumulator
        +:(depend-on [date.block schematic.block])
      ::
      ::  TODO: Here we are passing a single ~ for :scry-blocked. Should we
      ::  be passing one or multiple resource back instead? Maybe not? Are
      ::  we building blocking schematics, which they themselves will scry?
      ::
      [build [%blocks blocks ~] accessed-builds]
    --
  ::  |utilities:per-event: helper arms
  ::
  ::+|  utilities
  ::
  ++  this  .
  ::  +intercepted-scry: use local results as a scry facade
  ::
  ++  intercepted-scry
    %-  sloy  ^-  slyd
    |=  [ref=* (unit (set monk)) =term =beam]
    ^-  (unit (unit (cask)))
    ?>  ?=([@ *] ref)
    =/  hoon-version=@ud  -.ref
    ::
    ~|  hoon-version=hoon-version
    ?>  ?=(?(%143 %151) hoon-version)
    ::
    =/  vane=(unit ?(%c %g))  ((soft ?(%c %g)) (end 3 1 term))
    ?~  vane
      ~
    =/  care=(unit care:clay)  ((soft care:clay) (rsh 3 1 term))
    ?~  care
      ~
    ::
    =/  =resource
      [u.vane u.care rail=[[p.beam q.beam] s.beam]]
    ::  TODO: handle other kinds of +case
    ::
    =/  date=@da
      ~|  bad-case+r.beam
      ?>  ?=(%da -.r.beam)
      p.r.beam
    ::
    =/  =build  [date %scry resource]
    ::  if the actual scry produces a value, use that value; otherwise use local
    ::
    =/  scry-response  (scry +<.$)
    ::
    ?^  scry-response
      scry-response
    ::  look up the scry result from our permanent state
    ::
    ::    Note: we can't freshen this cache entry because we can't modify
    ::    the state in this gate.
    ::
    =/  local-result  (~(get by results.state) build)
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
    [~ ~ `(cask)`local-cage]
  ::  +mark-as-done: store :build as complete and produce any unblocked clients
  ::
  ::    We may not know about these unblocked clients, so we register them in
  ::    the state.
  ::
  ++  mark-as-done
    |=  =build
    ^-  [(list ^build) _state]
    ::
    =/  client-builds=(list ^build)
      (~(get-clients by-build-dag blocked-builds.state) build)
    ::
    =.  blocked-builds.state
      %+  roll  client-builds
      ::
      |=  [client=^build blocked-builds=_blocked-builds.state]
      ::
      (~(del by-build-dag blocked-builds) client build)
    ::
    :_  state
    ::
    %+  roll  client-builds
    ::
    |=  [client=^build next-builds=(list ^build)]
    ::
    ?:  (is-build-blocked client)
      next-builds
    [client next-builds]
  ::  +send-mades: send one %made move for :build per listener in :listeners
  ::
  ++  send-mades
    |=  [=build listeners=(list listener)]  ^+  this
    ::
    =^  result  results.state  (access-cache build)
    ::
    ?>  ?=([~ %value *] result)
    ::
    %_    this
        moves
      %+  roll  listeners
      |=  [=listener moves=_moves]
      ::
      :_  moves
      :*  duct.listener  %give
          %made  date.build  %complete  build-result.u.result
      ==
    ==
  ::  +unlink-sub-builds
  ::
  ++  unlink-sub-builds
    |=  =build
    ^+  ..execute
    ::
    =/  kids=(list ^build)
      %~  tap  in
      %-  ~(uni in (~(get ju sub-builds.components.state) build))
      (~(get ju sub-builds.provisional-components.state) build)
    ::
    =.  components.state
      (~(del-build by-build-dag components.state) build)
    ::
    =.  provisional-components.state
      (~(del-build by-build-dag provisional-components.state) build)
    ::
    %+  roll  kids
    |=  [kid=^build accumulator=_..execute]
    ::
    =.  ..execute  accumulator
    (cleanup kid)
  ::  +advance-live-listeners: move live listeners from :old to :new
  ::
  ++  advance-live-listeners
    |=  [old=build new=build]
    ^+  state
    ::
    =/  old-live-listeners=(list listener)
      =-  (skim - is-listener-live)
      =-  ~(tap in `(set listener)`(fall - ~))
      (~(get by listeners.state) old)
    ::
    =/  old-root-listeners
      ~(tap in (fall (~(get by root-listeners.state) old) ~))
    ::
    =.  state
      %+  roll  old-root-listeners
      |=  [=listener state=_state]
      ::
      ?.  (is-listener-live listener)
        state
      %_    state
      ::
          root-listeners
        =-  (~(put ju -) new listener)
        (~(del ju root-listeners.state) old listener)
      ::
          builds-by-listener
        (~(put by builds-by-listener.state) duct.listener [new &])
      ==
    ::
    %+  roll  old-live-listeners
    |=  [=listener accumulator=_state]
    =.  state  accumulator
    ::  if :listener ain't live, we wrote this wrong
    ::
    ?>  live.listener
    ::
    =.  listeners.state  (~(put ju listeners.state) new listener)
    ::
    (remove-listener-from-build listener old)
  ::  +root-live-listeners: live listeners for which :build is the root build
  ::
  ++  root-live-listeners
    |=  =build
    ^-  (list listener)
    ::
    (skim (root-listeners build) is-listener-live)
  ::  +root-once-listeners: once listeners for which :build is the root build
  ::
  ++  root-once-listeners
    |=  =build
    ^-  (list listener)
    ::
    (skip (root-listeners build) is-listener-live)
  ::  +root-listeners: listeners for which :build is the root build
  ::
  ++  root-listeners
    |=  =build
    ^-  (list listener)
    ::
    =-  ~(tap in `(set listener)`(fall - ~))
    (~(get by root-listeners.state) build)
  ::  +is-build-blocked: is :build blocked on either builds or a resource?
  ::
  ++  is-build-blocked
    |=  =build
    ^-  ?
    ::
    ?:  (~(has by sub-builds.blocked-builds.state) build)
      &
    ?.  ?=(%scry -.schematic.build)
      |
    (~(has by blocks.state) resource.schematic.build build)
  ::  +is-build-cached:
  ::
  ++  is-build-cached
    |=  =build
    ^-  ?
    ?=([~ %value *] (~(get by results.state) build))
  ::  +is-build-live: whether this is a live or a once build
  ::
  ++  is-build-live
    |=  =build
    ^-  ?
    ::
    ?:  ?=(%pin -.schematic.build)
      %.n
    ?:  (has-pinned-client build)
      %.n
    ::  check if :build has any live listeners
    ::
    =/  listeners  ~(tap in (fall (~(get by listeners.state) build) ~))
    ?~  listeners
      %.y
    (lien `(list listener)`listeners is-listener-live)
  ::  +has-pinned-client: %.y if any of our ancestors are a %pin
  ::
  ++  has-pinned-client
    |=  =build
    ^-  ?
    ::  iterate across all clients recursively, exiting early on %pin
    ::
    =/  clients  (~(get-clients by-build-dag components.state) build)
    |-
    ?~  clients
      %.n
    ?:  ?=(%pin -.schematic.i.clients)
      %.y
    %_    $
        clients
      %+  weld  t.clients
      (~(get-clients by-build-dag components.state) i.clients)
    ==
  ::  +access-cache: access the +cache-line for :build, updating :last-accessed
  ::
  ::    Usage:
  ::    ```
  ::    =^  maybe-cache-line  results.state  (access-cache build)
  ::    ```
  ::
  ++  access-cache
    |=  =build
    ^-  [(unit cache-line) _results.state]
    ::
    =/  maybe-original=(unit cache-line)  (~(get by results.state) build)
    ?~  maybe-original
      [~ results.state]
    ::
    =/  original=cache-line  u.maybe-original
    ::
    ?:  ?=(%tombstone -.original)
      [`original results.state]
    ::
    =/  mutant=cache-line  original(last-accessed now)
    ::
    [`mutant (~(put by results.state) build mutant)]
  ::  +finalize: convert per-event state to moves and persistent state
  ::
  ::    Converts :done-live-roots to %made +move's, performs +duct
  ::    accounting, and runs +cleanup on completed once builds and
  ::    stale live builds.
  ::
  ++  finalize
    ^-  [(list move) ford-state]
    ::  once we're done, +flop :moves to put them in chronological order
    ::
    =<  [(flop moves) state]
    ::
    =/  discs  ~(tap in dirty-discs)
    ::
    |-  ^+  this
    ?~  discs  this
    ::
    =*  disc  i.discs
    ::  resources: all resources on :disc
    ::
    =/  resources=(set resource)
      (fall (~(get by resources-by-disc.state) disc) ~)
    ::  subscription-duct: if any, the duct that previously made a subscription
    ::
    =/  subscription-duct
      (~(get by original-clay-subscriptions) disc)
    ::  if no resources on :disc, don't make a new clay subscription
    ::
    ?~  resources
      ::  no resources, no subscriptions, no action.
      ::
      ?~  subscription-duct
        $(discs t.discs)
      ::  cancel any clay subscriptions since we don't have any resources left
      ::
      =.  moves  :_  moves
        (clay-cancel-subscription-move u.subscription-duct disc)
      ::
      =.  clay-subscriptions.state  (~(del by clay-subscriptions.state) disc)
      ::
      =.  latest-by-disc.state  (~(del by latest-by-disc.state) disc)
      ::
      $(discs t.discs)
    ::  prevent thrashing; don't unsubscribe then immediately resubscribe
    ::
    ::    When we send a request to a foreign ship, that ship may have
    ::    started responding before we send a cancellation. In that case,
    ::    canceling and then resubscribing might cause the foreign ship
    ::    to send the response twice, which would be extra network traffic.
    ::
    ?:  ?&  (~(has by original-clay-subscriptions) disc)
        ::
            (~(has by clay-subscriptions.state) disc)
        ::
            .=  (~(get by original-resources-by-disc) disc)
            (~(get by resources-by-disc.state) disc)
        ==
      ::
      $(discs t.discs)
    ::  if we had a previous subscription on a different duct, send a cancel
    ::
    ::    Clay effectively has a (map duct subscription). We need to explicitly
    ::    cancel the old subscription only if the ducts differ, as otherwise
    ::    the new subscription will replace the old one.
    ::
    =?  moves  &(?=(^ subscription-duct) !=(duct u.subscription-duct))
      :_  moves
      (clay-cancel-subscription-move u.subscription-duct disc)
    ::  send a new clay request using the current duct
    ::
    =.  moves  :_  moves
      (clay-add-subscription-move disc resources)
    ::
    =.  clay-subscriptions.state  (~(put by clay-subscriptions.state) disc duct)
    ::
    $(discs t.discs)
  ::  +cleanup: try to clean up a build and its sub-builds
  ::
  ++  cleanup
    |=  =build
    ^+  this
    ::   does this build even exist?!
    ::
    ?.  (~(has ju by-date.builds.state) date.build schematic.build)
      this
    ::
    ::  if something depends on this build, no-op and return
    ::
    ?:  ?|  (~(has by client-builds.components.state) build)
            (~(has by client-builds.provisional-components.state) build)
            (~(has by old.rebuilds.state) build)
            (~(has by listeners.state) build)
        ==
      ::  ~&  :*  %cleanup-no-op
      ::          build=(build-to-tape build)
      ::          has-client-builds=(~(has by client-builds.components.state) build)
      ::          has-provisional=(~(has by client-builds.provisional-components.state) build)
      ::          has-old-rebuilds=(~(has by old.rebuilds.state) build)
      ::          listeners=(~(get by listeners.state) build)
      ::      ==
      this
    ::  ~&  [%cleaning-up (build-to-tape build)]
    ::  remove :build from :state, starting with its cache line
    ::
    =.  results.state  (~(del by results.state) build)
    ::  remove :build from the list of attempted builds
    ::
    =.  builds.state  (~(del by-builds builds.state) build)
    ::  if no more builds at this date, remove the date from :resource-updates
    ::
    =?    resource-updates.state
        !(~(has by by-date.builds.state) date.build)
      (~(del by resource-updates.state) date.build)
    ::
    =?    blocks.state
        ::
        ?=(%scry -.schematic.build)
      ::
      =/  =scry-request
        =,  resource.schematic.build
        [vane care `beam`[[ship.disc.rail desk.disc.rail [%da date.build]] spur.rail]]
      ::
      (~(del ju blocks.state) scry-request build)
    ::  check if :build depends on a live clay +resource
    ::
    =/  has-live-resource  ?=([%scry %c *] schematic.build)
    ::  clean up dependency tracking and maybe cancel clay subscription
    ::
    =?  this  has-live-resource
      ::  type system didn't know, so tell it again
      ::
      ?>  ?=([%scry %c *] schematic.build)
      ::
      =/  =resource  resource.schematic.build
      =/  =disc  (extract-disc resource)
      ::
      =/  should-delete-resource=?
        ::  checks if there are other live builds of this resource
        ::
        =/  dates=(list @da)
          (fall (~(get by by-schematic.builds.state) schematic.build) ~)
        ?!
        %+  lien  dates
        |=  date=@da
        ^-  ?
        =/  other-build  [date schematic.build]
        =/  listeners=(set listener)
          (fall (~(get by listeners.state) other-build) ~)
        ::
        (lien ~(tap in listeners) is-listener-live)
      ::
      =?  resources-by-disc.state  should-delete-resource
        (~(del ju resources-by-disc.state) disc resource)
      ::
      =?  dirty-discs  should-delete-resource
        (~(put in dirty-discs) disc)
      ::
      this
    ::  this also recurses on our children
    ::
    =.  ..execute  (unlink-sub-builds build)
    ::  if there is a newer rebuild of :build, delete the linkage
    ::
    =/  rebuild  (~(get by new.rebuilds.state) build)
    =?  rebuilds.state  ?=(^ rebuild)
      %_  rebuilds.state
        new  (~(del by new.rebuilds.state) build)
        old  (~(del by old.rebuilds.state) u.rebuild)
      ==
    ::  if we have a :newer-build, clean it up too
    ::
    =/  newer-build
      (~(find-next by-schematic by-schematic.builds.state) build)
    ::
    ?~  newer-build
      this
    ::
    (cleanup u.newer-build)
  ::  +clay-sub-wire: the wire to use for a clay subscription
  ::
  ++  clay-sub-wire
    |=  =disc  ^-  wire
    ::
    =+  [their desk]=disc
    ::
    /(scot %p our)/clay-sub/(scot %p their)/[desk]
  ::  +clay-add-subscription-move: subscribes to :resources
  ::
  ++  clay-add-subscription-move
    |=  [=disc resources=(set resource)]
    ^-  move
    ::  request-contents: the set of [care path]s to subscribe to in clay
    ::
    =/  request-contents=(set [care:clay path])
      %-  sy  ^-  (list [care:clay path])
      %+  murn  ~(tap in `(set resource)`resources)
      |=  =resource  ^-  (unit [care:clay path])
      ::
      ?.  ?=(%c -.resource)  ~
      ::
      `[care.resource (flop spur.rail.resource)]
    ::  if :request-contents is `~`, this code is incorrect
    ::
    ?<  ?=(~ request-contents)
    ::  their: requestee +ship
    ::
    =+  [their desk]=disc
    =/  latest-date  (~(got by latest-by-disc.state) disc)
    ::
    =/  =note
      :^  %c  %warp  sock=[our their]
      ^-  riff:clay
      [desk `[%mult case=[%da latest-date] request-contents]]
    ::
    [duct [%pass wire=(clay-sub-wire disc) note]]

  ::  +clay-cancel-subscription-move: builds a cancel move
  ::
  ++  clay-cancel-subscription-move
    |=  [old-duct=^duct =disc]
    ^-  move
    ::
    =+  [their desk]=disc
    =/  =note
      :^  %c  %warp  sock=[our their]
      ^-  riff:clay
      [desk ~]
    ::
    [old-duct [%pass wire=(clay-sub-wire disc) note]]
  --
--
::
::  end =~
::
.  ==
=,  ford-api  ::  TODO remove once in vane
::
::::  vane core
  ::
=|  ax=axle
|=  [now=@da eny=@ scry-gate=sley]
::  allow jets to be registered within this core
::
~%  %ford-d  ..is  ~  ::  XX  why the '-d'?
::
::  ^?  ::  to be added to real vane
::
|%
::  +call: handle a +task:able from arvo
::
++  call
  |=  [=duct type=* wrapped-task=(hobo task:able)]
  ^-  [p=(list move) q=_ford-gate]
  ::  unwrap :task from :wrapped-task
  ::
  =/  task=task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ((hard task:able) p.wrapped-task)
  ::
  ?-    -.task
      ::  %build: request to perform a build
      ::
      %build
   ::  perform the build indicated by :task
   ::
   ::    First, we find or create the :ship-state for :our.task,
   ::    modifying :state-by-ship as necessary. Then we dispatch to the |ev
   ::    by constructing :event-args and using them to create :start-build,
   ::    which performs the build. The result of :start-build is a pair of
   ::    :moves and a mutant :ship-state. We update our :state-by-ship map
   ::    with the new :ship-state and produce it along with :moves.
   ::
   =^  ship-state  state-by-ship.ax  (find-or-create-ship-state our.task)
   =*  event-args  [[our.task duct now scry-gate] ship-state]
   =*  start-build  start-build:(per-event event-args)
   =^  moves  ship-state  (start-build schematic.task)
   =.  state-by-ship.ax  (~(put by state-by-ship.ax) our.task ship-state)
   ::
   [moves ford-gate]
  ::
      ::  %kill: cancel a %build
      ::
      %kill
    ::
    =/  ship-state  ~|(our+our.task (~(got by state-by-ship.ax) our.task))
    =*  event-args  [[our.task duct now scry-gate] ship-state]
    =^  moves  ship-state  cancel:(per-event event-args)
    =.  state-by-ship.ax  (~(put by state-by-ship.ax) our.task ship-state)
    ::
    [moves ford-gate]
  ::
      ::  %wipe: wipe the cache, clearing half the entries
      ::
      %wipe
    ::
    =/  ship-states=(list [@p ford-state])  ~(tap by state-by-ship.ax)
    ::  wipe each ship in the state separately
    ::
    =.  state-by-ship.ax
      %+  roll  ship-states
      |=  [[ship=@p state=ford-state] accumulator=(map @p ford-state)]
      ::
      (~(put by accumulator) ship (wipe state))
    ::
    [~ ford-gate]
  ::
      %wegh
    :_  ford-gate
    :_  ~
    :^  duct  %give  %mass
    ^-  mass
    :-  %turbo
    :-  %|
    %+  turn  ~(tap by state-by-ship.ax)     :: XX single-home
    |=  {our/@ ford-state}  ^-  mass
    :+  (scot %p our)  %|
    ::  TODO: Other vanes don't reveal everything. Which parts should we report?
    :~  [%results [%& results]]
        [%builds [%& builds]]
        :+  %components  %|
        :~  [%normal [%& components]]
            [%provisional [%& provisional-components]]
        ==
        [%rebuilds [%& rebuilds]]
        [%blocks [%& blocks]]
        [%builds-by-listener [%& builds-by-listener]]
    ==
  ==
::  +wipe: wipe half a +ford-state's cache, in LRU (least recently used) order
::
++  wipe
  |=  state=ford-state
  ^+  state
  ::
  =/  cache-list=(list [build cache-line])  ~(tap by results.state)
  ::
  =/  split-cache=[(list [build cache-line]) (list [build cache-line])]
    %+  skid  cache-list
    |=([=build =cache-line] ?=(%tombstone -.cache-line))
  ::
  =/  tombstones=(list [build cache-line])  -.split-cache
  =/  values=(list [build cache-line])      +.split-cache
  ::  sort the cache lines in chronological order by :last-accessed
  ::
  =/  sorted=(list [build cache-line])
    %+  sort  values
    |=  [a=[=build =cache-line] b=[=build =cache-line]]
    ^-  ?
    ::
    ?>  ?=(%value -.cache-line.a)
    ?>  ?=(%value -.cache-line.b)
    ::
    (lte last-accessed.cache-line.a last-accessed.cache-line.b)
  ::
  =/  num-entries=@  (lent cache-list)
  ::  num-stale: half of :num-entries, rounded up in case :num-entries is 1
  ::
  =/  num-stale  (sub num-entries (div num-entries 2))
  ~&  "ford: wipe: {<num-stale>} cache entries"
  ::
  =/  stale=(list [build cache-line])  (scag num-stale sorted)
  =/  fresh=(list [build cache-line])  (slag num-stale sorted)
  ::
  =/  stale-tombstones=(list [build cache-line])
    %+  turn  stale
    |=  [=build =cache-line]
    ^+  +<
    [build [%tombstone ~]]
  ::
  =|  results=(map build cache-line)
  ::
  =.  results  (~(gas by results) tombstones)
  =.  results  (~(gas by results) stale-tombstones)
  =.  results  (~(gas by results) fresh)
  ::
  state(results results)
::  +take: receive a response from another vane
::
++  take
  |=  [=wire =duct wrapped-sign=(hypo sign)]
  ^-  [p=(list move) q=_ford-gate]
  ::  unwrap :sign from :wrapped-sign
  ::
  ::    TODO: verify wrapped-sign isn't an evil vase?
  ::
  =/  =sign  q.wrapped-sign
  ::  TODO: support other responses
  ::
  ::  parse :wire into :our, :ship-state, and :resource
  ::
  ?>  ?=([@ @ *] wire)
  ::  we know :our is already in :state-by-ship because we sent this request
  ::
  =/  our=@p  (slav %p i.wire)
  =/  ship-state  ~|(our+our (~(got by state-by-ship.ax) our))
  =*  event-args  [[our duct now scry-gate] ship-state]
  ::  %clay-sub: response to a clay %mult subscription
  ::
  =^  moves  ship-state
    ?:  =(%clay-sub i.t.wire)
      ::
      ?>  ?=([%c %wris *] sign)
      =+  [ship desk]=(raid:wired t.t.wire ~[%p %tas])
      ::
      =*  rebuild  rebuild:(per-event event-args)
      (rebuild ship desk case.sign care-paths.sign)
    ::  %resource: response to a request for a +resource
    ::
    ?.  =(%scry-request i.t.wire)
      ::
      ~|(unknown-take+i.t.wire !!)
    ::
    ?>  ?=([%c %writ *] sign)
    ::  scry-request: the +scry-request we had previously blocked on
    ::
    =/  =scry-request
      ~|  [%bad-scry-request wire]
      (need (path-to-scry-request t.t.wire))
    ::  scry-result: parse a (unit cage) from :sign
    ::
    ::    If the result is `~`, the requested resource was not available.
    ::
    =/  scry-result=(unit cage)
      ?~  riot.sign
        ~
      `r.u.riot.sign
    ::  unblock the builds that had blocked on :resource
    ::
    =*  unblock  unblock:(per-event event-args)
    (unblock scry-request scry-result)
  ::
  =.  state-by-ship.ax  (~(put by state-by-ship.ax) our ship-state)
  ::
  [moves ford-gate]
::  +load: migrate old state to new state (called on vane reload)
::
++  load
  |=  old=axle
  ^+  ..^$
  ::
  ~!  %loading
  ..^$(ax old)
::  +stay: produce current state
::
++  stay  `axle`ax
::  +scry: request a path in the urbit namespace
::
++  scry
  |=  *
  [~ ~]
::  %utilities
::
::+|
::
++  ford-gate  ..$
::  +find-or-create-ship-state: find or create a ford-state for a @p
::
::    Accesses and modifies :state-by-ship.
::
++  find-or-create-ship-state
  |=  our=@p
  ^-  [ford-state _state-by-ship.ax]
  ::
  =/  existing  (~(get by state-by-ship.ax) our)
  ?^  existing
    [u.existing state-by-ship.ax]
  ::
  =|  new-state=ford-state
  [new-state (~(put by state-by-ship.ax) our new-state)]
--
