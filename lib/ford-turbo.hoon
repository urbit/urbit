!:
::
::  sys/zuse/hoon
::
::  |ford: build system vane interface
::
|%
++  ford-api  ^?
  |%
  ::  |able:ford: ford's public +move interface
  ::
  ++  able  ^?
    |%
    ::  +task:able:ford: requests to ford
    ::
    +=  task
      $%  ::  %make: perform a build, either live or once
          ::
          $:  %make
              ::  our: who our ship is (remove after cc-release)
              ::
              our=@p
              ::  plan: the schematic to build
              ::
              =schematic
          ==
          ::  %kill: stop a build; send on same duct as original %make request
          ::
          $:  %kill
              ::  our: who our ship is (remove after cc-release)s
              ::
              our=@p
          ==
          ::  %wegh: produce memory usage information
          ::
          [%wegh ~]
          ::  %wipe: clear cache
          ::
          [%wipe ~]
      ==
    ::  +gift:able:ford: responses from ford
    ::
    +=  gift
      $%  ::  %mass: memory usage; response to %wegh +task
          ::
          [%mass p=mass]
          ::  %made: build result; response to %make +task
          ::
          $:  %made
              ::  date: formal date of the build
              ::
              date=@da
              ::  result: result of the build; either complete build, or error
              ::
              $=  result
              $%  ::  %complete: contains the result of the completed build
                  ::
                  [%complete build-result]
                  ::  %incomplete: couldn't finish build; contains error message
                  ::
                  [%incomplete tang]
      ==  ==  ==
    --
  ::  +disc: a desk on a ship; can be used as a beak that varies with time
  ::
  +=  disc  [=ship =desk]
  ::  +rail: a time-varying full path
  ::
  ::    This can be thought of as a +beam without a +case, which is what
  ::    would specify the time. :spur is flopped just like the +spur in a +beam.
  ::
  +=  rail  [=disc =spur]
  ::  +dependency: dependency on a value from the urbit namespace
  ::
  +=  dependency
    $:  ::  vane which we are querying
        ::
        vane=?(%c %g)
        ::  type of request
        ::
        ::    TODO: care:clay should be cleaned up in zuse as it is a general
        ::    type, not a clay specific one.
        ::
        care=care:clay
        ::  path on which to depend, missing time which will be filled in
        ::
        =rail
    ==
  ::  +build-result: the referentially transparent result of a +build
  ::
  ::    A +build produces either an error or a result. A result is a tagged
  ::    union of the various kinds of datatypes a build can produce. The tag
  ::    represents the sub-type of +schematic that produced the result.
  ::
  +=  build-result
    $%  ::  %error: the build produced an error whose description is :message
        ::
        [%error message=tang]
        ::  %result: result of successful +build, tagged by +schematic sub-type
        ::
        $:  %result
            $^  [head=build-result tail=build-result]
            $%  [%$ =cage]
                [%pin date=@da =build-result]
                [%alts =build-result]
                [%bake =cage]
                [%bunt =cage]
                [%call =vase]
                [%cast =cage]
                [%core =vase]
                [%diff =cage]
                [%dude =build-result]
                [%hood =scaffold]
                [%join =cage]
                [%mash =cage]
                [%mute =cage]
                [%pact =cage]
                [%path =rail]
                [%plan =vase]
                [%reef =vase]
                [%ride =vase]
                [%same =build-result]
                [%scry =cage]
                [%slim [=type =nock]]
                [%slit =type]
                [%vale =cage]
                [%volt =cage]
    ==  ==  ==
  ::
  ::  +schematic: plan for building
  ::
  ++  schematic
    ::    If the head of the +schematic is a pair, it's an auto-cons
    ::    schematic. Its result will be the pair of results of its
    ::    sub-schematics.
    ::
    $^  [head=schematic tail=schematic]
    ::
    $%  ::  %$: literal value. Produces its input unchanged.
        ::
        $:  %$
            ::  literal: the value to be produced by the build
            ::
            literal=cage
        ==
        ::  %pin: pins a sub-schematic to a date
        ::
        ::    There is a difference between live builds and once builds. In
        ::    live builds, we produce results over and over again and aren't
        ::    pinned to a specifc time. In once builds, we want to specify a
        ::    specific date, which we apply recursively to any sub-schematics
        ::    contained within :schematic.
        ::
        ::    If a build has a %pin at the top level, we consider it to be a
        ::    once build. Otherwise, we consider it to be a live build. We do
        ::    this so schematics which depend on the result of a once build can
        ::    be cached, giving the client explicit control over the caching
        ::    behaviour.
        ::
        $:  %pin
            ::  date: time at which to perform the build
            ::
            date=@da
            ::  schematic: wrapped schematic of pinned time
            ::
            =schematic
        ==
        ::  %alts: alternative build choices
        ::
        ::    Try each choice in :choices, in order; accept the first one that
        ::    succeeds. Note that the result inherits the dependencies of all
        ::    failed schematics, as well as the successful one.
        ::
        $:  %alts
            ::  choices: list of build options to try
            ::
            choices=(list schematic)
        ==
        ::  %bake: run a file through a renderer
        ::
        $:  %bake
            ::  renderer: name of renderer; also its file path in ren/
            ::
            renderer=term
            ::  query-string: the query string of the renderer's http path
            ::
            query-string=coin
            ::  path-to-render: full path of file to render
            ::
            path-to-render=rail
        ==
        ::  %bunt: produce the default value for a mark
        ::
        $:  %bunt
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark; also its file path in mar/
            ::
            mark=term
        ==
        ::  %call: call a gate on a sample
        ::
        $:  %call
            ::  gate: schematic whose result is a gate
            ::
            gate=schematic
            ::  sample:  schematic whose result will be the gate's sample
            ::
            sample=schematic
        ==
        ::  %cast: cast the result of a schematic through a mark
        ::
        $:  %cast
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark; also its file path in ren/
            ::
            mark=term
            ::  input: schematic whose result will be run through the mark
            ::
            input=schematic
        ==
        ::  %core: build a hoon program from a source file
        ::
        $:  %core
            ::  source-path: clay path from which to load hoon source
            ::
            source-path=rail
        ==
        ::  %diff: produce marked diff from :first to :second
        ::
        $:  %diff
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  old: schematic producing data to be used as diff starting point
            ::
            start=schematic
            ::  new: schematic producing data to be used as diff ending point
            ::
            end=schematic
        ==
        ::  %dude: wrap a failure's error message with an extra message
        ::
        $:  %dude
            ::  error: a trap producing an error message to wrap the original
            ::
            error=(trap tank)
            ::  attempt: the schematic to try, whose error we wrap, if any
            ::
            attempt=schematic
        ==
        ::  %hood: create a +hood from a hoon source file
        ::
        $:  %hood
            ::  source-path: clay path from which to load hoon source
            ::
            source-path=rail
        ==
        ::  %join: merge two diffs into one diff; produces `~` if conflicts
        ::
        $:  %join
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of the mark to use for diffs; also file path in mar/
            ::
            mark=term
            ::  first: schematic producing first diff
            ::
            first=schematic
            ::  second: schematic producing second diff
            ::
            second=schematic
        ==
        ::  %mash: force a merge, annotating any conflicts
        ::
        $:  %mash
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark used in diffs; also file path in mar/
            ::
            mark=term
            ::  first: schematic producing first diff
            ::
            first=schematic
            ::  second: schematic producing second diff
            ::
            second=schematic
        ==
        ::  %mute: mutate a noun by replacing its wings with new values
        ::
        $:  %mute
            ::  subject: schematic producing the noun to mutate
            ::
            subject=schematic
            ::  mutations: axes and schematics to produce their new contents
            ::
            mutations=(list (pair wing schematic))
        ==
        ::  %pact: patch a marked noun by applying a diff
        ::
        $:  %pact
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark to use in diff; also file path in mar/
            ::
            mark=term
            ::  start: schematic producing a noun to be patched
            ::
            start=schematic
            ::  diff: schematic producing the diff to apply to :start
            ::
            diff=schematic
        ==
        ::  %path: resolve a path with `-`s to a path with `/`s
        ::
        ::    Resolve +file-path to a path containing a file, replacing
        ::    any `-`s in the path with `/`s if no file exists at the
        ::    original path. Produces an error if multiple files match,
        ::    e.g. a/b/c and a/b-c, or a/b/c and a-b/c.
        ::
        ::    TODO verify current implementation
        ::
        $:  %path
            ::  disc the +disc within which to resolve :file-path
            ::
            location=disc
            ::  file-path: the path to resolve
            ::
            file-path=@tas
        ==
        ::  %plan: build a hoon program from a preprocessed source file
        ::
        $:  %plan
            ::  source-path: the clay path of the hoon source file
            ::
            source-path=rail
            ::  query-string: the query string of the http request
            ::
            query-string=coin
            ::  scaffold: preprocessed hoon source and imports
            ::
            =scaffold
        ==
        ::  %reef: produce a hoon+zuse kernel. used internally for caching
        ::
        [%reef ~]
        ::  %ride: eval hoon as formula with result of a schematic as subject
        ::
        $:  %ride
            ::  formula: a hoon to be evaluated against a subject
            ::
            formula=hoon
            ::  subject: a schematic whose result will be used as subject
            ::
            subject=schematic
        ==
        ::  %same: the identity function
        ::
        ::    Functionally used to "unpin" a build for caching reasons. If you
        ::    run a %pin build, it is treated as a once build and is therefore
        ::    not cached. Wrapping the %pin schematic in a %same schematic
        ::    converts it to a live build, which will be cached due to live
        ::    build subscription semantics.
        ::
        $:  %same
            ::  schematic that we evaluate to
            ::
            =schematic
        ==
        ::  %scry: lookup a value from the urbit namespace
        ::
        $:  %scry
            ::  request: the request to be made against the namespace
            ::
            =dependency
        ==
        ::  %slim: compile a hoon against a subject type
        ::
        $:  %slim
            ::  compile-time subject type for the :formula
            ::
            subject-type=type
            ::  formula: a +hoon to be compiled to (pair type nock)
            ::
            formula=hoon
        ==
        ::  %slit: get type of gate product
        ::
        $:  %slit
            ::  gate: a vase containing a gate
            ::
            gate=vase
            ::  sample: a vase containing the :gate's sample
            ::
            sample=vase
        ==
        ::  %vale: coerce a noun to a mark, validated
        ::
        $:  %vale
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark to use; also file path in mar/
            ::
            mark=term
            ::  input: the noun to be converted using the mark
            ::
            input=*
        ==
        ::  %volt: coerce a noun to a mark, unsafe
        ::
        $:  %volt
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark to use; also file path in mar/
            ::
            mark=term
            ::  input: the noun to be converted using the mark
            ::
            input=*
        ==
    ==
  ::
  ::  +scaffold: program construction in progress
  ::
  ::    A source file with all its imports and requirements, which will be
  ::    built and combined into one final product.
  ::
  +=  scaffold
    $:  ::  zuse-version: the kelvin version of the standard library
        ::
        zuse-version=@ud
        ::  structures: files from %/sur which are included
        ::
        structures=(list cable)
        ::  libraries: files from %/lib which are included
        ::
        libraries=(list cable)
        ::  cranes: a list of resources to transform and include
        ::
        cranes=(list crane)
        ::  sources: hoon sources, either parsed or on the filesystem
        ::
        sources=(list brick)
    ==
  ::  +cable: a reference to something on the filesystem
  ::
  +=  cable
    $:  ::  expand-namespace: expose internal faces to subject
        ::
        expand-namespace=?
        ::  file-path: location in clay
        ::
        file-path=term
        ::  remote-location: if not `~`, remote location of file
        ::
        remote-location=(unit (pair case ship))
    ==
  ::  +brick: hoon code, either directly specified or referencing clay
  ::
  +=  brick
    $%  $:  ::  %direct: inline parsed hoon
            ::
            %direct
            source=hoon
        ==
        $:  ::  %indirect: reference to a hoon file in clay
            ::
            %indirect
            location=beam
    ==  ==
  ::  +truss: late-bound path
  ::
  ::    TODO: the +tyke data structure should be rethought, possibly as part
  ::    of this effort since it is actually a `(list (unit hoon))`, when it
  ::    only represents @tas. It should be a structure which explicitly
  ::    represents a path with holes that need to be filled in.
  ::
  +=  truss
    $:  pre=(unit tyke)
        pof=(unit [p=@ud q=tyke])
    ==
  ::  +crane: parsed rune used to include and transform resources
  ::
  ::    Cranes lifting cranes lifting cranes!
  ::
  ::    A recursive tree of Ford directives that specifies instructions for
  ::    including and transforming resources from the Urbit namespace.
  ::
  +=  crane
    $%  $:  ::  %fssg: `/~` hoon literal
            ::
            ::    `/~ <hoon>` produces a crane that evaluates arbitrary hoon.
            ::
            %fssg
            =hoon
        ==
        $:  ::  %fsbc: `/$` process query string
            ::
            ::    `/$` will call a gate with the query string supplied to this
            ::    build. If no query string, this errors.
            ::
            %fsbc
            =hoon
        ==
        $:  ::  %fsbr: `/|` first of many options that succeeds
            ::
            ::    `/|` takes a series of cranes and produces the first one
            ::    (left-to-right) that succeeds. If none succeed, it produces
            ::    stack traces from all of its arguments.
            ::
            %fsbr
            ::  choices: cranes to try
            ::
            choices=(list crane)
        ==
        $:  ::  %fsts: `/=` wrap a face around a crane
            ::
            ::    /= runs a crane (usually produced by another ford rune), takes
            ::    the result of that crane, and wraps a face around it.
            ::
            %fsts
            ::  face: face to apply
            ::
            face=term
            ::  crane: internal build step
            ::
            =crane
        ==
        $:  ::  %fsdt: `/.` null-terminated list
            ::
            ::    Produce a null-terminated list from a sequence of cranes,
            ::    terminated by a `==`.
            ::
            %fsdt
            ::  items: cranes to evaluate
            ::
            items=(list crane)
        ==
        $:  ::  %fscm: `/,` switch by path
            ::
            ::    `/,` is a switch statement, which picks a branch to evaluate
            ::    based on whether the current path matches the path in the
            ::    switch statement. Takes a sequence of pairs of (path, crane)
            ::    terminated by a `==`.
            ::
            %fscm
            ::  cases: produces evaluated crane of first +spur match
            ::
            cases=(list (pair spur crane))
        ==
        $:  ::  %fscn: `/%` propagate extra arguments into renderers
            ::
            ::    `/%` will forward extra arguments (usually from Eyre) on to
            ::    any renderer in :crane. Without this, renderers that use `/$`
            ::    to read the extra arguments will crash.
            ::
            %fscn
            =crane
        ==
        $:  ::  %fspm: `/&` pass through a series of marks
            ::
            ::    `/&` passes a crane through multiple marks, right-to-left.
            ::
            %fspm
            ::  marks: marks to apply to :crane, in reverse order
            ::
            marks=(list mark)
            =crane
        ==
        $:  ::  %fscb: `/_` run a crane on each file in the current directory
            ::
            ::    `/_` takes a crane as an argument. It produces a new crane
            ::    representing the result of mapping the supplied crane over the
            ::    list of files in the current directory. The keys in the
            ::    resulting map are the basenames of the files in the directory,
            ::    and each value is the result of running that crane on the
            ::    contents of the file.
            ::
            %fscb
            =crane
        ==
        $:  ::  %fssm: `/;` operate on
            ::
            ::    `/;` takes a hoon and a crane. The hoon should evaluate to a
            ::    gate, which is then called with the result of the crane as its
            ::    sample.
            ::
            %fssm
            =hoon
            =crane
        ==
        $:  ::  %fscl: `/:` evaluate at path
            ::
            ::    `/:` takes a path and a +crane, and evaluates the crane with
            ::    the current path set to the supplied path.
            ::
            %fscl
            ::  path: late bound path to be resolved relative to current beak
            ::
            ::    This becomes current path of :crane
            ::
            path=truss
            =crane
        ==
        $:  ::  %fskt: `/^` cast
            ::
            ::    `/^` takes a +mold and a +crane, and casts the result of the
            ::    crane to the mold.
            ::
            %fskt
            ::  mold: evaluates to a mold to be applied to :crane
            ::
            mold=hoon
            =crane
        ==
        $:  ::  %fszp: `/!mark/` evaluate as hoon, then pass through mark
            ::
            %fszp
            =mark
        ==
        $:  ::  %fszy: `/mark/` passes current path through :mark
            ::
            %fszy
            =mark
    ==  ==
  --
--
::
::  sys/ford/hoon
::
::  when ford becomes a real vane, it'll start from a vase
::
::  |=  pit=vase
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
      ::  %make: perform a build
      ::
      $%  $:  %make
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
              =case
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
      ::  builds-by-schematic: all attempted builds, sorted by time
      ::
      ::    For each schematic we've attempted to build at any time,
      ::    list the formal dates of all build attempts, sorted newest first.
      ::
      builds-by-schematic=(map schematic (list @da))
      ::  builds-by-date: all attempted builds, grouped by time
      ::
      builds-by-date=(jug @da schematic)
      ::  components: bidirectional linkages between sub-builds and clients
      ::
      ::    The first of the two jugs maps from a build to its sub-builds.
      ::    The second of the two jugs maps from a build to its client builds.
      ::
      $=  components
      $:  ::  sub-builds: jug from a build to its sub-builds
          ::
          sub-builds=(jug build build)
          ::  client-builds: jug from a build to its client builds
          ::
          client-builds=(jug build build)
      ==
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
      ::  blocks: map from +dependency to all builds waiting for its retrieval
      ::
      blocks=(jug dependency build)
  ::
  ::  build request tracking
  ::
      ::  listeners: external requests for a build
      ::
      listeners=(jug build listener)
      ::  builds-by-listener: reverse lookup for :listeners
      ::
      builds-by-listener=(map duct [=build live=?])
      ::  root-builds: only the root builds, not all builds.
      ::
      root-builds=(jug build listener)
  ::
  ::  update tracking
  ::
      ::  dependencies: live clay dependencies
      ::
      ::    Used for looking up which +dependency's rely on a particular
      ::    +disc, so that we can send a new Clay subscription with all
      ::    the resources we care about within that disc.
      ::
      dependencies=(jug disc dependency)
      ::  latest-by-disc: latest formal date of a completed live build on disc
      ::
      ::    Updated each time we complete a build of a +dependency,
      ::    if the build's formal date is later than the stored value.
      ::
      latest-by-disc=(map disc @da)
      ::  clay-subscriptions: ducts we'll use to cancel existing clay requests
      ::
      clay-subscriptions=(set disc)
      ::  dependency-updates: all clay updates we need to know about
      ::
      ::    dependency-updates stores all Clay changes at dates that
      ::    Ford needs to track because Ford is tracking attempted builds with
      ::    that formal date.
      ::
      dependency-updates=(jug @da dependency)
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
  $%  ::  %result: the result of running a +build, and its last access time
      ::
      $:  %result
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
::  +vane: short names for vanes
::
::    TODO: move to zuse
::
+=  vane  ?(%a %b %c %d %e %f %g)
--
=,  format
|%
::  +build-to-tank: convert :build to a printable format
::
++  build-to-tank
  |=  =build
  ^-  tank
  ::
  =+  [date schematic]=build
  ::
  :-  %leaf
  %+  weld  (trip (scot %da date))
  %+  weld  "  "
  ::
  %-  trip
  ?+    -.schematic
      -.schematic
    ::
    %$  %literal
    ^  %autocons
  ==
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
::  +dependency-to-path: encode a +dependency in a +wire
::
::    If :dependency is live, create a +beam from :rail.dependency
::    by using revision 0 as the +case and encode that.
::
++  dependency-to-path
  |=  =dependency
  ^-  path
  =/  term=term  (cat 3 [vane care]:dependency)
  [term (en-beam (extract-beam dependency date=~))]
::  +path-to-dependency: decode a +dependency from a +wire
::
++  path-to-dependency
  |=  =path
  ^-  (unit dependency)
  ::
  =/  scry-request=(unit scry-request)  (path-to-scry-request path)
  ?~  scry-request
    ~
  =+  [vane care bem]=u.scry-request
  =/  beam=beam  bem
  =/  rail=rail  [disc=[p.beam q.beam] spur=s.beam]
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
::  +extract-beam: obtain a +beam from a +dependency
::
::    Fills case with [%ud 0] for live dependencies if :date is `~`.
::    For once dependencies, ignore :date.
::
++  extract-beam
  |=  [=dependency date=(unit @da)]  ^-  beam
  ::
  =/  case=case  ?~(date [%ud 0] [%da u.date])
  ::
  =,  rail.dependency
  [[ship.disc desk.disc case] spur]
::  +extract-disc: obtain a +disc from a +dependency
::
++  extract-disc
  |=  =dependency  ^-  disc
  disc.rail.dependency
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
      %mash  ~[first.schematic second.schematic]
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
  ==
::  +result-to-cage
::
::    Maybe we should return vases instead of cages.
::
++  result-to-cage
  |=  result=build-result
  ^-  cage
  ?:  ?=(%error -.result)
    [%tang !>(message.result)]
  ?-    -.+.result
      ^      [%noun (slop q:$(result head.result) q:$(result tail.result))]
      %$     cage.result
      %pin   $(result build-result.result)
      %alts  $(result build-result.result)
      %bake  cage.result
      %bunt  cage.result
      %call  [%noun vase.result]
      %cast  cage.result
      %core  [%noun vase.result]
      %diff  cage.result
      %dude  $(result build-result.result)
      %hood  [%noun !>(scaffold.result)]
      %join  cage.result
      %mash  cage.result
      %mute  cage.result
      %pact  cage.result
      %path  [%noun !>(rail.result)]
      %plan  [%noun vase.result]
      %reef  [%noun vase.result]
      %ride  [%noun vase.result]
      %same  $(result build-result.result)
      %scry  cage.result
      %slim  [%noun !>([type nock]:result)]
      %slit  [%noun !>(type.result)]
      %vale  cage.result
      %volt  cage.result
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
::  +by-schematic: door for manipulating :builds-by-schematic.ford-state
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
::  +per-event: per-event core
::
++  per-event
  ::  moves: the moves to be sent out at the end of this event, reversed
  ::
  =|  moves=(list move)
  ::  dirty-discs: discs whose dependencies have changed during this event
  ::
  =|  dirty-discs=(set disc)
  ::  scry-results: responses to scry's to handle in this event
  ::
  ::    If a value is `~`, the requested resource is not available.
  ::    Otherwise, the value will contain a +cage.
  ::
  =|  scry-results=(map dependency (unit cage))
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
  ::  original-dependencies: :dependencies.state at event start
  ::
  =/  original-dependencies  dependencies.state
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
      =/  build=build  [now schematic]
      ::
      =:    listeners.state
        (~(put ju listeners.state) build [duct %.y])
      ::
          builds-by-listener.state
        (~(put by builds-by-listener.state) duct [build %.y])
      ::
          root-builds.state
        (~(put ju root-builds.state) build [duct %.y])
      ==
      ::
      (execute build from=~)
    ::
    ++  start-once-build
      ^+  this
      =/  pin-date=@da  (date-from-schematic schematic)
      =/  build=build  [pin-date schematic]
      ::  associate +listener with :build in :state
      ::
      =:  listeners.state
        (~(put ju listeners.state) build [duct %.n])
      ::
          builds-by-listener.state
        (~(put by builds-by-listener.state) duct [build %.n])
      ::
          root-builds.state
        (~(put ju root-builds.state) build [duct %.n])
      ==
      ::
      (execute build from=~)
    ::
    --
  ::  +rebuild: rebuild any live builds based on +dependency updates
  ::
  ++  rebuild
    |=  [=beak care-paths=(set [care=care:clay =path])]
    ^-  [(list move) ford-state]
    ::
    =<  finalize
    ::
    =/  date=@da  ?>(?=(%da -.r.beak) p.r.beak)
    =/  disc=disc  [p.beak q.beak]
    ::  delete the now-dead clay subscription
    ::
    =.  clay-subscriptions.state  (~(del in clay-subscriptions.state) disc)
    ::
    =/  dependencies=(list dependency)
      %+  turn  ~(tap in care-paths)
      |=  [care=care:clay =path]  ^-  dependency
      ::
      [%c care rail=[disc spur=(flop path)]]
    ::  store changed dependencies persistently in case rebuilds finish later
    ::
    =.  dependency-updates.state
      %+  roll  dependencies
      |=  [=dependency dependency-updates=_dependency-updates.state]
      ::
      (~(put ju dependency-updates) date dependency)
    ::  rebuild dependency builds at the new date
    ::
    %+  roll  dependencies
    |=  [=dependency that=_this]
    ::
    =/  build=build  [date `schematic`[%scry dependency]]
    ::
    =/  previous-build=^build
      (need (~(find-previous by-schematic builds-by-schematic.state) build))
    ::
    (execute:that build from=`previous-build)
  ::  +unblock: continue builds that had blocked on :dependency
  ::
  ++  unblock
    |=  [=dependency scry-result=(unit cage)]
    ^-  [(list move) ford-state]
    ::
    =<  finalize
    ::  find all the :blocked-builds to continue
    ::
    =/  blocked-builds  ~(tap in (~(get ju blocks.state) dependency))
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
    =.  scry-results  (~(put by scry-results) dependency scry-result)
    ::
    %+  roll  blocked-builds
    |=  [=build that=_this]
    (execute:that build from=~)
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
    ::
    =.  state  (remove-listener-from-build [duct live] build)
    ::
    (cleanup build)
  ::  +remove-listener-from-build: recursively remove listener from (sub)builds
  ::
  ++  remove-listener-from-build
    |=  [=listener =build]
    ^+  state
    %_    state
        listeners
      ::  remove mappings from :build and its sub-builds to :duct
      ::
      =/  builds=(list ^build)  ~[build]
      ::
      |-  ^+  listeners.state
      ?~  builds  listeners.state
      ::
      =.  listeners.state
        (~(del ju listeners.state) i.builds listener)
      ::
      =/  sub-builds  (~(get ju sub-builds.components.state) i.builds)
      ::
      $(builds (welp t.builds ~(tap in sub-builds)))
    ::
        ::  remove mapping from :duct to :build
        ::
        builds-by-listener
      (~(del by builds-by-listener.state) duct.listener)
    ::
        root-builds
      (~(del ju root-builds.state) build listener)
    ==
  ::  |construction: arms for performing builds
  ::
  ::+|  construction
  ::
  ::  +execute: main recursive construction algorithm
  ::
  ::    Runs +make on :build if necessary, and recurses potentially
  ::    "upward" to :build's clients and "downward" to :build's sub-builds.
  ::    Enqueues moves to Clay to request resources for blocked +scry
  ::    operations and places completed root builds in :done-live-roots
  ::    to be processed at the end of the event.
  ::
  ++  execute
    |=  [=build from=(unit build)]
    ^+  this
    ::  normalize :date.build for a %pin schematic
    ::
    =?  date.build  ?=(%pin -.schematic.build)  date.schematic.build
    ::  if the build is complete and not a %tombstone, we're done
    ::
    =^  maybe-cache-line  results.state  (access-cache build)
    ::  copy all the ducts from :from to this one
    ::
    =?  listeners.state  ?=(^ from)
      =/  consolidated
        %-  ~(uni in (fall (~(get by listeners.state) build) ~))
        (fall (~(get by listeners.state) u.from) ~)
      ?~  consolidated
        listeners.state
      (~(put by listeners.state) build consolidated)
    ::
    ?:  ?=([~ %result *] maybe-cache-line)
      ::
      this
    ::  place :build in :state if it isn't already there
    ::
    =:
        builds-by-date.state
      (~(put ju builds-by-date.state) date.build schematic.build)
    ::
        builds-by-schematic.state
      (~(put by-schematic builds-by-schematic.state) build)
    ==
    ::  find :previous-build and :previous-result in :state
    ::
    =/  previous-build
      (~(find-previous by-schematic builds-by-schematic.state) build)
    ::
    =^  previous-result  results.state
      ?~  previous-build  [~ results.state]
      (access-cache u.previous-build)
    ::  kids: sub-builds of :build
    ::
    =/  kids  ~(tap in (~(get ju sub-builds.components.state) build))
    ::  result: +build-result to be populated by +make or a previous result
    ::
    =|  result=(unit build-result)
    ::
    =<  ..execute
    |^
    ::
    ::  if :build is unchanged from :previous-build, don't run +make
    ::
    =^  subs-changed  results.state  sub-builds-changed
    ::
    ?.  |(subs-changed dependencies-changed)
      ::  copy :previous-result to new date
      ::
      ?>  ?=([~ %result *] previous-result)
      ::
      =.  result  `build-result.u.previous-result
      ::
      =~  store-result
          update-live-tracking
          ::
          ?<  ?=(~ previous-build)
          =.  state  (promote-live-listeners u.previous-build build)
          ::
          link-rebuilds
          =.  ..execute  (send-mades build current-once-listeners)
          delete-once-listeners
      ==
    ::  the build isn't complete, so try running +make on it
    ::
    =^  made  ..execute  (make build)
    ::  dispatch on the product of +make
    ::
    ?-    -.result.made
        ::  %build-result: build completed and produced its result
        ::
        %build-result
      ::
      =.  result  `build-result.result.made
      ::
      =>  store-result
      =>  update-live-tracking
      ::
      =?  state  &(?=(^ previous-build) ?=(^ previous-result))
        (promote-live-listeners u.previous-build build)
      ::
      =.  ..execute  (send-mades build (root-once-listeners build))
      =>  delete-once-listeners
      ::  if result is same as previous, note sameness
      ::
      =/  same-result=?
        ?<  ?=(~ result)
        ::
        ?&  ?=([~ %result *] previous-result)
            =(u.result build-result.u.previous-result)
        ==
      ::
      ?:  same-result
        ::
        link-rebuilds
      ::
      =.  ..execute  (send-mades build (root-live-listeners build))
      =>  ?~(previous-build this (cleanup u.previous-build))
      =>  (cleanup build)
      =>  ::  recurse "upward" into client builds now that :build is done
          ::
          =/  clients=(list ^build)
            ~(tap in (fall (~(get by client-builds.components.state) build) ~))
          ::
          |-  ^+  this
          ?~  clients  this
          ::  don't copy listeners upward
          ::
          ::    We need to copy listeners downward so that sub-builds inherit
          ::    listeners from the root. We don't want to copy listeners upward
          ::    because then all roots would get listeners copied to each
          ::    other.
          ::
          $(clients t.clients, ..execute (execute i.clients from=~))
      ::
      |-  ^+  this
      =/  next  (~(find-next by-schematic builds-by-schematic.state) build)
      ?~  next
        ::  no future build
        ::
        this
      ::
      =^  next-result  results.state  (access-cache u.next)
      ?~  next-result
        ::  unfinished future build
        ::
        this
      ?:  ?=(%result -.u.next-result)
        ::
        =.  state  (promote-live-listeners build u.next)
        =.  this  (cleanup build)
        ::
        =.  ..execute  (send-mades u.next (root-live-listeners u.next))
        ::
        $(build u.next)
      ::
      this(..execute (execute u.next from=~))
    ::
        ::  %blocks: build got stuck and produced a list of blocks
        ::
        %blocks
      ::
      =/  blocks=(list ^build)  builds.result.made
      |-  ^+  this
      ?~  blocks  this
      ::
      =*  block  i.blocks
      $(blocks t.blocks, ..execute (execute block from=`build))
    ==
    ::  +sub-builds-changed: did sub-builds change since :previous-build?
    ::
    ::    Produces the answer and a mutant version of :results.state,
    ::    since it freshens cache lines.
    ::
    ++  sub-builds-changed  ^-  [any-changed=? _results.state]
      ::  if there's nothing to promote, consider :build changed
      ::
      ?~  previous-build  [& results.state]
      ::  grab the sub-builds of :previous-build
      ::
      =/  old-sub-builds
        (~(get ju sub-builds.components.state) u.previous-build)
      ::
      =/  subs  ~(tap in old-sub-builds)
      ::  freshen cache for old sub-builds and their rebuilds
      ::
      ::    If any :subs don't have a :rebuild, :build needs to be rebuilt.
      ::
      %+  roll  subs
      |=  $:  sub=^build
              [any-changed=_| results=_results.state]
          ==
      ::  place :results in the state so +access-cache sees them
      ::
      =.  results.state  results
      ::  freshen the cache for :sub
      ::
      =^  old-result  results.state  (access-cache sub)
      ::  find a later +build of :sub with an identical result
      ::
      =/  rebuild  (~(get by new.rebuilds.state) sub)
      ::  if there's no :rebuild of :sub, :build needs to be rebuilt.
      ::
      ?~  rebuild
        [& results.state]
      ::  freshen the cache for :rebuild
      ::
      =^  new-result  results.state  (access-cache u.rebuild)
      ::
      [any-changed results.state]
    ::  +dependencies-changed: did dependencies change since :previous-build?
    ::
    ++  dependencies-changed  ^-  ?
      ?.  ?=(%scry -.schematic.build)
        |
      ::
      =/  dependency  dependency.schematic.build
      ::
      ?.  ?=(%c -.dependency)
        |
      ::
      =/  updates  (fall (~(get by dependency-updates.state) date.build) ~)
      ::
      (~(has in updates) dependency)
    ::  +current-once-listeners: once listeners on :build
    ::
    ++  current-once-listeners  ^-  (list listener)
      (skip current-listeners is-listener-live)
    ::  +current-live-listeners: live listeners on :build
    ::
    ++  current-live-listeners  ^-  (list listener)
      (skim current-listeners is-listener-live)
    ::  +current-listeners: listeners on :build, both live and once
    ::
    ++  current-listeners  ^-  (list listener)
      ~(tap in (fall (~(get by listeners.state) build) ~))
    ::  +link-rebuilds: link old and new same build in :rebuilds.state
    ::
    ++  link-rebuilds  ^+  this
      ::
      ?<  ?=(~ previous-build)
      ::
      %_    this
          old.rebuilds.state
        (~(put by old.rebuilds.state) build u.previous-build)
      ::
          new.rebuilds.state
        (~(put by new.rebuilds.state) u.previous-build build)
      ==
    ::  update :dirty-discs.per-event to reflect the fact that :build is done
    ::
    ++  update-live-tracking  ^+  this
      ?.  ?=(%scry -.schematic.build)
        this
      ::
      =/  dependency  dependency.schematic.build
      ::
      ?.  ?=(%c -.dependency)
        this
      ::
      =/  disc=disc  disc.rail.dependency
      ::
      this(dirty-discs (~(put in dirty-discs) disc))
    ::  +delete-once-listeners: remove once listeners on :build from :state
    ::
    ++  delete-once-listeners  ^+  this
      %_    this
          state
        %+  roll  (root-once-listeners build)
        |=  [=listener accumulator=_state]
        =.  state  accumulator
        (remove-listener-from-build listener build)
      ==
    ::  +store-result: store :result in :state
    ::
    ++  store-result
      ::
      ?<  ?=(~ result)
      ::
      %_    this
          results.state
        ::
        %+  ~(put by results.state)  build
        [%result last-accessed=now build-result=u.result]
      ==
    ::  +cleanup: cleanup :build; wraps ^cleanup
    ::
    ++  cleanup
      |=  build=^build  ^+  this
      this(..execute (^cleanup build))
    ::
    ++  this  .
    --
  ::  +make: attempt to perform :build, non-recursively
  ::
  ::    Registers component linkages between :build and its sub-builds.
  ::    Attempts to perform +scry if necessary. Does not directly enqueue
  ::    any moves.
  ::
  ++  make
    |=  =build
    ^-  $:  ::  result: result of running a build
            ::
            $=  result
            $%  ::  %build-result: the build completed
                ::
                [%build-result =build-result]
                ::  %blocks: :build is waiting on other builds or a dependency
                ::
                [%blocks builds=(list ^build)]
            ==
            ::  possibly mutated version of the +per-event core
            ::
            _this
        ==
    ::  dispatch based on the kind of +schematic in :build
    ::
    |^  ?-    -.schematic.build
        ::
            ^  (autocons [head tail]:schematic.build)
        ::
            %$  (literal literal.schematic.build)
        ::
            %pin   (pin [date schematic]:schematic.build)
            %alts  !!
            %bake  !!
            %bunt  !!
            %call  !!
            %cast  !!
            %core  !!
            %diff  !!
            %dude  !!
            %hood  !!
            %join  !!
            %mash  !!
            %mute  !!
            %pact  !!
            %path  !!
            %plan  !!
            %reef  !!
            %ride  (ride [formula subject]:schematic.build)
            %same  (same schematic.schematic.build)
            %slit  !!
            %slim  (slim [subject-type formula]:schematic.build)
            %scry  (scry dependency.schematic.build)
            %vale  !!
            %volt  !!
        ==
    ::  |schematic-handlers:make: implementation of the schematics
    ::
    ::    All of these produce a value of the same type as +make itself.
    ::
    ::  +|  schematic-handlers
    ::
    ++  autocons
      |=  [head=schematic tail=schematic]
      ::
      =^  head-result  state  (depend-on head)
      =^  tail-result  state  (depend-on tail)
      ::
      =|  blocks=(list ^build)
      =?  blocks  ?=(~ head-result)  [[date.build head] blocks]
      =?  blocks  ?=(~ tail-result)  [[date.build tail] blocks]
      ::  if either build blocked, we're not done
      ::
      ?^  blocks
        ::
        [[%blocks blocks] this]
      ::
      ?<  ?=(~ head-result)
      ?<  ?=(~ tail-result)
      ::
      =-  [[%build-result -] this]
      `build-result`[%result u.head-result u.tail-result]
    ::
    ++  literal
      |=  =cage
      [[%build-result %result %$ cage] this]
    ::
    ++  pin
      |=  [date=@da =schematic]
      ::
      =^  result  state  (depend-on schematic)
      ::
      ?~  result
        [[%blocks [date schematic]~] this]
      [[%build-result %result %pin date u.result] this]
    ::
    ++  ride
      |=  [formula=hoon =schematic]
      ::
      =^  result  state  (depend-on schematic)
      ?~  result
        [[%blocks [date.build schematic]~] this]
      ::
      =*  subject  u.result
      =*  subject-cage  (result-to-cage subject)
      =/  slim-schematic=^schematic  [%slim p.q.subject-cage formula]
      =^  slim-result  state  (depend-on slim-schematic)
      ?~  slim-result
        [[%blocks [date.build slim-schematic]~] this]
      ::
      ?:  ?=(%error -.u.slim-result)
        :_  this
        [%build-result %error [%leaf "%ride: "] message.u.slim-result]
      ::
      ?>  ?=([%result %slim *] u.slim-result)
      ::
      =/  val
        (mock [q.q.subject-cage nock.u.slim-result] intercepted-scry)
      ::  val is a toon, which might be a list of blocks.
      ::
      ?-    -.val
      ::
          %0
        [[%build-result %result %ride [type.u.slim-result p.val]] this]
      ::
          %1
        =/  blocked-paths=(list path)  ((hard (list path)) p.val)
        ::
        =/  blocks-or-failures=(list (each ^build tank))
          %+  turn  blocked-paths
          |=  =path
          ::
          =/  scry-request=(unit scry-request)  (path-to-scry-request path)
          ?~  scry-request
            [%| [%leaf "ford: %slim: invalid scry path: {<path>}"]]
          ::
          =*  case  r.beam.u.scry-request
          ::
          ?.  ?=(%da -.case)
            [%| [%leaf "ford: %slim: invalid case in scry path: {<path>}"]]
          ::
          =/  date=@da  p.case
          ::
          =/  dependency=(unit dependency)  (path-to-dependency path)
          ?~  dependency
            :-  %|
            [%leaf "ford: %slim: invalid dependency in scry path: {<path>}"]
          ::
          =/  sub-schematic=^schematic  [%pin date %scry u.dependency]
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
          [[%build-result %error failed] this]
        ::  no failures
        ::
        =/  blocks=(list ^build)
          %+  turn  blocks-or-failures
          |=  block=(each ^build tank)
          ?>  ?=(%& -.block)
          ::
          p.block
        ::
        =.  state
          %+  roll  blocks
          |=  [block=^build accumulator=_state]
          =.  state  accumulator
          +:(depend-on schematic.block)
        ::
        [[%blocks blocks] this]
      ::
          %2
        =/  message=tang  [[%leaf "ford: %ride failed:"] p.val]
        [[%build-result %error message] this]
      ==
    ::
    ++  same
      |=  =schematic
      ::
      =^  result  state  (depend-on schematic)
      ::
      ?~  result
        [[%blocks [date.build schematic]~] this]
      [[%build-result %result %same u.result] this]
    ::
    ++  scry
      |=  =dependency
      ::  construct a full +beam to make the scry request
      ::
      =/  beam  (extract-beam dependency `date.build)
      ::  extract :disc from :beam
      ::
      =/  disc=disc  (extract-disc dependency)
      ::
      =/  is-live-clay=?  &((is-build-live build) ?=(%c -.dependency))
      ::  link :disc to :dependency
      ::
      =?    dependencies.state
          is-live-clay
        (~(put ju dependencies.state) [disc dependency])
      ::  update :latest-by-disc.state if :date.build is later
      ::
      =?    latest-by-disc.state
          ?&  is-live-clay
          ::
              =/  latest-date  (~(get by latest-by-disc.state) disc)
              ::
              ?|  ?=(~ latest-date)
                  (gth date.build u.latest-date)
          ==  ==
        ::
        (~(put by latest-by-disc.state) disc date.build)
      ::  mark :disc as dirty if we're building a live dependency
      ::
      =?    dirty-discs
          is-live-clay
        (~(put in dirty-discs) disc)
      ::  perform scry operation if we don't already know the result
      ::
      ::    Look up :dependency in :scry-results.per-event to avoid
      ::    rerunning a previously blocked +scry.
      ::
      =/  scry-response
        ?:  (~(has by scry-results) dependency)
          (~(get by scry-results) dependency)
        (^scry [%143 %noun] ~ `@tas`(cat 3 [vane care]:dependency) beam)
      ::  scry blocked
      ::
      ?~  scry-response
        ::  :build blocked on :dependency
        ::
        ::    Enqueue a request +move to fetch the blocked resource.
        ::    Link :block and :build in :blocks.state so we know
        ::    which build to rerun in a later event when we +unblock
        ::    on that +dependency.
        ::
        =/  already-blocked=?  (~(has by blocks.state) dependency)
        ::  store :dependency in persistent state
        ::
        =.  blocks.state  (~(put ju blocks.state) dependency build)
        ::
        ?:  already-blocked
          ::  this dependency was already blocked
          ::
          [[%blocks ~] this]
        ::  construct new :move to request blocked resource
        ::
        =/  wire=path
          (welp /(scot %p our)/dependency (dependency-to-path dependency))
        ::
        =/  note=note
          =,  rail.dependency
          :*  %c  %warp  sock=[our their=ship.disc]
              [desk.disc `[%sing care.dependency case=[%da date.build] spur]]
          ==
        ::
        =.  moves  [[duct=~ [%pass wire note]] moves]
        [[%blocks ~] this]
      ::  scry failed
      ::
      ?~  u.scry-response
        =/  error=tang
          :~  leaf+"scry failed for"
              leaf+"%c{(trip care.dependency)} {<(en-beam beam)>}"
          ==
        [[%build-result %error error] this]
      ::  scry succeeded
      ::
      [[%build-result %result %scry u.u.scry-response] this]
    ::
    ++  slim
      |=  [subject-type=type formula=hoon]
      ::
      =/  compiled=(each (pair type nock) tang)
        (mule |.((~(mint ut subject-type) [%noun formula])))
      ::
      ?-  -.compiled
        %|  [[%build-result %error [leaf+"%slim failed: " p.compiled]] this]
        %&  [[%build-result %result %slim p.compiled] this]
      ==
    ::  |utilities:make: helper arms
    ::
    ::+|  utilities
    ::
    ::  +depend-on: register component linkage between :build and :kid
    ::
    ++  depend-on
      |=  kid=schematic
      ^-  [(unit build-result) ford-state]
      ::
      =/  sub-build=^build  [date.build kid]
      ::
      =:
          builds-by-date.state
        (~(put ju builds-by-date.state) date.build kid)
      ::
          builds-by-schematic.state
        (~(put by-schematic builds-by-schematic.state) sub-build)
      ::
          sub-builds.components.state
        (~(put ju sub-builds.components.state) build sub-build)
      ::
          client-builds.components.state
        (~(put ju client-builds.components.state) sub-build build)
      ==
      ::
      =^  maybe-cache-line  results.state  (access-cache sub-build)
      ?~  maybe-cache-line
        [~ state]
      ::
      =*  cache-line  u.maybe-cache-line
      ?:  ?=(%tombstone -.cache-line)
        [~ state]
      ::
      [`build-result.cache-line state]
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
    =/  type=type  ((hard type) +.ref)
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
    =/  dependency=dependency
      [u.vane u.care rail=[[p.beam q.beam] s.beam]]
    ::  TODO: handle other kinds of +case
    ::
    =/  date=@da
      ~|  bad-case+r.beam
      ?>  ?=(%da -.r.beam)
      p.r.beam
    ::
    =/  build=build  [date %scry dependency]
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
    ?.  -:(nets:wa type `^type`p.q.local-cage)
      ~&  [%scry-nest-fail term=term beam=beam]
      [~ ~]
    ::
    [~ ~ `(cask)`local-cage]
  ::  +send-mades: send one %made move for :build per listener in :listeners
  ::
  ++  send-mades
    |=  [=build listeners=(list listener)]  ^+  this
    ::
    =^  result  results.state  (access-cache build)
    ::
    ?>  ?=([~ %result *] result)
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
  ::  +promote-live-listeners: move live listeners from :old to :new
  ::
  ++  promote-live-listeners
    |=  [old=build new=build]
    ^+  state
    ::
    =/  old-live-listeners=(list listener)
      =-  (skim - is-listener-live)
      =-  ~(tap in `(set listener)`(fall - ~))
      (~(get by listeners.state) old)
    ::
    =.  state
      %+  roll  old-live-listeners
      |=  [=listener state=_state]
      ::  if :listener ain't live, we wrote this wrong
      ::
      ?>  live.listener
      ::  move :listener off :previous-build onto :build
      ::
      %_    state
          listeners
        =-  (~(put ju -) new listener)
        (~(del ju listeners.state) old listener)
      ::
          builds-by-listener
        (~(put by builds-by-listener.state) duct.listener [new &])
      ==
    ::
    %+  roll  ~(tap in (fall (~(get by root-builds.state) old) ~))
    |=  [=listener state=_state]
    ::
    =?    root-builds.state
        (is-listener-live listener)
      ::
      =-  (~(put ju -) new listener)
      (~(del ju root-builds.state) old listener)
    ::
    state
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
    (~(get by root-builds.state) build)
  ::  +is-build-live: whether this is a live or a once build
  ::
  ++  is-build-live
    |=  =build  ^-  ?
    ::
    ?:  ?=(%pin -.schematic.build)
      %.n
    =/  has-pinned-client
      ::  iterate across all clients recursively, exiting early on %pin
      ::
      =/  clients  ~(tap in (~(get ju client-builds.components.state) build))
      |-
      ?~  clients
        %.n
      ?:  ?=(%pin -.schematic.i.clients)
        %.y
      %_    $
          clients
        %+  weld  t.clients
        ~(tap in (~(get ju client-builds.components.state) i.clients))
      ==
    ?:  has-pinned-client
      %.n
    ::  check if :build has any live listeners
    ::
    =/  listeners  ~(tap in (fall (~(get by listeners.state) build) ~))
    ?~  listeners
      %.y
    (lien `(list listener)`listeners is-listener-live)
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
  ::  +finalize: convert per-event state to moves and per sistent state
  ::
  ::    Converts :done-live-roots to %made +move's, performs +duct
  ::    accounting, and runs +cleanup on completed once builds and
  ::    stale live builds.
  ::
  ::    TODO: needs rework to support live builds
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
    ::  dependencies: all dependencies on :disc
    ::
    =/  dependencies=(set dependency)
      (fall (~(get by dependencies.state) disc) ~)
    ::  if no dependencies on :disc, don't make a new clay subscription
    ::
    ?~  dependencies
      ::  cancel clay subscriptions when we don't have any dependencies left
      ::
      ?:  (~(has in original-clay-subscriptions) disc)
        =+  [their desk]=disc
        =/  note=note
          :^  %c  %warp  sock=[our their]
          ^-  riff:clay
          [desk ~]
        ::
        =.  moves  :_  moves
          ^-  move
          [duct=~ [%pass wire=(clay-sub-wire disc) note]]
        ::
        =.  clay-subscriptions.state  (~(del in clay-subscriptions.state) disc)
        ::
        =.  latest-by-disc.state  (~(del by latest-by-disc.state) disc)
        ::
        $(discs t.discs)
      ::
      $(discs t.discs)
    ::  prevent thrashing; don't unsubscribe then immediately resubscribe
    ::
    ::    When we send a request to a foreign ship, that ship may have
    ::    started responding before we send a cancellation. In that case,
    ::    cancelling and then resubscribing might cause the foreign ship
    ::    to send the response twice, which would be extra network traffic.
    ::
    ?:  ?&  (~(has in original-clay-subscriptions) disc)
        ::
            (~(has in clay-subscriptions.state) disc)
        ::
            .=  (~(get by original-dependencies) disc)
            (~(get by dependencies.state) disc)
        ==
      ::
      $(discs t.discs)
    ::  request-contents: the set of [care path]s to subscribe to in clay
    ::
    =/  request-contents=(set [care:clay path])
      %-  sy  ^-  (list [care:clay path])
      %+  murn  ~(tap in `(set dependency)`dependencies)
      |=  =dependency  ^-  (unit [care:clay path])
      ::
      ?.  ?=(%c -.dependency)  ~
      ::
      `[care.dependency (flop spur.rail.dependency)]
    ::  if :request-contents is `~`, this code is incorrect
    ::
    ?<  ?=(~ request-contents)
    ::  their: requestee +ship
    ::
    =+  [their desk]=disc
    =/  latest-date  (~(got by latest-by-disc.state) disc)
    ::
    =/  note=note
      :^  %c  %warp  sock=[our their]
      ^-  riff:clay
      [desk `[%mult case=[%da latest-date] request-contents]]
    ::
    =.  moves  :_  moves
      ^-  move
      [duct=~ [%pass wire=(clay-sub-wire disc) note]]
    ::
    =.  clay-subscriptions.state  (~(put in clay-subscriptions.state) disc)
    ::
    $(discs t.discs)
  ::  +cleanup: try to clean up a build and its sub-builds
  ::
  ++  cleanup
    |=  =build
    ^+  this
    ::
    ::  if something depends on this build, no-op and return
    ::
    ?:  ?|  (~(has by client-builds.components.state) build)
            (~(has by old.rebuilds.state) build)
            (~(has by listeners.state) build)
        ==
      this
    ::  remove :build from :state, starting with its cache line
    ::
    =.  results.state  (~(del by results.state) build)
    ::  remove :date.build from list of dates for this schematic
    ::
    =.  builds-by-schematic.state
      (~(del by-schematic builds-by-schematic.state) build)
    ::  remove :build from :builds-by-date
    ::
    =.  builds-by-date.state
      (~(del ju builds-by-date.state) date.build schematic.build)
    ::  if no more builds at this date, remove the date from :dependency-updates
    ::
    =?    dependency-updates.state
        !(~(has by builds-by-date.state) date.build)
      (~(del by dependency-updates.state) date.build)
    ::
    =?    blocks.state
        ::
        ?=(%scry -.schematic.build)
      ::
      =*  dependency  dependency.schematic.build
      ::
      (~(del ju blocks.state) dependency build)
    ::  check if :build depends on a live clay +dependency
    ::
    =/  has-live-dependency  ?=([%scry %c *] schematic.build)
    ::  clean up dependency tracking and maybe cancel clay subscription
    ::
    =?  this  has-live-dependency
      ::  type system didn't know, so tell it again
      ::
      ?>  ?=([%scry %c *] schematic.build)
      ::
      =/  dependency  dependency.schematic.build
      =/  disc=disc  (extract-disc dependency)
      ::
      =/  should-delete-dependency=?
        ::  checks if there are other live builds of this dependency
        ::
        =/  dates=(list @da)
          (fall (~(get by builds-by-schematic.state) schematic.build) ~)
        ?!
        %+  lien  dates
        |=  date=@da
        ^-  ?
        =/  other-build  [date schematic.build]
        =/  listeners=(set listener)
          (fall (~(get by root-builds.state) other-build) ~)
        ::
        (lien ~(tap in listeners) is-listener-live)
      ::
      =?  dependencies.state  should-delete-dependency
        (~(del ju dependencies.state) disc dependency)
      ::
      =?  dirty-discs  should-delete-dependency
        (~(put in dirty-discs) disc)
      ::
      this
    ::  kids: :build's sub-builds
    ::
    =/  kids=(list ^build)
      ~(tap in (~(get ju sub-builds.components.state) build))
    ::  remove the mapping from :build to its sub-builds
    ::
    =.  sub-builds.components.state
      (~(del by sub-builds.components.state) build)
    ::  for each +build in :kids, remove :build from its clients
    ::
    =.  client-builds.components.state
      %+  roll  kids
      |=  [kid=^build clients=_client-builds.components.state]
      (~(del ju clients) kid build)
    ::  if there is a newer rebuild of :build, delete the linkage
    ::
    =/  rebuild  (~(get by new.rebuilds.state) build)
    =?  rebuilds.state  ?=(^ rebuild)
      %_  rebuilds.state
        new  (~(del by new.rebuilds.state) build)
        old  (~(del by old.rebuilds.state) build)
      ==
    ::  recurse on :kids
    ::
    =.  this
      %+  roll  kids
      |=  [kid=^build that=_this]
      (cleanup:that kid)
    ::  recurse on :rebuild; note this must be done after recursing on :kids
    ::
    ?~  rebuild
      this
    (cleanup u.rebuild)
  ::  +clay-sub-wire: the wire to use for a clay subscription
  ::
  ++  clay-sub-wire
    |=  =disc  ^-  wire
    ::
    =+  [their desk]=disc
    ::
    /(scot %p our)/clay-sub/(scot %p their)/[desk]
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
=|  axle
|=  [now=@da eny=@ scry=sley]
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
  ^-  [(list move) _this]
  ::  unwrap :task from :wrapped-task
  ::
  =/  task=task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ((hard task:able) p.wrapped-task)
  ::
  ?-    -.task
      ::  %make: request to perform a build
      ::
      %make
   ::  perform the build indicated by :task
   ::
   ::    First, we find or create the :ship-state for :our.task,
   ::    modifying :state-by-ship as necessary. Then we dispatch to the |ev
   ::    by constructing :event-args and using them to create :start-build,
   ::    which performs the build. The result of :start-build is a pair of
   ::    :moves and a mutant :ship-state. We update our :state-by-ship map
   ::    with the new :ship-state and produce it along with :moves.
   ::
   =^  ship-state  state-by-ship  (find-or-create-ship-state our.task)
   =*  event-args  [[our.task duct now scry] ship-state]
   =*  start-build  start-build:(per-event event-args)
   =^  moves  ship-state  (start-build schematic.task)
   =.  state-by-ship  (~(put by state-by-ship) our.task ship-state)
   ::
   [moves this]
  ::
      ::  %kill: cancel a %make
      ::
      %kill
    ::
    =/  ship-state  ~|(our+our.task (~(got by state-by-ship) our.task))
    =*  event-args  [[our.task duct now scry] ship-state]
    =^  moves  ship-state  cancel:(per-event event-args)
    =.  state-by-ship  (~(put by state-by-ship) our.task ship-state)
    ::
    [moves this]
  ::
      %wipe  !!
  ::
      %wegh  !!
  ==
::  +take: receive a response from another vane
::
++  take
  |=  [=wire =duct wrapped-sign=(hypo sign)]
  ^-  [(list move) _this]
  ::  unwrap :sign from :wrapped-sign
  ::
  ::    TODO: verify wrapped-sign isn't an evil vase?
  ::
  =/  sign=sign  q.wrapped-sign
  ::  TODO: support other responses
  ::
  ::  parse :wire into :our, :ship-state, and :dependency
  ::
  ?>  ?=([@ @ *] wire)
  ::  we know :our is already in :state-by-ship because we sent this request
  ::
  =/  our=@p  (slav %p i.wire)
  =/  ship-state  ~|(our+our (~(got by state-by-ship) our))
  =*  event-args  [[our duct now scry] ship-state]
  ::  %clay-sub: response to a clay %mult subscription
  ::
  =^  moves  ship-state
    ?:  =(%clay-sub i.t.wire)
      ::
      ?>  ?=([%c %wris *] sign)
      =+  [ship desk]=(raid:wired t.t.wire ~[%p %tas])
      ::
      =*  rebuild  rebuild:(per-event event-args)
      (rebuild [ship desk case.sign] care-paths.sign)
    ::  %dependency: response to a request for a +dependency
    ::
    ?.  =(%dependency i.t.wire)
      ::
      ~|(unknown-take+i.t.wire !!)
    ::
    ?>  ?=([%c %writ *] sign)
    ::  dependency: the +dependency we had previously blocked on
    ::
    =/  dependency
      ~|  [%bad-dependency wire]
      (need (path-to-dependency t.t.wire))
    ::  scry-result: parse a (unit cage) from :sign
    ::
    ::    If the result is `~`, the requested resource was not available.
    ::
    =/  scry-result=(unit cage)
      ?~  riot.sign
        ~
      `r.u.riot.sign
    ::  unblock the builds that had blocked on :dependency
    ::
    =*  unblock  unblock:(per-event event-args)
    (unblock dependency scry-result)
  ::
  =.  state-by-ship  (~(put by state-by-ship) our ship-state)
  ::
  [moves this]
::  %utilities
::
::+|
::
++  this  .
::  +find-or-create-ship-state: find or create a ford-state for a @p
::
::    Accesses and modifies :state-by-ship.
::
++  find-or-create-ship-state
  |=  our=@p
  ^-  [ford-state _state-by-ship]
  ::
  =/  existing  (~(get by state-by-ship) our)
  ?^  existing
    [u.existing state-by-ship]
  ::
  =|  new-state=ford-state
  [new-state (~(put by state-by-ship) our new-state)]
--
