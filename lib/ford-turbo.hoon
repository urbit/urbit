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
  ::  +resource: time-varying dependency on a value from the urbit namespace
  ::
  +=  resource
    $:  ::  vane which we are querying
        ::
        vane=?(%c %g)
        ::  type of request
        ::
        ::    TODO: care:clay should be cleaned up in zuse as it is a general
        ::    type, not a clay specific one.
        ::
        care=care:clay
        ::  path on which to depend, missing time, which will be filled in
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
        ::  %success: result of successful +build, tagged by +schematic sub-type
        ::
        $:  %success
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
            ::  resource: a namespace request, with unspecified time
            ::
            ::    Schematics can only be resolved when specifying a time,
            ::    which will convert this +resource into a +scry-request.
            ::
            =resource
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
      blocks=(jug resource build)
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
      clay-subscriptions=(set disc)
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
              ::  scry-blocked: resource that :build blocked on
              ::
              scry-blocked=(unit resource)
          ==
      ==
      ::  sub-builds: subbuilds of :build
      ::
      ::    While running +make on :build, we need to keep track of any
      ::    sub-builds that we try to access so we can keep track of
      ::    component linkages and cache access times.
      ::
      sub-builds=(list build)
      ::  clear-sub-builds: replace previous sub-builds with :sub-builds
      ::
      ::    Some schematics will completely rerun their resource registration,
      ::    but others will need to add to their previous resources.
      ::
      clear-sub-builds=?
  ==
::  +vane: short names for vanes
::
::    TODO: move to zuse
::
+=  vane  ?(%a %b %c %d %e %f %g)
--
=,  format
|%
::  +build-to-tape: convert :build to a printable format
::
++  build-to-tape
  |=  =build
  ^-  tape
  ::
  =/  enclose  |=(tape "[{+<}]")
  =/  date=@da  date.build
  =/  schematic=schematic  schematic.build
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
      %scry
    (spud (en-beam (extract-beam resource.schematic ~)))
  ::
    ::    %slim
    ::  "slim {<subject-type.schematic>} {<formula.schematic>}"
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
::  +resource-to-path: encode a +resource in a +wire
::
::    If :resource is live, create a +beam from :rail.resource
::    by using revision 0 as the +case and encode that.
::
++  resource-to-path
  |=  =resource
  ^-  path
  =/  term=term  (cat 3 [vane care]:resource)
  [term (en-beam (extract-beam resource date=~))]
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
::  +extract-beam: obtain a +beam from a +resource
::
::    Fills case with [%ud 0] for live resources if :date is `~`.
::    For once resources, ignore :date.
::
++  extract-beam
  |=  [=resource date=(unit @da)]  ^-  beam
  ::
  =/  case=case  ?~(date [%ud 0] [%da u.date])
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
  =|  scry-results=(map resource (unit cage))
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
      =/  build=build  [now schematic]
      ::
      =.  state  (associate-build build duct %.y)
      ::
      (execute-loop (sy build ~))
    ::
    ++  start-once-build
      ^+  this
      =/  pin-date=@da  (date-from-schematic schematic)
      =/  build=build  [pin-date schematic]
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
    |=  [=resource scry-result=(unit cage)]
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
    =.  scry-results  (~(put by scry-results) resource scry-result)
    ::  find all the :blocked-builds to continue
    ::
    =/  blocked-builds  (~(get ju blocks.state) resource)
    ::
    (execute-loop blocked-builds)
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
        =-  ~(tap in -)
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
      ::    or promote it using an old build. It also might add this builds
      ::    sub-builds to :candidate-builds.
      ::
      ++  gather-build
        |=  =build
        ^+  ..execute
        ::  normalize :date.build for a %pin schematic
        ::
        =?  date.build  ?=(%pin -.schematic.build)  date.schematic.build
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
        ::  clear the components
        ::
        =?    ..execute
            clear-sub-builds.made
          (unlink-sub-builds build.made)
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
        =/  disc=disc  (extract-disc resource)
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
                clear-sub-builds=?
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
          %+  turn
            %+  weld
              (~(get-clients by-build-dag components.state) previous-build)
            ::
            =/  older-build  (~(get by old.rebuilds.state) previous-build)
            ?~  older-build
              ~
            ::
            (~(get-clients by-build-dag components.state) u.older-build)
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
            =-  ~(tap in -)
            ::  omit :build; it's all *other* client listeners
            ::
            =-  (~(del in -) build)
            =-  (fall - ~)
            (~(get by client-builds.provisional-components.state) sub-build)
          |=  [build=^build listeners=(set listener)]
          ::
          %-  ~(uni in listeners)
          (fall (~(get by listeners.state) build) ~)
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
        =.  state
          %+  roll  ~(tap in orphaned-listeners)
          |=  [=listener accumulator=_state]
          =.  state  accumulator
          ::
          (remove-listener-from-build listener sub-build)
        ::  remove the orphaned build from provisional builds
        ::
        =.  provisional-components.state
          (~(del by-build-dag provisional-components.state) build sub-build)
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
                    scry-blocked=(unit resource)
                ==
                sub-builds=(list build)
            ==
        ^+  ..execute
        ::  if we scryed, send clay a request for the path we blocked on reading
        ::
        =?    moves
            ?=(^ scry-blocked)
          ::
          =*  resource  u.scry-blocked
          ::  TODO: handle other vanes
          ::
          ?>  ?=(%c vane.resource)
          ::
          [(clay-request-for-resource date.build resource) moves]
        ::  register resource block in :blocks.state
        ::
        =?    blocks.state
            ?=(^ scry-blocked)
          ::
          ?>  ?=(%scry -.schematic.build)
          =*  resource  resource.schematic.build
          ::
          (~(put ju blocks.state) resource build)
        ::  register blocks on sub-builds in :blocked-builds.state
        ::
        =.  state  (register-sub-build-blocks build blocks)
        ::
        ..execute
      ::  +clay-request-for-resource: new move to request blocked resource
      ::
      ++  clay-request-for-resource
        |=  [date=@da =resource]
        ^-  move
        ::
        =/  wire=path
          (welp /(scot %p our)/resource (resource-to-path resource))
        ::
        =/  note=note
          =/  disc=disc  (extract-disc resource)
          =,  rail.resource
          :*  %c  %warp  sock=[our their=ship.disc]  desk.disc
              `[%sing care.resource case=[%da date] spur]
          ==
        ::
        [duct=~ [%pass wire note]]
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
        ::  deal with block already being unblocked
        ::
        ::    If :block was run in the same batch as :build.made, and we've
        ::    already processed its result, then :build.made has already
        ::    been unblocked. Don't reblock ourselves since nothing will
        ::    unblock us.
        ::
        ?:  (~(has by results.state) block)
          state
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
      =/  resource  resource.schematic.build
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
    ::  dispatch based on the kind of +schematic in :build
    ::
    |^  ?-    -.schematic.build
        ::
            ^  (autocons [head tail]:schematic.build)
        ::
            %$  (literal literal.schematic.build)
        ::
            %pin   (pin [date schematic]:schematic.build)
            %alts  (alts choices.schematic.build)
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
            %scry  (scry resource.schematic.build)
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
      ^-  build-receipt
      ::
      =^  head-result  accessed-builds  (depend-on head)
      =^  tail-result  accessed-builds  (depend-on tail)
      ::
      =|  blocks=(list ^build)
      =?  blocks  ?=(~ head-result)  [[date.build head] blocks]
      =?  blocks  ?=(~ tail-result)  [[date.build tail] blocks]
      ::  if either build blocked, we're not done
      ::
      ?^  blocks
        ::
        [build [%blocks blocks ~] accessed-builds |]
      ::
      ?<  ?=(~ head-result)
      ?<  ?=(~ tail-result)
      ::
      =-  [build [%build-result -] accessed-builds |]
      `build-result`[%success u.head-result u.tail-result]
    ::
    ++  literal
      |=  =cage
      ^-  build-receipt
      [build [%build-result %success %$ cage] accessed-builds |]
    ::
    ++  pin
      |=  [date=@da =schematic]
      ^-  build-receipt
      ::
      =^  result  accessed-builds  (depend-on schematic)
      ::
      ?~  result
        [build [%blocks [date schematic]~ ~] accessed-builds |]
      [build [%build-result %success %pin date u.result] accessed-builds |]
    ::
    ++  alts
      |=  choices=(list schematic)
      ^-  build-receipt
      ::
      ?~  choices
        :*  build
            [%build-result %error [leaf+"%alts: all options failed"]~]
            accessed-builds
            &
        ==
      ::
      =^  result  accessed-builds  (depend-on i.choices)
      ?~  result
        [build [%blocks [date.build i.choices]~ ~] accessed-builds &]
      ::
      ?:  ?=([%error *] u.result)
        $(choices t.choices)
      ::
      [build [%build-result %success %alts u.result] accessed-builds &]
    ::
    ++  ride
      |=  [formula=hoon =schematic]
      ^-  build-receipt
      ::
      =^  result  accessed-builds  (depend-on schematic)
      ?~  result
        [build [%blocks [date.build schematic]~ ~] accessed-builds |]
      ::
      =*  subject  u.result
      =*  subject-cage  (result-to-cage subject)
      =/  slim-schematic=^schematic  [%slim p.q.subject-cage formula]
      =^  slim-result  accessed-builds  (depend-on slim-schematic)
      ?~  slim-result
        [build [%blocks [date.build slim-schematic]~ ~] accessed-builds |]
      ::
      ?:  ?=(%error -.u.slim-result)
        :*  build
            [%build-result %error [%leaf "%ride: "] message.u.slim-result]
            accessed-builds
            |
        ==
      ::
      ?>  ?=([%success %slim *] u.slim-result)
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
            |
        ==
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
          =/  resource=(unit resource)  (path-to-resource path)
          ?~  resource
            :-  %|
            [%leaf "ford: %slim: invalid resource in scry path: {<path>}"]
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
          [build [%build-result %error failed] accessed-builds |]
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
          +:(depend-on schematic.block)
        ::
        ::  TODO: Here we are passing a single ~ for :scry-blocked. Should we
        ::  be passing one or multiple resource back instead? Maybe not? Are
        ::  we building blocking schematics, which they themselves will scry?
        ::
        [build [%blocks blocks ~] accessed-builds |]
      ::
          %2
        =/  message=tang  [[%leaf "ford: %ride failed:"] p.val]
        [build [%build-result %error message] accessed-builds |]
      ==
    ::
    ++  same
      |=  =schematic
      ^-  build-receipt
      ::
      =^  result  accessed-builds  (depend-on schematic)
      ::
      ?~  result
        [build [%blocks [date.build schematic]~ ~] accessed-builds |]
      [build [%build-result %success %same u.result] accessed-builds |]
    ::
    ++  scry
      ::  TODO: All accesses to :state which matter happens in this function;
      ::  those calculations need to be lifted out of +make into +execute.
      ::
      |=  =resource
      ^-  build-receipt
      ::  construct a full +beam to make the scry request
      ::
      =/  beam  (extract-beam resource `date.build)
      ::  perform scry operation if we don't already know the result
      ::
      ::    Look up :resource in :scry-results.per-event to avoid
      ::    rerunning a previously blocked +scry.
      ::
      =/  scry-response
        ?:  (~(has by scry-results) resource)
          (~(get by scry-results) resource)
        (^scry [%143 %noun] ~ `@tas`(cat 3 [vane care]:resource) beam)
      ::  scry blocked
      ::
      ?~  scry-response
        ::  :build blocked on :resource
        ::
        ::    Enqueue a request +move to fetch the blocked resource.
        ::    Link :block and :build in :blocks.state so we know
        ::    which build to rerun in a later event when we +unblock
        ::    on that +resource.
        ::
        =/  already-blocked=?  (~(has by blocks.state) resource)
        ::  store :resource in persistent state
        ::
        =.  blocks.state  (~(put ju blocks.state) resource build)
        ::
        ?:  already-blocked
          ::  this resource was already blocked, so don't duplicate move
          ::
          [build [%blocks ~ ~] accessed-builds |]
        ::
        [build [%blocks ~ `resource] accessed-builds |]
      ::  scry failed
      ::
      ?~  u.scry-response
        =/  error=tang
          :~  leaf+"scry failed for"
              leaf+"%c{(trip care.resource)} {<(en-beam beam)>}"
          ==
        [build [%build-result %error error] accessed-builds |]
      ::  scry succeeded
      ::
      [build [%build-result %success %scry u.u.scry-response] accessed-builds |]
    ::
    ++  slim
      |=  [subject-type=type formula=hoon]
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
          |
      ==
    ::  |utilities:make: helper arms
    ::
    ::+|  utilities
    ::
    ++  depend-on
      |=  kid=schematic
      ^-  [(unit build-result) _accessed-builds]
      =/  sub-build=^build  [date.build kid]
      ::
      =.  accessed-builds  [sub-build accessed-builds]
      ::  +access-cache will mutate :results.state
      ::
      ::    It's okay to ignore this because the accessed-builds get gathered
      ::    and merged during the +reduce step.
      ::
      =/  maybe-cache-line  -:(access-cache sub-build)
      ?~  maybe-cache-line
        [~ accessed-builds]
      ::
      =*  cache-line  u.maybe-cache-line
      ?:  ?=(%tombstone -.cache-line)
        [~ accessed-builds]
      ::
      [`build-result.cache-line accessed-builds]
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
    =/  resource=resource
      [u.vane u.care rail=[[p.beam q.beam] s.beam]]
    ::  TODO: handle other kinds of +case
    ::
    =/  date=@da
      ~|  bad-case+r.beam
      ?>  ?=(%da -.r.beam)
      p.r.beam
    ::
    =/  build=build  [date %scry resource]
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
      =-  ~(tap in -)
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
    ::  if no resources on :disc, don't make a new clay subscription
    ::
    ?~  resources
      ::  cancel clay subscriptions when we don't have any resources left
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
    ::    canceling and then resubscribing might cause the foreign ship
    ::    to send the response twice, which would be extra network traffic.
    ::
    ?:  ?&  (~(has in original-clay-subscriptions) disc)
        ::
            (~(has in clay-subscriptions.state) disc)
        ::
            .=  (~(get by original-resources-by-disc) disc)
            (~(get by resources-by-disc.state) disc)
        ==
      ::
      $(discs t.discs)
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
      =*  resource  resource.schematic.build
      ::
      (~(del ju blocks.state) resource build)
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
      =/  resource  resource.schematic.build
      =/  disc=disc  (extract-disc resource)
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
  ::  parse :wire into :our, :ship-state, and :resource
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
    ::  %resource: response to a request for a +resource
    ::
    ?.  =(%resource i.t.wire)
      ::
      ~|(unknown-take+i.t.wire !!)
    ::
    ?>  ?=([%c %writ *] sign)
    ::  resource: the +resource we had previously blocked on
    ::
    =/  resource
      ~|  [%bad-resource wire]
      (need (path-to-resource t.t.wire))
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
    (unblock resource scry-result)
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
