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
              ::  date: the formal date of the build, or ~ for live
              ::
              date=(unit @da)
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
  +=  disc  (pair ship desk)
  ::  +view: a possibly time-varying view of a +disc
  ::
  ::    Either it contains a +case, which means it's pinned to a time,
  ::    or the (unit case) is ~, meaning the time can vary.
  ::
  +=  view  (trel ship desk (unit case))
  ::  +rail: a possibly time-varying full path
  ::
  ::    Either its +view contains a +case, which means it's pinned to a time
  ::    (in which case it's equivalent to a +beam), or the (unit case) is ~,
  ::    meaning the time can vary.
  ::
  +=  rail  (pair view spur)
  ::  +dependency: dependency on a value from the urbit namespace
  ::
  +=  dependency
    $%  ::  live dependency on a clay path; varies with time
        ::
        [%clay-live care=care:clay bel=(pair disc spur)]
        ::  once dependency on a clay path; pinned time
        ::
        [%clay-once care=care:clay =beam]
        ::  live dependency on a gall path; varies with time
        ::
        [%gall-live care=care:clay bel=(pair disc spur)]
        ::  once dependency on a gall path; pinned time
        ::
        [%gall-once care=care:clay =beam]
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
            $%  [%$ cage]
                [%alts build-result]
                [%bake cage]
                [%bunt cage]
                [%call vase]
                [%cast cage]
                [%core vase]
                [%diff cage]
                [%dude build-result]
                [%file cage]
                [%hood scaffold]
                [%join cage]
                [%mash cage]
                [%mute cage]
                [%pact cage]
                [%path (pair disc spur)]
                [%plan vase]
                [%reef vase]
                [%ride vase]
                [%slit type]
                [%slim (pair type nock)]
                [%scry cage]
                [%vale cage]
                [%volt cage]
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
            ::  location: where in clay to load the mark from
            ::
            location=view
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
            ::  location: where in clay to load the mark from
            ::
            location=view
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
            ::  location: where in clay to load the mark from
            ::
            location=view
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
            ::  location: where in clay to load the mark from
            ::
            location=view
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
            ::  location: where in clay to load the mark from
            ::
            location=view
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
            ::  location: where in clay to load the mark from
            ::
            location=view
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
            ::  location: the +disc within which to resolve :file-path
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
            ::  location: where in clay to load the mark from
            ::
            location=view
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
            ::  location: where in clay to load the mark from
            ::
            location=view
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
        structures/(list cable)
        ::  libraries: files from %/lib which are included
        ::
        libraries/(list cable)
        ::  cranes: a list of resources to transform and include
        ::
        cranes/(list crane)
        ::  sources: hoon sources, either parsed or on the filesystem
        ::
        sources/(list brick)
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
    $:  pre/(unit tyke)
        pof/(unit {p/@ud q/tyke})
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
++  move
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
              ::  date: the formal date of the build, or ~ for live
              ::
              date=(unit @da)
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
      builds-by-listener=(map duct build)
  ::
  ::  update tracking
  ::
      ::  live-leaf-builds: which live builds depend on a live +dependency
      ::
      live-leaf-builds=(jug dependency build)
      ::  live-root-builds: which live builds depend on any files in a +disc
      ::
      live-root-builds=(jug disc build)
      ::  dependencies: dependencies of a live build
      ::
      dependencies=(map build (jug disc dependency))
      ::  dependency-updates: all clay updates we need to know about
      ::
      ::    dependency-updates stores all Clay changes at dates that
      ::    Ford needs to track because Ford is tracking attempted builds with
      ::    that formal date. The changed dependencies are grouped first by
      ::    date, then within a single date, they're grouped by +disc.
      ::
      dependency-updates=(map @da (jug disc dependency))
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
::  +make-product: the result of running +make
::
+=  make-product
  $:  ::  result: result of running a build
      ::
      $=  result
      $%  ::  %build-result: the build completed
          ::
          [%build-result =build-result]
          ::  %blocks: the build is waiting on something else
          ::
          [%blocks blocks=(set block)]
      ==
      ::  possibly mutated version of the rest of the persistent state
      ::
      ford-state
  ==
::  +block: something a build can get stuck on
::
+=  block
  $%  ::  %build: the build blocked on another build, :build
      ::
      [%build =build]
      ::  %dependency: the build blocked on an external :dependency
      ::
      [%dependency =dependency]
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
::  +vane: short names for vanes
::
::    TODO: move to zuse
::
+=  vane  ?(%a %b %c %d %e %f %g)
--
=,  format
|%
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
::  +to-wire: encode a +dependency in a +wire
::
::    If :dependency is live, create a +beam from :bel.dependency
::    by using revision 0 as the +case and encode that.
::
++  to-wire
  |=  =dependency
  ^-  wire
  |^  ?-    -.dependency
          ::
          %clay-live
        ::
        =/  beam=beam
          [[ship=p.p.bel desk=q.p.bel case=[%ud 0]] spur=q.bel]:dependency
        ::
        (encode %c care.dependency & beam)
      ::
          %clay-once
        ::
        (encode %c care.dependency | beam.dependency)
      ::
          %gall-live
        =/  beam=beam
          [[ship=p.p.bel desk=q.p.bel case=[%ud 0]] spur=q.bel]:dependency
        ::
        (encode %g care.dependency & beam)
      ::
          %gall-once
        ::
        (encode %g care.dependency | beam.dependency)
      ==
  ::  +encode:to-wire: encode :vane, :care, :live, and :beam into a +wire
  ::
  ++  encode
    |=  [vane=?(%c %g) care=care:clay live=? =beam]
    ^-  wire
    ::
    [vane care (scot %f live) (en-beam beam)]
  --
::  +from-wire: decode a +dependency from a +wire
::
++  from-wire
  |=  =wire
  ^-  dependency
  ::
  ?>  ?=([@ @ @ *] wire)
  ::  parse :wire's components into :vane, :care, :live, and :beam
  ::
  =/  vane  ((hard ?(%c %g)) i.wire)
  =/  care  ((hard care:clay) i.t.wire)
  =/  live  =(0 (slav %f i.t.t.wire))
  =/  beam  (need (de-beam ((hard ^wire) t.t.t.wire)))
  ::
  ?.  live
    ?:  =(%c vane)
      [%clay-once care beam]
    [%gall-once care beam]
  ::
  =/  bel  [disc=[p.beam q.beam] spur=s.beam]
  ::
  ?:  =(%c vane)
    [%clay-live care bel]
  [%gall-live care bel]
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
  ::  done-live-roots: live root builds completed in this event, reversed
  ::
  =|  done-live-roots=(list build)
  ::  scry-results: responses to scry's to handle in this event
  ::
  ::    If a value is `~`, the requested resource is not available.
  ::    Otherwise, the value will contain a +cage.
  ::
  =|  scry-results=(map dependency (unit cage))
  ::  the +per-event door; each event will have a different sample
  ::
  |_  [[our=@p =duct now=@da scry=sley] state=ford-state]
  ::  |entry-points: externally fired arms
  ::
  ::+|  entry-points
  ::
  ::  +start-build: perform a fresh +build, either live or once
  ::
  ++  start-build
    |=  [=schematic date=(unit @da)]
    ^-  [(list move) ford-state]
    ::
    =<  finalize
    ::
    =+  [live when]=?~(date [& now] [| u.date])
    =/  build=build  [when schematic]
    ::  associate +listener with :build in :state
    ::
    =:  listeners.state
      (~(put ju listeners.state) build [duct live])
    ::
        builds-by-listener.state
      (~(put by builds-by-listener.state) duct build)
    ==
    ::
    (execute build)
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
    ::  store changed dependencies persistently in case rebuilds finish later
    ::
    =.  dependency-updates.state
      %+  roll  ~(tap in care-paths)
      |=  [[care=care:clay =path] dependency-updates=_dependency-updates.state]
      ::
      =/  dependency=dependency  [%clay-live care bel=[disc spur=path]]
      ::
      =-  (~(put by dependency-updates) date -)
      =-  (~(put ju -) disc dependency)
      (fall (~(get by dependency-updates) date) ~)
    ::
    =/  dependencies  (~(got by (~(got by dependency-updates.state) date)) disc)
    ::
    =/  builds-to-rebuild=(set build)
      %+  roll  ~(tap in dependencies)
      |=  [=dependency builds-to-rebuild=(set build)]
      ::
      %-  ~(uni in builds-to-rebuild)
      (fall (~(get by live-leaf-builds.state) dependency) ~)
    ::  rebuild :builds-to-rebuild at the new :date
    ::
    %+  roll  ~(tap in builds-to-rebuild)
    |=  [old-build=build that=_this]
    =/  new-build=build  [date schematic.old-build]
    (execute:that new-build)
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
    (execute:that build)
  ::
  ++  cancel  !!
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
    |=  =build
    ^+  this
    ::
    ::  if the build is complete, we're done
    ::
    ::    TODO: rebuild if %tombstone
    ::
    ::  TODO replace with arm that updates :last-accessed
    ?^  (~(get by results.state) build)
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
    ::
    =/  previous-build
      (~(find-previous by-schematic builds-by-schematic.state) build)
    ::
    ::  TODO use arm that updates :last-accessed
    =/  previous-result
      ?~  previous-build  ~
      (~(get by results.state) u.previous-build)
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
    ?.  |(sub-builds-changed dependencies-changed)
      ::  copy :previous-result to new date
      ::
      ?>  ?=([~ %result *] previous-result)
      ::
      =.  result  `build-result.u.previous-result
      ::
      =~  store-result
          update-live-tracking
          promote-live-listeners
          link-rebuilds
          (send-mades current-once-listeners)
          delete-once-listeners
      ==
    ::  the build isn't complete, so try running +make on it
    ::
    =^  made  state  (make build)
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
      =>  promote-live-listeners
      =>  (send-mades current-once-listeners)
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
      =>  (send-mades current-live-listeners)
      =>  ?~(previous-build this (cleanup u.previous-build))
      =>  (cleanup build)
      ::  recurse "upward" into client builds now that :build is done
      ::
      =/  clients=(list ^build)
        ~(tap in (fall (~(get by client-builds.components.state) build) ~))
      ::
      %+  roll  clients
      |=  [client=^build _this]
      this(..execute (execute client))
    ::
        ::  %blocks: build got stuck and produced a set of blocks
        ::
        %blocks
      ::
      =/  blocks  ~(tap in blocks.result.made)
      %+  roll  blocks
      |=  [block=block _this]
      ::  recurse "downward" into the builds we blocked on
      ::
      ?-    -.block
          ::  %build: :build blocked on a sub-build, so run the sub-build
          ::
          %build
        ::
        this(..execute (execute build.block))
      ::
          ::  %dependency: :build blocked on a +dependency
          ::
          ::    Enqueue a request +move to fetch the blocked resource.
          ::    Link :block and :build in :blocks.state so we know
          ::    which build to rerun in a later event when we +unblock
          ::    on that +dependency.
          ::
          %dependency
        ::
        =/  dep=dependency  dependency.block
        ::  TODO: remove to handle other kinds of +dependency
        ::
        ?>  ?=(%clay-once -.dep)
        ::  store :dependency in persistent state
        ::
        =.  blocks.state  (~(put ju blocks.state) `dependency`dep build)
        ::  construct new :move to request blocked resource
        ::
        =/  wire=wire  (welp /(scot %p our)/dependency (to-wire dep))
        =/  note=note
          =,  dep
          :*  %c  %warp  sock=[our their=p.beam]
              [q.beam `[%sing care case=r.beam spur=s.beam]]
          ==
        ::
        =.  moves  [[duct=~ [%pass wire note]] moves]
        this
      ==
    ==
    ::  +sub-builds-changed: did sub-builds change since :previous-build?
    ::
    ++  sub-builds-changed  ^-  ?
      ?~  previous-build  &
      =/  old-sub-builds
        (~(get ju sub-builds.components.state) u.previous-build)
      ::
      %+  lien  ~(tap in old-sub-builds)
      |=  sub=^build  ^-  ?
      ::
      =/  sub-result  (~(got by results.state) sub)
      ?:  ?=(%tombstone -.sub-result)  &
      ::
      =/  next  (~(find-next by-schematic builds-by-schematic.state) sub)
      ?~  next  |
      ::
      ?:  (gth date.u.next date.build)  |
      ::
      =/  next-result  (~(get by results.state) u.next)
      ?~  next-result  |
      ?:  ?=(%tombstone -.u.next-result)  &
      ::
      !=(build-result.u.next-result build-result.sub-result)
    ::  +dependencies-changed: did dependencies change since :previous-build?
    ::
    ++  dependencies-changed  ^-  ?
      =/  dependencies-jug=(jug disc dependency)
        ?~  previous-build  ~
        (fall (~(get by dependencies.state) u.previous-build) ~)
      ::
      =/  dependencies=(set dependency)
        %+  roll  ~(tap by dependencies-jug)
        |=  [[=disc deps=(set dependency)] accumulator=(set dependency)]
        ::
        (~(uni in accumulator) deps)
      ::
      =/  updates-jug=(jug disc dependency)
        (fall (~(get by dependency-updates.state) date.build) ~)
      ::
      =/  updates=(set dependency)
        %+  roll  ~(tap by updates-jug)
        |=  [[=disc deps=(set dependency)] accumulator=(set dependency)]
        ::
        (~(uni in accumulator) deps)
      ::
      !=(~ (~(int in dependencies) updates))
    ::  +promote-live-listeners: move live listeners :previous-build -> :build
    ::
    ++  promote-live-listeners  ^+  this
      ::
      ?~  previous-build
        this
      ::
      %_    this
          state
        ::
        %+  roll  previous-live-listeners
        |=  [=listener state=_state]
        ::
        %_    state
            listeners
          =-  (~(put ju -) build listener)
          (~(del ju listeners.state) u.previous-build listener)
        ::
            builds-by-listener
          (~(put by builds-by-listener.state) duct.listener build)
        ==
      ==
    ::  +current-once-listeners: once listeners on :build
    ::
    ++  current-once-listeners  ^-  (list listener)
      (skip current-listeners |=([* live=?] live))
    ::  +current-live-listeners: live listeners on :build
    ::
    ++  current-live-listeners  ^-  (list listener)
      (skim current-listeners |=([* live=?] live))
    ::  +current-listeners: listeners on :build, both live and once
    ::
    ++  current-listeners  ^-  (list listener)
      ~(tap in (fall (~(get by listeners.state) build) ~))
    ::  +previous-live-listeners: live listeners on :previous-build
    ::
    ++  previous-live-listeners  ^-  (list listener)
      ::  even if :previous-result is a %tombstone, still grab its listeners
      ::
      =/  previous-listeners=(set listener)
        ?~  previous-build  ~
        ?~  previous-result  ~
        (fall (~(get by listeners.state) u.previous-build) ~)
      ::
      (skim ~(tap in previous-listeners) |=([* live=?] live))
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
    ::  update :state to reflect the fact that :build is done
    ::
    ::    Potentially mutates :live-leaf-builds.state,
    ::    :live-root-builds.state, and :done-live-roots.per-event.
    ::
    ++  update-live-tracking  ^+  this
      ::  populate :live-leaf-builds.state with :build's :dependencies
      ::
      =/  dependencies-jug=(jug disc dependency)
        (fall (~(get by dependencies.state) build) ~)
      ::
      =/  dependencies=(set dependency)
        %+  roll  ~(tap by dependencies-jug)
        |=  [[=disc deps=(set dependency)] accumulator=(set dependency)]
        ::
        (~(uni in accumulator) deps)
      ::
      =.  live-leaf-builds.state
        %+  roll  ~(tap in dependencies)
        |=  [=dependency live-leaf-builds=_live-leaf-builds.state]
        ::  if :dependency is not live, don't add it to :live-leaf-builds
        ::
        ?.  ?=(?(%clay-live %gall-live) -.dependency)
          live-leaf-builds
        (~(put ju live-leaf-builds) dependency build)
      ::  recursively gather :discs that :build depends on and store them
      ::
      =/  discs=(set disc)  ~(key by dependencies-jug)
      =.  discs
        |-  ^+  discs
        ?~  kids  discs
        ::
        =/  grandkids  ~(tap in (~(get ju sub-builds.components.state) i.kids))
        =/  kid-deps  (fall (~(get by dependencies.state) i.kids) ~)
        =/  kid-discs  ~(key by kid-deps)
        ::
        $(kids (weld t.kids grandkids), discs (~(uni in discs) kid-discs))
      ::
      =.  live-root-builds.state
        %+  roll  ~(tap in discs)
        |=  [=disc live-root-builds=_live-root-builds.state]
        (~(put ju live-root-builds) disc build)
      ::  prepend :build to :done-live-roots, which is in reverse order
      ::
      =?    done-live-roots
          ::  need these declarations, otherwise mint-coke error
          =/  current-live=(list listener)  current-live-listeners
          =/  previous-live=(list listener)  previous-live-listeners
          ::
          |(?=(^ current-live) ?=(^ previous-live))
        ::
        [build done-live-roots]
      ::
      this
    ::  +send-mades: send one %made move per listener in :listeners
    ::
    ++  send-mades
      |=  [listeners=(list listener)]  ^+  this
      ::
      ?<  ?=(~ result)
      ::
      %_    this
          moves
        %+  roll  listeners
        |=  [=listener moves=_moves]
        ::
        :_  moves
        :*  duct.listener  %give
            %made  date.build  %complete  u.result
        ==
      ==
    ::  +delete-once-listeners: remove once listeners on :build from :state
    ::
    ++  delete-once-listeners  ^+  this
      %_    this
          state
        %+  roll  current-once-listeners
        |=  [=listener state=_state]
        ::
        %_    state
            listeners
          (~(del ju listeners.state) build listener)
        ::
            builds-by-listener
          (~(del by builds-by-listener.state) duct.listener)
        ==
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
      this(state (^cleanup build))
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
    ^-  make-product
    ::  dispatch based on the kind of +schematic in :build
    ::
    |^  ?-    -.schematic.build
        ::
            ^  (autocons head.schematic.build tail.schematic.build)
        ::
            %$  (literal literal.schematic.build)
        ::
            %alts  !!
            %bake  !!
            %bunt  !!
            %call  !!
            %cast  !!
            %core  !!
            %diff  !!
            %dude  !!
            %file  !!
            %hood  !!
            %join  !!
            %mash  !!
            %mute  !!
            %pact  !!
            %path  !!
            %plan  !!
            %reef  !!
            %ride  !!
            %slit  !!
            %slim  !!
            %scry  (scry dependency.schematic.build)
            %vale  !!
            %volt  !!
        ==
    ::
    ++  literal
      |=  =cage  ^-  make-product
      [[%build-result %result %$ cage] state]
    ::
    ++  autocons
      |=  [head=schematic tail=schematic]  ^-  make-product
      ::
      =^  head-result  state  (depend-on head)
      =^  tail-result  state  (depend-on tail)
      ::
      =|  blocks=(set block)
      =?  blocks  ?=(~ head-result)  (~(put in blocks) [%build date.build head])
      =?  blocks  ?=(~ tail-result)  (~(put in blocks) [%build date.build tail])
      ::  if either build blocked, we're not done
      ::
      ?^  blocks
        ::
        [[%blocks blocks] state]
      ::
      ?<  ?=(~ head-result)
      ?<  ?=(~ tail-result)
      ::
      =-  [[%build-result -] state]
      `build-result`[%result u.head-result u.tail-result]
    ::
    ++  scry
      |=  =dependency  ^-  make-product
      ::  construct a full +beam to make the scry request
      ::
      =/  beam=beam
        ?-    -.dependency
            ?(%clay-live %gall-live)
          =,  bel.dependency
          [beak=[p.p q.p [%da date.build]] spur=q]
        ::
            ?(%clay-once %gall-once)
          beam.dependency
        ==
      ::  extract :disc from :beam
      ::
      =/  disc=disc  [p q]:beam
      ::  link :dependency to :build
      ::
      =.  dependencies.state
        %+  ~(put by dependencies.state)  build
        %-  ~(put ju (fall (~(get by dependencies.state) build) ~))
        ::
        [disc dependency]
      ::  perform scry operation if we don't already know the result
      ::
      ::    Look up :dependency in :scry-results.per-event to avoid
      ::    rerunning a previously blocked +scry.
      ::
      =/  scry-response
        ?:  (~(has by scry-results) dependency)
          (~(get by scry-results) dependency)
        (^scry ~ ~ `@tas`(cat 3 %c care.dependency) beam)
      ::  scry blocked
      ::
      ?~  scry-response
        ^-  make-product
        [[%blocks (sy [%dependency dependency]~)] state]
      ::  scry failed
      ::
      ?~  u.scry-response
        =/  error=tang
          :~  leaf+"clay-once scry failed for"
              leaf+"%c{(trip care.dependency)} {<(en-beam beam)>}"
          ==
        ^-  make-product
        [[%build-result %error error] state]
      ::  scry succeeded
      ::
      ^-  make-product
      [[%build-result %result %scry u.u.scry-response] state]
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
      =/  maybe-cache-line  (~(get by results.state) sub-build)
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
  ::  +finalize: convert per-event state to moves and persistent state
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
    ::  discs: the set of discs on which we'll make clay requests
    ::
    =/  discs=(set disc)
      %+  roll  done-live-roots
      |=  [=build discs=(set disc)]
      %-  ~(uni in discs)
      ~(key by (fall (~(get by dependencies.state) build) ~))
    ::  if none of the completed builds depend on a +disc, no-op
    ::
    ?~  discs
      this
    ::  root builds that depend on :discs; will not be `~`
    ::
    =/  roots=(set build)
      %+  roll  ~(tap in `(set disc)`discs)
      |=  [=disc roots=(set build)]
      (~(uni in roots) (~(get ju live-root-builds.state) disc))
    ::
    ?>  ?=(^ roots)
    ::  produce moves
    ::
    =.  moves  %-  welp  :_  moves
      ::
      %+  turn  ~(tap in `(set disc)`discs)
      |=  =disc  ^-  move
      ::  dependencies: all dependencies on :valid-disc for any of :roots
      ::
      =/  dependencies=(set dependency)
        %+  roll  ~(tap in `(set build)`roots)
        |=  [=build dependencies=(set dependency)]
        ::
        =/  all-deps=(jug ^disc dependency)
          (fall (~(get by dependencies.state) build) ~)
        ::
        (~(uni in dependencies) (~(get ju all-deps) disc))
      ::  :dependencies must not be `~`
      ::
      ?>  ?=(^ dependencies)
      ::  request-contents: the set of [care path]s to subscribe to in clay
      ::
      =/  request-contents=(set [care:clay path])
        %-  sy  ^-  (list [care:clay path])
        %+  murn  ~(tap in `(set dependency)`dependencies)
        |=  =dependency  ^-  (unit [care:clay path])
        ?:  ?=(?(%gall-live %gall-once) -.dependency)
          ~
        =-  `[care.dependency -]
        ?-  -.dependency
          %clay-live  spur=q.bel.dependency
          %clay-once  spur=s.beam.dependency
        ==
      ::  their: requestee +ship
      ::
      =/  their=@p
        ?-  -.n.dependencies
          ?(%clay-live %gall-live)  ship=p.p.bel.n.dependencies
          ?(%clay-once %gall-once)  ship=p.beam.n.dependencies
        ==
      ::
      =/  desk=term  q.disc
      ::
      =/  note=note
        :^  %c  %warp  sock=[our their]
        ^-  riff:clay
        [desk `[%mult case=[%da date.n.roots] request-contents]]
      ::
      ^-  move
      [duct=~ [%pass wire=/(scot %p our)/clay-sub/(scot %p their)/[desk] note]]
    ::
    this
  ::  +cleanup: try to clean up a build and its sub-builds
  ::
  ++  cleanup
    |=  =build
    ^-  ford-state
    ::
    ::  if something depends on this build, no-op and return
    ::
    ?:  ?|  (~(has by client-builds.components.state) build)
            (~(has by old.rebuilds.state) build)
            (~(has by listeners.state) build)
        ==
      state
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
    ::  direct-deps: dependencies of :build itself, not :kids
    ::
    =/  direct-deps  (fall (~(get by dependencies.state) build) ~)
    ::  kids: :build's sub-builds
    ::
    =/  kids  ~(tap in (~(get ju sub-builds.components.state) build))
    ::  gather :dependencies from :build and its :kids
    ::
    =/  all-deps=(jug disc dependency)  direct-deps
    =.  all-deps
      |-  ^+  all-deps
      ?~  kids  all-deps
      ::
      =/  grandkids  ~(tap in (~(get ju sub-builds.components.state) i.kids))
      =/  kid-deps  (fall (~(get by dependencies.state) i.kids) ~)
      ::
      $(kids (weld t.kids grandkids), all-deps (unify-jugs all-deps kid-deps))
    ::  remove :build's direct dependencies
    ::
    =.  dependencies.state  (~(del by dependencies.state) build)
    ::  remove :build from :blocks
    ::
    =/  dep-values
      =-  ~(tap in -)
      %+  roll  ~(val by direct-deps)
      |=  [deps=(set dependency) dep-values=(set dependency)]
      (~(uni in dep-values) deps)
    ::
    =.  blocks.state
      %+  roll  dep-values
      |=  [dep=dependency blocks=_blocks.state]
      (~(del ju blocks) dep build)
    ::  for each dependency :build relied on, remove it from :live-leaf-builds
    ::
    =.  live-leaf-builds.state
      %+  roll  dep-values
      |=  [dep=dependency live-leaf-builds=_live-leaf-builds.state]
      (~(del ju live-leaf-builds) dep build)
    ::  for each +disc :build relied on, delete :build from :live-root-builds
    ::
    =/  discs  ~(tap in ~(key by all-deps))
    =.  live-root-builds.state
      %+  roll  discs
      |=  [=disc live-root-builds=_live-root-builds.state]
      (~(del ju live-root-builds.state) disc build)
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
    =.  state
      %+  roll  kids
      |=  [kid=^build new-state=_state]
      (cleanup(state new-state) kid)
    ::  recurse on :rebuild; note this must be done after recursing on :kids
    ::
    =?  state  ?=(^ rebuild)  (cleanup u.rebuild)
    ::
    state
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
   =*  start-build  ~(start-build per-event event-args)
   =^  moves  ship-state  (start-build schematic.task date.task)
   =.  state-by-ship  (~(put by state-by-ship) our.task ship-state)
   ::
   [moves this]
  ::
      %kill  !!
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
      =*  rebuild  ~(rebuild per-event event-args)
      (rebuild [ship desk case.sign] care-paths.sign)
    ::  %dependency: response to a request for a +dependency
    ::
    ?:  =(%dependency i.t.wire)
      ::
      ?>  ?=([%c %writ *] sign)
      ::  dependency: the +dependency we had previously blocked on
      ::
      =/  dependency  (from-wire t.t.wire)
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
      =*  unblock  ~(unblock per-event event-args)
      (unblock dependency scry-result)
    ::
    ~|(unknown-take+i.t.wire !!)
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
