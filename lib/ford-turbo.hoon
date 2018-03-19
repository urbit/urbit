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
        [%clay-live ren=care:clay bel=(pair disc spur)]
        ::  once dependency on a clay path; pinned time
        ::
        [%clay-once ren=care:clay bem=beam]
        ::  live dependency on a gall path; varies with time
        ::
        [%gall-live ren=care:clay bel=(pair disc spur)]
        ::  once dependency on a gall path; pinned time
        ::
        [%gall-once ren=care:clay bem=beam]
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
                [%dude build-result]
                [%file cage]
                [%hood scaffold]
                [%path (pair disc spur)]
                [%plan vase]
                [%reef vase]
                [%ride vase]
                [%slit type]
                [%slim (pair type nock)]
                [%scry cage]
                [%vale cage]
                [%volt cage]
            ::
            ::  clay diff and merge operations
            ::
                [%diff cage]
                [%join cage]
                [%mash cage]
                [%mute cage]
                [%pact cage]
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
            request=dependency
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
    ::
    ::  clay diff and merge operations
    ::
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
++  note
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
--
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
      components=(bi-jug build build)
      ::  rebuilds: bidirectional linkages between old and new identical builds
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
      ::  listeners: external requests for a build, both live (:live=&) and once
      ::
      listeners=(jug build [listener=duct live=?])
      ::  builds-by-listener: reverse lookup for :listeners; find build by duct
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
          last-accessed=@da
          ::  build-result: the referentially transparent result of a +build
          ::
          =build-result
      ==
      ::  %tombstone: marker that this build has been run and its result wiped
      ::
      [%tombstone ~]
  ==
::  +bi-jug: bi-directional jug
::
::    A pair of jugs. The first jug maps from :a to :b; the second maps
::    backward from :b to :a. Both jugs must be kept in sync with each other to
::    be a valid bi-jug. If :a and :b are the same, it can be used to represent
::    arbitrary DAGs traversable in approximately log time in either direction.
::
++  bi-jug
  |*  $:  ::  a: key type for forward mapping; value type for backward mapping
          ::
          a=mold
          ::  b: value type for forward mapping: key type for backward mapping
          ::
          b=mold
      ==
  (pair (jug a b) (jug b a))
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
::  +by-schematic: door for manipulating :builds-by-schematic.ford-state
::
++  by-schematic
  |_  builds=(map schematic (list @da))
  ::  +put: add a +build to :builds
  ::
  ::    The :dates in :build-by-schematic are sorted in reverse
  ::    chronological order.
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
  ++  del
    |=  =build
    ^+  builds
    =.  builds  %+  ~(put by builds)  schematic.build
      ::
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
  --
--
|%
::  +ev: per-event core
::
++  ev
  ::  completed-builds: root builds completed in this event, in reverse order
  ::
  =|  completed-builds=(list build)
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
    ::  add :build to our state
    ::
    =:  listeners.state
      (~(put ju listeners.state) build [duct live])
    ::
        builds-by-listener.state
      (~(put by builds-by-listener.state) duct build)
    ::
        builds-by-date.state
      (~(put ju builds-by-date.state) date.build schematic.build)
    ::
        builds-by-schematic.state
      (~(put by-schematic builds-by-schematic.state) build)
    ==
    ::
    (execute build live)
  ::
  ++  rebuild  !!
  ++  unblock  !!
  ++  cancel  !!
  ::  |construction: arms for performing builds
  ::
  ::+|  construction
  ::
  ++  execute
    |=  [=build live=?]
    ^+  this
    ::
    =^  made  state  (make build)
    ::
    ?-    -.made
        ::  %&: build completed and produced its result
        ::
        %&
      =*  cache-entry  [%result last-accessed=now build-result=p.made]
      ::  prepend :build to :completed-builds, which is in reverse order
      ::
      =.  completed-builds  [build completed-builds]
      ::
      =.  results.state  (~(put by results.state) build cache-entry)
      ::
      this
    ::
        ::  %|: build got stuck and produced a set of blocks
        ::
        %|
      !!
    ==
  ::
  ++  make
    |=  =build
    ^-  [(each build-result (set block)) ford-state]
    |^
    ?-    -.schematic.build
        ^  !!
        %$  (literal literal.schematic.build)
    ::
        %alts  !!
        %bake  !!
        %bunt  !!
        %call  !!
        %cast  !!
        %core  !!
        %dude  !!
        %file  !!
        %hood  !!
        %path  !!
        %plan  !!
        %reef  !!
        %ride  !!
        %slit  !!
        %slim  !!
        %scry  !!
        %vale  !!
        %volt  !!
    ::
    ::  clay diff and merge operations
    ::
        %diff  !!
        %join  !!
        %mash  !!
        %mute  !!
        %pact  !!
    ==
    ++  literal
      |=  =cage
      [[%& %result %$ cage] state]
    --
  ::  |utilities:
  ::
  ::+|  utilities
  ::
  ++  this  .
  ::  +finalize: convert local state to moves and persistent state
  ::
  ::    TODO: needs rework to support live builds
  ::
  ++  finalize
    ^-  [(list move) ford-state]
    ::  mades: list of %made moves to emit, one per duct on a completed build
    ::
    =|  moves=(list move)
    ::  sort completed-builds chronologically (they were originally reversed)
    ::
    =.  completed-builds  (flop completed-builds)
    ::
    =<  [moves state]
    ::  process the completed builds in a loop
    ::
    |-  ^-  [moves=(list move) _this]
    ::  exit condition: no builds left to process
    ::
    ?~  completed-builds
      [moves this]
    ::
    =*  build  i.completed-builds
    ::  look up :build's result from cache
    ::
    =/  cache-line  (~(got by results.state) build)
    ::  :build just completed, so there's no way it could have been reclaimed
    ::
    ?>  ?=(%result -.cache-line)
    ::  create moves to send out for this build
    ::
    =/  moves-for-build
      %+  turn  ~(tap in (~(get ju listeners.state) build))
      |=  [duct=^duct live=?]
      [duct %give %made date.build %complete build-result.cache-line]
    ::  remove all ducts related to this build
    ::
    =.  builds-by-listener.state
      %+  roll  moves-for-build
      |=  [=move builds-by-listener=_builds-by-listener.state]
      =*  duct  -.move
      (~(del by builds-by-listener) duct)
    ::
    =.  listeners.state  (~(del by listeners.state) build)
    ::  try to delete this build entirely if nothing depends on it
    ::
    =.  state  (cleanup build)
    ::  recurse with changes applied
    ::
    %_    $
        completed-builds          t.completed-builds
        moves                     (welp moves moves-for-build)
    ==
  ::  +cleanup: try to clean up a build and its sub-builds
  ::
  ++  cleanup
    |=  =build
    ^-  ford-state
    ::
    =*  sub-builds  p.components.state
    =*  client-builds  q.components.state
    ::
    =*  new-builds  new.rebuilds.state
    =*  old-builds  old.rebuilds.state
    ::  if something depends on this build, no-op and return
    ::
    ?:  ?|  (~(has by client-builds) build)
            (~(has by old-builds) build)
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
    =/  kids  ~(tap in (~(get ju sub-builds) build))
    ::  gather :dependencies from :build and its :kids
    ::
    =/  dependencies=(jug disc dependency)  direct-deps
    =.  dependencies
      |-  ^+  dependencies
      ?~  kids  dependencies
      ::
      =/  grandkids  ~(tap in (~(get ju sub-builds) i.kids))
      =/  kid-deps-set  (~(get by dependencies.state) i.kids)
      =/  kid-deps  ~(tap by (fall kid-deps-set ~))
      ::  TODO replace with ~(uni ju dependencies) kid-deps), requires +uni:ju
      ::
      =/  unified-deps
        |-  ^+  dependencies
        ?~  kid-deps  dependencies
        ::
        ::=+  [disc deps-set]=i.kid-deps is broken because of q face?
        =/  disc=disc  p.i.kid-deps
        =/  deps-set=(set dependency)  q.i.kid-deps
        =/  deps  ~(tap in deps-set)
        =.  dependencies
          |-  ^+  dependencies
          ?~  deps  dependencies
          ::
          $(deps t.deps, dependencies (~(put ju dependencies) disc i.deps))
        ::
        $(kid-deps t.kid-deps)
      ::
      $(kids (weld t.kids grandkids), dependencies unified-deps)
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
    =/  discs  ~(tap in ~(key by dependencies))
    =.  live-root-builds.state
      %+  roll  discs
      |=  [=disc live-root-builds=_live-root-builds.state]
      (~(del ju live-root-builds.state) disc build)
    ::  remove the mapping from :build to its sub-builds
    ::
    =.  sub-builds  (~(del by sub-builds) build)
    ::  for each +build in :kids, remove :build from its clients
    ::
    =.  client-builds
      %+  roll  kids
      |=  [kid=^build clients=_client-builds]
      (~(del ju clients) kid ^build)
    ::  if there is a newer rebuild of :build, delete the linkage
    ::
    =/  rebuild  (~(get by new-builds) build)
    =?  rebuilds.state  ?=(^ rebuild)
      %_  rebuilds.state
        new  (~(del by new-builds) build)
        old  (~(del by old-builds) build)
      ==
    ::  recurse on :kids
    ::
    =.  state
      |-  ^+  state
      ?~  kids  state
      ::
      =.  state  ^$(build i.kids)
      $(kids t.kids)
    ::  recurse on :rebuild; note this must be done after recursing on :kids
    ::
    =?  state  ?=(^ rebuild)  $(build u.rebuild)
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
  ^-  [(list move) q=_this]
  ::  unwrap task
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
   ::    by constructing :event-args and using them to create a :build-func
   ::    that performs the build. The result of :build-func is a pair of
   ::    :moves and a mutant :ship-state. We update our :state-by-ship map
   ::    with the new :ship-state and produce it along with :moves.
   ::
   =^  ship-state  state-by-ship  (find-or-create-ship-state our.task)
   =*  event-args  [[our.task duct now scry] ship-state]
   =*  build-func  ~(start-build ev event-args)
   =^  moves  ship-state  (build-func schematic.task date.task)
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
