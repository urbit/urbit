!:
::  clay (4c), revision control
::
::  This is split in three top-level sections:  structure definitions, main
::  logic, and arvo interface.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::
::  Here are the structures.  `++raft` is the formal arvo state.  It's also
::  worth noting that many of the clay-related structures are defined in zuse.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
|=  pit/vase
=,  clay
=>  |%
++  aeon  @ud                                           ::  version number
::
::  Recursive structure of a desk's data.
::
::  We keep an ankh only for the current version of local desks.  Everywhere
::  else we store it as (map path lobe).
::
++  ankh                                                ::  expanded node
  $:  fil/(unit {p/lobe q/cage})                        ::  file
      dir/(map @ta ankh)                                ::  folders
  ==                                                    ::
::
::  Part of ++mery, representing the set of changes between the mergebase and
::  one of the desks being merged.
::
::  --  `new` is the set of files in the new desk and not in the mergebase.
::  --  `cal` is the set of changes in the new desk from the mergebase except
::      for any that are also in the other new desk.
::  --  `can` is the set of changes in the new desk from the mergebase and that
::      are also in the other new desk (potential conflicts).
::  --  `old` is the set of files in the mergebase and not in the new desk.
::
++  cane
  $:  new/(map path lobe)
      cal/(map path lobe)
      can/(map path cage)
      old/(map path $~)
  ==
::
::  Type of request.
::
::  %u checks for existence, %v produces a ++dome of all desk data, %w with a
::  time or label case gets the aeon at that case, %w with a number case is not
::  recommended, %x gets file contents, %y gets a directory listing, and %z gets
::  a recursive hash of the file contents and children.
::
++  care  ?($u $v $w $x $y $z)
::
::  Keeps track of subscribers.
::
::  A map of requests to a set of all the subscribers who should be notified
::  when the request is filled/updated.
::
++  cult  (jug rove duct)
::
::  Domestic desk state.
::
::  Includes subscriber list, dome (desk content), possible commit state (for
::  local changes), and possible merge state (for incoming merges).
::
++  dojo
  $:  qyx/cult                                          ::  subscribers
      dom/dome                                          ::  desk state
      dok/(unit dork)                                   ::  commit state
      mer/(unit mery)                                   ::  merge state
  ==
::
::  Desk state.
::
::  Includes a checked-out ankh with current content, most recent version, map
::  of all version numbers to commit hashes (commits are in hut.rang), and map
::  of labels to version numbers.
::
++  dome
  $:  ank/ankh                                          ::  state
      let/aeon                                          ::  top id
      hit/(map aeon tako)                               ::  versions by id
      lab/(map @tas aeon)                               ::  labels
  ==                                                    ::
::
::  Commit state.
::
::  --  `del` is the paths we're deleting.
::  --  `ink` is the insertions of hoon files (short-circuited for
::      bootstrapping).
::  --  `ins` is all the other insertions.
::  --  `dig` is all the %dif changes (i.e. we were given a diff to apply).
::  --  `dif` is the diffs in `dig` applied to their files.
::  --  `muc` is all the %mut changes (i.e. we were give a new version of a
::      file).
::  --  `muh` is the hashes of all the new content in `muc`.
::  --  `mut` is the diffs between `muc` and the original files.
::  --  `mim` is a cache of all new content that came with a mime mark.  Often,
::      we need to convert to mime anyway to send to unix, so we just keep it
::      around.
::
++  dork                                                ::  diff work
  $:  del/(list path)                                   ::  deletes
      ink/(list (pair path cage))                       ::  hoon inserts
      ins/(unit (list (pair path cage)))                ::  inserts
      dig/(map path cage)                               ::  store diffs
      dif/(unit (list (trel path lobe cage)))           ::  changes
      muc/(map path cage)                               ::  store mutations
      muh/(map path lobe)                               ::  store hashes
      mut/(unit (list (trel path lobe cage)))           ::  mutations
      mim/(map path mime)                               ::  mime cache
  ==                                                    ::
::
::  Hash of a blob, for lookup in the object store (lat.ran)
::
++  lobe  @uvI                                          ::  blob ref
::
::  Merge state.
::
::  Merges are said to be from 'ali' to 'bob'.  See ++me for more details.
::
::  --  `sor` is the urbit and desk of ali.
::  --  `hen` is the duct that instigated the merge.
::  --  `gem` is the merge strategy.  These are described in `++fetched-ali`.
::  --  `wat` is the current step of the merge process.
::  --  `cas` is the case in ali's desk that we're merging from.
::  --  `ali` is the commit from ali's desk.
::  --  `bob` is the commit from bob's desk.
::  --  `bas` is the commit from the mergebase.
::  --  `dal` is the set of changes from the mergebase to ali's desk.
::  --  `dob` is the set of changes from the mergebase to bob's desk.
::      Check ++cane for more details on these two
::  --  `bof` is the set of changes to the same files in ali and bob.  Null for
::      a file means a conflict while a cage means the diffs have been merged.
::  --  `bop` is the result of patching the original files with the above merged
::      diffs.
::  --  `new` is the newly-created commit.
::  --  `ank` is the ankh for the new state.
::  --  `erg` is the sets of files that should be told to unix.  True means to
::      write the file while false means to delete the file.
::  --  `gon` is the return value of the merge.  On success we produce a set of
::      the paths that had conflicting changes.  On failure we produce an error
::      code and message.
::
++  mery                                                ::  merge state
  $:  sor/(pair ship desk)                              ::  merge source
      hen/duct                                          ::  formal source
      gem/germ                                          ::  strategy
      wat/wait                                          ::  waiting on
      cas/case                                          ::  ali's case
      ali/yaki                                          ::  ali's commit
      bob/yaki                                          ::  bob's commit
      bas/yaki                                          ::  mergebase
      dal/cane                                          ::  diff(bas,ali)
      dob/cane                                          ::  diff(bas,bob)
      bof/(map path (unit cage))                        ::  conflict diffs
      bop/(map path cage)                               ::  conflict patches
      new/yaki                                          ::  merge(dal,dob)
      ank/ankh                                          ::  new state
      erg/(map path ?)                                  ::  ergoable changes
      gon/(each (set path) (pair term (list tank)))     ::  return value
  ==                                                    ::
::
::  Like a ++mood, except with a cache of the state at the starting version.
::
++  moot  {p/case q/case r/path s/(map path lobe)}      ::  stored change range
::
::  New desk data.
::
::  Sent to other ships to update them about a particular desk.  Includes a map
::  of all new aeons to hashes of their commits, the most recent aeon, and sets
::  of all new commits and data.
::
++  nako                                                ::  subscription state 
  $:  gar/(map aeon tako)                               ::  new ids
      let/aeon                                          ::  next id
      lar/(set yaki)                                    ::  new commits
      bar/(set plop)                                    ::  new content
  ==                                                    ::
::
::  Formal vane state.
::
::  --  `fat` is a collection of our domestic ships.
::  --  `hoy` is a collection of foreign ships where we know something about
::      their clay.
::  --  `ran` is the object store.
::  --  `mon` is a collection of mount points (mount point name to urbit
::      location).
::  --  `hez` is the unix duct that %ergo's should be sent to.
::
++  raft                                                ::  filesystem
  $:  fat/(map ship room)                               ::  domestic
      hoy/(map ship rung)                               ::  foreign
      ran/rang                                          ::  hashes
      mon/(map term beam)                               ::  mount points
      hez/(unit duct)                                   ::  sync duct
  ==                                                    ::
::
::  Object store.
::
::  Maps of commit hashes to commits and content hashes to content.
::
++  rang                                                ::  
  $:  hut/(map tako yaki)                               ::
      lat/(map lobe blob)                               ::
  ==                                                    ::
::
::  Unvalidated response to a request.
::
::  Like a ++rant, but with a page of data rather than a cage of it.
::
++  rand                                                ::  unvalidated rant
          $:  p/{p/care q/case r/@tas}                  ::  clade release book
              q/path                                    ::  spur
              r/page                                    ::  data
          ==                                            ::
::
::  Generic desk state.
::
::  --  `lim` is the most recent date we're confident we have all the
::      information for.  For local desks, this is always `now`.  For foreign
::      desks, this is the last time we got a full update from the foreign
::      urbit.
::  --  `ref` is a possible request manager.  For local desks, this is null.
::      For foreign desks, this keeps track of all pending foreign requests
::      plus a cache of the responses to previous requests.
::  --  `qyx` is the set of subscriptions, with listening ducts. These
::      subscriptions exist only until they've been filled.
::  --  `dom` is the actual state of the filetree.  Since this is used almost
::      exclusively in `++ze`, we describe it there.
::  --  `dok` is a possible set of outstanding requests to ford to perform
::      various tasks on commit.  This is null iff we're not in the middle of
::      a commit.
::  --  `mer` is the state of a possible pending merge.  This is null iff
::      we're not in the middle of a merge.  Since this is used almost
::      exclusively in `++me`, we describe it there.
::
++  rede                                                ::  universal project
          $:  lim/@da                                   ::  complete to
              ref/(unit rind)                           ::  outgoing requests
              qyx/cult                                  ::  subscribers
              dom/dome                                  ::  revision state
              dok/(unit dork)                           ::  outstanding diffs
              mer/(unit mery)                           ::  outstanding merges
          ==                                            ::
::
::  Foreign request manager.
::
::  When we send a request to a foreign ship, we keep track of it in here.  This
::  includes a request counter, a map of request numbers to requests, a reverse
::  map of requesters to request numbers, a simple cache of common %sing
::  requests, and a possible nako if we've received data from the other ship and
::  are in the process of validating it.
::
++  rind                                                ::  request manager
          $:  nix/@ud                                   ::  request index
              bom/(map @ud {p/duct q/rave})             ::  outstanding
              fod/(map duct @ud)                        ::  current requests
              haw/(map mood (unit cage))                ::  simple cache
              nak/(unit nako)                           ::  pending validation
          ==                                            ::
::
::  Domestic ship.
::
::  `hun` is the duct to dill, and `dos` is a collection of our desks.
::
++  room                                                ::  fs per ship
          $:  hun/duct                                  ::  terminal duct
              dos/(map desk dojo)                       ::  native desk
          ==                                            ::
::
::  Stored request.
::
::  Like a ++rave but with caches of current versions for %next and %many.
::  Generally used when we store a request in our state somewhere.
::
++  rove                                                ::  stored request
          $%  {$sing p/mood}                            ::  single request
              {$next p/mood q/(unit (each cage lobe))}  ::  next version
              {$many p/? q/moot}                        ::  change range
          ==                                            ::
::
::  Foreign desk data.
::
++  rung  rus/(map desk rede)                           ::  neighbor desks
::
::  Hash of a commit, for lookup in the object store (hut.ran)
::
++  tako  @                                             ::  yaki ref
::
::  Merge state.
::
++  wait  $?  $null   $ali    $diff-ali   $diff-bob     ::  what are we
              $merge  $build  $checkout   $ergo         ::  waiting for?
          ==                                            ::
::
::  Commit.
::
::  List of parents, content, hash of self, and time commited.
::
++  yaki                                                ::  snapshot
          $:  p/(list tako)                             ::  parents
              q/(map path lobe)                         ::  fileset
              r/tako                                    ::  
          ::                                            ::  XX s?
              t/@da                                     ::  date
          ==                                            ::
::
::  Unvalidated blob
::
++  plop  blob                                          ::  unvalidated blob
--  =>
|%
++  move  {p/duct q/(wind note gift:able)}              ::  local move
++  gift                                                ::  out result <-$
      $%  {$ergo p/@tas q/mode}                         ::  version update
          {$hill p/(list @tas)}                         ::  mount points
          {$mack p/(unit tang)}                         ::  ack
          {$mass p/mass}                                ::  memory usage
          {$mere p/(each (set path) (pair term tang))}  ::  merge result
          {$note p/@tD q/tank}                          ::  debug message
          {$ogre p/@tas}                                ::  delete mount point
          {$writ p/riot}                                ::  response
      ==                                                ::
++  note                                                ::  out request $->
  $%  $:  $a                                            ::  to %ames
  $%  {$wont p/sock q/path r/*}                         ::
  ==  ==                                                ::
      $:  $c                                            ::  to %clay
  $%  {$info p/@p q/@tas r/nori}                        ::  internal edit
      {$merg p/@p q/@tas r/@p s/@tas t/case u/germ}     ::  merge desks
      {$warp p/sock q/riff}                             ::
  ==  ==                                                ::
      $:  $d                                            ::
  $%  {$flog p/{$crud p/@tas q/(list tank)}}            ::  to %dill
  ==  ==                                                ::
      $:  $f                                            ::
  $%  {$exec p/@p q/(unit {beak silk:ford})}                 ::
  ==  ==                                                ::
      $:  $t                                            ::
  $%  {$wait p/@da}                                     ::
      {$rest p/@da}                                     ::
  ==  ==  ==                                            ::
++  riot  (unit rant)                                   ::  response+complete
++  sign                                                ::  in result $<-
          $?  $:  $a                                    ::  by %ames
          $%  {$woot p/ship q/coop}                     ::
          ==  ==                                        ::
              $:  $c                                    ::  by %clay
          $%  {$note p/@tD q/tank}                      ::
              {$mere p/(each (set path) (pair term tang))}
              {$writ p/riot}                            ::
          ==  ==                                        ::
              $:  $f                                    ::
          $%  {$made p/@uvH q/gage:ford}                     ::
          ==  ==                                        ::
              $:  $t                                    ::
          $%  {$wake $~}                                ::  timer activate
          ==  ==                                        ::
              $:  @tas                                  ::  by any
          $%  {$crud p/@tas q/(list tank)}              ::
          ==  ==  ==                                    ::
--  =>
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::  section 4cA, filesystem logic
::
::  This core contains the main logic of clay.  Besides `++ze`, this directly
::  contains the logic for commiting new revisions (local urbits), managing
::  and notifying subscribers (reactivity), and pulling and validating content
::  (remote urbits).
::
::  The state includes:
::
::  --  current time `now`
::  --  current duct `hen`
::  --  local urbit `our`
::  --  target urbit `her`
::  --  target desk `syd`
::  --  all vane state `++raft` (rarely used, except for the object store)
::
::  For local desks, `our` == `her` is one of the urbits on our pier.  For
::  foreign desks, `her` is the urbit the desk is on and `our` is the local
::  urbit that's managing the relationship with the foreign urbit.  Don't mix
::  up those two, or there will be wailing and gnashing of teeth.
::
::  While setting up `++de`, we check if the given `her` is a local urbit.  If
::  so, we pull the room from `fat` in the raft and get the desk information
::  from `dos` in there.  Otherwise, we get the rung from `hoy` and get the
::  desk information from `rus` in there.  In either case, we normalize the
::  desk information to a `++rede`, which is all the desk-specific data that
::  we utilize in `++de`.  Because it's effectively a part of the `++de`
::  state, let's look at what we've got:
::
::  --  `lim` is the most recent date we're confident we have all the
::      information for.  For local desks, this is always `now`.  For foreign
::      desks, this is the last time we got a full update from the foreign
::      urbit.
::  --  `ref` is a possible request manager.  For local desks, this is null.
::      For foreign desks, this keeps track of all pending foreign requests
::      plus a cache of the responses to previous requests.
::  --  `qyx` is the set of subscriptions, with listening ducts. These
::      subscriptions exist only until they've been filled.
::  --  `dom` is the actual state of the filetree.  Since this is used almost
::      exclusively in `++ze`, we describe it there.
::  --  `dok` is a possible set of outstanding requests to ford to perform
::      various tasks on commit.  This is null iff we're not in the middle of
::      a commit.
::  --  `mer` is the state of a possible pending merge.  This is null iff
::      we're not in the middle of a merge.  Since this is used almost
::      exclusively in `++me`, we describe it there.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
|%
++  de                                                  ::  per desk
  |=  {now/@da hen/duct raft}
  |=  {{our/@p her/@p} syd/desk}
  =*  ruf  +>+<+>
  =+  ^-  {hun/(unit duct) rede}
      =+  rom=(~(get by fat.ruf) her)
      ?~  rom
        :-  ~
        %+  fall
          (~(get by rus:(fall (~(get by hoy.ruf) her) *rung)) syd)
        :*  lim=~2000.1.1
            ref=[~ *rind]
            qyx=~
            dom=*dome
            dok=~
            mer=~
        ==
      :-  `hun.u.rom
      =+  jod=(fall (~(get by dos.u.rom) syd) *dojo)
      :*  lim=now
          ref=~
          qyx=qyx.jod
          dom=dom.jod
          dok=dok.jod
          mer=mer.jod
      ==
  =*  red  ->
  =|  mow/(list move)
  |%
  ++  abet                                              ::  resolve
    ^-  {(list move) raft}
    :_  =+  rom=(~(get by fat.ruf) her)
        ?~  rom
          =+  rug=(~(put by rus:(fall (~(get by hoy.ruf) her) *rung)) syd red)
          ruf(hoy (~(put by hoy.ruf) her rug))
        =+  dos=(~(put by dos.u.rom) syd [qyx dom dok mer])
        ruf(fat (~(put by fat.ruf) her [(need hun) dos]))
    (flop mow)
  ::
  ::  Handle `%sing` requests
  ::
  ++  aver
    |=  mun/mood
    ^-  (unit (unit (each cage lobe)))
    =+  ezy=?~(ref ~ (~(get by haw.u.ref) mun))
    ?^  ezy
      `(bind u.ezy |=(a/cage [%& a]))
    =+  nao=(case-to-aeon:ze q.mun)
    ::  ~&  [%aver-mun nao [%from syd lim q.mun]]
    ?~(nao ~ (read-at-aeon:ze u.nao mun))
  ::
  ++  ford-fail  |=(tan/tang ~|(%ford-fail (mean tan)))
  ::
  ::  Takes either a result or a stack trace.  If it's a stack trace, we crash;
  ::  else, we produce the result.
  ::
  ++  unwrap-tang
    |*  res/(each * tang)
    ?:(?=($& -.res) p.res (mean p.res))
  ::
  ::  Parse a gage to a list of pairs of cages, crashing on error.
  ::
  ::  Composition of ++gage-to-cages-or-error and ++unwrap-tang.  Maybe same as
  ::  ++gage-to-success-cages?
  ::
  ++  gage-to-cages
    |=  gag/gage:ford
    ^-  (list (pair cage cage))
    (unwrap-tang (gage-to-cages-or-error gag))
  ::
  ::  Same as ++gage-to-cages-or-error except crashes on error.  Maybe same as
  ::  ++gage-to-cages?
  ::
  ++  gage-to-success-cages
    |=  gag/gage:ford
    ^-  (list (pair cage cage))
    ?.  ?=($tabl -.gag)
      (ford-fail ?-(-.gag $| p.gag, $& [>%strange-gage p.p.gag<]~))
    %+  murn  p.gag
    |=  {key/gage:ford val/gage:ford}  
    ^-  (unit {cage cage})
    ?.  ?=($& -.key)
      (ford-fail ?-(-.key $| p.key, $tabl [>%strange-gage<]~))
    ?-  -.val
      $tabl  (ford-fail >%strange-gage< ~)
      $&     (some [p.key p.val])
      $|     =.  p.val  [(sell q.p.key) p.val]
             ~>  %slog.[0 %*(. >%ford-fail syd %her %why< |2.+> p.val)]
             ~
    ==
  ::
  ::  Expects a single-level gage (i.e. a list of pairs of cages).  If the
  ::  result is of a different form, or if some of the computations in the gage
  ::  failed, we produce a stack trace.  Otherwise, we produce the list of pairs
  ::  of cages.
  ::
  ++  gage-to-cages-or-error
    |=  gag/gage:ford
    ^-  (each (list (pair cage cage)) tang)
    ?:  ?=($| -.gag)  (mule |.(`$~`(ford-fail p.gag)))
    ?.  ?=($tabl -.gag)
      (mule |.(`$~`(ford-fail >%strange-gage p.p.gag< ~)))
    =<  ?+(. [%& .] {@ *} .)
    |-  ^-  ?((list {cage cage}) (each $~ tang))
    ?~  p.gag  ~
    =*  hed  i.p.gag
    ?-  -.p.i.p.gag
      $tabl  (mule |.(`$~`(ford-fail >%strange-gage< ~)))
      $|     (mule |.(`$~`(ford-fail p.p.i.p.gag)))
      $&     ?-  -.q.i.p.gag
        $tabl  (mule |.(`$~`(ford-fail >%strange-gage< ~)))
        $|     (mule |.(`$~`(ford-fail p.q.i.p.gag)))
        $&     =+  $(p.gag t.p.gag)
               ?+(- [[p.p p.q]:i.p.gag -] {@ *} -)
    ==       ==
  ::
  ::  Assumes the list of pairs of cages is actually a listified map of paths
  ::  to cages, and converts it to (map path cage) or a stack trace on error.
  ::
  ++  cages-to-map
    |=  tay/(list (pair cage cage))
    =|  can/(map path cage)
    |-  ^-  (each (map path cage) tang)
    ?~  tay   [%& can]
    =*  pax  p.i.tay
    ?.  ?=($path p.pax)
      (mule |.(`$~`~|([%expected-path got=p.pax] !!)))
    $(tay t.tay, can (~(put by can) ((hard path) q.q.pax) q.i.tay))
  ::
  ::  Queue a move.
  ::
  ++  emit
    |=  mof/move
    %_(+> mow [mof mow])
  ::
  ::  Queue a list of moves
  ::
  ++  emil
    |=  mof/(list move)
    %_(+> mow (weld mof mow))
  ::
  ::  Produce either null or a result along a subscription.
  ::
  ::  Producing null means subscription has been completed or cancelled.
  ::
  ++  balk
    |=  {hen/duct cay/(unit (each cage lobe)) mun/mood}
    ^+  +>
    ?~  cay  (blub hen)
    (blab hen mun u.cay)
  ::
  ::  Set timer.
  ::
  ++  bait
    |=  {hen/duct tym/@da}
    (emit hen %pass /tyme %t %wait tym)
  ::
  ::  Cancel timer.
  ::
  ++  best
    |=  {hen/duct tym/@da}
    (emit hen %pass /tyme %t %rest tym)
  ::
  ::  Give subscription result.
  ::
  ::  Result can be either a direct result (cage) or a lobe of a result.  In
  ::  the latter case we fetch the data at the lobe and produce that.
  ::
  ++  blab
    |=  {hen/duct mun/mood dat/(each cage lobe)}
    ^+  +>
    ?:  ?=($& -.dat)
      (emit hen %give %writ ~ [p.mun q.mun syd] r.mun p.dat)
    %-  emit
    :*  hen  %pass  [%blab p.mun (scot q.mun) syd r.mun]
        %f  %exec  our  ~  [her syd q.mun]  (lobe-to-silk:ze r.mun p.dat)
    ==
  ::
  ::  Give next step in a subscription.
  ::
  ++  bleb
    |=  {hen/duct ins/@ud hip/(unit (pair aeon aeon))}
    ^+  +>
    %^  blab  hen  [%w [%ud ins] ~]
    :-  %&
    ?~  hip
      [%null [%atom %n ~] ~]
    [%nako !>((make-nako:ze u.hip))]
  ::
  ::  Tell subscriber that subscription is done.
  ::
  ++  blub
    |=  hen/duct
    (emit hen %give %writ ~)
  ::
  ::  Lifts a function so that a single result can be fanned out over a set of
  ::  subscriber ducts.
  ::
  ::  Thus, `((duct-lift func) subs arg)` runs `(func sub arg)` for each `sub`
  ::  in `subs`.
  ::
  ++  duct-lift
    |*  send/_|=({duct *} ..duct-lift)
    |=  {a/(set duct) arg/_+<+.send}  ^+  ..duct-lift
    =+  all=(~(tap by a))
    |-  ^+  ..duct-lift
    ?~  all  ..duct-lift
    =.  +>.send  ..duct-lift
    $(all t.all, duct-lift (send i.all arg))
  ::
  ++  blub-all  (duct-lift |=({a/duct $~} (blub a)))    ::  lifted ++blub
  ++  blab-all  (duct-lift blab)                        ::  lifted ++blab
  ++  balk-all  (duct-lift balk)                        ::  lifted ++balk
  ++  bleb-all  (duct-lift bleb)                        ::  lifted ++bleb
  ::
  ::  Sends a tank straight to dill for printing.
  ::
  ++  print-to-dill
    |=  {car/@tD tan/tank}
    =+  bar=emit
    =+  foo=+26.bar
    =+  moo=,.+26.bar
    (emit (need hun) %give %note car tan)
  ::
  ::  Transfer a request to another ship's clay.
  ::
  ++  send-over-ames
    |=  {a/duct b/path c/ship d/{p/@ud q/riff}}
    (emit a %pass b %a %wont [our c] [%c %question p.q.d (scot %ud p.d) ~] q.d)
  ::
  ::  Create a request that cannot be filled immediately.
  ::
  ::  If it's a local request, we just put in in `qyx`, setting a timer if it's
  ::  waiting for a particular time.  If it's a foreign request, we add it to
  ::  our request manager (ref, which is a ++rind) and make the request to the
  ::  foreign ship.
  ::
  ++  duce                                              ::  produce request
    |=  rov/rove
    ^+  +>
    =.  rov  (dedupe rov)
    =.  qyx  (~(put ju qyx) rov hen)
    ?~  ref
      (mabe rov |=(@da (bait hen +<)))
    |-  ^+  +>+.$
    =+  rav=(reve rov)
    =+  ^=  vaw  ^-  rave
      ?.  ?=({$sing $v *} rav)  rav
      [%many %| [%ud let.dom] `case`q.p.rav r.p.rav]
    =+  inx=nix.u.ref
    =.  +>+.$
      =<  ?>(?=(^ ref) .)
      (send-over-ames hen [(scot %ud inx) ~] her inx syd ~ vaw)
    %=  +>+.$
      nix.u.ref  +(nix.u.ref)
      bom.u.ref  (~(put by bom.u.ref) inx [hen vaw])
      fod.u.ref  (~(put by fod.u.ref) hen inx)
    ==
  ::
  ::  If a similar request exists, switch to the existing request.
  ::
  ::  "Similar" requests are those %next and %many requests which are the same
  ::  up to starting case, but we're already after the starting case.  This
  ::  stacks later requests for something onto the same request so that they
  ::  all get filled at once.
  ::
  ++  dedupe                                            ::  find existing alias
    |=  rov/rove  ^-  rove
    =;  ros/(list rove)  ?+(ros rov {^ $~} i.ros)
    ?-    -.rov
        $sing  ~
        $next
      ?~  (case-to-aeon:ze q.p.rov)  ~
      %-  ~(rep by qyx)
      |=  {{a/rove *} b/(list rove)}  ^+  b
      =-  ?.(- b [a b])
      ?&  ?=($next -.a)
          =(p.a p.rov(q q.p.a))
          ?=(^ (case-to-aeon:ze q.p.a))
      ==
    ::
        $many
      ?~  (case-to-aeon:ze p.q.rov)  ~
      %-  ~(rep by qyx)
      |=  {{a/rove *} b/(list rove)}  ^+  b
      =-  ?.(- b [a b])
      ?&  ?=($many -.a)
          =(a rov(p.q p.q.a))
          ?=(^ (case-to-aeon:ze p.q.a))
      ==
    ==
  ::
  ::  Takes a list of changed paths and finds those paths that are inside a
  ::  mount point (listed in `mon`).
  ::
  ::  Output is a map of mount points to {length-of-mounted-path set-of-paths}.
  ::
  ++  must-ergo
    |=  can/(list path)
    ^-  (map term (pair @ud (set path)))
    %-  malt  ^-  (list (trel term @ud (set path)))
    %+  murn  (~(tap by mon))
    |=  {nam/term bem/beam}
    ^-  (unit (trel term @ud (set path)))
    =-  ?~(- ~ `[nam (lent s.bem) (silt `(list path)`-)])
    %+  skim  can
    |=  pax/path
    &(=(p.bem her) =(q.bem syd) =((flop s.bem) (scag (lent s.bem) pax)))
  ::
  ::  Initializes a new mount point.
  ::
  ++  mont
    |=  {pot/term pax/path}
    ^+  +>
    =+  can=(turn (~(tap by q:(aeon-to-yaki:ze let.dom))) head)
    =+  mus=(skim can |=(paf/path =(pax (scag (lent pax) paf))))
    ?~  mus
      +>.$
    %-  emit
    :*  hen  %pass  [%ergoing (scot %p her) syd ~]  %f
        %exec  our  ~  [her syd %da now]  %tabl
        ^-  (list (pair silk:ford silk:ford))
        %+  turn  `(list path)`mus
        |=  a/path
        ^-  (pair silk:ford silk:ford)
        :-  [%$ %path !>(a)]
        :+  %cast  %mime
        =+  (need (need (read-x:ze let.dom a)))
        ?:  ?=($& -<)
          [%$ p.-]
        (lobe-to-silk:ze a p.-)
    ==
  ::
  ::  Cancel a request.
  ::
  ::  For local requests, we just remove it from `qyx`.  For foreign requests,
  ::  we remove it from `ref` and tell the foreign ship to cancel as well.
  ::
  ++  cancel-request                                    ::  release request
    ^+  .
    =^  ros/(list rove)  qyx
      :_  (~(run by qyx) |=(a/(set duct) (~(del in a) hen)))
      %-  ~(rep by qyx)
      |=  {{a/rove b/(set duct)} c/(list rove)}
      ?.((~(has in b) hen) c [a c])
    ?~  ref
      =>  .(ref `(unit rind)`ref)     ::  XX TMI
      ?:  =(~ ros)  +                                        ::  XX handle?
      |-  ^+  +>
      ?~  ros  +>
      $(ros t.ros, +> (mabe i.ros |=(@da (best hen +<))))
    ^+  ..cancel-request
    =+  nux=(~(get by fod.u.ref) hen)
    ?~  nux  ..cancel-request
    =:  fod.u.ref  (~(del by fod.u.ref) hen)
        bom.u.ref  (~(del by bom.u.ref) u.nux)
      ==
    (send-over-ames hen [(scot %ud u.nux) ~] her u.nux syd ~)
  ::
  ::  Handles a request.
  ::
  ::  `%sing` requests are handled by ++aver.  `%next` requests are handled by
  ::  running ++aver at the given case, and then subsequent cases until we find
  ::  a case where the two results aren't equivalent.  If it hasn't happened
  ::  yet, we wait.  `%many` requests are handled by producing as much as we can
  ::  and then waiting if the subscription range extends into the future.
  ::
  ++  start-request
    |=  rav/rave
    ^+  +>
    ?-    -.rav
        $sing
      =+  ver=(aver p.rav)
      ?~  ver
        (duce rav)
      ?~  u.ver
        (blub hen)
      (blab hen p.rav u.u.ver)
    ::
        $next
      =+  ver=(aver p.rav)
      ?~  ver
        (duce [- p ~]:rav)
      ?~  u.ver
        (blub hen)
      =+  yon=+((need (case-to-aeon:ze q.p.rav)))
      |-  ^+  +>.^$
      ?:  (gth yon let.dom)
        (duce -.rav p.rav u.ver)
      =+  var=(aver p.rav(q [%ud yon]))
      ?~  var
        ~&  [%oh-no rave=rav aeon=yon letdom=let.dom]
        +>.^$
      ?~  u.var
        (blab hen p.rav %& %null [%atom %n ~] ~)          ::  only her %x
      ?:  (equivalent-data:ze u.u.ver u.u.var)
        $(yon +(yon))
      (blab hen p.rav u.u.var)
    ::
        $many
      =+  nab=(case-to-aeon:ze p.q.rav)
      ?~  nab
        ?>  =(~ (case-to-aeon:ze q.q.rav))
        (duce (rive rav))
      =+  huy=(case-to-aeon:ze q.q.rav)
      ?:  &(?=(^ huy) |((lth u.huy u.nab) &(=(0 u.huy) =(0 u.nab))))
        (blub hen)
      =+  top=?~(huy let.dom u.huy)
      =+  ear=(lobes-at-path:ze top r.q.rav)
      =.  +>.$
        (bleb hen u.nab ?:(p.rav ~ `[u.nab top]))
      ?^  huy
        (blub hen)
      =+  ^=  ptr  ^-  case
          [%ud +(let.dom)]
      (duce `rove`[%many p.rav ptr q.q.rav r.q.rav ear])
    ==
  ::
  ::  Print a summary of changes to dill.
  ::
  ++  print-changes
    |=  {wen/@da lem/nuri}
    ^+  +>
    =+  pre=`path`~[(scot %p her) syd (scot %ud let.dom)]
    ?-  -.lem
      $|  (print-to-dill '=' %leaf :(weld (trip p.lem) " " (spud pre)))
      $&  |-  ^+  +>.^$
          ?~  p.lem  +>.^$
          =.  +>.^$
            %+  print-to-dill
              ?-(-.q.i.p.lem $del '-', $ins '+', $dif ':')
            :+  %rose  ["/" "/" ~]
            %+  turn  (weld pre p.i.p.lem)
            |=  a/cord
            ?:  ((sane %ta) a)
              [%leaf (trip a)]
            [%leaf (dash:us (trip a) '\'')]
          $(p.lem t.p.lem)
    ==
  ::
  ::  This is the entry point to the commit flow.  It deserves some
  ::  explaining, since it's rather long and convoluted.
  ::
  ::  In short, ++edit takes a ++nori and turns it into a ++nuri, which is the
  ::  same thing except that every change is a misu instead of a miso.  Thus,
  ::  insertions are converted to the correct mark, diffs are applied, and
  ::  mutations (change content by replacement) are diffed.  It also fills out
  ::  the other fields in `++dork`.  We run `++apply-edit` to create the final
  ::  nuri and execute the changes.
  ::
  ::  We take a `++nori`, which is either a label-add request or a `++soba`,
  ::  which is a list of changes.  If it's a label, it's easy and we just pass
  ::  it to `++execute-changes:ze`.
  ::
  ::  If the given `++nori` is a list of file changes, then we our goal is to
  ::  convert the list of `++miso` changes to `++misu` changes.  In other
  ::  words, turn the `++nori` into a `++nuri`.  Then, we pass it to
  ::  `++execute-changes:ze`, which applies the changes to our state, and then
  ::  we check out the new revision.  XX  reword
  ::
  ::  Anyhow, enough of high-level talk.  It's time to get down to the
  ::  nitty-gritty.
  ::
  ::  When we get a list of `++miso` changes, we split them into four types:
  ::  deletions, insertions, diffs (i.e. change from diff), and mutations
  ::  (i.e. change from new data).  We do four different things with them.
  ::
  ::  For deletions, we just fill in `del` in `++dork` with a list of the
  ::  deleted files.
  ::
  ::  For insertions, we distinguish bewtween `%hoon` files and all other
  ::  files.  For `%hoon` files, we just store them to `ink` in `++dork` so
  ::  that we add diff them directly.  `%hoon` files have to be treated
  ::  specially to make the bootstrapping sequence work, since the mark
  ::  definitions are themselves `%hoon` files.
  ::
  ::  For the other files, we make a `%tabl` compound ford request to convert
  ::  the data for the new file to the the mark indicated by the last knot in
  ::  the path.
  ::
  ::  For diffs, we make a `%tabl` compound ford request to apply the diff to
  ::  the existing content.  We also store the diffs in `dig` in `++dork`.
  ::
  ::  For mutations, we make a `%tabl` compound ford request to convert the
  ::  given new data to the mark of the already-existing file.  Later on in
  ::  `++take-castify` we'll create the ford request to actually perform the
  ::  diff.  We also store the mutations in `muc` in `++dork`.  I'm pretty
  ::  sure that's useless because who cares about the original data.
  ::  XX delete `muc`.
  ::
  ::  Finally, for performance reasons we cache any of the data that came in
  ::  as a `%mime` cage.  We do this because many commits come from unix,
  ::  where they're passed in as `%mime` and need to be turned back into it
  ::  for the ergo.  We cache both `%hoon` and non-`%hoon` inserts and
  ::  mutations.
  ::
  ::  At this point, the flow of control goes through the three ford requests
  ::  back to `++take-inserting`, `++take-diffing`, and `++take-castifying`,
  ::  which itself leads to `++take-mutating`.  Once each of those has
  ::  completed, we end up at `++apply-edit`, where our unified story picks up
  ::  again.
  ::
  ++  edit                                              ::  apply changes
    |=  {wen/@da lem/nori}
    ^+  +>
    ?:  ?=($| -.lem)
      =^  hat  +>.$
        (execute-changes:ze wen lem)
      ?~  hat
        +>.$
      wake:(print-changes:(checkout-ankh u.hat) wen lem)
    ?.  =(~ dok)
      ~&  %already-applying-changes  +>
    =+  del=(skim p.lem :(corl (cury test %del) head tail))
    =+  ins=(skim p.lem :(corl (cury test %ins) head tail))
    =+  dif=(skim p.lem :(corl (cury test %dif) head tail))
    =+  mut=(skim p.lem :(corl (cury test %mut) head tail))
    =^  ink  ins
      ^-  {(list (pair path miso)) (list (pair path miso))}
      %+  skid  `(list (pair path miso))`ins
      |=  {pax/path mis/miso}
      ?>  ?=($ins -.mis)
      ?&  ?=({$hoon *} (flop pax))
          ?=($mime p.p.mis)
      ==
    =.  +>.$
      %-  emil
      ^-  (list move)
      :~  :*  hen  %pass
              [%inserting (scot %p her) syd (scot %da wen) ~]
              %f  %exec  our  ~  [her syd %da wen]  %tabl
              ^-  (list (pair silk:ford silk:ford))
              %+  turn  ins
              |=  {pax/path mis/miso}
              ?>  ?=($ins -.mis)
              :-  [%$ %path -:!>(*path) pax]
              =+  =>((flop pax) ?~(. %$ i))
              [%cast - [%$ p.mis]]
          ==
          :*  hen  %pass
              [%diffing (scot %p her) syd (scot %da wen) ~]
              %f  %exec  our  ~  [her syd %da wen]  %tabl
              ^-  (list (pair silk:ford silk:ford))
              %+  turn  dif
              |=  {pax/path mis/miso}
              ?>  ?=($dif -.mis)
              =+  (need (need (read-x:ze let.dom pax)))
              ?>  ?=($& -<)
              :-  [%$ %path -:!>(*path) pax]
              [%pact [%$ p.-] [%$ p.mis]]
          ==
          :*  hen  %pass
              [%castifying (scot %p her) syd (scot %da wen) ~]
              %f  %exec  our  ~  [her syd %da wen]  %tabl
              ^-  (list (pair silk:ford silk:ford))
              %+  turn  mut
              |=  {pax/path mis/miso}
              ?>  ?=($mut -.mis)
              :-  [%$ %path -:!>(*path) pax]
              =+  (lobe-to-mark:ze (~(got by q:(aeon-to-yaki:ze let.dom)) pax))
              [%cast - [%$ p.mis]]
          ==
      ==
    %_    +>.$
        dok
      :-  ~
      :*  (turn del |=({pax/path mis/miso} ?>(?=($del -.mis) pax)))
      ::
          %+  turn  ink
          |=  {pax/path mis/miso}
          ^-  (pair path cage)
          ?>  ?=($ins -.mis)
          =+  =>((flop pax) ?~(. %$ i))
          [pax - [%atom %t ~] ((hard @t) +>.q.q.p.mis)]
      ::
          ~
      ::
          %-  malt
          (turn dif |=({pax/path mis/miso} ?>(?=($dif -.mis) [pax p.mis])))
      ::
          ~
      ::
          %-  malt
          (turn mut |=({pax/path mis/miso} ?>(?=($mut -.mis) [pax p.mis])))
      ::
          ~
      ::
          ~
      ::
          %-  molt  ^-  (list (pair path mime))
          ;:  weld
            ^-  (list (pair path mime))
            %+  murn  ins
            |=  {pax/path mis/miso}
            ^-  (unit (pair path mime))
            ?>  ?=($ins -.mis)
            ?.  ?=($mime p.p.mis)
              ~
            `[pax ((hard mime) q.q.p.mis)]
          ::
            ^-  (list (pair path mime))
            %+  murn  ink
            |=  {pax/path mis/miso}
            ^-  (unit (pair path mime))
            ?>  ?=($ins -.mis)
            ?>  ?=($mime p.p.mis)
            `[pax ((hard mime) q.q.p.mis)]
          ::
            ^-  (list (pair path mime))
            %+  murn  mut
            |=  {pax/path mis/miso}
            ^-  (unit (pair path mime))
            ?>  ?=($mut -.mis)
            ?.  ?=($mime p.p.mis)
              ~
            `[pax ((hard mime) q.q.p.mis)]
          ==
      ==
    ==
  ::
  ::  Handle result of insertion.
  ::
  ::  For commit flow overview, see ++edit.
  ::
  ::  Insertions are cast to the correct mark, and here we put the result in
  ::  ins.dok.  If dif and mut are full in dok (i.e. we've already processed
  ::  diffs and mutations), then we go ahead and run ++apply-edit.
  ::
  ++  take-inserting
    |=  {wen/@da res/gage:ford}
    ^+  +>
    ?~  dok
      ~&  %clay-take-inserting-unexpected-made  +>.$
    ?.  =(~ ins.u.dok)
      ~&  %clay-take-inserting-redundant-made  +>.$
    =-  =.  ins.u.dok  `-
        ?:  ?&  ?=(^ dif.u.dok)
                ?=(^ mut.u.dok)
            ==
          (apply-edit wen)
        +>.$
    ^-  (list (pair path cage))
    %+  turn  (gage-to-success-cages res)
    |=  {pax/cage cay/cage}
    ?.  ?=($path p.pax)
      ~|(%clay-take-inserting-strange-path-mark !!)
    [((hard path) q.q.pax) cay]
  ::
  ::  Handle result of diffing.
  ::
  ::  For commit flow overview, see ++edit.
  ::
  ::  Diffs are applied to the original data, and here we put the result in
  ::  dif.dok.  If ins and mut are full in dok (i.e. we've already processed
  ::  insertions and mutations), then we go ahead and run ++apply-edit.
  ::
  ++  take-diffing
    |=  {wen/@da res/gage:ford}
    ^+  +>
    ?~  dok
      ~&  %clay-take-diffing-unexpected-made  +>.$
    ?.  =(~ dif.u.dok)
      ~&  %clay-take-diffing-redundant-made  +>.$
    =-  =.  dif.u.dok  `-
        ?:  ?&  ?=(^ ins.u.dok)
                ?=(^ mut.u.dok)
            ==
          (apply-edit wen)
        +>.$
    ^-  (list (trel path lobe cage))
    %+  turn  (gage-to-cages res)
    |=  {pax/cage cay/cage}
    ^-  (pair path (pair lobe cage))
    ?.  ?=($path p.pax)
      ~|(%clay-take-diffing-strange-path-mark !!)
    =+  paf=((hard path) q.q.pax)
    [paf (page-to-lobe:ze [p q.q]:cay) (~(got by dig.u.dok) paf)]
  ::
  ::  Handle result of casting mutations.
  ::
  ::  For commit flow overview, see ++edit.
  ::
  ::  The new content from a mutation is first casted to the correct mark, and
  ::  here we hash the correctly-marked content and put the result in muh.dok.
  ::  Then we diff the new content against the original content.  The result of
  ::  this is handled in ++take-mutating.
  ::
  ++  take-castify
    |=  {wen/@da res/gage:ford}
    ^+  +>
    ?~  dok
      ~&  %clay-take-castifying-unexpected-made  +>.$
    ?.  =(~ muh.u.dok)
      ~&  %clay-take-castifying-redundant-made  +>.$
    =+  ^-  cat/(list (pair path cage))
        %+  turn  (gage-to-cages res)
        |=  {pax/cage cay/cage}
        ?.  ?=($path p.pax)
          ~|(%castify-bad-path-mark !!)
        [((hard path) q.q.pax) cay]
    =.  muh.u.dok
          %-  malt
          %+  turn  cat
          |=  {pax/path cay/cage}
          [pax (page-to-lobe:ze [p q.q]:cay)]
    %-  emit
    :*  hen  %pass
        [%mutating (scot %p her) syd (scot %da wen) ~]
        %f  %exec  our  ~  [her syd %da wen]  %tabl
        ^-  (list (pair silk:ford silk:ford))
        %+  turn  cat
        |=  {pax/path cay/cage}
        :-  [%$ %path -:!>(*path) pax]
        =+  (lobe-to-silk:ze pax (~(got by q:(aeon-to-yaki:ze let.dom)) pax))
        [%diff - [%$ cay]]
    ==
  ::
  ::  Handle result of diffing mutations.
  ::
  ::  For commit flow overview, see ++edit.
  ::
  ::  We put the calculated diffs of the new content vs the old content (from
  ::  ++take-castify) in mut.dok.  If ins and mut are full in dok (i.e. we've
  ::  already processed insertions and diffs), then we go ahead and run
  ::  ++apply-edit.
  ::
  ++  take-mutating
    |=  {wen/@da res/gage:ford}
    ^+  +>
    ?~  dok
      ~&  %clay-take-mutating-unexpected-made  +>.$
    ?.  =(~ mut.u.dok)
      ~&  %clay-take-mutating-redundant-made  +>.$
    =-  =.  mut.u.dok  `-
        ?:  ?&  ?=(^ ins.u.dok)
                ?=(^ dif.u.dok)
            ==
          (apply-edit wen)
        +>.$
    ^-  (list (trel path lobe cage))
    %+  murn  (gage-to-cages res)
    |=  {pax/cage cay/cage}
    ^-  (unit (pair path (pair lobe cage)))
    ?.  ?=($path p.pax)
      ~|(%clay-take-mutating-strange-path-mark !!)
    ?:  ?=($null p.cay)
      ~
    =+  paf=((hard path) q.q.pax)
    `[paf (~(got by muh.u.dok) paf) cay]
  ::
  ::  Now that dok is completely filled, we can apply the changes in the commit.
  ::
  ::  We collect the relevant data from dok and run ++execute-changes to apply
  ::  them to our state.  Then we run ++checkout-ankh to update our ankh (cache
  ::  of the content at the current aeon).
  ::
  ++  apply-edit
    |=  wen/@da
    ^+  +>
    ::  XX we do the same in ++take-patch, which is confusing and smells foul.
    ::  Here we run ++execute-changes, but we throw away the state changes.  The
    ::  call in ++take-patch is the one that matters, but we print out changes
    ::  here, and we also use that info to call ++checkout-ankh (which is what
    ::  leads to the ++take-patch call).
    ::
    ::  I'm guessing this shouldn't call ++execute-changes at all but rather
    ::  generate the information it needs directly.
    =+  ^-  sim/(list (pair path misu))
        ?~  dok
          ~|(%no-changes !!)
        ?>  ?=(^ ins.u.dok)
        ?>  ?=(^ dif.u.dok)
        ?>  ?=(^ mut.u.dok)
        ;:  weld
          ^-  (list (pair path misu))
          (turn del.u.dok |=(pax/path [pax %del ~]))
        ::
          ^-  (list (pair path misu))
          (turn ink.u.dok |=({pax/path cay/cage} [pax %ins cay]))
        ::
          ^-  (list (pair path misu))
          (turn u.ins.u.dok |=({pax/path cay/cage} [pax %ins cay]))
        ::
          ^-  (list (pair path misu))
          (turn u.dif.u.dok |=({pax/path cal/{lobe cage}} [pax %dif cal]))
        ::
          ^-  (list (pair path misu))
          (turn u.mut.u.dok |=({pax/path cal/{lobe cage}} [pax %dif cal]))
        ==
    =+  hat=(execute-changes:ze wen %& sim)
    ?~  dok  ~&  %no-changes  !!
    ?~  -.hat
      ([print-changes(dok ~)]:.(+>.$ +.hat) wen %& sim)
    (checkout-ankh(lat.ran lat.ran.+.hat) u.-.hat)
  ::
  ::  Takes a map of paths to lobes and tells ford to convert to an ankh.
  ::
  ::  Specifically, we tell ford to convert each lobe into a blob, then we call
  ::  ++take-patch to apply the result to our current ankh and update unix.
  ::
  ++  checkout-ankh
    |=  hat/(map path lobe)
    ^+  +>
    %-  emit
    :*  hen  %pass  [%patching (scot %p her) syd ~]  %f
        %exec  our  :^  ~  [her syd %da now]  %tabl
        ^-  (list (pair silk:ford silk:ford))
        %+  turn  (~(tap by hat))
        |=  {a/path b/lobe}
        ^-  (pair silk:ford silk:ford)
        :-  [%$ %path-hash !>([a b])]
        (lobe-to-silk:ze a b)
    ==
  ::
  ::  Handle the result of the ford call in ++checkout-ankh.
  ::
  ::  We apply the changes by calling ++execute-changes, then we convert the
  ::  result of the ford call from ++checkout-ankh into a map of paths to data
  ::  for the current aeon of this desk.  We turn this into an ankh and store
  ::  it to our state.  Finally, we choose which paths need to be synced to
  ::  unix, and convert the data at those paths to mime (except those paths
  ::  which were added originally as mime, because we still have that stored in
  ::  mim in dok).  The result is handled in ++take-ergo.
  ::
  ++  take-patch
    |=  res/gage:ford
    ^+  +>
    ::  ~&  %taking-patch
    ?:  ?=($| -.res)
      =.  dok  ~
      (print-to-dill '!' %rose [" " "" ""] leaf+"clay patch failed" p.res)
    ::  ~&  %editing
    =+  ^-  sim/(list (pair path misu))
        ?~  dok
          ~|(%no-changes !!)
        ?>  ?=(^ ins.u.dok)
        ?>  ?=(^ dif.u.dok)
        ?>  ?=(^ mut.u.dok)
        ;:  weld
          ^-  (list (pair path misu))
          (turn del.u.dok |=(pax/path [pax %del ~]))
        ::
          ^-  (list (pair path misu))
          (turn ink.u.dok |=({pax/path cay/cage} [pax %ins cay]))
        ::
          ^-  (list (pair path misu))
          (turn u.ins.u.dok |=({pax/path cay/cage} [pax %ins cay]))
        ::
          ^-  (list (pair path misu))
          (turn u.dif.u.dok |=({pax/path cal/{lobe cage}} [pax %dif cal]))
        ::
          ^-  (list (pair path misu))
          (turn u.mut.u.dok |=({pax/path cal/{lobe cage}} [pax %dif cal]))
        ==
    =^  hat  +>.$  (execute-changes:ze now %& sim)
                                      ::  XX  do same in ++apply-edit
    ?~  dok  ~&  %no-dok  +>.$
    =>
      %=    .
          +>.$
        ?<  ?=($~ hat)                                   ::  XX  whut?
        (print-changes now %& sim)
      ==
    ?~  dok  ~&  %no-dok  +>.$
    =+  ^-  cat/(list (trel path lobe cage))
        %+  turn  (gage-to-cages res)
        |=  {pax/cage cay/cage}
        ?.  ?=($path-hash p.pax)
          ~|(%patch-bad-path-mark !!)
        [-< -> +]:[((hard {path lobe}) q.q.pax) cay]
    ::  ~&  %canned
    ::  ~&  %checking-out
    =.  ank.dom  (map-to-ankh:ze (malt cat))
    ::  ~&  %checked-out
    ::  ~&  %waking
    =.  +>.$  =>(wake ?>(?=(^ dok) .))
    ::  ~&  %waked
    ?~  hez  +>.$(dok ~)
    =+  mus=(must-ergo (turn sim head))
    ?:  =(~ mus)
      +>.$(dok ~)
    =+  ^-  sum/(set path)
        =+  (turn (~(tap by mus)) (corl tail tail))
        %+  roll  -
        |=  {pak/(set path) acc/(set path)}
        (~(uni in acc) pak)
    =+  can=(malt sim)
    ::  ~&  %forming-ergo
    ::  =-  ~&  %formed-ergo  -
    %-  emit(dok ~)
    :*  hen  %pass  [%ergoing (scot %p her) syd ~]  %f
        %exec  our  ~  [her syd %da now]  %tabl
        ^-  (list (pair silk:ford silk:ford))
        %+  turn  (~(tap in sum))
        |=  a/path
        ^-  (pair silk:ford silk:ford)
        :-  [%$ %path !>(a)]
        =+  b=(~(got by can) a)
        ?:  ?=($del -.b)
          [%$ %null !>(~)]
        =+  (~(get by mim.u.dok) a)
        ?^  -  [%$ %mime !>(u.-)]
        :+  %cast  %mime
        =+  (need (need (read-x:ze let.dom a)))
        ?:  ?=($& -<)
          [%$ p.-]
        (lobe-to-silk:ze a p.-)
    ==
  ::
  ::  Send new data to unix.
  ::
  ::  Combine the paths in mim in dok and the result of the ford call in
  ::  ++take-patch to create a list of nodes that need to be sent to unix (in
  ::  an %ergo card) to keep unix up-to-date.  Send this to unix.
  ::
  ++  take-ergo
    |=  res/gage:ford
    ^+  +>
    ?:  ?=($| -.res)
      (print-to-dill '!' %rose [" " "" ""] leaf+"clay ergo failed" p.res)
    ?~  hez  ~|(%no-sync-duct !!)
    =+  ^-  can/(map path (unit mime))
        %-  malt  ^-  mode
        %+  turn  (gage-to-cages res)
        |=  {pax/cage mim/cage}
        ?.  ?=($path p.pax)
          ~|(%ergo-bad-path-mark !!)
        :-  ((hard path) q.q.pax)
        ?.  ?=($mime p.mim)
          ~
        `((hard mime) q.q.mim)
    =+  mus=(must-ergo (turn (~(tap by can)) head))
    %-  emil
    %+  turn  (~(tap by mus))
    |=  {pot/term len/@ud pak/(set path)}
    :*  u.hez  %give  %ergo  pot
        %+  turn  (~(tap in pak))
        |=  pax/path
        [(slag len pax) (~(got by can) pax)]
    ==
  ::
  ::  Called when a foreign ship answers one of our requests.
  ::
  ::  After updating ref (our request manager), we handle %x, %w, and %y
  ::  responses.  For %x, we call ++validate-x to validate the type of the
  ::  response.  For %y, we coerce the result to an arch.
  ::
  ::  For %w, we check to see if it's a @ud response (e.g. for
  ::  cw+//~sampel-sipnym/desk/~time-or-label).  If so, it's easy.  Otherwise,
  ::  we look up our subscription request, then assert the response was a nako.
  ::  If this is the first update for a desk, we assume everything's well-typed
  ::  and call ++apply-foreign-update directly.  Otherwise, we call
  ::  ++validate-plops to verify that the data we're getting is well typed.
  ::
  ::  Be careful to call ++wake if/when necessary (i.e. when the state changes
  ::  enough that a subscription could be filled).  Every case must call it
  ::  individually.
  ::
  ++  take-foreign-update                              ::  external change
    |=  {inx/@ud rut/(unit rand)}
    ^+  +>
    ?>  ?=(^ ref)
    |-  ^+  +>+.$
    =+  ruv=(~(get by bom.u.ref) inx)
    ?~  ruv  +>+.$
    =>  ?.  |(?=($~ rut) ?=($sing -.q.u.ruv))  .
        %_  .
          bom.u.ref  (~(del by bom.u.ref) inx)
          fod.u.ref  (~(del by fod.u.ref) p.u.ruv)
        ==
    ?~  rut
      =+  rav=`rave`q.u.ruv
      =<  ?>(?=(^ ref) .)
      %_    wake
          lim
        ?.(&(?=($many -.rav) ?=($da -.q.q.rav)) lim `@da`p.q.q.rav)
      ::
          haw.u.ref
        ?.  ?=($sing -.rav)  haw.u.ref
        (~(put by haw.u.ref) p.rav ~)
      ==
    ?-    p.p.u.rut
        $u
      ~|  %im-thinkin-its-prolly-a-bad-idea-to-request-rang-over-the-network
      !!
    ::
        $v
      ~|  %weird-we-shouldnt-get-a-dome-request-over-the-network
      !!
    ::
        $x
      =<  ?>(?=(^ ref) .)
      (validate-x p.p.u.rut q.p.u.rut q.u.rut r.u.rut)
    ::
        $w
      =.  haw.u.ref
        %+  ~(put by haw.u.ref)
          [p.p.u.rut q.p.u.rut q.u.rut]
        :+  ~
          p.r.u.rut
        ?+  p.r.u.rut  ~|  %strange-w-over-nextwork  !!
          $aeon  !>(((hard aeon) q.r.u.rut))
          $null  [[%atom %n ~] ~]
          $nako  !>(~|([%harding [&1 &2 &3]:q.r.u.rut] ((hard nako) q.r.u.rut)))
        ==
      ?.  ?=($nako p.r.u.rut)  [?>(?=(^ ref) .)]:wake
      =+  rav=`rave`q.u.ruv
      ?>  ?=($many -.rav)
      |-  ^+  +>+.^$
      =+  nez=[%w [%ud let.dom] ~]
      =+  nex=(~(get by haw.u.ref) nez)
      ?~  nex  +>+.^$
      ?~  u.nex  +>+.^$  ::  should never happen
      =.  nak.u.ref  `((hard nako) q.q.u.u.nex)
      =.  +>+.^$
        ?:  =(0 let.dom)
          =<  ?>(?=(^ ref) .)
          %+  apply-foreign-update
            ?.(?=($da -.q.q.rav) ~ `p.q.q.rav)
          (need nak.u.ref)
        =<  ?>(?=(^ ref) .)
        %^    validate-plops
            [%ud let.dom]
          ?.(?=($da -.q.q.rav) ~ `p.q.q.rav)
        bar:(need nak.u.ref)
      %=  $
        haw.u.ref  (~(del by haw.u.ref) nez)
      ==
    ::
        $y
      =<  ?>(?=(^ ref) .)
      %_    wake
          haw.u.ref
        %+  ~(put by haw.u.ref)
          [p.p.u.rut q.p.u.rut q.u.rut]
        `[p.r.u.rut !>(((hard arch) q.r.u.rut))]
      ==
    ::
        $z
      ~|  %its-prolly-not-reasonable-to-request-ankh-over-the-network-sorry
      !!
    ==
  ::
  ::  Check that given data is actually of the mark it claims to be.
  ::
  ::  Result is handled in ++take-foreign-x
  ::
  ++  validate-x
    |=  {car/care cas/case pax/path peg/page}
    ^+  +>
    %-  emit
    :*  hen  %pass
        [%foreign-x (scot %p our) (scot %p her) syd car (scot cas) pax]
        %f  %exec  our  ~  [her syd cas]
        (vale-page peg)
    ==
  ::
  ::  Create a silk to validate a page.
  ::
  ::  If the mark is %hoon, we short-circuit the validation for bootstrapping
  ::  purposes.
  ::
  ++  vale-page
    |=  a/page
    ^-  silk:ford
    ?.  ?=($hoon p.a)  [%vale a]
    ?.  ?=(@t q.a)  [%dude |.(>%weird-hoon<) %ride [%fail ~] %$ *cage]
    [%$ p.a [%atom %t ~] q.a]
  ::
  ::  Verify the foreign data is of the the mark it claims to be.
  ::
  ::  This completes the receiving of %x foreign data.
  ::
  ++  take-foreign-x
    |=  {car/care cas/case pax/path res/gage:ford}
    ^+  +>
    ?>  ?=(^ ref)
    ?.  ?=($& -.res)
      ~|  "validate foreign x failed"
      =+  why=?-(-.res $| p.res, $tabl ~[>%bad-marc<])
      ~>  %mean.|.(%*(. >[%plop-fail %why]< |1.+> why))
      !!
    ?>  ?=(@ p.p.res)
    wake(haw.u.ref (~(put by haw.u.ref) [car cas pax] `p.res))
  ::
  ::  When we get a %w foreign update, store this in our state.
  ::
  ::  We get the commits and blobs from the nako and add them to our object
  ::  store, then we update the map of aeons to commits and the latest aeon.
  ::
  ::  We call ++wake at the end to update anyone whose subscription is fulfilled
  ::  by this state change.
  ::
  ++  apply-foreign-update                              ::  apply subscription
    |=  $:  lem/(unit @da)                              ::  complete up to
            gar/(map aeon tako)                         ::  new ids
            let/aeon                                    ::  next id
            lar/(set yaki)                              ::  new commits
            bar/(set blob)                              ::  new content
        ==
    ^+  +>
    =<  wake
    =+  ^-  nut/(map tako yaki)
        %-  molt  ^-  (list (pair tako yaki))
        %+  turn  (~(tap in lar))
        |=  yak/yaki
        [r.yak yak]
    =+  ^-  nat/(map lobe blob)
        %-  molt  ^-  (list (pair lobe blob))
        %+  turn  (~(tap in bar))
        |=  bol/blob
        [p.bol bol]
    ~|  :*  %bad-foreign-update
            :*  gar=gar
                let=let
                nut=(~(run by nut) $~)
                nat=(~(run by nat) $~)
            ==
            :*  hitdom=hit.dom
                letdom=let.dom
                hutran=(~(run by hut.ran) $~)
                latran=(~(run by lat.ran) $~)
            ==
        ==
    =+  hit=(~(uni by hit.dom) gar)
    =+  let=let
    =+  hut=(~(uni by hut.ran) nut)
    =+  lat=(~(uni by lat.ran) nat)
    =+  ?:  =(0 let)  ~
        =+  yon=`aeon`1                                 ::  sanity check
        |-
        ~|  yon=yon
        =+  tak=(~(got by hit) yon)
        =+  yak=(~(got by hut) tak)
        =+  %-  ~(urn by q.yak)
            |=  {pax/path lob/lobe}
            ~|  [pax=path lob=lobe]
            (~(got by lat) lob)
        ?:  =(let yon)
          ~
        $(yon +(yon))
    %=  +>.$
      lim       (max (fall lem lim) lim)
      hit.dom   hit
      let.dom   (max let let.dom)
      hut.ran   hut
      lat.ran   lat
    ==
  ::
  ::  Make sure that incoming data is of the correct type.
  ::
  ::  This is a ford call to make sure that incoming data is of the mark it
  ::  claims to be.  The result is handled in ++take-foreign-plops.
  ::
  ++  validate-plops
    |=  {cas/case lem/(unit @da) pop/(set plop)}
    ^+  +>
    =+  lum=(scot %da (fall lem *@da))
    %-  emit
    :*  hen  %pass
        [%foreign-plops (scot %p our) (scot %p her) syd lum ~]
        %f  %exec  our  ~  [her syd cas]  %tabl
        ^-  (list (pair silk:ford silk:ford))
        %+  turn  (~(tap in pop))
        |=  a/plop
        ?-  -.a
          $direct  [[%$ %blob !>([%direct p.a *page])] (vale-page p.q.a q.q.a)]
          $delta
            [[%$ %blob !>([%delta p.a q.a *page])] (vale-page p.r.a q.r.a)]
        ==
    ==
  ::
  ::  Verify that foreign plops validated correctly.  If so, apply them to our
  ::  state.
  ::
  ++  take-foreign-plops
    |=  {lem/(unit @da) res/gage:ford}
    ^+  +>
    ?>  ?=(^ ref)
    ?>  ?=(^ nak.u.ref)
    =+  ^-  lat/(list blob)
        %+  turn  ~|("validate foreign plops failed" (gage-to-cages res))
        |=  {bob/cage cay/cage}
        ?.  ?=($blob p.bob)
          ~|  %plop-not-blob
          !!
        =+  bol=((hard blob) q.q.bob)
        ?-  -.bol
          $delta      [-.bol p.bol q.bol p.cay q.q.cay]
          $direct     [-.bol p.bol p.cay q.q.cay]
        ==
    %^    apply-foreign-update
        lem
      gar.u.nak.u.ref
    :+  let.u.nak.u.ref
      lar.u.nak.u.ref
    (silt lat)
  ::
  ++  mabe                                            ::  maybe fire function
    |=  {rov/rove fun/$-(@da _.)}
    ^+  +>.$
    %+  fall
      %+  bind
        ^-  (unit @da)
        ?-    -.rov
            $sing
          ?.  ?=($da -.q.p.rov)  ~
          `p.q.p.rov
        ::
            $next  ~
            $many
          %^  hunt  lth
            ?.  ?=($da -.p.q.rov)  ~
            ?.((lth now p.p.q.rov) ~ [~ p.p.q.rov])
          ?.  ?=($da -.q.q.rov)  ~
          (hunt gth [~ now] [~ p.q.q.rov])
        ==
      fun
    +>.$
  ::
  ++  reve
    |=  rov/rove
    ^-  rave
    ?-  -.rov
      $sing  rov
      $next  [- p]:rov
      $many  [%many p.rov p.q.rov q.q.rov r.q.rov]
    ==
  ::
  ++  rive
    |=  rav/{$many p/? q/moat}
    ^-  rove
    [%many p.rav p.q.rav q.q.rav r.q.rav ~]
  ::
  ::  Loop through open subscriptions and check if we can fill any of them.
  ::
  ++  wake                                            ::  update subscribers
    ^+  .
    =+  xiq=(~(tap by qyx))
    =|  xaq/(list {p/rove q/(set duct)})
    |-  ^+  ..wake
    ?~  xiq
      ..wake(qyx (~(gas by *cult) xaq))
    ?:  =(~ q.i.xiq)  $(xiq t.xiq, xaq xaq)           :: drop forgotten
    ?-    -.p.i.xiq
        $sing
      =+  cas=?~(ref ~ (~(get by haw.u.ref) `mood`p.p.i.xiq))
      ?^  cas
        %=    $
            xiq  t.xiq
            ..wake  ?~  u.cas  (blub-all q.i.xiq ~)
                    (blab-all q.i.xiq p.p.i.xiq %& u.u.cas)
        ==
      =+  nao=(case-to-aeon:ze q.p.p.i.xiq)
      ?~  nao  $(xiq t.xiq, xaq [i.xiq xaq])
      ::  ~&  %reading-at-aeon
      =+  vid=(read-at-aeon:ze u.nao p.p.i.xiq)
      ::  ~&  %red-at-aeon
      ?~  vid
        ::  ?:  =(0 u.nao)
        ::    ~&  [%oh-poor `path`[syd '0' r.p.p.i.xiq]]
        ::    $(xiq t.xiq)
        ::  ~&  [%oh-well desk=syd mood=p.p.i.xiq aeon=u.nao]
        $(xiq t.xiq, xaq [i.xiq xaq])
      $(xiq t.xiq, ..wake (balk-all q.i.xiq u.vid p.p.i.xiq))
    ::
        $next
      =*  mun  p.p.i.xiq
      ::  =*  dat  q.p.i.xiq    XX can't fuse right now
      ?~  q.p.i.xiq
        =+  ver=(aver mun)
        ?~  ver
          $(xiq t.xiq, xaq [i.xiq xaq])
        ?~  u.ver
          $(xiq t.xiq, ..wake (blub-all q.i.xiq ~))
        $(xiq t.xiq, xaq [i.xiq(q.p u.ver) xaq])
      =+  var=(aver mun(q [%ud let.dom]))
      ?~  var
        ~&  [%oh-noes mood=mun letdom=let.dom]
        $(xiq t.xiq)
      ?~  u.var
        $(xiq t.xiq, ..wake (blab-all q.i.xiq mun %& %null [%atom %n ~] ~))
      ?:  (equivalent-data:ze u.q.p.i.xiq u.u.var)
        $(xiq t.xiq, xaq [i.xiq xaq])
      $(xiq t.xiq, ..wake (blab-all q.i.xiq mun u.u.var))
    ::
        $many
      =+  mot=`moot`q.p.i.xiq
      =+  nab=(case-to-aeon:ze p.mot)
      ?~  nab
        $(xiq t.xiq, xaq [i.xiq xaq])
      =+  huy=(case-to-aeon:ze q.mot)
      ?~  huy
        =.  p.mot  [%ud +(let.dom)]
        %=  $
          xiq     t.xiq
          xaq     [i.xiq(q.p mot) xaq]
          ..wake  =+  ^=  ear
                      (lobes-at-path:ze let.dom r.mot)
                  ?:  =(s.mot ear)  ..wake
                  (bleb-all q.i.xiq let.dom ?:(p.p.i.xiq ~ `[u.nab let.dom]))
        ==
      %=  $
        xiq     t.xiq
        ..wake  =-  (blub-all:- q.i.xiq ~)
                =+  ^=  ear
                    (lobes-at-path:ze u.huy r.mot)
                ?:  =(s.mot ear)  (blub-all q.i.xiq ~)
                (bleb-all q.i.xiq +(u.nab) ?:(p.p.i.xiq ~ `[u.nab u.huy]))
      ==
    ==
  ++  drop-me
    ^+  .
    ?~  mer
      .
    %-  emit(mer ~)  ^-  move  :*
      hen.u.mer  %give  %mere  %|  %user-interrupt
      >sor.u.mer<  >our<  >cas.u.mer<  >gem.u.mer<  ~
    ==
  ::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::
  ::  This core has no additional state, and the distinction exists purely for
  ::  documentation.  The overarching theme is that `++de` directly contains
  ::  logic for metadata about the desk, while `++ze` is composed primarily
  ::  of helper functions for manipulating the desk state (`++dome`) itself.
  ::  Functions include:
  ::
  ::  --  converting between cases, commit hashes, commits, content hashes,
  ::      and content
  ::  --  creating commits and content and adding them to the tree
  ::  --  finding which data needs to be sent over the network to keep the
  ::  --  other urbit up-to-date
  ::  --  reading from the file tree through different `++care` options
  ::  --  the `++me` core for merging.
  ::
  ::  The dome is composed of the following:
  ::
  ::  --  `ank` is the ankh, which is the file data itself.  An ankh is both
  ::      a possible file and a possible directory.  An ankh has both:
  ::      --  `fil`, a possible file, stored as both a cage and its hash
  ::      --  `dir`, a map of @ta to more ankhs.
  ::  --  `let` is the number of the most recent revision.
  ::  --  `hit` is a map of revision numbers to commit hashes.
  ::  --  `lab` is a map of labels to revision numbers.
  ::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ++  ze
    |%
    ::  These convert between aeon (version number), tako (commit hash), yaki
    ::  (commit data structure), lobe (content hash), and blob (content).
    ++  aeon-to-tako  ~(got by hit.dom)
    ++  aeon-to-yaki  (cork aeon-to-tako tako-to-yaki)
    ++  lobe-to-blob  ~(got by lat.ran)
    ++  tako-to-yaki  ~(got by hut.ran)
    ++  lobe-to-mark
      |=  a/lobe
      =>  (lobe-to-blob a)
      ?-  -
        $delta      p.q
        $direct     p.q
      ==
    ::
    ::  Creates a silk to put a type on a page (which is a {mark noun}).
    ::
    ++  page-to-silk                                    :: %hoon bootstrapping
      |=  a/page
      ?.  ?=($hoon p.a)  [%volt a]
      [%$ p.a [%atom %t ~] q.a]
    ::
    ::  Creates a silk out of a lobe (content hash).
    ::
    ++  lobe-to-silk
      |=  {pax/path lob/lobe}
      ^-  silk:ford
      =+  ^-  hat/(map path lobe)
          ?:  =(let.dom 0)
            ~
          q:(aeon-to-yaki let.dom)
      =+  lol=`(unit lobe)`?.(=(~ ref) `0vsen.tinel (~(get by hat) pax))
      |-  ^-  silk:ford
      ?:  =([~ lob] lol)
        =+  (need (need (read-x let.dom pax)))
        ?>  ?=($& -<)
        [%$ p.-]
      =+  bol=(~(got by lat.ran) lob)
      ?-  -.bol
        $direct     (page-to-silk q.bol)
        $delta      ~|  delta+q.q.bol
                    [%pact $(lob q.q.bol) (page-to-silk r.bol)]
      ==
    ::
    ::  Hashes a page to get a lobe.
    ::
    ++  page-to-lobe  |=(page (shax (jam +<)))
    ::
    ::  Checks whether two pieces of data (either cages or lobes) are the same.
    ::
    ++  equivalent-data
      |=  {one/(each cage lobe) two/(each cage lobe)}
      ^-  ?
      ?:  ?=($& -.one)
        ?:  ?=($& -.two)
          =([p q.q]:p.one [p q.q]:p.two)
        =(p.two (page-to-lobe [p q.q]:p.one))
      ?:  ?=($& -.two)
        =(p.one (page-to-lobe [p q.q]:p.two))
      =(p.one p.two)
    ::
    ::  Make a direct blob out of a page.
    ::
    ++  make-direct-blob
      |=  p/page
      ^-  blob
      [%direct (page-to-lobe p) p]
    ::
    ::  Make a delta blob out of a lobe, mark, lobe of parent, and page of diff.
    ::
    ++  make-delta-blob
      |=  {p/lobe q/{p/mark q/lobe} r/page}
      ^-  blob
      [%delta p q r]
    ::
    ::  Make a commit out of a list of parents, content, and date.
    ::
    ++  make-yaki
      |=  {p/(list tako) q/(map path lobe) t/@da}
      ^-  yaki
      =+  ^=  has
          %^  cat  7  (sham [%yaki (roll p add) q t])
          (sham [%tako (roll p add) q t])
      [p q has t]
    ::
    ::  Reduce a case to an aeon (version number)
    ::
    ::  We produce null if we can't yet reduce the case for whatever resaon
    ::  (usually either the time or aeon hasn't happened yet or the label hasn't
    ::  been created), we produce null.
    ::
    ++  case-to-aeon
      |=  lok/case                                      ::  act count through
      ^-  (unit aeon)
      ?-    -.lok
          $da
        ?:  (gth p.lok lim)  ~
        |-  ^-  (unit aeon)
        ?:  =(0 let.dom)  [~ 0]                         ::  avoid underflow
        ?:  %+  gte  p.lok
            =<  t
            ~|  [%letdom let=let.dom hit=hit.dom hut=(~(run by hut.ran) $~)]
            ~|  [%getdom (~(get by hit.dom) let.dom)]
            %-  aeon-to-yaki
            let.dom
          [~ let.dom]
        $(let.dom (dec let.dom))
      ::
          $tas  (~(get by lab.dom) p.lok)
          $ud   ?:((gth p.lok let.dom) ~ [~ p.lok])
      ==
    ::
    ::  Convert a map of paths to data into an ankh.
    ::
    ++  map-to-ankh
      |=  hat/(map path (pair lobe cage))
      ^-  ankh
      ::  %-  cosh
      %+  roll  (~(tap by hat) ~)
      |=  {{pat/path lob/lobe zar/cage} ank/ankh}
      ^-  ankh
      ::  %-  cosh
      ?~  pat
        ank(fil [~ lob zar])
      =+  nak=(~(get by dir.ank) i.pat)
      %=  ank
        dir  %+  ~(put by dir.ank)  i.pat
             $(pat t.pat, ank (fall nak *ankh))
      ==
    ::
    ::  Applies a change list, creating the commit and applying it to the
    ::  current state.
    ::
    ::  Also produces the new data from the commit for convenience.
    ::
    ++  execute-changes
      |=  {wen/@da lem/nuri}
      ^-  {(unit (map path lobe)) _..ze}
      ?-  -.lem
        $&
           =^  yak  lat.ran  (forge-yaki wen p.lem)     ::  create new commit
           ?.  ?|  =(0 let.dom)
                   !=((lent p.yak) 1)
                   !=(q.yak q:(aeon-to-yaki let.dom))
               ==
             `..ze                                      ::  silently ignore
           =:  let.dom  +(let.dom)
               hit.dom  (~(put by hit.dom) +(let.dom) r.yak)
               hut.ran  (~(put by hut.ran) r.yak yak)
           ==
           [`q.yak ..ze]
           ::  +>.$(ank (map-to-ankh q.yak))
        $|
           ?<  (~(has by lab.dom) p.lem)
           [~ ..ze(lab.dom (~(put by lab.dom) p.lem let.dom))]
      ==
    ::
    ::  Create a commit out of a list of changes against the current state.
    ::
    ::  First call ++apply-changes to apply the list of changes and get the new
    ::  state of the content.  Then, call ++update-lat to add any new content to
    ::  the blob store.  Finally, create the new yaki (commit) and produce both
    ::  it and the new lat (blob store).
    ::
    ++  forge-yaki
      |=  {wen/@da lem/suba}
      =+  par=?:(=(0 let.dom) ~ [(aeon-to-tako let.dom) ~])
      =+  new=(apply-changes lem)
      =+  gar=(update-lat new lat.ran)
      :-  (make-yaki par +.gar wen)                     ::  from existing diff
      -.gar                                             ::  fix lat
    ::
    ::  Apply a list of changes against the current state and produce the new
    ::  state.
    ::
    ++  apply-changes                                   ::   apply-changes:ze
      |=  lar/(list {p/path q/misu})                    ::  store changes
      ^-  (map path blob)
      =+  ^=  hat                                       ::  current state
          ?:  =(let.dom 0)                              ::  initial commit
            ~                                           ::  has nothing
          =<  q
          %-  aeon-to-yaki
          let.dom
      =-  =+  sar=(silt (turn lar |=({p/path *} p)))    ::  changed paths
          %+  roll  (~(tap by hat) ~)                   ::  find unchanged
          =<  .(bat bar)
          |=  {{pax/path gar/lobe} bat/(map path blob)}
          ?:  (~(has in sar) pax)                       ::  has update
            bat
          %+  ~(put by bat)  pax
          ~|  [pax gar (lent (~(tap by lat.ran)))]
          (lobe-to-blob gar)                            ::  use original
      ^=  bar  ^-  (map path blob)
      %+  roll  lar
      |=  {{pax/path mys/misu} bar/(map path blob)}
      ^+  bar
      ?-    -.mys
          $ins                                          ::  insert if not exist
        ?:  (~(has by bar) pax)  !!                     ::
        ?:  (~(has by hat) pax)  !!                     ::
        %+  ~(put by bar)  pax
        %-  make-direct-blob
        ?:  &(?=($mime -.p.mys) =([%hoon ~] (slag (dec (lent pax)) pax)))
          `page`[%hoon +.+.q.q.p.mys]
        [p q.q]:p.mys
      ::
          $del                                          ::  delete if exists
        ?.  |((~(has by hat) pax) (~(has by bar) pax))  !!
        (~(del by bar) pax)
      ::
          $dif                                          ::  mutate, must exist
        =+  ber=(~(get by bar) pax)                     ::  XX  typed
        =+  her==>((flop pax) ?~(. %$ i))
        ?~  ber
          =+  har=(~(get by hat) pax)
          ?~  har  !!
          %+  ~(put by bar)  pax
          (make-delta-blob p.mys [(lobe-to-mark u.har) u.har] [p q.q]:q.mys)
                                                        :: XX check vase !evil
        ::  XX of course that's a problem, p.u.ber isn't in rang since it
        ::     was just created.  We shouldn't be sending multiple
        ::     diffs
        ::  %+  ~(put by bar)  pax
        ::  %^  make-delta-blob  p.mys
        ::    [(lobe-to-mark p.u.ber) p.u.ber]
        ::  [p q.q]:q.mys
        ::                                              :: XX check vase !evil
        ~|([%two-diffs-for-same-file syd pax] !!)
      ==
    ::
    ::  Update the object store with new blobs.
    ::
    ::  Besides new object store, converts the given (map path blob) to
    ::  (map path lobe).
    ::
    ++  update-lat                                      ::   update-lat:ze
      |=  {lag/(map path blob) sta/(map lobe blob)}     ::  fix lat
      ^-  {(map lobe blob) (map path lobe)}
      %+  roll  (~(tap by lag) ~)
      =<  .(lut sta)
      |=  {{pat/path bar/blob} {lut/(map lobe blob) gar/(map path lobe)}}
      ?~  (~(has by lut) p.bar)
        [lut (~(put by gar) pat p.bar)]
      :-  (~(put by lut) p.bar bar)
      (~(put by gar) pat p.bar)
    ::
    ::  Gets a map of the data at the given path and all children of it.
    ::
    ++  lobes-at-path
      |=  {yon/aeon pax/path}
      ^-  (map path lobe)
      ?:  =(0 yon)  ~
      %-  malt
      %+  skim
        %.  ~
        %~  tap  by
          =<  q
          %-  aeon-to-yaki
          yon
        ==
      |=  {p/path q/lobe}
      ?|  ?=($~ pax)
          ?&  !?=($~ p)
              =(-.pax -.p)
              $(p +.p, pax +.pax)
      ==  ==
    ::
    ::  Creates a nako of all the changes between a and b.
    ::
    ++  make-nako
      |=  {a/aeon b/aeon}
      ^-  nako
      :+  ?>  (lte b let.dom)
          |-
          ?:  =(b let.dom)
            hit.dom
          $(hit.dom (~(del by hit.dom) let.dom), let.dom (dec let.dom))
        b
      ?:  =(0 b)
        [~ ~]
      (data-twixt-takos (~(get by hit.dom) a) (aeon-to-tako b))
    ::
    ::  Gets the data between two commit hashes, assuming the first is an
    ::  ancestor of the second.
    ::
    ::  Get all the takos before `a`, then get all takos before `b` except the
    ::  ones we found before `a`.  Then convert the takos to yakis and also get
    ::  all the data in all the yakis.
    ::
    ++  data-twixt-takos
      |=  {a/(unit tako) b/tako}
      ^-  {(set yaki) (set plop)}
      =+  old=?~(a ~ (reachable-takos u.a))
      =+  ^-  yal/(set tako)
          %-  silt
          %+  skip
            (~(tap in (reachable-takos b)))
          |=(tak/tako (~(has in old) tak))
      :-  (silt (turn (~(tap in yal)) tako-to-yaki))
      (silt (turn (~(tap in (new-lobes (new-lobes ~ old) yal))) lobe-to-blob))
    ::
    ::  Traverses parentage and finds all ancestor hashes
    ::
    ++  reachable-takos                                 ::  reachable
      |=  p/tako
      ^-  (set tako)
      =+  y=(tako-to-yaki p)
      %+  roll  p.y
      =<  .(s (~(put in *(set tako)) p))
      |=  {q/tako s/(set tako)}
      ?:  (~(has in s) q)                               ::  already done
        s                                               ::  hence skip
      (~(uni in s) ^$(p q))                             ::  otherwise traverse
    ::
    ::  Get all the lobes that are referenced in `a` except those that are
    ::  already in `b`.
    ::
    ++  new-lobes                                       ::  object hash set
      |=  {b/(set lobe) a/(set tako)}                   ::  that aren't in b
      ^-  (set lobe)
      %+  roll  (~(tap in a) ~)
      |=  {tak/tako bar/(set lobe)}
      ^-  (set lobe)
      =+  yak=(tako-to-yaki tak)
      %+  roll  (~(tap by q.yak) ~)
      =<  .(far bar)
      |=  {{path lob/lobe} far/(set lobe)}
      ^-  (set lobe)
      ?~  (~(has in b) lob)                             ::  don't need
        far
      =+  gar=(lobe-to-blob lob)
      ?-  -.gar
        $direct    (~(put in far) lob)
        $delta     (~(put in $(lob q.q.gar)) lob)
      ==
    ::
    ::  Should be refactored, is only called form `++read`, and even then it
    ::  can't be called with `$v` as the care, so it's really just a crash.
    ::
    ::  To be clear the refactoring should start at ++read-at-aeon and probably
    ::  eliminate ++read and ++query
    ::
    ++  query                                           ::    query:ze
      |=  ren/$?($u $v $x $y $z)                        ::  endpoint query
      ^-  (unit cage)
      ?-  ren
        $u  !!  ::  [~ %null [%atom %n] ~]
        $v  [~ %dome !>(dom)]
        $x  !!  ::  ?~(q.ank.dom ~ [~ q.u.q.ank.dom])
        $y  !!  ::  [~ %arch !>(as-arch)]
        $z  !!  ::  [~ %ankh !>(ank.dom)]
      ==
    ::
    ::  See ++query.
    ::
    ++  read                                            ::    read:ze
      |=  mun/mood                                      ::  read at point
      ^-  (unit cage)
      ?:  ?=($v p.mun)
        [~ %dome !>(dom)]                               ::  dead code
      ?:  &(?=($w p.mun) !?=($ud -.q.mun))
        ?^(r.mun ~ [~ %aeon !>(let.dom)])               ::  dead code
      ?:  ?=($w p.mun)
        =+  ^=  yak
            %-  aeon-to-yaki
            let.dom
        ?^(r.mun ~ !!) :: [~ %w !>([t.yak (forge-nori yak)])])-all
      (query(ank.dom ank:(descend-path:(zu ank.dom) r.mun)) p.mun) ::  dead code
    ::
    ::  Checks for existence of a node at an aeon.
    ::
    ::  This checks for existence of content at the node, and does *not* look
    ::  at any of its children.
    ::
    ++  read-u
      |=  {yon/aeon pax/path}
      ^-  (unit (unit (each {$null (hypo $~)} lobe)))
      =+  tak=(~(get by hit.dom) yon)
      ?~  tak
        ~
      ``[%& %null [%atom %n ~] ~]
    ::
    ::  Gets the dome (desk state) at a particular aeon.
    ::
    ::  For past aeons, we don't give an actual ankh in the dome, but the rest
    ::  of the data is legit.
    ::
    ++  read-v
      |=  {yon/aeon pax/path}
      ^-  (unit (unit {$dome (hypo dome)}))
      ?:  (lth yon let.dom)
        :*  ~  ~  %dome  -:!>(%dome)
            ank=`[[%ank-in-old-v-not-implemented *ankh] ~ ~]
            let=yon
            hit=(molt (skim (~(tap by hit.dom)) |=({p/@ud *} (lte p yon))))
            lab=(molt (skim (~(tap by lab.dom)) |=({* p/@ud} (lte p yon))))
        ==
      ?:  (gth yon let.dom)
        ~
      ``[%dome -:!>(*dome) dom]
    ::
    ::  Gets the data at a node.
    ::
    ::  If it's in our ankh (current state cache), we can just produce the
    ::  result.  Otherwise, we've got to look up the node at the aeon to get the
    ::  content hash, use that to find the blob, and use the blob to get the
    ::  data.  We also special-case the hoon mark for bootstrapping purposes.
    ::
    ++  read-x
      |=  {yon/aeon pax/path}
      ^-  (unit (unit (each cage lobe)))
      ?:  =(0 yon)
        [~ ~]
      =+  tak=(~(get by hit.dom) yon)
      ?~  tak
        ~
      ?:  &(?=($~ ref) =(yon let.dom))
        :-  ~
        %+  bind
          fil.ank:(descend-path:(zu ank.dom) pax)
        |=(a/{p/lobe q/cage} [%& q.a])
      =+  yak=(tako-to-yaki u.tak)
      =+  lob=(~(get by q.yak) pax)
      ?~  lob
        [~ ~]
      =+  mar=(lobe-to-mark u.lob)
      ?.  ?=($hoon mar)
        [~ ~ %| u.lob]
      :^  ~  ~  %&
      :+  mar  [%atom %t ~]
      |-  ^-  @t                      ::  (urge cord) would be faster
      =+  bol=(lobe-to-blob u.lob)
      ?:  ?=($direct -.bol)
        ((hard @t) q.q.bol)
      ?>  ?=($delta -.bol)
      =+  txt=$(u.lob q.q.bol)
      ?>  ?=($txt-diff p.r.bol)
      =+  dif=((hard (urge cord)) q.r.bol)
      =,  format
      =+  pac=(of-wain (lurk:differ (to-wain (cat 3 txt '\0a')) dif))
      (end 3 (dec (met 3 pac)) pac)
    ::
    ::  Gets an arch (directory listing) at a node.
    ::
    ++  read-y
      |=  {yon/aeon pax/path}
      ^-  (unit (unit {$arch (hypo arch)}))
      ?:  =(0 yon)
        ``[%arch -:!>(*arch) *arch]
      =+  tak=(~(get by hit.dom) yon)
      ?~  tak
        ~
      =+  yak=(tako-to-yaki u.tak)
      =+  len=(lent pax)
      :^  ~  ~  %arch
      ::  ~&  cy+pax
      :-  -:!>(*arch)
      ^-  arch
      :-  (~(get by q.yak) pax)
      ^-  (map knot $~)
      %-  molt  ^-  (list (pair knot $~))
      %+  turn
        ^-  (list (pair path lobe))
        %+  skim  (~(tap by (~(del by q.yak) pax)))
        |=  {paf/path lob/lobe}
        =(pax (scag len paf))
      |=  {paf/path lob/lobe}
      =+  pat=(slag len paf)
      [?>(?=(^ pat) i.pat) ~]
    ::
    ::  Gets a recursive hash of a node and all its children.
    ::
    ++  read-z
      |=  {yon/aeon pax/path}
      ^-  (unit (unit {$uvi (hypo @uvI)}))
      ?:  =(0 yon)
        ``uvi+[-:!>(*@uvI) *@uvI]
      =+  tak=(~(get by hit.dom) yon)
      ?~  tak
        ~
      =+  yak=(tako-to-yaki u.tak)
      =+  len=(lent pax)
      :: ~&  read-z+[yon=yon qyt=~(wyt by q.yak) pax=pax]
      =+  ^-  descendants/(list (pair path lobe))
          ::  ~&  %turning
          ::  =-  ~&  %turned  -
          %+  turn
            ::  ~&  %skimming
            ::  =-  ~&  %skimmed  -
            %+  skim  (~(tap by (~(del by q.yak) pax)))
            |=  {paf/path lob/lobe}
            =(pax (scag len paf))
          |=  {paf/path lob/lobe}
          [(slag len paf) lob]
      =+  us=(~(get by q.yak) pax)
      ^-  (unit (unit {$uvi (hypo @uvI)}))
      :^  ~  ~  %uvi
      :-  -:!>(*@uvI)
      ?:  &(?=($~ descendants) ?=($~ us))
        *@uvI
      %+  roll
        ^-  (list (pair path lobe))
        [[~ ?~(us *lobe u.us)] descendants]
      |=({{path lobe} @uvI} (shax (jam +<)))
    ::
    ::  Get a value at an aeon.
    ::
    ::  Value can be either null, meaning we don't have it yet, {null null},
    ::  meaning we know it doesn't exist, or {null null (each cage lobe)},
    ::  meaning we either have the value directly or a content hash of the
    ::  value.
    ::
    ::  Should change last few lines to an explicit ++read-w.
    ::
    ++  read-at-aeon                                    ::    read-at-aeon:ze
      |=  {yon/aeon mun/mood}                           ::  seek and read
      ^-  (unit (unit (each cage lobe)))
      ?:  &(?=($w p.mun) !?=($ud -.q.mun))              ::  NB only her speed
        ?^(r.mun [~ ~] [~ ~ %& %aeon !>(yon)])
      ?:  ?=($u p.mun)
        (read-u yon r.mun)
      ?:  ?=($v p.mun)
        (bind (read-v yon r.mun) (lift |=(a/cage [%& a])))
      ?:  ?=($x p.mun)
        (read-x yon r.mun)
      ?:  ?=($y p.mun)
        ::  =-  ~&  :*  %dude-someones-getting-curious
        ::              mun=mun
        ::              yon=yon
        ::              our=our
        ::              her=her
        ::              syd=syd
        ::              hep=-
        ::          ==
        ::      -
        (bind (read-y yon r.mun) (lift |=(a/cage [%& a])))
      ?:  ?=($z p.mun)
        (bind (read-z yon r.mun) (lift |=(a/cage [%& a])))
      %+  bind
        (rewind yon)
      |=  a/(unit _+>.$)
      ^-  (unit (each cage lobe))
      ?~  a
        ~
      `(unit (each cage lobe))`(bind (read:u.a mun) |=(a/cage [%& a]))
    ::
    ::  Stubbed out, should be removed in the refactoring mentioned in ++query.
    ::
    ++  rewind                                          ::    rewind:ze
      |=  yon/aeon                                      ::  rewind to aeon
      ^-  (unit (unit _+>))
      ?:  =(let.dom yon)  ``+>
      ?:  (gth yon let.dom)  !!                         ::  don't have version
      =+  hat=q:(aeon-to-yaki yon)
      ?:  (~(any by hat) |=(a/lobe ?=($delta [-:(lobe-to-blob a)])))
        ~
      ~
      ::=+  ^-  (map path cage)
      ::    %-  ~(run by hat)
      ::    |=  a=lobe
      ::    =+  (lobe-to-blob a)
      ::    ?-(-.- %direct q.-, %delta !!)
      ::`+>.$(ank.dom (map-to-ankh -), let.dom yon)
    ::
    ::  Traverse an ankh.
    ::
    ++  zu                                              ::  filesystem
      |=  ank/ankh                                      ::  filesystem state
      =|  ram/path                                      ::  reverse path into
      |%
      ++  descend                                       ::  descend
        |=  lol/@ta
        ^+  +>
        =+  you=(~(get by dir.ank) lol)
        +>.$(ram [lol ram], ank ?~(you [~ ~] u.you))
      ::
      ++  descend-path                                  ::  descend recursively
        |=  way/path
        ^+  +>
        ?~(way +> $(way t.way, +> (descend i.way)))
      --
    ::
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ::
    ::  This core is specific to any currently running merge.  This is
    ::  basically a simple (DAG-shaped) state machine.  We always say we're
    ::  merging from 'ali' to 'bob'.  The basic steps, not all of which are
    ::  always needed, are:
    ::
    ::  --  fetch ali's desk
    ::  --  diff ali's desk against the mergebase
    ::  --  diff bob's desk against the mergebase
    ::  --  merge the diffs
    ::  --  build the new state
    ::  --  "checkout" (apply to actual `++dome`) the new state
    ::  --  "ergo" (tell unix about) any changes
    ::
    ::  The state filled in order through each step.  See ++mery for a
    ::  description of the state.
    ::
    ::
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ++  me                                              ::  merge ali into bob
      |=  {ali/(pair ship desk) alh/(unit dome) new/?}  ::  from
      =+  bob=`(pair ship desk)`[our syd]               ::  to
      =+  ^-  dat/(each mery term)
          ?~  mer
            ?:  new
              =+  *mery
              [%& -(sor ali:+, hen hen:+, wat %null)]
            [%| %not-actually-merging]
          ?.  new
            ?:  =(ali sor.u.mer)
              [%& u.mer]
            ~&  :*  %already-merging-from-somewhere-else
                    ali=ali
                    sor=sor.u.mer
                    gem=gem.u.mer
                    wat=wat.u.mer
                    cas=cas.u.mer
                    hen=hen
                    henmer=hen.u.mer
                ==
            [%| %already-merging-from-somewhere-else]
          ~&  :*  %already-merging-from-somewhere
                  ali=ali
                  sor=sor.u.mer
                  gem=gem.u.mer
                  wat=wat.u.mer
                  cas=cas.u.mer
                  hen=hen
                  henmer=hen.u.mer
              ==
          [%| %already-merging-from-somewhere]
      ?:  ?=($| -.dat)
        ~|(p.dat !!)
      =+  dat=p.dat
      =|  don/?                                         ::  keep going
      |%
      ::
      ::  Resolve.  If we're done, produce a result.
      ::
      ++  abet
        ^+  ..me
        ?:  don
          ..me(mer `dat)
        =.  mer  ~
        =>  (emit hen.dat %give %mere gon.dat)
        ..me
      ::
      ::  Send a move.
      ::
      ++  emit
        |=  move
        %_(+> ..ze (^emit +<))
      ::
      ::  Send a list of moves.
      ::
      ++  emil
        |=  (list move)
        %_(+> ..ze (^emil +<))
      ::
      ::  Route responses from clay or ford.
      ::
      ::  Check that the stage of the response is the same as the stage we think
      ::  we're in, and call the appropriate function for that stage.
      ::
      ++  route
        |=  {sat/term res/(each riot gage:ford)}
        ^+  +>.$
        ?.  =(sat wat.dat)
          ~|  :*  %hold-your-horses-merge-out-of-order
                  sat=sat
                  wat=wat.dat
                  ali=ali
                  bob=bob
                  hepres=-.res
              ==
           !!
        ?+  +<  ~|((crip <[%bad-stage sat ?~(-.res %riot %gage)]>) !!)
          {$ali $& *}       %.(p.res fetched-ali)
          {$diff-ali $| *}  %.(p.res diffed-ali)
          {$diff-bob $| *}  %.(p.res diffed-bob)
          {$merge $| *}     %.(p.res merged)
          {$build $| *}     %.(p.res built)
          {$checkout $| *}  %.(p.res checked-out)
          {$ergo $| *}      %.(p.res ergoed)
        ==
      ::
      ::  Start a merge.
      ::
      ::  Sets cas.dat, gem.dat, and bob.dat.  Unless there's an error, leads
      ::  to ++fetch-ali.
      ::
      ++  start
        |=  {cas/case gem/germ}
        ^+  +>
        ?:  &(=(0 let.dom) !?=(?($init $that) gem))
          (error:he %no-bob-desk ~)
        =.  cas.dat  cas
        =.  gem.dat  gem
        ?:  =(0 let.dom)
          fetch-ali(gem.dat %init)
        =+  (~(get by hit.dom) let.dom)
        ?~  -
          (error:he %no-bob--version ~)
        =+  (~(get by hut.ran) u.-)
        ?~  -
          (error:he %no-bob-commit ~)
        fetch-ali(bob.dat u.-)
      ::
      ::  Tell clay to get the state at the requested case for ali's desk.
      ::
      ++  fetch-ali
        ^+  .
        %-  emit(wat.dat %ali)
        :*  hen  %pass
            [%merge (scot %p p.bob) q.bob (scot %p p.ali) q.ali %ali ~]
            %c  %warp  [p.bob p.ali]  q.ali
            `[%sing %v cas.dat /]
        ==
      ::
      ::  Parse the state of ali's desk, and get the most recent commit.
      ::
      ::  Sets ali.dat.
      ::
      ++  fetched-ali
        |=  rot/riot
        ^+  +>
        ?~  rot
          (error:he %bad-fetch-ali ~)
        =+  ^=  dum
            %-  (hard {ank/* let/@ud hit/(map @ud tako) lab/(map @tas @ud)})
            q.q.r.u.rot
        ?:  =(0 let.dum)
          (error:he %no-ali-desk ~)
        =+  (~(get by hit.dum) let.dum)
        ?~  -
          (error:he %no-ali-version ~)
        =+  (~(get by hut.ran) u.-)
        ?~  -
          (error:he %no-ali-commit ~)
        =.  ali.dat  u.-
        |-
        ?-    gem.dat
        ::
        ::  If this is an %init merge, we set the ali's commit to be bob's, and
        ::  we checkout the new state.
        ::
            $init
          =.  new.dat  ali.dat
          =.  hut.ran  (~(put by hut.ran) r.new.dat new.dat)
          =.  erg.dat  (~(run by q.ali.dat) |=(lobe %&))
          checkout
        ::
        ::  If this is a %this merge, we check to see if ali's and bob's commits
        ::  are the same, in which case we're done.  Otherwise, we check to see
        ::  if ali's commit is in the ancestry of bob's, in which case we're
        ::  done.  Otherwise, we create a new commit with bob's data plus ali
        ::  and bob as parents.  Then we checkout the new state.
        ::
            $this
          ?:  =(r.ali.dat r.bob.dat)  done:he
          ?:  (~(has in (reachable-takos r.bob.dat)) r.ali.dat)  done:he
          =.  new.dat  (make-yaki [r.ali.dat r.bob.dat ~] q.bob.dat now)
          =.  hut.ran  (~(put by hut.ran) r.new.dat new.dat)
          =.  erg.dat  ~
          checkout
        ::
        ::  If this is a %that merge, we check to see if ali's and bob's commits
        ::  are the same, in which case we're done.  Otherwise, we create a new
        ::  commit with ali's data plus ali and bob as parents.  Then we
        ::  checkout the new state.
        ::
            $that
          ?:  =(r.ali.dat r.bob.dat)  done:he
          =.  new.dat  (make-yaki [r.ali.dat r.bob.dat ~] q.ali.dat now)
          =.  hut.ran  (~(put by hut.ran) r.new.dat new.dat)
          =.  erg.dat
            %-  malt  ^-  (list {path ?})
            %+  murn  (~(tap by (~(uni by q.bob.dat) q.ali.dat)))
            |=  {pax/path lob/lobe}
            ^-  (unit {path ?})
            =+  a=(~(get by q.ali.dat) pax)
            =+  b=(~(get by q.bob.dat) pax)
            ?:  =(a b)
              ~
            `[pax !=(~ a)]
          checkout
        ::
        ::  If this is a %fine merge, we check to see if ali's and bob's commits
        ::  are the same, in which case we're done.  Otherwise, we check to see
        ::  if ali's commit is in the ancestry of bob's, in which case we're
        ::  done.  Otherwise, we check to see if bob's commit is in the ancestry
        ::  of ali's.  If not, this is not a fast-forward merge, so we error
        ::  out.  If it is, we add ali's commit to bob's desk and checkout.
        ::
            $fine
          ?:  =(r.ali.dat r.bob.dat)
            ::  ~&  [%fine-trivial ali=<ali> bob=<bob> r.ali.dat r.bob.dat]
            done:he
          ?:  (~(has in (reachable-takos r.bob.dat)) r.ali.dat)
            ::  ~&  [%fine-mostly-trivial ali=<ali> bob=<bob>]
            done:he
          ?.  (~(has in (reachable-takos r.ali.dat)) r.bob.dat)
            ::  ~&  [%fine-not-so-trivial ali=<ali> bob=<bob>]
            (error:he %bad-fine-merge ~)
          ::  ~&  [%fine-lets-go ali=<ali> bob=<bob>]
          =.  new.dat  ali.dat
          =.  erg.dat
            %-  malt  ^-  (list {path ?})
            %+  murn  (~(tap by (~(uni by q.bob.dat) q.ali.dat)))
            |=  {pax/path lob/lobe}
            ^-  (unit {path ?})
            =+  a=(~(get by q.ali.dat) pax)
            =+  b=(~(get by q.bob.dat) pax)
            ?:  =(a b)
              ~
            `[pax !=(~ a)]
          checkout
        ::
        ::  If this is a %meet, %mate, or %meld merge, we may need to fetch
        ::  more data.  If this merge is either trivial or a fast-forward, we
        ::  short-circuit to either ++done or the %fine case.
        ::
        ::  Otherwise, we find the best common ancestor(s) with
        ::  ++find-merge-points.  If there's no common ancestor, we error out.
        ::  Additionally, if there's more than one common ancestor (i.e. this
        ::  is a criss-cross merge), we error out.  Something akin to git's
        ::  recursive merge should probably be used here, but it isn't.
        ::
        ::  Once we have our single best common ancestor (merge base), we store
        ::  it in bas.dat.  If this is a %mate or %meld merge, we need to diff
        ::  ali's commit against the merge base, so we pass control over to
        ::  ++diff-ali.
        ::
        ::  Otherwise (i.e. this is a %meet merge), we create a list of all the
        ::  changes between the mege base and ali's commit and store it in
        ::  dal.dat, and we put a similar list for bob's commit in dob.dat.
        ::  Then we create bof, which is the a set of changes in both ali and
        ::  bob's commits.  If this has any members, we have conflicts, which is
        ::  an error in a %meet merge, so we error out.
        ::
        ::  Otherwise, we merge the merge base data with ali's data and bob's
        ::  data, which produces the data for the new commit, which we put in
        ::  new.dat.  Then we checkout the new data.
        ::
            ?($meet $mate $meld)
          ?:  =(r.ali.dat r.bob.dat)
            done:he
          ?.  (~(has by hut.ran) r.bob.dat)
            (error:he %bad-bob-tako >r.bob.dat< ~)
          ?:  (~(has in (reachable-takos r.bob.dat)) r.ali.dat)
            done:he
          ?:  (~(has in (reachable-takos r.ali.dat)) r.bob.dat)
            $(gem.dat %fine)
          =+  r=(find-merge-points:he ali.dat bob.dat)
          ?~  r
            (error:he %merge-no-merge-base ~)
          ?.  ?=({* $~ $~} r)
            =+  (lent (~(tap in `(set yaki)`r)))
            (error:he %merge-criss-cross >[-]< ~)
          =.  bas.dat  n.r
          ?:  ?=(?($mate $meld) gem.dat)
            diff-ali
          =.  new.dal.dat
            %-  molt
            %+  skip  (~(tap by q.ali.dat))
            |=  {pax/path lob/lobe}
            (~(has by q.bas.dat) pax)
          =.  cal.dal.dat
            %-  molt
            %+  skip  (~(tap by q.ali.dat))
            |=  {pax/path lob/lobe}
            =+  (~(get by q.bas.dat) pax)
            |(=(~ -) =([~ lob] -))
          =.  can.dal.dat
            ~
          =.  old.dal.dat
            %-  malt  ^-  (list {path $~})
            %+  murn  (~(tap by q.bas.dat))
            |=  {pax/path lob/lobe}
            ^-  (unit (pair path $~))
            ?.  =(~ (~(get by q.ali.dat) pax))
              ~
            `[pax ~]
          =.  new.dob.dat
            %-  molt
            %+  skip  (~(tap by q.bob.dat))
            |=  {pax/path lob/lobe}
            (~(has by q.bas.dat) pax)
          =.  cal.dob.dat
            %-  molt
            %+  skip  (~(tap by q.bob.dat))
            |=  {pax/path lob/lobe}
            =+  (~(get by q.bas.dat) pax)
            |(=(~ -) =([~ lob] -))
          =.  can.dob.dat
            ~
          =.  old.dob.dat
            %-  malt  ^-  (list {path $~})
            %+  murn  (~(tap by q.bas.dat))
            |=  {pax/path lob/lobe}
            ^-  (unit (pair path $~))
            ?.  =(~ (~(get by q.bob.dat) pax))
              ~
            `[pax ~]
          =+  ^=  bof
              %-  %~  int  by
                    %-  ~(uni by `(map path *)`new.dal.dat)
                    %-  ~(uni by `(map path *)`cal.dal.dat)
                    %-  ~(uni by `(map path *)`can.dal.dat)
                    `(map path *)`old.dal.dat
                  ==
              %-  ~(uni by `(map path *)`new.dob.dat)
              %-  ~(uni by `(map path *)`cal.dob.dat)
              %-  ~(uni by `(map path *)`can.dob.dat)
              `(map path *)`old.dob.dat
          ?^  bof
            (error:he %meet-conflict >(~(run by `(map path *)`bof) $~)< ~)
          =+  ^-  old/(map path lobe)
              %+  roll  (~(tap by (~(uni by old.dal.dat) old.dob.dat)))
              =<  .(old q.bas.dat)
              |=  {{pax/path $~} old/(map path lobe)}
              (~(del by old) pax)
          =+  ^=  hat
              %-  ~(uni by old)
              %-  ~(uni by new.dal.dat)
              %-  ~(uni by new.dob.dat)
              %-  ~(uni by cal.dal.dat)
              cal.dob.dat
          =+  ^-  del/(map path ?)
              (~(run by (~(uni by old.dal.dat) old.dob.dat)) |=($~ %|))
          =.  new.dat
            (make-yaki [r.ali.dat r.bob.dat ~] hat now)
          =.  hut.ran  (~(put by hut.ran) r.new.dat new.dat)
          =.  erg.dat  %-  ~(uni by del)
                       ^-  (map path ?)
                       %.  |=(lobe %&)
                       ~(run by (~(uni by new.dal.dat) cal.dal.dat))
          checkout
        ==
      ::
      ::  Common code for ++diff-ali and ++diff-bob.
      ::
      ::  Diffs a commit against a the mergebase.  Result comes back in either
      ::  ++diffed-ali or ++diffed-ali.
      ::
      ++  diff-bas
        |=  {nam/term yak/yaki oth/(trel ship desk case) yuk/yaki}
        ^+  +>
        %-  emit
        :*  hen  %pass
            =+  (cat 3 %diff- nam)
            [%merge (scot %p p.bob) q.bob (scot %p p.ali) q.ali - ~]
            %f  %exec  p.bob  ~  [p.oth q.oth r.oth]  %tabl
            ^-  (list (pair silk:ford silk:ford))
            %+  murn  (~(tap by q.bas.dat))
            |=  {pax/path lob/lobe}
            ^-  (unit (pair silk:ford silk:ford))
            =+  a=(~(get by q.yak) pax)
            ?~  a
              ~
            ?:  =(lob u.a)
              ~
            =+  (~(get by q.yuk) pax)
            ?~  -
              ~
            ?:  =(u.a u.-)
              ~
            :-  ~
            :-  [%$ %path !>(pax)]
            [%diff (lobe-to-silk pax lob) (lobe-to-silk pax u.a)]
        ==
      ::
      ::  Diff ali's commit against the mergebase.
      ::
      ++  diff-ali
        ^+  .
        (diff-bas(wat.dat %diff-ali) %ali ali.dat [p.ali q.ali cas.dat] bob.dat)
      ::
      ::  Store the diff of ali's commit versus the mergebase in dal.dat and
      ::  call ++diff-bob.
      ::  
      ++  diffed-ali
        |=  res/gage:ford
        ^+  +>
        =+  tay=(gage-to-cages-or-error res)
        ?:  ?=($| -.tay)
          (error:he %diff-ali-bad-made leaf+"merge diff ali failed" p.tay)
        =+  can=(cages-to-map p.tay)
        ?:  ?=($| -.can)
          (error:he %diff-ali p.can)
        ?:  ?=($| -.gon.dat)
          +>.$
        =.  new.dal.dat
          %-  molt
          %+  skip  (~(tap by q.ali.dat))
          |=  {pax/path lob/lobe}
          (~(has by q.bas.dat) pax)
        =.  cal.dal.dat
          %-  molt  ^-  (list (pair path lobe))
          %+  murn  (~(tap by q.bas.dat))
          |=  {pax/path lob/lobe}
          ^-  (unit (pair path lobe))
          =+  a=(~(get by q.ali.dat) pax)
          =+  b=(~(get by q.bob.dat) pax)
          ?.  ?&  ?=(^ a)
                  !=([~ lob] a)
                  =([~ lob] b)
              ==
            ~
          `[pax +.a]
        =.  can.dal.dat  p.can
        =.  old.dal.dat
          %-  malt  ^-  (list {path $~})
          %+  murn  (~(tap by q.bas.dat))
          |=  {pax/path lob/lobe}
          ?.  =(~ (~(get by q.ali.dat) pax))
            ~
          (some pax ~)
        diff-bob
      ::
      ::  Diff bob's commit against the mergebase.
      ::
      ++  diff-bob
        ^+  .
        (diff-bas(wat.dat %diff-bob) %bob bob.dat [p.bob q.bob da+now] ali.dat)
      ::
      ::  Store the diff of bob's commit versus the mergebase in dob.dat and
      ::  call ++merge.
      ::
      ++  diffed-bob
        |=  res/gage:ford
        ^+  +>
        =+  tay=(gage-to-cages-or-error res)
        ?:  ?=($| -.tay)
          (error:he %diff-bob-bad-made leaf+"merge diff bob failed" p.tay)
        =+  can=(cages-to-map p.tay)
        ?:  ?=($| -.can)
          (error:he %diff-bob p.can)
        ?:  ?=($| -.gon.dat)
          +>.$
        =.  new.dob.dat
          %-  molt
          %+  skip  (~(tap by q.bob.dat))
          |=  {pax/path lob/lobe}
          (~(has by q.bas.dat) pax)
        =.  cal.dob.dat
          %-  molt  ^-  (list (pair path lobe))
          %+  murn  (~(tap by q.bas.dat))
          |=  {pax/path lob/lobe}
          ^-  (unit (pair path lobe))
          =+  a=(~(get by q.ali.dat) pax)
          =+  b=(~(get by q.bob.dat) pax)
          ?.  ?&  ?=(^ b)
                  !=([~ lob] b)
                  =([~ lob] a)
              ==
            ~
          `[pax +.b]
        =.  can.dob.dat  p.can
        =.  old.dob.dat
          %-  malt  ^-  (list {path $~})
          %+  murn  (~(tap by q.bas.dat))
          |=  {pax/path lob/lobe}
          ?.  =(~ (~(get by q.bob.dat) pax))
            ~
          (some pax ~)
        merge
      ::
      ::  Merge the conflicting diffs in can.dat.dat and can.dob.dat.
      ::
      ::  Result is handled in ++merged.
      ::
      ++  merge
        ^+  .
        |-  ^+  +.$
        ?+    gem.dat  ~|  [%merge-weird-gem gem.dat]  !!
            ?($mate $meld)
          %-  emit(wat.dat %merge)
          :*  hen  %pass
              [%merge (scot %p p.bob) q.bob (scot %p p.ali) q.ali %merge ~]
              %f  %exec  p.bob  ~  [p.bob q.bob da+now]  %tabl
              ^-  (list (pair silk:ford silk:ford))
              %+  turn  (~(tap by (~(int by can.dal.dat) can.dob.dat)))
              |=  {pax/path *}
              ^-  (pair silk:ford silk:ford)
              =+  cal=(~(got by can.dal.dat) pax)
              =+  cob=(~(got by can.dob.dat) pax)
              =+  ^=  her
                  =+  (slag (dec (lent pax)) pax)
                  ?~(- %$ i.-)
              :-  [%$ %path !>(pax)]
              [%join her [%$ cal] [%$ cob]]
          ==
        ==
      ::
      ::  Put merged changes in bof.dat and call ++build.
      ::
      ++  merged
        |=  res/gage:ford
        =+  tay=(gage-to-cages-or-error res)
        ?:  ?=($| -.tay)
          (error:he %merge-bad-made leaf+"merging failed" p.tay)
        =+  can=(cages-to-map p.tay)
        ?:  ?=($| -.can)
          (error:he %merge p.can)
        =+  bof=(~(run by p.can) (flit |=({a/mark ^} !?=($null a))))
        ?:  ?=($| -.gon.dat)
          +>.$
        =.  bof.dat  bof
        build
      ::
      ::  Apply the patches in bof.dat to get the new merged content.
      ::
      ::  Result is handled in ++built
      ::
      ++  build
        ^+  .
        %-  emit(wat.dat %build)
        :*  hen  %pass
            [%merge (scot %p p.bob) q.bob (scot %p p.ali) q.ali %build ~]
            %f  %exec  p.bob  ~  [p.bob q.bob da+now]  %tabl
            ^-  (list (pair silk:ford silk:ford))
            %+  murn  (~(tap by bof.dat))
            |=  {pax/path cay/(unit cage)}
            ^-  (unit (pair silk:ford silk:ford))
            ?~  cay
              ~
            :-  ~
            :-  [%$ %path !>(pax)]
            =+  (~(get by q.bas.dat) pax)
            ?~  -
              ~|  %mate-strange-diff-no-base
              !!
            [%pact (lobe-to-silk pax u.-) [%$ u.cay]]
        ==
      ::
      ::  Create new commit.
      ::
      ::  Gather all the changes between ali's and bob's commits and the
      ::  mergebase.  This is similar to the %meet of ++fetched-ali, except
      ::  where they touch the same file, we use the merged versions we created
      ::  earlier (bop.dat).
      ::
      ::  Sum all the changes into a new commit (new.dat), and checkout.
      ::
      ++  built
        |=  res/gage:ford
        ^+  +>
        =+  tay=(gage-to-cages-or-error res)
        ?:  ?=($| -.tay)
          (error:he %build-bad-made leaf+"delta building failed" p.tay)
        =+  bop=(cages-to-map p.tay)
        ?:  ?=($| -.bop)
          (error:he %built p.bop)
        ?:  ?=($| -.gon.dat)
          +>.$
        =.  bop.dat  p.bop
        =+  ^-  con/(map path *)                        ::  2-change conflict
            %-  molt
            %+  skim  (~(tap by bof.dat))
            |=({pax/path cay/(unit cage)} ?=($~ cay))
        =+  ^-  cas/(map path lobe)                     ::  conflict base
            %-  ~(urn by con)
            |=  {pax/path *}
            (~(got by q.bas.dat) pax)
        =.  con                                         ::  change+del conflict
          %-  ~(uni by con)
          %-  malt  ^-  (list {path *})
          %+  skim  (~(tap by old.dal.dat))
          |=  {pax/path $~}
          ?:  (~(has by new.dob.dat) pax)
            ~|  %strange-add-and-del
            !!
          (~(has by can.dob.dat) pax)
        =.  con                                         ::  change+del conflict
          %-  ~(uni by con)
          %-  malt  ^-  (list {path *})
          %+  skim  (~(tap by old.dob.dat))
          |=  {pax/path $~}
          ?:  (~(has by new.dal.dat) pax)
            ~|  %strange-del-and-add
            !!
          (~(has by can.dal.dat) pax)
        =.  con                                         ::  add+add conflict
          %-  ~(uni by con)
          %-  malt  ^-  (list {path *})
          %+  skip  (~(tap by (~(int by new.dal.dat) new.dob.dat)))
          |=  {pax/path *}
          =((~(got by new.dal.dat) pax) (~(got by new.dob.dat) pax))
        ?:  &(?=($mate gem.dat) ?=(^ con))
          =+  (turn (~(tap by `(map path *)`con)) |=({path *} >[+<-]<))
          (error:he %mate-conflict -)
        =+  ^-  old/(map path lobe)                     ::  oldies but goodies
            %+  roll  (~(tap by (~(uni by old.dal.dat) old.dob.dat)))
            =<  .(old q.bas.dat)
            |=  {{pax/path $~} old/(map path lobe)}
            (~(del by old) pax)
        =+  ^-  can/(map path cage)                     ::  content changes
            %-  molt
            ^-  (list (pair path cage))
            %+  murn  (~(tap by bof.dat))
            |=  {pax/path cay/(unit cage)}
            ^-  (unit (pair path cage))
            ?~  cay
              ~
            `[pax u.cay]
        =^  hot  lat.ran                                ::  new content
          ^-  {(map path lobe) (map lobe blob)}
          %+  roll  (~(tap by can))
          =<  .(lat lat.ran)
          |=  {{pax/path cay/cage} hat/(map path lobe) lat/(map lobe blob)}
          =+  ^=  bol
              =+  (~(get by q.bas.dat) pax)
              ?~  -
                ~|  %mate-strange-diff-no-base
                !!
              %^    make-delta-blob
                  (page-to-lobe [p q.q]:(~(got by bop.dat) pax))
                [(lobe-to-mark u.-) u.-]
              [p q.q]:cay
          [(~(put by hat) pax p.bol) (~(put by lat) p.bol bol)]
        ::  ~&  old=(~(run by old) mug)
        ::  ~&  newdal=(~(run by new.dal.dat) mug)
        ::  ~&  newdob=(~(run by new.dob.dat) mug)
        ::  ~&  caldal=(~(run by cal.dal.dat) mug)
        ::  ~&  caldob=(~(run by cal.dob.dat) mug)
        ::  ~&  hot=(~(run by hot) mug)
        ::  ~&  cas=(~(run by cas) mug)
        =+  ^-  hat/(map path lobe)                     ::  all the content
          %-  ~(uni by old)
          %-  ~(uni by new.dal.dat)
          %-  ~(uni by new.dob.dat)
          %-  ~(uni by cal.dal.dat)
          %-  ~(uni by cal.dob.dat)
          %-  ~(uni by hot)
          cas
        ::  ~&  >  hat=(~(run by hat) mug)
        =+  ^-  del/(map path ?)
            (~(run by (~(uni by old.dal.dat) old.dob.dat)) |=($~ %|))
        =.  gon.dat  [%& (silt (turn (~(tap by con)) head))]
        =.  new.dat
          (make-yaki [r.ali.dat r.bob.dat ~] hat now)
        =.  hut.ran  (~(put by hut.ran) r.new.dat new.dat)
        =.  erg.dat  %-  ~(uni by del)
                     ^-  (map path ?)
                     %.  |=(lobe %&)
                     %~  run  by
                       %-  ~(uni by new.dal.dat)
                       %-  ~(uni by cal.dal.dat)
                       %-  ~(uni by cas)
                       hot
                     ==
        checkout
      ::
      ::  Convert new commit into actual data (i.e. blobs rather than lobes).
      ::
      ::  Result is handled in ++checked-out.
      ::
      ++  checkout
        ^+  .
        =+  ^-  val/beak
            ?:  ?=($init gem.dat)
              [p.ali q.ali cas.dat]
            [p.bob q.bob da+now]
        %-  emit(wat.dat %checkout)
        :*  hen  %pass
            [%merge (scot %p p.bob) q.bob (scot %p p.ali) q.ali %checkout ~]
            %f  %exec  p.bob  ~  val  %tabl
            ^-  (list (pair silk:ford silk:ford))
            %+  murn  (~(tap by q.new.dat))
            |=  {pax/path lob/lobe}
            ^-  (unit (pair silk:ford silk:ford))
            ?:  (~(has by bop.dat) pax)
              ~
            `[[%$ %path !>(pax)] (merge-lobe-to-silk:he pax lob)]
        ==
      ::
      ::  Apply the new commit to our state and, if we need to tell unix about
      ::  some of the changes, call ++ergo.
      ::
      ++  checked-out
        |=  res/gage:ford
        ^+  +>
        =+  tay=(gage-to-cages-or-error res)
        ?:  ?=($| -.tay)
          (error:he %checkout-bad-made leaf+"merge checkout failed" p.tay)
        =+  can=(cages-to-map p.tay)
        ?:  ?=($| -.can)
          (error:he %checkout p.can)
        ?:  ?=($| -.gon.dat)
          +>.$
        =.  let.dom  +(let.dom)
        =.  hit.dom  (~(put by hit.dom) let.dom r.new.dat)
        =.  ank.dat
          %-  map-to-ankh:ze
          %-  ~(run by (~(uni by bop.dat) p.can))
          |=(cage [(page-to-lobe p q.q) +<])
        =.  ank.dom  ank.dat
        =>  .(..wake wake)
        ?~  hez  done:he
        =+  mus=(must-ergo (turn (~(tap by erg.dat)) head))
        ?:  =(~ mus)  done:he
        ergo
      ::
      ::  Cast all the content that we're going to tell unix about to %mime.
      ::
      ::  Result is handled in ++ergoed.
      ::
      ++  ergo
        ^+  .
        =+  ^-  sum/(set path)
            =+  (must-ergo (turn (~(tap by erg.dat)) head))
            =+  (turn (~(tap by -)) (corl tail tail))
            %+  roll  -
            |=  {pak/(set path) acc/(set path)}
            (~(uni in acc) pak)
        =+  zez=ze(ank.dom ank.dat)
        =+  ^-  val/beak
            ?:  ?=($init gem.dat)
              [p.ali q.ali cas.dat]
            [p.bob q.bob da+now]
        %-  emit(wat.dat %ergo)
        :*  hen  %pass
            [%merge (scot %p p.bob) q.bob (scot %p p.ali) q.ali %ergo ~]
            %f  %exec  p.bob  ~  val  %tabl
            ^-  (list (pair silk:ford silk:ford))
            %+  turn  (~(tap in sum))
            |=  a/path
            ^-  (pair silk:ford silk:ford)
            :-  [%$ %path !>(a)]
            =+  b=(~(got by erg.dat) a)
            ?.  b
              [%$ %null !>(~)]
            :+  %cast  %mime
            (lobe-to-silk:zez a (~(got by q.new.dat) a))
        ==
      ::
      ::  Tell unix about the changes made by the merge.
      ::
      ++  ergoed
        |=  res/gage:ford
        ^+  +>
        =+  tay=(gage-to-cages-or-error res)
        ?:  ?=($| -.tay)
          (error:he %ergo-bad-made leaf+"merge ergo failed" p.tay)
        =+  =|  nac/mode
            |-  ^-  tan/$^(mode {p/term q/tang})
            ?~  p.tay  nac
            =*  pax  p.i.p.tay
            ?.  ?=($path p.pax)
              [%ergo >[%expected-path got=p.pax]< ~]
            =*  mim  q.i.p.tay
            =+  mit=?.(?=($mime p.mim) ~ `((hard mime) q.q.mim))
            $(p.tay t.p.tay, nac :_(nac [((hard path) q.q.pax) mit]))
        ?:  ?=({@ *} tan)  (error:he tan)
        =+  `can/(map path (unit mime))`(malt tan)
        ?~  hez
          (error:he %ergo-no-hez ~)
        ?:  ?=($| -.gon.dat)
          +>.$
        =+  mus=(must-ergo (turn (~(tap by erg.dat)) head))
        =<  done:he
        %-  emil
        %+  turn  (~(tap by mus))
        |=  {pot/term len/@ud pak/(set path)}
        :*  u.hez  %give  %ergo  pot
            %+  turn  (~(tap in pak))
            |=  pax/path
            [(slag len pax) (~(got by can) pax)]
        ==
      ::
      ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      ::
      ::  This core is a small set of helper functions to assist in merging.
      ::
      ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      ++  he
        |%
        ::
        ::  Assert that we're goig to be returning something, and set don to
        ::  true, so that ++abet knows we're done.
        ::
        ++  done
          ^+  ..he
          ?<  ?=($| -.gon.dat)
          ..he(don |)
        ::
        ::  Cancel the merge gracefully and produce an error.
        ::
        ++  error
          |=  {err/term tan/(list tank)}
          ^+  ..he
          ..he(don |, gon.dat [%| err >ali< >bob< >cas.dat< >gem.dat< tan])
        ::
        ::  Create a silk to turn a lobe into a blob.
        ::
        ::  We short-circuit if we already have the content somewhere.
        ::
        ++  merge-lobe-to-silk
          |=  {pax/path lob/lobe}
          ^-  silk:ford
          =+  hat=q.ali.dat
          =+  hot=q.bob.dat
          =+  ^=  lal
              %+  biff  alh
              |=  had/dome
              (~(get by q:(tako-to-yaki (~(got by hit.had) let.had))) pax)
          =+  lol=(~(get by hot) pax)
          |-  ^-  silk:ford
          ?:  =([~ lob] lol)
            =+  (need (need (read-x let.dom pax)))
            ?>  ?=($& -<)
            [%$ p.-]
          ?:  =([~ lob] lal)
            [%$ +:(need fil.ank:(descend-path:(zu ank:(need alh)) pax))]
          =+  bol=(~(got by lat.ran) lob)
          ?-  -.bol
            $direct     (page-to-silk q.bol)
            $delta      [%pact $(lob q.q.bol) (page-to-silk r.bol)]
          ==
        ::
        ::  Find the most recent common ancestor(s).
        ::
        ++  find-merge-points
          |=  {p/yaki q/yaki}                           ::  maybe need jet
          ^-  (set yaki)
          %-  reduce-merge-points
          =+  r=(reachable-takos r.p)
          |-  ^-  (set yaki)
          ?:  (~(has in r) r.q)  (~(put in *(set yaki)) q)
          %+  roll  p.q
          |=  {t/tako s/(set yaki)}
          ?:  (~(has in r) t)
            (~(put in s) (tako-to-yaki t))              ::  found
          (~(uni in s) ^$(q (tako-to-yaki t)))          ::  traverse
        ::
        ::  Helper for ++find-merge-points.
        ::
        ++  reduce-merge-points
          |=  unk/(set yaki)                            ::  maybe need jet
          =|  gud/(set yaki)
          =+  ^=  zar
              ^-  (map tako (set tako))
              %+  roll  (~(tap in unk))
              |=  {yak/yaki qar/(map tako (set tako))}
              (~(put by qar) r.yak (reachable-takos r.yak))
          |-
          ^-  (set yaki)
          ?~  unk  gud
          =+  bun=(~(del in `(set yaki)`unk) n.unk)
          ?:  %+  levy  (~(tap by (~(uni in gud) bun)) ~)
              |=  yak/yaki
              !(~(has in (~(got by zar) r.yak)) r.n.unk)
            $(gud (~(put in gud) n.unk), unk bun)
          $(unk bun)
        --
      --
    --
  --
--
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::              section 4cA, filesystem vane
::
::  This is the arvo interface vane.  Our formal state is a `++raft`, which
::  has five components:
::
::  --  `fat` is the state for all local desks.
::  --  `hoy` is the state for all foreign desks.
::  --  `ran` is the global, hash-addressed object store.
::  --  `mon` is the set of mount points in unix.
::  --  `hez` is the duct to the unix sync.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
=|                                                    ::  instrument state
    $:  $1                                            ::  vane version
        ruf/raft                                      ::  revision tree
    ==                                                ::
|=  {now/@da eny/@ ski/sley}                          ::  activate
^?                                                    ::  opaque core
|%                                                    ::
++  call                                              ::  handle request
  |=  $:  hen/duct
          hic/(hypo (hobo task:able))
      ==
  =>  %=    .                                         ::  XX temporary
          q.hic
        ^-  task:able
        ?:  ?=($soft -.q.hic)
          =+
          ~|([%bad-soft (@t -.p.q.hic)] ((soft task:able) p.q.hic))
          ?~  -
            ~&  [%bad-softing (@t -.p.q.hic)]  !!
          u.-
        ?:  (~(nest ut -:!>(*task:able)) | p.hic)  q.hic
        ~&  [%clay-call-flub (@tas `*`-.q.hic)]
        ((hard task:able) q.hic)
      ==
  ^+  [p=*(list move) q=..^$]
  ?-    -.q.hic
      $boat
    :_  ..^$
    [hen %give %hill (turn (~(tap by mon.ruf)) head)]~
  ::
      $drop
    =^  mos  ruf
      =+  den=((de now hen ruf) [. .]:p.q.hic q.q.hic)
      abet:drop-me:den
    [mos ..^$]
  ::
      $info
    ?:  =(%$ q.q.hic)
      [~ ..^$]
    =^  mos  ruf
      =+  den=((de now hen ruf) [. .]:p.q.hic q.q.hic)
      abet:(edit:den now r.q.hic)
    [mos ..^$]
  ::
      $init
    :_  %_    ..^$
            fat.ruf
          ?<  (~(has by fat.ruf) p.q.hic)
          (~(put by fat.ruf) p.q.hic [-(hun hen)]:[*room .])
        ==
    =+  [bos=(sein:title p.q.hic) can=(clan:title p.q.hic)]
    %-  zing  ^-  (list (list move))
    :~  ?:  =(bos p.q.hic)  ~
        [hen %pass /init-merge %c %merg p.q.hic %base bos %kids da+now %init]~
    ::
        ~
    ==
  ::
      $into
    =.  hez.ruf  `hen
    :_  ..^$
    =+  bem=(~(get by mon.ruf) p.q.hic)
    ?:  &(?=($~ bem) !=(%$ p.q.hic))
      ~|([%bad-mount-point-from-unix p.q.hic] !!)
    =+  ^-  bem/beam
        ?^  bem
          u.bem
        [[?>(?=(^ fat.ruf) p.n.fat.ruf) %base %ud 1] ~]
    =+  rom=(~(get by fat.ruf) p.bem)
    ?~  rom
      ~
    =+  dos=(~(get by dos.u.rom) q.bem)
    ?~  dos
      ~
    ?:  =(0 let.dom.u.dos)
      =+  cos=(mode-to-soba ~ s.bem q.q.hic r.q.hic)
      =+  ^-  {one/(list {path miso}) two/(list {path miso})}
          %+  skid  cos
          |=  {a/path b/miso}
          ?&  ?=($ins -.b)
              ?=($mime p.p.b)
              ?=({$hoon $~} (slag (dec (lent a)) a))
          ==
      :~  [hen %pass /one %c %info p.bem q.bem %& one]
          [hen %pass /two %c %info p.bem q.bem %& two]
      ==
    =+  yak=(~(got by hut.ran.ruf) (~(got by hit.dom.u.dos) let.dom.u.dos))
    =+  cos=(mode-to-soba q.yak (flop s.bem) q.q.hic r.q.hic)
    [hen %pass /both %c %info p.bem q.bem %& cos]~
  ::
      $merg                                               ::  direct state up
    ?:  =(%$ q.q.hic)
      [~ ..^$]
    =^  mos  ruf
      =+  den=((de now hen ruf) [. .]:p.q.hic q.q.hic)
      abet:abet:(start:(me:ze:den [r.q.hic s.q.hic] ~ &) t.q.hic u.q.hic)
    [mos ..^$]
  ::
      $mont
    =.  hez.ruf  ?^(hez.ruf hez.ruf `[[%$ %sync ~] ~])
    =+  pot=(~(get by mon.ruf) p.q.hic)
    ?^  pot
      ~&  [%already-mounted pot]
      [~ ..^$]
    =.  mon.ruf
      (~(put by mon.ruf) p.q.hic [q.q.hic r.q.hic %ud 0] (flop s.q.hic))
    =+  yar=(~(get by fat.ruf) q.q.hic)
    ?~  yar
      [~ ..^$]
    =+  dos=(~(get by dos.u.yar) r.q.hic)
    ?~  dos
      [~ ..^$]
    =^  mos  ruf
      =+  den=((de now hen ruf) [. .]:q.q.hic r.q.hic)
      abet:(mont:den p.q.hic s.q.hic)
    [mos ..^$]
  ::
      $ogre
    ?~  hez.ruf
      ~&  %no-sync-duct
      [~ ..^$]
    ?@  p.q.hic
      ?.  (~(has by mon.ruf) p.q.hic)
        ~&  [%not-mounted p.q.hic]
        [~ ..^$]
      :_  ..^$(mon.ruf (~(del by mon.ruf) p.q.hic))
      [u.hez.ruf %give %ogre p.q.hic]~
    :_  %_    ..^$
            mon.ruf
          %-  molt
          %+  skip  (~(tap by mon.ruf))
          (corl (cury test p.q.hic) tail)
        ==
    %+  turn
      (skim (~(tap by mon.ruf)) (corl (cury test p.q.hic) tail))
    |=  {pot/term bem/beam}
    [u.hez.ruf %give %ogre pot]
  ::
      $warp
    =^  mos  ruf
      =+  den=((de now hen ruf) p.q.hic p.q.q.hic)
      ::  =-  ~?  ?=([~ %sing %w *] q.q.q.hic)
      ::        :*  %someones-warping
      ::            rav=u.q.q.q.hic
      ::            mos=-<
      ::        ==
      ::      -
      =<  abet
      ?~  q.q.q.hic
        cancel-request:den
      (start-request:den u.q.q.q.hic)
    [mos ..^$]
  ::
      $went
    ::  this won't happen until we send responses.
    !!
  ::
      $west
    ?:  ?=({$question *} q.q.hic)
      =+  ryf=((hard riff) r.q.hic)
      :_  ..^$
      :~  [hen %give %mack ~]
          :-  hen
          :^  %pass  [(scot %p p.p.q.hic) (scot %p q.p.q.hic) t.q.q.hic]
            %c
          [%warp [p.p.q.hic p.p.q.hic] ryf]
      ==
    ?>  ?=({$answer @ @ $~} q.q.hic)
    =+  syd=(slav %tas i.t.q.q.hic)
    =+  inx=(slav %ud i.t.t.q.q.hic)
    =^  mos  ruf
      =+  den=((de now hen ruf) p.q.hic syd)
      abet:(take-foreign-update:den inx ((hard (unit rand)) r.q.hic))
    [[[hen %give %mack ~] mos] ..^$]
  ::
      $wegh
    :_  ..^$  :_  ~
    :^  hen  %give  %mass
    :-  %clay
    :-  %|
    :~  domestic+[%& fat.ruf]
        foreign+[%& hoy.ruf]
        :-  %object-store  :-  %|
        :~  commits+[%& hut.ran.ruf]
            blobs+[%& lat.ran.ruf]
        ==
    ==
  ==
::
::  All timers are handled by `%behn` nowadays.
++  doze
  |=  {now/@da hen/duct}
  ^-  (unit @da)
  ~
::
++  load
  =>  |%
      ++  cult-0  (map duct rove)
      ++  dojo-0  (cork dojo |=(a/dojo a(qyx *cult-0)))
      ++  rede-0  (cork rede |=(a/rede a(qyx *cult-0)))
      ++  room-0  (cork room |=(a/room a(dos (~(run by dos.a) dojo-0))))
      ++  rung-0  (cork rung |=(a/rung a(rus (~(run by rus.a) rede-0))))
      ++  raft-0
        %+  cork  raft
        |=(a/raft a(fat (~(run by fat.a) room-0), hoy (~(run by hoy.a) rung-0)))
      ++  axle    $%({$0 ruf/raft-0} {$1 ruf/raft})
      --
  |=  old/axle
  ^+  ..^$
  ?-  -.old
    $1  ..^$(ruf ruf.old)
    $0  =/  cul
          |=  a/cult-0  ^-  cult
          %-  ~(gas ju *cult)
          (turn (~(tap by a)) |=({p/duct q/rove} [q p]))
        =/  rom
          =+  doj=|=(a/dojo-0 a(qyx (cul qyx.a)))
          |=(a/room-0 a(dos (~(run by dos.a) doj)))
        =/  run
          =+  red=|=(a/rede-0 a(qyx (cul qyx.a)))
          |=(a/rung-0 a(rus (~(run by rus.a) red)))
        =+  r=ruf.old
        $(old [%1 r(fat (~(run by fat.r) rom), hoy (~(run by hoy.r) run))])
  ==
::
++  scry                                              ::  inspect
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=($& -.why)  ~
  =*  his  p.why
  ::  ~&  scry+[ren `path`[(scot %p his) syd ~(rent co lot) tyl]]
  ::  =-  ~&  %scry-done  -
  =+  got=(~(has by fat.ruf) his)
  =+  luk=?.(?=($$ -.lot) ~ ((soft case) p.lot))
  ?~  luk  [~ ~]
  ?:  =(%$ ren)
    [~ ~]
  =+  run=((soft care) ren)
  ?~  run  [~ ~]
  =+  den=((de now [/scryduct ~] ruf) [. .]:his syd)
  =+  (aver:den u.run u.luk tyl)
  ?~  -               -
  ?~  u.-             -
  ?:  ?=($& -.u.u.-)  ``p.u.u.-
  ~
::
++  stay  [%1 ruf]
++  take                                              ::  accept response
  |=  {tea/wire hen/duct hin/(hypo sign)}
  ^+  [p=*(list move) q=..^$]
  ?:  ?=({$merge @ @ @ @ @ $~} tea)
    ?>  ?=(?($writ $made) +<.q.hin)
    =+  our=(slav %p i.t.tea)
    =*  syd  i.t.t.tea
    =+  her=(slav %p i.t.t.t.tea)
    =*  sud  i.t.t.t.t.tea
    =*  sat  i.t.t.t.t.t.tea
    =+  dat=?-(+<.q.hin $writ [%& p.q.hin], $made [%| q.q.hin])
    =+  ^-  kan/(unit dome)
        %+  biff  (~(get by fat.ruf) her)
        |=  room
        %+  bind  (~(get by dos) sud)
        |=  dojo
        dom
    =^  mos  ruf
      =+  den=((de now hen ruf) [. .]:our syd)
      abet:abet:(route:(me:ze:den [her sud] kan |) sat dat)
    [mos ..^$]
  ?:  ?=({$blab care @ @ *} tea)
    ?>  ?=($made +<.q.hin)
    ?.  ?=($& -.q.q.hin)
      ~|  %blab-fail
      ~>  %mean.|.(?+(-.q.q.hin -.q.q.hin $| p.q.q.hin))
      !!                              ::  interpolate ford fail into stack trace
    :_  ..^$  :_  ~
    :*  hen  %give  %writ  ~
        ^-  {care case @tas}
        [i.t.tea ((hard case) +>:(slay i.t.t.tea)) i.t.t.t.tea]
    ::
        `path`t.t.t.t.tea
        `cage`p.q.q.hin
    ==
  ?-    -.+.q.hin
      $crud
    [[[hen %slip %d %flog +.q.hin] ~] ..^$]
  ::
      $made
    ?~  tea  !!
    ?+    -.tea  !!
        $inserting
      ?>  ?=({@ @ @ $~} t.tea)
      =+  our=(slav %p i.t.tea)
      =+  syd=(slav %tas i.t.t.tea)
      =+  wen=(slav %da i.t.t.t.tea)
      =^  mos  ruf
        =+  den=((de now hen ruf) [. .]:our syd)
        abet:(take-inserting:den wen q.q.hin)
      [mos ..^$]
    ::
        $diffing
      ?>  ?=({@ @ @ $~} t.tea)
      =+  our=(slav %p i.t.tea)
      =+  syd=(slav %tas i.t.t.tea)
      =+  wen=(slav %da i.t.t.t.tea)
      =^  mos  ruf
        =+  den=((de now hen ruf) [. .]:our syd)
        abet:(take-diffing:den wen q.q.hin)
      [mos ..^$]
    ::
        $castifying
      ?>  ?=({@ @ @ $~} t.tea)
      =+  our=(slav %p i.t.tea)
      =+  syd=(slav %tas i.t.t.tea)
      =+  wen=(slav %da i.t.t.t.tea)
      =^  mos  ruf
        =+  den=((de now hen ruf) [. .]:our syd)
        abet:(take-castify:den wen q.q.hin)
      [mos ..^$]
    ::
        $mutating
      ?>  ?=({@ @ @ $~} t.tea)
      =+  our=(slav %p i.t.tea)
      =+  syd=(slav %tas i.t.t.tea)
      =+  wen=(slav %da i.t.t.t.tea)
      =^  mos  ruf
        =+  den=((de now hen ruf) [. .]:our syd)
        abet:(take-mutating:den wen q.q.hin)
      [mos ..^$]
    ::
        $patching
      ?>  ?=({@ @ $~} t.tea)
      =+  our=(slav %p i.t.tea)
      =+  syd=(slav %tas i.t.t.tea)
      =^  mos  ruf
        =+  den=((de now hen ruf) [. .]:our syd)
        abet:(take-patch:den q.q.hin)
      [mos ..^$]
    ::
        $ergoing
      ?>  ?=({@ @ $~} t.tea)
      =+  our=(slav %p i.t.tea)
      =+  syd=(slav %tas i.t.t.tea)
      =^  mos  ruf
        =+  den=((de now hen ruf) [. .]:our syd)
        abet:(take-ergo:den q.q.hin)
      [mos ..^$]
    ::
        $foreign-plops
      ?>  ?=({@ @ @ @ $~} t.tea)
      =+  our=(slav %p i.t.tea)
      =+  her=(slav %p i.t.t.tea)
      =*  syd  i.t.t.t.tea
      =+  lem=(slav %da i.t.t.t.t.tea)
      =^  mos  ruf
        =+  den=((de now hen ruf) [our her] syd)
        abet:(take-foreign-plops:den ?~(lem ~ `lem) q.q.hin)
      [mos ..^$]
    ::
        $foreign-x
      ?>  ?=({@ @ @ @ @ *} t.tea)
      =+  our=(slav %p i.t.tea)
      =+  her=(slav %p i.t.t.tea)
      =+  syd=(slav %tas i.t.t.t.tea)
      =+  car=((hard care) i.t.t.t.t.tea)
      =+  ^-  cas/case
          =+  (slay i.t.t.t.t.t.tea)
          ?>  ?=({$~ $$ case} -)
          ->+
      =*  pax  t.t.t.t.t.t.tea
      =^  mos  ruf
        =+  den=((de now hen ruf) [our her] syd)
        abet:(take-foreign-x:den car cas pax q.q.hin)
      [mos ..^$]
    ==
  ::
      $mere
    ?:  ?=($& -.p.+.q.hin)
      ~&  'initial merge succeeded'
      [~ ..^$]
    ~>  %slog.
        :^  0  %rose  [" " "[" "]"]
        :^    leaf+"initial merge failed"
            leaf+"my most sincere apologies"
          >p.p.p.+.q.hin<
        q.p.p.+.q.hin
    [~ ..^$]
  ::
      $note  [[hen %give +.q.hin]~ ..^$]
      $wake
    ~|  %why-wakey  !!
    ::  =+  dal=(turn (~(tap by fat.ruf) ~) |=([a=@p b=room] a))
    ::  =|  mos=(list move)
    ::  |-  ^-  [p=(list move) q=_..^^$]
    ::  ?~  dal  [mos ..^^$]
    ::  =+  une=(un i.dal now hen ruf)
    ::  =^  som  une  wake:une
    ::  $(dal t.dal, ruf abet:une, mos (weld som mos))
  ::
      $writ
    ?>  ?=({@ @ *} tea)
    ~|  i=i.tea
    ~|  it=i.t.tea
    =+  our=(slav %p i.tea)
    =+  him=(slav %p i.t.tea)
    :_  ..^$
    :~  :*  hen  %pass  /writ-wont  %a
            %wont  [our him]  [%c %answer t.t.tea]
            (bind p.+.q.hin rant-to-rand)
        ==
    ==
  ::
      $woot
    [~ ..^$]
    :: ?~  r.q.hin  [~ ..^$]
    :: ~&  [%clay-lost p.q.hin r.q.hin tea]
    :: [~ ..^$]
  ==
::
++  rant-to-rand
  |=  rant
  ^-  rand
  [p q [p q.q]:r]
::
++  mode-to-soba
  |=  {hat/(map path lobe) pax/path all/? mod/mode}
  ^-  soba
  %+  weld
    ^-  (list (pair path miso))
    ?.  all
      ~
    =+  mad=(malt mod)
    =+  len=(lent pax)
    =+  ^-  descendants/(list path)
        %+  turn
          %+  skim  (~(tap by hat))
          |=  {paf/path lob/lobe}
          =(pax (scag len paf))
        |=  {paf/path lob/lobe}
        (slag len paf)
    %+  murn
      descendants
    |=  pat/path
    ^-  (unit (pair path {$del $~}))
    ?:  (~(has by mad) pat)
      ~
    `[(weld pax pat) %del ~]
  ^-  (list (pair path miso))
  %+  murn  mod
  |=  {pat/path mim/(unit mime)}
  ^-  (unit (pair path miso))
  =+  paf=(weld pax pat)
  ?~  mim
    =+  (~(get by hat) paf)
    ?~  -
      ~&  [%deleting-already-gone pax pat]
      ~
    `[paf %del ~]
  =+  (~(get by hat) paf)
  ?~  -
    `[paf %ins %mime -:!>(*mime) u.mim]
  `[paf %mut %mime -:!>(*mime) u.mim]
--
