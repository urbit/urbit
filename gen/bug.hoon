::  "Hello world" sample generator
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
~&  %buggin
^%
:-  %say
|=  *
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
::  %d produces a set of desks, %u checks for existence, %v produces a ++dome of
::  all desk data, %w with a time or label case gets the aeon at that case, %w
::  with a number case is not recommended, %x gets file contents, %y gets a
::  directory listing, and %z gets a recursive hash of the file contents and
::  children.
::
:: ++  care  ?($d $u $v $w $x $y $z)
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
  $:  qyx/@ud                                          ::  subscribers
      dom/@ud                                          ::  desk state
      dok/(unit @ud)                                   ::  commit state
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
  $:  fat/(map @p room)                               ::  domestic
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
              {$many p/? q/moat r/(map path lobe)}      ::  change range
          ==                                            ::
::
::  Foreign desk data.
::
+=  rung  rus/(map desk rede)                           ::  neighbor desks
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
--
=>  |%
    ++  de
      |=  {now/@da hen/duct yex/ankh}
      !!
    --
:-  %noun
"hello, world"
