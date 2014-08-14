Clay
====

Clay is our filesystem.

data models
-----------

###`++raft`, formal state

```
++  raft                                                ::  filesystem
          $:  fat=(map ship room)                       ::  domestic
              hoy=(map ship rung)                       ::  foreign
              ran=rang                                  ::  hashes
          ==                                            ::
```

This is the state of our vane.  Anything that must be remembered between calls
to clay must be stored in this state.

`fat` is the set of domestic servers.  This stores all the information that is
specfic to a particular ship on this pier.  The keys to this map are the ships
on the current pier.

`hoy` is the set of foreign servers that we know anything about.  This stores
all the information that is specific to a particular foreign ship.  The keys to
this map are all the ships whose filesystems we have attempted to access
through clay.

`ran` is the store of all commits and deltas, keyed by hash.  The is where all
the "real" data we know is stored; the rest is "just bookkeeping".

###`++room`, filesystem per domestic ship

```
++  room                                                ::  fs per ship
          $:  hun=duct                                  ::  terminal duct
              hez=(unit duct)                           ::  sync duch
              dos=(map desk dojo)                       ::  native desk
          ==                                            ::
```

This is the representation of the filesystem of a ship on our pier.

`hun` is the duct that we use to send messages to dill to display notifications
of filesystem changes.  Only `%note` gifts should be produced along this duct.
This is set by the `%init` kiss.

`hez`, if present, is the duct we use to send sync messages to unix so that
they end up in the pier unix directory.  Only `%ergo` gifts should be producd
along this duct.  This is set by `%into` and `%invo` gifts.

`dos` is a well-known operating system released in 1981.  It is also the set of
desks on this ship, mapped to their data.

###`++desk`, filesystem branch

```
++  desk  ,@tas                                         ::  ship desk case spur
```

This is the name of a branch of the filesystem.  The default desks are "arvo",
"main", and "try".  More may be created by simply referencing them.  Desks have
independent histories and states, and they may be merged into each other.

###`++dojo`, domestic desk state

```
++  dojo  ,[p=cult q=dome]                              ::  domestic desk state
```

This is the all the data that is specific to a particular desk on a domestic
ship.  `p` is the set of subscribers to this desk and `q` is the data in the
desk.

###`++cult`, subscriptions

```
++  cult  (map duct rave)                               ::  subscriptions
```

This is the set of subscriptions to a particular desk.  The keys are the ducts
from where the subscriptions requests came.  The results will be produced along
these ducts.  The values are a description of the requested information.

###`++rave`, general subscription request

```
++  rave                                                ::  general request
          $%  [& p=mood]                                ::  single request
              [| p=moat]                                ::  change range
          ==                                            ::
```

This represents a subscription request for a desk.  The request can be for
either a single item in the desk or else for a range of changes on the desk.

###`++mood`, single subscription request

```
++  mood  ,[p=care q=case r=path]                       ::  request in desk
```

This represents a request for the state of the desk at a particular commit,
specfied by `q`.  `p` specifies what kind of information is desired, and `r`
specifies the path we are requesting.

###`++moat`, range subscription request

```
++  moat  ,[p=case q=case]                              ::  change range
```

This represents a request for all changes between `p` and `q`.  Note that there
is currently no way to request to be notified only on changes to particular
paths in the filesystem.  You must subscribe to the entire desk.

###`++care`, clay submode

```
++  care  ?(%u %v %w %x %y %z)                          ::  clay submode
```

This specifies what type of information is requested in a subscription or a
scry.

`%u` requests the `++rang` at the current moment.  Because this information is
not stored for any moment other than the present, we crash if the `++case` is
not a `%da` for now.

`%v` requests the `++dome` at the specified commit.

`%w` requests the current revsion number of the desk.

`%x` requests the file at a specified path at the specified commit.  If there
is no node at that path or if the node has no contents (that is, if `q:ankh` is
null), then this produces null.

`%y` requests a `++arch` of the specfied commit at the specified path.

`%z` requests the `++ankh` of the specified commit at the specfied path.

###`++arch`, shallow filesystem node

```
++  arch  ,[p=@uvI q=(unit ,@uvI) r=(map ,@ta ,~)]      ::  fundamental node
```

This is analogous to `++ankh` except that the we have neither our contents nor
the ankhs of our children.  The other fields are exactly the same, so `p` is a
hash of the associated ankh, `u.q`, if it exists, is a hash of the contents of
this node, and the keys of `r` are the names of our children.  `r` is a map to
null rather than a set so that the ordering of the map will be equivalent to
that of `r:ankh`, allowing efficient conversion.

###`++case`, specifying a commit

```
++  case                                                ::  ship desk case spur
          $%  [%da p=@da]                               ::  date
              [%tas p=@tas]                             ::  label
              [%ud p=@ud]                               ::  number
          ==                                            ::
```

A commit can be referred to in three ways: `%da` refers to the commit that was
at the head on date `p`, `%tas` refers to the commit labeled `p`, and `%ud`
refers to the commit numbered `p`.  Note that since these all can be reduced
down to a `%ud`, only numbered commits may be referenced with a `++case`.

###`++dome`, desk data

```
++  dome                                                ::  project state
          $:  ang=agon                                  ::  pedigree
              ank=ankh                                  ::  state
              let=@ud                                   ::  top id
              hit=(map ,@ud tako)                       ::  changes by id
              lab=(map ,@tas ,@ud)                      ::  labels
          ==                                            ::
```

This is the data that is actually stored in a desk.

`ang` is unused and should be removed.

`ank` is the current state of the desk.  Thus, it is the state of the
filesystem at revison `let`.  The head of a desk is always a numbered commit.

`let` is the number of the most recently numbered commit.  This is also the
total number of numbered commits.

`hit` is a map of numerical ids to hashes of commits.  These hashes are mapped
into their associated commits in `hut:rang`.  In general, the keys of this map
are exactly the numbers from 1 to `let`, with no gaps.  Of course, when there
are no numbered commits, `let` is 0, so `hit` is null.  Additionally, each of
the commits is an ancestor of every commit numbered greater than this one.
Thus, each is a descendant of every commit numbered less than this one.  Since
it is true that the date in each commit (`t:yaki`) is no earlier than that of
each of its parents, the numbered commits are totally ordered in the same way
by both pedigree and date.  Of course, not every commit is numbered.  If that
sounds too complicated to you, don't worry about it.  It basically behaves
exactly as you would expect.

`lab` is a map of textual labels to numbered commits.  Note that labels can
only be applied to numbered commits.  Labels must be unique across a desk.

###`++ankh`, filesystem node

```
++  ankh                                                ::  fs node (new)
          $:  p=cash                                    ::  recursive hash
              q=(unit ,[p=cash q=*])                    ::  file
              r=(map ,@ta ankh)                         ::  folders
          ==                                            ::
```

This is a single node in the filesystem.  This may be file or a directory or
both.  In earth filesystems, a node is a file xor a directory.  On mars, we're
inclusive, so a node is a file ior a directory.

`p` is a recursive hash that depends on the contents of the this file or
directory and on any children.

`q` is the contents of this file, if any.  `p.q` is a hash of the contents
while `q.q` is the data itself.

`r` is the set of children of this node.  In the case of a pure file, this is
empty.  The keys are the names of the children and the values are, recursively,
the nodes themselves.

###`++cash`, ankh hash

```
++  cash  ,@uvH                                         ::  ankh hash
```

This is a 128-bit hash of an ankh.  These are mostly stored within ankhs
themselves, and they are used to check for changes in possibly-deep
hierarchies.

###`++rung`, filesystem per neighbor ship

```
++  rung  $:  rus=(map desk rede)                       ::  neighbor desks
          ==                                            ::
```

This is the filesystem of a neighbor ship.  The keys to this map are all the
desks we know about on their ship.

###`++rede`, desk state

```
++  rede                                                ::  universal project
          $:  lim=@da                                   ::  complete to
              qyx=cult                                  ::  subscribers
              ref=(unit rind)                           ::  outgoing requests
              dom=dome                                  ::  revision state
          ==                                            ::
```

This is our knowledge of the state of a desk, either foreign or domestic.

`lim` is the date of the last full update.  We only respond to requests for
stuff before this time.

`qyx` is the list of subscribers to this desk.  For domestic desks, this is
simply `p:dojo`, all subscribers to the desk, while in foreign desks this is
all the subscribers from our ship to the foreign desk.

`ref` is the request manager for the desk.

`dom` is the actual data in the desk.

###`++rind`, request manager

```
++  rind                                                ::  request manager
          $:  nix=@ud                                   ::  request index
              bom=(map ,@ud ,[p=duct q=rave])           ::  outstanding
              fod=(map duct ,@ud)                       ::  current requests
              haw=(map mood (unit))                     ::  simple cache
          ==                                            ::
```

This is the request manager for a desk.

`nix` is one more than the index of the most recent request.  Thus, it is the
next available request number.

`bom` is the set of outstanding requests.  The keys of this map are some subset
of the numbers between 0 and one less than `nix`.  The members of the map are
exactly those requests that have not yet been fully satisfied.

`fod` is the same set as `bom`, but from a different perspective.  In
particular, the values of `fod` are the same as the values of `bom`, and the
`p` out of the values of `bom` are the same as the keys of `fod`.  Thus, we can
map ducts to their associated request number and `++rave`, and we can map
numbers to their associated duct and `++rave`.

`haw` is a map from simple requests to their values.  This acts as a cache for
requests that have already been made.  Thus, the second request for a
particular `++mood` is nearly instantaneous.

###`++rang`, data store

```
++  rang  $:  hut=(map tako yaki)                       ::
              lat=(map lobe blob)                       ::
          ==                                            ::
```

This is a set of data keyed by hash.  Thus, this is where the "real" data is
stored, but it is only meaningful if we know the hash of what we're looking
for.

`hut` is a map from hashes to commits.  We often get the hashes from
`hit:dome`, which keys them by logical id.  Not every commit has an id.

`lat` is a map from hashes to the actual data.  We often get the hashes from a
`++yaki`, a commit, which references this map to get the data.  There is no
`++blob` in any `++yaki`.  They are only accessible through this map.

###`++tako`, commit reference

```
++  tako  ,@                                            ::  yaki ref
```

This is a hash of a `++yaki`, a commit.  These are most notably used as the
keys in `hut:rang`, where they are associated with the actual `++yaki`, and as
the values in `hit:dome`, where sequential ids are associated with these.

###`++yaki`, commit

```
++  yaki  ,[p=(list tako) q=(map path lobe) r=tako t=@da] ::  commit
```

This is a single commit.

`p` is a list of the hashes of the parents of this commit.  In most cases, this
will be a single commit, but in a merge there may be more parents.  In theory,
there may be an arbitrary number of parents, but in practice merges have
exactly two parents.  This may change in the future.  For commit 1, there is no
parent.

`q` is a map of the paths on a desk to the data at that location.  If you
understand what a `++lobe` and a `++blob` is, then the type signature here
tells the whole story.

`r` is the hash associated with this commit.

`t` is the date at which this commit was made.

###`++lobe`, data reference

```
++  lobe  ,@                                            ::  blob ref
```

This is a hash of a `++blob`.  These are most notably used in `lat:rang`, where
they are associated with the actual `++blob`, and as the values in `q:yaki`,
where paths are associated with their data in a commit.

###`++blob`, data

```
++  blob  $%  [%delta p=lobe q=lobe r=udon]             ::  delta on q
              [%direct p=lobe q=* r=umph]               ::
              [%indirect p=lobe q=* r=udon s=lobe]      ::
          ==                                            ::
```

This is a node of data.  In every case, `p` is the hash of the blob.

`%delta` is the case where we define the data by a delta on other data.  In
practice, the other data is always the previous commit, but nothing depends on
this.  `q` is the hash of the parent blob, and `r` is the delta.

`%direct` is the case where we simply have the data directly.  `q` is the data
itself, and `r` is any preprocessing instructions.  These almost always come
from the creation of a file.

`%indirect` is both of the preceding cases at once.  `q` is the direct data,
`r` is the delta, and `s` is the parent blob.  It should always be the case
that applying `r` to `s` gives the same data as `q` directly (with the
prepreprocessor instructions in `p.r`).  This exists purely for performance
reasons.  This is unused, at the moment, but in general these should be created
when there are a long line of changes so that we do not have to traverse the
delta chain back to the creation of the file.

###`++udon`, abstract delta

```
++  udon                                                ::  abstract delta
          $:  p=umph                                    ::  preprocessor
              $=  q                                     ::  patch
              $%  [%a p=* q=*]                          ::  trivial replace
                  [%b p=udal]                           ::  atomic indel
                  [%c p=(urge)]                         ::  list indel
                  [%d p=upas q=upas]                    ::  tree edit
              ==                                        ::
          ==                                            ::
```

This is an abstract change to a file.  This is a superset of what would
normally be called diffs.  Diffs usually refer to changes in lines of text
while we have the ability to do more interesting deltas on arbitrary data
structures.

`p` is any preprocessor instructions.

`%a` refers to the trival delta of a complete replace of old data with new
data.

`%b` refers to changes in an opaque atom on the block level.  This has very
limited usefulness, and is not used at the moment.

`%c` refers to changes in a list of data.  This is often lines of text, which
is your classic diff.  We, however, will work on any list of data.

`%d` refers to changes in a tree of data.  This is general enough to describe
changes to any hoon noun, but often more special-purpose delta should be
created for different content types.  This is not used at the moment, and may
in fact be unimplemented.

###`++urge`, list change

```
++  urge  |*(a=_,* (list (unce a)))                     ::  list change
```

This is a parametrized type for list changes.  For example, `(urge ,@t)` is a
list change for lines of text.

###`++unce`, change part of a list.

```
++  unce  |*  a=_,*                                     ::  change part
          $%  [%& p=@ud]                                ::  skip[copy]
              [%| p=(list a) q=(list a)]                ::  p -> q[chunk]
          ==                                            ::  
```

This is a single change in a list of elements of type `a`.  For example, `(unce ,@t)` is
a single change in a lines of text.

`%&` means the next `p` lines are unchanged.

`%|` means the lines `p` have changed to `q`.

###`++umph`, preprocessing information

```
++  umph                                                ::  change filter
          $|  $?  %a                                    ::  no filter
                  %b                                    ::  jamfile
                  %c                                    ::  LF text
              ==                                        ::
          $%  [%d p=@ud]                                ::  blocklist
          ==                                            ::
```

This space intentionally left undocumented.  This stuff will change once we get
a well-typed clay.


###`++upas`, tree change

```
++  upas                                                ::  tree change (%d)
          $&  [p=upas q=upas]                           ::  cell
          $%  [%0 p=axis]                               ::  copy old
              [%1 p=*]                                  ::  insert new
              [%2 p=axis q=udon]                        ::  mutate!
          ==                                            ::
```

This space intentionally left undocumented.  This stuff is not known to work,
and will likely change when we get a well-typed clay.  Also, this is not a
complicated type; it is not difficult to work out the meaning.
