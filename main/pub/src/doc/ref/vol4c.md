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

###`++rung`, filesystem per neighbor ship

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
