`%clay` 
========

`%clay` filesystem utilities.

## Paths

### Structure

Urbit paths have a very specific structure. First, since the clay
filesystem has a global namespace, the first element in any path
is the particular urbit whose filesystem you are trying to
access.

The second element specifies which desk you wish to access on
that urbit. Desks are independent branches (in the
revision-control sense) of their filesystem.  

The third element specifies the revision number for that
desk.  The remainder of the path is the path to the file.

Thus, a path in clay is:

`/urbit/desk/revision/path`.  

For example, to get revision 5 of `/try/readme/md` off the `home`
desk on `~sampel-sipnym`, use:

`/~sampel-sipnym/home/5/try/readme/md`.

### Shortcuts

`%` refers to the current working
directory.  `%%` refers to our parent, `%%%` refers to our
grandparent, and so forth.

For example:

    XX  TBD


From the other direction, inserting a `=` into a path copies the
corresponding element from the current path into the path that
you're trying to access.

For example, if the current path is referencing our ship at the
current time, to reference `/try/readme`, use:

`/===try/readme`.


### Accessing commits

There are three ways to refer to particular commits in the
revision history.  First, one can use the revision number.
Second, one can use any absolute time between the one numbered
commit and the next (inclusive of the first, exclusive of the
second). Thirdly, every desk has a map of labels to revision
numbers. These labels may be used to refer to specific commits.


## `ls` 

`+ls /path` gives a directory listing at a path 

## `cat`

`+cat /path`
prints out the file at the given path.

## `mount`

It's often useful to "mount" the clay filesystem to unix, so that
you can interact with it with the traditional unix tools.  The
syntax to do this is as follows:

    |mount /path [%mount-point]

This mirrors the desk out to unix in at the path
`<pier-directory> <mount-point>`.  If you don't supply a
`%mount-point`, we use the last element in the path.  Thus, if
you mount `%/pub/doc`, it'll by default put it in `doc`.

*The mount point is monitored Dropbox-style, so every change you
make to the file in unix is automatically commited to clay.*

You can unmount by specifying either the path or the mount point.

    |unmount /path
    |unmount %mount-point

## `merge`

Often, it's useful to be able to merge a desk into another desk.
The other desk does not, of course, need to be on the same urbit:
for example, the standard way to distribute an app is to put it
on a desk and let other people merge it into their own urbit.

The syntax is as follows:

    |merge %to-desk ~from-urbit %from-desk [%strategy]

There are seven different merge strategies.  Throughout our
discussion, we'll say that the merge is from Alice's desk to
Bob's.

###  Native strategies

A `%init` merge should be used iff it's the first commit to a
desk.  The head of Alice's desk is used as the number 1 commit to
Bob's desk. Obviously, the ancestry remains intact when
traversing the parentage of the commit, even though previous
commits are not numbered for Bob's desk.

A `%this` merge means to keep what's in Bob's desk, but join the
ancestry. Thus, the new commit has the head of each desk as
parents, but the data is exactly what's in Bob's desk. For those
following along in git, this is the 'ours' merge strategy, not
the '--ours' option to the 'recursive' merge strategy. In other
words, even if Alice makes a change that does not conflict with
Bob, we throw it away.

A `%that` merge means to take what's in Alice's desk, but join
the ancestry. This is the reverse of `%this`.

A `%fine` merge is a "fast-forward" merge. This succeeds iff one
head is in the ancestry of the other. In this case, we use the
descendant as our new head.

For `%meet`, `%mate`, and `%meld` merges, we first find the most
recent common ancestor to use as our merge base. If we have no
common ancestors, then we fail. If we have multiple most
recent common ancestors, then we have a criss-cross situation,
which should be handled delicately. At present, we don't handle
this kind of situation, but something akin to git's 'recursive'
strategy should be implemented in the future.

There's a functional inclusion ordering on `%fine`, `%meet`,
`%mate`, and `%meld` such that if an earlier strategy would have
succeeded, then every later strategy will produce the same
result. Put another way, every earlier strategy is the same as
every later strategy except with a restricted domain.

A `%meet` merge only succeeds if the changes from the merge base
to Alice's head (hereafter, "Alice's changes") are in different
files than Bob's changes. In this case, the parents are both
Alice's and Bob's heads, and the data is the merge base plus
Alice's changed files plus Bob's changed files.

A `%mate` merge attempts to merge changes to the same file when
both Alice and Bob change it. If the merge is clean, we use it;
otherwise, we fail. A merge between different types of changes --
for example, deleting a file vs changing it -- is always a
conflict. If we succeed, the parents are both Alice's and Bob's
heads, and the data is the merge base plus Alice's changed files
plus Bob's changed files plus the merged files.

A `%meld` merge will succeed even if there are conflicts. If
there are conflicts in a file, then we use the merge base's
version of that file, and we produce a set of files with
conflicts. The parents are both Alice's and Bob's heads, and the
data is the merge base plus Alice's changed files plus Bob's
changed files plus the successfully merged files plus the merge
base's version of the conflicting files.

###  Metastrategies

There's also a meta-strategy `%auto`, which is the most common.
If no strategy is supplied, then `%auto` is assumed.  `%auto`
checks to see if Bob's desk exists, and if it doesn't we use a
`%init` merge.  Otherwise, we progressively try `%fine`,
`%meet`, and `%mate` until one succeeds.

If none succeed, we merge Bob's desk into a scratch desk.  Then,
we merge Alice's desk into the scratch desk with the `%meld`
option to force the merge. For each file in the produced set of
conflicting files, we call the `++mash` function for the
appropriate mark, which annotates the conflicts if we know how.

Finally, we display a message to the user informing them of the
scratch desk's existence, which files have annotated conflicts,
and which files have unannotated conflicts. When the user has
resolved the conflicts, they can merge the scratch desk back into
Bob's desk. This will be a `%fine` merge since Bob's head is in
the ancestry of the scratch desk.

## Autosync

Since clay is reactive, it's possible for changes to the
filesystem to cause various actions.  An important use of this is
in enabling "autosync".  When a desk is synced to another, any
changes to the first desk are automatically applied to the
second.

This isn't simply mirroring, since the local desk might have
changes of its own.  We use the full merge capabilities of clay
to try to make the merge clean.  If there are conflicts, it'll
notify you and let you resolve them.

There can be complex sync flows, some of which are useful.
Often, many urbits will be synced to some upstream desk that is
trusted to provide updates.  Sometimes, it's useful to sync two
desks to each other, so that changes to one or the other are
mirrored.

The syntax for syncing and unsyncing desks is as follows:

    |sync %to-desk ~from-urbit %from-desk
    |unsync %to-desk ~from-urbit %from-desk