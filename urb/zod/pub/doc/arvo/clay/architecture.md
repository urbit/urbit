# clay

## high-level

clay is the primary filesystem for the arvo operating system,
which is the core of an urbit. The architecture of clay is
intrinsically connected with arvo, but we assume no knowledge of
either arvo or urbit. We will point out only those features of
arvo that are necessary for an understanding of clay, and we will
do so only when they arise.

The first relevant feature of arvo is that it is a deterministic
system where input and output are defined as a series of events
and effects. The state of arvo is simply a function of its event
log. None of the effects from an event are emitted until the
event is entered in the log and persisted, either to disk or
another trusted source of persistence, such as a Kafka cluster.
Consequently, arvo is a single-level store: everything in its
state is persistent. 

In a more traditional OS, everything in RAM can be erased at any
time by power failure, and is always erased on reboot. Thus, a
primary purpose of a filesystem is to ensure files persist across
power failures and reboots.  In arvo, both power failures and
reboots are special cases of suspending computation, which is
done safely since our event log is already persistent. Therefore,
clay is not needed in arvo for persistence. Why, then, do we have a
filesystem? There are two answers to this question.

First, clay provides a filesystem tree, which is a convenient
user interface for some applications. Unix has the useful concept
of virtual filesystems, which are used for everything from direct
access to devices, to random number generators, to the /proc
tree. It is easy and intuitive to read from and write to a
filesystem tree.

Second, clay has a distributed revision control system baked into
it.  Traditional filesystems are not revision controlled, so
userspace software -- such as git -- is written on top of them to
do so. clay natively provides the same functionality as modern
DVCSes, and more.

clay has two other unique properties that we'll cover later on:
it supports typed data and is referentially transparent. 

### Revision Control

Every urbit has one or more "desks", which are independently
revision-controlled branches. Each desk contains its own mark
definitions, apps, doc, and so forth.

Traditionally, an urbit has at least a base and a home desk. The
base desk has all the system software from the distribution. the
home desk is a fork of base with all the stuff specific to the
user of the urbit.

A desk is a series of numbered commits, the most recent of which
represents the current state of the desk. A commit is composed of
(1) an absolute time when it was created, (2) a list of zero or
more parents, and (3) a map from paths to data.

Most commits have exactly one parent, but the initial commit on a
desk may have zero parents, and merge commits have more than one
parent.

The non-meta data is stored in the map of paths to data. It's
worth noting that no constraints are put on this map, so, for
example, both /a/b and /a/b/c could have data. This is impossible
in a traditional Unix filesystem since it means that /a/b is both
a file and a directory. Conventionally, the final element in the
path is its mark -- much like a filename extension in Unix. Thus,
/doc/readme.md in Unix is stored as /doc/readme/md in urbit.

The data is not stored directly in the map; rather, a hash of the
data is stored, and we maintain a master blob store. Thus, if the
same data is referred to in multiple commits (as, for example,
when a file doesn't change between commits), only the hash is
duplicated.

In the master blob store, we either store the data directly, or
else we store a diff against another blob. The hash is dependent
only on the data within and not on whether or not it's stored
directly, so we may on occasion rearrange the contents of the
blob store for performance reasons.

Recall that a desk is a series of numbered commits. Not every
commit in a desk must be numbered. For example, if the base desk
has had 50 commits since home was forked from it, then a merge
from base to home will only add a single revision number to home,
although the full commit history will be accessible by traversing
the parentage of the individual commits.

We do guarantee that the first commit is numbered 1, commits are
numbered consecutively after that (i.e. there are no "holes"),
the topmost commit is always numbered, and every numbered commit
is an ancestor of every later numbered commit.

There are three ways to refer to particular commits in the
revision history.  Firstly, one can use the revision number.
Secondly, one can use any absolute time between the one numbered
commit and the next (inclusive of the first, exclusive of the
second). Thirdly, every desk has a map of labels to revision
numbers. These labels may be used to refer to specific commits.

Additionally, clay is a global filesystem, so data on other urbit
is easily accessible the same way as data on our local urbit.  In
general, the path to a particular revision of a desk is
/~urbit-name/desk-name/revision.  Thus, to get /try/readme/md
from revision 5 of the home desk on ~sampel-sipnym, we refer to
/~sampel-sipnym/home/5/try/readme/md.  Clay's namespace is thus
global and referentially transparent.

XXX reactivity here?

### A Typed Filesystem

Since clay is a general filesystem for storing data of arbitrary
types, in order to revision control correctly it needs to be
aware of types all the way through.  Traditional revision control
does an excellent job of handling source code, so for source code
we act very similar to traditional revision control. The
challenge is to handle other data similarly well.

For example, modern VCSs generally support "binary files", which
are files for which the standard textual diffing, patching, and
merging algorithms are not helpful. A "diff" of two binary files
is just a pair of the files, "patching" this diff is just
replacing the old file with the new one, and "merging"
non-identical diffs is always a conflict, which can't even be
helpfully annotated. Without knowing anything about the structure
of a blob of data, this is the best we can do.

Often, though, "binary" files have some internal structure, and
it is possible to create diff, patch, and merge algorithms that
take advantage of this structure. An image may be the result of a
base image with some set of operations applied. With algorithms
aware of this set of operations, not only can revision control
software save space by not having to save every revision of the
image individually, these transformations can be made on parallel
branches and merged at will.

Suppose Alice is tasked with touching up a picture, improving the
color balance, adjusting the contrast, and so forth, while Bob
has the job of cropping the picture to fit where it's needed and
adding textual overlay.  Without type-aware revision control,
these changes must be made serially, requiring Alice and Bob to
explicitly coordinate their efforts. With type-aware revision
control, these operations may be performed in parallel, and then
the two changesets can be merged programmatically.

Of course, even some kinds of text files may be better served by
diff, patch, and merge algorithms aware of the structure of the
files. Consider a file containing a pretty-printed JSON object.
Small changes in the JSON object may result in rather significant
changes in how the object is pretty-printed (for example, by
addding an indentation level, splitting a single line into
multiple lines).

A text file wrapped at 80 columns also reacts suboptimally with
unadorned Hunt-McIlroy diffs. A single word inserted in a
paragraph may push the final word or two of the line onto the
next line, and the entire rest of the paragraph may be flagged as
a change. Two diffs consisting of a single added word to
different sentences may be flagged as a conflict. In general,
prose should be diffed by sentence, not by line.

As far as the author is aware, clay is the first generalized,
type-aware revision control system.  We'll go into the workings
of this system in some detail.

### Marks

Central to a typed filesystem is the idea of types. In clay, we
call these "marks". A mark is a file that defines a type,
conversion routines to and from the mark, and diff, patch, and
merge routines.

For example, a `%txt` mark may be a list of lines of text, and it
may include conversions to `%mime` to allow it to be serialized
and sent to a browswer or to the unix filesystem. It will also
include Hunt-McIlroy diff, patch, and merge algorithms.

A `%json` mark would be defined as a json object in the code, and
it would have a parser to convert from `%txt` and a printer to
convert back to `%txt`. The diff, patch, and merge algorithms are
fairly straightforward for json, though they're very different
from the text ones.

More formally, a mark is a core with three arms, `++grab`,
`++grow`, and `++grad`. In `++grab` is a series of functions to
convert from other marks to the given mark.  In `++grow` is a
series of functions to convert from the given mark to other
marks. In `++grad` is `++diff`, `++pact`, `++join`, and `++mash`.

The types are as follows, in an informal pseudocode:

    ++  grab:
      ++  mime: <mime> -> <mark-type>
      ++  txt: <txt> -> <mark-type>
      ...
    ++  grow:
      ++  mime: <mark-type> -> <mime>
      ++  txt: <mark-type> -> <txt>
      ...
    ++  grad
      ++  diff: (<mark-type>, <mark-type>) -> <diff-type>
      ++  pact: (<mark-type>, <diff-type>) -> <mark-type>
      ++  join: (<diff-type>, <diff-type>) -> <diff-type> or NULL
      ++  mash: (<diff-type>, <diff-type>) -> <diff-type>

These types are basically what you would expect. Not every mark
has each of these functions defined -- all of them are optional
in the general case.

In general, for a particular mark, the `++grab` and `++grow` entries
(if they exist) should be inverses of each other.

In `++grad`, `++diff` takes two instances of a mark and produces
a diff of them. `++pact` takes an instance of a mark and patches
it with the given diff. `++join` takes two diffs and attempts to
merge them into a single diff. If there are conflicts, it
produces null. `++mash` takes two diffs and forces a merge,
annotating any conflicts.

In general, if `++diff` called with A and B produces diff D, then
`++pact` called with A and D should produce B. Also, if `++join`
of two diffs does not produce null, then `++mash` of the same
diffs should produce the same result.

Alternately, instead of `++diff`, `++pact`, `++join`, and
`++mash`, a mark can provide the same functionality by defining
`++sted` to be the name of another mark to which we wish to
delegate the revision control responsibilities. Then, before
running any of those functions, clay will convert to the other
mark, and convert back afterward. For example, the `%hoon` mark
is revision-controlled in the same way as `%txt`, so its `++grad`
is simply `++sted %txt`. Of course, `++txt` must be defined in
`++grow` and `++grab` as well.

Every file in clay has a mark, and that mark must have a
fully-functioning `++grad`. Marks are used for more than just
clay, and other marks don't need a `++grad`, but if a piece of
data is to be saved to clay, we must know how to revision-control
it.

Additionally, if a file is to be synced out to unix, then it must
have conversion routines to and from the `%mime` mark.

##Using clay

### Reading and Subscribing

When reading from Clay, there are three types of requests.  A
`%sing` request asks for data at single revsion.  A `%next`
request asks to be notified the next time there's a change to
given file.  A `%many` request asks to be notified on every
change in a desk for a range of changes.

For `%sing` and `%next`, there are generally three things to be
queried.  A `%u` request simply checks for the existence of a
file at a path.  A `%x` request gets the data in the file at a
path.  A `%y` request gets a hash of the data in the file at the
path combined with all its children and their data.  Thus, `%y`
of a node changes if it or any of its children change.

A `%sing` request is fulfilled immediately if possible.  If the
requested revision is in the future, or is on another ship for
which we don't have the result cached, we don't respond
immediately.  If the requested revision is in the future, we wait
until the revision happens before we respond to the request.  If
the request is for data on another ship, we pass on the request
to the other ship.  In general, Clay subscriptions, like most
things in Urbit, aren't guaranteed to return immediately.
They'll return when they can, and they'll do so in a
referentially transparent manner.

A `%next` request checks query at the given revision, and it
produces the result of the query the next time it changes, along
with the revsion number when it changes.  Thus, a `%next` of a
`%u` is triggered when a file is added or deleted, a `%next of a
`%x` is triggered when a file is added, deleted, or changed, and
a `%next` of a `%y` is triggered when a file or any of its
children is added, deleted, or changed.

A `%many` request is triggered every time the given desk has a
new revision.  Unlike a `%next`, a `%many` has both a start and
an end revsion, after which it stops returning.  For `%next`, a
single change is reported, and if the caller wishes to hear of
the next change, it must resubscribe.  For `%many`, every revsion
from the start to the end triggers a response.  Since a `%many`
request doesn't ask for any particular data, there aren't `%u`,
`%x`, and `%y` versions for it.

### Unix sync

One of the primary functions of clay is as a convenient user
interface. While tools exist to use clay from within urbit, it's
often useful to be able to treat clay like any other filesystem
from the Unix perspective -- to "mount" it, as it were.

From urbit, you can run `|mount /path/to/directory %mount-point`,
and this will mount the given clay directory to the mount-point
directory in Unix. Every file is converted to `%mime` before it's
written to Unix, and converted back when read from Unix. The
entire directory is watched (a la Dropbox), and every change is
auto-committed to clay.

### Merging

Merging is a fundamental operation for a distributed revision
control system. At their root, clay's merges are similar to
git's, but with some additions to accomodate typed data. There
are seven different merge strategies.

Throughout our discussion, we'll say that the merge is from
Alice's desk to Bob's. Recall that a commit is a date (for all
new commits this will be the current date), a list of parents,
and the data itself.

A `%init` merge should be used iff it's the first commit to a
desk.  The head of Alice's desk is used as the number 1 commit to
Bob's desk. Obviously, the ancestry remains intact through
traversing the parentage of the commit even though previous
commits are not numbered for Bob's desk.

A `%this` merge means to keep what's in Bob's desk, but join the
ancestry. Thus, the new commit has the head of each desk as
parents, but the data is exactly what's in Bob's desk. For those
following along in git, this is the 'ours' merge strategy, not
the '--ours' option to the 'recursive' merge strategy. In other
words, even if Alice makes a change that does not conflict with
Bob, we throw it away. It's Bob's way or the highway.

A `%that` merge means to take what's in Alice's desk, but join
the ancestry. This is the reverse of `%this`.

A `%fine` merge is a "fast-forward" merge. This succeeds iff one
head is in the ancestry of the other. In this case, we use the
descendant as our new head.

For `%meet`, `%mate`, and `%meld` merges, we first find the most
recent common ancestor to use as our merge base. If we have no
common ancestors, then we fail. If we have more than one most
recent common ancestor, then we have a criss-cross situation,
which should be handled delicately. At present, we delicately
throw up our hands and give up, but something akin to git's
'recursive' strategy should be implemented in the future.

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
both Alice and bob change it. If the merge is clean, we use it;
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

That's the extent of the merge options in clay proper. In
userspace there's a final option `%auto`, which is the most
common.  `%auto` checks to see if Bob's desk exists, and if it
doesn't we use a `%init` merge. Otherwise, we progressively try
`%fine`, `%meet`, and `%mate` until one succeeds.

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

### Autosync

Tracking and staying in sync with another desk is another
fundamental operation. We call this "autosync". This doesn't mean
simply mirroring a desk, since that wouldn't allow local changes.
We simply want to apply changes as they are made upstream, as
long as there are no conflicts with local changes.

This is implemented by watching the other desk, and, when it has
changes, merging these changes into our desk with the usual merge
strategies.

Note that it's quite reasonable for two desks to be autosynced to
each other. This results in any change on one desk being mirrored
to the other and vice versa.

Additionally, it's fine to set up an autosync even if one desk,
the other desk, or both desks do not exist. The sync will be
activated when the upstream desk comes into existence and will
create the downstream desk if needed.
