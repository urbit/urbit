---
next: true
sort: 6
title: Filesystem handbook
---

# Filesystem handbook

Urbit has its own revision-controlled filesystem, the `%clay`
vane.  `%clay` is like a simplified `git`, but more reactive,
and also typed.   Okay, this makes no sense.

The most common way to use `%clay` is to mount a `%clay` node in
a Unix directory.  The Urbit process will watch this directory
and automatically record edits as changes, Dropbox style.  The
mounted directory is always at the root of your pier directory.

## Commands

Note that in both commands and generators, a currently unbound
case (such as a version in the future) will make the calculation
block, not complete.  A remote case will cause a network request.
A remote, unbound case will cause a waiting subscription.

### Mounting to Unix

#### `|mount [pax=path pot=$|(~ [knot ~])]`

Mount the path `pax` at the Unix mount point `pot`, the name of a
subdirectory in your pier.

    |mount %/pub/doc %documents

with a `$PIER` of `/home/nixon/urbit/fintud-macrep`, will mount
`%/pub/doc` in `/home/nixon/urbit/fintud-macrep/documents`.

The mount point is optional; if it's not supplied, the last knot
in the path (`%doc`) will be used.

#### `|unmount [mon=$|(term [knot path]) ~] `

Undo a mount, either by specifying the path or the mount point:

    |unmount %/pub/doc
    |unmount %documents

It's a good habit to also delete the Unix subtree, but Urbit
doesn't do it for you.

### Revision-control operations

#### `|merge [syd=desk src=beak how=$|(~ [germ ~])]`

Merge the beak `src` into the desk `syd`, with optional merge
strategy `how`.

The `src` beak can be a desk (`%home`); a plot-desk cell
(`[~doznec %home]`); or a plot-desk-case path (`/=home=`).

    |merge %home-work /=home= %fine
    |merge %home-work /=home=

#### `|sync [syd=desk her=plot org=$|(~ [desk ~])]`

Activate autosync from the plot `her` and source desk `org`, into
the desk `syd`.  If `org` is omitted, it's the same as `syd`:

    |sync %home-local ~doznec %home
    |sync %home ~doznec

Note that `|merge` takes a path because it needs a source case
(revision), which would make no sense for `|sync`.

#### `|label [syd=desk lab=term]`

Label the current version of desk `syd`:

#### `|unsync [syd=desk her=plot org=desk ~]`

Turn off autosync.  The argument needs to match the original
`|sync` perfectly, or Urbit will become angry and confused.

### Filesystem manipulation

#### `|rm [paz=(list path)]`

Remove any leaf at each of the paths in `paz`.
    
    |rm /===/pub/fab/nixon/hoon

Remember that folders in `%clay` are a consequence of the tree of
leaves; there is no `rmdir` or `mkdir`.

#### `|cp [too=path fro=path how=$|(~ [germ ~])]`

Copy the subtree `fro` into the subtree `too`, committing it with
the specified merge strategy.

#### `|mv [too=path fro=path how=$|(~ [germ ~])]`

In `%clay`, `|mv` is just a shorthand for `|cp` then `|rm`.  The
`|rm` doesn't happen unless the `|cp` succeeds, obviously -- it's
good to be transactional.

### Filesystem generators

#### `+cal [paz=(list path)]`
#### `+cat [pax=path]`

Produce the noun, if any, at each of these (global) paths.
`+cat` produces one result, `+cal` a list.

#### `+ls [pax=path ~]`

Produce the list of names in the folder at `pax`.

Because generators aren't passed the dojo's default path, unlike
the current directory in Unix, it's not possible to build an
trivial `+ls` that's the equivalent of Unix `ls`.  You always
have to write `+ls %`.

#### `+ll [pax=path ~]`

Like `+ls`, but the result is a list of full paths.  Useful as
Urbit equivalent of the Unix wildcard `*`.

## A quick overview of `%clay`

`%clay` is a typed, global revision-control system.  Or in other
words, a typed, global referentially transparent namespace.  It's
difficult to understate how awesome this is.

(Actually, in Layer 4 and 5 code, you can use the Hoon `.^` rune
to literally *dereference* this namespace.  And in Layer 5, a
generator will even *block* until the resource is available.)

(Another awesome global immutable namespace is IPFS.  But IPFS is
distributed, whereas `%clay` is just decentralized.  IPFS stores
resources around the network in a DHT, like Freenet or
Bittorrent; `%clay` stores resources on the publisher's server,
like HTTP or git.)

### Path format

As a noun, a path in `%clay` is a `(list knot)`, where each
segment is an `@ta` atom -- URL-safe text, restricted to `[a z]`,
`[0 9]`, `.`, `-`, `_` and `~`.  The list is a tuple terminated
with a Hoon null, `~`.

As an ordinary Hoon noun, `[%foo %bar %baz]` has this structure.
But Hoon also supports the Unix path syntax: `/foo/bar/baz` is
the same noun.

### Relative paths

The Hoon path syntax is always defined relative to a default
path, which is configuration state in the Hoon parser.  In
`:dojo`, this works a little like the Unix current directory.

(But note that in Unix, relative paths are expanded by the
application, which can read the current directory from the
environment.  In Urbit, the current directory and variables are
hidden by the dojo from any code it runs.  The parser generates
the absolute path -- more like the way a Unix shell parser
unglobs `*`.)

Relative path syntax: `%` is the default path (Unix `.`).  `%%`
is the parent path (Unix `..`).  Unix does not have `...`,
`....`, etc.  But Urbit has `%%%`, `%%%%`, etc.  Urbit has no
local relative paths; in Unix, `foo/bar` is a shorthand for
`./foo/bar`, but in Urbit you have to write `%/foo/bar`. 

Unix has no top-level substitution syntax, but Urbit does.  If
the default path is `/foo/bar/baz`, `/=/moo` means `/foo/moo`,
and `/=/moo/=/goo` means `/foo/moo/baz/goo`.  Also, instead of
`/=/=/zoo` or `/=/=/=/voo`, write `/==zoo` or `/===voo`.  Your
fingers have enough miles on them already.

### Beak

The top three knots in a `%clay` path are `/plot/desk/case`,
where `plot` is of course an urbit; `desk` is a branch name; and
`case` is a revision identity, which is either (a) a label, (b) a
date, or (c) a change number.  For obscure reasons, this prefix
is called the `beak`.

### Spur

The rest of the path, or `spur`, navigates a tree of `node`
nouns.  A `node` is like an inode in a Unix filesystem, but
different.

An inode is *either* a file or a directory.  A `node` is *both* a
folder (which may be empty) and an optional leaf (a noun).

There is no `rmdir` or `mkdir`; an empty node is automatically
pruned, and creating a node creates its path.  The absence of a
file-or-directory mode bit eliminates all kinds of strange corner
cases, especially in merging.

### Leaf

`%clay` is a typed filesystem, or more precisely a *marked* one.
When we sync Unix and Urbit paths, we convert a Unix file extension
(an informal specifications) into a Urbit `*mark*` (an 
executable specification)

The mark name is actually the last knot in the path.  Or to put
it differently: if any `%clay` node has a leaf, its name within
its parent is its mark.

This is ridiculously confusing without examples.  Suppose we have
the following Unix files, with directories to match:

    doc.md
    doc/intro.md
    doc/start.md

These become the Urbit files

    %/doc/md
    %/doc/intro/md
    %/doc/start/md
 
The folder map of the `%/doc` node contains three entries: `%md`,
`%intro`, `%start`.  The folder of `%/doc/intro` and that of
`%/doc/start` each contain one entry: `%md` (the mark of an atom
in Markdown syntax).

Perhaps this example helps explains *why* `%clay` uses this node
design.  One, it's a simple index-page model for any kind of
published tree.  Two, this tree can expand its leaves smoothly,
just by adding content: if we decided `%/doc/start` was not a
leaf but a tree, we could just add `%/doc/start/child/md`.

And three, the `%clay` node structure syncs invertibly with an
equivalent, and not unduly weird, Unix inode layout.

### Mounting to Unix

The most convenient way of interacting with `%clay` is mounting
it to Unix, and modifying it with Unix tools.  The mount
directory is a flat subdirectory of your Urbit pier.

When you have a live mount point, Urbit monitors it with
`inotify()` or equivalent.  (It would be neat to have a FUSE
driver, but we don't.)  If you shut your urbit off, it will
recheck the mount point when it reloads.

Unix files beginning with `.`, with no extension, with an
extension that doesn't map to an Urbit mark, or containing data
that doesn't validate to the mark, are ignored.  Depending on the
extension, there may be a more or less complex conversion from
the Unix length/bytestream pair to the Urbit noun.

### More about desks and marks

The Hoon source code for a mark like `%md` is in
`/===/mar/md/hoon`.  But relative to what beak?  What's in the
`/===`?

The mark source of a leaf in `%clay` is always relative to its
own plot, desk and case.  For example, a leaf at

    `/~fintud-macrep/home/31/pub/doc/hello/md`

is controlled by the mark source

    `/~fintud-macrep/home/31/mar/md/hoon`

If there is no such file or it doesn't compile, the mark is
effectively treated as `%noun`, ie, an arbitrary value.

(Note that when updating a mark, any update which shrinks the set
of nouns in that mark needs to at least adapt old nouns to new.
Also, mark source updates should be very slow, but aren't.  They
should validate all nouns against the new mark, but don't.)

What can you do with a mark?  Validate an arbitrary noun; perform
diffs, patches, and and conflict merges; transform to or from
another mark.  The `%ford` vane, which builds and converts nouns,
can even discover and apply multi-step conversion paths.

Marks are also used to describe network messages.  In this case,
the mark source beak is the beak of the receiving urbit.

### Desks and merging

As in any git-shaped revision control system, the core operation
of the system is merging.

One of the effects of same-beak marks is that it doesn't make
sense to create an empty desk.  You can't populate an empty desk
properly with typed files.  Instead, a new desk should be merged
from an existing desk -- normally the default desk, `%home`.

It's also generally bad style to edit directly in the desk you
want to modify.  Your Unix filesystem changes will appear as a
stream of small, unstructured changes.  You should be editing a
working desk.  Conventionally, to change `%home`, merge `%home`
into `%home-work`, edit there, and merge back as a "commit."
Ideally, your "commits" include modifications to a text file that
acts as a changelog.

So merges are important.  Again as in `git`, merge strategies are
important.  That said, if you are not doing exciting things with 
`%clay`, you can skip the strategy subsection.  By default,
`%clay` will always use the `%auto` meta-strategy, which will
always work if you're not doing exciting things.

#### Merge strategies

There are seven different merge strategies.  Throughout our
discussion, we'll say that the merge is from Alice's desk to
Bob's.

##### Direct strategies

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

##### Meta-strategies

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

### Autosync

Since `%clay` is reactive, it has a subscription interface.
Changes to the filesystem create events which code at Layers 3 or
4 (vanes or apps) can listen to.

The `:hood` appliance uses subscriptions to implement "autosync".
When one desk is synced to another, any changes to the first desk
are automatically applied to the second -- for any two desks, on
any two urbits.

Autosync isn't just mirroring.  The target desk might have
changes of its own.  We use the full merge capabilities of
`%clay` to try to make the merge clean.  If there are conflicts,
it'll notify you through `:talk`, and ask you to resolve.

There can be complex sync flows, many of which are useful.
Often, many urbits will be synced to some upstream desk that is
trusted to provide updates.  Sometimes, it's useful to sync two
desks to each other, so that changes to one or the other are
mirrored.  Cyclical sync structures are normal and healthy.
Also, one desk can be the target of multiple autosyncs.
