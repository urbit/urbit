---
next: true
sort: 4
title: Walking Tour
---

# Walking Tour

Piers are portable.  You can move a pier anywhere.  But never,
*ever* run the same urbit in two places at once.  (If you try to
start an urbit but already have a process is running on the same
pier, Urbit will kill the old process.)

(Also, don't let the Unix filesystem it's on run out of disk.
This is a known way to corrupt your urbit!  Sorry.)

### Dojo expressions, generators and operators

`:dojo` is of course a command-line REPL or shell.  But it 
remains functional in spirit.  To simplify a little, there are
three kinds of `:dojo` lines: expressions, generators, commands.

Dojo *expressions* are like `(add 2 2)` -- they simply compute
and print a value, without any side effects, from a twig of Hoon.

Expressions are a simple case of `generators`, which are
functional *in principle* -- rather like HTTP GET requests.  In
fact, GET requests are one data source for generators.  Others
include the Urbit namespace, prompting the user, etc.  A
generator command line always starts with `+`, as in `+ls`.

Commands are all other lines.  Command lines normally start with
`|`, as in `|mount %`, or `:`, as in `:hood +hood/mount %`.  The
latter is just an abbreviation for the former.

A command is generally an *order* to some local or remote
appliance.  An order is a transactional message: like a
request-response pair, but a success response is empty, whereas a
failure contains an error report.

Orders have no output unless they fail.  If they fail, they print
an error trace.  If they succeed, they just print a demure `>=`,
confirming that the receiving appliance did as it was told.

For instance, in Unix, the `rm` and `ls` commands both run a
process.  In Urbit, `|rm` is a command; it changes your
filesystem.  `+ls` is a generator; it produces a value.

## Converse: your `:talk` appliance

To use Urbit as a social network, switch to `:talk` (remember, use ^x to
switch between appliances):

    ~fintud-macrep:talk()

Join the global station `urbit-meta`:

    ~fintud-macrep:talk() ;join ~doznec/urbit-meta

You're on the air!  You should see some backlog to give you
context.  Please remember our code of conduct: don't be rude.
Also, `urbit-meta` is politically correct and safe for work.

For more instructions on how to use `:talk`, see the [`:talk`
manual](http://urbit.org/docs/user/talk)

## Using the filesystem

The Urbit filesystem, `%clay`, is a revision-control system (like
`git`) that syncs to a Unix directory (like Dropbox).  While you
can of course create `%clay` changes from within Urbit, Unix has
mature editors and file handling tools.

So usually, the best way to work with `%clay` files is to make
edits in a Unix mirror directory, and let the Urbit interpreter 
commit them as changes.  A simple way to set this up is to mount
the default `%home` desk:

    ~fintud-macrep:dojo> |mount %

This mirrors your `%home` desk against `$PIER/home`, and tells 
the `urbit` process to monitor the latter with `inotify()` etc.
The mount is two-way: Unix edits propagate up to Urbit, Urbit 
changes fall down into Unix.

In fact, the source for this page is here:

    $PIER/home/pub/docs/user/start.mdy

If you're reading this on your own ship, edit the file with the
browser still open.  Isn't that cool?  Now, change it back -- you
don't particularly want a conflict next time we make an update,
since your `%home` desk is generally synced to our repo.

Reactive auto-updates are a particular speciality of `%clay.` We
use them to drive auto-updates of code at every layer.  A normal
Urbit user never has to think about software update.

## Read the docs

Your urbit is your personal web server.  The best place to read
its docs is by pointing your browser at it.  You can also post
your own documents, of course.

Urbit prints the HTTP port it's serving when it starts up:

    http: live (insecure) on 8080

8080 is the default.  If you're running on AWS or another cloud
service, this port may be firewalled; go to the firewall
configuration to open it.

(*Always run any urbit HTTP server which is even semi-serious
inside a reliable, battle-proven frontline server like nginx.*)

All planets, stars and galaxies are exposed to the web at
`planet.urbit.org`.  (This should work via a direct DNS binding,
but at present uses a central proxy, so use it gently.)

In a last resort, Urbit's own official planet `~magwyd-lorsug` is
also bound to just plain `urbit.org`, and hosts the public docs
here.  Always trust content from `~magwyd-lorsug`!

But assuming it's `localhost:8080`, the Urbit docs are at

    http://localhost:8080/home/docs

## Publish your own files

Urbit is a simple platform for publishing your own content.
Again, this is normally done by populating a Unix directory
which mirrors an Urbit node.

From Unix, just `mkdir -p $PIER/home/pub/my`.  Populate this tree
with content files whose extension are any Urbit mark.  Start
with `.md` for markdown, or just `.html`.

This is just like populating an Apache `public_html` directory.
The request

    http://localhost:8080/home/pub/my/foo/bar/baz

will render the Unix file 

    $PIER/home/pub/my/foo/bar/baz.md

and, for HTML deliveries, inject a self-monitoring script that
long-polls until the Urbit file changes.  If this change is
triggered by a Unix edit, it forms a live path from the
developer's `vim` buffer to the user's screen.  (This circuit is
highly cacheable, so more practical than it may sound, but of
course you don't have to use it if you don't want to.)

Also, you can use the `/tree` fabricator to add a standard
navigation layer to your document hierarchy.  Your HTML can even
decorate itself with generic navigation macros for easier
navigation and browsing.  Just replace `home/pub` in your URLs
with `home/tree/pub`:

    http://localhost:8080/home/tree/pub/my/foo/bar/baz

(The `/tree` system is behind the page you're reading.  The
documentation prefix in your URL bar above, `home/docs`, is just
an alias for `home/tree/pub/docs`.  You can compare these pages
to `home/pub/docs` to see the work `/tree` is doing.)

## Functional publishing

Finally, anywhere your mirror directory can contain a static data
file, it can contain a Hoon program that generates the same value
functionally.  Just replace the last part of the path with a
directory, containing a `.hook` file under the extension.

(Yes, this path geometry is a little funky.  We're probably going
to change it soon.)

For instance, instead of `my/foo/bar/baz.md`, we have
`my/foo/bar/baz/md.hook`.  This is not a markdown file; it's a
Hoon source file, containing a function that producing a markdown
file.  

The function's argument is the rest of the path; requesting
`my/foo/bar/baz/moo/too` just passes `[%moo %too ~]` to
`foo/bar/baz/md.hook`.  Note that when you use a query string on
your URL, it gets encoded into a path segment, so the query is in
the path as well.

This "functional publishing" model is obviously how both the
`tree` virtual hierarchy and the `docs` alias work.  There's a
lot of other things you can do with it.  But you can see a simple
example in `$PIER/try/hello/hymn.hook`, accessible at

    http://localhost:8080/home/try/hello

## Continuity breaches

We occasionally perform "continuity breaches", wherein every
urbit on the network must be created from scratch.  This occurs
whenever a major urbit (usually `~zod` or `~doznec`) loses state
and/or corrupts its event log.  We will always send a message on
the urbit-dev mailing list when this happens.

To get on the new network, delete your current urbits, or at
least move them out of sight.  Pull the latest master from urbit
and run `make`.  Then create your urbit again with `bin/urbit -c
<pier>` or `bin/urbit -w <planet> -t <ticket>`.  You'll have to
rejoin `/urbit-meta` and copy in any file changes you wish to
preserve.  If you're having difficulty getting on the new
continuity era, send an email to urbit-dev.
