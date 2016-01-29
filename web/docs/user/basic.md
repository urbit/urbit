---
next: true
sort: 3
title: Basic Operation
---

# Basic Operation

Once urbit is installed, if you have an invitation, it's a planet
like `~fintud-macrep` and a ticket like
`~fortyv-tombyt-tabsen-sonres`.  Run

    bin/urbit -w fintud-macrep -t fortyv-tombyt-tabsen-sonres

(You can leave the `~` on, but it annoys some Unix shells.)

If you don't have an invitation, pick a nickname for your comet,
like `mycomet`.  Urbit will randomly generate a 128-bit plot:

    bin/urbit -c mycomet

Either way, creating your urbit will take some time.  Some of
this time involves generating keys; some of it involves
downloading code over Urbit itself.

If you turn off your pier at any time (either "nicely" with `^D`
from talk or the dojo or by power-failure, oom-killer, or other
calamity), then you can start it up again with the path to your
pier directory (your planet name if you have an invitation,
otherwise the name you chose after `-c`):

    bin/urbit fintud-macrep

or

    bin/urbit mycomet

## Basic operation

Out of the box, your urbit is running two default appliances,
`:dojo` (a shell or REPL) and `:talk`.  Switch between them with
`^X`.  Note that all apps share an output log, but `^X` switches
the prompt.

`^D` from either default appliance exits the urbit process.

If your plot is `~fintud-macrep`, the dojo prompt is

    ~fintud-macrep:dojo>

Type any Hoon expression at the command line and see the result:

    ~fintud-macrep:dojo> (add 2 2)

You'll see:

    > (add 2 2)
    4
    ~fintud-macrep:dojo>

While there are some tools for interacting with urbit's
filesystem direclty, often it's useful to edit urbit files from
Unix.  Use `|mount` to set up a Dropbox-style sync directory in
your pier directory:

    ~fintud-macrep:dojo> |mount %
    >=
    ~fintud-macrep:dojo> |unmount %
    >=

Look inside your pier directory to find your files, and edit them
with your favorite Unix text editor.

Sometimes you want to get files from another urbit.  There's two
main commands to do this.

    ~fintud-macrep:dojo> |merge %examples ~wactex-ribmex %examples
    >=
    ; ~wactex-ribmex is your neighbor
    ; ~wactex-ribmex is your neighbor
    [time passes...]
    merged with strategy %init

This pulls a bunch of examples from `~wactex-ribmex`'s `%examples`
desk and puts them in our `%examples` desk.  A desk is a
branch (in the git branch sense) of our filesystem.

You can, of course, merge the examples into your `%home`
(default) desk if you wish.  Merging into a new desk creates it,
while merging into an existing desk does an intelligent git-style
merge, with conflict resolution and stuff.

    ~fintud-macrep:dojo> |sync %examples ~wactex-ribmex %examples
    >=
    activated sync from %examples on ~wactex-ribmex to %examples
    sync succeeded from %examples on ~wactex-ribmex to %examples
    ~fintud-macrep:dojo> |unsync %examples ~wactex-ribmex %examples
    ended autosync from %examples on ~wactex-ribmex to %examples

`|sync` does an initial `|merge`, then listens for changes on the
foreign desk and runs `|merge` automatically whenever it changes.
By default your `%home` desk is synced to your parent's `%kids`
desk, so the entire network is upgraded at once.  You may, of
course, unsync it, but this voids your warranty.

You can change which desk is your current working directory with:

    ~fintud-macrep:dojo> =dir /=examples=

> `dir` is a special dojo variable, but you can also create your
> own variables with the same syntax.  `=my-var (add 2 3)` allows
> you to use `my-var` for `5` anywhere later on.

When you change your desk all commands will use the files from
that desk.  Unfortunately, changing your working directory to
anywhere but the top directory is currently
[broken](http://github.com/urbit/urbit/issues/565).  If this is
fixed when you're reading this, please submit a pull request for
this document explaining how to do it!

The last command we'll discuss in this quickstart is `+moon`,
which only works on planets.

    ~fintud-macrep:dojo> +moon
    "moon: ~ramnec-rossug-fintud-macrep; ticket: ~holtyl-mognyl-dostyp-moslud"

This generates a random moon and ticket.  A moon is a subidentity
of your planet, so it's a separate urbit that's associated with
your planet.  If your planet is off, then your moon won't
function very well over the network (it'll work just fine
locally, of coures).

You can start up your moon with `bin/urbit -w moon-name -t
ticket`.  We recommend doing this for development because there
are still some instabilities that have the potential to explode
your planet if you develop on it.  If this happens, then your
planet will be unusable until the next continuity breach.  If you
tell us how you exploded your planet, we may give you a
replacement, but it's still a hassle for you.

Don't worry too much about exploding your moons -- you have four
billion of them, and they get replenished after every continuity
breach.  Still do file a bug, though.
