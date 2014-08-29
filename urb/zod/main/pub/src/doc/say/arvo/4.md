# Urbit is a global functional filesystem.

The whole idea of a filesystem is that a file is a function of a
path.  If it's an *immutable* filesystem like Urbit, the function
(though partial), once bound never changes.

This is kind of neat.  What'd be really neat, though, is if the
namespace operator, rather than just returning one static file,
produced an immutable function on your whole filesystem.  Or
better yet, on *all visible data in the world*.  

It would also be ideal if the product of this function was not
just a binary blob, but a *typed data structure*.  Even better -
the product might not be data, but code, or a mix of data and
code - a function, an object, a library, even an application.

What's basically irritating about the Web is that it's almost
this, but it's not this.  The Web is almost a web-of-data, but
it's not a web-of-data.  So it does not scale as a substrate for
complex higher-level communication patterns, and even for simple
use cases breaks frequently in production.

The road to a real *global functional filesystem* is perfectly
straightforward in principle.  If a function of one path is
immutable, so is a function of two, or n.  More generally, any
immutable function of a filename is a referentially transparent
name - a concept familiar in many functional languages.

We already put source files in immutable filesystems, which we
call "revision-control systems."  To convert this design into a
functional filesystem, all we have to do is define a precise
functional semantics for these source files, including in that
semantics the power to read back into the namespace.  

In principle this is quite straightforward.  We need exactly two
components for our functional filesystem: an immutable filesystem
and a functional language.  Neither are particularly exotic bits
of system software kit.

But they should be made to fit each other.  Could you strap
github to Haskell and make a global functional filesystem?  Well,
sort of.  Certainly in principle.  You could also strap a car
engine to a mountain bike, and make a motorcycle.  It wouldn't be
a very good motorcycle.

Urbit is not actually a very good motorcycle.  Not yet, anyway.
It is a motorcycle, though.  Let's take it for a spin.


# Get your urbit.

First, grab yourself a "destroyer" at urbit.org.  You'll come
away with an phonemic urbit name, or "ship" - mine is
`~tasfyn-partyv`.  Let's say `~lacser-dovlet` is yours.

As the length of the string suggests, here are only 2^32
destroyers, so we have to ration them.  You can skip this step,
but only at the expense of urbiting from a mere "submarine," a
self-generated 128-bit ship - like:

    ~bacbyl-dabdyr-marfel-palnep--tompex-nibnym-sidpet-ticfus

In theory a submarine can do anything a destroyer can.  In
practice, if you have any interest in networking with other
ships - don't be surprised if they assume you're a bot.

Obviously, your ship is the root of your own personal namespace.
`~lacser-dovlet` is your "me function."  Urbit, as a global
functional filesystem, is the union of everyone's me function.

(Urbit should not be confused with distributed storage systems,
like Freenet or IPFS.  In Urbit all your data is on your own
computer, though it may be a virtual computer in the cloud.)


# The shape of urbitspace.

But wait.  How, exactly, are these shorter names allocated?  

As Samuel L. Jackson once put it: for money, bitch!  But you're
an early adopter and we love you.  So free ships for you, your
friends, and everyone.

Seriously - it will be a *long* time, if ever, before you have
any trouble getting a free Urbit destroyer, from us or anyone.
The main thing we want to make sure is that we're giving them
out mostly to unique humans.  In the long run, this is quite hard
on the Internets.  We're still small so no abuser gives a hoot.

And we (the evil Tlon Corporation, to be exact) are *not* the 
only people who *can* give away destroyers, just the only people
doing it right now.  Actually, some of the people who hold this
right are kind of our enemies - or at least, disgruntled former
employees (of the evil Tlon Corporation).  They obviously can't
be trusted to collude in our evil schemes!  So if you fear us,
you're probably safe with them.

Moreover, more importantly: once you own a destroyer (or more
realistically, once you sail it away from our harbor), it's
cryptographically yours and we (the evil Tlon Corporation) have
no practical way to steal it back, spy on you, etc.  No, Urbit
isn't just another way of selling yourself into digital serfdom.

It's also important not to confuse Urbit with a cryptocurrency.
Urbitspace is digital land, not digital money.  It is more like
the DNS than like Bitcoin.  As in real estate, Urbit transactions
are unusual and have high frictional cost.  

Making Urbit a bad currency is a feature, not a bug.  Urbit is
designed to be neither particularly easy for governments to
control, nor particularly useful for criminals to abuse.  Both
are difficult goals to hit, but they go well together.  Any
system that attracts criminals will inherently also attract their
natural predator.

Also, as with land, the price of urbitspace is driven by direct
utility, not just collective speculation.  And the only way land
is ever given away for free is as a homestead.  Your free urbit
is for use, not for investment.  Urbitspace will be worth
something if and only if it's useful to someone.


# More namespace design digressions.

Feel free to skip these if you really couldn't care less why.

Why are ship names synthetic strings, not user-chosen strings?
The simplest way to assign handles is with a central authority,
like Facebook.  If you like being a digital serf, please, you
already know where to go.  But even if you issue handles with a
proof-of-work scheme (like Namecoin), you still have a name rush.
And a decentralized system can have no way of negotiating with
real-world name owners whose existing claims conflict.

Also, there are concrete social benefits to a completely
impersonal address.  An Urbit name is like a street name.  It
doesn't mean anything and is not intended to.  Even if it
accidentally means something, everyone ignores the accident -
when we get directions to a Bush Street or a Clinton Road, we
rarely even think of thinking of the politicians.  Worse, handles
in real life tend to be just lame.

Why use short names at all, not just hashes?  Why destroyers?
Why doesn't everyone just sail the Urbit seas in a submarine?

Two very important reasons.  One, with only 2^32 names, an Urbit
name can be as memorable as a natural human name, or at least a
name in a foreign language.  Two, when we have a limited supply
of names, network abusers don't find it so easy to disappear and
reappear under a new name.  

(In fact we're very confident that it will be possible to keep
Urbit spam-free in practice.  This would be harder if synthetic
short names were distributed as coins in a proof-of-work scheme.
Since Urbit treats namespace as property from day one, with no
"airdrop" to unproductive miners, there are no anonymous original
owners of Urbitspace.  Everyone who owns a block has an incentive
to distributed it only to well-qualified individual users: as
with both IP addresses and actual real estate, Urbit blocks can
easily develop a collective bad (or good) reputation.)

It is of course possible to use Urbit anonymously, of course.
But only in one way: somewhere along the decentralization chain
from the (well-identified) developers to the user, someone has to
transfer urbitspace and be trusted to forget the recipient.  It
is to be expected that, in order to provide this useful service,
the intermediary will demand some trust from the upstream users -
presumably, at least, evidence of humanity.

Point is: these are not the droids you're looking for.  You are
not selling yourself into digital serfdom.  You are escaping from
digital serfdom.  You are not selling yourself into digital
serfdom.  You are escaping from digital serfdom.  Freedom is
slavery.  Urbit is freedom.  Now, please raise your right hand
and continue with the installation process.

# Install the SDK.

(Instructions here.)  

If you got a destroyer, this will set up a local "yacht" on your
own computer.  (Otherwise, just work out of your local
submarine.)  Your destroyer lives in the cloud, where computing
is actually reliable.  Leave it there.  (You can sail it home, or
to some other harbor, any time you like.)

Suppose your destroyer is `~lacser-dovlet`, and your yacht is
`~sidpet-lacser-dovlet`.  Urbit will make the directory
`~/urb/sidpet-lacser-dovlet/`, and symlink it, for convenience,
to `~/urb/lacser-dovlet/`.

This is a hotsync directory.  When you edit, add or delete files
under the *pier* directory, `~/urb/sidpet-lacser-dovlet/`, it
syncs them automatically with your yacht, then syncs the yacht
with `~lacser-dovlet` in the sky.  Sort of like Dropbox.  Except
that (a) Dropbox is a hard disk and Urbit is a computer, and (b)
your Urbit computer is actually, in some sense, like, yours.
(Urbit is freedom.  Repeat.  Urbit is freedom.)


# Post and edit a webpage.

Start vere: 
    
    $ vere ~lacser-dovlet

Run vim on `~/urb/lacser-dovlet/main/pub/fab/fun/one.md`, and type:

    This is a *fun experiment* in markdown.

Save the file.  Then point a browser at

    https://lacser-dovlet.urbit.org/pub/fab/fun/one

You'll see the rendered markdown.  Then, change the file to read

  This is a *different fun experiment* in markdown.

Save it.  Then look at your browser.  Did you have to hit reload?
*How exactly did that file get from your editor to your browser?*


# Urbit filesystem basics.

It's difficult when looking at this system superficially to
separate the web server from the namespace.  Your namespace is
your namespace - the web server is just a legacy translation
layer from HTTP's rather non-functional namespace.

(In some senses it might make more sense to compute functionally
from Urbit's own command line.  But your browser is a much better
renderer than anything in Urbit, and you're better at using it.)

We have seen two kinds of filenames go into Urbit: Unix filenames
and URLs.  Obviously, neither of these is referentially
transparent.  And neither is the same thing as an Urbit path, or
*beam*.

An Urbit beam has mostly the same syntax as a Unix path.  The
beam always begins with the same three spans, `/ship/desk/case/`.

A ship is your computer, of course; a desk is a branch, project,
repository, etc; a case is a version.  Versions are labels,
change numbers, or dates.

Besides the fact that revision control is part of the filesystem, 
the only other weird thing about Urbit is that files and
directory are orthogonal.  In Unix, you can't have one file
`/one/two/three` and another file `/one/two/three/four`, because
the `three` inode would have to be both a file and a directory.  

Urbit uses different APIs for data and metadata requests, and
directories are implicit.  The directory `/one/two/three` exists if
and only if there are files with that path as a prefix.  There is
no `mkdir`, etc.

Without clunky stateful directories, our normal convention is to
use the last span of the path as the file type.  So where Unix
has `/one/two/three/four.html`, Urbit has `/one/two/three/four/html`.


# Functional transformations.

In the test above, we started with a Unix filename, synced it
against an Urbit beam, and served it up as a URL.  Ie, we went
from the Unix filename

    ~/urb/lacser-dovlet/main/pub/fab/fun/one.md

to the Urbit beam

    /~lacser-dovlet/main/~2014.7.30/pub/fab/fun/one/md

to the URL

    https://lacser-dovlet.urbit.org/pub/fab/fun/one

What happened internally?  Without getting into too much
detail, the clean URL above was defaulted to

    https://lacser-dovlet.urbit.org/pub/fab/fun/one.html

which then made Urbit try to serve the file

    /~lacser-dovlet/main/~2014.7.30/pub/fab/fun/one/html

which did not exist, but we found your markdown source

    /~lacser-dovlet/main/~2014.7.30/pub/fab/fun/one/md

and parsed it into the markdown tree

    /~lacser-dovlet/main/~2014.7.30/pub/fab/fun/one/down

which was converted into the HTML tree

    /~lacser-dovlet/main/~2014.7.30/pub/fab/fun/one/hymn

printed out into the HTML text file

    /~lacser-dovlet/main/~2014.7.30/pub/fab/fun/one/html

and finally, wrapped up as the MIME object

    /~lacser-dovlet/main/~2014.7.30/pub/fab/fun/one/mime

to ship back to you with a 200.  Then, when you saved `one.md`
again, we did all this again and showed you the new document.
(We'll again delay on how *this* happened.)


# Functional construction.

This kind of format translation is a relatively straightforward
use of functional FS techniques.  It's definitely handy but
doesn't really rise to the level of cool.

Here's something cool.  Again with `vim`, edit

    ~/urb/lacser-dovlet/pub/fab/fun/two/down.hook

to be 

    :~  :-  %par 
        :~  tex/"This is a "
            emp/bent/~[tex/"fun experiment "]
            tex/"in markdown."
    ==  ==

Now, point your browser at 

    https://lacser-dovlet.urbit.org/pub/fab/fun/two

What did we do here?  Instead of a static file containing
markdown text, we wrote a program in Hoon that generated 
a markdown value.  How did we find this?  We went from

    https://lacser-dovlet.urbit.org/pub/fab/fun/two.html

to the file you created, 

    /~lacser-dovlet/main/~2014.7.30/pub/fab/fun/two/down/hook

Then we followed a simple rule: you can make the file
`/one/two/three` by compiling and running the source file
`/one/two/three/hook`.  

    /~lacser-dovlet/main/~2014.7.30/pub/fab/fun/two/down

and then proceeded as above.  You can test that this is really a
program, and still autoloads and stuff, by doing something funky
- like:

    :~  :-  %par 
        :~  tex/"This is a "
            emp/bent/~[tex/"fun experiment "]
            tex/"in markdown; 2 + 2 is {<(add 2 2)>}."
    ==  ==


# Computing an actual function.

While these files are functions in a sort of abstract sense, they
are not actually functions in the normal sense of the word.  A
function needs arguments, after all.

Since we're talking to our function through the Web, it makes
sense to try to send arguments as query strings.  Also, though
we've had some fun generating markdown nouns (with totally
generic Hoon), it seems more appropriate since we're writing
webpages and stuff to generate HTML.

Let's edit 

    ~/urb/lacser-dovlet/main/pub/fab/fun/three/hymn.hook

and put in

    ;html
      ;head  ;title: Fun Experiment Three
      ==
      ;body
        ;p: This is an ;{i "HTML file."}
      ==
    ==

and view it at

    https://lacser-dovlet.urbit.org/pub/fab/fun/three

We saw above how a `%hymn` turns into an `%html`, so we don't 
need to repeat it.  But again, this source file is best seen
as a program which produces an HTML tree.  Sorta like the DOM.

(This syntax is also totally generic Hoon.  In a sense - Hoon is
customized without apology for building XML nouns.  In fact, it
has special cases for `<script`> and `<style>`, so it's even
slightly specific to HTML.  This is not the most elegant language
design decision in the world, but it seemed preferable to making
people use a separate template language.  Hoon is not the world's
best template language, but it's not just a template language.)
 
But what would be nicer would be - 

    =+  fib=|=(x=@ ?:((lth x 2) 1 (add $(x (dec x)) $(x (sub x 2)))))
    ;html
      ;head  ;title: Fun Experiment Three
      ==
      ;body
        ;p  ; This is an ;{i "HTML file"} which 
            ; computes the Fibonacci number
            ; of 12: {<(fib 12)>}.
        ==
      ==
    ==

There's clearly a *function* here.  But it's not *of* anything...


# An actual function, query string edition.

Okay, so we want an actual function.  In

    ~/urb/lacser-dovlet/pub/fab/fun/four/hymn.hook

put

    /=    gas  /$  fuel
    ::
    =+  arg=(biff (~(get by qix.gas) %number) (slat %ud))
    =+  fib=|=(x=@ ~+(?:((lth x 2) 1 (add $(x (dec x)) $(x (sub x 2))))))
    ::
    ;html
      ;head
        ;title: Fun Experiment Four
      ==
      ;body
        ;p: Welcome, {<cip.ced.gas>}!
        ;+  ?~  arg
              ;p: Usage: ?number=x
            ;p  ; This is an ;{i "HTML file"} which 
                ; computes the Fibonacci number
                ; of {<u.arg>}: {<(fib u.arg)>}.
            ==
      ==
    ==

Try it:

    https://lacser-dovlet.urbit.org/pub/fab/fun/four?number=14
    https://lacser-dovlet.urbit.org/pub/fab/fun/four?number=144
    https://lacser-dovlet.urbit.org/pub/fab/fun/four

This page does appear to be a function of the query string - and
some other stuff.  For one thing, Urbit is now tracking your
behavior.  Or at least your IP address.

Actually, at the Web level, the page broadly speaking is a
function of the query string and the session state.  The usual
Web cruft, basically.  

However, the page is not computed at the chaotic Web level - it's
computed at the functional publishing level.  Functional
publishing does not know the Web exists.  We can reveal the true
and complete input to the page function with a sneaky trick.

In 

    ~/urb/lacser-dovlet/pub/fab/fun/five/hymn.hook

put

    /=    ctx  /$  |=([p=beam q=path] +<)
    ::
    ;html
      ;head
        ;title: Fun Experiment Five
      ==
      ;body
        ;p: This page was built at {<(tope p.ctx)>}.
        ;br;
        ;p: The remainder path was {<q.ctx>}.
      ==
    ==

Try it:

    https://lacser-dovlet.urbit.org/pub/fab/fun/five

What you'll see is that `q.ctx`, which we used to be passing to
the Web path parser `fuel`, is an ugly (but URL-safe) string
containing data, encoded as a path component - let's say, 
`web/DATA`.  This string is actually inverted - properly
speaking, it's `DATA/web`.

When we want to compute a dynamic functional namespace which is
not just a trivial static file, or a translated file, we divide
the Urbit beam into two parts - *driver* and *remainder*.
The driver is the path to the function; the remainder is the
input to the function.  

In this case, the Urbit webserver asked the filesystem for

    /~lacser-dovlet/main/~2014.7.30/pub/fab/fun/five/DATA/web

with the content type `%html`.  (We lied a little earlier - the
content type, or `mark`, is sent separately to the filesystem.)

To resolve the beam, we travel up it from the bottom, looking for
drivers of some sort.  Urbit did not find any files in
`fun/five/DATA/web`.  It also did not find any files in
`fun/five/DATA`.  

Going up, it found `fun/five/hymn/hook`, which it took as a way of
making `fun/one` with the mark `%hymn`.  Ie, an HTML document
tree, which we know how to turn into `%html`, so great.

Then, it gave the remainder of the beam, `/DATA/web` - but 
inverted to `/web/DATA`, because we decode the remainder from
the end inward - as a resource to our page generator.  And this
is how we could print your IP address and stuff...

