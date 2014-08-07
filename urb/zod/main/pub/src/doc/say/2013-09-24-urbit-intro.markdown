---
layout: post
category: blog
author: cgy
title: Welcome to Urbit
date: 2013-9-24 15:00
---
But wait - what the hell is Urbit?

One of Urbit's problems is that we don't exactly have a word for
what Urbit is.  If there is such a word, it somehow means both
"operating system" and "network protocol," while somehow also
implying "functional" and "deterministic."  

Not only is there no such word, it's not even clear there
_should_ be one.  And if there was, could we even hear it?
As Wittgenstein said: if a lion could talk, we would not
understand him.  But heck, let's try anyway.

As a network protocol, we could call Urbit a "seven-layer
protocol" - that is, a protocol that specifies the complete
semantics of the general-purpose computer that processes it.  As
any decent IETF ninja will tell you, this is a very bad idea for
all sorts of extremely obvious reasons.

And from the OS perspective, Urbit is yet another slice of
userspace crap with the temerity to call itself an "operating
system." Urbit is not actually an OS in the bare-metal sense.
It's a VM that runs on Linux or OS X.  Someday it might be so
daring as to descend to Xen.  Urbit has no interest at all in
drivers, peripherals, etc.  It is just a UDP transceiver in the
cloud.  Worst of all - Urbit is not even preemptive.  A poser!
(Actually all the real work, as in node, is done by libuv.)

Moreover, this VM is formally isolated from your actual OS.  And
everything that uses it.  Nothing in Urbit can request system
services or call existing libraries.  So Urbit is not only badly
designed and fraudulently hyped.  It's also profoundly useless.

Well... your browser has been reasonably successful with this
restriction.  But your browser was never designed to be any kind
of OS.  To the extent that it's sort of become an OS, it is
specialized for the very undemanding job of being a client.  A
general-purpose client, which is neat.  But not a general-purpose
server - which is a much harder problem.

A general-purpose server is a slab of code that feels totally
confident when faced with the problem of storing _all your
personal and/or corporate data_, across _arbitrary functional
upgrades_, for _all time forever_, while efficiently executing
and managing _any useful program or programs, transient or
persistent_.  Yeah, that's a server OS.

So, conventionally, this industrial strength slab of code is
written with conventional OS techniques involving (a) bare metal
and (b) like, semaphores and shit.  The kernel alone is like 12
million lines of code.  Not that a browser is any much smaller.

And so, 20th-century network computing is the world's most
beautiful wire, between two dumpsters of shit spaghetti.  Two
_completely different_ dumpsters.  It turns out that with a big
enough dumpster of shit spaghetti, you can feed the world.  And
why not two?  Incompatibility creates jobs, you know.

Oh, and you can have a client without an identity.  But you can't
have a _server_ without an identity.  So Urbit has to solve _that_
problem.  Unless it's just going to be a Web server.  (Urbit is
actually a perfectly decent Web server.)  Unless it has an actual
identity model, your general-purpose server - presumably a
_network_ server - has no network effect.  No identity, no
network.  No network, who the hell cares?

And unlike your ordinary, common or garden Web application
server, Urbit does need that network effect.  You see, it's not
even our own work.  It's something we found.  On an unmarked USB
stick by the side of the road three kilometers from Area 51.  

We think it's probably still human engineering.  First of all,
there are no aliens.  Second, Urbit uses Unicode.  Do the aliens
have Unicode?  Or SHA-256?  _Where did Unicode come from,
anyway?_  And at the I/O level, we see UDP, HTTP, etc.  The
command line does Emacs keys.  At the very least, someone on
Earth (or at least _from_ Earth) has done some porting.

But other than these cosmetic details, there's not much sign of a
connection to ordinary Earth computing.  For instance, Urbit
isn't written in any of our Earth languages.  It is written in
something called Hoon, which is a strict, higher-order, typed
functional language - but has nothing in else in common with 
other Earth languages, functional or not.  Hoon does not even 
use standard PL theory.  And its syntax is just as alien, although 
at least it confines itself to the ASCII plane.  (And you
probably thought the "A" in "ASCII" stood for "American.")

Worse yet, Hoon is not written in anything normal.  It's written
in Hoon.  To be precise - the Hoon compiler compiles itself to a
much simpler automaton, Nock.  Besides machine language itself,
and its various bytecode homages, there are three simple formal
models of computing - Turing machines, lambda calculus, and Chuck
Moore.  There is also a fourth which no one has ever found useful
for anything: combinators.  Nock is a combinator automaton.

While nowhere near the simplest such automaton known, and
certainly not of any _theoretical_ interest, Nock is so stupid
that if you gzip the spec, it's only 374 bytes.  Nock's only
arithmetic operation is increment.  So decrement is an `O(n)`,
operation; add is `O(m*n)`... 

Clearly not a practical system.  Even if this... _thing_... that
sits on top of it was in any way, shape or form remotely sane.

So why not try it?  Heck, why not at least check it out?  Strange
and foolish dreamers may hie themselves to teh github:

[https://github.com/urbit](https://github.com/urbit)

Various people have built and run Urbit on Ubuntu, Homebrew and
MacPorts.  It's possible that all these people were stone cold
ninjas.  Urbit is a pretty cool toy, I think, if you're a ninja.
Unfortunately it is not really ready for ordinary rice farmers.
If you can't resolve build dependencies by hand, we're sorry.
Urbit is probably not yet ready for you.

Where should you run Urbit?  Ideally, in the cloud.  Urbit can
punch a decent NAT hole.  It doesn't like to, though - what does?
Bear in mind that your Urbit instance is a single-level store -
your state is saved as a log and a checkpoint (as in Redis -
except that Redis is both fast and reliable).  This will work
much better on server hardware.  That said, there are plenty of
good reasons to keep your servers in the closet with the plants.

Next, you need a ticket.  Or not.  You at least need to decide
whether you want a ticket or not.  Actually, the answer is
simple.  You do want one.  But you don't need one - not yet.

Because Urbit, the OS, never depends on Earth semantics, it needs
its own network protocol - Ames.  Ames is a P2P protocol with its
own public-key infrastructure.  (Ames is encrypted, but the
current cryptosuite, suite A, is worthless snakeoil.  Don't trust
it with your Trader Joe receipts.)  Ames runs over UDP, and pays
as little attention to IP routing as possible.  Needless to say,
Urbit does not use the DNS at all.

To sling packets on Ames, you need an identity.  Needless to say,
there are a lot of ways to do distributed cryptographic identity,
all of them bad.

The general reason all PKIs suck is called "Zooko's Triangle."
Your identities can be distributed, secure, or human-meaningful -
pick any two.  There is no way to solve Zooko's Triangle.  The
only way to attack it is to compromise on at least one vertex.

To see how Urbit works, let's start with a standard solution.  An
Urbit identity is called a "ship."  You can launch your own ship
by generating a 2048-bit RSA key and hashing it to a 128-bit
fingerprint, which is your identity.  This trivial design is the
basis of all distributed, secure PKIs.

Unfortunately, an identity should be above all a _name_.  A
128-bit fingerprint looks like this:

        316.931.986.049.624.498.975.974.005.376.666.572.699

which isn't a name.  For no better reason than the fact that,
unless you're an autistic savant, you are basically a monkey with
an overgrown monkey brain.  A name is something that fits in a 
register.  Your monkey brain doesn't have 128-bit registers.

Suppose we could use 64-bit fingerprints?  At first this seems
even less promising.  First of all, your monkey brain doesn't
have 64-bit registers, either.  Second, at 64 bits, collisions
are already a real problem.

But we can solve both these problems.  Your monkey brain doesn't
have 64-bit registers.  But anything that lets us do 64-bit
identities might stretch down to 32 bits.  And at 64 or 32 bits,
we can't assign identities by random hash.  So we'll have to find
another way of distributing them.

A 32-bit identity - or even a 16-bit identity - will still never
be human-meaningful.  Perhaps we can make it human-memorable.
Meaningful addresses are a nice feature in a social network, but
memorable addresses are essential.  And if we have memorable
addresses, perhaps we can build meaningful names on top.

The classic example of a memorable address is a street address.
Your street address identifies you precisely, using a name.  You
have this great brain hardware for remembering names.  But what
does the name of your street _mean_?  Nothing at all, usually.
And even if it does mean something, by accident or design, that
meaning has nothing at all to do with you.  (One of us grew up in
Columbia, Maryland, which has street names like "Greek Boy
Place.")

So all we need is a simple, tasteful, memorable way to remember
32 bits - and we're on our way to approximating the Triangle.

Decimal notation is the worst way of remembering a 32-bit number.
IP notation is a little better.  Urbit has a typically crude
approach: we map every byte to a CVC phoneme, making names like:

       ~tasfyn-partyv
       ~sivbud-barnel
       ~tomsyt-balsen

These strings, while quite meaningless, are no less memorable
than real human names in many a language.  Moreover, they form a
language of their own, and become more memorable as you use them.
And there are 4 billion of them, which (as we'll see) is almost
exactly the right number.

But how do we distribute them?  One obvious solution is a proof
of work scheme, as in Bitcoin.  Coordinating a global
proof-of-work scheme is quite nontrivial, however.  Also, there
is a second reason to compromise 100% decentralization: packet
routing.  It might be possible to use a blockchain as a global
routing table.  It would take some thinking about.

Furthermore, there's a clue here that the Bitcoin approach just
isn't getting.  The limited subspace of short names, within the
general space of 128-bit names, is essentially _real estate_.
There is absolutely no reason, moral or practical, to give this
real estate away for free to people whose only contribution is
generating CO2 on their GPUs.  Mining is not in any way a
productive activity.

Rather, initially, this real estate belongs to Urbit itself.  If
Urbit has value, its real estate has value.  If Urbit has no
value, its so-called real estate is a bunch of worthless bits.
Therefore, any value in the real estate can, should, and will be
used to bootstrap the system from an economic perspective.  Ie,
it belongs to and will be captured by Urbit's developers and/or
early adopters.  If you find this morally wrong, sorry.  You're
probably some kind of a communist.

But because Urbit is a free republican society - not (ahem) a
fascist corporate dictatorship like Google, Facebook or Twitter -
a crucial aspect of launching or transferring a ship is that the
decision is irreversible.  

As the master of an Urbit ship, your informal title is
cryptographic and _allodial_ - no one, not the government and
certainly not us, can challenge it.  Unless the attacker can
steal your secrets.  In which case, of course, she might as well
be you.  That's like Bitcoin too.

If Bitcoin is money, Urbit is land.  (Floating land is still
land, if there's a limited amount of it.)  You own both in the
same way, by proving you can keep a secret.  A Bitcoin is not
useful for anything, except selling to a greater fool.  (We're
just kidding - we're huge Bitcoin fans.)  But an Urbit ship is
directly useful, so long as Urbit itself is useful.

You fill your Bitcoin wallet either by creating new coins, or
buying old ones from Satoshi and his cronies.  You build your
Urbit fleet by buying ships from us and our cronies.  (Don't ask
what we had to do to get them from the aliens.  Those aliens are
into a lot of strange shit, man.)  Ships are transferable, but
Urbit is not designed to be a digital currency.  Transaction
overhead is artificially high.  Again, as in real estate.

Urbit at present is empty and worthless.  So 32-bit ships -
destroyers - are $0.  Launch a 128-bit ship (a submarine) and
ask me, ~tasfyn-partyv, for one.  We'll send you one, two, or a
dozen.  You can be as anonymous as you want, if you're polite.
But, if the network lives, a destroyer price will develop.  It
will be very low at first, but not zero.  Urbit is designed to be
free as in speech.  It's not designed to be free as in beer.

How, cryptographically, are cloud ships distributed?  And how are
packets routed?  The answer is the same - the prefix hierarchy.

In Urbit's naval terminology, ships above 64 bits are 
"submarines."  64-bit ships are "yachts." 32-bit, "destroyers."
16-bit, "cruisers."  8-bit, "carriers."  This also resembles a
feudal hierarchy, so it comes with a feudal terminology.  There
are 256 imperial carriers, 65.280 royal cruisers, 4.294.901.760
rebel destroyers, uncounted scads of private yachts, and more or
less infinitely many rogue submarines.

Every ship but a submarine has a "will," or certificate chain - a
linked list of "deeds."  The first deed in this list is signed by
the ship's hierarchical prefix, or "flagship."  Mere submarine are
independent; carriers create cruisers; cruisers create
destroyers; destroyers create yachts.  

A submarine is the fingerprint of its own public key; a carrier's
fingerprint is predefined in the kernel.  Anyone can create any
number of 128-bit submarines, whose free and independent
society the 64-bit naval hierarchy cannot interfere with.  And
of course, since Urbit is (a) in the public domain and (b) not
patented, anyone can fork Urbit and change the carrier
fingerprints.  Anyone can also create his own independent and
incompatible DNS, but efforts in this direction have not been
crowned with great success.  In general, the easier it is
technically to fork open-source code or an open standard, the
less likely a fork is to actually happen.

An independent ship is an independent reputation.  Your flagship
hierarchy, though it created your ship, has no control over it -
so your reputations are and should be separate.  But there are
not 2^64 independent reputations in Urbit, only 2^32.  Cruisers
have no control over the destroyers they create, but yachts have
no independence from the destroyers that created them.

The reason for this is simple - a destroyer corresponds not to a
person, but  to any institution with its own independent
reputation.  Yachts are for users, bots, or other sub-identities
of this institution.  Each destroyer has 2^32 of them, which is,
of course, a lot.

How does independence work in practice?  By pinning/TOFU.  For
any deed number, or "life," the first instance is accepted.  Thus
when changing secrets, perhaps to transfer a ship, the donor
signs a new deed created by the recipient.  Once any ship sees
this deed, it will never accept another signed by the old owner.
Thus, a cruiser cannot sell the same new destroyer twice.
Similarly, deed 7 of ~tasfyn-partyv signs deed 8; but no ship
which has a deed 8 for ~tasfyn-partyv will either accept deed 7,
or any other purported deed 8 which deed 7 later signs.

Preventing a "double spend" thus depends on propagating the
latest deed.  For this purpose, the ocean is divided into two
kinds of ships: friends and non-friends of the ship being
transferred.  The ship has a list of its cryptographic partners,
or "neighbors," for which it holds a symmetric key and a routing
(IP) address.  

The new owner sends the new deed to all the neighbors, thus
locking the old owner out of them.  Future new friends will get
the new owner's will the same way they get the new owner's IP
address - by a lookup through the flagship hierarchy.  Thus if we
update both neighbors and flagship, the old owner is locked out
and the new owner is locked in.

Technically, this is very much an imperfect procedure.  It relies
on social trust to make it effective.  For example, a malicious
seller could edit his neighbor list before delivering the ship.
You probably don't want to buy a used destroyer from someone you
distrust.  Of course, the same is true of cars.

And more broadly, the naval hierarchy can and should provide
general administrative support.  For one thing, the big ships
route your packets, at least when establishing contact with your
fellow destroyers.

So - do you want an Urbit destroyer?  You know you do.  Moreover,
they're free.  Of course, if you are satisfied with a name that
looks like

        ~machec-binnev-dordeb-sogduc--dosmul-sarrum-faplec-nidted

you can keep swimming around Urbit in your submarine.

For a while, anyway.  Because, besides memorability, there is
another reason for short names.

What's neat about short names is that there's a finite number of
them.  This is not a bug, but a feature.  Essentially, to borrow
the thinking of political scientist James Scott, a finite space
is _governable_.  An infinite space is ungovernable.

If there are an infinite number of identities, there is no way
for anyone to distinguish between a new user and a banned user.
A reputation can be punished by destroying it, but anyone can
start again at zero.  A parasite whose only reason to use the
network is to abuse it can keep coming back for more.  An email 
spammer will never run out of addresses to spam from.

IPv4 is a limited space, which almost but doesn't cure spam.  The
problem is that IPv4 addresses are neither personal nor property,
so there is generally no easy way to punish a spammer as he
deserves through IP blacklisting.  He is very unlikely to be in
any sense the owner of the IP address on his packets.

But if the email address and the IP address were the same thing,
and the present fuzzy economic relationship between the user of
an IP address were clear and simple, killing spam would become
easy.  You spam from a destroyer; you go on a list of spammers;
no one will accept your unsolicited messages, ever.

You can get around this.  You can buy a new destroyer.  But the
thing is - it costs you *money*.  You're not spamming for the
fun of it.  If a destroyer costs a mere $1, the spam you send
from it needs to earn you $1.  

This does not make it trivial for the forces of light to hunt you
down and render you into processed meat clippings.  But it sure
as heck evens the game.  Who will win?  I guess we'll see.

So do you want an Urbit destroyer? Read the [documentation](/2013/11/18/urbit-is-easy-ch1.html) for instructions. Come on, they're free...
