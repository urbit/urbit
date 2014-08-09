#Application programming in Arvo


**NOTE - not everything in this doc matches current reality!
Where this is the case, reality must be changed to match.**

ERRATUM: the beautiful ~/urb/SHIP... is not real.  Reference
your piers in the normal way.


## Hello, world.

Let's write a simple Arvo app, `foobug`.  Start Urbit:

    vere

From Unix, edit `~/urb/SHIP/main/app/foobug/core.hoon`, where 
SHIP is your ship *without* the ~ (for me, `tasfyn-partyv`):

    !:
    :::::::::   Foobug: a simple application.
    ::
    |_  [hid=hide vat=~]
    ++  peek
      |=  [you=ship pax=path]
      :-  %hymn
      ;html
        ;head
          ;title: Foobug!
        ==
        ;body
          ;p: Hello, world.
        ==
      ==
    --

In your browser, go to the URL:

    https://SHIP.urbit.org/foobug

Now you've written and executed your first Urbit app.

(If you are running `vere` locally, you'll go faster, not to
mention ease up on our servers, by going direct:

    https://localhost:PORT/foobug

where `PORT` is the HTTP port `vere` prints out when it starts,
usually `8080`.)

Leave your browser open and resume editing `foobug/core.hoon`.
Change "Hello" to "Goodbye."  Save the file.  Then, look at your
browser... now you've upgraded your first Urbit app.


## Introductory concepts.

Urbit apps are written in a funny little language called Hoon.
To be exact, you write an app named APPL by putting a Hoon program
in `~/urb/SHIP/main/APPL/core.hoon`.

How do you start APPL?  You don't.  Urbit starts it.  You also
don't restart it when you change the code; Urbit does that too.

Urbit loads an application as soon as someone tries to use it; it
tracks dependencies as it builds, and reloads when its source, or
any other hot-linked code or data dependency, local or remote,
changes.  

Type consistency is never broken in this process - if the
app state changes type across the reload, the replacement
brings an adapter function.  If the reboot fails, the old app
continues to run.  And as you saw, we inject a script which
auto-reloads your application page.  Just to be nice.

And of course, Urbit is a single-level store, so Urbit
applications with live state are inherently persistent and live
forever (or at least, as long as your Urbit ship).  Flushing
state to Urbit's %clay revision store - or even more drastically,
to a legacy database outside Urbit - may be desired but is not
required, not even (ideally) for mission-critical data.


## What we're doing here.

Let's look at `foobug` in a little more detail.  

Our `core.hoon` file starts with this boilerplate opening:

    !:
    :::::::::   Foobug: a simple application.
    ::

The `!:` enters debug mode.   Within the opening, we produce a core 
around the sample state

    |_  [hid=hide vat=~]

This is where all the data in our application lives.  `hid` is
the system's data, `vat` is our data.  Right now, `vat` is empty,
because our application is trivial.

Searching for where `hide` comes from, we find in `hoon.hoon`:

    ++  bone  ,@ud                                  ::  opaque duct
    ++  hide                                        ::  standard app state
            $:  $:  our=ship                        ::  owner/operator
                    app=term                        ::  application name 
                ==                                  ::
                sup=(map bone (pair ship path))     ::  subscription set
                $:  act=@ud                         ::  change number
                    eny=@uvI                        ::  entropy
                    lat=@da                         ::  date of last action
            ==  ==                                  ::

Everything above is easy to explain but `++bone`.  Perhaps the
simplest analogy: a Unix file descriptor.  

But this approximation won't really hold up under the crazy stuff
we're about to do.  Unfortunately, we can't explain apps without
`++hide`,`++hide` without `++bone` or `++bone` without Arvo.

## Arvo for undergrads

If you take an undergraduate OS class, you'll learn that there
are two kinds of general-purpose operating systems: communicating
sequential processes, and asynchronous event-driven actors.  Unix
and Go are CSP models; Node and Arvo are event loops. 

Across this pair of equally legitimate alternatives, every
architectural feature has its equivalent.  But most programmers
are more familiar with the CSP model.  For instance: in a pure
event OS, what's the equivalent of a file descriptor?  

In Arvo the answer is something called a `++duct` - or in
userspace, `++bone`.  The actual event data is a `++card`.  A
`++move`, or action, is a `(pair duct card)` in 

If you understand ducts, bones
and cards, you understand Arvo.

(Well, mostly.  But it's worth noting that Arvo proper, not the
vanes which actually do stuff, but the stuff in hoon.hoon that
makes everything do stuff, is only about 600 lines of code.)

Physically, a duct is just a list of a list of symbols.
Semantically, it's the *source* of an event - or even more
philosophically its *cause*.  But why do we need a data structure
for this?  Shouldn't the effects of an event be, in fact,
*independent* of its cause?


### Motivation

Arvo is a *structured* event system, designed to avoid the usual
state of complex event networks: event spaghetti.  In some ways
it seems more complicated than a simple standard approach.  It
is, but the systems built on top of it are much simpler.

"Event," at its worst, is an asynchronous synonym for "goto."
What is the difference between a `goto` and a `gosub`, anyway?
You may not know where your gosub goes.  But at least you know it
will in some way return to its caller.  Otherwise.. it's a goto.

In a vanilla functional event system, your application is defined
as an object with methods, one of which is something like "do" -
taking an event, returning a list of side effects and a new
application object.  One such side effect, in any reasonably
complex system, is to post an event.  Alas, this design - while
admirable on a small or simple scale - in bigger systems enters a
pathological mode in which events pile up in a sort of cascading
event ping-pong.  

This often even works.  But as with the old BASIC gotos, when it
doesn't, it's rather hard to figure out why.  So we need a gosub,
as it were.  A structured event model.  Slightly less simple than
nothing, but still not exactly rocket science.

This is slightly simplified from the actual code, but Arvo's 
structured event system is built around these models:

    ++  card  (pair term noun)              ::  event data
    ++  duct  (list (pair role wire))       ::  event source
    ++  gift  card                          ::  out result <-$
    ++  kiss  card                          ::  in request ->$
    ++  mold                                ::  general action
              $%  [%give (lone gift)]       ::
                  [%pass (pair wire note)]  ::
              ==                            ::
    ++  move  (pair duct mold)              ::  traced action
    ++  note  (pair term noun)              ::  out request $->
    ++  noun  ,*                            ::  any noun 
    ++  role  ,@tasD                        ::  role byte
    ++  term  ,@tas                         ::  symbolic name
    ++  span  ,@ta                          ::  wire segment
    ++  sign  (pair term noun)              ::  in result $<-
    ++  wire  (list span)                   ::  logical location

What the heck is this stuff?

Think about a function call in a normal procedural language, 
and its physical representation in a stack frame.  Wallace
Stevens showed us 24 ways of seeing a blackbird, but there are
only four ways of seeing a stack frame.  It can be (a) the
arguments as seen by the caller, (b) the arguments as seen by the
callee, (c) the return value as seen by the callee, (d) the
return value as seen by the caller.

In Arvo terms, (a) is a `++note`, (b) is a `++kiss`, (c) is
a `++sign`, and (d) is a `++gift`. 
 
You'll see that `++note` and `++sign` have a different shape from
`++kiss` and `++gift`.  The extra `++term` in a note or a sign
identifies the `++role` of the Arvo actor, or `vane`, that we're
calling to or returning from respectively.


### Vanes

A vane is a stateful actor within the Arvo kernel.  You are
writing user-level apps; you are not writing Arvo vanes.
But your apps use vane services, are hosted by a vane (`%gall`), 
and most important - follow the same general design pattern.

A vane's function is defined by its `role`, or interface
contract.  Te role is a single letter which defines the vane's
interface, and matches the first letter of the module that
implements it - like `e` for `%eyre`, or `c` for `%clay`.  

Eg, `%eyre` (source in `/===/arvo/eyre/hoon`) is a concrete
implementation of the `%e` interface, ie, HTTP.  If you change
`eyre.hoon`, `:reload %eyre` will install the new server.

The vanes at present:

    %a  %ames  packet networking
    %b  %batz  command line
    %c  %clay  revision control
    %d  %dill  console
    %e  %eyre  HTTP
    %f  %ford  functional construction
    %g  %gall  user-level applications


### Vane arms

An Arvo vane exports this basic interface (again, simplified for
educational purposes):

    |%
    ++  call                                ::  accept request
      |=  [hen=duct hic=kiss]
      ^-  [p=(list move) q=_+>]  !!
    ::
    ++  take                                ::  handle response
      |=  [hen=duct way=wire sih=sign]
      ^-  [p=(list move) q=_+>]  !!
    --

Whereas a hypothetical unstructured Arvo might look like:

    |%
    ++  do
      |=  car=card
      ^-  [p=(list card) q=_+>]  !!
    --

The vane hears that something happened.  It answers with `p`, a
list of more things to make happen, and `q`, the new itself.
This is simpler than Arvo; a `goto` is simpler than a `gosub`.

In Arvo, your vane exports two gates: `++call` for when another
vane calls you, `++take` for when someone you called returns.

The vane hears that *one of two* things happened.  Either it got
a request (a kiss) or a response (a sign).  There is no third
kind of a thing; there is no strange furry thing halfway between
a kiss and a sign.

Note that `++call` and `++take` produce the same product.  This
remains `p`, a list of more things to make happen, and `q`, the
vane's new value.

But now there are *two* kinds of `++move`, a `%give` and a
`%pass`.  If you suspect that `%give` means "return" and `%pass`
means "call", you may be beginning to understand Arvo.  But
first, deeper into ducts.

Arvo's structured events are the dual of procedure calls, but its
ducts are one phenomenon for which there is no exact equivalent
in any language we know.  Most procedural languages (sadly) do
not even expose their call stacks as user-level data structures.
The duct is not the full call stack, but something like a call
stack in which each frame is just the procedure name.   

But this is an inexact and meretricious analogy.  Let's look
concretely at how Arvo uses `%give` and `%pass` to build ducts.


### Ducts, events and types

Before we continue, it's essential to confess our sins in the
oversimplified models above.

In actual Arvo, there is no `++card` model at all, and each vane
has its own definition of `++gift`, `++kiss`, `++note`, `++move`
and `++sign`.  `++mold` is parameterized for your convenience
(don't use `%slip` or `%sick`, please).

Why do we have vanes, after all?  So they don't have to include
each other.  No two large systems can.  Thus, it is very much our
desire not to tangle all our vanes together - like a previous
version of Arvo in which *all events were jammed together as a
single `++card` model*.  There were almost 100 fronds in this
kelp, and it was just as heinous as it sounds.

Rather, every vane uses the type system to formally define the
gifts and notes it produces, and the kisses and signs it accepts.
Moreover, Arvo dynamically compiles every event application, and
thus actually uses these types to typecheck its events.  Event
data is never, ever de-typed and then re-rectified.  Signs and
notes even need to match the roles they come from / go to.

The downside of this is that it often makes sense to copy card
models from vane to vane, eg, from a service provider to a
service user.  Programmers should not feel the slightest
discomfort at this duplication.  Anyway, you only need to copy
the API tiles that you actually use.


### Duct examples

A duct is a `(list wire)` in which each `++wire` represents the
*cause* of a a `++call` or `++take` action.  The head of each
wire in the duct is a vane role, except for the bottom wire -
which is `%$`, for kisses from Unix to Arvo.

The rest of the wire represents an abstract location or *place*
within the vane of that role.  What does this "place" mean?  It
depends on the vane.  

Consider, for instance, the bottom wire in every duct - the
original cause of this event, which is always something that
happened at the Unix layer.  But Unix is not just one big blob.
For `%$` wires, the place is the Unix event channel - often 
quite directly corresponding to a file descriptor.

For example, HTTP requests come not from `/$`, and not even from
`/$/http/`, but from (for instance) `/$/http/0vh.ephnu/1/2`,
where `0vh.ephnu` uniquely identifies the current process's HTTP
server instance, `1` is its first connection, and `2` is that
connection's first request.  If we reboot the `vere` process, we
know we broke all the sockets of `0vh.ephnu`, which is obvious to
Unix but has to be explained to Arvo.

Ascending up in this same example, we see the deeper duct
 
    :~  /g/~zod/foobug/s/peer/~zod/goof
        /e/hoop/~zod/pidtyl-danpur-dozted-micsed/1/frog
        /$/http/0vh.ephnu/1/2
    ==

From the `/g` and `/e`, we know that the Unix event was sent
first to `%eyre` and then to `%gall`, ie, we seem to be talking
via HTTP to one of our applications.


### Duct assembly

How do we construct ducts?  Let's unroll the internal structure
of `++move`.  A move is either

    [p=duct q=[%give p=card]]

or

    [p=duct q=[%pass p=wire q=[p=role q=card]]]

From this move, we know exactly how Arvo will use what vane.
Suppose `%ford` produces a `%give` move with the duct above, 
and the card `[%test ~]`.  Where `vay` is `%gall` (`/g`), Arvo
will compute

    (take:vay t.p t.i.p [%f p.q])

where `t.p`, the rest of the popped duct, is 

    :~  /e/hoop/~zod/pidtyl-danpur-dozted-micsed/1/frog
        /$/http/0vh.ephnu/1/2
    ==

`t.i.p` is the location, `/~zod/foobug/s/peer/~zod/goof`,
`%f` is the returning vane (`%ford`), and `p.q` is the result
that `%ford` wants to return to `%gall`.

Suppose `%gall` wants to call back into `%ford` from exactly the
same location, with the same card.  It must produce the move

    [t.p %pass /~zod/foobug/s/peer/~zod/goof %f [%test ~]]

Where `vay` is `%ford`, Arvo will call

    (call:vay p [%test ~])

`p`, of course, being the original duct above.


### Duct semantics: independence principle

Recall the question we asked originally: if we don't intend to
let the programmer use the data in the duct, why is it there?
If we do intend to let the programmer use it, isn't that a
mistake?  Isn't there a contradiction here?  This duct system
seems reasonably straightforward.  But gosh, what is it *for*?

We now have a concrete answer to what a vane can do with a duct.
The first principle of responsible duct ownership: *a vane is not
allowed to make ducts*.  The only ducts a vane may produce are
those that Arvo has passed it.  Only Arvo makes ducts - like the
government with dollars.  

The second principle of responsible duct ownership: event
computations must not depend on duct contents.  Actually, it is
always wrong to peek into ducts.  You can, but don't.

Naturally, the Hoon type system cannot enforce these contracts.
We enforce the duct handling principles at the vane level by
noting that installing alternate vanes is like putting
aftermarket chips in your engine or overclocking your CPU.
Ma'am, we're sorry, but your warranty is void.

At the user level (ie, within `%gall`) we enforce the opacity of
ducts by... hiding them behind an opaque atom, the `++bone`.
It is impossible to imagine the variety of "weird machines" that
a malicious actor could otherwise contrive.  The wild maelstrom
of event `goto` hell is never far beneath the unrippled surface
of Arvo.


### Duct semantics: fool's mate

One very easy mistake to make is to confuse the call stack of
Arvo itself with the duct.  These are often logically aligned,
but by no means always so.  

Recall the vane arms:

    |%
    ++  call                                ::  accept request
      |=  [hen=duct hic=kiss]
      ^-  [p=(list move) q=_+>]  !!
    ::
    ++  take                                ::  handle response
      |=  [hen=duct way=wire sih=sign]
      ^-  [p=(list move) q=_+>]  !!
    --

Each of `++call` and `++take` has `hen=duct` in its sample.
Again, `hen` represents the *logical cause* of this event.

And the product of each contains a list of `++move`, which as we
recall is (fully expanded):

    $?  [p=duct q=[%give p=card]]
        [p=duct q=[%pass p=wire q=[p=role q=card]]]
    ==

The fool's mate of Arvo is the illusion that, for a call or take
with some `hen` in the sample, `p` in each product move must be
the same duct as that `hen`.  Obviously, if this were the case,
`(list mold)` rather than `(list move)` would be the product.

(Using procedural equivalence, we can see how this error aligns
with the Arvo call stack, or rather something like it.  Imagine
if a vane, instead of producing a move when it wanted to "call"
another vane, ie `%pass` to it, actually called it.  The call
stack (in this very impractical Arvo) would match the duct.)

But of course, it is quite common for `p`, the move duct, to 
equal `hen`, the event duct.  This gives us two categories of
`++move`, for every event: *cis* moves, for moves where `=(p
hen)`, and `*trans*` moves, for everything else.


### Duct semantics: cis and trans

The assembly example above contains only *cis* moves, which are
the typical pattern of a simple synchronous service.  In a single
turn of the Arvo main event loop, our HTTP request bounces from
`%eyre` to `%gall` to `%ford` and back again; the request
produces its own response.

In an asynchronous system like Urbit, synchronous *cis* services
remain the most common form of interaction.  But Urbit would be
super boring if it didn't come with a big helping of *trans*.

Suppose, for instance, we modify the above request pattern.
`%eyre` sends an application request to `%gall`, and `%gall` to
`%ford`, but `%ford` needs some global state not in its cache,
and has to consult `%clay`.  `%clay` recognizes the path as a
foreign one and asks `%ames` to send a network request on its
behalf.  When the state becomes available, the remote server uses
`%ames` to give a response message back to our `%clay` - which
gives a (confusingly named) `%writ` card back to `%ford`, etc.

The asynchrony, the *trans* event, in this service path belongs
to `%ames`, because `%ames` matches the outgoing message with 
the message response, and sends the response back on the incoming
duct.  This process is rather less obvious than it may seem.

To be exact: when `++call` asks `%ames` to send a message, the
only moves produced *in this event* are a list of packets heading
to the Ethernet port.  (Even then, we hardly would send all our
little packetses off at once.)

Rather, the actual, direct or *cis* cause of the `move` that
responds from `%ames` back to `%clay` is, of course, a packet
that `%ames` gets from the network.  How does it get back to
`%clay`?  

Obviously, the outgoing-message `++call` (caused by `%clay`) must
*save* its `hen` duct somewhere in the bowls of %ames, mapped to
a message GUID.  When an incoming-packet `++call`, some
milliseconds, seconds, or for that matter months later, produces
a message that decodes to a responding GUID, `%ames` will emit
moves that respond to the original `hen`.

If we labor much over this concept, which may be seem stone cold
obvious, note that *trans* moves are obviously the converse of
the common CSP concept of blocking requests.  (Exercise for the
undergraduate reader: what is the event converse of deadlock?)



### The case against CSP

Under various names, the war between events and CSP has been
going on at least as long as that between Chevy and Ford,
Makita and Husqvarna, or emacs and vi.

As in each of these cases, it's quite simple.  CSP sucks.  It
belongs in a museum of the 20th century, next to T-tops, East
Germany and Windows 3.1.

Evidence point: every time a platform exports native threads,
which actually have a scheduler like, you know, actual threads,
someone builds "green threads" on top of it - ie, nonpreemptive
threads.  One common hint that something you're doing sucks:
every time you do it, someone comes along and feels the need to
do it over in the opposite way.

Moreover, "nonpreemptive threads" are, with all due respect, a
contradiction in terms.  Sirs, you are already halfway to the
devil!  I know what's under your "nonpreemptive threads." It's an
event system, isn't it?  What else could it be?

If your underlying event system is of the `goto` school, sure,
it probably pays to keep it under a prophylactic layer.  It would
be possible to use the usual functional techniques to make it
look very much like our message event above "blocked," like a
green thread.  It would be not much more than a layer of
obscurity, though.  Unnecessary layers are considered harmful.

The case against CSP is that *the state of a CSP system is not
well-defined at any regular point in time*.  Eg, there is
(generally) no systematic or well-defined way of stopping a CSP
process and saving it to a file, even with some loss of work.
Not, in any CSP system, that it's impossible to implement a 
tool that does this - but that there is generally no
straightforward definition of intermediate state, and certainly
no mapping from any such abstraction to actual internal data.

This is where a true event system, not even pretending to be a
CSP machine, shines.  Not only is its state precisely defined
between events (and if you have to halt an event computer within
an event, you can just throw away the computation) - its *data is
not mixed up with code*.

Passive data - Hoon nouns without cores in them - is vastly
preferable in almost every context to active data.  For example,
it is always a mistake to send active data over the network -
where it is not actively impossible, it is actively idiotic.
Also, it's a relatively common operation to throw away all your
code, while keeping all your data.  The more the two are mixed
together, the more fun it is untwingling them apart.

Note that when `%ames` saves the duct it got from `++call`, 
*it is not saving code*.  It is not saving a callback, a closure,
a promise or even a continuation.  (Uncharitable mockers may
recall the long-running struggle of HN, written in the CSP Lisp
Arc, to manage the limited pool of continuations mapped to URLs,
resulting in the dreaded "expired link" error.  This is because a
continuation contains a stack and is more apt than not to be as
big as a bus.)


### The case against events

Obviously, asynchronous events are the future.  It's clear that
when the CSP dinosaurs are in the museum, it'll be the event
loops who *visit* that museum.  Nonetheless, a few architectural
quirks are worth noticing.

One of these is that *asynchronous systems have no flow control*,
and as a result *congestion control is an end-to-end problem*.

Obviously, flow control makes no sense if you can't block a
process.  Or rather, the only thing that makes sense is *logical*
flow control.  Logical flow control is, always and everywhere,
congestion control.

End-to-end congestion control is a somewhat harder problem than 
classic congestion control.  On the downside, E2ECC requires
actual, active compliance with CC logic not just in the stack,
but by the end application.  Also, heuristics are considerably 
more difficult, because the roundtrip time for a packet that
completes a message includes not just network latency, but also
computation latency for the whole transaction, from business
logic to disk - a metric with considerable variance.

One mitigating factor: E2ECC is not really a hard problem for
apps to solve.  The closer the programmer is to the endpoint, the
easier it is to behave in a sane way.  For example, if an app (a)
obeys a simple request-response structure in which only one
request is outstanding at a time, and (b) the size of the
requests is sane, the behavior of the app is sane.

The first law of E2ECC is: don't try to talk to people who aren't
talking back to you.  The second law of E2ECC is: if it is
absolutely necessary to keep buttonholing people who are not
responding, give a neighbor a break and at least practice
exponential backoff.  For instance, if my ship has been trying to
send a message to your ship for, say, a year, it is appropriate
for me to send you a packet again, say, every six hours.

As for the CC algorithms, we are still screwing around...

And of course, threads (real threads, anyway) excel when it's
time to go parallel.  There is an event converse of explicit
parallelism: implicit parallelism.  In which we cleverly figure
out that we can use physical parallelism to compute our event
function, which is serial, using these multiple cores.  This does
not seem incredibly hard - given the high stability of said event
function, arbitrary amounts of tedious hardcoding become possible.

## From Arvo to `%gall`, kernel to user.

The structured event logic which connects the vanes is in fact
repeated at two other layers.  At the bottom, the interface Arvo
presents to Unix (or whatever native OS) makes Arvo itself look
like one big vane.  

And on top, `%gall` treats the apps it runs like little vanes.
These analogies are highly imprecise and all details differ.
Even the arm names differ.

But a vane in this more abstract sense is any object that can (a)
accept requests (`++call`) and handle responses (`++take`),
generating a list of actions (`++move`) and a new state; (b)
expose a namespace (`++scry`); (c) reboot itself safely (`++load`
and `++stay`).  So long as you understand all these concepts,
you understand both kernel and user programming in Arvo.


## The goals of `%gall`.

Broadly speaking, `%gall` has two goals: (a) let `%gall`
apps consume and provide kernel vane services; (b) let `%gall`
apps define an event arena, similar to that of kernel Arvo, but
between apps and across the network.  

Thus in (a) we talk to an inner ring, and in (b) we stay in the
outer ring.  Applying our CSP metaphor, the good old "system
call" can be recognized in (a), though the system-call model does
not work so well if the kernel wants to call application
services; (b) is obviously some sort of RPC, or at least IPC.

(The main challenge in `%gall` is maintaining type integrity
without revalidation (except of course for network data), for
both (a) and (b).  Even with the tools Hoon provides, this is
difficult.  The best way to understand how it works is to read
the code, especially `++song`.)


## Back to our application.

We're now in a position to explain `++hide`, the standard state.

This is the `%gall` equivalent of Unix's "u area": the kernel state
specific to your application.  `%gall` resets the hide every time
it calls you.  You can change it or not; `%gall` does not notice.


    ++  hide                              ::  standard app state
      $:  $:  our=ship                    ::  owner/operator
              app=term                    ::  application name 
          ==                              ::
          sup=(map bone (pair ship path)) ::  subscription set
          pus=(jug path bone)             ::  noitpircsbus set
          $:  act=@ud                     ::  change number
              eny=@uvI                    ::  entropy
              lat=@da                     ::  date of last action
      ==  ==                              ::

Let's run quickly through the meaning of these legs:

`our.hid` is the host identity, ie, ~SHIP or `~tasfyn-partyv`.
`app.hid` is the formal name of the application, ie, APPL or
`%foobug`.  `our` and `app` never change.

`sup.hid` is a map from a communication channel, or `bone`, to a
user identity and a subscription path. `pus.hid` maps each path
back to a set of bones. This internal `%gall` state is also
useful to your app, as we'll see soon.  otherwise get sweaty
maintaining it.

`act.hid` is the change number - ie, the number of times the
application's state (ie, `vat`) has changed in any way.
`lat.hid` is the date of the last change.  `eny.hid` is at least
256 bits of entropy, not detectably reused after this
construction.

`vat` is your application state.  Your goal and ours is to
serve, preserve and improve it.  `foobug` has nothing in 
there right now, of course, but watch out!


## The application core.

Your job as a programmer is to (a) define the `vat`, ie, your
state, and fill in the various arms of the core.  

Because `%gall` uses dynamic compilation on your core, there is
no strict tile that it has to match.  In fact, %gall can even
decide how to call you based on what arms your core exports, a
very un-Hoon-like reflexive behavior.

Below you'll find definitions of each standard arm.  For your
convenience, they all have four letters and start with `p`.


## Application gates: `++peek`.

`++peek` is the one gate in Foobug so far.  Abstractly:

    ++  peek
      |=  [you=ship pax=path]
      ^-  [mark noun]  !!

## Example: `++peek`.

A `noun` in our abstract arm definitions doesn't mean we don't
use the type system - it means each app uses it differently.
For instance, as we wrote above:

    ++  peek
      |=  [you=ship pax=path]
      ^-  [mark manx]
      :-  %hymn 
      ;html
        ;title: Foobug!
        ;body
          ;p: Hello, world.
        ==
      ==

`++peek` is simply a view function.  `you` is the user doing
the viewing, and `pax` is a path which represents what she's
looking at. 

In the example so far, we ignore both `pax` and `you`.  Actually,
The uses you make of `you` and `pax` are entirely up to you.
Broadly speaking, `pax` represents a path, place, channel,
folder, or other subcontext within your application.

But what does `++peek` produce?  A... `[mark manx]`?  Alas, this 
requires another small digression.


## Content types and XML; ++mark and ++manx.

In `foobug`, the product of `++peek` is a cell `[mark manx]`:

    ++  mark  term                                    ::  content type
    ++  manx  $|(@tas [t=marx c=marl])                ::  XML node
    ++  mane  $|(@tas [@tas @tas])                    ::  XML name/space
    ++  manx  $|(@tas [t=marx c=marl])                ::  XML node
    ++  marl  (list manx)                             ::  XML node list
    ++  mars  ,[t=[n=%$ a=[i=[n=%$ v=tape] t=~]] c=~] ::  XML cdata
    ++  mart  (list ,[n=mane v=tape])                 ::  XML attributes
    ++  marx  $|(@tas [n=mane a=mart])                ::  XML tag

`++manx` is not of course a complete and perfect representation
of XML.  But in most cases, it will do.  Sorry, XML.

`++mark` is a *mark*, or simplified content type.  We ditch the
elaborate and bizarre hierarchical scheme of MIME, and stick with
a flat terminal that in general should equal the Unix file
extension.  `Marks` can also be called `protocols`, as they are 
formats for typed data being sent over the network.

Our marks here is `%hymn`, which is logically an HTML noun.
Right now the `%hymn` mark is just an arbitrary `manx`, though it
really should reflect the DTD to at least some level.

(It is important to recognize that although we can and do
construct both a rectifier and a type (the type being just the
range of the rectifier) for any working mark, the type is often
more general than the marks.  It is ideal for them to match
exactly.  But while every noun within the marks must be within
the type, not every noun within the type is within the marks -
just as not every valid `++manx` is a proper HTML5 noun.)

One useful service that the build vane, `%ford`, provides:
automatic rectification (conversion of untyped to typed nouns) of
marks, and automatic translation between them.

For instance, your browser showed you "Hello, world" as the
above.  This came across as an HTTP, ie MIME, response.  We had
to translate %hymn into (flat textual) %html, then %html to
%mime.  All of these are trivial translations, which makes their
implementations ideal for educational purposes:

    ~/=main=/sys/hymn/ref/gate/hoon
    ~/=main=/sys/hymn/tan/html/gate/hoon
    ~/=main=/sys/html/ref/gate/hoon
    ~/=main=/sys/html/tan/mime/gate/hoon
    ~/=main=/sys/mime/ref/gate/hoon

As for the Hoon syntax used to build HTML (or any XML, although
Hoon interpolation is disabled for the special cases of
`<script>` and `<style>`), see the `++sail` documentation.

(Other, inferior languages lack built-in XML template syntax and
are forced to put XML assembly in separate files compiled by
template DSLs.  Somehow no one has noticed that this is lame.)

Actually XML is just a tree and plain Hoon, without `++sail`, 
is pretty good at consing up trees.  But XML also does weird tree
promotion things with flows.  So it's even nicer to compromise
with reality and include special-purpose syntax.


## Application gates: `++poke`.

`foobug` uses this simple generic `++poke`:

    ++  poke
      |=  [ost=bone you=ship som=noun]
      ^-  [(list move) _+>]  !!

The simple `++poke` arm is reserved for the native marks your app
wants to hear its messages in.  But in fact, none of these `%gall` 
features are specific to `%eyre` or HTTP; anything `%eyre` can
do, `%ames` can do.  And generally better.

Messages always come with a marks, and the first arm name we try
is `++poke` plus that mark.  For instance, messages from `%eyre` 
usually come as JSON:  

    ++  poke-json
      |=  [ost=bone you=ship jon=json]
      ^-  [(list move) _+>]  !!

If `foobug` exported this arm, `%gall` would have used it.  Since
it doesn't, we fall back to the generic `++poke`.

If you've read the Arvo doc, the product of `++poke` is a typical
Arvo event product.  It is not a list of kernel moves and a vane
core; it's a list of user moves and an app core.  Different
layer, same difference.


## Example: `++poke`.

With `++poke`, we can build a working stateful app:

    !:
    :::::::::   Foobug: a simple application.
    ::
    |_  [hid=hide vat=[%0 p=@ud]]
    ++  poke
      |=  [ost=bone *]
      :_  +>(p.vat +(p.vat))
      :~  [ost %give %nice ~]
      ==
    ::
    ++  peek
      |=  [you=ship pax=path]
      :-  %hymn
      ^-  manx
      ;html
        ;head
          ;title: Foobug!
        ==
        ;body
          ;p: Dude, the answer is {<p.vat>}.
          ;button(onclick "bump()"): (bump.)
          ;script
            ; var mess = 0;
            ;
            ; function bump() {
            ;   xhr = new XMLHttpRequest();
            ;   xhr.onload = function() { mess++; } 
            ;   xhr.open("PUT", "/tim/"+user+"/"+appl+"/"+port+"/"+mess);
            ;   xhr.setRequestHeader("content-type", "text/json");
            ;   xhr.send(JSON.stringify({oryx: oryx, xyro: {}}));
            ; }
          ==
        ==
      ==
    --

Open this same page in two tabs.  Hit the button.  Is it magic?
No!  It's technology!

What we did here: we added some vanilla JS that sends a JSON
message from client to server.  This message, generated by code
generated in `++peek`, winds up hitting us back in `++poke`.

We don't even bother modeling the app's moves in this sample,
because this silly little `foobug` makes only one type of move.
In more complex apps, we'll do it the vane way.

Here, our app has received a *message* and is charged with
producing a *response*, either positive or negative.  Success
is indicated by giving a `%nice` gift, as in this example.
Failure is indicated by giving a `%mean` gift with a unit of a
pair of a term and a list of tanks.  If there is a term and
list of tanks, the term respresents an error code and the list
of tanks is a human-readable explanation for the error.  If
not, then we simply failed without explanation.  Over HTTP, a
`%nice` is a `200` status code with JSON "ok" set to true.  A
`%mean` is a `200` status code with JSON "ok" set to false.  If
there is an error term and message, then we pass along the error
term labeled "err" and the message labeled "res".

Note the HTTP API by which the client script communicates with
the app.  To send a message back to the app, we need four
variables, three of which `%eyre` thoughtfully injects into your
application page.  The injected variables are `user`, `appl`, 
`port`, and the mysterious `oryx`.

`user` is the authenticated user id.  (As in, the request will
fail if not authenticated as this identity.)  `appl` is our
application, ie `foobug`.  `oryx` is a CSRF token, which must
be wrapped around all JSON submissions using the pattern above:
`{oryx: oryx, xyro: message}`.  SAVE THE ORYX!  SERVE THE XYRO!
Sorry.

`port` is an opaque number which represents the *server instance*
of this application - essentially, the page load instance.  Each
time we ship an application main page, we inject a different
`port`.  All service requests within this logical instance need
to find it by including the `port` in their URL.

Note also that the request uses PUT to send.  For this to be
properly restful, we need a `mess` counter that tracks the
message number.  We are idempotently stating instances in a
sequence, not posting events to a queue.  We can and do retry our
PUTs, and a retried PUT returns the original response.

And why did the number in the browser change when we pressed the
button?  Seems there's some devilish scriptage appearing
automatically in our simple little htmls.  From the injection
point in `%eyre`:

      ;script
        ; var tries = 0;
        ; var cnt = 0;
        ; var next = "/gie/"+user+"/"+appl+"/"+port+"/self/"+(cnt + 1);
        ; call = function() {
        ;   xhr = new XMLHttpRequest();
        ;   xhr.open('GET', next, true);
        ;   xhr.addEventListener('load', function() {
        ;     if ( this.status >= 500 ) {
        ;       return delay();
        ;     }
        ;     cnt++;
        ;     if ( this.status >= 400 ) {
        ;       document.alert("neighbor, please.");
        ;     }
        ;     document.location.reload();
        ;   });
        ;   xhr.addEventListener('error', delay);
        ;   xhr.addEventListener('abort', delay);
        ;   xhr.send();
        ; }
        ; keep = function() {
        ;   setTimeout(call,1000*tries);
        ;   tries++;
        ; }
        ; call();
      ==

[As Javascript - this is a mess.  Someone help unfsck it please!]

In short, our application is listening to changes in itself.
This is the same script that made it originally reload `Hello,
world` as `Goodbye, world`, of course.  But that time, we
actually changed the application source code.  

This time, we've only changed some data.  But still, something
regenerated the page.  Huh?  Why?


## The ugly truth.

The ugly truth is that `++peek` is really a fake feature - only
for beginners and demos.  Sometimes useful in development, which
is why we have it in there.  Or it's our excuse, anyway.

But `++peek`, which looks beautiful, is actually ugly.  To be
exact, it is doing things automagically which don't scale.
Serious applications in production cannot and should not use
`++peek` - they should use its serious cousin, `++peer`.

The reason `++peek` works in this example is that we implicitly
subscribed to the `%self` stream when first we peeked.  Then,
when our `++poke` changed the state of `foobug`, the magic behind
`++peek` did what it does: recompute all subscriptions to
`foobug`, and send full updates if the new value is different.
The HTML interpolation in `++peek` indeed depends on `p.vat`.

For many apps, obviously including little `foobug`, this is fine.
For many other purposes it's hilariously lame.

And wait.  What are these subscription things?


## Subscriptions considered awesome.

Actually, all requests to a `%gall` app are subscriptions.  The
only way to perform a one-time request is to subscribe, then
immediately unsubscribe.

The subscription model is simple: the client is subscribed to an
abstract channel within your app, defined as a `++path`.  The
empty path, `/` or `~`, is the application front page as
originally requested.

Each subscription you create is a `stream`, with a numeric
stream identifier set by the client.   Streams logically produce
a sequence of updates numbered in order.  Streams of the same
name and instance cannot be reused.  Don't use streams 0 or 1;
0 is for the front page, 1 is the message stream.

Subscribers (besides being terminated) can receive two kinds of
updates - complete refreshes (`%rust`) and differential updates 
(%rush).  Over HTTP, the only difference is the status code - 
`200` for `%rust`, `203` for `%rush`.  

It is entirely the server's decision whether to send `%rust` or
`%rush`.  For instance, the client should not notice when the
server reboots, except that it gets an extra `%rust`.

As for the `++mark` of each, this is for server and client to 
coordinate.  Obviously, just as there is no universal content
type, there is no universal difference type.  When in doubt,
use `%json`.

If there is an error on a subscription, then a `%mean` may be
sent.  Once an error is sent on a subscription, there will be
no more messages on that subscription.  In general, the error
should be displayed to the user, unless the application
recognizes the error term and knows how to handle it.  See
documentation for ++poke for the HTTP representation.

## Application gates: `++peer`.

    ++  peer
      |=  [ost=bone you=ship way=path]
      ^-  [(list move) _+>]
      !!

`++peer` is like `++peek` for grownups.  If `++peer` is defined
on your core, `++peek` is ignored.

After a successful `++peer`, it's the responsibility of the
*application* to send `%rust` and `%rush` moves, on `ost` the
subscription duct, needed, until the subscription is pulled.

Bear in mind that a bone can only do one thing, once - you will
not receive a `++peer` on an already subscribed bone.


## Application gates: `++pull`.

    ++  pull 
      |=  ost=bone
      ^-  [(list move) _+>]
      !!

`++pull` revokes a subscription.  `ost` will not be reused.
`%gall` will of course reset `sup.hid` as needed, so you should
only declare `++pull` if you have additional application state
for a subscription.


## Application gates: `++park`, `++prep`.

At present, we are using the default `%gall` handling of a
reboot, which is simply to replace `vat` in the new core, the entire
user state, with its value from the old core.  Don't try to
change the type if you do this, obviously.  (We already got a
pass on changing it from `~`.)

But if we define the `++park` and `++prep` arms, we can manage
this behavior manually.  Let's add a clam for our state to the 
model section:

    =>  |%                                ::    models
    ++  axle  $%  [%0 p=@ud]              ::  all state
              ==                          ::
    ++  move  (pair bone ,[%give gift])   ::  action
    ++  gift                              ::  result
              $%  [%nice ~]               ::
              ==                          ::
    --                                    ::
    |_  [hid=hide vat=axle]

and put in `++park` and `++prep` arms:

    ++  park
      ^-  axle
      !!
    ::
    ++  prep
      |=  eld=(unit axle)
      ^-  [(list move) _+>]  !!
     
These should be fairly self-explanatory.  Obviously, our present
`++axle` is version `%0` of `foobug`, and its only state is an
unsigned integer.  When `foobug` eventually morphs into, say,
Emacs, its version number may be in four digits, but it should
still know how to upgrade itself from a version `%0`, 2014,
infant `foobug`. 

Of course, if `++prep` gets a null `eld`, the application is
being created for the first time.  That happens too.

Note also that we manually generate fake unsubscription events
before a reboot, leaving your core logically without subscribers
before it shuts down and similarly naked when it starts up.  The
subscriptions are then reapplied, so that the actual remote
subscribers notice no difference (except for maybe an excess
`%rust` or two).  The result is that `++park` does not need
to manage subscription state across reboots - and should not try.

##  Application gates: `++pour`.

`%gall` apps can use exactly the same `%pass` moves as any vane.
To handle responses from these requests, export `++pour`:

    ++  pour
      |=  [way=path sih=sign]
      ^-  [(list move) _+>]  !!

This works exactly like the vane-level `++take`.


# HTTP API

That was `%gall` seen from the Hoon side.  But what does it look
like from the Web side?

##  HTTP GET: G `gog`/`gig`

Let's go over the request pattern from start to finish.  First, 
your original request

    https://SHIP.urbit.org/foobug

is an informal URL, meant for users to look at - so the first
thing we have to do is to translate it into a formal URL, meant
for Urbit to parse unambiguously.  The proper URL here is:

    https://SHIP.urbit.org/gog/foobug

Gog?  And Magog?  No, `gog` is a three-letter string redundant
with request metadata.  The first 'g' means a (G)ET request, the
'o' means the client must be authenticated as the (o)wner of
SHIP, and the final 'g' means we are (g)oing to the front page of
an app - in this case, `foobug`.

To send a formal request as another user:

    https://SHIP.urbit.org/gig/USER/foobug

Note, `gig` not `gog`.  If you are already authenticated as USER,
`https://SHIP.urbit.org/foobug` will figure this out.  But when
we send automated requests, we don't like much "figuring out" to
be in our request chain.

There are a number of ways to construct the domain and port of
Urbit URLs - depending on the way the ship uses DNS.  If we have
a `SHIP.urbit.org` domain, we can obviously parse the owner from
the name.  If we don't, we need an extra SHIP in the path:

    https://SHIP.urbit.org/SHIP/gig/USER/foobug

The best way to construct URLs from within a single-page app is
to let the DNS take care of itself, and use relative URLs like

    /SHIP/gig/USER/foobug

There should be helper functions to help you do this.  But, there
aren't.  So for now you roll your own.

The response to a successful `gog` or `gig` request is intended
as the main page of a stylish modern single-page web application.
This is of course the page you generate in `++peek`, but this
being the modern web, we have to inject some stuff into it.

To be precise, we inject JS variables at the start of `<head>`
and a script at the end of `<body>`.  The variables are named
`appl`, `ship`, `page`, `oryx`, `user`, and `auto`.  The script is
an autoloader gated by `auto`.

`app` is APPL.  It's always set.

`ship` is SHIP.  It's always set.

`user` is the string USER.  If `!=(user ship)`, construct
requests with the `o` infix and no USER:

    https://SHIP.urbit.org/gog/APPL
    https://SHIP.urbit.org/pom/APPL/5/30

Otherwise, use the `i` infix and a USER:

    https://SHIP.urbit.org/gig/USER/foobug
    https://SHIP.urbit.org/pim/USER/foobug/5/30

`oryx` is an opaque string %gall, which serves as a CSRF token.
All PUT requests must be of the form `{oryx=string xyro=message}`.

`auto`, which defaults to `auto=true`, turns the built-in
autoreload (which listens for the main page of the app itself) on
and off.  You may want to write your own better one.

`port` (not to be confused with an IP port) is a string, a
decimal number in Earth format (no dots) which opaquely
identifies the server-side client instance (within `%eyre`,
Urbit's web server).  The `gog` or `gig` request creates this
instance; all requests driven from that instance will include
`port` in the URL.

(GET requests are not supposed to have any side effects on
the server, a prohibition which if actually enforced would
prevent the entire Web from, for instance, logging its pageviews.
Like everyone else we flirt with this restriction, violating it
only in ways which do not look like state changes to the client.
Ports are also recycled, for instance, when `%eyre` decides for
arbitrary reasons that they're no longer being used.)

All requests other than `g` for go are logically within a port.
Let's look at what we can do within a port - effectively, within
an instance of a single-page application:


## HTTP PUT: S `tos`/`tis` (subscribe to path)

The PUT request

    https://SHIP.urbit.org/tos/APPL/PORT/STREAM/PATH

initiates a subscription.  With `goe`/`gie` requests on the same
stream.  (see below), the client application can pull (by long
polling only, for now) a sequence of responses from the server.
However, the response to this PUT itself is the first response in
the stream.

The stream number is any number greater than 1 that you haven't
used before with this port.

Logically, the content is identified by the trailing path, and
updated by `%rust` or `%rush` responses - distinguished by 200 or
203 codes respectively, and defined as complete versus
differential updates.

In practice, no one transporting a stream knows or cares what
the path or responses means.  You can use it for any nonsense
and probably will.  We suggest using it logically, however.

The only content in this PUT is a JSON object (text/json), 
the CSRF token {oryx: value}.  You had better get this right.
Just resend the `oryx` variable, which is right there above you.

The subscription is canceled by a `tou`/`tiu` (see below).


## HTTP GET: E `goe`/`gie` (pull stream update)

The GET request

    https://SHIP.urbit.org/goe/APPL/PORT/STREAM/SEQUENCE

pulls response SEQUENCE (an Earth decimal) from subscription
STREAM (a symbol name) in PORT and APPL.

If you have a specific content type you want, use a matching
suffix, and the server will try to translate:

    https://SHIP.urbit.org/goe/APPL/PORT/STREAM/SEQUENCE.json

The stream `self` is used to pick up responses (other than the
first) to `gog` requests, ie, updates of the main request.  
You'll see it used in the autorequest script.  Also, don't name
your stream `mess`.

The server wants you to pull requests in order and does not like
pipelining.  Once it hears a request for SEQUENCE, it assumes it
can delete (SEQUENCE - 1).  Screwups here will generate a 204 -
see below.


## HTTP PUT: M `tom`/`tim` (send message).

The PUT request

    https://SHIP.urbit.org/tom/APPL/PORT/SEQUENCE

sends client-to-server message SEQUENCE (an Earth decimal), and
receives the corresponding response (always json).  

The post body should be text/json.  It should be of the form
{oryx: string, xyro: json}.

The result of this request will be its response.  If the request
breaks for some non-fatal reason, it should be retried - the
sequence number will prevent actual re-application, and the
duplicate will simply be a re-request of that result.  All our
PUT requests are idempotent if repeated strictly.

The result (if successful) is always `200` and usually `json`.
Set a content-type suffix to engage the usual translator.

As in G requests, picking up response N will result in the
deletion of response N-1.  Keep it simple, please.


## HTTP PUT: U `tou`/`tiu` (unsubscribe).

The PUT request

    https://SHIP.urbit.org/tou/APPL/PORT/STREAM

unsubscribes from STREAM (see above), if not already
unsubscribed.  The response is always a 204.


## HTTP error codes: 200, 203, 204, 4**, 5**.

As mentioned previously, on an E request, `200` means `%rust`, a
complete reflex, and `203` means `%rush`, a differential update.
Elsewhere, only `200` is success.

A 204 error (no content) on any request except M or U means
client and server are out of sync somehow, and the application
page should be reloaded.  On M, 204 means the server accepts
but does not choose to respond to the message.

A 400 class error on a G request should render the error page,
probably not without a reload driver.  The error page remains a
subscriber and will update itself if a new result is available.

A 400 class error on an E, S, or U request means the subscription
has been cancelled (typically with prejudice) by the server.  
Render the error page.

A 400 class error on an M request means the message has been
rejected.  (Its sequence number, however, is not reclaimed.)
The application must decide whether or not to show the error.

A 500 class error should be silently retried with exponentially
increasing delay.  504s, for instance, are common in long
polling.

