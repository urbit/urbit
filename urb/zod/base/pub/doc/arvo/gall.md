<div class="short">

`%gall`
=======

Our application server manager.

It allows applications and vanes to send messages to applications and
subscribe to data streams. This requires `%gall` to be a sort of a
hypervisor. Messages coming into `%gall` are routed to the intended
application, and the response comes back along the same route. If the
intended target is on another ship, `%gall` will behind-the-scenes route
it through ames to the other ship to run. This provides an abstraction
where all apps on all ships are communicated with over the same
interface.

`%gall` neither accepts events from unix nor produces effects. It exists
entirely for the benefit of other vanes and, in particular,
applications. Eyre exposes `%gall`'s interface over http, and ames does
the same over the ames network. `%gall` uses ford to compile and run the
applications.

</div>

------------------------------------------------------------------------

Cards
=====

`%gall` accepts the following cards. The first three are the most
commonly used, while the others are primarily used internally.

[`%mess`](#mess)
================

Sends a message to an app. This will result in a call to the app's
`++poke` arm. The response is exactly one of a `%nice` if the action
succeeded or a `%mean` if not.

------------------------------------------------------------------------

[`%show`](#show)
================

Subscribes to a stream from an app. This will result in a call to the
app's either `++peek` or `++peer` arm. The first response will always be
either a `%nice` or a `%mean`, indicating whether or not the
subscription was successful. After the first response, there will be
zero or more responses of either `%rush` or `%rust`, which communicate
either a differntial or full update to the data stream. There may be a
`%mean`, which indicates that the subscription has been canceled and no
more responses will be received along this stream.

------------------------------------------------------------------------

[`%nuke`](#nuke)
================

Unsubscribes the current duct from its stream. This receives a response
of either a `%nice` or a `%mean`. Note that a response of `%nice` does
not imply that the current duct was in fact subscribed to any stream.

------------------------------------------------------------------------

[`%init`](#init)
================

Initializes a ship's apps. This should be called exactly once for each
ship on the pier. This produces no moves in response.

------------------------------------------------------------------------

[`%sire`](#sire)
================

Instantiates a child app. The app will be at path `[p parent-path]`, and
it will be an instance of the `q` app. The only response will be `%gone`
when the child dies.

------------------------------------------------------------------------

[`%rote`](#rote)
================

Signifies a remote request from ames. `r` should be of type `rook`. This
how an app on a foreign ship may send a `%mess`, `%show`, or `%nuke`
card. Note that `%gall` automatically converts `%mess`, `%show`, and
`%nuke` into ames messages behind the scenes, so the only entity that
should use `%rote` and `%roth` is ames. Formally, the response is either
a `%nice` or a `%mean`, which ames uses to give a positive or negative
ack. A logical response comes by passing a `%roth` card.

------------------------------------------------------------------------

[`%roth`](#roth)
================

Gives the response received from a remote request. `r` should be of type
`roon`. This is how an app responds to a foreign request with a `%rush`,
`%rust`, `%nice`, or `%mean`. The response is either a `%nice` or a
`%mean`. Even though we, as the proverb goes, "never ack an ack", we do
need to acknowledge these responses since they're really independent
one-way messages.

------------------------------------------------------------------------

[`%wipe`](#wipe)
================

Wipes the given app from memory. This is generally considered a hack,
but it is sometimes useful during development to wipe the state of an
app. We don't guarantee that this actually completely wipes the app.
Generally, you want to use a `%cide` card if you actually want to kill
an app. This gives no response.

------------------------------------------------------------------------

[`%cide`](#cide)
================

Kills an app and all its children. Even though it's not technically a
part of `%gall`'s interface since it's not in `++kiss` and can't be
called from the outside, it's worth mentioning `%cide`, which may be
called from within `%gall` apps. It should call `++part` to allow the
app any last words. This gives no response.

------------------------------------------------------------------------

Service Gates
=============

[`++poke`]()
============

Handles incoming messages. Most commonly with an associated `%logo`. For
example `++poke-json` handles an incoming JSON request from `%eyre`.

[`++peer`]()
============

Handles incoming subscriptions.

[`++pull`]()
============

Handles dropping subscribers.

[`++pour`]()
============

Handles responses to `%pass` moves.

[`++park`]()
============

Save state on update.

[`++prep`]()
============

Load state on update.
