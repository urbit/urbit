# Tutorial 3: Messenger
The last major aspect of shrubbery that we need to cover is the dependency system. This consists of the `+deps` arm and the `%rely` poke.

By the end of this tutorial you’ll understand how dependencies work and how to use them. You should also start to see how you can design functionality that involves multiple shrubs interacting across multiple ships.

We’ll take a look at Messenger. Messenger is one shrub, located at `/imp/messenger.hoon`, but it relies on five other shrubs to work.
- `/imp/messenger` is the high-level interface and “service provider” that enables the user to create groupchats and 1-to-1 DMs, and invite other ships to them.
- `/imp/message` is a `~` stub that allows us to create messages in the namespace
- `/imp/message-pub` is a shrub that takes a poke to store messages in the namespace as its kids.
- `/imp/message-sub` mirrors messages from a `/imp/message-pub` shrub into its own state.
- `/imp/groupchat` creates a publisher/subscriber pair, and can invite other ships to post/subscribe to the `/imp/message-pub` shrub.
- `/imp/dm` negotiates a two-way pub/sub relationship and mirrors state between both parties.

One motivation behind this design is to split off functionality into simple, reusable shrubs. `/imp/dm` needn’t just be the DM shrub for `/messenger`, it could also be used off-the-shelf for 1-to-1 chats in shrubs like `/chess`, `/twitter`, or `/ebay`.

We'll skip covering the Messenger frontend. While there are new ideas here, it's very similar to the Tasks tutorial which is the better context for more frontend. This tutorial will focus on several shrubs interoperating to form one tool. Let's look at `/imp/message-sub`, then the whole system that Messenger uses to manage chats.

## /imp/message-sub
```hoon
/@  message  ::  [from=ship now=time contents=@t]
^-  kook:neo
|%
++  state  pro/%sig
++  poke  ~
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%da |]
      [pro/%message ~]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~  :-  %pub
      :+  req=&  [pro/%sig (sy %sig ~)]
      :+  ~  %y
      %-  ~(gas by *lads:neo)
      :~  :-  [|/%da |]
          [pro/%message ~]
      ==
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo state=pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    [~ sig/!>(~)]
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(%rely stud)
    :_  state
    =+  !<([=term =stem:neo] vax)
    ?>  ?=(%y -.q.stem)
    ::  only get new kids
    =/  kids
      %+  skim
        ~(val by kids.q.stem)
      |=  [=ever:neo =mode:neo =pail:neo]
      =(%add mode)
    ?:  =(~ kids)
      ~
    =/  pai=pail:neo  pail:(snag 0 kids)
    =/  mes  !<(message q.pai)
    :~  :-  (welp here.bowl ~[da/now.mes])
        [%make [%message `pai ~]]
    ==
  --
--
```

## The +deps arm
The only part of this system that needs to define its dependencies is `/imp/message-sub`.

```hoon
::
::  define dependencies
++  deps
  ::  deps:neo is (map term fief:neo)
  ::  a map of a tag to a fief:neo, one for
  ::  each dependency we want to type here
  %-  ~(gas by *deps:neo)
  :~  ::
      ::  %pub is an arbitrary tag which refers to
      ::  this dependency within this +deps arm
      :*  %pub
          ::  fief:neo is [required=? =quay]
          req=&
          ::  quay:neo is (pair lash:neo (unit port:neo))
          ::  lash:neo defines the shrub's state and pokes
          [[%pro %sig] (sy %sig ~)]
          %-  some
          ::  port:neo constrains the shrub's kids
          :-  %y
          %-  ~(gas by *lads:neo)
          :~  :-  [[%.n %da] |]
              [[%pro %message] ~]
          ==
      ==
  ==
```

With regards to the lifecycle of the shrub, the `+deps` arm types the shrubs whose names and locations are passed in as an argument in the `%make` card when this shrub is created.

```hoon
[%make %message-sub ~ (malt ~[[%pub (snoc host.poke %pub)]])]
```

In this card, `+malt` creates a `(map term pith:neo)` where the `term` is `%pub` and the path represented by the `pith` is `/path/to/pub`. At `/path/to/pub`, there’s a shrub which is the canonical “publisher” or “host” of a chat, whether that’s a group chat or a DM. Whenever there’s a state change in the publisher shrub, the `/imp/message-sub` created in this card will be notified about it, but we’ll cover that in more detail in the next section.

The `%pub` term will act as the key in the map of dependencies, corresponding to the`%pub` key that `/imp/message-sub` used as a tag in its `+deps` arm. This term allows us to differentiate between shrubs being given to us in the `%make` card’s `conf:neo`.

The `required` flag specifies that when this shrub is made, it must be passed in a `conf:neo` that contains paths to existing shrubs. If we don't pass in a conf, or if no publisher exists at `/path/to/pub`, the `/imp/message-sub` we’re trying to `%make` here will fail to build. It can only exist with reference to the publisher shrub.

The last “new” idea here is `%sig`. This is a special case of `stud:neo` which tells `/app/neo` that the shrub has no state. We can do the same thing for pokes, as above with `(sy %sig ~)`.

If you look at `/imp/sig.hoon`, it’s just a `~` stub like the `%txt` implementation. `%sig` imps are not special and are not treated differently to any other stub, they’re just a stylistic convention to say that we don’t care about the state of the shrub in question; it could be anything, we won’t constrain it at all. There’s also a `/pro/sig.hoon` which lets us do the same thing for pokes.

## Handling state changes in our dependencies
Unlike a Gall agent, a shrub does not send out `%facts` to subscribers in the event of changes to its state, all of which has to be manually implemented by that agent’s developer. Instead, when its state changes `/app/neo` automatically sends a `%rely` poke to shrubs that have declared the shrub as a dependency. The developer of the listener shrub is the only one who has to write the logic to handle this.

In its `+deps` arm, the `/imp/message-sub` shrub declares the type and behaviour of the shrubs it will accept as dependencies. Shrubs that conform to that type, like `/imp/message-pub`, can be passed in through the `%make` card via the `conf` and `/imp/message-sub` will listen to those shrubs for state changes.

```hoon
++  deps
  %-  ~(gas by *deps:neo)
  :~  :*  %pub
          req=&
          [[%pro %sig] (sy %sig ~)]
          %-  some
          :-  %y
          %-  ~(gas by *lads:neo)
          :~  :-  [[%.n %da] %.n]
              [[%pro %message] ~]
          ==
      ==
  ==
```

Like we saw in the `+kids` arm in the previous tutorial, the `+deps` arm specifies this `%y` constant. This is a `care:neo`.

In the `+kids` arm the `%y` care declares that this shrub is constraining its immediate children, and the `%z` care that it’s recursively constraining all of its descendants in the namespace.

In `+deps`, the `%y` care declares that we’re only listening for state changes in the dependency shrub’s immediate children. If it were `%z`, we’d be subscribing to state changes for all of the dependency shrub’s descendants.

The other `care:neo` you’ll commonly see is `%x`, which refers to a single shrub. You wouldn’t use this in `+kids`, but you might use it in `+deps`.

### Handling %rely pokes
Let’s see how `/imp/message-sub` handles the `%rely` pokes it recevives from dependencies.

```hoon
++  poke
  ::
  ::  we receive a stud and vase from the publisher shrub when
  ::  there's a state change we've declared we care about
  |=  [=stud:neo vax=vase]
  ^-  (quip card:neo pail:neo)
  ::  we only handle one poke, %rely
  ?>  =(%rely stud)
  ::  we don't change our own state
  :_  state
  ::  the vase we receive from the publisher is a (pair term stem:neo)
  =+  !<([=term =stem:neo] vax)
  ::
  ::  assert the care in the stem is %y, so the publisher
  ::  is telling us about itself or one of its kids
  ?>  ?=(%y -.q.stem)
  =/  kids
    ::  get newly-created kids from the stem we received
    %+  skim
      ::  kids.q.stem is a (map pith [=ever:neo =mode:neo =pail:neo])
      ~(val by kids.q.stem)
    ::
    ::  ever:neo is the publisher shrub's version numbers
    ::  mode:neo is ?(%add %dif %del)
    ::  pail:neo is a (pair stud vase)
    |=  [=ever:neo =mode:neo =pail:neo]
    ::  return every kid whose mode is %add, meaning it's just been created
    =(%add mode)
  ?:  =(~ kids)
    ::  if there are no new kids, do nothing
    ~
  ::  if there are new kids, assume there's only one new kid and
  ::  get the pail of the first new kid in the list of kids we created
  =/  pai=pail:neo
    pail:(snag 0 kids)
  ::  get the message from the pail of the
  ::  new kid the publisher has told us about
  =/  mes
    !<(message q.pai)
  ::  make a new kid of our own that contains the new message from
  ::  the publisher, keeping our /message-sub namespace in
  ::  sync with the publisher's namespace
  :~  :-  (welp here.bowl ~[da/now.mes])
      [%make [%message `pai ~]]
  ==
```

The above is mostly self-explanatory, but it’s worth expanding on `stem:neo` and `mode:neo`.

```hoon
+$  stem
  $~  [*ever %x %stud *vase]
  %+  pair  ever
  $%  [%x =pail]
      [%y =pail kids=(map pith [=ever =mode =pail])]
      [%z =pail kids=(map pith [=ever =mode =pail])]
  ==
```

The `stem` is head-tagged with a `care:neo`. If the shrub’s dependent is listening with a `%z` care, it can still distinguish between `stem`s relating to the dependency, its kids, or its other descendants.

All stems come with an `ever`, which contains version numbers for the dependency and its descendants. `%x` `stem`s come with a `pail` with the new state of the dependency shrub. The `%y` and `%z` `stem`s come with a `pail` for the dependency shrub, and a map containing all the relevant descendants of that dependency shrub. The `pith` keys in that map give us the locations of the descendants, and the `[=ever =mode =pail]` values give us their own version numbers, their `mode`, and their current state.

The `mode` of these kids is either `%add`, `%dif`, or `%del`. If it’s `%add`, the dependency is telling us it’s a new kid. If `%dif`, the kid isn’t new but its state has changed. If `%del`, it’s telling us the kid was deleted and giving us its final state.

## Messenger: Overview
There are several shrubs working in tandem here to provide groupchat and DM functionality. Now that we know how `/imp/message-sub` works, let's look at the overall structure.

### DMs in shrubbery
If you wanted to implement 1-on-1 DMs in your shrub, you could just include `/imp/dm` in your `+deps` arm and send `%make` and `%poke` cards to it. If that doesn't do what you need, you could base your own DM functionality on this.

`/imp/dm` is short and simple enough that it might be nice to show an equivalent `/app/dm` Gall agent, but there isn't one. `/imp/dm` delegates a lot of functionality to service providers in a way that Gall apps theoretically could, but in practice never do. With the constraints they can set in the `+kids` and `+deps` arms, shrubs can make guarantees about the functionality of third-party service providers in other shrubs and/or other desks. Those service providers can make breaking changes to that functionality, but as long as they have some way of converting data to the type that their parents or dependents accept, those shrubs will still be able to interoperate.

#### kook:neo

```hoon
::  /imp/dm.hoon
++  state  [%pro %ship]
++  poke  (sy %dm-diff ~)
++  kids
  %-  some
  :-  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [[%.n %theirs] %.n]
      [[%pro %message-pub] (sy %sig ~)]
      :-  [[%.n %mine] %.n]
      [[%pro %message-sub] (sy %sig ~)]
  ==
++  deps  *deps:neo
++  form
  ...
```

A DM shrub only stores one ship, the `@p` of whoever you're chatting with. It only has two kids: `/path/to/this/dm/theirs` and `/path/to/this/dm/mine`. It chooses the service providers at `/imp/message-pub` and `/imp/message-sub` to store the state of `/theirs` and `/mine`, while setting no constraints on the types of poke those service providers can take. In other words, it delegates the functionality to two new instances of `/imp/message-pub` and `/imp/message-sub` rather than subscribing to existing Gall agents that may or may not exist.

```hoon
$%  [%initiate partner=ship provider=pith]
    [%invited partner=ship dm=pith]
  ::
    [%acked dm=pith]
  ::
    [%post text=@t]
==
```

`/imp/dm` takes four pokes: `%initiate`, `%invited`, `%acked`, and `%post`.
- `%initiate`: we initiate a DM, specifying the other ship and the service provider we want to use.
- `%invited`: someone invites us to a DM, specifying their `@p` and the `pith` for us to send DMs to them.
- `%acked`: acknowledge creation of a DM.
- `%post`: send a DM.

#### curb:neo
Cells like `[%pro %ship]` and `[%pro %message-pub]` are examples of `$curb:neo`. This is a powerful type that's beyond the remit of these tutorials, but it's worth clarifying what these cells mean.

```hoon
+$  curb
  $~  [%pro %$]
  $%  [%or p=(list curb)]
      [%only p=stud]
      [%rol p=stud q=curb]
      [%not p=curb q=curb]
      [%pro p=stud]
      [%any ~]
  ==
```

In all of the shrubs we've looked at in these tutorials we could replace every `%pro` curb with the likes of `[%only %ship]` and `[%only %message-pub]` and lose none of the functionality we've looked at. The `[%only %ship]` curb just declares that the state is exclusively a `ship`. However, the `[%pro ship]` curb says that the state can be any type *which can be converted to a `ship` through an available `/con` file*. This has implications for interoperability and state transitions we have not yet fully explored.

#### form:neo
When `/imp/dm` is first created with a `%make` card, it needs to be created with some pre-defined state. The intial state it accepts has to be a `%dm-diff`. This is a little unusual, but it's essentially the same as a Gall agent sending a poke to itself `+on-init`, except the poke is defined by the external service provider. Since the service provider could be on our ship or someone else's, the `+init` arm takes both `%initiate` and `%invited` pokes. The `+init` arm is the only context in which these pokes are handles.

```hoon
++  init
  |=  old=(unit pail:neo)
  ^-  (quip card:neo pail:neo)
  ?~  old  !!
  ?>  =(%dm-diff p.u.old)
  =/  poke  !<(dm-diff q.u.old)
  ?+    -.poke  !!
      :: create me with a pith to a service provider
      :: to start a new DM with them
      %initiate
    :_  ship/!>(partner.poke)
    :~  :-  (snoc here.bowl %pub)
        [%make %message-pub ~ ~]
        ::
        :-  provider.poke
        [%poke dm-diff/!>([%invited here.bowl])]
    ==
  ::
      :: create me with a pith to an inviter's dm
      :: to accept their DM request
      %invited
    :_  ship/!>(partner.poke)
    :~  :-  (snoc here.bowl %pub)
        [%make %message-pub ~ ~]
        ::
        :-  (snoc here.bowl %sub)
        [%make %message-sub ~ (malt ~[[%pub (snoc dm.poke %pub)]])]
        ::
        :-  dm.poke
        [%poke dm-diff/!>([%acked here.bowl])]
    ==
  ==
```

The `+poke` arm handles `%acked` and `%post` pokes. When we receive an `%acked`, we create an `/imp/message-sub` to subscribe to DMs from the "publisher", which is whoever we're going to talk to. (DM state is symmetrical: both ships are publishing to eachtoher and subscribed to eachother.) When we receive a `%post` from the publisher, we add it to our service provider at `/path/to/this/dm/pub`, which publishes the DM state to the other ship.

```hoon
++  poke
  |=  [=stud:neo vax=vase]
  ^-  (quip card:neo pail:neo)
  ?>  =(%dm-diff stud)
  =/  poke  !<(dm-diff vax)
  ?+    -.poke  !!
      :: invitee pokes me with a pith to their DM
      :: to finalize the negotiation
      %acked
    =/  partner  !<(ship q.state)
    ?>  =(partner ship.src.bowl)
    :_  state
    :~  :-  (snoc here.bowl %sub)
        [%make %message-sub ~ (malt ~[[%pub (snoc dm.poke %pub)]])]
    ==
  ::
      %post
    ?>  =(our ship.src):bowl
    :_  state
    :~  :-  (snoc here.bowl %pub)
        [%poke txt/!>(text.poke)]
    ==
  ==
```

### Group chats in shrubbery
`/imp/groupchat` uses exactly the same service providers as `/imp/dm` for publishing and subscribing to messages. The only difference is that it's negotiating state between several ships using a one-to-many flow, rather than carefully mirroring state between two ships.

#### kook:neo
Like `/imp/dm`, `/imp/groupchat` just defines two kids at `/.../pub` and `/.../sub` for service providers.

```hoon
++  state  [%pro %groupchat]
++  poke   (sy %groupchat-diff ~)
++  kids
  :-  some
  :-  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [[%.n %pub] %.n]
      [[%pro %message-pub] (sy %sig ~)]
      :-  [[%.n %sub] %.n]
      [[%pro %message-sub] (sy %sig ~)]
  ==
++  deps  *deps:neo
++  form
  ...
```

The state is a `groupchat`, which is just a set of members, pending members, and a `pith` for the location of this chat in the namespace.

```hoon
$:  members=(set ship)
    pending=(set ship)
    host=pith
==
```

`/imp/groupchat` takes six pokes:
- `%invite`: Invite someone to the groupchat.
- `%remove`: Kick someone from the groupchat. (Currently only removes their ability to post.)
- `%invited`: Receive an invite to a groupchat.
- `%acked`: Acknowledge acceptance of a groupchat invite.
- `%post-to-host`: Send a post to the groupchat's host.
- `%host-to-pub`: Send a post from the groupchat's host to their publisher shrub.

```hoon
$%  [%invite =ship provider=pith]
    [%remove =ship]
    [%invited host=pith]
    [%acked ~]
    [%post-to-host text=@t]
    [%host-to-pub text=@t]
==
```

#### +init
When it's initialized, `/imp/groupchat` has either been created by a host on their own ship, or it's been created on a new member's ship by the host.

If it has no state, it creates the new chat with `%message-pub` and `%message-sub` providers. If it does have some initial state, it assumes it's being created by a foreign host ship and takes that state to be the chat history. It only needs to create a `%message-sub` to receive new messages from the publisher.

```hoon
++  init
  |=  old=(unit pail:neo)
  ^-  (quip card:neo pail:neo)
  ::  default case: make new groupchat with self as only member,
    ::  and subscribe to that publisher
  ::  XX - maybe move ordering is unpredictable here
  ?~  old
    :_  :-  %groupchat
        !>([(sy our.bowl ~) ~ here.bowl])
    :~  :-  (snoc here.bowl %pub)
        [%make %message-pub ~ ~]
        ::
        :-  (snoc here.bowl %sub)
        [%make %message-sub ~ (malt ~[[%pub (snoc here.bowl %pub)]])]
    ==
  ::  otherwise, I've been created as an invitee to
  ::  someone else's groupchat
  ?>  =(%groupchat-diff p.u.old)
  =/  poke  !<(groupchat-diff q.u.old)
  ?+    -.poke  !!
      %invited
    :_  :-  %groupchat
        !>([~ ~ host.poke])
    :~  :-  (snoc here.bowl %sub)
        [%make %message-sub ~ (malt ~[[%pub (snoc host.poke %pub)]])]
        ::
        :-  host.poke
        [%poke groupchat-diff/!>([%acked ~])]
    ==
  ==
```

#### +poke
In the `+poke` arm we handle `%invite`, `%remove`, `%acked`, and `%post-to-host`, which is mostly a wrapper around `%host-to-pub`.

Even though it doesn't handle messages — just access control — `/imp/groupchat` has some state to manage. Moreso than antyhing we've seen before, the below should look a lot like a Gall agent.

{what's the heuristic for "should I store this data in "top-level" shrub state or in the namespace below it?}

```hoon
++  poke
  |=  [=stud:neo vax=vase]
  ^-  (quip card:neo pail:neo)
  ?>  =(%groupchat-diff stud)
  =/  sta  !<(groupchat q.state)
  =/  poke  !<(groupchat-diff vax)
  ?+    -.poke  !!
      :: if I'm the host, poke someone's provider to invite them to chat
      %invite
    ?>  =(our ship.src):bowl
    ?<  (~(has in members.sta) ship.poke)
    ::  ?>  =(our.bowl ->.host.sta) :: XX need @p, have @t ?
    :_  :-  %groupchat
        !>(sta(pending (~(put in pending.sta) ship.poke)))
    :~  :-  provider.poke
        [%poke groupchat-diff/!>([%invited here.bowl])]
    ==
  ::
      ::  remove someone from chat. this only removes their ability to post;
      ::  they'll still be receiving new messages!
      %remove
    ?>  =(our ship.src):bowl
    ?>  (~(has in members.sta) ship.poke)
    :-  ~
    :-  %groupchat
    !>  %=  sta
          pending  (~(del in pending.sta) ship.src.bowl)
          members  (~(del in members.sta) ship.src.bowl)
        ==
  ::
      :: when invitee acks, remove them from pending
      :: and add them to pub's permissions
      %acked
    ?>  (~(has in pending.sta) ship.src.bowl)
    :-  ~
    :-  %groupchat
    !>  %=  sta
          pending  (~(del in pending.sta) ship.src.bowl)
          members  (~(put in members.sta) ship.src.bowl)
        ==
  ::
      %post-to-host
    :_  state
    :~  :-  host.sta
        [%poke groupchat-diff/!>([%host-to-pub text.poke])]
    ==
  ::
      %host-to-pub
    ?>  (~(has in members.sta) ship.src.bowl)
    :_  state
    :~  :-  (snoc here.bowl %pub)
        [%poke message/!>([ship.src.bowl now.bowl text.poke])]
    ==
  ==
```

### Messenger shrub
`/imp/messenger` is the top-level interface through which users can create, post in, and manage groupchats and DMs. This is the only part of the system that has a UI.

#### kook:neo
Messenger has no state. This shrub is just an interface for creating groupchats and DMs, which are its kids. If those kids are `/imp/dm`s they take `%dm-diff`s, and if they're `/imp/groupchat`s they take `%groupchat-diff`s.

{what's going on with the `%t` piths?}

```hoon
++  state  [%pro %sig]
++  poke  (sy %dm-diff %groupchat-diff ~)
++  kids
  %-  some
  :-  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [[%.n %t] %.n]
      [[%pro %dm] (sy %dm-diff ~)]
      :-  [[%.n %t] %.n]
      [[%pro %groupchat] (sy %groupchat-diff ~)]
  ==
++  deps  *deps:neo
++  form
  ...
```

Messenger only supports three user actions: creating a new DM, creating a new groupchat, and inviting someone to a groupchat. Everything else is handled by this shrub's kids.

```hoon
$%  [%new-dm partner=ship]
    [%new-groupchat name=@t]
    [%invite-to-groupchat name=@t =ship]
==
```

#### +init
Messenger does nothing much on `+init`. Here's `%sig` again, this time head-tagging an empty `pail:neo`. This shrub stores no state, so we can't inject some when we initialize it.

```hoon
++  init
  |=  old=(unit pail:neo)
  ^-  (quip card:neo pail:neo)
  [~ [%sig !>(~)]]
```

#### +poke
`/imp/messenger` takes `%messenger-diff`s, but it also takes `%dm-diff`s and `%groupchat-diff`s and, if they're invites to those chats, `%make`s the chat at the right location in the namespace.

{what is the difference between `%invite-to-groupchat` and `%invited` `%groupchat-diff`?}

```hoon
++  poke
  |=  [=stud:neo vax=vase]
  ^-  (quip card:neo pail:neo)
  ?+    stud  !!
      %dm-diff
    =/  poke  !<(dm-diff vax)
    ?>  =(%invited -.poke)
    :_  state
    :~  :-  (welp here.bowl ~[%dms p/ship.src.bowl])
        [%make %dm `dm-diff/vax ~]
    ==
  ::
      %groupchat-diff
    =/  poke  !<(groupchat-diff vax)
    ?+    -.poke  !!
        %invited
      :_  state
      :~  :-  (welp here.bowl ~[%groupchats p/ship.src.bowl (rear host.poke)])
          [%make %groupchat `groupchat-diff/vax ~]
      ==
    ==
  ::
      %messenger-diff
    ?>  =(our ship.src):bowl
    =/  poke  !<(messenger-diff vax)
    ?-    -.poke
        %new-dm
      =/  provider  ~[p/partner.poke %home %messenger]
      :_  state
      :~  :-  (welp here.bowl ~[%dms p/partner.poke])
          [%make %dm `dm-diff/!>([%initiate partner.poke provider]) ~]
      ==
    ::
        %new-groupchat
      :_  state
      :~  :-  (welp here.bowl ~[%groupchats p/our.bowl t/name.poke])
          [%make %groupchat ~ ~]
      ==
    ::
        %invite-to-groupchat
      =/  provider  ~[p/ship.poke %home %messenger]
      :_  state
      :~  :-  (welp here.bowl ~[%groupchats p/our.bowl t/name.poke])
          [%poke groupchat-diff/!>([%invite ship.poke provider])]
      ==
    ==
  ==
```

### /imp/message-pub
The only part of the Messenger backend left to consider is `/imp/message-pub`. This only imports `/pro/txt` and `/pro/message`.

#### kook:neo
`/imp/message-pub` only has one job, and that's to `%make` `%message`s as kids. Shrubs don't know anything about the shrubs above them that they weren't explicitly told in their `%make` card, so `/imp/message-pub` doesn't know or care whether it's being used to publish DMs or groupchat messages.

```hoon
++  state  pro/%sig
++  poke  (sy %message %txt ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%da |]
      [pro/%message (sy %sig ~)]
  ==
++  deps  *deps:neo
++  form
  ...
```

#### +init
Like `/imp/messenger`, `/imp/message-pub` takes no state.

```hoon
++  init
  |=  old=(unit pail:neo)
  ^-  (quip card:neo pail:neo)
  [~ sig/!>(~)]
```

#### +poke
All that happens in the `+poke` arm is this shrub creating `%message`s below it in the namespace. {But in what context are we receiving well-formed `%message`s and when are we receiving `%txt`s which we have to use to construct valid `%message`s?}

```hoon
++  poke
  |=  [=stud:neo vax=vase]
  ^-  (quip card:neo pail:neo)
  ?>  =(our ship.src):bowl
  :_  state
  ?+    stud  !!
      %message
    =/  msg  !<(message vax)
    :~  :-  (welp here.bowl ~[da/now.msg])
        [%make %message `message/vax ~]
    ==
      %txt
    =/  contents=@t  !<(txt vax)
    :~  :-  (welp here.bowl ~[da/now.bowl])
        [%make %message `message/!>([ship.src.bowl now.bowl contents]) ~]
    ==
  ==
```
