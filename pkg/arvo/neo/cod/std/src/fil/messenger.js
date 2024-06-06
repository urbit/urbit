# Tutorial 3: Messenger
The last major component of a `kook:neo` that we need to cover is `+deps`, which is one component of shrubbery’s dependencies system. The other is the `%rely` poke, which is also covered here.

By the end of this tutorial you’ll understand how dependencies work and how to use them. You should also start to see how you can design functionality that involves multiple shrubs interacting across multiple ships.

We’ll take a look at Messenger. Messenger is one shrub, located at `/imp/messenger.hoon`, but it relies on five other shrubs to work.
- `/imp/messenger` is the high-level interface and “service provider” that enables user to create groupchats and 1-to-1 DMs, and invite other ships to them.
- `/imp/message` is a `~` stub that allows us to create `/message`s in the namespace
- `/imp/message-pub` is a shrub that publishes `/message`s from its host ship.
- `/imp/message-sub` mirrors `/message`s from a `/message-pub` shrub into its own state.
- `/imp/groupchat` creates a publisher/subscriber pair, and can invite other shrubs on other ships to post/subscribe to the `/message-pub` shrub.
- `/imp/dm` negotiates a two-way pub/sub relationship and mirrors state between both parties.

One motivation behind this design is to split off functionality into simple, reusable shrubs. This is good practice in general, but in the context of shrubbery “reusable” shrubs could be leveraged by any other shrub on Urbit. `/imp/dm` needn’t just be the DM shrub for `/messenger`, it could also be used off-the-shelf for 1-to-1 chats in shrubs like `/chess`, `/twitter`, or `/ebay`.

## /imp/message-pub
```hoon
/@  txt      ::  cord
/@  message  ::  [from=ship now=time contents=@t]
::
^-  kook:neo
|%
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
  --
--
```

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

## /imp/dm
```hoon
::
::  $dm-diff
::  $%  [%initiate partner=ship provider=pith]
::      [%invited partner=ship dm=pith]
::      [%acked dm=pith]
::      [%post text=@t]
::  ==
/@  dm-diff
^-  kook:neo
|%
++  state  pro/%ship  :: who I'm chatting with
++  poke  (sy %dm-diff ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%theirs |]
      [pro/%message-pub (sy %sig ~)]
      :-  [|/%mine |]
      [pro/%message-sub (sy %sig ~)]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo state=pail:neo]
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
  --
--
```

## /imp/groupchat
```hoon
::
::  $groupchat
::  $:  members=(set ship)
::      pending=(set ship)
::      host=pith
::  ==
/@  groupchat
::
::  $groupchat-diff
::  $%  [%invite =ship provider=pith]
::      [%remove =ship]
::      [%invited host=pith]
::      [%acked ~]
::      [%post-to-host text=@t]
::      [%host-to-pub text=@t]
::  ==
/@  groupchat-diff
^-  kook:neo
|%
++  state  pro/%groupchat
++  poke  (sy %groupchat-diff ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%pub |]
      [pro/%message-pub (sy %sig ~)]
      :-  [|/%sub |]
      [pro/%message-sub (sy %sig ~)]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo state=pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    ::  default case: make new groupchat with self as only member,
    ::  and subscribe to that publisher
    ::  XX - maybe move ordering is unpredictable here
    ?~  old
      :_  :-  %groupchat
          !>([(sy our.bowl ~) ~ (snoc here.bowl %pub)])
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
      :_  groupchat/!>([~ ~ host.poke])
      :~  :-  (snoc here.bowl %sub)
          [%make %message-sub ~ (malt ~[[%pub (snoc host.poke %pub)]])]
          ::
          :-  host.poke
          [%poke groupchat-diff/!>([%acked ~])]
      ==
    ==
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
  --
--
```
## /imp/messenger
```hoon
::
::  $dm-diff
::  $%  [%initiate partner=ship provider=pith]
::      [%invited partner=ship dm=pith]
::      [%acked dm=pith]
::      [%post text=@t]
::  ==
/@  dm-diff
::
::  $groupchat-diff
::  $%  [%invite =ship provider=pith]
::      [%remove =ship]
::      [%invited host=pith]
::      [%acked ~]
::      [%post-to-host text=@t]
::      [%host-to-pub text=@t]
::  ==
/@  groupchat-diff
::
::  $messenger-diff
::  $%  [%new-dm partner=ship]
::      [%new-groupchat name=@t]
::      [%invite-to-groupchat name=@t =ship]
::  ==
/@  messenger-diff
^-  kook:neo
|%
++  state  pro/%sig
++  poke  (sy %dm-diff %groupchat-diff ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%t |]
      [pro/%dm (sy %dm-diff ~)]
      :-  [|/%t |]
      [pro/%groupchat (sy %groupchat-diff ~)]
  ==
++  deps  *deps:neo
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
  --
--
```

## The +deps arm
The only part of this system that uses the `+deps` arm is `/message-sub`.

```hoon
::
::  define dependencies
++  deps
  ::  deps:neo is (map term fief:neo)
  %-  ~(gas by *deps:neo)
  :~  :*  %pub
          ::  fief:neo is [required=? =quay]
          req=&
          ::  quay:neo is (pair lash:neo (unit port:neo)
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

In this card, `+malt` creates a `(map term pith:neo)` where the `term` is `%pub` and the path represented by the `pith` is `/path/to/chat/pub`. At `/path/to/chat/pub`, there’s a shrub which is the canonical “publisher” or “host” of a chat, whether that’s a group chat or a DM. Whenever there’s a state change in the publisher shrub, the `/message-sub` created in this card will be notified about it, but we’ll cover that in more detail in the next section.

The `%pub` `term` will act as the key in the `map` of dependencies. This means `/message-sub` could have several dependencies. `%pub` doesn’t refer to anything, it’s just being defined in the `+deps` arm to differentiate between shrubs being passed in the card’s `conf:neo`; think of how you’d use head tags to route different cases in the `+on-poke` arm of a Gall agent.

The `required` flag specfies whether the shrub being described in `+deps` needs to already exist in order for this shrub to build. If no publisher exists at `/path/to/chat/pub`, the `/message-sub` we’re trying to `%make` here will fail to build. It can only exist with reference to the publisher shrub.

The last “new” idea here is `%sig`. If you look at `/imp/sig.hoon`, it’s just a `~` stub like the `%txt` implementation. `%sig` imps are not special and are not treated differently to any other stub, they’re just a stylistic convention to say that we don’t care about the state of the shrub in question; it could be anything, we won’t constrain it at all. There’s also a `/pro/sig.hoon` which lets us do the same thing for pokes.

## Listening for state changes in our dependencies
Unlike a Gall agent, a shrub does not send out `%facts` to subscribers in the event of changes to its state, all of which has to be manually implemented by that agent’s developer. Instead, the shrub listens for changes to its own state and automatically sends a `%rely` poke to shrubs that have declared it as a dependency. The developer of the listener shrub is the only one who has to write the logic to handle this.

In its `+deps` arm, the `/message-sub` shrub declares the type and behaviour of the shrubs it will accept as dependencies. Shrubs that conform to that type, like `/message-pub` , can be passed in through the `%make` card and `/message-sub` will listen to those shrubs for state changes.

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

Like we saw in the `+kids` arm in the previous tutorial, the `+deps` arm specifies this `%y` constant. What’s that doing? This is a `care:neo`, similar to the `care`s we see in Arvo vanes.

In the `+kids` arm the `%y` care declares that this shrub is constraining its immediate children, and the `%z` care that it’s recursively constraining all of its descendants in the namespace.

In `+deps`, the `%y` care declares that we’re only listening for state changes in the dependency shrub’s immediate children. If it were `%z`, we’d be subscribing to state changes for all of the dependency shrub’s descendants.

The other `care:neo` you’ll commonly see is `%x`, which refers to a single shrub. You wouldn’t use this in `+kids`, but you might use it in `+deps`.

### Handling %rely pokes
Let’s see how `/message-sub` handles the `%rely` pokes it recevives from dependencies.

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
