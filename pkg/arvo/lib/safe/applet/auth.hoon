/-  *safe-applet
::
::  The default toplevel applet
::
::  The auth applet owns the individual boards and owns the banlist.
::
|%
++  parent-event
  input:toplevel-interface
::
+$  child-event
  $:  now=@da
      =community-signature
  ==
::  user events which target the toplevel node are all about doing membership checks
::
+$  user-event
  $%  ::  initializes the community, setting the current ship up as the first moderator
      ::
      [%init ~]
      ::  invites a new user to the community
      ::
      [%invite ship=@p]
      ::  creates a new board
      ::
      [%create name=@t app-type=@t =signature-type]
      ::
      ::
      ::  TODO:
      ::  - %reorder to change display order
  ==
::
+$  private-event
  $%  [%ok ~]
      [%created name=@t app-type=@t]
  ==
::
+$  private-state
  $:  ::  moderator tags
      ::
      ::    while the banlist is public, the moderators are not.
      ::
      moderators=(set @udpoint)
  ==
::
+$  snapshot
  $:  ::  this node keeps track of the
      ::
      banned-tags=(map @udpoint @da)
      ::
      ::
      display-order=(list [name=@t app-type=@t])
  ==
::
+$  child-returned
  $%  [%accept ~]
      [%reject ~]
  ==
::
+$  return-event
  output:toplevel-interface
::
+$  on-process-response
  $%  [%log =private-event =return-event]
      [%create @t @t =signature-type e=(unit private-event) =child-event]
      [%return =return-event]
  ==
::
+$  on-child-response
  $%  [%return =return-event]
  ==
::  +on-route: everything routes through the toplevel node. this is what does 
::
++  on-route
  |=  [=path =parent-event =snapshot =private-state]
  ^-  (either return-event child-event)
  ::
  ?:  ?=(%ship -.full-signature.parent-event)
    ::  the toplevel auth deals only in ring signatures.
    ::
    [%l %reject ~]
  ::  The toplevel auth is meant to always be in %community signing mode.
  ::
  ?.  ?=(^ y.raw.ring-signature.full-signature.parent-event)
    [%l %reject ~]
  ::
  =/  community-tag=@udpoint  u.y.raw.ring-signature.full-signature.parent-event
  ::
  ?:  (~(has by banned-tags.snapshot) community-tag)
    ::
    ~&  [%banned-tag-trying-to-post community-tag]
    [%l %reject ~]
  ::
  =/  is-moderator=?  (~(has in moderators.private-state) community-tag)
  ::
  ~&  [%is-moderator is-moderator]
  ::
  [%r now.parent-event is-moderator community-tag]
::
++  on-process-event
  |=  [=parent-event =user-event =snapshot =private-state]
  ^-  [on-process-response _private-state]
  ::  Handle init specially. It is the only command that can be executed
  ::  without having any moderator state.
  ::
  ?:  ?=(%init -.user-event)
    ::  ignore reinitialization attempts
    ::
    ?.  =(~ moderators.private-state)
      [[%return [%reject ~]] private-state]
    ::  assert that the signature is a valid %community signed ring
    ::
    ?.  ?&  ?=(%ring -.full-signature.parent-event)
            ?=(^ y.raw.ring-signature.full-signature.parent-event)
        ==
      [[%return [%reject ~]] private-state]
    ::
    =/  moderator-tag=@udpoint  u.y.raw.ring-signature.full-signature.parent-event
    ~&  [%init-with-moderator moderator-tag]
    :-  [%return [%accept ~]]
    private-state(moderators (~(put in moderators.private-state) moderator-tag))
  ::  assert that the signature is a valid %community signed ring and that it
  ::  was built by a moderator
  ::
  ?.  ?&  ?=(%ring -.full-signature.parent-event)
          ?=(^ y.raw.ring-signature.full-signature.parent-event)
          (~(has in moderators.private-state) u.y.raw.ring-signature.full-signature.parent-event)
      ==
    [[%return [%reject ~]] private-state]
  ::
  ?-    -.user-event
  ::
      %invite
    [[%return [%accept-and-invite-member ship.user-event]] private-state]
  ::
      %create
    ~&  [%inside-create user-event]
    :_  private-state
    :*  %create
        name.user-event
        app-type.user-event
        %unlinked
        `[%created name.user-event app-type.user-event]
        [now.parent-event %.y u.y.raw.ring-signature.full-signature.parent-event]
    ==
  ==
::
++  apply-event-to-snapshot
  |=  [user-event=(unit user-event) =private-event =snapshot]
  ^-  _snapshot
  ::
  ?~  user-event
    ?-    -.private-event
        %ok
      snapshot
    ::
        %created
      snapshot(display-order (weld display-order.snapshot [[name app-type]:private-event ~]))
    ==
  ::
  ?-    -.u.user-event
  ::
      ?(%init %invite)
    ::  the init and invite causes no changes to public state, only private
    ::  state, and never gets persisted to the log.
    ::
    snapshot
  ::
      %create
    ::  TODO: Keep the list of subnodes in our public state for ordering and stuff.
    ::
    snapshot
  ==
::
++  on-child-return
  |=  [=child-returned =private-state]
  ^-  [on-child-response _private-state]
  [[%return child-returned] private-state]
--
