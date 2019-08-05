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
  processed-signature
::  user events which target the toplevel node are all about doing membership checks
::
+$  user-event
  $%  [%invite ship=@p]
      [%create name=@t app-type=@t =signature-type]
  ==
::
+$  private-event
  ~
::
+$  private-state
  ~
::
+$  snapshot
  $:  ::  this node keeps track of the
      ::
      banned-tags=(map @udpoint @da)
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
      [%create @t @t =signature-type =child-event]
      [%return =return-event]
  ==
::  +on-route: everything routes through the toplevel node. this is what does 
::
::    TODO: child-event or return-event, to force routing to be able to act
::    and terminate processing?
::
++  on-route
  |=  [=path =parent-event =private-state]
  ^-  (unit child-event)
  ::
  ?-    -.parent-event
      %ship
    ~&  [%todo-verify-signature-for-ship ship.parent-event]
    `[%ship ship.parent-event]
  ::
      %ring
    ::
    =/  ships=(set @p)
      %-  sy
      %+  turn
        ~(tap by participants.ring-signature.parent-event)
      head
    ::
    ~&  [%todo-verify-ring-signature-for ships]
    ::
    `[%ring / y.raw.ring-signature.parent-event]
  ==
::
++  on-process-event
  |=  [=parent-event =user-event =private-state]
  ^-  [on-process-response _private-state]
  ::  todo: since we're the toplevel node, we also need to perform auth here.
  ::
  ?-    -.user-event
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
        :: TODO: We must get our current node name and weld our new name to the end.
        ::
        (process-signature / parent-event)
    ==
  ==
::
++  apply-event-to-snapshot
  |=  [=user-event =private-event =snapshot]
  ^-  _snapshot
  ?-    -.user-event
  ::
      %invite
    snapshot
  ::
      %create
    snapshot
  ==
::
++  on-child-return
  |=  [=child-returned =private-state]
  ^-  [return-event _private-state]
  [child-returned private-state]
--
