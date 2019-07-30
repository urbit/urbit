::  Common data structures used on both the server and the client.
::
/-  *safe-applet
|%
::  every root node keeps metadata about the entire server, which is used for
::  ring signatures.
::
++  top-state
  $:  invited=(set @p)
      community-name=@t
      original-host=@p
  ==
::  +snapshot: representation for being on a ship
::
++  snapshot
  $~  [%auth ~ %community *vase ~]
  $:  ::  which of the built-in app types we represent
      ::
      app-type=@t
      ::  the top-state; community identification information only on /
      ::
      top-state=(unit top-state)
      ::  what sort of signature should be sent to this node
      ::
      =signature-type
      ::  the public per-applet snapshot data
      ::
      snapshot=vase
      ::  the live iterable children are tracked as a set inside the snapshot
      ::
      children=(set @t)
  ==
::  all events that could exist in an event log
::
++  event-log-item
  $%  ::  a special init which must be the first item in the / node.
      ::
      $:  %toplevel-init
          initial-invited=(set @p)
          community-name=@t
          original-host=@p
          app-type=@t
          =signature-type
      ==
      ::  invites a person into the community. only valid on the / node.
      ::
      [%toplevel-invite ship=@p]
  ::
      ::
      ::  the first item in any normal event log; sets signature information,
      ::  but calls no node code
      [%init route=path app-type=@t =signature-type]
      ::
      ::  when sending across the wire, just send the value in the vase, not
      ::  the type. the other side knows what app its for and at least for now,
      ::  the remote will call the mold.
      ::
      $:  %log
          ::  TODO: Next action isn't splitting up the files, but making sure
          ::  local signatures appear in the %log!
          ::
          ::  msg-signature=full-signature
          ::  route=path
          user-event=vase
          private-event=vase
      ==
      ::
      ::  creates a new child node under this
      [%create sub-id=@t app-type=@t =signature-type]
  ==
::  representation of a snapshot on the wire; contains no vases
::
++  transport-snapshot
  $:  ::  which of the built-in app types we represent
      ::
      app-type=@t
      ::  the top-state; community identification information only on /
      ::
      top-state=(unit top-state)
      ::  what sort of signature should be sent to this node
      ::
      =signature-type
      ::  a snapshot reconstitutable with app-type into a real vase
      ::
      raw-snapshot=*
      ::  the live iterable children are tracked as a set inside the snapshot
      ::
      children=(set @t)
  ==
::  representation of an event-log-item on the wire; contains no vases
::
++  transport-event-log-item
  $%  ::  a special init which must be the first item in the / node.
      ::
      $:  %toplevel-init
          initial-invited=(set @p)
          community-name=@t
          original-host=@p
          type=@t
          =signature-type
      ==
      ::  invites a person into the community. only valid on the / node.
      ::
      [%toplevel-invite ship=@p]
  ::
      ::
      ::  the first item in any normal event log; sets signature information,
      ::  but calls no node code
      [%init route=path type=@t =signature-type]
      ::
      ::  when sending across the wire, just send the value in the vase, not
      ::  the type. the other side knows what app its for and at least for now,
      ::  the remote will call the mold.
      ::
      $:  %log
          ::  msg-signature=full-signature
          ::  route=path
          user-event=*
          private-event=*
      ==
      ::
      ::  creates a new child node under this
      [%create sub-id=@t type=@t =signature-type]
  ==
--
