/-  common=safe-common
::  Representation of a client state tree
::
|%
+$  subscribed
  $?  :: we aren't subscribed at all
      ::
      %unsubscribed
      ::  we are waiting for the first return for our +peer
      ::
      %pending
      ::  we have received at least our first +peer, but no %quit.
      ::
      %subscribed
  ==
::
+$  node
  $~  [%unsubscribed ~ ~ ~ ~]
  $:  ::  whether this node is currently subscribed to the server for updates
      ::
      =subscribed
      ::  an event log with snapshots in it.
      ::
      partial-event-log=(list [id=@ud item=event-item])
      ::  the current state of the node
      ::
      ::    The :partial-event-log may have %snapshots in them, but the head
      ::    may not be a snapshot so always have the current head rendered as
      ::    a snapshot.
      ::
      ::    However, we may not have any information about this node, only
      ::    understand that its exists in the graph.
      ::
      snapshot=(unit snapshot:common)
      ::  live children state we know about
      ::
      ::    When we see a %create event, we place a ~ in this node until we
      ::    follow it.
      ::
      children=(map @t (unit node))
      ::  archived children state
      ::
      ::    In the event log, when a %remove event occurs, that data gets
      ::    wiped on the server. On the client, removed nodes go to the
      ::    archive where they are no longer modifiable.
      ::
      ::    In the case that we never learned anything about the thread other
      ::    than its existence, we don't add anything here.
      ::
      archived=(map @t node)
  ==
::  state created from event log items that
::
++  top-state
  $:  invited=(set @p)
      community-name=@t
      original-host=@p
  ==
::  on the client, we may only have events 1,2,3 and then a snapshot of 10
::  and then events 11,12,13.
::
++  event-item
  $%  [%snapshot =snapshot:common]
      [%event =event-log-item:common]
  ==
--
