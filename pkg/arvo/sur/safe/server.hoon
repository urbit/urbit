/-  common=safe-common
|%
::  the server state is a hierarchy of node objects, referred to by path
::
::    Unlike the client, the server keeps no history. Broadcasts of events are
::    done immediately when received.
::
++  node
  $~  [1 *snapshot:common *vase ~]
  $:  ::  the next event id from the
      ::
      next-event-id=@ud
      ::  the public snapshot; all state that can be reconstructed on the
      ::  client is here.
      ::
      =snapshot:common
      ::  private applet state never shared with clients
      ::
      private-state=vase
      ::  the state of all child nodes
      ::
      children=(map @t node)
  ==
--
