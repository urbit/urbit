/-  common=safe-common
|%
::  the server state is a hierarchy of node objects, referred to by path
::
++  node
  $~  [~ 1 *snapshot:common *vase ~]
  $:  ::  the relevant parts of the server state should always be rebuildable
      ::  from the event log itself.
      ::
      event-log=(list [id=@ud =event-log-item:common])
      ::  the next event id from the
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
