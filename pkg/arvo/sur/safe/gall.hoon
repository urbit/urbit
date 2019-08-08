/-  common=safe-common
::  Data structures used for gall to pass data structures across ships.
::
|%
::  Pokes you can send to safe. This contains a tag for gall's pattern matching
::  crud.
::
+$  poke
  [%safe-poke command=poke-command]
::  The actual poking data structure for use inside the app.
::
++  poke-command
  $%  ::  a local action which can only be done by :our
      ::
      [%create-community name=@t initial-invitees=(set @p)]
      ::  a local action which can only be done by :our
      ::
      [%send-message host=@p name=@t =path data=*]
      ::  a poke which anyone can do.
      ::
      [%receive-message name=@t =client-to-server:common]
  ==
::  Peers you can send to safe. This contains a tag for gall's pattern matching
::  crud.
::
+$  peer
  [%safe-peer command=peer-command]
::  The actual peering data structure for use inside the app.
::
+$  peer-command
  $%  ::  from +peer-diff:common
      ::
      [%snapshot id=@u snapshot=transport-snapshot:common]
      ::  from +peer-diff:common
      ::
      [%event id=@u event=transport-event-log-item:common]
      ::  sent when the host community either doesn't exist or the requester
      ::  isn't allowed to access it.
      ::
      [%not-found ~]
  ==
--
