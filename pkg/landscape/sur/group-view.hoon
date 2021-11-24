/-  *resource, *group
^?
|%
+$  app  ?(%graph %groups)
+$  uid  @uvH
::
::  $request: State of a join request
::
::    .started: Time request first sent
::    .ship: Host of group
::    .progress: Progress of request
::    .share-co: Automatically share contact?
::    .autojoin: Automatically join graphs
::    .app: Whether we're joining a group or a graph
::    .invite: Associated invites
::
+$  request
  $:  started=time
      =ship
      =progress
      =app
      share-co=?
      autojoin=?
      invite=(set uid)
  ==
::
+$  action
  $%  ::  host side
      [%create name=term =policy title=@t description=@t]
      [%remove =resource]
      ::  client side
      $:  %join
          =resource
          =ship
          =app
          share-contact=?
          autojoin=?
      ==
      [%abort =resource]
      [%leave =resource]
      ::
      [%invite =resource ships=(set ship) description=@t]
      ::  pending ops
      [%done =resource]
  ==
::  $progress: state of a join request
::
::     %start: Waiting on add poke to succeed
::     %added: Waiting on groups
::     %metadata: Waiting on metadata
::     final: Join request succeeded/errors
+$  progress
  ?(%start %added %metadata final)
::
::  $final: resolution of a join request
::
::     %no-perms: Failed, did not have permissions
::     %abort: Join request manually aborted
::     %strange: Failed unexpectedly
::     %done: Succeeded
::
+$  final
  ?(%no-perms %abort %strange %done)
::
+$  update
  $%  [%initial initial=(map resource request)]
      [%started =resource =request]
      [%progress =resource =progress]
      [%hide =resource]
  ==
--
