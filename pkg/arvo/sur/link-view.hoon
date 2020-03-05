::  link-view: encapsulating link management
::
|%
++  view-action
  $%  ::  %create: create a new link collection
      ::
      ::    with specified metadata and group. %ships creates a new group,
      ::    :real-group indicates whether it's global (&) or local (|).
      ::
      $:  %create
          =path
          title=@t
          description=@t
          members=create-members
          real-group=?
      ==
    ::
      ::  %delete: remove collection from local metadata & stop syncing
      ::
      ::    if the resource is associated with a real group, and we're the owner,
      ::    this deletes it for everyone
      ::
      [%delete =path]
    ::
      ::  %invite: add to resource's group and send invite
      ::
      [%invite =path ships=(set ship)]
  ==
::
++  create-members
  $%  [%group =path]
      [%ships ships=(set ship)]
  ==
--
