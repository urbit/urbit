/-  *resource
^?
|%
::
+$  app-name      term
+$  md-resource   [=app-name =resource]
+$  association   [group=resource =metadatum]
+$  associations  (map md-resource association)
+$  group-preview
  $:  group=resource
      channels=associations
      members=@ud
      channel-count=@ud
      =metadatum
  ==
::
+$  color  @ux
+$  url    @t
::
::  $vip-metadata: variation in permissions
::
::    This will be passed to the graph-permissions mark
::    conversion to allow for custom permissions.
::
::    %reader-comments: Allow readers to comment, regardless
::      of whether they can write. (notebook, collections)
::    %member-metadata: Allow members to add channels (groups)
::    %$: No variation
::
+$  vip-metadata  ?(%reader-comments %member-metadata %$)
+$  md-config
  $~  [%empty ~]
  $%  [%group feed=(unit (unit md-resource))]
      [%graph module=term] 
      [%empty ~]
  ==
::
+$  metadatum
  $:  title=cord
      description=cord
      =color
      date-created=time
      creator=ship
      config=md-config
      picture=url
      preview=?
      hidden=?
      vip=vip-metadata
  ==
::
+$  action
  $%  [%add group=resource resource=md-resource =metadatum]
      [%remove group=resource resource=md-resource]
      [%initial-group group=resource =associations]
  ==
::
+$  hook-update
   $%  [%req-preview group=resource]
       [%preview group-preview]
   ==
::
+$  update
  $%  action
      [%associations =associations]
      $:  %updated-metadata 
          group=resource
          resource=md-resource 
          before=metadatum
          =metadatum
      ==
  ==
--
