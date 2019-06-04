|%
+$  item-id
  $?  coll=@tas
      [coll=@tas post=@tas]
      [coll=@tas post=@tas comment=@tas]
  ==
::
+$  action
  $%  $:  %new-collection  
          name=@tas 
          title=@t 
          com=comment-config
          edit=edit-config
          perm=perm-config
      ==
  ::
      $:  %new-post 
          coll=@tas
          name=@tas
          title=@t
          com=comment-config
          perm=perm-config
          content=@t
      ==
  ::
      [%new-comment coll=@tas post=@tas content=@t]
  ::
      [%delete item-id]
  ::
      $:  %edit-collection
          name=@tas
          title=@t
          com=comment-config
          edit=edit-config
          perm=perm-config
      ==
  ::
      $:  %edit-post
          coll=@tas
          name=@tas
          title=@t
          com=comment-config
          perm=perm-config
          content=@t
      ==
  ::
      [%edit-comment coll=@tas post=@tas id=@tas content=@t]
  ::
      [%invite coll=@tas who=(list ship)]
  ::
      [%serve coll=@tas]
      [%unserve coll=@tas]
  ::
      [%subscribe who=@p coll=@tas]
      [%unsubscribe who=@p coll=@tas]
  ::
  ==
::
+$  collection-info
  $:  owner=@p
      title=@t
      filename=@tas
      comments=comment-config
      allow-edit=edit-config
      date-created=@da
      last-modified=@da
  ==
::
+$  post-info
  $:  creator=@p
      title=@t
      collection=@tas
      filename=@tas
      comments=comment-config
      date-created=@da
      last-modified=@da
      pinned=?
  ==
::
+$  comment-info
  $:  creator=@p
      collection=@tas
      post=@tas
      date-created=@da
      last-modified=@da
  ==
::
+$  perm-config  [read=rule:clay write=rule:clay]
::
::
+$  comment-config  $?(%open %closed %none)
::
::
+$  edit-config     $?(%post %comment %all %none)
::
+$  rumor  delta
::  $%  [%collection who=@p col=@tas dat=(each collection-info tang)]
::      [%post who=@p col=@tas pos=@tas dat=(each [post-info manx] tang)]
::      [%comments who=@p col=@tas pos=@tas dat=(each (list [comment-info manx]) tang)]
::      [%serve who=@p nom=@tas col=collection]
::  ==
::
+$  collection
  $:  col=(each collection-info tang)
      pos=(map @tas (each [post-info manx] tang))
      com=(map @tas (each (list [comment-info manx]) tang))
      order=[pin=(list @tas) unpin=(list @tas)]
  ==
::
+$  state
  $:  pubs=(map @tas collection)
      subs=(map [ship @tas] collection)
      awaiting=(map @tas [builds=(set wire) partial=(unit delta)])
      latest=(list [who=ship coll=@tas post=@tas])
      unread=(set [who=ship coll=@tas post=@tas])
  ==
::
+$  delta
  $%  [%collection who=@p col=@tas dat=(each collection-info tang)]
      [%post who=@p col=@tas pos=@tas dat=(each [post-info manx] tang)]
      [%comments who=@p col=@tas pos=@tas dat=(each (list [comment-info manx]) tang)]
      [%total who=@p col=@tas dat=collection]
  ==
--
