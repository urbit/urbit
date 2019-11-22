|%
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
          who=@p
          coll=@tas
          name=@tas
          title=@t
          com=comment-config
          perm=perm-config
          content=@t
      ==
  ::
      [%new-comment who=@p coll=@tas post=@tas content=@t]
  ::
      [%delete-collection coll=@tas]
      [%delete-post coll=@tas post=@tas]
      [%delete-comment coll=@tas post=@tas comment=@tas]
  ::
      [%edit-collection name=@tas title=@t]
  ::
      $:  %edit-post
          who=@p
          coll=@tas
          name=@tas
          title=@t
          com=comment-config
          perm=perm-config
          content=@t
      ==
  ::
      [%invite coll=@tas title=@t who=(list ship)]
      [%reject-invite who=@p coll=@tas]
  ::
      [%serve coll=@tas]
      [%unserve coll=@tas]
  ::
      [%subscribe who=@p coll=@tas]
      [%unsubscribe who=@p coll=@tas]
  ::
      [%read who=@p coll=@tas post=@tas]
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
+$  comment  [info=comment-info body=@t]
::
+$  perm-config  [read=rule:clay write=rule:clay]
::
+$  comment-config  $?(%open %closed %none)
::
+$  edit-config     $?(%post %comment %all %none)
::
+$  rumor  delta
::
+$  publish-dir  (map path publish-file)
::
+$  publish-file
  $%  [%udon @t]
      [%publish-info collection-info]
      [%publish-comment comment]
  ==
::
+$  collection
  $:  col=(each collection-info tang)
      pos=(map @tas dat=(each [post-info manx @t] tang))
      com=(map @tas dat=(each (list [comment-info @t]) tang))
      order=[pin=(list @tas) unpin=(list @tas)]
      contributors=[mod=?(%white %black) who=(set @p)]
      subscribers=(set @p)
      last-update=@da
  ==
::
+$  delta
  $%  [%collection who=@p col=@tas dat=(each collection-info tang)]
      [%post who=@p col=@tas pos=@tas dat=(each [post-info manx @t] tang)]
      [%comments who=@p col=@tas pos=@tas dat=(each (list comment) tang)]
      [%total who=@p col=@tas dat=collection]
      [%remove who=@p col=@tas pos=(unit @tas)]
  ==
::
+$  update
  $%  [%invite add=? who=@p col=@tas title=@t]
      [%unread add=? keys=(set [who=@p coll=@tas post=@tas])]
  ==
--
