/-  *rw-security
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
+$  group-info
  $%  [%old par=path sub=path]
      [%new par=(set ship) sub=(set ship) sec=rw-security]
  ==
::
+$  action-2
  $%  [%new-book book=@tas title=@t group=group-info]
      [%new-note who=@p book=@tas note=@tas title=@t body=@t]
      [%new-comment who=@p book=@tas note=@tas body=@t]
  ::
      [%edit-book book=@tas new-title=(unit @t) new-group=(unit group-info)]
      [%edit-note who=@p book=@tas note=@tas new-title=@t new-body=@t]
      [%edit-comment who=@p book=@tas note=@tas comment=@tas new-body=@t]
  ::
      [%del-book book=@tas]
      [%del-note who=@p book=@tas note=@tas]
      [%del-comment who=@p book=@tas note=@tas comment=@tas]
  ::
      [%subscribe who=@p book=@tas]
      [%unsubscribe who=@p book=@tas]
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
+$  comment
  $:  author=@p
      date-created=@da
      last-modified=@da
      body=@t
  ==
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
  $:  col=[=bone dat=(each collection-info tang)]
      pos=(map @tas [=bone dat=(each [post-info manx @t] tang)])
      com=(map @tas [=bone dat=(each (list comment) tang)])
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
