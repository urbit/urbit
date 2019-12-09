/-  *rw-security
|%
::
+$  group-info
  $%  [%old par=path sub=path]
      [%new par=(set ship) sub=(set ship) sec=rw-security]
  ==
::
+$  action
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
+$  comment
  $:  author=@p
      date-created=@da
      last-edit=@da
      content=@t
  ==
::
+$  note
  $:  author=@p
      title=@t
      filename=@tas
      date-created=@da
      last-edit=@da
      file=@t
      build=(each manx tang)
      comments=(map @da comment)
  ==
::
+$  notebook
  $:  title=@t
      date-created=@da
      notes=(map @tas note)
      order=(list @tas)
      pinned=(set @tas)
      participants=path
      subscribers=path
  ==
::
+$  notebook-delta
  $%  [%book book=@tas data=notebook]
      [%book-meta book=@tas title=@t]
      [%note book=@tas note=@tas data=note]
      [%comment book=@tas note=@tas comment-date=@da data=comment]
      [%del-book book=@tas]
      [%del-note book=@tas note=@tas]
      [%del-comment book=@tas note=@tas comment=@da]
  ==
::
+$  primary-delta  !!
--
