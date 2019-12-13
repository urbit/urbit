/-  *rw-security
|%
::
+$  group-info
  $%  [%old writers=path subscribers=path]
      [%new writers=(set ship) subscribers=(set ship) sec=rw-security]
  ==
::
+$  action
  $%  [%new-book book=@tas title=@t about=@t coms=? group=group-info]
      [%new-note who=@p book=@tas note=@tas title=@t body=@t]
      [%new-comment who=@p book=@tas note=@tas body=@t]
  ::
      [%edit-book book=@tas title=@t about=@t coms=? group=(unit group-info)]
      [%edit-note who=@p book=@tas note=@tas title=@t body=@t]
      [%edit-comment who=@p book=@tas note=@tas comment=@tas body=@t]
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
      description=@t
      comments=?
      writers=path
      subscribers=path
      date-created=@da
      notes=(map @tas note)
      order=(list @tas)
      pinned=(set @tas)
  ==
::
+$  notebook-info
  $:  title=@t
      description=@t
      comments=?
      writers=path
      subscribers=path
  ==
::
+$  notebook-delta
  $%  [%add-book host=@p book=@tas data=notebook]
      [%add-note host=@p book=@tas note=@tas data=note]
      [%add-comment host=@p book=@tas note=@tas comment-date=@da data=comment]
  ::
      [%edit-book host=@p book=@tas data=notebook]
      [%edit-note host=@p book=@tas note=@tas data=note]
      [%edit-comment host=@p book=@tas note=@tas comment-date=@da data=comment]
  ::
      [%del-book host=@p book=@tas]
      [%del-note host=@p book=@tas note=@tas]
      [%del-comment host=@p book=@tas note=@tas comment=@da]
  ==
::
+$  primary-delta
  $%  notebook-delta
  ==
--
