::
::::  /hoon/collections/lib
  ::
/?  309
/+  cram
::
::
|%
+=  collection  [meta=config data=(map nom=knot =item)]
+=  item
  $%  [%collection col=collection]
      [%raw raw=raw-item]
      [%both col=collection raw=raw-item]
  ==
+=  raw-item
  $%  [%umd meta=(map knot cord) data=@t]
  ==
::
+=  config
  $:  full-path=beam
      name=@t
      description=@t
    ::
      owner=@p
    ::
      date-created=@da
      last-modified=@da
    ::
      type=@tas
      comments=?
      sort-key=(unit @)
      visible=?
    ::
  ==
::
+=  action
  $:  who=ship
      dek=desk
      acts=(list sub-action)
  ==
+=  sub-action
  $%  [%write pax=path for=form]
      [%delete pax=path]
      [%perms pax=path r=rule:clay w=rule:clay]
    ::
      [%collection pax=path name=@t desc=@t comments=? visible=? type=@tas]
      [%post pax=path name=@t type=@tas comments=? content=@t]
      [%comment pax=path content=@t]
  ==
::
+=  form
  $%  [%umd @t]
      [%collections-config config]
  ==
::
--
