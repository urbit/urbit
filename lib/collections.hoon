::
::::  /hoon/collections/lib
  ::
/?  309
/+  cram, elem-to-react-json
::
::
|%
+=  collection  [meta=config data=(map nom=knot =item)]  
+=  item
  $~  [%error ~]
  $%  [%collection col=collection]
      [%raw raw=raw-item]
      [%both col=collection raw=raw-item]
      [%error ~]
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
      [%post pax=path name=@t type=@tas comments=? content=@t edit=?]
      [%comment pax=path content=@t]
  ==
::
+=  form
  $%  [%umd @t]
      [%collections-config config]
  ==
::
++  collection-error
  |=  col=collection
  ^-  ?
  |-
  =/  vals=(list item)  ~(val by data.col)
  %+  roll  vals
  |=  [i=item out=_|]
  ^-  ?
  ?:  out  out
  ?+  -.i
    %.n
    %error       %.y
    %collection  ^$(col col.i)
    %both        ^$(col col.i)
  ==
::::
::::  /mar/snip
::::
++  words  1
++  hedtal
  =|  met/marl
  |=  a/marl  ^-  {hed/marl tal/marl}
  ?~  a  [~ ~]
  :: looks like it only terminates if it finds an h1?
  ?.  ?=($h1 n.g.i.a)
    ?:  ?=($meta n.g.i.a)
      $(a t.a, met [i.a met])
    =+  had=$(a c.i.a)
    ?^  -.had  had
    $(a t.a)
  [c.i.a (weld (flop met) (limit words t.a))]
  ::
::
++  limit
  |=  {lim/@u mal/marl}
  =<  res
  |-  ^-  {rem/@u res/marl}
  ?~  mal  [lim ~]
  ?~  lim  [0 ~]
  =+  ^-  {lam/@u hed/manx}
    ?:  ?=(_;/(**) i.mal)
      [lim ;/(tay)]:(deword lim v.i.a.g.i.mal)
    [rem ele(c res)]:[ele=i.mal $(mal c.i.mal)]
  [rem - res]:[hed $(lim lam, mal t.mal)]
::
++  deword
  |=  {lim/@u tay/tape}  ^-  {lim/@u tay/tape}
  ?~  tay  [lim tay]
  ?~  lim  [0 ~]
  =+  wer=(dot 1^1 tay)
  ?~  q.wer
    [lim - tay]:[i.tay $(tay t.tay)]
  =+  nex=$(lim (dec lim), tay q.q.u.q.wer)
  [-.nex [(wonk wer) +.nex]]
::
::  json
::
++  item-to-json
  |=  itm=item
  ^-  json
  ?-    -.itm
      %error  (frond:enjs:format %error ~)
  ::
      %collection
    %+  frond:enjs:format
    %collection  (collection-to-json col.itm)
  ::
      %raw
    %-  frond:enjs:format
    [%item (raw-to-json raw.itm)]
  ::
      %both
    %-  pairs:enjs:format
    :~  [%item (raw-to-json raw.itm)]
        [%collection (collection-to-json col.itm)]
    ==
  ==
::
++  collection-to-json
  |=  col=collection
  ^-  json
  %-  pairs:enjs:format
  :~  [%meta (config-to-json meta.col)]
      :+  %data  %a
      %+  turn  ~(tap by data.col)
      |=  [nom=knot ite=item]
      ^-  json
      %-  pairs:enjs:format
      :~  [%filename %s nom]
          [%item (item-to-json ite)]
      ==
  ==
::
++  raw-to-json
  |=  raw=raw-item
  ^-  json
  =/  elm=manx  elm:(static:cram (ream data.raw))
  =/  rec=json  (elem-to-react-json elm)
  %-  pairs:enjs:format
  :~  [%data rec]
      [%meta (meta-to-json meta.raw)]
  ==
::
++  config-to-json
  |=  con=config
  ^-  json
  ?:  =(con *config)
    ~
  %-  pairs:enjs:format
  :~  :-  %full-path
        :-  %a
        %+  turn  (en-beam:format full-path.con)
        |=  a=@ta
        [%s a]
      :-  %name           [%s name.con]
      :-  %desc           [%s description.con]
      :-  %owner          (ship:enjs:format owner.con)
      :-  %date-created   (time:enjs:format date-created.con)
      :-  %last-modified  (time:enjs:format last-modified.con)
      :-  %type           [%s type.con]
      :-  %comments       [%b comments.con]
      :-  %sort-key       ?~(sort-key.con ~ (numb:enjs:format u.sort-key.con))
      :-  %visible        [%b visible.con]
  ==
::
++  meta-to-json
  |=  meta=(map knot cord)
  ^-  json
  %-  pairs:enjs:format
  %+  turn  ~(tap by meta)
  |=  [key=@t val=@t]
  ^-  [@t json]
  [key [%s val]]
::
++  umd-to-front
  |=  u=@t
  ^-  (map knot cord)
  %-  ~(run by inf:(static:cram (ream u)))
  |=  a=dime  ^-  cord
  ?+  (end 3 1 p.a)  (scot a)
    %t  q.a
  ==
--
