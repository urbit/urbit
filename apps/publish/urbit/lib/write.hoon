/-  *write
/+  elem-to-react-json
|%
::
++  front-to-post-info
  |=  fro=(map knot cord)
  ^-  post-info
  =/  got  ~(got by fro)
  ~|  %invalid-frontmatter
  :*  (slav %p (got %creator))
      (got %title)
      (got %collection)
      (got %filename)
      (comment-config (got %comments))
      (slav %da (got %date-created))
      (slav %da (got %last-modified))
      (rash (got %pinned) (fuss %true %false))
  ==
::
++  front-to-comment-info
  |=  fro=(map knot cord)
  ^-  comment-info
  =/  got  ~(got by fro)
  ~|  %invalid-frontmatter
  :*  (slav %p (got %creator))
      (got %collection)
      (got %post)
      (slav %da (got %date-created))
      (slav %da (got %last-modified))
  ==
::
++  collection-info-to-json
  |=  con=collection-info
  ^-  json
  %-  pairs:enjs:format
  :~  :-  %owner          [%s (scot %p owner.con)]
      :-  %title          [%s title.con]
      :-  %comments       [%s comments.con]
      :-  %allow-edit     [%s allow-edit.con]
      :-  %date-created   (time:enjs:format date-created.con)
      :-  %last-modified  (time:enjs:format last-modified.con)
      :-  %filename       [%s filename.con]
  ==
::
++  post-info-to-json
  |=  info=post-info
  ^-  json
  %-  pairs:enjs:format
  :~  :-  %creator        [%s (scot %p creator.info)]
      :-  %title          [%s title.info]
      :-  %comments       [%s comments.info]
      :-  %date-created   (time:enjs:format date-created.info)
      :-  %last-modified  (time:enjs:format last-modified.info)
      :-  %pinned         [%b pinned.info]
      :-  %filename       [%s filename.info]
      :-  %collection     [%s collection.info]
  ==
::
++  comment-info-to-json
  |=  info=comment-info
  ^-  json
  %-  pairs:enjs:format
  :~  :-  %creator        [%s (scot %p creator.info)]
      :-  %date-created   (time:enjs:format date-created.info)
      :-  %last-modified  (time:enjs:format last-modified.info)
      :-  %post       [%s post.info]
      :-  %collection     [%s collection.info]
  ==
::
++  tang-to-json
  |=  tan=tang
  %-  wall:enjs:format
  %-  zing
  %+  turn  tan
  |=  a=tank
  (wash [0 80] a)
::
++  string-to-symbol
  |=  tap=tape
  ^-  @tas
  %-  crip
  %+  turn  tap
  |=  a=@
  ?:  ?|  &((gte a 'a') (lte a 'z'))
          &((gte a '0') (lte a '9'))
      ==
    a
  ?:  &((gte a 'A') (lte a 'Z'))
    (add 32 a)
  '-'
::
++  collection-build-to-json
  |=  bud=(each collection-info tang)
  ^-  json
  ?:  ?=(%.y -.bud)
    (collection-info-to-json +.bud)
  (tang-to-json +.bud)
::
++  post-build-to-json
  |=  bud=(each [post-info manx @t] tang)
  ^-  json
  ?:  ?=(%.y -.bud)
    %-  pairs:enjs:format
    :~  info+(post-info-to-json +<.bud)
        body+(elem-to-react-json +>-.bud)
        raw+[%s +>+.bud]
    ==
  (tang-to-json +.bud)
::
++  comment-build-to-json
  |=  bud=(each (list [comment-info @t]) tang)
  ^-  json
  ?:  ?=(%.y -.bud)
    :-  %a
    %+  turn  p.bud
    |=  [com=comment-info bod=@t]
    ^-  json
    %-  pairs:enjs:format
    :~  info+(comment-info-to-json com)
        body+s+bod
    ==
  (tang-to-json +.bud)
::
++  total-build-to-json
  |=  col=collection
  ^-  json
  %-  pairs:enjs:format
  :~  info+(collection-build-to-json dat.col.col)
  ::
    :+  %posts
      %o
    %+  roll  ~(tap in ~(key by pos.col))
    |=  [post=@tas out=(map @t json)]
    =/  post-build  (~(got by pos.col) post)
    =/  comm-build  (~(got by com.col) post)

    %+  ~(put by out)
      post
    %-  pairs:enjs:format
    :~  post+(post-build-to-json dat.post-build)
        comments+(comment-build-to-json dat.comm-build)
    ==
  ::
    :-  %order
    %-  pairs:enjs:format
    :~  pin+a+(turn pin.order.col |=(s=@tas [%s s]))
        unpin+a+(turn unpin.order.col |=(s=@tas [%s s]))
    ==
  ::
    :-  %contributors
    %-  pairs:enjs:format
    :~  mod+s+mod.contributors.col
        :+  %who
          %a
        %+  turn  ~(tap in who.contributors.col)
        |=  who=@p
        (ship:enjs:format who)
    ==
  ::
    :+  %subscribers
      %a
    %+  turn  ~(tap in subscribers.col)
    |=  who=@p
    ^-  json
    (ship:enjs:format who)
  ==
::
++  state-to-json
  |=  sat=state
  ^-  json
  %-  pairs:enjs:format
  :~  :+  %pubs
        %o
      %+  roll  ~(tap by pubs.sat)
      |=  [[nom=@tas col=collection] out=(map @t json)]
      %+  ~(put by out)
        nom
      (total-build-to-json col)
  ::
      :+  %subs
        %o
      %-  ~(rep by subs.sat)
      |=  $:  [[who=@p nom=@tas] col=collection]
              out=(map @t [%o (map @t json)])
          ==
      =/  shp=@t  (rsh 3 1 (scot %p who))
      ?:  (~(has by out) shp)
        %+  ~(put by out)
          shp
        :-  %o 
        %+  ~(put by +:(~(got by out) shp))
          nom
        (total-build-to-json col)
      %+  ~(put by out)
        shp
      :-  %o
      (my [nom (total-build-to-json col)] ~)
  ::
      :+  %latest
        %a
      %+  turn  latest.sat
      |=  [who=@p coll=@tas post=@tas]
      %-  pairs:enjs:format
      :~  who+(ship:enjs:format who)
          coll+s+coll
          post+s+post
      ==
  ::
      :+  %unread
        %a
      %+  turn  ~(tap in unread.sat)
      |=  [who=@p coll=@tas post=@tas]
      %-  pairs:enjs:format
      :~  who+(ship:enjs:format who)
          coll+s+coll
          post+s+post
      ==
  ::
      :+  %invites
        %a
      %+  turn  ~(tap in invites.sat)
      |=  [[who=@p coll=@tas] title=@t]
      %-  pairs:enjs:format
      :~  who+(ship:enjs:format who)
          coll+s+coll
          title+s+title
      ==
  ==
::
--
