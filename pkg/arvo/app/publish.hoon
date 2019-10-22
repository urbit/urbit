::
::  /app/publish.hoon
::
/-  *publish
/+  *server, *publish
::
/=  index
  /^  $-(json manx)
  /:  /===/app/publish/index  /!noun/
::
/=  js
  /^  octs
  /;  as-octs:mimes:html
  /|  /:  /===/app/publish/js/index  /js/
      /~  ~
  ==
::
/=  css
  /^  octs
  /;  as-octs:mimes:html
  /|  /:  /===/app/publish/css/index  /css/
      /~  ~
  ==
::
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /|  /:  /===/app/publish/js/tile  /js/
      /~  ~
  ==
::
/=  images
  /^  (map knot @)
  /:  /===/app/publish/img  /_  /png/
::
|%
+$  state
  $:  pubs=(map @tas collection)
      subs=(map [ship @tas] collection)
      awaiting=(map @tas [builds=(set wire) partial=(unit delta)])
      latest=(list [who=ship coll=@tas post=@tas])
      unread=(set [who=ship coll=@tas post=@tas])
      invites=(map [who=ship coll=@tas] title=@t)
      outgoing=(map path bone)
  ==
::
+$  move  [bone card]
::
+$  card
  $%  [%info wire toro:clay]
      [%poke wire dock poke]
      [%perm wire desk path rite:clay]
      [%peer wire dock path]
      [%pull wire dock ~]
      [%quit ~]
      [%diff diff]
      [%build wire ? schematic:ford]
      [%kill wire ~]
      [%connect wire binding:eyre term]
      [%http-response http-event:http]
  ==
::
+$  poke
  $%  [%publish-action action]
      [%launch-action @tas path @t]
  ==
::
+$  diff
  $%  [%json json]
      [%publish-collection collection]
      [%publish-rumor rumor]
      [%publish-update update]
      [%export %publish-v0 publish-dir]
  ==
::
--
::
|_  [bol=bowl:gall %0 sat=state]
::
++  this  .
::  +our-beak: beak for this app, with case set to current invocation date
::
++  our-beak  /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)
::  +prep: set up eyre connection and modulo tile; adapt state
::
++  prep
  =>  |%
      ++  states
        $%  [%0 s=state]
        ==
      --
  |=  old=(unit states)
  ^-  (quip move _this)
  ?~  old
    :_  this
    :~  [ost.bol %connect / [~ /'~publish'] %publish]
        :*  ost.bol  %poke  /publish  [our.bol %launch]
            %launch-action  %publish  /publishtile  '/~publish/tile.js'
        ==
    ==
  ?-  -.u.old
    %0  [~ this(sat s.u.old)]
  ==
::
++  ships-to-whom
  |=  ships=(set @p)
  ^-  (set whom:clay)
  %-  ~(run in ships)
  |=  who=@p
  ^-  whom:clay
  [%.y who]
::
++  get-contributors
  |=  coll=@tas
  ^-  [mod=?(%white %black) who=(set @p)]
  =/  pax  (weld our-beak /web/publish/[coll])
  =/  pem=[r=dict:clay w=dict:clay]  .^([dict:clay dict:clay] %cp pax)
  :-  mod.rul.w.pem
  (resolve-real rul.w.pem)
::
++  resolve-real
  |=  rel=real:clay
  ^-  (set @p)
  %-  ~(uni in p.who.rel)
  %-  (set @p)
  %-  ~(rep by q.who.rel)
  |=  [[@ta cru=crew:clay] out=(set @p)]
  ^-  (set @p)
  (~(uni in out) cru)
::
++  whom-to-ships
  |=  whoms=(set whom:clay)
  ^-  (set @p)
  %-  ~(rep in whoms)
  |=  [who=whom:clay out=(set @p)]
  ?:  ?=(%.y -.who)
    (~(put in out) p.who)
  out
::
++  allowed
  |=  [who=@p mod=?(%read %write) pax=path]
  ^-  ?
  =.  pax  (weld our-beak pax)
  =/  pem=[dict:clay dict:clay]  .^([dict:clay dict:clay] %cp pax)
  ?-  mod
    %read   (allowed-by who -.pem)
    %write  (allowed-by who +.pem)
  ==
::  +allowed-by: checks if ship :who is allowed by the permission rules in :dic
::
++  allowed-by
  |=  [who=@p dic=dict:clay]
  ^-  ?
  ?:  =(who our.bol)  &
  =/  in-list=?
    ?|  (~(has in p.who.rul.dic) who)
      ::
        %-  ~(rep by q.who.rul.dic)
        |=  [[@ta cru=crew:clay] out=_|]
        ?:  out  &
        (~(has in cru) who)
    ==
  ?:  =(%black mod.rul.dic)
    !in-list
  in-list
::  +write-file: write file at path
::
++  write-file
  =,  space:userlib
  |=  [pax=path cay=cage]
  ^-  move
  =.  pax  (weld our-beak pax)
  [ost.bol %info (weld /write-file pax) (foal pax cay)]
::
++  delete-file
  =,  space:userlib
  |=  pax=path
  ^-  move
  =.  pax  (weld our-beak pax)
  [ost.bol %info (weld /write-file pax) (fray pax)]
::
++  update-udon-front
  |=  [fro=(map knot cord) udon=@t]
  ^-  @t
  %-  of-wain:format
  =/  tum  (trip udon)
  =/  id  (find ";>" tum)
  ?~  id
    %+  weld  (front-to-wain fro)
    (to-wain:format (crip (weld ";>\0a" tum)))
  %+  weld  (front-to-wain fro)
  (to-wain:format (crip (slag u.id tum)))
::
++  front-to-wain
  |=  a=(map knot cord)
  ^-  wain
  =/  entries=wain
    %+  turn  ~(tap by a)
    |=  b=[knot cord]
    =/  c=[term cord]  (,[term cord] b)
    (crip "  [{<-.c>} {<+.c>}]")
  ::
  ?~  entries  ~
  ;:  weld
    [':-  :~' ~]
    entries
    ['    ==' ~]
  ==
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [~ this]
  ?+  a
    [~ this]
  ::
      %print-bowl
    ~&  bol
    [~ this]
  ::
      %print-state
    ~&  sat
    [~ this]
  ::
      %state-surgery
    =/  pubs=[broken=(list [@p @tas @tas]) new=(map @tas collection)]
      %-  ~(rep by pubs.sat)
      |=  $:  [nom=@tas col=collection]
              broken=(list [@p @tas @tas])
              pubs=(map @tas collection)
          ==
      ^-  [(list [@p @tas @tas]) (map @tas collection)]
      ::
      =/  bad-posts=(list [@p @tas @tas])
        %-  ~(rep by pos.col)
        |=  $:  [pos=@tas dat=[=bone dat=(each [post-info manx @t] tang)]]
                broken=(list [@p @tas @tas])
            ==
        ^-  (list [@p @tas @tas])
        ?:  -.dat.dat
          broken
        [[our.bol nom pos] broken]
      ::
      =.  pin.order.col
        %+  skip  pin.order.col
        |=  pos=@tas
        ^-  ?
        ?~  (find [our.bol nom pos]~ bad-posts)
          %.n
        %.y
      ::
      =.  unpin.order.col
        %+  skip  unpin.order.col
        |=  pos=@tas
        ^-  ?
        ?~  (find [our.bol nom pos]~ bad-posts)
          %.n
        %.y
      ::
      [(welp broken bad-posts) (~(put by pubs) nom col)]
    ::
    =/  subs=[broken=(list [@p @tas @tas]) new=(map [@p @tas] collection)]
      %-  ~(rep by subs.sat)
      |=  $:  [[who=@p nom=@tas] col=collection]
              broken=(list [@p @tas @tas])
              subs=(map [@p @tas] collection)
          ==
      ^-  [(list [@p @tas @tas]) (map [@p @tas] collection)]
      ::
      =/  bad-posts=(list [@p @tas @tas])
        %-  ~(rep by pos.col)
        |=  $:  [pos=@tas dat=[=bone dat=(each [post-info manx @t] tang)]]
                broken=(list [@p @tas @tas])
            ==
        ^-  (list [@p @tas @tas])
        ?:  -.dat.dat
          broken
        [[who nom pos] broken]
      ::
      ::
      =.  pin.order.col
        %+  skip  pin.order.col
        |=  pos=@tas
        ?~  (find [who nom pos]~ bad-posts)
          %.n
        %.y
      ::
      =.  unpin.order.col
        %+  skip  unpin.order.col
        |=  pos=@tas
        ?~  (find [who nom pos]~ bad-posts)
          %.n
        %.y
      ::
      [(welp broken bad-posts) (~(put by subs) [who nom] col)]
    ::
    =/  new-latest=(list [@p @tas @tas])
      %+  skip  latest.sat
      |=  elm=[@p @tas @tas]
      ^-  ?
      ?^  (find [elm]~ broken.pubs)
        %.y
      ?^  (find [elm]~ broken.subs)
        %.y
      %.n
    ::
    =/  new-unread=(set [@p @tas @tas])
      %-  sy
      %+  skip  ~(tap in unread.sat)
      |=  elm=[@p @tas @tas]
      ^-  ?
      ?^  (find [elm]~ broken.pubs)
        %.y
      ?^  (find [elm]~ broken.subs)
        %.y
      %.n
    ::
    =/  mow=(list move)
      %-  ~(rep by new.pubs)
      |=  [[nom=@tas col=collection] out=(list move)]
      ^-  (list move)
      =/  del=delta  [%total our.bol nom col]
      (welp (affection del) out)
    ::
    :-  mow
    %=  this
      latest.sat    new-latest
      unread.sat    new-unread
      pubs.sat      new.pubs
      subs.sat      new.subs
    ==
  ::
  ==
::
++  da
  |_  moves=(list move)
  ::
  ++  da-this  .
  ::
  ++  da-done
    ^-  (quip move _this)
    [(flop moves) this]
  ::
  ++  da-emit
    |=  mov=move
    %_  da-this
      moves  [mov moves]
    ==
  ::
  ++  da-emil
    |=  mov=(list move)
    %_  da-this
      moves  (welp (flop mov) moves)
    ==
  ::
  ++  da-change
    |=  del=delta
    ^+  da-this
    ?-  -.del
    ::
        %collection
      =/  old=(unit collection)
        ?:  =(our.bol who.del)
          (~(get by pubs.sat) col.del)
        (~(get by subs.sat) who.del col.del)
      =/  new=collection
        ?~  old
          [[ost.bol dat.del] ~ ~ [~ ~] [%white ~] ~ now.bol]
        u.old(col [ost.bol dat.del], last-update now.bol)
      =?  contributors.new  =(our.bol who.del)
        (get-contributors col.del)
      =?  pubs.sat  =(our.bol who.del)
        (~(put by pubs.sat) col.del new)
      =?  subs.sat  !=(our.bol who.del)
        (~(put by subs.sat) [who.del col.del] new)
      (da-emil (affection del))
    ::
        %post
      =/  old=(unit collection)
        ?:  =(our.bol who.del)
          (~(get by pubs.sat) col.del)
        (~(get by subs.sat) who.del col.del)
      =/  new=collection
        ?~  old
          :*  [0 %.n ~]  (my [pos.del ost.bol dat.del] ~)  ~
              [~ ~]  [%white ~]  ~  now.bol
          ==
        %=  u.old
          pos          (~(put by pos.u.old) pos.del [ost.bol dat.del])
          last-update  now.bol
        ==
      =?  pubs.sat  =(our.bol who.del)
        (~(put by pubs.sat) col.del new)
      =?  subs.sat  !=(our.bol who.del)
        (~(put by subs.sat) [who.del col.del] new)
      =.  da-this
        ?:  -.dat.del
          (da-insert who.del col.del pos.del)
        (da-remove who.del col.del pos.del)
      (da-emil (affection del))
    ::
        %comments
      =/  old=(unit collection)
        ?:  =(our.bol who.del)
          (~(get by pubs.sat) col.del)
        (~(get by subs.sat) who.del col.del)
      =/  new=collection
        ?~  old
          :*  [0 %.n ~]  ~  (my [pos.del ost.bol dat.del] ~)
              [~ ~]  [%white ~]  ~  now.bol
          ==
        %=  u.old
          com          (~(put by com.u.old) pos.del [ost.bol dat.del])
          last-update  now.bol
        ==
      =?  pubs.sat  =(our.bol who.del)
        (~(put by pubs.sat) col.del new)
      =?  subs.sat  !=(our.bol who.del)
        (~(put by subs.sat) [who.del col.del] new)
      (da-emil (affection del))
    ::
        %total
      =?  contributors.dat.del  =(our.bol who.del)
        (get-contributors col.del)
      =?  pubs.sat  =(our.bol who.del)
        (~(put by pubs.sat) col.del dat.del)
      =?  subs.sat  !=(our.bol who.del)
        (~(put by subs.sat) [who.del col.del] dat.del(order [~ ~]))
      ::
      =/  posts=(list [@tas bone (each [post-info manx @t] tang)])
        ~(tap by pos.dat.del)
      =.  da-this
      |-
      ?~  posts
        da-this
      ?.  +>-.i.posts
        %=  $
          da-this  (da-remove who.del col.del -.i.posts)
          posts    t.posts
        ==
      %=  $
        da-this  (da-insert who.del col.del -.i.posts)
        posts    t.posts
      ==
      (da-emil (affection del))
    ::
        %remove
      ::  remove blog
      ::
      ?~  pos.del
        ::  collect post ids for blog, delete blog, and sent out moves
        ::
        =^  posts  da-this
          ?:  =(our.bol who.del)
            ::  if its our blog, we must send out notifications to subscribers
            ::
            =/  old=(unit collection)  (~(get by pubs.sat) col.del)
            ?~  old
              [~ da-this]
            =.  pubs.sat  (~(del by pubs.sat) col.del)
            :-  ~(tap in ~(key by pos.u.old))
            (da-emil (affection del))
          ::  if its not our blog, we need to pull subscription
          ::
          =/  old=(unit collection)  (~(get by subs.sat) who.del col.del)
          ?~  old
            [~ da-this]
          =.  subs.sat  (~(del by subs.sat) who.del col.del)
          :-  ~(tap in ~(key by pos.u.old))
          %-  da-emil
          :-  [ost.bol %pull /collection/[col.del] [who.del %publish] ~]
          (affection-primary del)
        ::  iterate through post ids collected before, removing each from
        ::  secondary indices in state
        ::
        =.  da-this
        |-
        ?~  posts
          da-this
        %=  $
          da-this  (da-remove who.del col.del i.posts)
          posts    t.posts
        ==
        da-this
      ::  remove post
      ::
      =/  old=(unit collection)
        ?:  =(our.bol who.del)
          (~(get by pubs.sat) col.del)
        (~(get by subs.sat) who.del col.del)
      ?~  old
        da-this
      ?.  (~(has in ~(key by pos.u.old)) u.pos.del)
        da-this
      =/  new=collection
        %=  u.old
          pos  (~(del by pos.u.old) u.pos.del)
          com  (~(del by com.u.old) u.pos.del)
        ==
      =.  da-this  (da-emil (affection del))
      ?:  =(our.bol who.del)
        =.  pubs.sat  (~(put by pubs.sat) col.del new)
        =.  da-this  (da-remove who.del col.del u.pos.del)
        (da-emil (affection del))
      =.  subs.sat  (~(put by subs.sat) [who.del col.del] new)
      =.  da-this  (da-remove who.del col.del u.pos.del)
      (da-emil (affection-primary del))
    ::
    ==
  ::
  ++  da-remove-unread
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    =.  unread.sat  (~(del in unread.sat) who coll post)
    (da-emil make-tile-moves)
  ::
  ++  da-remove-latest
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    =/  ids=(list @)  (fand [who coll post]~ latest.sat)
    =.  latest.sat
    |-
    ?~  ids
      latest.sat
    %=  $
      latest.sat  (oust [i.ids 1] latest.sat)
      ids  t.ids
    ==
    (da-emil make-tile-moves)
  ::
  ++  da-remove-order
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    =/  col=(unit collection)  (get-coll-by-index who coll)
    ?~  col
      da-this
    =/  new=collection  u.col
    =/  pin-ids=(list @)  (fand [post]~ pin.order.new)
    =.  pin.order.new
    |-
    ?~  pin-ids
      pin.order.new
    %=  $
      pin.order.new  (oust [i.pin-ids 1] pin.order.new)
      pin-ids  t.pin-ids
    ==
    ::
    =/  unpin-ids=(list @)  (fand [post]~ unpin.order.new)
    =.  unpin.order.new
    |-
    ?~  unpin-ids
      unpin.order.new
    %=  $
      unpin.order.new  (oust [i.unpin-ids 1] unpin.order.new)
      unpin-ids  t.unpin-ids
    ==
    =?  pubs.sat  =(who our.bol)
      (~(put by pubs.sat) coll new)
    =?  subs.sat  !=(who our.bol)
      (~(put by subs.sat) [who coll] new)
    (da-emil make-tile-moves)
  ::
  ++  da-remove
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    =.  da-this  (da-remove-unread +<)
    =.  da-this  (da-remove-latest +<)
    =.  da-this  (da-remove-order +<)
    da-this
  ::
  ++  da-insert-unread
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    ::  assume we've read our own posts
    ::
    =?  unread.sat  !=(who our.bol)
      (~(put in unread.sat) who coll post)
    (da-emil make-tile-moves)
  ::
  ++  da-insert-latest
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    =/  new-date=@da  date-created:(need (get-post-info-by-index who coll post))
    =/  pre=(list [@p @tas @tas])  ~
    =/  suf=(list [@p @tas @tas])  latest.sat
    =?  latest.sat  =(~ (find [who coll post]~ latest.sat))
      |-
      ?~  suf
        (weld pre [who coll post]~)
      =/  i-date=@da  date-created:(need (get-post-info-by-index i.suf))
      ?:  (gte new-date i-date)
        (weld pre [[who coll post] suf])
      %=  $
        suf  t.suf
        pre  (snoc pre i.suf)
      ==
    da-this
  ::
  ++  da-insert-order
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    =/  new-post=post-info  (need (get-post-info-by-index who coll post))
    =/  col=collection  (need (get-coll-by-index who coll))
    ::
    =/  pre=(list @tas)  ~
    =/  suf=(list @tas)
      ?:  pinned.new-post
        pin.order.col
      unpin.order.col
    ::
    ?:  ?=(^ (find [post]~ suf))
      da-this
    =/  new-list=(list @tas)
    |-
    ?~  suf
      (snoc pre post)
    ?:  =(post i.suf)
      (weld pre suf)
    =/  i-date=@da  date-created:(need (get-post-info-by-index who coll i.suf))
    ?:  (gte date-created.new-post i-date)
      (weld pre [post suf])
    %=  $
      suf  t.suf
      pre  (snoc pre i.suf)
    ==
    ::
    =.  order.col
      ?:  pinned.new-post
        [new-list unpin.order.col]
      [pin.order.col new-list]
    ::
    =?  pubs.sat  =(our.bol who)
      (~(put by pubs.sat) coll col)
    =?  subs.sat  !=(our.bol who)
      (~(put by subs.sat) [who coll] col)
    da-this
  ::
  ++  da-insert
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    =.  da-this  (da-insert-unread +<)
    =.  da-this  (da-insert-latest +<)
    =.  da-this  (da-insert-order +<)
    da-this
  --
::  +bake: apply delta
::
++  bake
  |=  del=delta
  ^-  (quip move _this)
  da-done:(da-change:da del)
::  +affection: rumors to primary
::
++  affection-primary
  |=  del=delta
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib /primary bol)
  |=  [b=bone *]
  ^-  move
  [b %diff %publish-rumor del]
::  +affection: rumors to interested
::
++  affection
  |=  del=delta
  ^-  (list move)
  %-  zing
  %+  turn  ~(tap by sup.bol)
  |=  [b=bone s=ship p=path]
  ^-  (list move)
  =/  rum=(unit rumor)  (feel p del)
  ?~  rum
    ~
  [b %diff %publish-rumor u.rum]~
::  +feel: delta to rumor
::
++  feel
  |=  [query=wire del=delta]
  ^-  (unit rumor)
  ?+  query
    ~
      [%primary ~]
    [~ del]
  ::
      [%collection @t ~]
    =/  coll=@tas  i.t.query
    ?:  =(coll col.del)
      [~ del]
    ~
  ::
  ==
::
++  get-post-by-index
  |=  [who=@p coll=@tas post=@tas]
  ^-  (unit (each [post-info manx @t] tang))
  =/  col=(unit collection)
    ?:  =(our.bol who)
      (~(get by pubs.sat) coll)
    (~(get by subs.sat) who coll)
  ?~  col  ~
  =/  pos=(unit [bone (each [post-info manx @t] tang)])
    (~(get by pos.u.col) post)
  ?~  pos  ~
  [~ +.u.pos]
::
++  get-post-info-by-index
  |=  [who=@p coll=@tas post=@tas]
  ^-  (unit post-info)
  =/  col=(unit collection)
    ?:  =(our.bol who)
      (~(get by pubs.sat) coll)
    (~(get by subs.sat) who coll)
  ?~  col  ~
  =/  pos=(unit [bone (each [post-info manx @t] tang)])
    (~(get by pos.u.col) post)
  ?~  pos  ~
  ?:  ?=(%.n -.+.u.pos)  ~
  [~ -.p.+.u.pos]
::
++  get-coll-by-index
  |=  [who=@p coll=@tas]
  ^-  (unit collection)
  ?:  =(our.bol who)
    (~(get by pubs.sat) coll)
  (~(get by subs.sat) who coll)
::
++  made
  |=  [wir=wire wen=@da mad=made-result:ford]
  ^-  (quip move _this)
  ?+  wir
    [~ this]
  ::
      [%collection @t ~]
    =/  col=@tas  i.t.wir
    =/  awa  (~(get by awaiting.sat) col)
    ::
    =/  dat=(each collection-info tang)
      ?:  ?=([%incomplete *] mad)
        [%.n tang.mad]
      ?:  ?=([%error *] build-result.mad)
        [%.n message.build-result.mad]
      ?>  ?=(%bake +<.build-result.mad)
      ?>  ?=(%publish-info p.cage.build-result.mad)
      [%.y (collection-info q.q.cage.build-result.mad)]
    ::
    ?~  awa
      (bake [%collection our.bol col dat])
    =.  builds.u.awa  (~(del in builds.u.awa) wir)
    ?~  partial.u.awa
      ?~  builds.u.awa
        ::  one-off build, make delta and process it
        ::
        =.  awaiting.sat  (~(del by awaiting.sat) col)
        (bake [%collection our.bol col dat])
      ::  1st part of multi-part, store partial delta and don't process it
      ::
      =/  del=delta
        :*  %total  our.bol  col  [ost.bol dat]
            ~  ~  [~ ~]  [%white ~]  ~  now.bol
        ==
      =.  awaiting.sat  (~(put by awaiting.sat) col builds.u.awa `del)
      [~ this]
    ::
    ?~  builds.u.awa
      ::  last part of multipart, update partial delta and process it
      ::
      ?>  ?=(%total -.u.partial.u.awa)
      =/  del=delta
        :*  %total
            our.bol
            col
            [ost.bol dat]
            pos.dat.u.partial.u.awa
            com.dat.u.partial.u.awa
            [~ ~]
            [%white ~]
            ~
            now.bol
        ==
      =.  awaiting.sat  (~(del by awaiting.sat) col)
      (bake del)
    ::  nth part of multi-part, update partial delta and don't process it
    ::
    ?>  ?=(%total -.u.partial.u.awa)
    =/  del=delta
      :*  %total
          our.bol
          col
          [ost.bol dat]
          pos.dat.u.partial.u.awa
          com.dat.u.partial.u.awa
          [~ ~]
          [%white ~]
          ~
          now.bol
      ==
    =.  awaiting.sat  (~(put by awaiting.sat) col builds.u.awa `del)
    [~ this]
  ::
      [%post @t @t ~]
    =/  col=@tas  i.t.wir
    =/  pos=@tas  i.t.t.wir
    =/  awa  (~(get by awaiting.sat) col)
    ::
    =/  dat=(each [post-info manx @t] tang)
      ?:  ?=([%incomplete *] mad)
        [%.n tang.mad]
      ?:  ?=([%error *] build-result.mad)
        [%.n message.build-result.mad]
      ?>  ?=(%bake +<.build-result.mad)
      ?>  ?=(%publish-post p.cage.build-result.mad)
      [%.y (,[post-info manx @t] q.q.cage.build-result.mad)]
    ::
    ?~  awa
      (bake [%post our.bol col pos dat])
    =.  builds.u.awa  (~(del in builds.u.awa) wir)
    ?~  partial.u.awa
      ?~  builds.u.awa
        ::  one-off build, make delta and process it
        ::
        =.  awaiting.sat  (~(del by awaiting.sat) col)
        (bake [%post our.bol col pos dat])
      ::  1st part of multi-part, store partial delta and don't process it
      ::
      =/  del=delta
        :*  %total  our.bol  col  [0 %.n ~]  (my [pos ost.bol dat] ~)
            ~  [~ ~]  [%white ~]  ~  now.bol
        ==
      =.  awaiting.sat  (~(put by awaiting.sat) col builds.u.awa `del)
      [~ this]
    ::
    ?~  builds.u.awa
      ::  last part of multipart, update partial delta and process it
      ::
      ?>  ?=(%total -.u.partial.u.awa)
      =/  del=delta
        :*  %total
            our.bol
            col
            col.dat.u.partial.u.awa
            (~(put by pos.dat.u.partial.u.awa) pos [ost.bol dat])
            com.dat.u.partial.u.awa
            [~ ~]
            [%white ~]
            ~
            now.bol
        ==
      =.  awaiting.sat  (~(del by awaiting.sat) col)
      (bake del)
    ::  nth part of multi-part, update partial delta and don't process it
    ::
    ?>  ?=(%total -.u.partial.u.awa)
    =/  del=delta
      :*  %total
          our.bol
          col
          col.dat.u.partial.u.awa
          (~(put by pos.dat.u.partial.u.awa) pos [ost.bol dat])
          com.dat.u.partial.u.awa
          [~ ~]
          [%white ~]
          ~
          now.bol
      ==
    =.  awaiting.sat  (~(put by awaiting.sat) col builds.u.awa `del)
    [~ this]
  ::
      [%comments @t @t ~]
    =/  col=@tas  i.t.wir
    =/  pos=@tas  i.t.t.wir
    =/  awa  (~(get by awaiting.sat) col)
    ::
    =/  dat=(each (list [comment-info @t]) tang)
      ?:  ?=([%incomplete *] mad)
        [%.n tang.mad]
      ?:  ?=([%error *] build-result.mad)
        [%.n message.build-result.mad]
      ?>  ?=(%bake +<.build-result.mad)
      ?>  ?=(%publish-comments p.cage.build-result.mad)
      [%.y (,(list [comment-info @t]) q.q.cage.build-result.mad)]
    ::
    ?~  awa
      (bake [%comments our.bol col pos dat])
    =.  builds.u.awa  (~(del in builds.u.awa) wir)
    ?~  partial.u.awa
      ?~  builds.u.awa
        ::  one-off build, make delta and process it
        ::
        =.  awaiting.sat  (~(del by awaiting.sat) col)
        (bake [%comments our.bol col pos dat])
      ::  1st part of multi-part, store partial delta and don't process it
      ::
      =/  del=delta
        :*  %total  our.bol  col  [0 %.n ~]  ~  (my [pos ost.bol dat] ~)
            [~ ~]  [%white ~]  ~  now.bol
        ==
      =.  awaiting.sat  (~(put by awaiting.sat) col builds.u.awa `del)
      [~ this]
    ::
    ?~  builds.u.awa
      ::  last part of multipart, update partial delta and process it
      ::
      ?>  ?=(%total -.u.partial.u.awa)
      =/  del=delta
        :*  %total
            our.bol
            col
            col.dat.u.partial.u.awa
            pos.dat.u.partial.u.awa
            (~(put by com.dat.u.partial.u.awa) pos [ost.bol dat])
            [~ ~]
            [%white ~]
            ~
            now.bol
        ==
      =.  awaiting.sat  (~(del by awaiting.sat) col)
      (bake del)
    ::  nth part of multi-part, update partial delta and don't process it
    ::
    ?>  ?=(%total -.u.partial.u.awa)
    =/  del=delta
      :*  %total
          our.bol
          col
          col.dat.u.partial.u.awa
          pos.dat.u.partial.u.awa
          (~(put by com.dat.u.partial.u.awa) pos [ost.bol dat])
          [~ ~]
          [%white ~]
          ~
          now.bol
      ==
    =.  awaiting.sat  (~(put by awaiting.sat) col builds.u.awa `del)
    [~ this]
  ==
::
++  make-kills
  |=  [coll=@tas post=(unit @tas)]
  ^-  (list move)
  =/  col=(unit collection)  (~(get by pubs.sat) coll)
  ?~  col
    ~|  [%non-existent-collection coll]  !!
  ?~  post
    =/  kills=(list move)
      %+  roll  ~(tap by pos.u.col)
      |=  [[pos=@tas b=bone *] out=(list move)]
      :*  [b %kill /post/[coll]/[pos] ~]
          [b %kill /comments/[coll]/[pos] ~]
          out
      ==
    [[bone.col.u.col %kill /collection/[coll] ~] kills]
  ::
  =/  pos-bone  bone:(~(got by pos.u.col) u.post)
  =/  com-bone  bone:(~(got by com.u.col) u.post)
  :~  [pos-bone %kill /post/[coll]/[u.post] ~]
      [com-bone %kill /comments/[coll]/[u.post] ~]
  ==
::
++  make-deletes
  |=  [coll=@tas post=(unit @tas)]
  ^-  (list move)
  =/  files=(list path)
    ?~  post
      .^((list path) %ct (weld our-beak /web/publish/[coll]))
    .^((list path) %ct (weld our-beak /web/publish/[coll]/[u.post]))
  %+  turn  files
  |=  pax=path
  ^-  move
  (delete-file pax)
::
++  mack
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  %-  (slog u.err)
  [~ this]
::
++  poke-publish-action
  |=  act=action
  ^-  (quip move _this)
  ?-  -.act
  ::
      %new-collection
    ?.  =(our.bol src.bol)
      ::  no one else is permitted to create blogs
      ::
      [~ this]
    ?:  (~(has by pubs.sat) name.act)
      [~ this]
    ::
    =/  conf=collection-info
      :*  our.bol
          title.act
          name.act
          com.act
          edit.act
          now.bol
          now.bol
      ==
    =/  pax=path  /web/publish/[name.act]/publish-info
    =/  blog-perms=card
      :*  %perm  /perms  q.byk.bol
          /web/publish/[name.act]
          %rw  `read.perm.act  `write.perm.act
      ==
    =/  info-perms=card
      :*  %perm  /perms  q.byk.bol
          /web/publish/[name.act]/publish-info
          %rw  `*rule:clay  `*rule:clay
      ==
    ::
    =/  wir=wire  /collection/[name.act]
    =/  schema=schematic:ford
      :*  %bake
          %publish-info
          *coin
          [[our.bol q.byk.bol] /[name.act]/publish/web]
      ==
    :_  this
    :~  (write-file pax %publish-info !>(conf))
        [ost.bol blog-perms]
        [ost.bol info-perms]
        [ost.bol %build wir %.y schema]
    ==
  ::
      %new-post
    ?.  =(who.act our.bol)
      :_  this
      [ost.bol %poke /forward [who.act %publish] %publish-action act]~
    =/  pax=path  /web/publish/[coll.act]/[name.act]/udon
    ?.  (allowed src.bol %write pax)
      [~ this]
    =/  col=(unit collection)  (~(get by pubs.sat) coll.act)
    ?~  col
      [~ this]
    ?:  (~(has by pos.u.col) name.act)
      [~ this]
    =.  content.act  (cat 3 content.act '\0a')  :: XX fix udon parser
    =/  front=(map knot cord)
      %-  my
      :~  [%creator (scot %p src.bol)]
          [%title title.act]
          [%collection coll.act]
          [%filename name.act]
          [%comments com.act]
          [%date-created (scot %da now.bol)]
          [%last-modified (scot %da now.bol)]
          [%pinned %false]
      ==
    =/  out=@t  (update-udon-front front content.act)
    ::
    =/  post-wir=wire  /post/[coll.act]/[name.act]
    =/  post-schema=schematic:ford
      :*  %bake
          %publish-post
          *coin
          [[our.bol q.byk.bol] /[name.act]/[coll.act]/publish/web]
      ==
    ::
    =/  comments-wir=wire  /comments/[coll.act]/[name.act]
    =/  comments-schema=schematic:ford
      :*  %bake
          %publish-comments
          *coin
          [[our.bol q.byk.bol] /[name.act]/[coll.act]/publish/web]
      ==
    ::
    =/  post-perms=card
      :*  %perm  /perms  q.byk.bol
          /web/publish/[coll.act]/[name.act]/udon
          %w  `[%white (ships-to-whom (sy src.bol ~))]
      ==
    =/  comment-perms=card
      :*  %perm  /perms  q.byk.bol
          /web/publish/[coll.act]/[name.act]
          %w  `[%black ~]
      ==
    :_  this
    :~  (write-file pax %udon !>(out))
        [ost.bol post-perms]
        [ost.bol comment-perms]
        [ost.bol %build comments-wir %.y comments-schema]
        [ost.bol %build post-wir %.y post-schema]
    ==
  ::
      %new-comment
    ?.  =(who.act our.bol)
      :_  this
      [ost.bol %poke /forward [who.act %publish] %publish-action act]~
    =/  pax=path
      /web/publish/[coll.act]/[post.act]/(scot %da now.bol)/publish-comment
    ?.  (allowed src.bol %write pax)
      [~ this]
    =/  col=(unit collection)  (~(get by pubs.sat) coll.act)
    ?~  col
      [~ this]
    ?.  (~(has by pos.u.col) post.act)
      [~ this]

    =/  com=comment
      [[src.bol coll.act post.act now.bol now.bol] content.act]
    ::
    =/  comment-perms=card
      :*  %perm  /perms  q.byk.bol  pax
          %w  `[%white ~]
      ==
    ::
    :_  this
    :~  (write-file pax %publish-comment !>(com))
        [ost.bol comment-perms]
    ==
  ::
      %delete-collection
    ?.  =(src.bol our.bol)
      [~ this]
    =/  kills    (make-kills coll.act ~)
    =/  deletes  (make-deletes coll.act ~)
    =/  del=delta  [%remove our.bol coll.act ~]
    =^  moves  this  (bake del)
    ::
    :-
    ;:  welp
      kills
      moves
      make-tile-moves
      deletes
    ==
    %=  this
      awaiting.sat  (~(del by awaiting.sat) coll.act)
    ==
  ::
      %delete-post
    ?.  =(src.bol our.bol)
      [~ this]
    =/  kills    (make-kills coll.act `post.act)
    =/  deletes  (make-deletes coll.act `post.act)
    =/  del=delta  [%remove our.bol coll.act `post.act]
    =^  moves  this  (bake del)
    ::
    :_  this
    ;:  welp
      kills
      moves
      make-tile-moves
      deletes
    ==
  ::
      %delete-comment
    ?.  =(src.bol our.bol)
      [~ this]
    :_  this
    [(delete-file /web/publish/[coll.act]/[post.act]/[comment.act]/udon)]~
  ::
      %edit-collection
    ?.  =(src.bol our.bol)
      [~ this]
    =/  pax=path  /web/publish/[name.act]/publish-info
    =/  col=(unit collection)  (~(get by pubs.sat) name.act)
    ?~  col
      [~ this]
    ?:  ?=(%.n -.dat.col.u.col)
      [~ this]
    =/  out=collection-info  p.dat.col.u.col(title title.act)
    :_  this
    [(write-file pax %publish-info !>(out))]~
  ::
      %edit-post
    ?.  =(who.act our.bol)
      :_  this
      [ost.bol %poke /forward [who.act %publish] %publish-action act]~
    ::
    =/  pax=path  /web/publish/[coll.act]/[name.act]/udon
    ?.  (allowed src.bol %write pax)
      [~ this]
    =/  col=(unit collection)  (~(get by pubs.sat) coll.act)
    ?~  col
      [~ this]
    ?.  (~(has by pos.u.col) name.act)
      [~ this]
    ::
    =/  pos=(unit (each [post-info manx @t] tang))
      (get-post-by-index who.act coll.act name.act)
    ?~  pos
      ~|  %editing-non-existent-post  !!
    =/  date-created=@da
      ?:  ?=(%.y -.u.pos)
        date-created.-.p.u.pos
      now.bol
    ::
    =.  content.act  (cat 3 content.act '\0a')  :: XX fix udon parser
    =/  front=(map knot cord)
      %-  my
      :~  [%creator (scot %p src.bol)]
          [%title title.act]
          [%collection coll.act]
          [%filename name.act]
          [%comments com.act]
          [%date-created (scot %da date-created)]
          [%last-modified (scot %da now.bol)]
          [%pinned %false]
      ==
    =/  out=@t  (update-udon-front front content.act)
    ::
    :_  this
    [(write-file pax %udon !>(out))]~
  ::
  ::  %invite: if the action is from us it means send invites to other people
  ::           if its from someone else it means we've been invited
  ::
      %invite
    ?:  =(our.bol src.bol)
      =/  new-act=action  [%invite coll.act title.act ~]
      :_  this
      %+  turn  who.act
      |=  who=@p
      ^-  move
      [ost.bol %poke /forward [who %publish] %publish-action new-act]
    =.  invites.sat  (~(put by invites.sat) [src.bol coll.act] title.act)
    :_  this
    %+  welp  make-tile-moves
    ::
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [b=bone *]
    [b %diff %publish-update %invite %.y src.bol coll.act title.act]
  ::
  ::  %reject-invite: remove invite from list, acceptance is handled by
  ::                  %subscribe action
  ::
      %reject-invite
    =/  title=(unit @t)   (~(get by invites.sat) [who.act coll.act])
    ?~  title
      [~ this]
    =.  invites.sat  (~(del by invites.sat) [who.act coll.act])
    :_  this
    %+  welp  make-tile-moves
    ::
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [b=bone *]
    ^-  move
    [b %diff %publish-update %invite %.n who.act coll.act u.title]
  ::
  ::  %serve:
  ::
      %serve
    :: XX specialize this check for subfiles
    ?.  =(our.bol src.bol)
      [~ this]
    ?:  (~(has by pubs.sat) coll.act)
      [~ this]
    =/  files=(list path)
      .^((list path) %ct (weld our-beak /web/publish/[coll.act]))
    ?>  ?=(^ (find [/web/publish/[coll.act]/publish-info]~ files))
    =/  all=[moves=(list move) builds=(set wire)]
    %+  roll  files
    |=  [pax=path out=[moves=(list move) builds=(set wire)]]
    ?+  pax
      out
    ::
      [%web %publish @tas %publish-info ~]
      ?>  =(coll.act i.t.t.pax)
      =/  wir=wire  /collection/[coll.act]
      =/  schema=schematic:ford
        :*  %bake
            %publish-info
            *coin
            [[our.bol q.byk.bol] /[coll.act]/publish/web]
        ==
      =/  blog-perms=card
        :*  %perm  /perms  q.byk.bol
            /web/publish/[coll.act]
            %rw  `[%black ~]  `[%white (ships-to-whom (sy our.bol ~))]
        ==
      =/  info-perms=card
        :*  %perm  /perms  q.byk.bol
            /web/publish/[coll.act]/publish-info
            %rw  `*rule:clay  `*rule:clay
        ==
      %=  out
        builds  (~(put in builds.out) wir)
      ::
          moves
        :*  [ost.bol %build wir %.y schema]
            [ost.bol blog-perms]
            [ost.bol info-perms]
            moves.out
        ==
      ==
    ::
      [%web %publish @tas @tas %udon ~]
      ?>  =(coll.act i.t.t.pax)
      =/  post  i.t.t.t.pax
      =/  post-wir=wire  /post/[coll.act]/[post]
      =/  post-schema=schematic:ford
        :*  %bake
            %publish-post
            *coin
            [[our.bol q.byk.bol] /[post]/[coll.act]/publish/web]
        ==
      ::
      =/  comments-wir=wire  /comments/[coll.act]/[post]
      =/  comments-schema=schematic:ford
        :*  %bake
            %publish-comments
            *coin
            [[our.bol q.byk.bol] /[post]/[coll.act]/publish/web]
        ==
      =/  post-perms=card
        :*  %perm  /perms  q.byk.bol
            /web/publish/[coll.act]/[post]/udon
            %w  `[%white (ships-to-whom (sy our.bol ~))]
        ==
      =/  comment-perms=card
        :*  %perm  /perms  q.byk.bol
            /web/publish/[coll.act]/[post]
            %w  `[%black ~]
        ==
      %=    out
          moves
        :*  [ost.bol %build post-wir %.y post-schema]
            [ost.bol %build comments-wir %.y comments-schema]
            [ost.bol post-perms]
            [ost.bol comment-perms]
            moves.out
        ==
      ::
          builds
        (~(uni in builds.out) (sy post-wir comments-wir ~))
      ==
    ::
    ==
    :-  moves.all
    %=  this
      awaiting.sat  (~(put by awaiting.sat) coll.act builds.all ~)
    ==
  ::
  ::  %unserve:
  ::
      %unserve
    ::  XX  pull subscriptions for unserved collections
    ::
    ?.  =(our.bol src.bol)
      [~ this]
    =/  kills  (make-kills coll.act ~)
    =/  del=delta  [%remove our.bol coll.act ~]
    =^  moves  this  (bake del)
    ::
    :-
    ;:  welp
      moves
      make-tile-moves
      kills
    ==
    %=  this
      awaiting.sat  (~(del by awaiting.sat) coll.act)
    ==
  ::
  ::  %subscribe: sub to a foreign blog; remove invites for that blog
  ::
      %subscribe
    =/  wir=wire  /collection/[coll.act]
    =/  title=(unit @t)  (~(get by invites.sat) [who.act coll.act])
    =.  invites.sat  (~(del by invites.sat) [who.act coll.act])
    :_  this(outgoing.sat (~(put by outgoing.sat) wir ost.bol))
    ;:  welp
      make-tile-moves
      [ost.bol %peer wir [who.act %publish] wir]~
      ?~  title  ~
      %+  turn  (prey:pubsub:userlib /primary bol)
      |=  [b=bone *]
      ^-  move
      [b %diff %publish-update %invite %.n who.act coll.act u.title]
    ==
  ::
  ::  %unsubscribe: unsub from a foreign blog, delete all state related to it
  ::
      %unsubscribe
    =/  wir=wire  /collection/[coll.act]
    =/  bon=(unit bone)  (~(get by outgoing.sat) wir)
    ?~  bon
      ~|  %nonexistent-subscription^wir  !!
    =/  new-latest=(list [@p @tas @tas])
      %+  skim  latest.sat
      |=  [who=@p coll=@tas post=@tas]
      ?&  =(who our.bol)
          =(coll coll.act)
      ==
    ::
    =.  unread.sat
      ^-  (set [@p @tas @tas])
      %-  sy
      %+  skim  ~(tap in unread.sat)
      |=  [who=@p coll=@tas post=@tas]
      ?&  =(who our.bol)
          =(coll coll.act)
      ==
    :_  %=  this
      subs.sat      (~(del by subs.sat) who.act coll.act)
      latest.sat    new-latest
      outgoing.sat  (~(del by outgoing.sat) wir)
    ==
    :-  [u.bon %pull wir [who.act %publish] ~]
    %+  welp  make-tile-moves
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [b=bone *]
    ^-  move
    [b %diff %publish-rumor %remove who.act coll.act ~]
  ::
  ::  %read: notify that we've seen a post
  ::
      %read
    =.  unread.sat  (~(del in unread.sat) who.act coll.act post.act)
    :_  this
    %+  welp  make-tile-moves
    ::
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [b=bone *]
    ^-  move
    [b %diff %publish-update %unread %.n (sy [who.act coll.act post.act] ~)]
  ::
  ==
::
++  bound
  |=  [wir=wire success=? binding=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
::  +poke-handle-http-request: received on a new connection established
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  ::
  =/  request-line  (parse-request-line url.request.inbound-request)
  ?+  request-line
    :_  this
    [ost.bol %http-response not-found:app]~
  ::  images
  ::
      [[[~ %png] [%'~publish' @t ~]] ~]
    =/  filename=@t  i.t.site.request-line
    =/  img=(unit @t)  (~(get by images) filename)
    ?~  img
      :_  this
      [ost.bol %http-response not-found:app]~
    :_  this
    [ost.bol %http-response (png-response:app (as-octs:mimes:html u.img))]~
  ::  styling
  ::
      [[[~ %css] [%'~publish' %index ~]] ~]
    :_  this
    [ost.bol %http-response (css-response:app css)]~
  ::  scripting
  ::
      [[[~ %js] [%'~publish' %index ~]] ~]
    :_  this
    [ost.bol %http-response (js-response:app js)]~
  ::  tile js
  ::
      [[[~ %js] [%'~publish' %tile ~]] ~]
    :_  this
    [ost.bol %http-response (js-response:app tile-js)]~
  ::  home page; redirect to recent
  ::
      [[~ [%'~publish' ~]] ~]
    =/  hym=manx  (index (state-to-json sat))
    :_  this
    [ost.bol %http-response (redirect:app '/~publish/recent')]~
  ::  recent page
  ::
      [[~ [%'~publish' %recent ~]] ~]
    =/  hym=manx  (index (state-to-json sat))
    :_  this
    [ost.bol %http-response (manx-response:app hym)]~
  ::  subscriptions
  ::
      [[~ [%'~publish' %subs ~]] ~]
    =/  hym=manx  (index (state-to-json sat))
    :_  this
    [ost.bol %http-response (manx-response:app hym)]~
  ::  published
  ::
      [[~ [%'~publish' %pubs ~]] ~]
    =/  hym=manx  (index (state-to-json sat))
    :_  this
    [ost.bol %http-response (manx-response:app hym)]~
  ::  new post
  ::
      [[~ [%'~publish' %new-post ~]] ~]
    =/  hym=manx  (index (state-to-json sat))
    :_  this
    [ost.bol %http-response (manx-response:app hym)]~
  ::  new blog
  ::
      [[~ [%'~publish' %new-blog ~]] ~]
    =/  hym=manx  (index (state-to-json sat))
    :_  this
    [ost.bol %http-response (manx-response:app hym)]~
  ::  blog
  ::
      [[~ [%'~publish' @t @t ~]] ~]
    =/  who=(unit @p)  (slaw %p i.t.site.request-line)
    =/  blog=@tas      i.t.t.site.request-line
    =/  hym=manx  (index (state-to-json sat))
    :_  this
    [ost.bol %http-response (manx-response:app hym)]~
  ::  blog post
  ::
      [[~ [%'~publish' @t @t @t ~]] ~]
    =/  who=(unit @p)  (slaw %p i.t.site.request-line)
    =/  blog=@tas      i.t.t.site.request-line
    =/  post=@tas      i.t.t.t.site.request-line
    ::
    =/  hym=manx  (index (state-to-json sat))
    :_  this
    [ost.bol %http-response (manx-response:app hym)]~
  ::
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
++  make-tile-moves
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib /publishtile bol)
  |=  [b=bone *]
  ^-  move
  [b %diff %json make-tile-json]
::
++  make-tile-json
  ^-  json
  %-  pairs:enjs:format
  :~  invites+(numb:enjs:format ~(wyt by invites.sat))
      new+(numb:enjs:format ~(wyt in unread.sat))
  ==
::
++  poke-import
  |=  i=*
  ^-  (quip move _this)
  ?>  ?=([%publish-v0 *] i)
  =/  dir=publish-dir  ;;(publish-dir +.i)
  ::  make moves to save all files to clay, and
  ::  make moves to call %serve for each collection
  ::
  =/  out=[mow=(list move) sob=soba:clay]
    %+  roll  ~(tap by dir)
    |=  [[pax=path fil=publish-file] mow=(list move) sob=soba:clay]
    =/  mis=miso:clay
      (feel:space:userlib (weld our-beak pax) -.fil !>(+.fil))
    ?+  pax
      [mow sob]
    ::
        [%web %publish * %publish-info ~]
      =/  col=@tas  &3.pax
      =/  wir=wire  /collection/[col]
      =/  schema=schematic:ford
        :*  %bake
            %publish-info
            *coin
            [[our.bol q.byk.bol] /[col]/publish/web]
        ==
      =/  blog-perms=card
        :*  %perm  /perms  q.byk.bol
            /web/publish/[col]
            %rw  `[%black ~]  `[%white (ships-to-whom (sy our.bol ~))]
        ==
      =/  info-perms=card
        :*  %perm  /perms  q.byk.bol
            /web/publish/[col]/publish-info
            %rw  `*rule:clay  `*rule:clay
        ==
      :-  :*  [ost.bol %build wir %.y schema]
              [ost.bol blog-perms]
              [ost.bol info-perms]
              mow
          ==
      [[pax mis] sob]
    ::
        [%web %publish * * %udon ~]
      =/  col=@tas  &3.pax
      =/  pos=@tas  &4.pax
      =/  post-wir=wire  /post/[col]/[pos]
      =/  post-schema=schematic:ford
        :*  %bake
            %publish-post
            *coin
            [[our.bol q.byk.bol] /[pos]/[col]/publish/web]
        ==
      =/  comment-wir=wire  /comments/[col]/[pos]
      =/  comment-schema=schematic:ford
        :*  %bake
            %publish-comments
            *coin
            [[our.bol q.byk.bol] /[pos]/[col]/publish/web]
        ==
      =/  post-perms=card
        :*  %perm  /perms  q.byk.bol
            /web/publish/[col]/[pos]/udon
            %w  `[%white (ships-to-whom (sy our.bol ~))]
        ==
      =/  comment-perms=card
        :*  %perm  /perms  q.byk.bol
            /web/publish/[col]/[pos]
            %w  `[%black ~]
        ==
      :-  :*  [ost.bol %build post-wir %.y post-schema]
              [ost.bol %build comment-wir %.y comment-schema]
              [ost.bol post-perms]
              [ost.bol comment-perms]
              mow
          ==
      [[pax mis] sob]
    ::
        [%web %publish * * * %publish-comment ~]
      :-  mow
      [[pax mis] sob]
    ::
    ==
  ::
  =/  tor=toro:clay
    [q.byk.bol %.y sob.out]
  :_  this
  [[ost.bol %info /import tor] mow.out]
::
++  peer-export
  |=  pax=path
  ^-  (quip move _this)
  =/  pal=(list path)  .^((list path) %ct (weld our-beak /web/publish))
  ::
  =/  dir=publish-dir
  %+  roll  pal
  |=  [pax=path out=publish-dir]
  ^-  publish-dir
  ?+  pax
    out
  ::
      [%web %publish * %publish-info ~]
    =/  fil=collection-info  .^(collection-info %cx (welp our-beak pax))
    (~(put by out) pax [%publish-info fil])
  ::
      [%web %publish * * %udon ~]
    =/  fil=@t  .^(@t %cx (welp our-beak pax))
    (~(put by out) pax [%udon fil])
  ::
      [%web %publish * * * %publish-comment ~]
    =/  fil=comment  .^(comment %cx (welp our-beak pax))
    (~(put by out) pax [%publish-comment fil])
  ==
  ::
  :_  this
  [ost.bol %diff %export %publish-v0 dir]~
::
++  peer-publishtile
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  [ost.bol %diff %json make-tile-json]~
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  ?.  =(our.bol src.bol)
    ::  only we are allowed to subscribe on primary
    ::
    :_  this
    [ost.bol %quit ~]~
  [~ this]
::
++  pull
  |=  wir=wire
  ^-  (quip move _this)
  ?+  wir
    [~ this]
  ::
      [%collection @t ~]
    =/  coll=@tas  i.t.wir
    =/  col=(unit collection)  (~(get by pubs.sat) coll)
    ?~  col
      [~ this]
    =/  new=collection
      u.col(subscribers (~(del in subscribers.u.col) src.bol))
    [~ this(pubs.sat (~(put by pubs.sat) coll new))]
  ::
  ==
::
++  peer-collection
  |=  wir=wire
  ^-  (quip move _this)
  ?.  ?=([@tas ~] wir)
    [~ this]
  =/  coll=@tas  i.wir
  =/  pax  /web/publish/[coll]
  ?.  (allowed src.bol %read pax)
    :_  this
    [ost.bol %quit ~]~
  ::
  =/  col=(unit collection)  (~(get by pubs.sat) coll)
  ?~  col
    :_  this
    [ost.bol %quit ~]~
  =/  new=collection
    u.col(subscribers (~(put in subscribers.u.col) src.bol))
  =/  rum=rumor
    [%total our.bol coll new]
  :_  this(pubs.sat (~(put by pubs.sat) coll new))
  [ost.bol %diff %publish-rumor rum]~
::
++  diff-publish-rumor
  |=  [wir=wire rum=rumor]
  ^-  (quip move _this)
  (bake rum)
::  +poke-handle-http-cancel: received when a connection was killed
::
++  poke-handle-http-cancel
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  [~ this]
::
--
