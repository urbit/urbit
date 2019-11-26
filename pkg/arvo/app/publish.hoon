::
::  /app/publish.hoon
::
/-  *publish
/+  *server, *publish, default-agent, verb
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
!:
|%
::
+$  versioned-state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  pubs=(map @tas collection)
      subs=(map [ship @tas] collection)
      awaiting=(map @tas [builds=(set wire) partial=(unit delta)])
      latest=(list [who=ship coll=@tas post=@tas])
      unread=(set [who=ship coll=@tas post=@tas])
      invites=(map [who=ship coll=@tas] title=@t)
  ==
::
+$  card  card:agent:gall
::
--
::
=|  state-zero
=*  state  -
^-  agent:gall
=<
  %+  verb  |
  |_  bol=bowl:gall
  +*  this      .
      pub-core  +>
      pc        ~(. pub-core bol)
      def       ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    :_  this
    :~  [%pass /bind/publish %arvo %e %connect [~ /'~publish'] %publish]
        :*  %pass  /launch/publish  %agent  [our.bol %launch]  %poke
            %launch-action  !>([%publish /publishtile '/~publish/tile.js'])
        ==
    ==
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    `this(state !<(state-zero old))
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+    mark  (on-poke:def mark vase)
          %noun
        (poke-noun:pc !<(* vase))
          %publish-action
        (poke-publish-action:pc !<(action vase))
          %handle-http-request
        =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
        :_  state
        %+  give-simple-payload:app  eyre-id
        %+  require-authorization:app  inbound-request
        poke-handle-http-request:pc
          %import
        (poke-import:pc !<(* vase))
          %handle-http-cancel
        [~ state]
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      ?+  path  (on-watch:def path)
        [%export *]         (peer-export:pc t.path)
        [%publishtile *]    (peer-publishtile:pc t.path)
        [%primary *]        (peer-primary:pc t.path)
        [%collection *]     (peer-collection:pc t.path)
        [%http-response *]  [~ state]
      ==
    [cards this]
  ::
  ++  on-leave
    |=  =wire
    ^-  (quip card _this)
    =^  cards  state
      (pull:pc wire)
    [cards this]
  ::
  ++  on-peek  on-peek:def
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      =^  cards  state
        (reap:pc wire p.sign)
      [cards this]
    ::
        %kick
      ?.  ?=([%collection *] wire)
        (on-agent:def wire sign)
      =^  cards  state
        (quit-collection:pc t.wire)
      [cards this]
    ::
        %fact
      ?.  ?=(%publish-rumor p.cage.sign)
        (on-agent:def wire sign)
      =^  cards  state
        (bake:pc !<(rumor q.cage.sign))
      [cards this]
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+    -.sign-arvo  (on-arvo:def wire sign-arvo)
    ::
        %e
      ?:  ?=(%bound +<.sign-arvo)
        [~ this]
      (on-arvo:def wire sign-arvo)
    ::
        %f
      ?.  ?=(%made +<.sign-arvo)
        (on-arvo:def wire sign-arvo)
      =^  cards  state
        (made:pc wire date.sign-arvo result.sign-arvo)
      [cards this]
    ::
        %c
      ?.  ?=(%done +<.sign-arvo)
        (on-arvo:def wire sign-arvo)
      ?~  error.sign-arvo
        [~ this]
      ((slog tang.u.error.sign-arvo) [~ this])
    ==
  ::
  ++  on-fail  on-fail:def
  --
::
|_  bol=bowl:gall
::  +our-beak: beak for this app, with case set to current invocation date
::
++  our-beak  /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)
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
  ^-  card 
  =.  pax  (weld our-beak pax)
  [%pass (weld /write-file pax) %arvo %c %info (foal pax cay)]
::
++  delete-file
  =,  space:userlib
  |=  pax=path
  ^-  card
  =.  pax  (weld our-beak pax)
  [%pass (weld /remove-file pax) %arvo %c %info (fray pax)]
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
  ^-  (quip card _state)
  ?.  =(src.bol our.bol)
    [~ state]
  ?+  a
    [~ state]
  ::
      %print-bowl
    ~&  bol
    [~ state]
  ::
      %print-state
    ~&  state
    [~ state]
  ::
      %state-surgery
    =/  pubs=[broken=(list [@p @tas @tas]) new=(map @tas collection)]
      %-  ~(rep by pubs)
      |=  $:  [nom=@tas col=collection]
              broken=(list [@p @tas @tas])
              pubs=(map @tas collection)
          ==
      ^-  [(list [@p @tas @tas]) (map @tas collection)]
      ::
      =/  bad-posts=(list [@p @tas @tas])
        %-  ~(rep by pos.col)
        |=  $:  [pos=@tas dat=(each [post-info manx @t] tang)]
                broken=(list [@p @tas @tas])
            ==
        ^-  (list [@p @tas @tas])
        ?:  -.dat
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
      %-  ~(rep by subs)
      |=  $:  [[who=@p nom=@tas] col=collection]
              broken=(list [@p @tas @tas])
              subs=(map [@p @tas] collection)
          ==
      ^-  [(list [@p @tas @tas]) (map [@p @tas] collection)]
      ::
      =/  bad-posts=(list [@p @tas @tas])
        %-  ~(rep by pos.col)
        |=  $:  [pos=@tas dat=(each [post-info manx @t] tang)]
                broken=(list [@p @tas @tas])
            ==
        ^-  (list [@p @tas @tas])
        ?:  -.dat
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
      %+  skip  latest
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
      %+  skip  ~(tap in unread)
      |=  elm=[@p @tas @tas]
      ^-  ?
      ?^  (find [elm]~ broken.pubs)
        %.y
      ?^  (find [elm]~ broken.subs)
        %.y
      %.n
    ::
    =/  mow=(list card)
      %-  ~(rep by new.pubs)
      |=  [[nom=@tas col=collection] out=(list card)]
      ^-  (list card)
      =/  del=delta  [%total our.bol nom col]
      (welp (affection del) out)
    ::
    :-  mow
    %=  state
      latest  new-latest
      unread  new-unread
      pubs    new.pubs
      subs    new.subs
    ==
  ::
  ==
::
++  da
  |_  moves=(list card)
  ::
  ++  da-this  .
  ::
  ++  da-done
    ^-  (quip card _state)
    [(flop moves) state]
  ::
  ++  da-emit
    |=  mov=card
    %_  da-this
      moves  [mov moves]
    ==
  ::
  ++  da-emil
    |=  mov=(list card)
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
          (~(get by pubs) col.del)
        (~(get by subs) who.del col.del)
      =/  new=collection
        ?~  old
          [dat.del ~ ~ [~ ~] [%white ~] ~ now.bol]
        u.old(col dat.del, last-update now.bol)
      =?  contributors.new  =(our.bol who.del)
        (get-contributors col.del)
      =?  pubs  =(our.bol who.del)
        (~(put by pubs) col.del new)
      =?  subs  !=(our.bol who.del)
        (~(put by subs) [who.del col.del] new)
      (da-emil (affection del))
    ::
        %post
      =/  old=(unit collection)
        ?:  =(our.bol who.del)
          (~(get by pubs) col.del)
        (~(get by subs) who.del col.del)
      =/  new=collection
        ?~  old
          :*  [%.n ~]  (my [pos.del dat.del] ~)  ~
              [~ ~]  [%white ~]  ~  now.bol
          ==
        %=  u.old
          pos          (~(put by pos.u.old) pos.del dat.del)
          last-update  now.bol
        ==
      =?  pubs  =(our.bol who.del)
        (~(put by pubs) col.del new)
      =?  subs  !=(our.bol who.del)
        (~(put by subs) [who.del col.del] new)
      =.  da-this
        ?:  -.dat.del
          (da-insert who.del col.del pos.del)
        (da-remove who.del col.del pos.del)
      (da-emil (affection del))
    ::
        %comments
      =/  old=(unit collection)
        ?:  =(our.bol who.del)
          (~(get by pubs) col.del)
        (~(get by subs) who.del col.del)
      =/  new=collection
        ?~  old
          :*  [%.n ~]  ~  (my [pos.del dat.del] ~)
              [~ ~]  [%white ~]  ~  now.bol
          ==
        %=  u.old
          com          (~(put by com.u.old) pos.del dat.del)
          last-update  now.bol
        ==
      =?  pubs  =(our.bol who.del)
        (~(put by pubs) col.del new)
      =?  subs  !=(our.bol who.del)
        (~(put by subs) [who.del col.del] new)
      (da-emil (affection del))
    ::
        %total
      =?  contributors.dat.del  =(our.bol who.del)
        (get-contributors col.del)
      =?  pubs  =(our.bol who.del)
        (~(put by pubs) col.del dat.del)
      =?  subs  !=(our.bol who.del)
        (~(put by subs) [who.del col.del] dat.del(order [~ ~]))
      ::
      =/  posts=(list [@tas (each [post-info manx @t] tang)])
        ~(tap by pos.dat.del)
      =.  da-this
      |-
      ?~  posts
        da-this
      ?.  +<.i.posts
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
            =/  old=(unit collection)  (~(get by pubs) col.del)
            ?~  old
              [~ da-this]
            =.  pubs  (~(del by pubs) col.del)
            :-  ~(tap in ~(key by pos.u.old))
            (da-emil (affection del))
          ::  if its not our blog, we need to pull subscription
          ::
          =/  old=(unit collection)  (~(get by subs) who.del col.del)
          ?~  old
            [~ da-this]
          =.  subs  (~(del by subs) who.del col.del)
          :-  ~(tap in ~(key by pos.u.old))
          %-  da-emil
          :-  [%pass /collection/[col.del] %agent [who.del %publish] %leave ~]
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
          (~(get by pubs) col.del)
        (~(get by subs) who.del col.del)
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
        =.  pubs  (~(put by pubs) col.del new)
        =.  da-this  (da-remove who.del col.del u.pos.del)
        (da-emil (affection del))
      =.  subs  (~(put by subs) [who.del col.del] new)
      =.  da-this  (da-remove who.del col.del u.pos.del)
      (da-emil (affection-primary del))
    ::
    ==
  ::
  ++  da-remove-unread
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    =.  unread  (~(del in unread) who coll post)
    (da-emil make-tile-moves)
  ::
  ++  da-remove-latest
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    =/  ids=(list @)  (fand [who coll post]~ latest)
    =.  latest
    |-
    ?~  ids
      latest
    %=  $
      latest  (oust [i.ids 1] latest)
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
    =?  pubs  =(who our.bol)
      (~(put by pubs) coll new)
    =?  subs  !=(who our.bol)
      (~(put by subs) [who coll] new)
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
    =?  unread  !=(who our.bol)
      (~(put in unread) who coll post)
    (da-emil make-tile-moves)
  ::
  ++  da-insert-latest
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    =/  new-date=@da  date-created:(need (get-post-info-by-index who coll post))
    =/  pre=(list [@p @tas @tas])  ~
    =/  suf=(list [@p @tas @tas])  latest
    =?  latest  =(~ (find [who coll post]~ latest))
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
    =?  pubs  =(our.bol who)
      (~(put by pubs) coll col)
    =?  subs  !=(our.bol who)
      (~(put by subs) [who coll] col)
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
  ^-  (quip card _state)
  da-done:(da-change:da del)
::  +affection: rumors to primary
::
++  affection-primary
  |=  del=delta
  ^-  (list card)
  [%give %fact `/primary %publish-rumor !>(del)]~
::  +affection: rumors to interested
::
++  affection
  |=  del=delta
  ^-  (list card)
  =/  wir=wire  /collection/[col.del]
  :~  [%give %fact `/primary %publish-rumor !>(del)]
      [%give %fact `wir %publish-rumor !>(del)]
  ==
::
++  get-post-by-index
  |=  [who=@p coll=@tas post=@tas]
  ^-  (unit (each [post-info manx @t] tang))
  =/  col=(unit collection)
    ?:  =(our.bol who)
      (~(get by pubs) coll)
    (~(get by subs) who coll)
  ?~  col  ~
  =/  pos=(unit (each [post-info manx @t] tang))
    (~(get by pos.u.col) post)
  pos
::
++  get-post-info-by-index
  |=  [who=@p coll=@tas post=@tas]
  ^-  (unit post-info)
  =/  col=(unit collection)
    ?:  =(our.bol who)
      (~(get by pubs) coll)
    (~(get by subs) who coll)
  ?~  col  ~
  =/  pos=(unit (each [post-info manx @t] tang))
    (~(get by pos.u.col) post)
  ?~  pos  ~
  ?:  ?=(%.n -.u.pos)  ~
  [~ -.p.u.pos]
::
++  get-coll-by-index
  |=  [who=@p coll=@tas]
  ^-  (unit collection)
  ?:  =(our.bol who)
    (~(get by pubs) coll)
  (~(get by subs) who coll)
::
++  made
  |=  [wir=wire wen=@da mad=made-result:ford]
  ^-  (quip card _state)
  ?+  wir
    [~ state]
  ::
      [%collection @t ~]
    =/  col=@tas  i.t.wir
    =/  awa  (~(get by awaiting) col)
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
        =.  awaiting  (~(del by awaiting) col)
        (bake [%collection our.bol col dat])
      ::  1st part of multi-part, store partial delta and don't process it
      ::
      =/  del=delta
        :*  %total  our.bol  col  dat
            ~  ~  [~ ~]  [%white ~]  ~  now.bol
        ==
      =.  awaiting  (~(put by awaiting) col builds.u.awa `del)
      [~ state]
    ::
    ?~  builds.u.awa
      ::  last part of multipart, update partial delta and process it
      ::
      ?>  ?=(%total -.u.partial.u.awa)
      =/  del=delta
        :*  %total
            our.bol
            col
            dat
            pos.dat.u.partial.u.awa
            com.dat.u.partial.u.awa
            [~ ~]
            [%white ~]
            ~
            now.bol
        ==
      =.  awaiting  (~(del by awaiting) col)
      (bake del)
    ::  nth part of multi-part, update partial delta and don't process it
    ::
    ?>  ?=(%total -.u.partial.u.awa)
    =/  del=delta
      :*  %total
          our.bol
          col
          dat
          pos.dat.u.partial.u.awa
          com.dat.u.partial.u.awa
          [~ ~]
          [%white ~]
          ~
          now.bol
      ==
    =.  awaiting  (~(put by awaiting) col builds.u.awa `del)
    [~ state]
  ::
      [%post @t @t ~]
    =/  col=@tas  i.t.wir
    =/  pos=@tas  i.t.t.wir
    =/  awa  (~(get by awaiting) col)
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
        =.  awaiting  (~(del by awaiting) col)
        (bake [%post our.bol col pos dat])
      ::  1st part of multi-part, store partial delta and don't process it
      ::
      =/  del=delta
        :*  %total  our.bol  col  [%.n ~]  (my [pos dat] ~)
            ~  [~ ~]  [%white ~]  ~  now.bol
        ==
      =.  awaiting  (~(put by awaiting) col builds.u.awa `del)
      [~ state]
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
            (~(put by pos.dat.u.partial.u.awa) pos dat)
            com.dat.u.partial.u.awa
            [~ ~]
            [%white ~]
            ~
            now.bol
        ==
      =.  awaiting  (~(del by awaiting) col)
      (bake del)
    ::  nth part of multi-part, update partial delta and don't process it
    ::
    ?>  ?=(%total -.u.partial.u.awa)
    =/  del=delta
      :*  %total
          our.bol
          col
          col.dat.u.partial.u.awa
          (~(put by pos.dat.u.partial.u.awa) pos dat)
          com.dat.u.partial.u.awa
          [~ ~]
          [%white ~]
          ~
          now.bol
      ==
    =.  awaiting  (~(put by awaiting) col builds.u.awa `del)
    [~ state]
  ::
      [%comments @t @t ~]
    =/  col=@tas  i.t.wir
    =/  pos=@tas  i.t.t.wir
    =/  awa  (~(get by awaiting) col)
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
        =.  awaiting  (~(del by awaiting) col)
        (bake [%comments our.bol col pos dat])
      ::  1st part of multi-part, store partial delta and don't process it
      ::
      =/  del=delta
        :*  %total  our.bol  col  [%.n ~]  ~  (my [pos dat] ~)
            [~ ~]  [%white ~]  ~  now.bol
        ==
      =.  awaiting  (~(put by awaiting) col builds.u.awa `del)
      [~ state]
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
            (~(put by com.dat.u.partial.u.awa) pos dat)
            [~ ~]
            [%white ~]
            ~
            now.bol
        ==
      =.  awaiting  (~(del by awaiting) col)
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
          (~(put by com.dat.u.partial.u.awa) pos dat)
          [~ ~]
          [%white ~]
          ~
          now.bol
      ==
    =.  awaiting  (~(put by awaiting) col builds.u.awa `del)
    [~ state]
  ==
::
++  make-kills
  |=  [coll=@tas post=(unit @tas)]
  ^-  (list card)
  =/  col=(unit collection)  (~(get by pubs) coll)
  ?~  col
    ~|  [%non-existent-collection coll]  !!
  ?~  post
    =/  kills=(list card)
      %+  roll  ~(tap by pos.u.col)
      |=  [[pos=@tas *] out=(list card)]
      :*  [%pass /post/[coll]/[pos] %arvo %f %kill ~]
          [%pass /comments/[coll]/[pos] %arvo %f %kill ~]
          out
      ==
    [[%pass /collection/[coll] %arvo %f %kill ~] kills]
  ::
  :~  [%pass /post/[coll]/[u.post] %arvo %f %kill ~]
      [%pass /comments/[coll]/[u.post] %arvo %f %kill ~]
  ==
::
++  make-deletes
  |=  [coll=@tas post=(unit @tas)]
  ^-  (list card)
  =/  files=(list path)
    ?~  post
      .^((list path) %ct (weld our-beak /web/publish/[coll]))
    .^((list path) %ct (weld our-beak /web/publish/[coll]/[u.post]))
  %+  turn  files
  |=  pax=path
  ^-  card
  (delete-file pax)
::
++  mack
  |=  [wir=wire err=(unit tang)]
  ^-  (quip card _state)
  ?~  err
    [~ state]
  %-  (slog u.err)
  [~ state]
::
++  poke-publish-action
  |=  act=action
  ^-  (quip card _state)
  ?-  -.act
  ::
      %new-collection
    ?.  =(our.bol src.bol)
      ::  no one else is permitted to create blogs
      ::
      [~ state]
    ?:  (~(has by pubs) name.act)
      [~ state]
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
    ::
    =/  blog-perms=task:able:clay
      :*  %perm  q.byk.bol
          /web/publish/[name.act]
          %rw  `read.perm.act  `write.perm.act
      ==
    =/  info-perms=task:able:clay
      :*  %perm  q.byk.bol
          /web/publish/[name.act]/publish-info
          %rw  `*rule:clay  `*rule:clay
      ==
    =/  schema=schematic:ford
      :*  %bake
          %publish-info
          *coin
          [[our.bol q.byk.bol] /[name.act]/publish/web]
      ==
    =/  pax=path  /web/publish/[name.act]/publish-info
    :_  state
    :~  (write-file pax %publish-info !>(conf))
        [%pass /perms %arvo %c blog-perms]
        [%pass /perms %arvo %c info-perms]
        [%pass /collection/[name.act] %arvo %f %build %.y schema]
    ==
  ::
      %new-post
    ?.  =(who.act our.bol)
      :_  state
      [%pass /forward %agent [who.act %publish] %poke %publish-action !>(act)]~
    =/  pax=path  /web/publish/[coll.act]/[name.act]/udon
    ?.  (allowed src.bol %write pax)
      [~ state]
    =/  col=(unit collection)  (~(get by pubs) coll.act)
    ?~  col
      [~ state]
    ?:  (~(has by pos.u.col) name.act)
      [~ state]
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
    =/  post-perms=task:able:clay
      :*  %perm  q.byk.bol
          /web/publish/[coll.act]/[name.act]/udon
          %w  `[%white (ships-to-whom (sy src.bol ~))]
      ==
    =/  comment-perms=task:able:clay
      :*  %perm  q.byk.bol
          /web/publish/[coll.act]/[name.act]
          %w  `[%black ~]
      ==
    :_  state
    :~  (write-file pax %udon !>(out))
        [%pass /perms %arvo %c post-perms]
        [%pass /perms %arvo %c comment-perms]
        [%pass comments-wir %arvo %f %build %.y comments-schema]
        [%pass post-wir %arvo %f %build %.y post-schema]
    ==
  ::
      %new-comment
    ?.  =(who.act our.bol)
      :_  state
      [%pass /forward %agent [who.act %publish] %poke %publish-action !>(act)]~
    =/  pax=path
      /web/publish/[coll.act]/[post.act]/(scot %da now.bol)/publish-comment
    ?.  (allowed src.bol %write pax)
      [~ state]
    =/  col=(unit collection)  (~(get by pubs) coll.act)
    ?~  col
      [~ state]
    ?.  (~(has by pos.u.col) post.act)
      [~ state]
    =/  com=comment
      [[src.bol coll.act post.act now.bol now.bol] content.act]
    ::
    =/  comment-perms=task:able:clay  [%perm q.byk.bol pax %w `[%white ~]]
    ::
    :_  state
    :~  (write-file pax %publish-comment !>(com))
        [%pass /perms %arvo %c comment-perms]
    ==
  ::
      %delete-collection
    ?.  =(src.bol our.bol)
      [~ state]
    =/  kills    (make-kills coll.act ~)
    =/  deletes  (make-deletes coll.act ~)
    =/  del=delta  [%remove our.bol coll.act ~]
    =^  moves  state  (bake del)
    ::
    :-
    ;:  welp
      kills
      moves
      make-tile-moves
      deletes
    ==
    %=  state
      awaiting  (~(del by awaiting) coll.act)
    ==
  ::
      %delete-post
    ?.  =(src.bol our.bol)
      [~ state]
    =/  kills    (make-kills coll.act `post.act)
    =/  deletes  (make-deletes coll.act `post.act)
    =/  del=delta  [%remove our.bol coll.act `post.act]
    =^  moves  state  (bake del)
    ::
    :_  state
    ;:  welp
      kills
      moves
      make-tile-moves
      deletes
    ==
  ::
      %delete-comment
    ?.  =(src.bol our.bol)
      [~ state]
    :_  state
    [(delete-file /web/publish/[coll.act]/[post.act]/[comment.act]/udon)]~
  ::
      %edit-collection
    ?.  =(src.bol our.bol)
      [~ state]
    =/  pax=path  /web/publish/[name.act]/publish-info
    =/  col=(unit collection)  (~(get by pubs) name.act)
    ?~  col
      [~ state]
    ?:  ?=(%.n -.col.u.col)
      [~ state]
    =/  out=collection-info  p.col.u.col(title title.act)
    :_  state
    [(write-file pax %publish-info !>(out))]~
  ::
      %edit-post
    ?.  =(who.act our.bol)
      :_  state
      [%pass /forward %agent [who.act %publish] %poke %publish-action !>(act)]~
    ::
    =/  pax=path  /web/publish/[coll.act]/[name.act]/udon
    ?.  (allowed src.bol %write pax)
      [~ state]
    =/  col=(unit collection)  (~(get by pubs) coll.act)
    ?~  col
      [~ state]
    ?.  (~(has by pos.u.col) name.act)
      [~ state]
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
    :_  state
    [(write-file pax %udon !>(out))]~
  ::
  ::  %invite: if the action is from us it means send invites to other people
  ::           if its from someone else it means we've been invited
  ::
      %invite
    ?:  =(our.bol src.bol)
      =/  new-act=action  [%invite coll.act title.act ~]
      :_  state
      %+  turn  who.act
      |=  who=@p
      ^-  card
      [%pass /forward %agent [who %publish] %poke %publish-action !>(new-act)]
    =.  invites  (~(put by invites) [src.bol coll.act] title.act)
    =/  upd=update  [%invite %.y src.bol coll.act title.act]
    :_  state
    %+  welp  make-tile-moves
    [%give %fact `/primary %publish-update !>(upd)]~
  ::
  ::  %reject-invite: remove invite from list, acceptance is handled by
  ::                  %subscribe action
  ::
      %reject-invite
    =/  title=(unit @t)   (~(get by invites) [who.act coll.act])
    ?~  title
      [~ state]
    =.  invites  (~(del by invites) [who.act coll.act])
    =/  upd=update  [%invite %.n who.act coll.act u.title]
    :_  state
    %+  welp  make-tile-moves
    [%give %fact `/primary %publish-update !>(upd)]~
  ::
  ::  %serve:
  ::
      %serve
    :: XX specialize this check for subfiles
    ?.  =(our.bol src.bol)
      [~ state]
    ?:  (~(has by pubs) coll.act)
      [~ state]
    =/  files=(list path)
      .^((list path) %ct (weld our-beak /web/publish/[coll.act]))
    ?>  ?=(^ (find [/web/publish/[coll.act]/publish-info]~ files))
    =/  all=[moves=(list card) builds=(set wire)]
    %+  roll  files
    |=  [pax=path out=[moves=(list card) builds=(set wire)]]
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
      %=  out
        builds  (~(put in builds.out) wir)
      ::
          moves
        :*  [%pass wir %arvo %f %build %.y schema]
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
      %=    out
          moves
        :*  [%pass post-wir %arvo %f %build %.y post-schema]
            [%pass comments-wir %arvo %f %build %.y comments-schema]
            moves.out
        ==
      ::
          builds
        (~(uni in builds.out) (sy post-wir comments-wir ~))
      ==
    ::
    ==
    :-  moves.all
    %=  state
      awaiting  (~(put by awaiting) coll.act builds.all ~)
    ==
  ::
  ::  %unserve:
  ::
      %unserve
    ::  XX  pull subscriptions for unserved collections
    ::
    ?.  =(our.bol src.bol)
      [~ state]
    =/  kills  (make-kills coll.act ~)
    =/  del=delta  [%remove our.bol coll.act ~]
    =^  moves  state  (bake del)
    ::
    :-
    ;:  welp
      moves
      make-tile-moves
      kills
    ==
    %=  state
      awaiting  (~(del by awaiting) coll.act)
    ==
  ::
  ::  %subscribe: sub to a foreign blog; remove invites for that blog
  ::
      %subscribe
    =/  wir=wire  /collection/[coll.act]
    =/  title=(unit @t)  (~(get by invites) [who.act coll.act])
    =.  invites  (~(del by invites) [who.act coll.act])
    :_  state
    ;:  welp
      make-tile-moves
      [%pass wir %agent [who.act %publish] %watch wir]~
      ?~  title  ~
      =/  upd=update  [%invite %.n who.act coll.act u.title]
      [%give %fact `/primary %publish-update !>(upd)]~
    ==
  ::
  ::  %unsubscribe: unsub from a foreign blog, delete all state related to it
  ::
      %unsubscribe
    =/  wir=wire  /collection/[coll.act]
    =/  new-latest=(list [@p @tas @tas])
      %+  skim  latest
      |=  [who=@p coll=@tas post=@tas]
      ?&  =(who our.bol)
          =(coll coll.act)
      ==
    ::
    =.  unread
      ^-  (set [@p @tas @tas])
      %-  sy
      %+  skim  ~(tap in unread)
      |=  [who=@p coll=@tas post=@tas]
      ?&  =(who our.bol)
          =(coll coll.act)
      ==
    :_  %=  state
      subs      (~(del by subs) who.act coll.act)
      latest    new-latest
    ==
    :-  [%pass wir %agent [who.act %publish] %leave ~]
    %+  welp  make-tile-moves
    =/  rum=rumor  [%remove who.act coll.act ~]
    [%give %fact `/primary %publish-rumor !>(rum)]~
  ::
  ::  %read: notify that we've seen a post
  ::
      %read
    =.  unread  (~(del in unread) who.act coll.act post.act)
    :_  state
    %+  welp  make-tile-moves
    ::
    =/  upd=update  [%unread %.n (sy [who.act coll.act post.act] ~)]
    [%give %fact `/primary %publish-update !>(upd)]~
  ::
  ==
::
++  quit-collection
  |=  wir=wire
  ^-  (quip card _state)
  =/  pax=path  (weld /collection wir)
  :_  state
  [%pass pax %agent [src.bol %publish] %watch pax]~
::
++  bound
  |=  [wir=wire success=? binding=binding:eyre]
  ^-  (quip card _state)
  [~ state]
::
::  +poke-handle-http-request: received on a new connection established
::
++  poke-handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  ::
  =/  request-line  (parse-request-line url.request.inbound-request)
  ?+  request-line
    not-found:gen
  ::  images
  ::
      [[[~ %png] [%'~publish' @t ~]] ~]
    =/  filename=@t  i.t.site.request-line
    =/  img=(unit @t)  (~(get by images) filename)
    ?~  img
      not-found:gen
    (png-response:gen (as-octs:mimes:html u.img))
  ::  styling
  ::
      [[[~ %css] [%'~publish' %index ~]] ~]
    (css-response:gen css)
  ::  scripting
  ::
      [[[~ %js] [%'~publish' %index ~]] ~]
    (js-response:gen js)
  ::  tile js
  ::
      [[[~ %js] [%'~publish' %tile ~]] ~]
    (js-response:gen tile-js)
  ::  home page; redirect to recent
  ::
      [[~ [%'~publish' ~]] ~]
    =/  hym=manx  (index (state-to-json state))
    (redirect:gen '/~publish/recent')
  ::  recent page
  ::
      [[~ [%'~publish' %recent ~]] ~]
    =/  hym=manx  (index (state-to-json state))
    (manx-response:gen hym)
  ::  subscriptions
  ::
      [[~ [%'~publish' %subs ~]] ~]
    =/  hym=manx  (index (state-to-json state))
    (manx-response:gen hym)
  ::  published
  ::
      [[~ [%'~publish' %pubs ~]] ~]
    =/  hym=manx  (index (state-to-json state))
    (manx-response:gen hym)
  ::  new post
  ::
      [[~ [%'~publish' %new-post ~]] ~]
    =/  hym=manx  (index (state-to-json state))
    (manx-response:gen hym)
  ::  new blog
  ::
      [[~ [%'~publish' %new-blog ~]] ~]
    =/  hym=manx  (index (state-to-json state))
    (manx-response:gen hym)
  ::  blog
  ::
      [[~ [%'~publish' @t @t ~]] ~]
    =/  hym=manx  (index (state-to-json state))
    (manx-response:gen hym)
  ::  blog post
  ::
      [[~ [%'~publish' @t @t @t ~]] ~]
    =/  hym=manx  (index (state-to-json state))
    (manx-response:gen hym)
  ::
  ==
::
++  state-to-json
  |=  sat=_state
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
  ^-  (list card)
  [%give %fact `/publishtile %json !>(make-tile-json)]~
::
++  make-tile-json
  ^-  json
  %-  pairs:enjs:format
  :~  invites+(numb:enjs:format ~(wyt by invites))
      new+(numb:enjs:format ~(wyt in unread))
  ==
::
++  poke-import
  |=  i=*
  ^-  (quip card _state)
  ?>  ?=([%publish-v0 *] i)
  =/  dir=publish-dir  ;;(publish-dir +.i)
  ::  make moves to save all files to clay, and
  ::  make moves to call %serve for each collection
  ::
  =/  out=[mow=(list card) sob=soba:clay]
    %+  roll  ~(tap by dir)
    |=  [[pax=path fil=publish-file] mow=(list card) sob=soba:clay]
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
      :-  :*  [%pass wir %arvo %f %build %.y schema]
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
      :-  :*  [%pass post-wir %arvo %f %build %.y post-schema]
              [%pass comment-wir %arvo %f %build %.y comment-schema]
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
  :_  state
  [[%pass /import %arvo %c %info tor] mow.out]
::
++  peer-export
  |=  pax=path
  ^-  (quip card _state)
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
  :_  state
  [%give %fact ~ %export !>([%publish-v0 dir])]~
::
++  peer-publishtile
  |=  wir=wire
  ^-  (quip card _state)
  :_  state
  [%give %fact ~ %json !>(make-tile-json)]~
::
++  peer-primary
  |=  wir=wire
  ^-  (quip card _state)
  ?.  =(our.bol src.bol)
    ::  only we are allowed to subscribe on primary
    ::
    :_  state
    [%give %kick ~ ~]~
  [~ state]
::
++  pull
  |=  wir=wire
  ^-  (quip card _state)
  ?+  wir
    [~ state]
  ::
      [%collection @t ~]
    =/  coll=@tas  i.t.wir
    =/  col=(unit collection)  (~(get by pubs) coll)
    ?~  col
      [~ state]
    =/  new=collection
      u.col(subscribers (~(del in subscribers.u.col) src.bol))
    [~ state(pubs (~(put by pubs) coll new))]
  ::
  ==
::
++  peer-collection
  |=  wir=wire
  ^-  (quip card _state)
  ?.  ?=([@tas ~] wir)
    [~ state]
  =/  coll=@tas  i.wir
  =/  pax  /web/publish/[coll]
  ?>  (allowed src.bol %read pax)
  =/  col=collection  (~(got by pubs) coll)
  =/  new=collection
    col(subscribers (~(put in subscribers.col) src.bol))
  =/  rum=rumor
    [%total our.bol coll new]
  :_  state(pubs (~(put by pubs) coll new))
  [%give %fact ~ %publish-rumor !>(rum)]~
::
++  reap
  |=  [wir=wire err=(unit tang)]
  ^-  (quip card _state)
  ?~  err
    [~ state]
  ?>  ?=([%collection @tas ~] wir)
  =/  col=@tas  i.t.wir
  %-  (slog [leaf+"failed to subscribe to blog: {<col>}"]~)
  [~ state]
::
--
