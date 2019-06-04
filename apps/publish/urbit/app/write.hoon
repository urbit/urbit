::
::  /app/write.hoon
::
/-  hall, *write
/+  *server, *write
::
/=  index
  /^  $-(json manx)
  /:  /===/app/write/index  /!noun/
::
/=  js
  /^  octs
  /;  as-octs:mimes:html
  /|  /:  /===/app/write/js/index  /js/
      /~  ~
  ==
::
/=  css
  /^  octs
  /;  as-octs:mimes:html
  /|  /:  /===/app/write/css/index  /css/
      /~  ~
  ==
::
|%
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
      [%connect wire binding:http-server term]
      [%http-response http-event:http]
      [%disconnect binding:http-server]
  ==
::
+$  poke
  $%  [%hall-action action:hall]
      [%write-action action]
  ==
::
+$  diff
  $%  [%hall-rumor rumor:hall]
      [%json json]
      [%write-collection collection]
      [%write-rumor rumor]
  ==
::
--
::
|_  [bol=bowl:gall sat=state]
::
++  this  .
::  +our-beak: beak for this app, with case set to current invocation date
::
++  our-beak  /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)
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
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  ~&  write-prep+act.bol
  ?~  old
    :_  this
    [ost.bol %connect / [~ /'~publish'] %write]~
  [~ this(sat (state u.old))] 
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [~ this]
  ?+  a
    [~ this]
  ::
      %test-build
    =/  schema=schematic:ford
      :-
      :*  
          %bake
          %write-info
          *coin
          [[our.bol q.byk.bol] /fora/write/web]
      ==
      :*  
          %bake
          %write-post
          *coin
          [[our.bol q.byk.bol] /post-1/fora/write/web]
      ==
    :_  this
    [ost.bol %build /test/build %.n schema]~
  ::
      %print-subs
    ~&  sup.bol
    [~ this]
  ::
      %kill-all-builds
    :_  this
    :~  [ost.bol %kill /collection/fora ~]
        [ost.bol %kill /post/fora/post-1 ~]
        [ost.bol %kill /comments/fora/post-1 ~]
        [ost.bol %kill /post/fora/post-2 ~]
        [ost.bol %kill /comments/fora/post-2 ~]
        [ost.bol %kill /post/fora/post-3 ~]
        [ost.bol %kill /comments/fora/post-3 ~]
    ==
  ::
  ::
      %send-diff
    =/  rum=json  (frond:enjs:format %poke-noun ~)
    =/  mov=(list move)
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [=bone *]
    [bone %diff %json rum]
    ~&  mov+mov
    [mov this]
  ::
      %peer
    ~&  %peer
    :_  this
    [ost.bol %peer /collection/fora [~zod %write] /collection/fora]~
  ::
      %pull
    ~&  %pull
    =/  wir=wire  /collection/fora
    :_  this
    [ost.bol %pull wir [~zod %write] ~]~
  ::
      %flush-state
    [~ this(sat *state)]
  ::
      %print-state
    ~&  sat
    [~ this]
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
          [dat.del ~ ~ ~ ~]
        [dat.del pos.u.old com.u.old order.u.old]
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
          [[%.n ~] (my [pos.del dat.del] ~) ~ ~ ~]
        [col.u.old (~(put by pos.u.old) pos.del dat.del) com.u.old order.u.old]
      =?  pubs.sat  =(our.bol who.del)
        (~(put by pubs.sat) col.del new)
      =?  subs.sat  !=(our.bol who.del)
        (~(put by subs.sat) [who.del col.del] new)
      =?  da-this  ?=(~ old)
        (da-insert who.del col.del pos.del)
      (da-emil (affection del))
    ::
        %comments
      =/  old=(unit collection)
        ?:  =(our.bol who.del)
          (~(get by pubs.sat) col.del)
        (~(get by subs.sat) who.del col.del)
      =/  new=collection
        ?~  old
          [[%.n ~] ~ (my [pos.del dat.del] ~) ~ ~]
        [col.u.old pos.u.old (~(put by com.u.old) pos.del dat.del) order.u.old]
      =?  pubs.sat  =(our.bol who.del)
        (~(put by pubs.sat) col.del new)
      =?  subs.sat  !=(our.bol who.del)
        (~(put by subs.sat) [who.del col.del] new)
      (da-emil (affection del))
    ::
        %total
      =?  pubs.sat  =(our.bol who.del)
        (~(put by pubs.sat) col.del dat.del)
      =?  subs.sat  !=(our.bol who.del)
        (~(put by subs.sat) [who.del col.del] dat.del)
      ::
      =/  posts=(list @tas)  ~(tap in ~(key by pos.dat.del))
      =.  da-this
      |-
      ?~  posts
        da-this
      %=  $
        da-this  (da-insert who.del col.del i.posts)
        posts    t.posts
      ==
      (da-emil (affection del))
    ::
    ==
  ::
  ++  da-insert
    |=  [who=@p coll=@tas post=@tas]
    ^+  da-this
    ::  assume we've read our own posts
    ::
    =?  unread.sat  !=(who our.bol)
      (~(put in unread.sat) who coll post)
    ::  insertion sort into latest
    ::
    =/  new-date=@da  date-created:(need (get-post-by-index who coll post))
    =/  pre=(list [@p @tas @tas])  ~
    =/  suf=(list [@p @tas @tas])  latest.sat

    =.  latest.sat
    |-
    ?~  suf
      (weld pre [who coll post]~)
    =/  i-date=@da  date-created:(need (get-post-by-index i.suf))
    ?:  (gte new-date i-date)
      (weld pre [[who coll post] suf])
    %=  $
      suf  t.suf
      pre  (snoc pre i.suf)
    ==
    ::  insertion sort into order
    ::
    =/  new-post=post-info  (need (get-post-by-index who coll post))
    =/  col=collection  (need (get-coll-by-index who coll))
    :: 
    =/  pre=(list @tas)  ~
    =/  suf=(list @tas)
      ?:  pinned.new-post
        pin.order.col
      unpin.order.col
    ::
    =/  new-list=(list @tas)
    |-
    ?~  suf
      (snoc pre post)
    =/  i-date=@da  date-created:(need (get-post-by-index who coll i.suf))
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
  --
::  +bake: apply delta
::
++  bake
  |=  del=delta
  ^-  (quip move _this)
  da-done:(da-change:da del)
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
  [b %diff %write-rumor u.rum]~
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
  ^-  (unit post-info)
  =/  col=(unit collection)
    ?:  =(our.bol who)
      (~(get by pubs.sat) coll)
    (~(get by subs.sat) who coll)
  ?~  col  ~
  =/  pos=(unit (each [post-info manx] tang))
    (~(get by pos.u.col) post)
  ?~  pos  ~
  ?:  ?=(%.n -.u.pos)  ~
  [~ -.p.u.pos]
::
++  get-coll-by-index
  |=  [who=@p coll=@tas]
  ^-  (unit collection)
  ?:  =(our.bol who)
    (~(get by pubs.sat) coll)
  (~(get by subs.sat) coll)
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
      ?>  ?=(%write-info p.cage.build-result.mad)
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
      =/  del=delta  [%total our.bol col dat ~ ~ ~ ~]
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
            dat
            pos.dat.u.partial.u.awa
            com.dat.u.partial.u.awa
            [~ ~]
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
          dat
          pos.dat.u.partial.u.awa
          com.dat.u.partial.u.awa
          [~ ~]
      ==
    =.  awaiting.sat  (~(put by awaiting.sat) col builds.u.awa `del)
    [~ this]
  ::
      [%post @t @t ~]
    =/  col=@tas  i.t.wir
    =/  pos=@tas  i.t.t.wir
    =/  awa  (~(get by awaiting.sat) col)
    ::
    =/  dat=(each [post-info manx] tang)
      ?:  ?=([%incomplete *] mad)
        [%.n tang.mad]
      ?:  ?=([%error *] build-result.mad)
        [%.n message.build-result.mad]
      ?>  ?=(%bake +<.build-result.mad)
      ?>  ?=(%write-post p.cage.build-result.mad)
      [%.y (,[post-info manx] q.q.cage.build-result.mad)]
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
      =/  del=delta  [%total our.bol col [%.n ~] (my [pos dat] ~) ~ ~ ~]
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
            (~(put by pos.dat.u.partial.u.awa) pos dat)
            com.dat.u.partial.u.awa
            [~ ~]
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
          (~(put by pos.dat.u.partial.u.awa) pos dat)
          com.dat.u.partial.u.awa
          [~ ~]
      ==
    =.  awaiting.sat  (~(put by awaiting.sat) col builds.u.awa `del)
    [~ this]
  ::
      [%comments @t @t ~]
    =/  col=@tas  i.t.wir
    =/  pos=@tas  i.t.t.wir
    =/  awa  (~(get by awaiting.sat) col)
    ::
    =/  dat=(each (list [comment-info manx]) tang)
      ?:  ?=([%incomplete *] mad)
        [%.n tang.mad]
      ?:  ?=([%error *] build-result.mad)
        [%.n message.build-result.mad]
      ?>  ?=(%bake +<.build-result.mad)
      ?>  ?=(%write-comments p.cage.build-result.mad)
      [%.y (,(list [comment-info manx]) q.q.cage.build-result.mad)]
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
      =/  del=delta  [%total our.bol col [%.n ~] ~ (my [pos dat] ~) ~ ~]
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
            (~(put by com.dat.u.partial.u.awa) pos dat)
            [~ ~]
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
          (~(put by com.dat.u.partial.u.awa) pos dat)
          [~ ~]
      ==
    =.  awaiting.sat  (~(put by awaiting.sat) col builds.u.awa `del)
    [~ this]
  ==
::
++  poke-write-action
  |=  act=action
  ^-  (quip move _this)
  ?-  -.act
  ::
      %new-collection
    ::  XX  check permissions of src.bol
    ::  XX  check if file already exists
    =/  conf=collection-info
      :*  our.bol
          title.act
          name.act
          com.act
          edit.act
          now.bol
          now.bol
      ==
    ::  XX  set permissions
    ::  XX  automatically serve collection
    ::      (add to set of builds)
    =/  pax=path  /web/write/[name.act]/write-info
    ::
    =/  wir=wire  /collection/[name.act]
    =/  schema=schematic:ford
      :*  %bake
          %write-info
          *coin
          [[our.bol q.byk.bol] /[name.act]/write/web]
      ==
    :_  this
    :~  (write-file pax %write-info !>(conf))
        [ost.bol %build wir %.y schema]
    ==
  ::
      %new-post
    ::  XX  check permissions of src.bol
    ::  XX  check if file already exists
    ::  XX  check if coll doesn't exist
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
    ::  XX  set permissions
    ::  XX  add to set of builds
    =/  pax=path  /web/write/[coll.act]/[name.act]/udon
    =/  out=@t    (update-udon-front front content.act)
    ::
    =/  post-wir=wire  /post/[coll.act]/[name.act]
    =/  post-schema=schematic:ford
      :*  %bake
          %write-post
          *coin
          [[our.bol q.byk.bol] /[name.act]/[coll.act]/write/web]
      ==
    ::
    =/  comments-wir=wire  /comments/[coll.act]/[name.act]
    =/  comments-schema=schematic:ford
      :*  %bake
          %write-comments
          *coin
          [[our.bol q.byk.bol] /[name.act]/[coll.act]/write/web]
      ==
    :_  this
    :~  (write-file pax %udon !>(out))
        [ost.bol %build comments-wir %.y comments-schema]
        [ost.bol %build post-wir %.y post-schema]
    ==
  ::
      %new-comment
    ::  XX  check permissions of src.bol
    ::  XX  check if file already exists
    =.  content.act  (cat 3 content.act '\0a')  :: XX fix udon parser
    =/  front=(map knot cord)
      %-  my
      :~  [%creator (scot %p src.bol)]
          [%collection coll.act]
          [%post post.act]
          [%date-created (scot %da now.bol)]
          [%last-modified (scot %da now.bol)]
      ==
    ::  XX  set permissions
    ::  XX  add to set of builds
    =/  pax=path  /web/write/[coll.act]/[post.act]/(scot %da now.bol)/udon
    =/  out=@t    (update-udon-front front content.act)
    :_  this
    [(write-file pax %udon !>(out))]~
  ::
      %delete
    [~ this]
  ::
      %edit-collection
    [~ this]
  ::
      %edit-post
    [~ this]
  ::
      %edit-comment
    [~ this]
  ::
      %invite
    [~ this]
  ::
  ::  %serve:
  ::
      %serve
    :: XX specialize this check for subfiles
    ?:  (~(has by pubs.sat) coll.act)
      [~ this]
    =/  files=(list path)
      .^((list path) %ct (weld our-beak /web/write/[coll.act]))
    =/  all=[moves=(list move) builds=(set wire)]
    %+  roll  files
    |=  [pax=path out=[moves=(list move) builds=(set wire)]]
    ?+  pax
      out
    ::
      [%web %write @tas %write-info ~]
      ?>  =(coll.act i.t.t.pax)
      =/  wir=wire  /collection/[coll.act]
      =/  schema=schematic:ford
        :*  %bake
            %write-info
            *coin
            [[our.bol q.byk.bol] /[coll.act]/write/web]
        ==
      %=  out
        moves   [[ost.bol %build wir %.y schema] moves.out]
        builds  (~(put in builds.out) wir)
      ==
    ::
      [%web %write @tas @tas %udon ~]
      ?>  =(coll.act i.t.t.pax)
      =/  post  i.t.t.t.pax
      =/  post-wir=wire  /post/[coll.act]/[post]
      =/  post-schema=schematic:ford
        :*  %bake
            %write-post
            *coin
            [[our.bol q.byk.bol] /[post]/[coll.act]/write/web]
        ==
      ::
      =/  comments-wir=wire  /comments/[coll.act]/[post]
      =/  comments-schema=schematic:ford
        :*  %bake
            %write-comments
            *coin
            [[our.bol q.byk.bol] /[post]/[coll.act]/write/web]
        ==
      %=    out
          moves
        :*  [ost.bol %build post-wir %.y post-schema]
            [ost.bol %build comments-wir %.y comments-schema]
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
    =/  col=(unit collection)  (~(get by pubs.sat) coll.act)
    ?~  col
      ~|  [%non-existent-collection coll.act]  !!
    =/  kills=(list move)
      %+  roll  ~(tap by pos.u.col)
      |=  [[post=@tas *] out=(list move)]
      :*  [ost.bol %kill /post/[coll.act]/[post] ~]
          [ost.bol %kill /comments/[coll.act]/[post] ~]
          out
      ==
    ::
    =/  new-latest=(list [@p @tas @tas])
      %+  skip  latest.sat
      |=  [who=@p coll=@tas post=@tas]
      ?&  =(who our.bol)
          =(coll coll.act)
      ==
    ::
    =/  new-unread=(set [@p @tas @tas])
      %-  sy
      %+  skip  ~(tap in unread.sat)
      |=  [who=@p coll=@tas post=@tas]
      ?&  =(who our.bol)
          =(coll coll.act)
      ==
    ::
    :-  [[ost.bol %kill /collection/[coll.act] ~] kills]
    %=  this
      pubs.sat      (~(del by pubs.sat) coll.act)
      awaiting.sat  (~(del by awaiting.sat) coll.act)
      latest.sat    new-latest
      unread.sat    new-unread
    ==
  ::
  ::  %subscribe:
  ::
      %subscribe
    =/  wir=wire  /collection/[coll.act]
    :_  this
    [ost.bol %peer wir [who.act %write] wir]~
  ::
  ::  %unsubscribe:
  ::
      %unsubscribe
    =/  new-latest=(list [@p @tas @tas])
      %+  skim  latest.sat
      |=  [who=@p coll=@tas post=@tas]
      ?&  =(who our.bol)
          =(coll coll.act)
      ==
    ::
    =/  new-unread=(set [@p @tas @tas])
      %-  sy
      %+  skim  ~(tap in unread.sat)
      |=  [who=@p coll=@tas post=@tas]
      ?&  =(who our.bol)
          =(coll coll.act)
      ==
    =/  wir=wire  /collection/[coll.act]
    :-  [ost.bol %pull wir [who.act %write] ~]~
    %=  this
      subs.sat    (~(del by subs.sat) who.act coll.act)
      latest.sat  new-latest
      unread.sat  new-unread
    ==
  ::
  ==
::
++  bound
  |=  [wir=wire success=? binding=binding:http-server]
  ^-  (quip move _this)
  [~ this]
::
::  +poke-handle-http-request: received on a new connection established
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  ::
  =/  request-line  (parse-request-line url.request.inbound-request)
  ?+  request-line
    :_  this
    [ost.bol %http-response not-found:app]~
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
    ?~  who  [[ost.bol %http-response not-found:app]~ this]
    =/  col=(unit collection)
      ?:  =(u.who our.bol)
        (~(get by pubs.sat) blog)
      (~(get by subs.sat) u.who blog)
    ?~  col  [[ost.bol %http-response not-found:app]~ this]
    =/  pos  (~(get by pos.u.col) post)
    ?~  pos  [[ost.bol %http-response not-found:app]~ this]
    

    =/  hym=manx  (index (state-to-json sat))
    :_  this
    [ost.bol %http-response (manx-response:app hym)]~
  ::
  ==
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
  [~ this]
::
++  peer-collection
  |=  wir=wire
  ^-  (quip move _this)
  ?.  ?=([@tas ~] wir)
    [~ this]
  ::  XX handle permissions for foreign subscriptions
  ::
  =/  coll=@tas  i.wir
  =/  col=(unit collection)  (~(get by pubs.sat) coll)
  ?~  col
    [~ this]
  =/  rum=rumor
    [%total our.bol coll u.col]
  :_  this
  [ost.bol %diff %write-rumor rum]~
::
++  diff-write-rumor
  |=  [wir=wire rum=rumor]
  ^-  (quip move _this)
  (bake rum)
::
::  +poke-handle-http-cancel: received when a connection was killed
::
++  poke-handle-http-cancel
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  [~ this]
::
--
