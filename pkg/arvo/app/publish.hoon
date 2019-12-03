::
/-  *publish
/+  *server, *publish, cram, default-agent
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
+$  card  card:agent:gall
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
      last-note=@da
      notes=(map @tas note)
      order=(list @tas)
      pinned=(set @tas)
  ==
::
+$  versioned-state
  $%  [%1 state-one]
  ==
::
+$  state-one
  $:  our-paths=(list path)
      books=(map @tas notebook)
      subs=(map [@p @tas] notebook)
      recent=(list [@p @tas @tas])
      unread=(set [@p @tas @tas])
  ==
--
::
=|  state-one
=*  state  -
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this  .
      def   ~(. (default-agent this %|) bol)
      main  ~(. +> bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    =/  lac  [%launch-action %publish /publishtile '/~publish/tile.js']
    =/  rav  [%sing %t [%da now.bol] /app/publish/notebooks]
    :_  this
    :~  [%pass /bind %arvo %e %connect [~ /'~publish'] %publish]
        [%pass /tile %agent [our.bol %launch] %poke %launch-action !>(lac)]
        [%pass /read/paths %arvo %c %warp our.bol q.byk.bol `rav]
    ==
  ::
  ++  on-save  !>(state)
  ::
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
::    [~ this(state !<(,[%1 state-one] old))]
    [~ this(state *state-one)]
  ::
  ++  on-poke
    |=  [mar=mark vas=vase]
    ^-  (quip card _this)
    ?+  mar  (on-poke:def mar vas)
        %noun
      ~&  state
      [~ this]
    ::
        %handle-http-request
      =+  !<([id=@ta req=inbound-request:eyre] vas)
      :_  this
      %+  give-simple-payload:app    id
      %+  require-authorization:app  req
      |=  req=inbound-request:eyre
      ^-  simple-payload:http
      not-found:gen
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+  path  (on-watch:def path)
      [%http-response *]  [~ this]
::      [%tile ~]
    ==
  ::
  ++  on-leave
    |=  path
    `this
  ::
  ++  on-peek
    |=  =path
    ~|  "unexpected scry into {<dap.bol>} on path {<path>}"
    !!
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?-    -.sign
        %poke-ack
      ?~  p.sign
        `this
      %-  (slog leaf+"poke failed from {<dap.bol>} on wire {<wire>}" u.p.sign)
      `this
    ::
        %watch-ack
      ?~  p.sign
        `this
      =/  =tank  leaf+"subscribe failed from {<dap.bol>} on wire {<wire>}"
      %-  (slog tank u.p.sign)
      `this
    ::
        %kick  `this
        %fact
      ~|  "unexpected subscription update to {<dap.bol>} on wire {<wire>}"
      ~|  "with mark {<p.cage.sign>}"
      !!
    ==
  ::
  ++  on-arvo
    |=  [wir=wire sin=sign-arvo]
    ^-  (quip card _this)
    ?+  wir
      (on-arvo:def wir sin)
    ::
        [%read %paths ~]
      ?>  ?=([?(%b %c) %writ *] sin)
      =/  rot=riot:clay  +>.sin
      ?>  ?=(^ rot)
      =^  cards  state
        (read-paths:main u.rot)
      [cards this]
    ::
        [%read %note *]
      ?>  ?=([?(%b %c) %writ *] sin)
      =/  rot=riot:clay  +>.sin
      =^  cards  state
        (read-note:main t.t.wir rot)
      [cards this]
    ::
        [%read %comment *]
      ?>  ?=([?(%b %c) %writ *] sin)
      =/  rot=riot:clay  +>.sin
      =^  cards  state
        (read-comment:main t.t.wir rot)
      [cards this]
    ::
        [%bind ~]
      [~ this]
    ==
  ::
  ++  on-fail
    |=  [=term =tang]
    %-  (slog leaf+"error in {<dap.bol>}" >term< tang)
    `this
  --
::
|_  bol=bowl:gall
::
++  read-paths
  |=  ran=rant:clay
  ^-  (quip card _state)
  =/  rav  [%next %t [%da now.bol] /app/publish/notebooks]
  =/  new  (filter-and-sort-paths !<((list path) q.r.ran))
  =/  dif  (diff-paths our-paths new)
  =^  del-moves  state  (del-paths del.dif)
  =^  add-moves  state  (add-paths add.dif)
  ::
  =/  cards=(list card)
    ;:  weld
      [%pass /read/paths %arvo %c %warp our.bol q.byk.bol `rav]~
      del-moves
      add-moves
    ==
  [cards state(our-paths new)]
::
++  read-note
  |=  [pax=path rot=riot:clay]
  ^-  (quip card _state)
  ?>  ?=([%app %publish %notebooks @ @ %udon ~] pax)
  =/  book-name  i.t.t.t.pax
  =/  note-name  i.t.t.t.t.pax
  =/  book  (~(get by books) book-name)
  ?~  book
    [~ state]
  =/  old-note  (~(get by notes.u.book) note-name)
  ?~  old-note
    [~ state]
  ?~  rot
    [~ state]
  =/  udon  !<(@t q.r.u.rot)
  =/  new-note=note  (form-note note-name udon)
  =.  date-created.new-note  date-created.u.old-note
  =.  comments.new-note      comments.u.old-note
  =.  notes.u.book  (~(put by notes.u.book) note-name new-note)
  =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
  :-  [%pass (welp /read/note pax) %arvo %c %warp our.bol rif]~
  state(books (~(put by books) book-name u.book))
::
++  read-comment
  |=  [pax=path rot=riot:clay]
  ^-  (quip card _state)
  ?>  ?=([%app %publish %notebooks @ @ @ %publish-comment ~] pax)
  =/  book-name     i.t.t.t.pax
  =/  note-name     i.t.t.t.t.pax
  =/  comment-date  (slaw %da i.t.t.t.t.t.pax)
  ?~  comment-date
    [~ state]
  =/  book  (~(get by books) book-name)
  ?~  book
    [~ state]
  =/  note  (~(get by notes.u.book) note-name)
  ?~  note
    [~ state]
  =/  old-comment  (~(get by comments.u.note) u.comment-date)
  ?~  old-comment
    [~ state]
  ?~  rot
    [~ state]
  =/  new-comment  !<(comment q.r.u.rot)
  =.  comments.u.note  (~(put by comments.u.note) u.comment-date new-comment)
  =.  notes.u.book  (~(put by notes.u.book) note-name u.note)
  =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
  :-  [%pass (welp /read/comment pax) %arvo %c %warp our.bol rif]~
  state(books (~(put by books) book-name u.book))
::
++  filter-and-sort-paths
  |=  paths=(list path)
  ^-  (list path)
  %+  sort
    %+  skim  paths
    |=  pax=path
    ?|  ?=([%app %publish %notebooks @ @ %udon ~] pax)
        ?=([%app %publish %notebooks @ @ @ %publish-comment ~] pax)
    ==
  |=  [a=path b=path]
  ^-  ?
  (lte (lent a) (lent b))
::
++  diff-paths
  |=  [old=(list path) new=(list path)]
  ^-  [del=(list path) add=(list path)]
  =/  del=(list path)  (skim old |=(p=path ?=(~ (find [p]~ new))))
  =/  add=(list path)  (skim new |=(p=path ?=(~ (find [p]~ old))))
  [del add]
::
++  del-paths
  |=  paths=(list path)
  ^-  (quip card _state)
  %+  roll  paths
  |=  [pax=path cad=(list card) sty=_state]
  ?+    pax  !!
      [%app %publish %notebooks @ @ %udon ~]
    =/  book-name  i.t.t.t.pax
    =/  note-name  i.t.t.t.t.pax
    =/  book  (~(get by books.sty) book-name)
    ?~  book
      [~ sty]
    =.  notes.u.book  (~(del by notes.u.book) note-name)
    :-  ~
    sty(books (~(put by books) book-name u.book))
  ::
      [%app %publish %notebooks @ @ @ %publish-comment ~]
    =/  book-name  i.t.t.t.pax
    =/  note-name  i.t.t.t.t.pax
    =/  comment-date  (slaw %da i.t.t.t.t.t.pax)
    ?~  comment-date
      [~ sty]
    =/  book  (~(get by books.sty) book-name)
    ?~  book
      [~ sty]
    =/  note  (~(get by notes.u.book) note-name)
    ?~  note
      [~ sty]
    =.  comments.u.note  (~(del by comments.u.note) u.comment-date)
    =.  notes.u.book  (~(put by notes.u.book) note-name u.note)
    :-  ~
    sty(books (~(put by books.sty) book-name u.book))
  ==
::
++  add-paths
  |=  paths=(list path)
  ^-  (quip card _state)
  %+  roll  paths
  |=  [pax=path cad=(list card) sty=_state]
  ?+    pax  !!
      [%app %publish %notebooks @ @ %udon ~]
    =/  book-name  i.t.t.t.pax
    =/  note-name  i.t.t.t.t.pax
    =/  new-note=note  (scry-note pax)
    =/  book=notebook
      %+  fall  (~(get by books.sty) book-name)
      [book-name now.bol now.bol ~ [note-name]~ ~]
    =/  old-note  (~(get by notes.book) note-name)
    ?^  old-note
      =.  date-created.new-note  date-created.u.old-note
      =.  comments.new-note      comments.u.old-note
      =.  notes.book  (~(put by notes.book) note-name new-note)
      =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
      :-  [%pass (welp /read/note pax) %arvo %c %warp our.bol rif]~
      sty(books (~(put by books.sty) book-name book))
    ::
    =/  comment-dir    /app/publish/notebooks/[book-name]/[note-name]
    =/  comment-paths  .^((list path) %ct (weld our-beak comment-dir))
    =+  ^-  [cards=(list card) new-comments=(map @da comment)]
      %+  roll  comment-paths
      |=  [pax=path cad=(list card) com=(map @da comment)]
      ?.  ?=([%app %publish %notebooks @ @ @ %publish-comment ~] pax)
        [cad com]
      =/  comment-name  (slaw %da i.t.t.t.t.t.pax)
      ?~  comment-name
        [cad com]
      =/  new-com  .^(comment %cx (welp our-beak pax))
      =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
      :-  [[%pass (welp /read/comment pax) %arvo %c %warp our.bol rif] cad]
      (~(put by com) u.comment-name new-com)
    =.  comments.new-note  new-comments
    =.  notes.book  (~(put by notes.book) note-name new-note)
    =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
    :-  [[%pass (welp /read/note pax) %arvo %c %warp our.bol rif] cards]
    sty(books (~(put by books.sty) book-name book))
  ::
      [%app %publish %notebooks @ @ @ %publish-comment ~]
    =/  book-name  i.t.t.t.pax
    =/  note-name  i.t.t.t.t.pax
    =/  comment-name  (slaw %da i.t.t.t.t.t.pax)
    =/  book  (~(get by books.sty) book-name)
    ?~  book
      [~ sty]
    =/  note  (~(get by notes.u.book) note-name)
    ?~  note
      [~ sty]
    ?~  comment-name
      [~ sty]
    =/  new-com  .^(comment %cx (welp our-beak pax))
    =.  comments.u.note  (~(put by comments.u.note) u.comment-name new-com)
    =.  notes.u.book  (~(put by notes.u.book) note-name u.note)
    =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
    :-  [%pass (welp /read/comment pax) %arvo %c %warp our.bol rif]~
    sty(books (~(put by books.sty) book-name u.book))
  ==
::
++  scry-note
  |=  pax=path
  ^-  note
  ?>  ?=([%app %publish %notebooks @ @ %udon ~] pax)
  =/  note-name  i.t.t.t.t.pax
  =/  udon=@t  .^(@t %cx (welp our-beak pax))
  (form-note note-name udon)
::
++  form-note
  |=  [note-name=@tas udon=@t]
  ^-  note
  =/  build=(each manx tang)
    %-  mule  |.
    ^-  manx
    elm:(static:cram (ream udon))
  ::
  =/  meta=(each (map term knot) tang)
    %-  mule  |.
    %-  ~(run by inf:(static:cram (ream udon)))
    |=  a=dime  ^-  cord
    ?+  (end 3 1 p.a)  (scot a)
      %t  q.a
    ==
  ::
  =/  author=@p  our.bol
  =?  author  ?=(%.y -.meta)
    %+  fall
      (biff (~(get by p.meta) %author) (slat %p))
    our.bol
  ::
  =/  title=@t  note-name
  =?  title  ?=(%.y -.meta)
    (fall (~(get by p.meta) %title) note-name)
  ::
  :*  author
      title
      note-name
      now.bol
      now.bol
      udon
      build
      ~
  ==
::
++  our-beak  /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)
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
::
++  write-file
  =,  space:userlib
  |=  [pax=path cay=cage]
  ^-  card
  =.  pax  (weld our-beak pax)
  [%pass (weld /write pax) %arvo %c %info (foal pax cay)]
::
++  delete-file
  =,  space:userlib
  |=  pax=path
  ^-  card
  =.  pax  (weld our-beak pax)
  [%pass (weld /delete pax) %arvo %c %info (fray pax)]
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
--
