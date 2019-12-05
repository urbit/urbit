::
/-  *publish, *group-store, *permission-hook, *permission-group-hook
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
      participants=path
      subscribers=path
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
        [%pass /permissions %agent [our.bol %permission-store] %watch /updates]
        :*  %pass  /invites  %agent  [our.bol %invite-store]  %watch
            /invitatory/publish
        ==
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
    ::
        %publish-action2
      =^  cards  state
        (poke-publish-action-2:main !<(action-2 vas))
      [cards this]
    ==
  ::
  ++  on-watch
    |=  pax=path
    ^-  (quip card _this)
    ?+    pax  (on-watch:def pax)
        [%http-response *]  [~ this]
    ::
        [%notebook @ ~]
      =/  book-name  i.t.pax
      =/  book  (~(get by books) book-name)
      !!
    ::
        [%tile ~]  !!
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
    |=  [wir=wire sin=sign:agent:gall]
    ^-  (quip card _this)
    ?-    -.sin
        %poke-ack   (on-agent:def wir sin)
    ::
        %watch-ack  (on-agent:def wir sin)
    ::
        %kick       (on-agent:def wir sin)
    ::
        %fact
      ?+  wir  (on-agent:def wir sin)
          [%subscribe @ @ ~]
        =/  who=@p     (slav %p i.t.wir)
        =/  book-name  i.t.t.wir
        !!
      ::
          [%permissions ~]  !!
      ::
          [%invites ~]  !!
      ==
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
  ~&  new-path-list+!<((list path) q.r.ran)
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
    =/  old-book  (~(get by books.sty) book-name)
    =+  ^-  [cards=(list card) new-book=notebook]
      ?~  old-book
        :-  ~
        :*  book-name
            now.bol
            now.bol
            ~
            [note-name]~
            ~
            /publish/[book-name]/participants
            /publish/[book-name]/subscribers
        ==
      [~ u.old-book]
    =/  old-note  (~(get by notes.new-book) note-name)
    ?^  old-note
      =.  date-created.new-note  date-created.u.old-note
      =.  comments.new-note      comments.u.old-note
      =.  notes.new-book  (~(put by notes.new-book) note-name new-note)
      =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
      :-  [%pass (welp /read/note pax) %arvo %c %warp our.bol rif]~
      sty(books (~(put by books.sty) book-name new-book))
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
    =.  notes.new-book  (~(put by notes.new-book) note-name new-note)
    =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
    :-  [[%pass (welp /read/note pax) %arvo %c %warp our.bol rif] cards]
    sty(books (~(put by books.sty) book-name new-book))
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
++  delete-dir
  |=  pax=path
  ^-  card
  =/  nor=nori:clay
    :-  %&
    %+  turn  .^((list path) %ct (weld our-beak pax))
    |=  pax=path
    ^-  [path miso:clay]
    [pax %del ~]
  [%pass (weld /delete pax) %arvo %c %info q.byk.bol nor]
::
++  add-front-matter
  |=  [fro=(map knot cord) udon=@t]
  ^-  @t
  %-  of-wain:format
  =/  tum  (trip udon)
  =/  id  (find ";>" tum)
  ?~  id
    %+  weld  (front-to-wain fro)
    (to-wain:format (crip :(weld ";>\0a" tum)))
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
++  group-poke
  |=  act=group-action
  ^-  card
  [%pass / %agent [our.bol %group-store] %poke %group-action !>(act)]
::
++  perm-hook-poke
  |=  act=permission-hook-action
  ^-  card
  :*  %pass
      /
      %agent
      [our.bol %permission-hook]
      %poke
      %permission-hook-action
      !>(act)
  ==
::
++  perm-group-hook-poke
  |=  act=permission-group-hook-action
  ^-  card
  :*  %pass
      /
      %agent
      [our.bol %permission-group-hook]
      %poke
      %permission-group-hook-action
      !>(act)
  ==
::
++  create-security
  |=  [par=path sub=path sec=rw-security]
  ^-  (list card)
  =+  ^-  [par-type=?(%black %white) sub-type=?(%black %white)]
    ?-  sec
      %channel  [%black %black]
      %village  [%white %white]
      %journal  [%black %white]
      %mailbox  [%white %black]
    ==
  :~  (perm-group-hook-poke [%associate par [[par par-type] ~ ~]])
      (perm-group-hook-poke [%associate sub [[sub sub-type] ~ ~]])
  ==
::
++  poke-publish-action-2
  |=  act=action-2
  ^-  (quip card _state)
  ?-    -.act
      %new-book
    ?>  (team:title our.bol src.bol)
    =+  ^-  [cards=(list card) par-path=path sub-path=path]
      ?-  -.group.act
          %old  [~ par.group.act sub.group.act]
          %new
        =/  par-path  /publish/[book.act]/participants
        =/  sub-path  /publish/[book.act]/subscribers
        :_  [par-path sub-path]
        ;:  weld
          :~  (group-poke [%bundle par-path])
              (group-poke [%bundle sub-path])
              (group-poke [%add par.group.act par-path])
              (group-poke [%add sub.group.act sub-path])
          ==
          (create-security par-path sub-path sec.group.act)
          :~  (perm-hook-poke [%add-owned par-path par-path])
              (perm-hook-poke [%add-owned sub-path sub-path])
          ==
        ==
      ==
    =/  new-book=notebook  [title.act now.bol now.bol ~ ~ ~ par-path sub-path]
    :-  cards
    state(books (~(put by books) book.act new-book))
  ::
      %new-note
    =/  pax=path  /app/publish/notebooks/[book.act]/[note.act]/udon
    =/  front=(map knot cord)
      %-  my
      :~  title+title.act
          author+(scot %p src.bol)
      ==
    =.  body.act  (cat 3 body.act '\0a')
    =/  file=@t   (add-front-matter front body.act)
    :_  state
    [(write-file pax %udon !>(file))]~
  ::
      %new-comment
    =/  pax=path
      %+  weld  /app/publish/notebooks
      /[book.act]/[note.act]/(scot %da now.bol)/publish-comment
    =/  new-comment=comment
      :*  author=src.bol
          date-created=now.bol
          last-edit=now.bol
          content=body.act
      ==
    :_  state
    [(write-file pax %publish-comment !>(new-comment))]~
  ::
      %edit-book
    ?>  (team:title our.bol src.bol)
    =/  book  (~(got by books) book.act)
    =?  title.book  ?=(^ new-title.act)
      u.new-title.act
    =+  ^-  [cards=(list card) par-path=path sub-path=path]
      ?~  new-group.act
        [~ participants.book subscribers.book]
      ?-  -.u.new-group.act
          %old  [~ par.u.new-group.act sub.u.new-group.act]
          %new
        =/  par-path  /publish/[book.act]/participants
        =/  sub-path  /publish/[book.act]/subscribers
        :_  [par-path sub-path]
        %+  weld
          :~  (group-poke [%bundle par-path])
              (group-poke [%bundle sub-path])
              (group-poke [%add par.u.new-group.act par-path])
              (group-poke [%add sub.u.new-group.act sub-path])
              (perm-hook-poke [%add-owned par-path par-path])
              (perm-hook-poke [%add-owned sub-path sub-path])
          ==
        (create-security par-path sub-path sec.u.new-group.act)
      ==
    =.  participants.book  par-path
    =.  subscribers.book   sub-path
    [~ state(books (~(put by books) book.act book))]
  ::
      %edit-note
    =/  pax=path  /app/publish/notebooks/[book.act]/[note.act]/udon
    =/  front=(map knot cord)
      %-  my
      :~  title+new-title.act
          author+(scot %p src.bol)
      ==
    =.  new-body.act  (cat 3 new-body.act '\0a')
    =/  file=@t   (add-front-matter front new-body.act)
    :_  state
    [(write-file pax %udon !>(file))]~
  ::
      %edit-comment
    =/  pax=path
      %+  weld  /app/publish/notebooks
      /[book.act]/[note.act]/[comment.act]/publish-comment
    =/  comment  .^(comment %cx (weld our-beak pax))
    =.  content.comment    new-body.act
    =.  last-edit.comment  now.bol
    :_  state
    [(write-file pax %publish-comment !>(comment))]~
  ::
      %del-book
    ?>  (team:title our.bol src.bol)
    =/  pax=path  /app/publish/notebooks/[book.act]
    :_  state(books (~(del by books) book.act))
    [(delete-dir pax)]~
  ::
      %del-note
    =/  pax=path  /app/publish/notebooks/[book.act]/[note.act]/udon
    :_  state
    [(delete-file pax)]~
  ::
      %del-comment
    =/  pax=path
      %+  weld  /app/publish/notebooks
      /[book.act]/[note.act]/[comment.act]/publish-comment
    :_  state
    [(delete-file pax)]~
  ::
      %subscribe
    =/  wir=wire  /subscribe/(scot %p who.act)/[book.act]
    :_  state
    [%pass wir %agent [who.act %publish] %watch /notebook/[book.act]]~
  ::
      %unsubscribe
    =/  wir=wire  /subscribe/(scot %p who.act)/[book.act]
    :_  state(subs (~(del by subs) who.act book.act))
    [%pass wir %agent [who.act %publish] %leave ~]~
  ==
::
--
