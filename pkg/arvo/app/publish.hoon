::
/-  *publish,
    *group-store,
    *group-hook,
    *permission-hook,
    *permission-group-hook,
    *permission-store,
    *invite-store,
    *metadata-store,
    *metadata-hook
/+  *server, *publish, cram, default-agent, dbug
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
+$  collection-zero  [* pos=(map @tas *) *]
::
+$  state-zero
  $:  pubs=(map @tas collection-zero)
      *
  ==
::
+$  state-two
  $:  our-paths=(list path)
      books=(map @tas notebook)
      subs=(map [@p @tas] notebook)
      tile-num=@ud
  ==
::
+$  versioned-state
  $%  [%1 state-two]
      [%2 state-two]
  ==
::
+$  metadata-delta
  $%  $:  %add
          group-path=path
          app-path=path
          title=@t
          desc=@t
          author=@p
          created=@da
      ==
      [%remove author=@p book=@tas]
  ==
--
::
=|  versioned-state
=*  state  -
%-  agent:dbug
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this  .
      def   ~(. (default-agent this %|) bol)
      main  ~(. +> bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    =/  lac  [%add %publish /publishtile '/~publish/tile.js']
    =/  rav  [%sing %t [%da now.bol] /app/publish/notebooks]
    :_  this
    :~  [%pass /bind %arvo %e %connect [~ /'~publish'] %publish]
        [%pass /tile %agent [our.bol %launch] %poke %launch-action !>(lac)]
        [%pass /read/paths %arvo %c %warp our.bol q.byk.bol `rav]
        [%pass /permissions %agent [our.bol %permission-store] %watch /updates]
        (invite-poke:main [%create /publish])
        :*  %pass  /invites  %agent  [our.bol %invite-store]  %watch
            /invitatory/publish
        ==
        :*  %pass  /  %agent  [our.bol %invite-store]  %poke  %invite-action
            !>([%create /publish])
        ==
    ==
  ::
  ++  on-save  !>(state)
  ::
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    =/  old-state=(each versioned-state tang)
      (mule |.(!<(versioned-state old)))
    ?:  ?=(%& -.old-state)
      ?-  -.p.old-state
          %1
        :_  this(state [%2 +.p.old-state])
        %-  zing
        %+  turn  ~(tap by books.p.old-state)
        |=  [name=@tas book=notebook]
        ^-  (list card)
        =/  group-host=(unit @p)
          ?>  ?=(^ writers.book)
          (slaw %p i.writers.book)
        ?~  group-host  ~
        ?:  =(u.group-host our.bol)  ~
        :~  %-  perm-group-hook-poke:main
            [%associate writers.book [[writers.book %white] ~ ~]]
          ::
            (perm-hook-poke:main [%add-owned writers.book writers.book])
        ==
      ::
          %2
        [~ this(state p.old-state)]
      ==
    =/  zero  !<(state-zero old)
    |^
    =/  rav  [%next %t [%da now.bol] /app/publish/notebooks]
    =/  tile-json
      (frond:enjs:format %notifications (numb:enjs:format 0))
    =/  init-cards=(list card)
      :~  [%pass /read/paths %arvo %c %warp our.bol q.byk.bol `rav]
          :*  %pass  /permissions  %agent  [our.bol %permission-store]  %watch
              /updates
          ==
          (invite-poke:main [%create /publish])
          :*  %pass  /invites  %agent  [our.bol %invite-store]  %watch
              /invitatory/publish
          ==
          [%give %fact [/publishtile]~ %json !>(tile-json)]
      ==
    =+  ^-  [kick-cards=(list card) old-subs=(jug @tas @p)]  kick-subs
    =/  inv-scry-pax
      /(scot %p our.bol)/invite-store/(scot %da now.bol)/invitatory/publish/noun
    =/  inv=(unit invitatory)  .^((unit invitatory) %gx inv-scry-pax)
    =|  new-state=state-two
    =?  tile-num.new-state  ?=(^ inv)
      ~(wyt by u.inv)
    :_  this(state [%2 new-state])
    ;:  weld
      kill-builds
      kick-cards
      init-cards
      (move-files old-subs)
    ==
    ::
    ++  kick-subs
      ^-  [(list card) (jug @tas @p)]
      =+  ^-  [paths=(list path) subs=(jug @tas @p)]
        %+  roll  ~(tap by sup.bol)
        |=  [[duct [who=@p pax=path]] paths=(list path) subs=(jug @tas @p)]
        ^-  [(list path) (jug @tas @p)]
        ?.  ?=([%collection @ ~] pax)
          [paths subs]
        =/  book-name  i.t.pax
        :-  [pax paths]
        (~(put ju subs) book-name who)
      ?~  paths
        [~ subs]
      [[%give %kick paths ~]~ subs]
    ::
    ++  kill-builds
      ^-  (list card)
      %-  zing
      %+  turn  ~(tap by pubs.zero)
      |=  [col-name=@tas col-data=collection-zero]
      ^-  (list card)
      :-  [%pass /collection/[col-name] %arvo %f %kill ~]
      %-  zing
      %+  turn  ~(tap by pos.col-data)
      |=  [pos-name=@tas *]
      :~  [%pass /post/[col-name]/[pos-name] %arvo %f %kill ~]
          [%pass /comments/[col-name]/[pos-name] %arvo %f %kill ~]
      ==
    ::
    ++  send-invites
      |=  [book=@tas subscribers=(set @p)]
      ^-  (list card)
      %+  turn  ~(tap in subscribers)
      |=  who=@p
      ^-  card
      =/  uid  (sham %publish who book eny.bol)
      =/  inv=invite
        :*  our.bol  %publish  /notebook/[book]  who
            (crip "invite for notebook {<our.bol>}/{(trip book)}")
        ==
      =/  act=invite-action  [%invite /publish uid inv]
      [%pass /invite %agent [who %invite-hook] %poke %invite-action !>(act)]
    ::
    ++  move-files
      |=  old-subs=(jug @tas @p)
      ^-  (list card)
      =+  ^-  [cards=(list card) sob=soba:clay]
        %+  roll  .^((list path) %ct (weld our-beak:main /web/publish))
        |=  [pax=path car=(list card) sob=soba:clay]
        ^-  [(list card) soba:clay]
        ?+    pax
            [car sob]
        ::
            [%web %publish @ %publish-info ~]
          =/  book-name  i.t.t.pax
          =/  old=old-info  .^(old-info %cx (welp our-beak:main pax))
          =/  group-pax  /~/(scot %p our.bol)/[book-name]
          =/  book=notebook-info
            [title.old '' =(%open comments.old) / /]
          =+  ^-  [grp-car=(list card) write-pax=path read-pax=path]
            (make-groups:main book-name [group-pax ~ %.n %.n] title.old '')
          =.  writers.book      write-pax
          =.  subscribers.book  read-pax
          =/  inv-car  (send-invites book-name (~(get ju old-subs) book-name))
          :-  :(weld car grp-car inv-car)
          ^-  soba:clay
          :+  [pax %del ~]
            :-  /app/publish/notebooks/[book-name]/publish-info
            [%ins %publish-info !>(book)]
          sob
        ::
            [%web %publish @ @ %udon ~]
          =/  book  i.t.t.pax
          =/  note  i.t.t.t.pax
          :-  car
          :+  [pax %del ~]
            :-  /app/publish/notebooks/[book]/[note]/udon
            [%ins %udon !>(.^(@t %cx (welp our-beak:main pax)))]
          sob
        ::
            [%web %publish @ @ @ %publish-comment ~]
          =/  book  i.t.t.pax
          =/  note  i.t.t.t.pax
          =/  comm  i.t.t.t.t.pax
          =/  old=old-comment  .^(old-comment %cx (welp our-beak:main pax))
          =/  new=comment  [creator.old date-created.old content.old]
          :-  car

          :+  [pax %del ~]
            :-  /app/publish/notebooks/[book]/[note]/[comm]/publish-comment
            [%ins %publish-comment !>(new)]
          sob
        ==
      [[%pass /move-files %arvo %c %info q.byk.bol %& sob] cards]
    --
  ::
  ++  on-poke
    |=  [mar=mark vas=vase]
    ^-  (quip card _this)
    ?+  mar  (on-poke:def mar vas)
        %noun
      ?:  =(%print-state q.vas)
        ~&  state
        [~ this]
      ?:  =(%print-bowl q.vas)
        ~&  bol
        [~ this]
      [~ this]
    ::
        %handle-http-request
      =+  !<([id=@ta req=inbound-request:eyre] vas)
      :_  this
      %+  give-simple-payload:app    id
      %+  require-authorization:app  req
      handle-http-request:main
    ::
        %publish-action
      =^  cards  state
        (poke-publish-action:main !<(action vas))
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
      =^  cards  state
        (watch-notebook:main pax)
      [cards this]
    ::
        [%primary ~]  [~ this]
    ::
        [%publishtile ~]
      =/  jon=json
        (frond:enjs:format %notifications (numb:enjs:format tile-num))
      :_  this
      [%give %fact ~ %json !>(jon)]~
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ::
  ++  on-agent
    |=  [wir=wire sin=sign:agent:gall]
    ^-  (quip card _this)
    ?-    -.sin
        %poke-ack   (on-agent:def wir sin)
    ::  If our subscribe failed, delete notebook associated with subscription if
    ::  it exists
    ::
        %watch-ack
      ?.  ?=([%subscribe @ @ ~] wir)
        (on-agent:def wir sin)
      ?~  p.sin
        [~ this]
      =/  who=@p     (slav %p i.t.wir)
      =/  book=@tas  i.t.t.wir
      =/  del  [%del-book who book]
      :_  this(subs (~(del by subs) who book))
      [%give %fact [/primary]~ %publish-primary-delta !>(del)]~
    ::  Resubscribe to any subscription we get kicked from. The case of actually
    ::  getting banned from a notebook is handled by %watch-ack
    ::
        %kick
      ?+  wir
        [~ this]
      ::
          [%subscribe @ @ ~]
        =/  who=@p     (slav %p i.t.wir)
        =/  book=@tas  i.t.t.wir
        :_  this
        [%pass wir %agent [who %publish] %watch /notebook/[book]]~
      ::
          [%permissions ~]
        :_  this
        [%pass /permissions %agent [our.bol %permission-store] %watch /updates]~
      ::
          [%invites ~]
        :_  this
        :_  ~
        :*  %pass  /invites  %agent  [our.bol %invite-store]  %watch
            /invitatory/publish
        ==
      ==
    ::
        %fact
      ?+  wir  (on-agent:def wir sin)
          [%subscribe @ @ ~]
        =/  who=@p     (slav %p i.t.wir)
        =/  book-name  i.t.t.wir
        ?>  ?=(%publish-notebook-delta p.cage.sin)
        =^  cards  state
          (handle-notebook-delta:main !<(notebook-delta q.cage.sin) state)
        [cards this]
      ::
          [%permissions ~]
        =^  cards  state
          (handle-permission-update:main !<(permission-update q.cage.sin))
        [cards this]
      ::
          [%invites ~]
        =^  cards  state
          (handle-invite-update:main !<(invite-update q.cage.sin))
        [cards this]
      ::
          [%collection *]
        [~ this]
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
        [%read %info *]
      ?>  ?=([?(%b %c) %writ *] sin)
      =/  rot=riot:clay  +>.sin
      =^  cards  state
        (read-info:main t.t.wir rot)
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
  ++  on-fail  on-fail:def
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
++  read-info
  |=  [pax=path rot=riot:clay]
  ^-  (quip card _state)
  ?>  ?=([%app %publish %notebooks @ %publish-info ~] pax)
  =/  book-name  i.t.t.t.pax
  ?~  rot
    [~ state]
  =/  info=notebook-info  !<(notebook-info q.r.u.rot)
  =/  new-book=notebook
    :*  title.info
        description.info
        comments.info
        writers.info
        subscribers.info
        now.bol
        ~  ~  ~
    ==
  =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
  =/  delta=notebook-delta
    [%edit-book our.bol book-name new-book]
  =^  cards  state
    (handle-notebook-delta delta state)
  :_  state
  :*  [%pass (welp /read/info pax) %arvo %c %warp our.bol rif]
      cards
  ==
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
  =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
  =/  delta=notebook-delta
    [%edit-note our.bol book-name note-name new-note]
  =^  cards  state
    (handle-notebook-delta delta state)
  :_  state
  :*  [%pass (welp /read/note pax) %arvo %c %warp our.bol rif]
      cards
  ==
::
++  read-comment
  |=  [pax=path rot=riot:clay]
  ^-  (quip card _state)
  ?>  ?=([%app %publish %notebooks @ @ @ %publish-comment ~] pax)
  ?~  rot
    [~ state]
  =/  comment-date  (slaw %da i.t.t.t.t.t.pax)
  ?~  comment-date
    [~ state]
  =/  book-name      i.t.t.t.pax
  =/  note-name      i.t.t.t.t.pax
  =/  new-comment    !<(comment q.r.u.rot)
  =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
  =/  delta=notebook-delta
    [%edit-comment our.bol book-name note-name u.comment-date new-comment]
  =^  cards  state
    (handle-notebook-delta delta state)
  :_  state
  :*  [%pass (welp /read/comment pax) %arvo %c %warp our.bol rif]
      cards
  ==
::
++  filter-and-sort-paths
  |=  paths=(list path)
  ^-  (list path)
  %+  sort
    %+  skim  paths
    |=  pax=path
    ?|  ?=([%app %publish %notebooks @ %publish-info ~] pax)
        ?=([%app %publish %notebooks @ @ %udon ~] pax)
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
      [%app %publish %notebooks @ %publish-info ~]
    =/  book-name  i.t.t.t.pax
    =/  delta=notebook-delta  [%del-book our.bol book-name]
    =^  cards  sty  (handle-notebook-delta delta sty)
    [(weld cards cad) sty]
  ::
      [%app %publish %notebooks @ @ %udon ~]
    =/  book-name  i.t.t.t.pax
    =/  note-name  i.t.t.t.t.pax
    =/  book  (~(get by books.sty) book-name)
    ?~  book
      [cad sty]
    =.  notes.u.book  (~(del by notes.u.book) note-name)
    =/  delta=notebook-delta  [%del-note our.bol book-name note-name]
    =^  cards  sty  (handle-notebook-delta delta sty)
    [(weld cards cad) sty]
  ::
      [%app %publish %notebooks @ @ @ %publish-comment ~]
    =/  book-name  i.t.t.t.pax
    =/  note-name  i.t.t.t.t.pax
    =/  comment-date  (slaw %da i.t.t.t.t.t.pax)
    ?~  comment-date
      [cad sty]
    =/  delta=notebook-delta
      [%del-comment our.bol book-name note-name u.comment-date]
    =^  cards  sty  (handle-notebook-delta delta sty)
    [(weld cards cad) sty]
  ==
::
++  add-paths
  |=  paths=(list path)
  ^-  (quip card _state)
  %+  roll  paths
  |=  [pax=path cad=(list card) sty=_state]
  ^-  (quip card _state)
  ?+    pax  !!
      [%app %publish %notebooks @ %publish-info ~]
    =/  book-name  i.t.t.t.pax
    =/  info=notebook-info  .^(notebook-info %cx (welp our-beak pax))
    =*  title  title.info
    =*  description  description.info
    =/  new-book=notebook
      :*  title
          description
          comments.info
          writers.info
          subscribers.info
          now.bol
          ~  ~  ~
      ==
    =+  ^-  [grp-car=(list card) write-pax=path read-pax=path]
      ?:  =(writers.new-book /)
        =/  group-path  /~/(scot %p our.bol)/[book-name]
        (make-groups book-name [group-path ~ %.n %.n] title description)
      [~ writers.info subscribers.info]
    =.  writers.new-book      write-pax
    =.  subscribers.new-book  read-pax
    =+  ^-  [read-cards=(list card) notes=(map @tas note)]
      (watch-notes /app/publish/notebooks/[book-name])
    =.  notes.new-book  notes
    =/  delta=notebook-delta  [%add-book our.bol book-name new-book]
    =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
    =^  update-cards  sty  (handle-notebook-delta delta sty)
    :_  sty
    ;:  weld
      grp-car
      [%pass (welp /read/info pax) %arvo %c %warp our.bol rif]~
      read-cards
      update-cards
      cad
    ==
  ::
      [%app %publish %notebooks @ @ %udon ~]
    =/  book-name  i.t.t.t.pax
    =/  note-name  i.t.t.t.t.pax
    =/  new-note=note  (scry-note pax)
    =+  ^-  [read-cards=(list card) comments=(map @da comment)]
      (watch-comments /app/publish/notebooks/[book-name]/[note-name])
    =.  comments.new-note  comments
    =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
    =/  delta=notebook-delta
      [%add-note our.bol book-name note-name new-note]
    =^  update-cards  sty  (handle-notebook-delta delta sty)
    :_  sty
    ;:  weld
      [%pass (welp /read/note pax) %arvo %c %warp our.bol rif]~
      read-cards
      update-cards
      cad
    ==
  ::
      [%app %publish %notebooks @ @ @ %publish-comment ~]
    =/  book-name  i.t.t.t.pax
    =/  note-name  i.t.t.t.t.pax
    =/  comment-name  (slaw %da i.t.t.t.t.t.pax)
    ?~  comment-name
      [~ sty]
    =/  new-com  .^(comment %cx (welp our-beak pax))
    =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
    ::
    =/  delta=notebook-delta
      [%add-comment our.bol book-name note-name u.comment-name new-com]
    =^  update-cards  sty  (handle-notebook-delta delta sty)
    :_  sty
    ;:  weld
      [%pass (welp /read/comment pax) %arvo %c %warp our.bol rif]~
      update-cards
      cad
    ==
  ==
::
++  watch-notes
  |=  pax=path
  ^-  [(list card) (map @tas note)]
  =/  paths  .^((list path) %ct (weld our-beak pax))
  %+  roll  paths
  |=  [pax=path cards=(list card) notes=(map @tas note)]
  ?.  ?=([%app %publish %notebooks @ @ %udon ~] pax)
    [cards notes]
  =/  book-name  i.t.t.t.pax
  =/  note-name  i.t.t.t.t.pax
  =/  new-note   (scry-note pax)
  =^  comment-cards  comments.new-note
    (watch-comments /app/publish/notebooks/[book-name]/[note-name])
  =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
  :_  (~(put by notes) note-name new-note)
  ;:  weld
    [%pass (welp /read/note pax) %arvo %c %warp our.bol rif]~
    comment-cards
    cards
  ==
::
++  watch-comments
  |=  pax=path
  ^-  [(list card) (map @da comment)]
  =/  paths  .^((list path) %ct (weld our-beak pax))
  %+  roll  paths
  |=  [pax=path cards=(list card) comments=(map @da comment)]
  ?.  ?=([%app %publish %notebooks @ @ @ %publish-comment ~] pax)
    [cards comments]
  =/  comment-name  (slaw %da i.t.t.t.t.t.pax)
  ?~  comment-name
    [cards comments]
  =/  new-com  .^(comment %cx (welp our-beak pax))
  =/  rif=riff:clay  [q.byk.bol `[%next %x [%da now.bol] pax]]
  :_  (~(put by comments) u.comment-name new-com)
  [[%pass (welp /read/comment pax) %arvo %c %warp our.bol rif] cards]
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
  =/  front-idx  (add 3 (need (find ";>" (trip udon))))
  =/  front-matter
    (cat 3 (end 3 front-idx udon) 'dummy text\0a')
  =/  body  (cut 3 [front-idx (met 3 udon)] udon)
  =/  snippet=@t  (of-wain:format (scag 1 (to-wain:format body)))
  =/  meta=(each (map term knot) tang)
    %-  mule  |.
    %-  ~(run by inf:(static:cram (ream front-matter)))
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
  =/  date-created=@da  now.bol
  =?  date-created  ?=(%.y -.meta)
    %+  fall
      (biff (~(get by p.meta) %date-created) (slat %da))
    now.bol
  ::
  =/  last-modified=@da  now.bol
  =?  last-modified  ?=(%.y -.meta)
    %+  fall
      (biff (~(get by p.meta) %last-modified) (slat %da))
    now.bol
  ::
  :*  author
      title
      note-name
      date-created
      last-modified
      %.y
      udon
      snippet
      ~
  ==
::
++  handle-permission-update
  |=  upd=permission-update
  ^-  (quip card _state)
  ?.  ?=(?(%remove %add) -.upd)
    [~ state]
  =/  book=(unit @tas)
    %+  roll  ~(tap by books)
    |=  [[nom=@tas book=notebook] out=(unit @tas)]
    ?:  =(path.upd subscribers.book)
      `nom
    out
  ?~  book
    [~ state]
  :_  state
  %-  zing
  %+  turn  ~(tap in who.upd)
  |=  who=@p
  ?.  (allowed who %read u.book)
    [%give %kick [/notebook/[u.book]]~ `who]~
  ?:  ?|(?=(%remove -.upd) (is-managed path.upd))
    ~
  =/  uid  (sham %publish who u.book eny.bol)
  =/  inv=invite
    :*  our.bol  %publish  /notebook/[u.book]  who
        (crip "invite for notebook {<our.bol>}/{(trip u.book)}")
    ==
  =/  act=invite-action  [%invite /publish uid inv]
  [%pass / %agent [our.bol %invite-hook] %poke %invite-action !>(act)]~
::
++  handle-invite-update
  |=  upd=invite-update
  ^-  (quip card _state)
  ?+    -.upd
    [~ state]
  ::
      %delete
    =/  scry-pax
      /(scot %p our.bol)/invite-store/(scot %da now.bol)/invitatory/publish/noun
    =/  inv=(unit invitatory)  .^((unit invitatory) %gx scry-pax)
    ?~  inv
      [~ state]
    =.  tile-num  (sub tile-num ~(wyt by u.inv))
    =/  jon=json  (frond:enjs:format %notifications (numb:enjs:format tile-num))
    :_  state
    [%give %fact [/publishtile]~ %json !>(jon)]~
  ::
      %invite
    =.  tile-num  +(tile-num)
    =/  jon=json  (frond:enjs:format %notifications (numb:enjs:format tile-num))
    :_  state
    [%give %fact [/publishtile]~ %json !>(jon)]~
  ::
      %decline
    =?  tile-num  (gth tile-num 0)
      (dec tile-num)
    =/  jon=json  (frond:enjs:format %notifications (numb:enjs:format tile-num))
    :_  state
    [%give %fact [/publishtile]~ %json !>(jon)]~
  ::
      %accepted
    ?>  ?=([%notebook @ ~] path.invite.upd)
    =/  book  i.t.path.invite.upd
    =/  wir=wire  /subscribe/(scot %p ship.invite.upd)/[book]
    =?  tile-num  (gth tile-num 0)
      (dec tile-num)
    =/  jon=json  (frond:enjs:format %notifications (numb:enjs:format tile-num))
    :_  state
    :~  [%pass wir %agent [ship.invite.upd %publish] %watch path.invite.upd]
        [%give %fact [/publishtile]~ %json !>(jon)]
    ==
  ==
::
++  watch-notebook
  |=  pax=path
  ?>  ?=([%notebook @ ~] pax)
  =/  book-name  i.t.pax
  ?.  (allowed src.bol %read book-name)
    ~|("not permitted" !!)
  =/  book  (~(got by books) book-name)
  =/  delta=notebook-delta
    [%add-book our.bol book-name book]
  :_  state
  [%give %fact ~ %publish-notebook-delta !>(delta)]~
::
++  our-beak  /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)
::
++  allowed
  |=  [who=@p mod=?(%read %write) book=@tas]
  ^-  ?
  =/  scry-bek  /(scot %p our.bol)/permission-store/(scot %da now.bol)
  =/  book=notebook  (~(got by books) book)
  =/  scry-pax
    ?:  =(%read mod)
      subscribers.book
    writers.book
  =/  full-pax  :(weld scry-bek /permitted/(scot %p who) scry-pax /noun)
  .^(? %gx full-pax)
::
++  write-file
  |=  [pax=path cay=cage]
  ^-  card
  =.  pax  (weld our-beak pax)
  [%pass (weld /write pax) %arvo %c %info (foal:space:userlib pax cay)]
::
++  delete-file
  |=  pax=path
  ^-  card
  =.  pax  (weld our-beak pax)
  [%pass (weld /delete pax) %arvo %c %info (fray:space:userlib pax)]
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
++  group-hook-poke
  |=  act=group-hook-action
  ^-  card
  [%pass / %agent [our.bol %group-hook] %poke %group-hook-action !>(act)]
::
++  contact-view-create
  |=  [=path ships=(set ship) title=@t description=@t]
  =/  act  [%create path ships title description]
  ^-  card
  [%pass / %agent [our.bol %contact-view] %poke %contact-view-action !>(act)]
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
++  invite-poke
  |=  act=invite-action
  ^-  card
  [%pass / %agent [our.bol %invite-store] %poke %invite-action !>(act)]
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
  |=  [read=path write=path sec=rw-security]
  ^-  (list card)
  =+  ^-  [read-type=?(%black %white) write-type=?(%black %white)]
    ?-  sec
      %channel  [%black %black]
      %village  [%white %white]
      %journal  [%black %white]
      %mailbox  [%white %black]
    ==
  :~  (perm-group-hook-poke [%associate read [[read read-type] ~ ~]])
      (perm-group-hook-poke [%associate write [[write write-type] ~ ~]])
  ==
::
++  generate-invites
  |=  [book=@tas invitees=(set ship)]
  ^-  (list card)
  %+  turn  ~(tap in invitees)
  |=  who=ship
  =/  uid  (sham %publish who book eny.bol)
  =/  inv=invite
    :*  our.bol  %publish  /notebook/[book]  who
        (crip "invite for notebook {<our.bol>}/{(trip book)}")
    ==
  =/  act=invite-action  [%invite /publish uid inv]
  [%pass / %agent [our.bol %invite-hook] %poke %invite-action !>(act)]
::
++  make-groups
  |=  [book=@tas group=group-info title=@t about=@t]
  ^-  [(list card) write=path read=path]
  ?>  ?=(^ group-path.group)
  =/  scry-path
    ;:(weld /=group-store/(scot %da now.bol) group-path.group /noun)
  =/  grp  .^((unit ^group) %gx scry-path)
  ?:  use-preexisting.group
    ?~  grp  !!
    ?.  (is-managed group-path.group)  !!
    :_  [group-path.group group-path.group]
    :~  %-  perm-group-hook-poke
        [%associate group-path.group [[group-path.group %white] ~ ~]]
      ::
        (perm-hook-poke [%add-owned group-path.group group-path.group])
    ==
  ::
  ?:  make-managed.group
    ?^  grp  [~ group-path.group group-path.group]
    ?.  (is-managed group-path.group)  !!
    =/  whole-grp  (~(put in invitees.group) our.bol)
    :_  [group-path.group group-path.group]
    [(contact-view-create [group-path.group whole-grp title about])]~
  ::  make unmanaged group
  =*  write-path  group-path.group
  =/  read-path   (weld write-path /read)
  ?^  grp  [~ write-path read-path]
  ?:  (is-managed group-path.group)  !!
  :_  [write-path read-path]
  %-  zing
  :~  [(group-poke [%bundle write-path])]~
      [(group-poke [%bundle read-path])]~
      [(group-hook-poke [%add our.bol write-path])]~
      [(group-hook-poke [%add our.bol read-path])]~
      [(group-poke [%add (sy our.bol ~) write-path])]~
      (create-security read-path write-path %journal)
      [(perm-hook-poke [%add-owned write-path write-path])]~
      [(perm-hook-poke [%add-owned read-path read-path])]~
      (generate-invites book (~(del in invitees.group) our.bol))
  ==
::
++  poke-publish-action
  |=  act=action
  ^-  (quip card _state)
  ?-    -.act
      %new-book
    ?.  (team:title our.bol src.bol)
      ~|("action not permitted" !!)
    ?:  (~(has by books) book.act)
      ~|("notebook already exists: {<book.act>}" !!)
    =+  ^-  [cards=(list card) write-pax=path read-pax=path]
      (make-groups book.act group.act title.act about.act)
    =/  new-book=notebook-info
      :*  title.act
          about.act
          coms.act
          write-pax
          read-pax
      ==
    =/  pax=path  /app/publish/notebooks/[book.act]/publish-info
    :_  state
    [(write-file pax %publish-info !>(new-book)) cards]
  ::
      %new-note
    ?:  &(=(src.bol our.bol) !=(our.bol who.act))
      :_  state
      [%pass /forward %agent [who.act %publish] %poke %publish-action !>(act)]~
    =/  book=(unit notebook)  (~(get by books) book.act)
    ?~  book
      ~|("nonexistent notebook {<book.act>}" !!)
    ?:  (~(has by notes.u.book) note.act)
      ~|("note already exists: {<note.act>}" !!)
    ?.  ?|  (team:title our.bol src.bol)
            (allowed src.bol %write book.act)
        ==
      ~|("action not permitted" !!)
    =/  pax=path  /app/publish/notebooks/[book.act]/[note.act]/udon
    =/  front=(map knot cord)
      %-  my
      :~  title+title.act
          author+(scot %p src.bol)
          date-created+(scot %da now.bol)
          last-modified+(scot %da now.bol)
      ==
    =/  file=@t   (add-front-matter front body.act)
    :_  state
    [(write-file pax %udon !>(file))]~
  ::
      %new-comment
    ?:  &(=(src.bol our.bol) !=(our.bol who.act))
      :_  state
      [%pass /forward %agent [who.act %publish] %poke %publish-action !>(act)]~
    =/  book=(unit notebook)  (~(get by books) book.act)
    ?~  book
      ~|("nonexistent notebook {<book.act>}" !!)
    ?.  (~(has by notes.u.book) note.act)
      ~|("nonexistent note {<note.act>}" !!)
    ?.  ?&  ?|  (team:title our.bol src.bol)
                (allowed src.bol %read book.act)
            ==
            comments.u.book
        ==
      ~|("action not permitted" !!)
    =/  pax=path
      %+  weld  /app/publish/notebooks
      /[book.act]/[note.act]/(scot %da now.bol)/publish-comment
    =/  new-comment=comment
      :*  author=src.bol
          date-created=now.bol
          content=body.act
      ==
    :_  state
    [(write-file pax %publish-comment !>(new-comment))]~
  ::
      %edit-book
    ?.  (team:title our.bol src.bol)
      ~|("action not permitted" !!)
    =/  book  (~(get by books) book.act)
    ?~  book
      ~|("nonexistent notebook" !!)
    =+  ^-  [cards=(list card) write-pax=path read-pax=path]
      ?~  group.act
        [~ writers.u.book subscribers.u.book]
      (make-groups book.act u.group.act title.act about.act)
    =/  new-info=notebook-info
      :*  title.act
          about.act
          coms.act
          write-pax
          read-pax
      ==
    =/  pax=path  /app/publish/notebooks/[book.act]/publish-info
    :_  state
    [(write-file pax %publish-info !>(new-info)) cards]
  ::
      %edit-note
    ?:  &(=(src.bol our.bol) !=(our.bol who.act))
      :_  state
      [%pass /forward %agent [who.act %publish] %poke %publish-action !>(act)]~
    =/  book=(unit notebook)  (~(get by books) book.act)
    ?~  book
      ~|("nonexistent notebook {<book.act>}" !!)
    =/  note=(unit note)  (~(get by notes.u.book) note.act)
    ?~  note
      ~|("nonexistent note: {<note.act>}" !!)
    ?.  ?|  (team:title our.bol src.bol)
            ?&  =(author.u.note src.bol)
                (allowed src.bol %write book.act)
            ==
        ==
      ~|("action not permitted" !!)
    =/  pax=path  /app/publish/notebooks/[book.act]/[note.act]/udon
    =/  front=(map knot cord)
      %-  my
      :~  title+title.act
          author+(scot %p src.bol)
          date-created+(scot %da date-created.u.note)
          last-modified+(scot %da now.bol)
      ==
    =/  file=@t   (add-front-matter front body.act)
    :_  state
    [(write-file pax %udon !>(file))]~
  ::
      %edit-comment
    ?:  &(=(src.bol our.bol) !=(our.bol who.act))
      :_  state
      [%pass /forward %agent [who.act %publish] %poke %publish-action !>(act)]~
    =/  book=(unit notebook)  (~(get by books) book.act)
    ?~  book
      ~|("nonexistent notebook {<book.act>}" !!)
    =/  not=(unit note)  (~(get by notes.u.book) note.act)
    ?~  not
      ~|("nonexistent note {<note.act>}" !!)
    =/  com=(unit comment)
      (~(get by comments.u.not) (slav %da comment.act))
    ?~  com
      ~|("nonexistent comment {<comment.act>}" !!)
    ?.  ?|  (team:title our.bol src.bol)
            ?&  =(author.u.com src.bol)
                (allowed src.bol %read book.act)
            ==
        ==
      ~|("action not permitted" !!)
    =/  pax=path
      %+  weld  /app/publish/notebooks
      /[book.act]/[note.act]/[comment.act]/publish-comment
    =/  new-comment  .^(comment %cx (weld our-beak pax))
    =.  content.new-comment    body.act
    :_  state
    [(write-file pax %publish-comment !>(new-comment))]~
  ::
      %del-book
    ?.  (team:title our.bol src.bol)
      ~|("action not permitted" !!)
    =/  book=(unit notebook)  (~(get by books) book.act)
    ?~  book
      ~|("nonexistent notebook {<book.act>}" !!)
    =/  pax=path  /app/publish/notebooks/[book.act]
    ?>  ?=(^ writers.u.book)
    ?>  ?=(^ subscribers.u.book)
    =/  cards=(list card)
      :~  (delete-dir pax)
          (perm-hook-poke [%remove writers.u.book])
          (perm-hook-poke [%remove subscribers.u.book])
      ==
    =?  cards  =('~' i.writers.u.book)
      [(group-poke [%unbundle writers.u.book]) cards]
    =?  cards  =('~' i.subscribers.u.book)
      [(group-poke [%unbundle subscribers.u.book]) cards]
    [cards state]
  ::
      %del-note
    ?:  &(=(src.bol our.bol) !=(our.bol who.act))
      :_  state
      [%pass /forward %agent [who.act %publish] %poke %publish-action !>(act)]~
    =/  book=(unit notebook)  (~(get by books) book.act)
    ?~  book
      ~|("nonexistent notebook {<book.act>}" !!)
    =/  note=(unit note)  (~(get by notes.u.book) note.act)
    ?~  note
      ~|("nonexistent note: {<note.act>}" !!)
    ?.  ?|  (team:title our.bol src.bol)
            ?&  =(author.u.note src.bol)
                (allowed src.bol %write book.act)
            ==
        ==
      ~|("action not permitted" !!)
    =/  pax=path  /app/publish/notebooks/[book.act]/[note.act]/udon
    :_  state
    [(delete-file pax)]~
  ::
      %del-comment
    ?:  &(=(src.bol our.bol) !=(our.bol who.act))
      :_  state
      [%pass /forward %agent [who.act %publish] %poke %publish-action !>(act)]~
    =/  book=(unit notebook)  (~(get by books) book.act)
    ?~  book
      ~|("nonexistent notebook {<book.act>}" !!)
    =/  note=(unit note)  (~(get by notes.u.book) note.act)
    ?~  note
      ~|("nonexistent note {<note.act>}" !!)
    =/  comment=(unit comment)
      (~(get by comments.u.note) (slav %da comment.act))
    ?~  comment
      ~|("nonexistent comment {<comment.act>}" !!)
    ?.  ?|  (team:title our.bol src.bol)
            ?&  =(author.u.comment src.bol)
                (allowed src.bol %read book.act)
            ==
        ==
      ~|("action not permitted" !!)
    =/  pax=path
      %+  weld  /app/publish/notebooks
      /[book.act]/[note.act]/[comment.act]/publish-comment
    :_  state
    [(delete-file pax)]~
  ::
      %subscribe
    ?>  (team:title our.bol src.bol)
    =/  wir=wire  /subscribe/(scot %p who.act)/[book.act]
    :_  state
    [%pass wir %agent [who.act %publish] %watch /notebook/[book.act]]~
  ::
      %unsubscribe
    ?>  (team:title our.bol src.bol)
    =/  wir=wire  /subscribe/(scot %p who.act)/[book.act]
    =/  del=primary-delta  [%del-book who.act book.act]
    :_  state(subs (~(del by subs) who.act book.act))
    :~  `card`[%pass wir %agent [who.act %publish] %leave ~]
        `card`[%give %fact [/primary]~ %publish-primary-delta !>(del)]
    ==
  ::
      %read
    ?>  (team:title our.bol src.bol)
    =/  book=(unit notebook)
      ?:  =(our.bol who.act)
        (~(get by books) book.act)
      (~(get by subs) who.act book.act)
    ?~  book
      ~|("nonexistent notebook: {<book.act>}" !!)
    =/  not=(unit note)  (~(get by notes.u.book) note.act)
    ?~  not
      ~|("nonexistent note: {<note.act>}" !!)
    =?  tile-num  &(!read.u.not (gth tile-num 0))
      (dec tile-num)
    =.  read.u.not  %.y
    =.  notes.u.book  (~(put by notes.u.book) note.act u.not)
    =?  books  =(our.bol who.act)
      (~(put by books) book.act u.book)
    =?  subs   !=(our.bol who.act)
      (~(put by subs) [who.act book.act] u.book)
    =/  jon=json
      (frond:enjs:format %notifications (numb:enjs:format tile-num))
    :_  state
    :~  [%give %fact [/primary]~ %publish-primary-delta !>(act)]
        [%give %fact [/publishtile]~ %json !>(jon)]
    ==
  ==
::
++  get-notebook
  |=  [host=@p book-name=@tas sty=_state]
  ^-  (unit notebook)
  ?:  =(our.bol host)
    (~(get by books.sty) book-name)
  (~(get by subs.sty) host book-name)
::
++  get-unread
  |=  book=notebook
  ^-  @ud
  %+  roll  ~(tap by notes.book)
  |=  [[nom=@tas not=note] out=@ud]
  ?:  read.not
    out
  +(out)
::
++  emit-updates-and-state
  |=  [host=@p book-name=@tas book=notebook del=notebook-delta sty=_state]
  ^-  (quip card _state)
  ?:  =(our.bol host)
    :_  sty(books (~(put by books.sty) book-name book))
    :~  [%give %fact [/notebook/[book-name]]~ %publish-notebook-delta !>(del)]
        [%give %fact [/primary]~ %publish-primary-delta !>(del)]
    ==
  =/  jon=json
    (frond:enjs:format %notifications (numb:enjs:format tile-num))
  :_  sty(subs (~(put by subs.sty) [host book-name] book))
  :~  [%give %fact [/primary]~ %publish-primary-delta !>(del)]
      [%give %fact [/publishtile]~ %json !>(jon)]
  ==
::
++  emit-metadata
  |=  del=metadata-delta
  ^-  (list card)
  |^
  ?-  -.del
      %add
    =/  preexisting  (metadata-scry group-path.del app-path.del)
    =/  meta=metadata
      %*  .  *metadata
          title         title.del
          description   desc.del
          date-created  created.del
          creator       author.del
      ==
    ?~  preexisting
      (add group-path.del app-path.del meta)
    =.  color.meta  color.u.preexisting
    (add group-path.del app-path.del meta)
  ::
      %remove
    =/  app-path  [(scot %p author.del) /[book.del]]
    =/  group-path  (group-from-book app-path)
    [(metadata-poke [%remove group-path [%publish app-path]])]~
  ==
  ::
  ++  add
    |=  [group-path=path app-path=path =metadata]
    ^-  (list card)
    [(metadata-poke [%add group-path [%publish app-path] metadata])]~
  ::
  ++  metadata-poke
    |=  act=metadata-action
    ^-  card
    [%pass / %agent [our.bol %metadata-hook] %poke %metadata-action !>(act)]
  ::
  ++  metadata-scry
    |=  [group-path=path app-path=path]
    ^-  (unit metadata)
    ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)  ~
    .^  (unit metadata)
      %gx
      (scot %p our.bol)
      %metadata-store
      (scot %da now.bol)
      %metadata
      (scot %t (spat group-path))
      %publish
      (scot %t (spat app-path))
      /noun
    ==
  ::
  ++  group-from-book
    |=  app-path=path
    ^-  path
    ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)
      ?:  ?=([@ ^] app-path)
        ~&  [%assuming-ported-legacy-publish app-path]
        [%'~' app-path]
      ~|  [%weird-publish app-path]
      !!
    =/  resource-indices
      .^  (jug resource group-path)
        %gy
        (scot %p our.bol)
        %metadata-store
        (scot %da now.bol)
        /resource-indices
      ==
    =/  groups=(set path)  (~(got by resource-indices) [%publish app-path])
    (snag 0 ~(tap in groups))
  --
::
++  metadata-hook-poke
  |=  act=metadata-hook-action
  ^-  card
  :*  %pass  /  %agent
      [our.bol %metadata-hook]
      %poke  %metadata-hook-action
      !>(act)
  ==
::
++  handle-notebook-delta
  |=  [del=notebook-delta sty=_state]
  ^-  (quip card _state)
  ?-    -.del
      %add-book
    =.  tile-num    (add tile-num (get-unread data.del))
    ?:  =(our.bol host.del)
      =^  cards  state
        (emit-updates-and-state host.del book.del data.del del sty)
      :_  state
      %-  zing
      :~  cards
          [(metadata-hook-poke [%add-owned writers.data.del])]~
          %-  emit-metadata
          :*  %add
              writers.data.del
              [(scot %p host.del) /[book.del]]
              title.data.del
              description.data.del
              host.del
              date-created.data.del
          ==
      ==
    =^  cards  state
      (emit-updates-and-state host.del book.del data.del del sty)
    :_  state
    :*  (group-hook-poke [%add host.del writers.data.del])
        (group-hook-poke [%add host.del subscribers.data.del])
        (metadata-hook-poke [%add-synced host.del writers.data.del])
        cards
    ==
  ::
      %add-note
    =/  book=(unit notebook)
      (get-notebook host.del book.del sty)
    ?~  book
      [~ sty]
    =.  read.data.del  =(our.bol author.data.del)
    =?  tile-num.sty  !read.data.del
      +(tile-num.sty)
    =.  notes.u.book  (~(put by notes.u.book) note.del data.del)
    (emit-updates-and-state host.del book.del u.book del sty)
  ::
      %add-comment
    =/  book=(unit notebook)
      (get-notebook host.del book.del sty)
    ?~  book
      [~ sty]
    =/  note  (~(get by notes.u.book) note.del)
    ?~  note
      [~ sty]
    =.  comments.u.note  (~(put by comments.u.note) comment-date.del data.del)
    =.  notes.u.book  (~(put by notes.u.book) note.del u.note)
    (emit-updates-and-state host.del book.del u.book del sty)
  ::
      %edit-book
    =/  old-book=(unit notebook)
      (get-notebook host.del book.del sty)
    ?~  old-book
      [~ sty]
    =/  new-book=notebook
      %=  data.del
        date-created  date-created.u.old-book
        notes         notes.u.old-book
        order         order.u.old-book
      ==
    =^  cards  state
      (emit-updates-and-state host.del book.del new-book del sty)
    :_  state
    %+  weld  cards
    %-  emit-metadata
    :*  %add
        writers.new-book
        [(scot %p host.del) /[book.del]]
        title.new-book
        description.new-book
        host.del
        date-created.new-book
    ==
  ::
      %edit-note
    =/  book=(unit notebook)
      (get-notebook host.del book.del sty)
    ?~  book
      [~ sty]
    =/  old-note  (~(get by notes.u.book) note.del)
    ?~  old-note
      [~ sty]
    =/  new-note=note
      %=  data.del
        date-created  date-created.u.old-note
        comments      comments.u.old-note
        read          read.u.old-note
      ==
    =.  notes.u.book  (~(put by notes.u.book) note.del new-note)
    (emit-updates-and-state host.del book.del u.book del sty)
  ::
      %edit-comment
    =/  book=(unit notebook)
      (get-notebook host.del book.del sty)
    ?~  book
      [~ sty]
    =/  note  (~(get by notes.u.book) note.del)
    ?~  note
      [~ sty]
    =.  comments.u.note  (~(put by comments.u.note) comment-date.del data.del)
    =.  notes.u.book  (~(put by notes.u.book) note.del u.note)
    (emit-updates-and-state host.del book.del u.book del sty)
  ::
      %del-book
    =/  book=(unit notebook)
      (get-notebook host.del book.del sty)
    ?~  book  [~ sty]
    ?.  =(our.bol host.del)
      =.  tile-num
        %+  sub  tile-num
        (get-unread (~(got by subs) host.del book.del))
      =/  jon=json
        (frond:enjs:format %notifications (numb:enjs:format tile-num.sty))
      :_  sty(subs (~(del by subs.sty) host.del book.del))
      %+  welp
        :~  [%give %fact [/primary]~ %publish-primary-delta !>(del)]
            [%give %fact [/publishtile]~ %json !>(jon)]
        ==
      ?:  (is-managed writers.u.book)  ~
      [(metadata-hook-poke [%remove writers.u.book])]~
    :_  sty(books (~(del by books.sty) book.del))
    %-  zing
    :~  [%give %fact [/notebook/[book.del]]~ %publish-notebook-delta !>(del)]~
        [%give %fact [/primary]~ %publish-primary-delta !>(del)]~
        (emit-metadata %remove host.del book.del)
      ::
        ?:  (is-managed writers.u.book)  ~
        [(metadata-hook-poke [%remove writers.u.book])]~
    ==
  ::
      %del-note
    =/  book=(unit notebook)
      (get-notebook host.del book.del sty)
    ?~  book
      [~ sty]
    =/  not=note  (~(got by notes.u.book) note.del)
    =?  tile-num  &(!read.not (gth tile-num 0))
      (dec tile-num)
    =.  notes.u.book  (~(del by notes.u.book) note.del)
    (emit-updates-and-state host.del book.del u.book del sty)
  ::
      %del-comment
    =/  book=(unit notebook)
      (get-notebook host.del book.del sty)
    ?~  book
      [~ sty]
    =/  note  (~(get by notes.u.book) note.del)
    ?~  note
      [~ sty]
    =.  comments.u.note  (~(del by comments.u.note) comment.del)
    =.  notes.u.book     (~(put by notes.u.book) note.del u.note)
    (emit-updates-and-state host.del book.del u.book del sty)
  ==
::
++  get-subscribers-json
  |=  book=@tas
  ^-  json
  :-  %a
  %+  roll  ~(val by sup.bol)
  |=  [[who=@p pax=path] out=(list json)]
  ^-  (list json)
  ?.  ?=([%notebook @ ~] pax)  out
  ?.  =(book i.t.pax)  out
  [[%s (scot %p who)] out]
::
++  get-notebook-json
  |=  [host=@p book-name=@tas]
  ^-  (unit json)
  =,  enjs:format
  =/  book=(unit notebook)
    ?:  =(our.bol host)
      (~(get by books) book-name)
    (~(get by subs) host book-name)
  ?~  book
    ~
  =/  notebook-json  (notebook-full-json host book-name u.book)
  ?>  ?=(%o -.notebook-json)
  =.  p.notebook-json
    (~(uni by p.notebook-json) (notes-page notes.u.book 0 50))
  =.  p.notebook-json
    (~(put by p.notebook-json) %subscribers (get-subscribers-json book-name))
  =/  notebooks-json  (notebooks-map-json our.bol books subs)
  ?>  ?=(%o -.notebooks-json)
  =/  host-books-json  (~(got by p.notebooks-json) (scot %p host))
  ?>  ?=(%o -.host-books-json)
  =.  p.host-books-json  (~(put by p.host-books-json) book-name notebook-json)
  =.  p.notebooks-json
    (~(put by p.notebooks-json) (scot %p host) host-books-json)
  `(pairs notebooks+notebooks-json ~)
::
++  get-note-json
  |=  [host=@p book-name=@tas note-name=@tas]
  ^-  (unit json)
  =,  enjs:format
  =/  book=(unit notebook)
    ?:  =(our.bol host)
      (~(get by books) book-name)
    (~(get by subs) host book-name)
  ?~  book
    ~
  =/  note=(unit note)  (~(get by notes.u.book) note-name)
  ?~  note
    ~
  =/  notebook-json  (notebook-full-json host book-name u.book)
  ?>  ?=(%o -.notebook-json)
  =/  note-json  (note-presentation-json u.book note-name u.note)
  =.  p.notebook-json  (~(uni by p.notebook-json) note-json)
  =/  notebooks-json  (notebooks-map-json our.bol books subs)
  ?>  ?=(%o -.notebooks-json)
  =/  host-books-json  (~(got by p.notebooks-json) (scot %p host))
  ?>  ?=(%o -.host-books-json)
  =.  p.host-books-json  (~(put by p.host-books-json) book-name notebook-json)
  =.  p.notebooks-json
    (~(put by p.notebooks-json) (scot %p host) host-books-json)
  `(pairs notebooks+notebooks-json ~)
::
++  is-managed
  |=  =path
  ^-  ?
  ?>  ?=(^ path)
  !=(i.path '~')
::
++  handle-http-request
  |=  req=inbound-request:eyre
  ^-  simple-payload:http
  =/  url  (parse-request-line url.request.req)
  ?+    url
      not-found:gen
  ::
      [[[~ %png] [%'~publish' @t ~]] ~]
    =/  filename=@t  i.t.site.url
    =/  img=(unit @t)  (~(get by images) filename)
    ?~  img
      not-found:gen
    (png-response:gen (as-octs:mimes:html u.img))
  ::
      [[[~ %css] [%'~publish' %index ~]] ~]
    (css-response:gen css)
  ::
      [[[~ %js] [%'~publish' %index ~]] ~]
    (js-response:gen js)
  ::
      [[[~ %js] [%'~publish' %tile ~]] ~]
    (js-response:gen tile-js)
  ::
  ::  pagination endpoints
  ::
  ::  all notebooks, short form
      [[[~ %json] [%'~publish' %notebooks ~]] ~]
    %-  json-response:gen
    %-  json-to-octs
    (notebooks-map-json our.bol books subs)
  ::
  ::  notes pagination
      [[[~ %json] [%'~publish' %notes @ @ @ @ ~]] ~]
    =/  host=(unit @p)  (slaw %p i.t.t.site.url)
    ?~  host
      not-found:gen
    =/  book-name  i.t.t.t.site.url
    =/  book=(unit notebook)
      ?:  =(our.bol u.host)
        (~(get by books) book-name)
      (~(get by subs) u.host book-name)
    ?~  book
      not-found:gen
    =/  start  (rush i.t.t.t.t.site.url dem)
    ?~  start
      not-found:gen
    =/  length  (rush i.t.t.t.t.t.site.url dem)
    ?~  length
      not-found:gen
    %-  json-response:gen
    %-  json-to-octs
    :-  %o
    (notes-page notes.u.book u.start u.length)
  ::
  ::  comments pagination
      [[[~ %json] [%'~publish' %comments @ @ @ @ @ ~]] ~]
    =/  host=(unit @p)  (slaw %p i.t.t.site.url)
    ?~  host
      not-found:gen
    =/  book-name  i.t.t.t.site.url
    =/  book=(unit notebook)
      ?:  =(our.bol u.host)
        (~(get by books) book-name)
      (~(get by subs) u.host book-name)
    ?~  book
      not-found:gen
    =/  note-name  i.t.t.t.t.site.url
    =/  note=(unit note)  (~(get by notes.u.book) note-name)
    ?~  note
      not-found:gen
    =/  start  (rush i.t.t.t.t.t.site.url dem)
    ?~  start
      not-found:gen
    =/  length  (rush i.t.t.t.t.t.t.site.url dem)
    ?~  length
      not-found:gen
    %-  json-response:gen
    %-  json-to-octs
    (comments-page comments.u.note u.start u.length)
  ::
  ::  single notebook with initial 50 notes in short form, as json
      [[[~ %json] [%'~publish' @ @ ~]] ~]
    =,  enjs:format
    =/  host=(unit @p)  (slaw %p i.t.site.url)
    ?~  host
      not-found:gen
    =/  book-name  i.t.t.site.url
    =/  book=(unit notebook)
      ?:  =(our.bol u.host)
        (~(get by books) book-name)
      (~(get by subs) u.host book-name)
    ?~  book
      not-found:gen
    =/  notebook-json  (notebook-full-json u.host book-name u.book)
    ?>  ?=(%o -.notebook-json)
    =.  p.notebook-json
      (~(uni by p.notebook-json) (notes-page notes.u.book 0 50))
    =.  p.notebook-json
      (~(put by p.notebook-json) %subscribers (get-subscribers-json book-name))
    =/  jon=json  (pairs notebook+notebook-json ~)
    (json-response:gen (json-to-octs jon))
  ::
  ::  single note, with initial 50 comments, as json
      [[[~ %json] [%'~publish' @ @ @ ~]] ~]
    =,  enjs:format
    =/  host=(unit @p)  (slaw %p i.t.site.url)
    ?~  host
      not-found:gen
    =/  book-name  i.t.t.site.url
    =/  book=(unit notebook)
      ?:  =(our.bol u.host)
        (~(get by books) book-name)
      (~(get by subs) u.host book-name)
    ?~  book
      not-found:gen
    =/  note-name  i.t.t.t.site.url
    =/  note=(unit note)  (~(get by notes.u.book) note-name)
    ?~  note
      not-found:gen
    =/  jon=json  o+(note-presentation-json u.book note-name u.note)
    (json-response:gen (json-to-octs jon))
  ::
  ::  presentation endpoints
  ::
  ::  all notebooks, short form, wrapped in html
      [[~ [%'~publish' ?(~ [%join *] [%new ~])]] ~]
    =,  enjs:format
    =/  jon=json  (pairs notebooks+(notebooks-map-json our.bol books subs) ~)
    (manx-response:gen (index jon))
  ::
  ::  single notebook, with initial 50 notes in short form, wrapped in html
      [[~ [%'~publish' %notebook @ @ *]] ~]
    =/  host=(unit @p)  (slaw %p i.t.t.site.url)
    ?~  host
      not-found:gen
    =/  book-name  i.t.t.t.site.url
    =/  book-json=(unit json)  (get-notebook-json u.host book-name)
    ?~  book-json
      not-found:gen
    (manx-response:gen (index u.book-json))
  ::
  ::  single notebook, with initial 50 notes in short form, wrapped in html
      [[~ [%'~publish' %popout %notebook @ @ *]] ~]
    =/  host=(unit @p)  (slaw %p i.t.t.t.site.url)
    ?~  host
      not-found:gen
    =/  book-name  i.t.t.t.t.site.url
    =/  book-json=(unit json)  (get-notebook-json u.host book-name)
    ?~  book-json
      not-found:gen
    (manx-response:gen (index u.book-json))
  ::
  ::  single note, with initial 50 comments, wrapped in html
      [[~ [%'~publish' %note @ @ @ *]] ~]
    =/  host=(unit @p)  (slaw %p i.t.t.site.url)
    ?~  host
      not-found:gen
    =/  book-name  i.t.t.t.site.url
    =/  note-name  i.t.t.t.t.site.url
    =/  note-json=(unit json)  (get-note-json u.host book-name note-name)
    ?~  note-json
      not-found:gen
    (manx-response:gen (index u.note-json))
  ::
  ::  single note, with initial 50 comments, wrapped in html
      [[~ [%'~publish' %popout %note @ @ @ *]] ~]
    =/  host=(unit @p)  (slaw %p i.t.t.t.site.url)
    ?~  host
      not-found:gen
    =/  book-name  i.t.t.t.t.site.url
    =/  note-name  i.t.t.t.t.t.site.url
    =/  note-json=(unit json)  (get-note-json u.host book-name note-name)
    ?~  note-json
      not-found:gen
    (manx-response:gen (index u.note-json))
  ==
::
--
