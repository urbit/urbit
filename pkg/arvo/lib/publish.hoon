/-  sur=publish
/+  elem-to-react-json
^?
=<  [. sur]
=,  sur
|%
::
++  enjs
  =,  enjs:format
  |%
  ::
  ++  tang
    |=  tan=^tang
    %-  wall
    %-  zing
    %+  turn  tan
    |=  a=^tank
    (wash [0 80] a)
  ::
  ++  note-build
    |=  build=(each manx ^tang)
    ^-  json
    ?:  ?=(%.y -.build)
      %-  pairs
      :~  success+b+%.y
          result+(elem-to-react-json p.build)
      ==
    %-  pairs
    :~  success+b+%.n
        result+(tang p.build)
    ==
  ::
  ++  notebooks-list
    |=  [our=@p books=(map @tas notebook) subs=(map [@p @tas] notebook)]
    ^-  json
    :-  %a
    %+  weld
      %+  turn  ~(tap by books)
      |=  [name=@tas book=notebook]
      (notebook-short book)
    %+  turn  ~(tap by subs)
    |=  [[host=@p name=@tas] book=notebook]
    (notebook-short book)
  ::
  ++  notebooks-map
    |=  [our=@p books=(map [@p @tas] notebook)]
    ^-  json
    =/  notebooks-map=json
      %-  ~(rep by books)
      |=  [[[host=@p book-name=@tas] book=notebook] out=json]
      ^-  json
      =/  host-ta  (scot %p host)
      ?~  out
        (frond host-ta (frond book-name (notebook-short book)))
      ?>  ?=(%o -.out)
      =/  books  (~(get by p.out) host-ta)
      ?~  books
        :-  %o
        (~(put by p.out) host-ta (frond book-name (notebook-short book)))
      ?>  ?=(%o -.u.books)
      =.  p.u.books  (~(put by p.u.books) book-name (notebook-short book))
      :-  %o
      (~(put by p.out) host-ta u.books)
    =?  notebooks-map  ?=(~ notebooks-map)
      [%o ~]
    notebooks-map
  ::
  ++  notebook-short
    |=  book=notebook
    ^-  json
    %-  pairs
    :~  title+s+title.book
        date-created+(time date-created.book)
        about+s+description.book
        num-notes+(numb ~(wyt by notes.book))
        num-unread+(numb (count-unread notes.book))
        comments+b+comments.book
        writers-group-path+s+(spat writers.book)
        subscribers-group-path+s+(spat subscribers.book)
    ==
  ::
  ++  notebook-full
    |=  [host=@p book-name=@tas book=notebook]
    ^-  json
    %-  pairs
    :~  title+s+title.book
        about+s+description.book
        date-created+(time date-created.book)
        num-notes+(numb ~(wyt by notes.book))
        num-unread+(numb (count-unread notes.book))
        notes-by-date+(notes-by-date notes.book)
        comments+b+comments.book
        writers-group-path+s+(spat writers.book)
        subscribers-group-path+s+(spat subscribers.book)
    ==
  ::
  ++  note-presentation
    |=  [book=notebook note-name=@tas not=note]
    ^-  (map @t json)
    =/  notes-list=(list [@tas note])
      %+  sort  ~(tap by notes.book)
      |=  [[@tas n1=note] [@tas n2=note]]
      (gte date-created.n1 date-created.n2)
    =/  idx=@  (need (find [note-name not]~ notes-list))
    =/  next=(unit [name=@tas not=note])
      ?:  =(idx 0)  ~
      `(snag (dec idx) notes-list)
    =/  prev=(unit [name=@tas not=note])
      ?:  =(+(idx) (lent notes-list))  ~
      `(snag +(idx) notes-list)
    =/  current=json  (note-full note-name not)
    ?>  ?=(%o -.current)
    =.  p.current  (~(put by p.current) %prev-note ?~(prev ~ s+name.u.prev))
    =.  p.current  (~(put by p.current) %next-note ?~(next ~ s+name.u.next))
    =/  notes=(list [@t json])  [note-name current]~
    =?  notes  ?=(^ prev)
      [[name.u.prev (note-short name.u.prev not.u.prev)] notes]
    =?  notes  ?=(^ next)
      [[name.u.next (note-short name.u.next not.u.next)] notes]
    %-  my
    :~  notes+(pairs notes)
        notes-by-date+a+(turn notes-list |=([name=@tas *] s+name))
    ==
  ::
  ++  note-full
    |=  [note-name=@tas =note]
    ^-  json
    %-  pairs
    :~  note-id+s+note-name
        author+s+(scot %p author.note)
        title+s+title.note
        date-created+(time date-created.note)
        snippet+s+snippet.note
        file+s+file.note
        num-comments+(numb ~(wyt by comments.note))
        comments+(comments-page comments.note 0 50)
        read+b+read.note
        pending+b+pending.note
    ==
  ::
  ++  notes-by-date
    |=  notes=(map @tas note)
    ^-  json
    =/  notes-list=(list [@tas note])
      %+  sort  ~(tap by notes)
      |=  [[@tas n1=note] [@tas n2=note]]
      (gte date-created.n1 date-created.n2)
    :-  %a
    %+  turn  notes-list
    |=  [name=@tas note]
    ^-  json
    [%s name]
  ::
  ++  note-short
    |=  [note-name=@tas =note]
    ^-  json
    %-  pairs
    :~  note-id+s+note-name
        author+s+(scot %p author.note)
        title+s+title.note
        date-created+(time date-created.note)
        num-comments+(numb ~(wyt by comments.note))
        read+b+read.note
        snippet+s+snippet.note
        pending+b+pending.note
    ==
  ::
  ++  notes-page
    |=  [notes=(map @tas note) start=@ud length=@ud]
    ^-  (map @t json)
    =/  notes-list=(list [@tas note])
      %+  sort  ~(tap by notes)
      |=  [[@tas n1=note] [@tas n2=note]]
      (gte date-created.n1 date-created.n2)
    %-  my
    :~  notes-by-date+a+(turn notes-list |=([name=@tas *] s+name))
        notes+o+(^notes-list (scag length (slag start notes-list)))
    ==
  ::
  ++  notes-list
    |=  notes=(list [@tas note])
    ^-  (map @t json)
    %+  roll  notes
    |=  [[name=@tas not=note] out-map=(map @t json)]
    ^-  (map @t json)
    (~(put by out-map) name (note-short name not))
  ::
  ++  comments-page
    |=  [comments=(map @da ^comment) start=@ud end=@ud]
    ^-  json
    =/  coms=(list [@da ^comment])
      %+  sort  ~(tap by comments)
      |=  [[d1=@da ^comment] [d2=@da ^comment]]
      (gte d1 d2)
    %-  comments-list
    (scag end (slag start coms))
  ::
  ++  comments-list
    |=  comments=(list [@da ^comment])
    ^-  json
    :-  %a
    (turn comments comment)
  ::
  ++  comment
    |=  [date=@da com=^comment]
    ^-  json
    %+  frond
      (scot %da date)
    %-  pairs
    :~  author+s+(scot %p author.com)
        date-created+(time date-created.com)
        content+s+content.com
        pending+b+pending.com
    ==
  --
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
++  count-unread
  |=  notes=(map @tas note)
  ^-  @ud
  %-  ~(rep by notes)
  |=  [[key=@tas val=note] count=@ud]
  ?:  read.val
    count
  +(count)
::
--
