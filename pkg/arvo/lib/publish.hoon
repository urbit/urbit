/-  *publish
/+  elem-to-react-json
|%
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
++  note-build-to-json
  |=  build=(each manx tang)
  ^-  json
  ?:  ?=(%.y -.build)
    %-  pairs:enjs:format
    :~  success+b+%.y
        result+(elem-to-react-json p.build)
    ==
  %-  pairs:enjs:format
  :~  success+b+%.n
      result+(tang-to-json p.build)
  ==
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
++  notebooks-list-json
  |=  [our=@p books=(map @tas notebook) subs=(map [@p @tas] notebook)]
  ^-  json
  =,  enjs:format
  :-  %a
  %+  weld
    %+  turn  ~(tap by books)
    |=  [name=@tas book=notebook]
    (notebook-short-json book)
  %+  turn  ~(tap by subs)
  |=  [[host=@p name=@tas] book=notebook]
  (notebook-short-json book)
::
++  notebooks-map-json
  |=  [our=@p books=(map @tas notebook) subs=(map [@p @tas] notebook)]
  ^-  json
  =,  enjs:format
  =/  subs-notebooks-map=json
    %-  ~(rep by subs)
    |=  [[[host=@p book-name=@tas] book=notebook] out=json]
    ^-  json
    =/  host-ta  (scot %p host)
    ?~  out
      (frond host-ta (frond book-name (notebook-short-json book)))
    ?>  ?=(%o -.out)
    =/  books  (~(get by p.out) host-ta)
    ?~  books
      :-  %o
      (~(put by p.out) host-ta (frond book-name (notebook-short-json book)))
    ?>  ?=(%o -.u.books)
    =.  p.u.books  (~(put by p.u.books) book-name (notebook-short-json book))
    :-  %o
    (~(put by p.out) host-ta u.books)
  =?  subs-notebooks-map  ?=(~ subs-notebooks-map)
    [%o ~]
  =/  our-notebooks-map=json
    %-  ~(rep by books)
    |=  [[book-name=@tas book=notebook] out=json]
    ^-  json
    ?~  out
      (frond book-name (notebook-short-json book))
    ?>  ?=(%o -.out)
    :-  %o
    (~(put by p.out) book-name (notebook-short-json book))
  ?~  our-notebooks-map
    subs-notebooks-map
  ?>  ?=(%o -.subs-notebooks-map)
  :-  %o
  (~(put by p.subs-notebooks-map) (scot %p our) our-notebooks-map)
::
++  notebook-short-json
  |=  book=notebook
  ^-  json
  =,  enjs:format
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
++  notebook-full-json
  |=  [host=@p book-name=@tas book=notebook]
  ^-  json
  =,  enjs:format
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
++  note-presentation-json
  |=  [book=notebook note-name=@tas not=note]
  ^-  (map @t json)
  =,  enjs:format
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
  =/  current=json  (note-full-json note-name not)
  ?>  ?=(%o -.current)
  =.  p.current  (~(put by p.current) %prev-note ?~(prev ~ s+name.u.prev))
  =.  p.current  (~(put by p.current) %next-note ?~(next ~ s+name.u.next))
  =/  notes=(list [@t json])  [note-name current]~
  =?  notes  ?=(^ prev)
    [[name.u.prev (note-short-json name.u.prev not.u.prev)] notes]
  =?  notes  ?=(^ next)
    [[name.u.next (note-short-json name.u.next not.u.next)] notes]
  %-  my
  :~  notes+(pairs notes)
      notes-by-date+a+(turn notes-list |=([name=@tas *] s+name))
  ==
::
++  note-full-json
  |=  [note-name=@tas =note]
  ^-  json
  =,  enjs:format
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
++  note-short-json
  |=  [note-name=@tas =note]
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  note-id+s+note-name
      author+s+(scot %p author.note)
      title+s+title.note
      date-created+(time date-created.note)
      num-comments+(numb ~(wyt by comments.note))
      read+b+read.note
      snippet+s+snippet.note
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
      notes+o+(notes-list-json (scag length (slag start notes-list)))
  ==
::
++  notes-list-json
  |=  notes=(list [@tas note])
  ^-  (map @t json)
  %+  roll  notes
  |=  [[name=@tas not=note] out-map=(map @t json)]
  ^-  (map @t json)
  (~(put by out-map) name (note-short-json name not))
::
++  comments-page
  |=  [comments=(map @da comment) start=@ud end=@ud]
  ^-  json
  =/  coms=(list [@da comment])
    %+  sort  ~(tap by comments)
    |=  [[d1=@da comment] [d2=@da comment]]
    (gte d1 d2)
  %-  comments-list-json
  (scag end (slag start coms))
::
++  comments-list-json
  |=  comments=(list [@da comment])
  ^-  json
  =,  enjs:format
  :-  %a
  (turn comments comment-json)
::
++  comment-json
  |=  [date=@da com=comment]
  ^-  json
  =,  enjs:format
  %+  frond:enjs:format
    (scot %da date)
  %-  pairs
  :~  author+s+(scot %p author.com)
      date-created+(time date-created.com)
      content+s+content.com
  ==
--
