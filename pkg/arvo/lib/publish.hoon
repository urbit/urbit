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
++  notebooks-list-json
  |=  [our=@p books=(map @tas notebook) subs=(map [@p @tas] notebook)]
  ^-  json
  =,  enjs:format
  :-  %a
  %+  weld
    %+  turn  ~(tap by books)
    |=  [name=@tas book=notebook]
    (notebook-short-json our name book)
  %+  turn  ~(tap by subs)
  |=  [[host=@p name=@tas] book=notebook]
  (notebook-short-json host name book)
::
++  notebook-short-json
  |=  [host=@p book-name=@tas book=notebook]
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  host+(ship host)
      id+s+book-name
      title+s+title.book
      date-created+(time date-created.book)
      num-notes+(numb ~(wyt by notes.book))
  ==
::
++  notebook-full-json
  |=  [host=@p name=@tas book=notebook]
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  host+(ship host)
      id+s+name
      title+s+title.book
      date-created+(time date-created.book)
  ::  subscribers
  ::  notes
  ==
::
++  note-short-json
  |=  [host=@p book-name=@tas note-name=@tas =note]
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  host+(ship host)
      book-id+s+book-name
      note-id+s+note-name
      author+(ship author.note)
      title+s+title.note
      date-created+(time date-created.note)
      num-comments+(numb ~(wyt by comments.note))
  ==
::
++  note-full-json
  |=  [host=@p book-name=@tas note-name=@tas =note]
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  host+(ship host)
      book-id+s+book-name
      note-id+s+note-name
      author+(ship author.note)
      title+s+title.note
      date-created+(time date-created.note)
      build+(note-build-to-json build.note)
      file+s+file.note
  ==
::
++  notes-page
  |=  [notes=(map @tas note) start=@ud length=@ud]
  ^-  json
  =/  notes-list=(list [@tas note])
    %+  sort  ~(tap by notes)
    |=  [[@tas n1=note] [@tas n2=note]]
    (gte date-created.n1 date-created.n2)
  %-  notes-list-json
  (scag length (slag start notes-list))
::
++  notes-list-json
  |=  notes=(list [@tas note])
  ^-  json
  =,  enjs:format
  :-  %a
  %+  turn  notes
  |=  [note-name=@tas =note]
  ^-  json
  %-  pairs
  :~  note-id+s+note-name
      author+(ship author.note)
      title+s+title.note
      date-created+(time date-created.note)
      num-comments+(numb ~(wyt by comments.note))
  ::  snippet
  ==
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
  :~  author+(ship author.com)
      date-created+(time date-created.com)
      content+s+content.com
  ==
--
