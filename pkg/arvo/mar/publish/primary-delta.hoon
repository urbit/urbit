::
::::  /hoon/action/publish/mar
  ::
/-  *publish
/+  *publish
::
|_  del=primary-delta
::
++  grab
  |%
  ++  noun  primary-delta
  --
++  grow
  |%
  ++  json
    %+  frond:enjs:format  -.del
    ?-  -.del
        %add-book
      %+  frond:enjs:format  (scot %p host.del)
      %+  frond:enjs:format  book.del
      (notebook-short-json data.del)
    ::
        %add-note
      %+  frond:enjs:format  (scot %p host.del)
      %+  frond:enjs:format  book.del
      (note-full-json note.del data.del)
    ::
        %add-comment
      %-  pairs:enjs:format
      :~  host+s+(scot %p host.del)
          book+s+book.del
          note+s+note.del
          comment+(comment-json comment-date.del data.del)
      ==
    ::
        %edit-book
      %+  frond:enjs:format  (scot %p host.del)
      %+  frond:enjs:format  book.del
      (notebook-short-json data.del)
    ::
        %edit-note
      %+  frond:enjs:format  (scot %p host.del)
      %+  frond:enjs:format  book.del
      (note-full-json note.del data.del)
    ::
        %edit-comment
      %-  pairs:enjs:format
      :~  host+s+(scot %p host.del)
          book+s+book.del
          note+s+note.del
          comment+(comment-json comment-date.del data.del)
      ==
    ::
        %del-book
      %-  pairs:enjs:format
      :~  host+s+(scot %p host.del)
          book+s+book.del
      ==
    ::
        %del-note
      %-  pairs:enjs:format
      :~  host+s+(scot %p host.del)
          book+s+book.del
          note+s+note.del
      ==
    ::
        %del-comment
      %-  pairs:enjs:format
      :~  host+s+(scot %p host.del)
          book+s+book.del
          note+s+note.del
          comment+s+(scot %da comment.del)
      ==
    ::
        %read
      %-  pairs:enjs:format
      :~  host+s+(scot %p who.del)
          book+s+book.del
          note+s+note.del
      ==
    ==
  --
--
