::
::::  /hoon/action/publish/mar
  ::
/-  *publish
=,  format
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
    =,  enjs:format
    %+  frond  -.del
    ?-  -.del
        %add-book
      (notebook-short-json host.del book.del data.del)
    ::
        %add-note
      (note-short-json host.del book.del note.del data.del)
    ::
        %add-comment
      %-  pairs:enjs:format
      :~  host+(ship host.del)
          book+s+book.del
          note+s+note.del
          comment+s+(scot %da comment-date.del)
          data+(comment-json data.del)
      ==
        %edit-book
      (notebook-short-json host.del book.del data.del)
    ::
        %edit-note
      (note-short-json host.del book.del note.del data.del)
    ::
        %edit-comment
      %-  pairs:enjs:format
      :~  host+(ship host.del)
          book+s+book.del
          note+s+note.del
          comment+s+(scot %da comment-date.del)
          comment+(comment-json data.del)
      ==
    ::
        %del-book
      %-  pairs:enjs:format
      :~  host+(ship host.del)
          book+s+book.del
      ==
    ::
        %del-note
      %-  pairs:enjs:format
      :~  host+(ship host.del)
          book+s+book.del
          note+s+note.del
      ==
    ::
        %del-comment
      %-  pairs:enjs:format
      :~  host+(ship host.del)
          book+s+book.del
          note+s+note.del
          comment+s+(scot %da date-created.del)
      ==
    --
  --
--
