::
::::  /hoon/action/publish/mar
  ::
/-  *publish
=,  format
::
|_  act=action
::
++  grow
  |%
  ++  tank  >act<
  --
::
++  grab
  |%
  ++  noun  action
  ++  json
    |=  jon=^json
    =,  dejs:format
    ;;  action
    |^  %-  of
        :~  new-book+new-book
            new-note+new-note
            new-comment+new-comment
            edit-book+edit-book
            edit-note+edit-note
            edit-comment+edit-comment
            del-book+del-book
            del-note+del-note
            del-comment+del-comment
            subscribe+subscribe
            unsubscribe+unsubscribe
        ==
    ::
    ++  new-book
      %-  ot
      :~  book+so
          title+so
          about+so
          coms+bo
          group+group-info
      ==
    ::
    ++  new-note
      %-  ot
      :~  who+(su fed:ag)
          book+so
          note+so
          title+so
          body+so
      ==
    ::
    ++  new-comment
      %-  ot
      :~  who+(su fed:ag)
          book+so
          note+so
          body+so
      ==
    ::
    ++  edit-book
      %-  ot
      :~  book+so
          title+so
          about+so
          coms+bo
          group+(mu group-info)
      ==
    ::
    ++  edit-note
      %-  ot
      :~  who+(su fed:ag)
          book+so
          note+so
          title+so
          body+so
      ==
    ::
    ++  edit-comment
      %-  ot
      :~  who+(su fed:ag)
          book+so
          note+so
          comment+(su ;~(pfix sig (cook year when:^so)))
          body+so
      ==
    ::
    ++  del-book  (ot book+so ~)
    ::
    ++  del-note  (ot who+(su fed:ag) book+so note+so ~)
    ::
    ++  del-comment
      %-  ot
      :~  who+(su fed:ag)
          book+so
          note+so
          comment+(su ;~(pfix sig (cook year when:^so)))
      ==
    ++  subscribe
      %-  ot
      :~  who+(su fed:ag)
          book+so
      ==
    ++  unsubscribe
      %-  ot
      :~  who+(su fed:ag)
          book+so
      ==
    ++  group-info
      %-  of
      :~  old+(ot writers+pa subscribers+pa ~)
          new+(ot writers+set-ship subscribers+set-ship sec+so ~)
      ==
    ++  set-ship  (ar (su fed:ag))
    --
  --
--
