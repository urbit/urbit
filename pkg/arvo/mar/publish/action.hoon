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
    |^  %.  jon
        %-  of
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
            read+read
            groupify+groupify
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
          comment+so
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
          comment+so
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
    ++  read
      %-  ot
      :~  who+(su fed:ag)
          book+so
          note+so
      ==
    ++  groupify
      %-  ot
      :~  book+so
          target+(mu pa)
          inclusive+bo
      ==
    ++  group-info
      %-  ot
      :~  group-path+pa
          invitees+set-ship
          use-preexisting+bo
          make-managed+bo
      ==
    ++  set-ship  (as (su fed:ag))
    --
  --
--
