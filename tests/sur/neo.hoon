/-  neo
/+  *test
|%
++  now   ~2024.4.11
++  reply-pith
  `pith:neo`#/messages/[da/now]/reply/[da/now]
++  reply-pish
  ^-  pish:neo
  :~  &/%messages
      |/%da  
      &/%reply
      |/%da
  ==
::
++  messages-pith
  `pith:neo`#/messages/[da/now]
++  messages-pish
  `pish:neo`(limo &/%messages |/%da ~)
++  comment-pith
  `pith:neo`#/note/[ud/3]/comment/[da/now]
++  comment-pish
  `pish:neo`(limo &/%note |/%ud &/%comment |/%da ~)
++  all-pish
  %-  ~(gas in *(set pish:neo))
  :~  reply-pish
      messages-pish
      comment-pish
  ==

  
++  test-peon-match
  ;:  weld
    %+  expect-eq  !>(&)
    !>((match:peon:neo messages-pish messages-pith))
  ::
    %+  expect-eq  !>(|)
    !>((match:peon:neo messages-pish reply-pith))
  ::
    %+  expect-eq  !>(&)
    !>((match:peon:neo reply-pish reply-pith))
  ==
++  test-peon-find
  ;:  weld
    %+  expect-eq  !>(`reply-pish)
    !>((find:peon:neo reply-pith all-pish))
  ::
    %+  expect-eq  !>(`comment-pish)
    !>((find:peon:neo comment-pith all-pish))
  ::
    %+  expect-eq  !>(~)
    !>((find:peon:neo comment-pith (~(del in all-pish) comment-pish)))
  ==
--
