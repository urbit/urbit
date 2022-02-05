/-  *story
/+  *story
!:
|_  tale=story
+$  story-diff       [additions=story deletions=story]
::
++  grad
  |%
  ++  form  %story-diff
  ++  diff
    |=  tory=story
    ^-  story-diff
    ::  Given new story (tory), old story (tale), compute the diff
    ::  additions = new - old
    ::  deletions = old - new
    [(dif-ju tory tale) (dif-ju tale tory)]
  ++  pact
    |=  dif=story-diff
    ::  compute the new story after applying dif to tale
    ^-  story
    =.  tale  (uni-ju tale additions.dif)
    =.  tale  (dif-ju tale deletions.dif)
    tale
  ++  join
    |=  [ali=story-diff bob=story-diff]
    ^-  (unit story-diff)
    =/  joined-additions  (uni-ju additions.ali additions.bob)
    =/  joined-deletions  (uni-ju deletions.ali deletions.bob)
    ::
    :: in a true join, we'd do an intersection and see if the vals are not exactly the same
    :: which means we have a conflict, then we'd produce null, kick the flow to mash
    %-  some
    [joined-additions joined-deletions]
  ++  mash
    ::  called by meld, force merge, annotating conflicts
    |=  $:  [als=ship ald=desk ali=story-diff]
            [bos=ship bod=desk bob=story-diff]
        ==
    ^-  story-diff
    (need (join ali bob)) :: XX temporary, only because join doesn't fail
  --
::
++  grow                                                ::  convert to
  |%                                                    ::
  ++  mime                                              ::  to %mime
    [/text/x-urb-story (as-octs:mimes:html (of-wain:format txt))]
  ++  txt
    ^-  wain
    %-  snoc  :_  ''  :: ensures terminating newline is present
    %+  murn  ~(tap by tale)
    |=  [[=tako:clay =proses]]
    ^-  (unit cord)
    ?~  proses  ~
    %-  some
    %-  crip
    ;:  welp
      (tako-to-text tako)
      (proses-to-text proses)
      "---"
    ==
  --
++  grab
  |%                                             ::  convert from
  ++  noun  story                                ::  clam from %noun
  ++  mime                                       ::  retrieve form %mime
    |=  [p=mite q=octs]
    =/  story-text    `@t`q.q
    `story`(rash story-text parse-story)
  --
--
