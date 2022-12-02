/-  *story
/+  *story
|_  tale=story
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
    ::  Compute the new story after applying dif to tale.
    ::
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
    ::  In a true join, we'd do a set intersection on the keys.
    ::  If they're not equal, we have a conflict.
    ::  In this case, we'd produce null and kick the flow to +mash
    ::
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
    %-  zing
    %+  join  `wain`~['---']
    %+  murn  ~(tap by tale)
    |=  [=tako:clay =proses]
    ^-  (unit wain)
    ?~  proses  ~
    (some (chapter-to-text tako proses))
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  story                                       ::  clam from %noun
  ++  mime                                              ::  retrieve from %mime
    |=  [p=mite q=octs]
    =/  story-text    `@t`q.q
    `story`(rash story-text parse-story)
  --
--
