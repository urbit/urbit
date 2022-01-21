/-  *story
!:
|_  tale=story
+$  story-diff  [additions=story deletions=story]
++  grad
  |%
  ++  form  %story-diff
  ++  diff
    :: diff between given story, tale and another story, tory
    |=  tory=story
    ^-  story-diff
    =/  additions  (~(dif by tory) tale)  :: tory(new) - tale(old)
    =/  deletions  (~(dif by tale) tory)  :: tale(old) - tory(new)
    [additions deletions]
  ++  pact
    ::  given tale=story, compute the new story after applying the given dif
    |=  dif=story-diff
    ^-  story
    =/  [additions=story deletions=story]  dif
    ::
    ::  a: story
    ::  b: story + additions = a + additions
    ::  c: story + additions - deletions = b - deletions
    ::
    ::  add all new keys of additions to tale, and overwrite existing keys of tale with value from additions
    =/  tale-with-additions  (~(uni by tale) additions)
    ::  return the tale with additions, but now remove all elements from it that are present in the deletions
    =/  tale-merged  (~(dif by tale-with-additions) deletions)
    tale-merged
  ++  join
    |=  [ali=story-diff bob=story-diff]
    ^-  (unit story-diff)
    =/  [additions-a=story deletions-a=story]  ali
    =/  [additions-b=story deletions-b=story]  bob
    =/  joined-additions  (~(uni by additions-b) additions-a)
    =/  joined-deletions  (~(uni by deletions-b) deletions-a)
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
    %+  snoc  :: ensure terminating newline is present
    %+  murn  ~(tap by tale)
    |=  [[=tako:clay proses=(set prose)]]
    =/  proses-list  ~(tap in proses)
    ?~  proses-list  ~
    =/  prose-a  i.proses-list
    %-  some
    (crip "commit: {<`@uv`tako>}\0a{(trip title.prose-a)}\0a\0a{(trip body.prose-a)}\0a---")
    ::(turn ~(tap by proses) |=(=prose ))
    ''
  --
++  grab
  |%                                             ::  convert from
  ++  noun  story                                ::  clam from %noun
  ++  mime                                       ::  retrieve form %mime
    |=  [p=mite q=octs]
    ^-  story
    =/  commit-parser 
      ;~  sfix                                     :: throw away the trailing newline
        ;~  pfix  (jest 'commit: ')                :: throw away leading literal 'commit'
          (cook @uv ;~(pfix (jest '0v') viz:ag))   :: parse a @uv
        ==
      ::
        (just '\0a')                               :: parse trailing newline
      ==
    ::
    =/  title-parser
      ;~  sfix                  :: throw away trailing newlines
        (cook crip (star prn))  :: parse any number of ascii characters, turn into cord
        (jest '\0a\0a')         :: parse two newlines
      ==
    ::
    =/  body-parser
      ;~  sfix                              :: parse the following and discard terminator
        %-  star                            :: parse 0 or more of the following
        %+  cook  crip                      :: convert to cord
        ;~  less  (jest '---\0a')           :: exclude the terminator from the following parse
          ;~(sfix (star prn) (just '\0a'))  :: parse 0 or more prn chars then discard literal newline
        ==
      ::
        (jest '---\0a')                     :: parse the terminator
      ==
    ::
    =/  story-parser
      %-  star           :: parse any number of the chapters
      ;~  plug           :: parse chapter: a commit, followed by a title, followed by a body
          commit-parser
          title-parser
          body-parser
      ==
    ::
    =/  story-text    `@t`q.q
    =/  parsed-story  `(list [@uv @t wain])`(rash story-text story-parser)
    %-  ~(gas by *story)
    %+  turn  parsed-story
    |=  [=tako:clay title=@t body=wain]
    :-  tako 
    %-  silt
    :~  [title (of-wain:format body)]
    ==
  --
--
