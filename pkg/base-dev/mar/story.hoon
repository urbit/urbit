|_  story=(map tako:clay [title=@t body=@t])
++  grad  %mime
::
++  grow                                                ::  convert to
  |%                                                    ::
  ++  mime                                              ::  to %mime
    [/text/x-urb-story (as-octs:mimes:html (of-wain:format txt))]
  ++  txt
    ^-  wain
    %+  turn  ~(tap by story)
    |=  [chapter=[tak=tako:clay message=[title=@t body=@t]]]
    =/  tak=tako:clay       tak.chapter
    =/  [title=@t body=@t]  +.chapter
    (crip "commit: {<`@uv`tak>}\0a{(trip title)}\0a\0a{(trip body)}\0a---\0a")
  --
++  grab
  |%                                             ::  convert from
  ++  noun  (map tako:clay ,[title=@t body=@t])  ::  clam from %noun
  ++  mime                                       ::  retrieve form %mime
    |=  [p=mite q=octs]
    ^-  (map tako:clay [title=@t body=@t])
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
        ;~  less  (jest '---\0a')              :: exclude '---' from the following parse
          ;~(sfix (star prn) (just '\0a'))  :: parse 0 or more prn chars then discard literal newline
        ==
      ::
        (jest '---\0a')                     :: parse the terminator
      ==
    ::
    =/  story-parser
      %-  star  :: parse any number of the chapters
      ;~  plug  :: parse chapter: a commit, followed by a title, followed by a body
          commit-parser
          title-parser
          body-parser
      ==
    ::
    =/  story-text    `@t`q.q
    =/  parsed-story  `(list [@uv @t wain])`(rash story-text story-parser)
    %-  ~(gas by *(map tako:clay [title=@t body=@t]))
    %+  turn  parsed-story
    |=  [tak=tako:clay title=@t body=wain]
    [tak title (of-wain:format body)]
  --
--
