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
    (crip "commit: {<`@uv`tak>}\0a\0a{(trip title)}\0a\0a{(trip body)}\0a---\0a")
  --
++  grab
  |%                                             ::  convert from
  ++  noun  (map tako:clay ,[title=@t body=@t])  ::  clam from %noun
  ++  mime  |=([p=mite q=octs] q.q)              ::  retrieve form %mime
  --
--
