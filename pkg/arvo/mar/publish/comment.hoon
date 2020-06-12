/-  *publish
|_  com=?(comment-2 comment-3)
::
::
++  grow
  |%
  ++  mime
    :-  /text/x-publish-comments
    (as-octs:mimes:html (of-wain:format txt))
  ++  txt
    ^-  wain
    ?:  ?=(comment-2 com)
      :*  (cat 3 'author: ' (scot %p author.com))
          (cat 3 'date-created: ' (scot %da date-created.com))
          '-----'
          (to-wain:format content.com)
      ==
    ?>  ?=(comment-3 com)
    :*  (cat 3 'author: ' (scot %p author.com))
        (cat 3 'date-created: ' (scot %da date-created.com))
        '-----'
        (to-wain:format content.com)
    ==
  --
++  grab
  |%
  ++  mime
    |=  [mite:eyre p=octs:eyre]
    |^  (rash q.p both-parser)
    ++  key-val
      |*  [key=rule val=rule]
      ;~(sfix ;~(pfix key val) gaq)
    ++  old-parser
      ;~  plug
        (key-val (jest 'creator: ~') fed:ag)
        (key-val (jest 'collection: ') sym)
        (key-val (jest 'post: ') sym)
        (key-val (jest 'date-created: ~') (cook year when:so))
        (key-val (jest 'last-modified: ~') (cook year when:so))
        ;~(pfix (jest (cat 3 '-----' 10)) (cook crip (star next)))
      ==
    ++  new-parser
      ;~  plug
        (key-val (jest 'author: ~') fed:ag)
        (key-val (jest 'date-created: ~') (cook year when:so))
        ;~(pfix (jest (cat 3 '-----' 10)) (cook crip (star next)))
      ==
    ++  both-parser
      ;~  pose
        %+  cook
          |=  [author=@ date-created=@da content=@t]
          ^-  comment
          [author date-created content %.n]
        new-parser
        %+  cook
          |=  [author=@ @ @ date-created=@da @ content=@t]
          ^-  comment
          [author date-created content %.n]
        old-parser
      ==
    --
  ++  noun  comment
  --
++  grad  %mime
--
