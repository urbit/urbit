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
    |^  (rash q.p ;~(sfix both-parser (punt gaq)))
    ++  key-val
      |*  [key=@tas val=rule]
      ;~(pfix (jest key) col ace val)
    ++  old-parser
      ;~  (glue gaq)
        (key-val %creator ;~(pfix sig fed:ag))
        (key-val %collection sym)
        (key-val %post sym)
        (key-val %title (cook crip (star prn)))
        (key-val %date-created ;~(pfix sig (cook year when:so)))
        (key-val %last-modified ;~(pfix sig (cook year when:so))
        (cold ~ (jest '-----'))
        (cook crip (star next)))
      ==
    ++  new-parser
      ;~  (glue gaq)
        (key-val %author ;~(pfix sig fed:ag))
        (key-val %date-created ;~(pfix sig (cook year when:so)))
        (cold ~ (jest '-----'))
        (cook crip (star next)))
      ==
    ++  both-parser
      ;~  pose
        %+  cook
          |=  [author=@ date-created=@da ~ content=@t]
          ^-  comment
          [author date-created content %.n]
        new-parser
        %+  cook
          |=  [author=@ @ @ date-created=@da @ ~ content=@t]
          ^-  comment
          [author date-created content %.n]
        old-parser
      ==
    --
  ++  noun  comment
  --
++  grad  %mime
--
