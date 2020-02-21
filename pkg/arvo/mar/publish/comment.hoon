/-  publish
!:
|_  com=comment:publish
::
::
++  grow
  |%
  ++  mime
    :-  /text/x-publish-comments
    (as-octs:mimes:html (of-wain:format txt))
  ++  txt
    ^-  wain
    :*  (cat 3 'creator: ' (scot %p creator.info.com))
        (cat 3 'collection: ' collection.info.com)
        (cat 3 'post: ' post.info.com)
        (cat 3 'date-created: ' (scot %da date-created.info.com))
        (cat 3 'last-modified: ' (scot %da last-modified.info.com))
        '-----'
        (to-wain:format body.com)
    ==
  --
++  grab
  |%
  ++  mime
    |=  [mite:eyre p=octs:eyre]
    (txt (to-wain:format q.p))
  ++  txt
    |=  txs=(pole @t)
    ^-  comment:publish
    ::  TODO: putting ~ instead of * breaks this but shouldn't
    ::
    ?>  ?=  $:  creator=@t
                collection=@t
                post=@t
                date-created=@t
                last-modified=@t
                line=@t
                body=*
             ==
           txs
    :_  (of-wain:format (wain body.txs))
    ::
    :*  %+  rash  creator.txs
        ;~(pfix (jest 'creator: ~') fed:ag)
    ::
        %+  rash  collection.txs
        ;~(pfix (jest 'collection: ') (cook crip (star next)))
    ::
        %+  rash  post.txs
        ;~(pfix (jest 'post: ') (cook crip (star next)))
    ::
        %+  rash  date-created.txs
        ;~  pfix
          (jest 'date-created: ~')
          (cook year when:so)
        ==
    ::
        %+  rash  last-modified.txs
        ;~  pfix
          (jest 'last-modified: ~')
          (cook year when:so)
        ==
    ::
    ==
  ++  noun  comment:publish
  --
++  grad  %mime
--
