::
::::  /hoon/config/collection/mar
  ::
/+  collections
!:
|_  con=config:collections
::
::
++  grow
  |%
  ++  mime
    :-  /text/x-collection-config
    (as-octs:mimes:html (of-wain:format txt))
  ++  txt
    ^-  wain
    ::
    :~  (cat 3 'full-path: ' (spat (en-beam:format full-path.con)))
        (cat 3 'name: ' name.con)
        (cat 3 'description: ' description.con)
      ::
        (cat 3 'author: ' (scot %p author.con))
      ::
        (cat 3 'date-created: ' (scot %da date-created.con))
        (cat 3 'last-modified: ' (scot %da last-modified.con))
      ::
        (cat 3 'type: ' type.con)
        (cat 3 'comments: ' ?:(comments.con 'y' 'n'))
        (cat 3 'sort-key: ' ?~(sort-key.con '~' (scot %ud u.sort-key.con)))
        (cat 3 'visible: ' ?:(visible.con 'y' 'n'))
    ==
  --
++  grab
  |%
  ++  mime
    |=  [mite:eyre p=octs:eyre]
    (txt (to-wain:format q.p))
  ++  txt
    |=  txs=(pole @t)
    ^-  config:collections
    ::  TODO: putting ~ instead of * breaks this but shouldn't
    ::
    ?>  ?=  $:  full-path=@t
                name=@t
                desc=@t
                author=@t
                dc=@t
                lm=@t
                type=@t
                com=@t
                sk=@t
                vis=@t
                *
             ==
           txs
    ::
    :*  %-  need
        %+  rash  full-path.txs
          ;~  pfix  (jest 'full-path: ')
            %+  cook  de-beam:format
            ;~(pfix fas (more fas urs:ab))
          ==
    ::
      (rash name.txs ;~(pfix (jest 'name: ') (cook crip (star next))))
    ::
      (rash desc.txs ;~(pfix (jest 'description: ') (cook crip (star next))))
    ::
      (rash author.txs ;~(pfix (jest 'author: ~') fed:ag))
    ::
      %+  rash  dc.txs
      ;~  pfix
        (jest 'date-created: ~')
        (cook year when:so)
      ==
    ::
      %+  rash  lm.txs
      ;~  pfix
        (jest 'last-modified: ~')
        (cook year when:so)
      ==
    ::
      (rash type.txs ;~(pfix (jest 'type: ') (cook crip (star next))))
    ::
      (rash com.txs ;~(pfix (jest 'comments: ') (fuss %y %n)))
    ::
      (rush sk.txs ;~(pfix (jest 'sort-key: ') dem:ag))
    ::
      (rash vis.txs ;~(pfix (jest 'visible: ') (fuss %y %n)))
    ==
  ++  noun  config:collections
  --
++  grad  %mime
--
