::
::::  /hoon/info/publish/mar
  ::
/-  publish
!:
|_  con=collection-info:publish
::
::
++  grow
  |%
  ++  mime
    :-  /text/x-publish-info
    (as-octs:mimes:html (of-wain:format txt))
  ++  txt
    ^-  wain
    :~  (cat 3 'owner: ' (scot %p owner.con))
        (cat 3 'title: ' title.con)
        (cat 3 'filename: ' filename.con)
        (cat 3 'comments: ' comments.con)
        (cat 3 'allow-edit: ' allow-edit.con)
        (cat 3 'date-created: ' (scot %da date-created.con))
        (cat 3 'last-modified: ' (scot %da last-modified.con))
    ==
  --
++  grab
  |%
  ++  mime
    |=  [mite:eyre p=octs:eyre]
    (txt (to-wain:format q.p))
  ++  txt
    |=  txs=(pole @t)
    ^-  collection-info:publish
    ::  TODO: putting ~ instead of * breaks this but shouldn't
    ::
    ?>  ?=  $:  owner=@t
                title=@t
                filename=@t
                comments=@t
                allow-edit=@t
                date-created=@t
                last-modified=@t
                *
             ==
           txs
    ::
    :*  %+  rash  owner.txs
        ;~(pfix (jest 'owner: ~') fed:ag)
    ::
        %+  rash  title.txs 
        ;~(pfix (jest 'title: ') (cook crip (star next)))
    ::
        %+  rash  filename.txs 
        ;~(pfix (jest 'filename: ') (cook crip (star next)))
    ::
      %+  rash  comments.txs
      ;~  pfix
        (jest 'comments: ')
        %+  cook  comment-config:publish
        ;~(pose (jest %open) (jest %closed) (jest %none))
      ==
    ::
      %+  rash  allow-edit.txs
      ;~  pfix
        (jest 'allow-edit: ')
        %+  cook  edit-config:publish
        ;~(pose (jest %post) (jest %comment) (jest %all) (jest %none))
      ==
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
    ==
  ++  noun  collection-info:publish
  --
++  grad  %mime
--
