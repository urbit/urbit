::
::::  /hoon/info/publish/mar
  ::
/-  *publish
!:
|_  info=notebook-info
::
::
++  grow
  |%
  ++  mime
    :-  /text/x-publish-info
    (as-octs:mimes:html (of-wain:format txt))
  ++  txt
    ^-  wain
    :~  (cat 3 'title: ' title.info)
        (cat 3 'description: ' description.info)
        (cat 3 'comments: ' ?:(comments.info 'on' 'off'))
        (cat 3 'writers: ' (spat writers.info))
        (cat 3 'subscribers: ' (spat subscribers.info))
    ==
  --
++  grab
  |%
  ++  mime
    |=  [mite:eyre p=octs:eyre]
    (txt (to-wain:format q.p))
  ++  txt
    |=  txs=(pole @t)
    ^-  notebook-info
    ::  TODO: putting ~ instead of * breaks this but shouldn't
    ::
    ?>  ?=  $:  title=@t
                description=@t
                comments=@t
                writers=@t
                subscribers=@t
                *
             ==
           txs
    ::
    :*  %+  rash  title.txs
        ;~(pfix (jest 'title: ') (cook crip (star next)))
    ::
        %+  rash  description.txs
        ;~(pfix (jest 'description: ') (cook crip (star next)))
    ::
        %+  rash  comments.txs
        ;~  pfix
          (jest 'comments: ')
          %+  cook
            |=  val=@t
            ^-  ?
            =(val %on)
          ;~(pose (jest %on) (jest %off))
        ==
    ::
        %+  rash  writers.txs
        ;~(pfix (jest 'writers: ') ;~(pfix net (more net urs:ab)))
    ::
        %+  rash  subscribers.txs
        ;~(pfix (jest 'subscribers: ') ;~(pfix net (more net urs:ab)))
    ==
  ++  noun  notebook-info
  --
++  grad  %mime
--
