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
    |^  (rash q.p both-parser)
    ++  key-val
      |*  [key=rule val=rule]
      ;~(sfix ;~(pfix key val) gaq)
    ++  old-parser
      ;~  plug
        (key-val (jest 'owner: ~') fed:ag)
        (key-val (jest 'title: ') (cook crip (star qit)))
        (key-val (jest 'filename: ') sym)
        %+  key-val  (jest 'comments: ')
          ;~(pose (jest %open) (jest %closed) (jest %none))
        %+  key-val  (jest 'allow-edit: ')
          ;~(pose (jest %post) (jest %comment) (jest %all) (jest %none))
        (key-val (jest 'date-created: ~') (cook year when:so))
        ;~  pose
          (key-val (jest 'last-modified: ~') (cook year when:so))
          ;~(pfix (jest 'last-modified: ~') (cook year when:so))
        ==
      ==
    ++  new-parser
      ;~  plug
        (key-val (jest 'title: ') (cook crip (star qit)))
        (key-val (jest 'description: ') (cook crip (star qit)))
        %+  key-val  (jest 'comments: ')
          (cook |=(a=@ =(%on a)) ;~(pose (jest %on) (jest %off)))
        (key-val (jest 'writers: ') ;~(pfix net (more net urs:ab)))
        ;~  pose
          (key-val (jest 'subscribers: ') ;~(pfix net (more net urs:ab)))
          ;~(pfix (jest 'subscribers: ') ;~(pfix net (more net urs:ab)))
        ==
      ==
    ++  both-parser
      ;~  pose
        new-parser
        %+  cook
          |=  [@ title=@t @ comments=@ *]
          ^-  notebook-info
          [title '' =('open' comments) / /]
        old-parser
      ==
    --
  ++  noun  notebook-info
  --
++  grad  %mime
--
