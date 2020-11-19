::
::::  /hoon/info/publish/mar
  ::  tombstoned, now unused
  ::
/-  *publish
!:
|_  info=notebook-info
::
::
++  grab
  |%
  ++  mime
    |=  [mite p=octs]
    |^  (rash q.p both-parser)
    ++  key-val
      |*  [key=rule val=rule]
      ;~(sfix ;~(pfix key val) gaq)
    ++  old-parser
      ;~  plug
        (key-val (jest 'owner: ~') fed:ag)
        (key-val (jest 'title: ') (cook crip (star prn)))
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
        (key-val (jest 'title: ') (cook crip (star prn)))
        (key-val (jest 'description: ') (cook crip (star prn)))
        %+  key-val  (jest 'comments: ')
          (cook |=(a=@ =(%on a)) ;~(pose (jest %on) (jest %off)))
        (key-val (jest 'writers: ') ;~(pfix fas (more fas urs:ab)))
        ;~  pose
          (key-val (jest 'subscribers: ') ;~(pfix fas (more fas urs:ab)))
          ;~(pfix (jest 'subscribers: ') ;~(pfix fas (more fas urs:ab)))
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
