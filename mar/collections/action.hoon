::
::::  /hoon/actions/collection/mar
  ::
/?  309
/+  collections
=,  collections
=,  format
::
|_  act=action:collections
::
++  grow
  |%
  ++  tank  >act<
  --
::
++  grab
  |%
  ++  noun  action:collections
  ++  json
    |=  jon=^json
    ;;  action:collections
    =<  (action jon)
    |%
    ++  action
      %-  ot:dejs
      :~  ship+(su:dejs fed:ag)
          desk+(su:dejs sym)
          :-  %acts
          %-  ar:dejs
          %-  of:dejs
          :~  write+write
              delete+delete
              perms+perms
              collection+collection
              post+post
              comment+comment
          ==
      ==
    ::
    ++  write
      %-  ot:dejs
      :~  path+(su:dejs ;~(pfix fas (more fas urs:ab)))
          mark+(su:dejs sym)
          data+so:dejs
      ==
    ::
    ++  delete
      %-  ot:dejs
      :~  path+(su:dejs ;~(pfix fas (more fas urs:ab)))
      ==
    ::
    ++  perms
        %-  ot:dejs
        :~  path+(su:dejs ;~(pfix fas (more fas urs:ab)))
            :-  %read
            %-  ot:dejs
            :~  mod+(su:dejs ;~(pose (jest %black) (jest %white)))
                who+whoms
            ==
            :-  %write
            %-  ot:dejs
            :~  mod+(su:dejs ;~(pose (jest %black) (jest %white)))
                who+whoms
            ==
        ==
    ::
    ++  whoms
      |=  jon=^json
      ^-  (set whom:clay)
      =/  x  ((ar:dejs (su:dejs fed:ag)) jon)
      ;;  (set whom:clay)
      %-  ~(run in (sy x))
      |=(w=@ [& w])
    --
    ::
    ++  collection
      %-  ot:dejs
      :~  path+(su:dejs ;~(pfix fas (more fas urs:ab)))
          name+sa
          desc+so:dejs
          comments+bo:dejs
          visible+bo:dejs
          type+(su:dejs sym)
      ==
    ::
    ++  post
      %-  ot:dejs
      :~  path+(su:dejs ;~(pfix fas (more fas urs:ab)))
          name+sa
          type+(su:dejs sym)
          comments+bo:dejs
          content+so:dejs
          edit+bo:dejs
      ==
    ::
    ++  comment
      %-  ot:dejs
      :~  path+(su:dejs ;~(pfix fas (more fas urs:ab)))
          content+so:dejs
      ==
    ::
    ++  sa            :: string as ta
      |=  jon=^json
      ?>  ?=([%s *] jon)
      (scot %tas p.jon)
  --
::
--












