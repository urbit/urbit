::
::::  /hoon/action/publish/mar
  ::
/?  309
/-  publish
=,  format
::
|_  act=action:publish
::
++  grow
  |%
  ++  tank  >act<
  --
::
++  grab
  |%
  ++  noun  action:publish
  ++  json
    |=  jon=^json
    %-  action:publish
    =<  (action jon)
    |%
    ++  action
      %-  of:dejs
      :~  new-collection+new-collection
          new-post+new-post
          new-comment+new-comment
      ::
          delete-collection+delete-collection
          delete-post+delete-post
          delete-comment+delete-comment
      ::
          edit-collection+edit-collection
          edit-post+edit-post
      ::
          invite+invite
          reject-invite+reject-invite
      ::
          serve+serve
          unserve+unserve
      ::
          subscribe+subscribe
          unsubscribe+unsubscribe
      ::
          read+read
      ==
    ::
    ++  new-collection
      %-  ot:dejs
      :~  name+so:dejs
          title+so:dejs
          comments+comment-config
          allow-edit+edit-config
          perm+perm-config
      ==
    ::
    ++  new-post
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+so:dejs
          name+so:dejs
          title+so:dejs
          comments+comment-config
          perm+perm-config
          content+so:dejs
      ==
    ::
    ++  new-comment
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+so:dejs
          name+so:dejs
          content+so:dejs
      ==
    ::
    ++  delete-collection
      %-  ot:dejs
      :~  coll+so:dejs
      ==
    ::
    ++  delete-post
      %-  ot:dejs
      :~  coll+so:dejs
          post+so:dejs
      ==
    ::
    ++  delete-comment
      %-  ot:dejs
      :~  coll+so:dejs
          post+so:dejs
          comment+so:dejs
      ==
    ::
    ++  edit-collection
      %-  ot:dejs
      :~  name+so:dejs
          title+so:dejs
      ==
    ::
    ++  edit-post
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+so:dejs
          name+so:dejs
          title+so:dejs
          comments+comment-config
          perm+perm-config
          content+so:dejs
      ==
    ::
    ++  edit-comment
      %-  ot:dejs
      :~  coll+so:dejs
          name+so:dejs
          id+so:dejs
          content+so:dejs
      ==
    ::
    ++  comment-config
      %-  su:dejs
      ;~(pose (jest %open) (jest %closed) (jest %none))
    ::
    ++  edit-config
      %-  su:dejs
      ;~(pose (jest %post) (jest %comment) (jest %all) (jest %none))
    ::
    ++  perm-config
        %-  ot:dejs
        :~  :-  %read
            %-  ot:dejs
            :~  mod+(su:dejs ;~(pose (jest %black) (jest %white)))
                who+whoms
            ==
            :-  %write
            %-  ot:dejs
            :~  mod+(su:dejs ;~(pose (jest %black) (jest %white)))
                who+whoms
        ==  ==
    ::
    ++  whoms
      |=  jon=^json
      ^-  (set whom:clay)
      =/  x  ((ar:dejs (su:dejs fed:ag)) jon)
      %-  (set whom:clay)
      %-  ~(run in (sy x))
      |=(w=@ [& w])
    ::
    ++  invite
      %-  ot:dejs
      :~  coll+so:dejs
          title+so:dejs
          who+(ar:dejs (su:dejs fed:ag))
      ==
    ::
    ++  reject-invite
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+so:dejs
      ==
    ::
    ++  serve
      %-  ot:dejs
      :~  coll+so:dejs
      ==
    ::
    ++  unserve
      %-  ot:dejs
      :~  coll+so:dejs
      ==
    ::
    ++  subscribe
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+so:dejs
      ==
    ::
    ++  unsubscribe
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+so:dejs
      ==
    ::
    ++  read
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+so:dejs
          post+so:dejs
      ==
    ::
    --
  --
--
