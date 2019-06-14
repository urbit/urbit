::
::::  /hoon/action/write/mar
  ::
/?  309
/-  write
=,  format
::
|_  act=action:write
::
++  grow
  |%
  ++  tank  >act<
  --
::
++  grab
  |%
  ++  noun  action:write
  ++  json
    |=  jon=^json
    %-  action:write
    =<  (action jon)
    |%
    ++  action
      %-  of:dejs
      :~  new-collection+new-collection
          new-post+new-post
          new-comment+new-comment
      ::
          delete+item-id
      ::
          edit-collection+edit-collection
          edit-post+edit-post
          edit-comment+edit-comment
      ::
          invite+invite
      ::
          serve+serve
          unserve+unserve
      ::
          subscribe+subscribe
          unsubscribe+unsubscribe
      ::
      ==
    ::
    ++  new-collection
      %-  ot:dejs
      :~  name+(su:dejs sym)
          title+so:dejs
          comments+comment-config
          allow-edit+edit-config
          perm+perm-config
      ==
    ::
    ++  new-post
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+(su:dejs sym)
          name+(su:dejs sym)
          title+so:dejs
          comments+comment-config
          perm+perm-config
          content+so:dejs
      ==
    ::
    ++  new-comment
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+(su:dejs sym)
          name+(su:dejs sym)
          content+so:dejs
      ==
    ::
    ++  edit-collection
      %-  ot:dejs
      :~  name+(su:dejs sym)
          title+so:dejs
          comments+comment-config
          allow-edit+edit-config
          perm+perm-config
      ==
    ::
    ++  edit-post
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+(su:dejs sym)
          name+(su:dejs sym)
          title+so:dejs
          comments+comment-config
          perm+perm-config
          content+so:dejs
      ==
    ::
    ++  edit-comment
      %-  ot:dejs
      :~  coll+(su:dejs sym)
          name+(su:dejs sym)
          id+(su:dejs sym)
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
    ++  item-id
      |=  jon=^json
      ^-  item-id:write
      ?>  ?=(%a -.jon)                          :: must be array
      ?<  ?=(~ +.jon)                           :: must have at least one item
      ?>  ?=([%s @t] -.+.jon)                   :: first item must be string
      =/  coll=@tas  (slav %tas +.-.+.jon)      :: get first item as @tas
      ?~  +.+.jon                               :: if only one item, return it
        coll
      ?>  ?=([%s @t] -.+.+.jon)                 :: second item must be string
      =/  post=@tas  (slav %tas +.-.+.+.jon)    :: get second item as @tas
      ?~  +.+.+.jon                             :: if two items, return them
        [coll post]
      ?>  ?=([%s @t] -.+.+.+.jon)               :: third item must be string
      =/  comm=@tas  (slav %tas +.-.+.+.+.jon)  :: get third item as @tas
      ?>  ?=(~ +.+.+.+.jon)                     :: no fourth item
      [coll post comm]
    ::
    ++  invite
      %-  ot:dejs
      :~  coll+(su:dejs sym)
          who+(ar:dejs (su:dejs fed:ag))
      ==
    ::
    ++  serve
      %-  ot:dejs
      :~  coll+(su:dejs sym)
      ==
    ::
    ++  unserve
      %-  ot:dejs
      :~  coll+(su:dejs sym)
      ==
    ::
    ++  subscribe
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+(su:dejs sym)
      ==
    ::
    ++  unsubscribe
      %-  ot:dejs
      :~  who+(su:dejs fed:ag)
          coll+(su:dejs sym)
      ==
    ::
    --
  --
--
