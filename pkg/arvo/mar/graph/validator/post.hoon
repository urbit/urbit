/-  *post, met=metadata-store
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ++  graph-permissions-add
    |=  vip=vip-metadata:met
    ?:  ?=([@ ~] index.p.i)
      [%yes %yes %no]
    [%yes %yes ?:(?=(%reader-comments vip) %yes %no)]
  ::
  ++  graph-permissions-remove
    |=  vip=vip-metadata:met
    [%yes %self %self]
  ::  +notification-kind: no notifications for now
  ::
  ++  notification-kind  ~
  ++  transform-add-nodes
    |=  [=index =post =atom was-parent-modified=?]
    ^-  [^index ^post]
    =-  [- post(index -)]
    ?~  index  !!
    ?:  ?=([@ ~] index)
      [atom ~]
    ?:  was-parent-modified
      ~|(%cannot-submit-parents-with-prepopulated-children !!)
    =/  ind=^index  index
    (snoc (snip ind) atom)
  --
++  grab
  |%
  :: +noun: validate post
  :: 
  ++  noun
    |=  p=*
    =/  ip  ;;(indexed-post p)
    ?>  ?=(^ contents.p.ip)
    ip
  --
::
++  grad  %noun
--
