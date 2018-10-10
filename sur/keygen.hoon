|%
+=  nodes  (map ship node)
+=  uodes  (map ship uode)
::
+=  node  [meta=meta seed=@ux wallet]
+=  uode  [meta=meta seed=@ux edkeys]
::
+=  meta  [typ=tape rev=@ud who=(unit ship)]
::
+=  wallet
  $:  keys=[public=@ux private=@ux]
      chain=@ux
      address=@ux
  ==
::
+=  vault
  $:  ticket=@q
      owner=node
      manage=nodes
      voting=nodes
      transfer=nodes
      spawn=nodes
      network=uodes
  ==
::
+=  edkeys  [crypt=keypair auth=keypair]
::
+=  keypair  [public=@ux secret=@ux]
::
+=  revisions  [manage=@ud voting=@ud transfer=@ud spawn=@ud network=@ud]
--
