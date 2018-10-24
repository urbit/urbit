|%
+=  nodes  (map ship node)
+=  uodes  (map ship uode)
::
+=  node  [meta=meta seed=@ux keys=wallet]
+=  uode  [meta=meta seed=@ux keys=edkeys]
::
+=  meta  [typ=tape rev=@ud who=(unit ship)]
::
+=  wallet  [public=@ux private=@ux chain=@ux]
::
+=  edkeys  [auth=keypair crypt=keypair]
::
+=  keypair  [public=@ux secret=@ux]
::
+=  revisions
  $:  owner=@ud
      transfer=@ud
      spawn=@ud
      delegate=@ud
      manage=@ud
      network=@ud
  ==
--
