|%
+=  nodes  (map ship node)
+=  uodes  (map ship uode)
::
+=  node  [meta=meta seed=tape keys=wallet]
+=  uode  [meta=meta seed=@ux keys=edkeys]
::
+=  meta  [typ=tape rev=@ud who=ship]
::
+=  wallet  [keys=[public=@ux private=@ux] addr=@ux chain=@ux]
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
