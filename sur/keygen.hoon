|%
+=  vault
  $:  ownership=nodes
      voting=nodes
      management=nodes
      transfer=nodes
      spawn=nodes
      network=uodes
  ==
::
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
  $:  ownership=@ud
      transfer=@ud
      spawn=@ud
      voting=@ud
      management=@ud
      network=@ud
  ==
--
