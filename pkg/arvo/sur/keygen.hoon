|%
+$  revision  @ud
+$  nodetype  tape
+$  mnemonic  tape
::
+$  vault
  $:  ownership=node
      voting=node
      management=node
      transfer=node
      spawn=node
      network=uode
  ==
::
+$  node  [type=nodetype seed=mnemonic keys=wallet]
+$  uode  [revi=revision seed=@ux keys=edkeys]
::
+$  wallet  [keys=[public=@ux private=@ux] addr=@ux chain=@ux]
::
+$  edkeys  [auth=keypair crypt=keypair]
::
+$  keypair  [public=@ux secret=@ux]
--
