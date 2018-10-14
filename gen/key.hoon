::  Create a private key-file
::
/-  *sole
::
:-  %ask
|=  $:  [now=@da eny=@uvJ bec=beak]
        [who=ship ~]
        life=_1
    ==
^-  (sole-result (cask cord))
%+  sole-yo  leaf+"generating keys for {(scow %p who)}, life #{(scow %ud life)}"
%+  sole-lo  [%| %pope-pass "passphrase: "]
%+  sole-go  (boss 256 (star prn))
|=  fra/@t
=/  bur  (shaz (add who (shaz fra)))
=/  cub  (pit:nu:crub:crypto 512 bur)
::
=/  pub=pass  pub:ex:cub
=/  mag=cord  (end 3 1 pub)
?>  =('b' mag)
=/  bod=@  (rsh 3 1 pub)
=/  cry=@  (rsh 8 1 bod)
=/  sgn=@  (end 8 1 bod)
%+  sole-yo  leaf+"  authentication: 0x{(render-hex-bytes:ethereum 32 sgn)}"
%+  sole-yo  leaf+"  networking:     0x{(render-hex-bytes:ethereum 32 cry)}"
%+  sole-yo  leaf+"ethereum public keys:"
::
=/  sed=seed:able:jael
  [who life sec:ex:cub ~]
%-  sole-so  [%atom (scot %uw (jam sed))]
