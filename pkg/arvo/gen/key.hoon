::  Create a private key-file
::
/-  *sole
/+  *generators, ethereum
::
:-  %ask
|=  $:  [now=@da eny=@uvJ bec=beak]
        [who=ship ~]
        rift=_0
        life=_1
    ==
^-  (sole-result (cask cord))
%+  print   leaf+"generating keys for {(scow %p who)}, life #{(scow %ud life)}"
%+  prompt  [%| %pope-pass "passphrase: "]
%+  parse   (boss 256 (star prn))
|=  fra=@t
=/  bur  (shaz (add who (shaz fra)))
=/  cub  (pit:nu:crub:crypto 512 bur)
::
=/  pub=pass  pub:ex:cub
=/  mag=cord  (end 3 pub)
?>  =('b' mag)
=/  bod=@  (rsh 3 pub)
=/  cry=@  (rsh 8 bod)
=/  sgn=@  (end 8 bod)
%+  print  leaf+"  authentication: 0x{(render-hex-bytes:ethereum 32 sgn)}"
%+  print  leaf+"  networking:     0x{(render-hex-bytes:ethereum 32 cry)}"
%+  print  leaf+"ethereum public keys:"
::
=/  fed=feed:jael
  [[%2 ~] who rift [life sec:ex:cub]~]
%-  produce  [%atom (scot %uw (jam fed))]
