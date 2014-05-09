!:
:: /=main=/autobegin/hoon
::
=>  .(-< `who=@p`-<)
=>  .(+ =>(+ ^/===/lib/pony))
|=  [est=time sen=@uw]
|=  [bud=@p tic=@p eny=@ ges=gens ~]
^-  bowl
=+  bos=(sein bud)
?>  !=(bos bud)
?>  !=(who bud)
=<  (moss est tic eny ges)
|%
++  moss
  |=  [now=@da tic=@p tey=@ ges=gens]
  ^-  bowl
  =+  bur=(shax (mix (jam ges) tey))
  =+  loy=(bruw 2.048 bur)
  %-  (post bos %ta [bud tic ges pub:ex:loy])
  |=  [now=@da rup=(unit ,*)]
  :_  ~
  ?~  rup  ~[la/leaf/"request rejected"]
  =+  mac=`mace`[[0 sec:ex:loy] ~]
  =+  wil=((hard (unit will)) u.rup)
  ?~  wil
    :~  [%la %leaf "request rejected - invalid ticket"]
    ==
  :~  [%la %leaf "request approved"]
      [%xy /a `card`[%cash bud mac u.wil]]
  ==
--
