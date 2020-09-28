|%
++  zpub-bytes
  |=  zpub=tape
  ^-  (list @ux)
  =/  as-atom=@
    (de-base58:mimes:html zpub)
  %+  turn
    (flop (rip 3 as-atom))
  |=(bytes=@ `@ux`bytes)
--
