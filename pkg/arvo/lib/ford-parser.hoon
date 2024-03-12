|%
++  run
  |=  txt=@t
  (scan (trip txt) apex:rein)
+$  file
  $:  deps=(list path)
      =hoon
  ==
++  rein
  |%
  ++  dep
    ^-  $-(nail (like path))
    (ifix [;~(plug fas lus gap) gay] stap)
  ++  deps
    (star dep)
  ++  hone
    =+  vaz=vast
    (ifix [gay gay] tall:vaz)
  ++  apex
    ::  ^-  $-(nail (like file))
    :: ^-  rule
    ;~(plug deps hone)
  --
--

