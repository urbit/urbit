/-  neo
|%
++  run
  |=  txt=@t
  (scan (trip txt) apex:rein)
+$  lib
  [face=(unit term) =name:neo]
+$  pro
  [face=term =stud:neo]
+$  file
  $:  pro=(list pro)
      lib=(list lib)
      =hoon
  ==
++  rein
  |%
  ++  nam
    :: ^-  $-(nail (like name:neo))
    ;~(plug ;~(pfix fas sig fed:ag) stip)
  ++  std
    ;~  pose
      ;~(plug sym ;~(pfix col sig fed:ag) ;~(pfix fas sym))
      sym
    ==
  ++  pro
    :: ^-  $-(nail (like ^pro))
    %+  rune  pat
    ;~  pose
      %+  cook
        |=  =stud:neo
        ?@  stud  [stud stud]
        [mark.stud stud]
      std
      ;~(plug sym ;~(pfix gap std))
    ==
  ++  lib
    :: ^-  $-(nail (like ^lib))
    %+  rune  cen
    ;~  pose
      (stag ~ nam)
      ;~(plug (stag ~ sym) ;~(pfix gap nam))
    ==
  ++  rune
    |*  [car=rule rul=rule]
    (ifix [;~(plug fas car gap) gay] rul)

  ++  libs
    :: ^-  $-(nail (like (list ^lib)))
    (star lib)
  ++  pros
    :: ^-  $-(nail (like (list ^pro)))
    (star pro)
  ++  hone
    :: ^-  $-(nail (like hoon))
    =+  vaz=vast
    (ifix [gay gay] tall:vaz)
  ++  apex
    :: ^-  rule
    ;~  plug 
      pros
      libs
      hone
    ==
  --
--

