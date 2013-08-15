!:
::  /=main=/toy/cat/hoon
::
|=  *
|=  *
|=  ape=(list path)
:_  ~
:_  ~
^-  gift
:+  %mu  [~ %atom %t]
=-  |-  ^-  (list ,@t)
    ?~(foz ~ (weld i.foz $(foz t.foz)))
^=  foz
=|  foz=(list (list ,@t))
|-  ^+  foz
?~  ape  ~
[(lore ((hard ,@) .^(%cx i.ape))) $(ape t.ape)]
