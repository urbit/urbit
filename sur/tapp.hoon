|*  [poke-data=mold out-peer-data=mold]
|%
::
::  Possible async calls
::
+$  card
  $%  [%hiss wire ~ %httr %hiss hiss:eyre]
      [%them wire ~]
      [%wait wire @da]
      [%rest wire @da]
      [%poke wire dock poke-data]
      [%peer wire dock path]
      [%pull wire dock ~]
      [%diff out-peer-data]
  ==
::
::  Possible async responses
::
+$  sign
  $%  [%sigh =httr:eyre]
      [%wake error=(unit tang)]
      [%coup =dock error=(unit tang)]
      [%quit =dock =path]
      [%reap =dock =path error=(unit tang)]
  ==
::
::  Outstanding contracts
::
+$  contract
  $%  [%wait at=@da]
      [%hiss ~]
  ==
--
