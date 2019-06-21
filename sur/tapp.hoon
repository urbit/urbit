|*  [poke-data=mold out-peer-data=mold]
|%
::
::  Possible async calls
::
+$  card
  $%  [%wait wire @da]
      [%rest wire @da]
      [%poke wire dock poke-data]
      [%peer wire dock path]
      [%pull wire dock ~]
      [%diff out-peer-data]
      [%request wire request:http outbound-config:http-client]
      [%cancel-request wire ~]
  ==
::
::  Possible async responses
::
+$  sign
  $%  [%wake error=(unit tang)]
      [%coup =dock error=(unit tang)]
      [%quit =dock =path]
      [%reap =dock =path error=(unit tang)]
      [%http-response response=client-response:http-client]
  ==
::
::  Outstanding contracts
::
+$  contract
  $%  [%wait at=@da]
      [%request ~]
  ==
--
