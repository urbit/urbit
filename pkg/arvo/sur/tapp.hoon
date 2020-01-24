|*  [poke-data=mold out-peer-data=mold]
|%
::
::  Possible async calls
::
+$  card
  $%  [%scry =path]
      [%wait wire @da]
      [%rest wire @da]
      [%poke wire dock poke-data]
      [%peer wire dock path]
      [%pull wire dock ~]
      [%diff out-peer-data]
      [%request wire request:http outbound-config:iris]
      [%cancel-request wire ~]
      [%connect wire binding:eyre term]
      [%http-response =http-event:http]
      [%rule wire %turf %put turf]
      [%source wire whos=(set ship) src=source:jael]
      [%sources wire ~]
      [%new-event wire =ship =udiff:point:able:jael]
      [%listen wire whos=(set ship) =source:jael]
      [%flog wire flog:dill]
  ==
::
::  Possible async responses
::
+$  sign
  $%  [%scry-result result=*]
      [%wake error=(unit tang)]
      [%coup =dock error=(unit tang)]
      [%quit =dock =path]
      [%reap =dock =path error=(unit tang)]
      [%bound success=? =binding:eyre]
      [%http-response response=client-response:iris]
      [%source whos=(set ship) =source:jael]
  ==
::
::  Outstanding contracts
::
+$  contract
  $%  [%wait at=@da]
      [%request ~]
  ==
--
