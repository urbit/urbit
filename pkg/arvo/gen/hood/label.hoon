::  Kiln: add label to current revision of desk
::
::::  /hoon/label/hood/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=[syd=desk lab=@tas ~] aeon=aeon:clay ~]
    ==
:: handle optional aeon
::
=/  aey=(unit aeon:clay)
  ?:  =(0 aeon)
    ~
  `aeon
:-  %kiln-label
[syd.arg lab.arg aey]
