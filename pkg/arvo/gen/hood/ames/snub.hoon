::  Modify Ames blocklist
::
/?    310
::
::::
  ::
:-  %say
|=  [^ [form=?(%allow %deny) ships=(list ship)] ~]
:-  %helm-ames-snub
[form ships]
