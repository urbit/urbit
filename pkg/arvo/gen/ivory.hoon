::  Produce an ivory pill
::
::::  /hoon/ivory/gen
  ::
/?    310
/+  pill
::
::::
  !:
:-  %say
|=  $:  [now=@da tick=@ud @ bec=beak]
        arg=$@(~ [top=path ~])
        ~
    ==
:-  %noun
^-  pill:pill
=/  sys=path
  ?^  arg  (en-pick now tick top.arg)
  (en-bema [p.bec q.bec [da+now ud+tick]] /sys)
(ivory:pill sys)
