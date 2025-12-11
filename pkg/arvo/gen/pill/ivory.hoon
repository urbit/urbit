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
|=  $:  [now=@da eny=@uvJ bec=beak]
        arg=$@(~ [top=path ~])
        ~
    ==
:-  %noun
^-  pill:pill
=/  sys=path
  ?^  arg  top.arg
  /(scot %p p.bec)/[q.bec]/(scot %da now)/sys
(ivory:pill sys)
