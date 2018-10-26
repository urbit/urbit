::  Hood, generic: load named hood component's state from backup
::
::::  /hoon/load/hood/gen
  ::
/?    310
/+    hood-womb
::
::::
  ::
:-  %say
|=  $:  {now/@da eny/@uvJ byk/beak}
        {{dap/term pas/@uw ~} ~}
    ==
^-  {$hood-load ?(part:hood-womb)}
?+  dap  ~|(unknown-backup+dap !!)
  $womb
    =+  dat=.^(@ %cx (en-beam:format byk /jam-crub/womb-part/bak/hood/app))
    [%hood-load ;;(part:hood-womb (cue (dy:crub:crypto pas dat)))]
==
