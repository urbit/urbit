/-  *lyre
|^
|=  [pax=path ark=arch]
^-  dom
=/  bem=beam  (need (de-beam:format pax))
=/  dom-list=(list dom)
  %+  turn  ~(tap by dir.ark)
  |=  [nom=@ta ~]
  ^-  dom
  [%button [%text nom] (go (flop [nom s.bem]))]
::
=?  dom-list  !?=(~ s.bem)
  :-  [%button [%text '../'] (go (flop (slag 1 s.bem)))]
  dom-list
::
:*  %padding  10  10  10  10
  [%vertical dom-list]
==
::
++  go
  |=  pax=path
  ^-  poke
  :+  %lyre
    %lyre-action
  (frond:enjs:format %set-path %s (spat pax))
--
