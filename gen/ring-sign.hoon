/+  ring
::
:-  %say
|=  [{now/@da eny/@uvJ bec/beak} [invited=(list @p) ~] ~]
:-  %noun
=/  s  (sign:ring p.bec now eny "Hello World" `[%scope 5] (sy invited))
~&  [%signature s]
=/  v  (verify:ring p.bec now "Hello World" s)
v
