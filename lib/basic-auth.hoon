!:
=+  keys=@t
|=  bal/(bale keys)
?~  key.bal
  ~|(%basic-auth-no-key ~_(leaf+"Run |init-auth-basic {<`path`dom.bal>}" !!))
=+  aut=authorization+(cat 3 'Basic ' key.bal)
~&  aut=`{@tas @t}`aut
|=(a/hiss [%send %_(a q.q (~(add ja q.q.a) -.aut +.aut))])
