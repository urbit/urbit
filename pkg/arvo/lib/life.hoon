/+  vial
=>  vial
|_  many=@
++  this  .
++  peek
  |=  pax=*
  ?>  =(pax /how/many)
  many
++  poke
  :: ~.  +7..
  |=  up=?
  ^-  _this
  ?:  up  this(many (inc many))
  this
--

:: [l=/v.100 s=[& %v]]
:: [l=/one/v.100 s=[[& &] [<one battery> %v]]
:: [l=/dec/one/v.100 s=[[& | & &] <dec battery> ~ <one battery> %v]
