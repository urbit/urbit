/+  vial
=>  vial
|_  many=@
++  this  .
++  peek
  |=  pax=*
  ?>  =(pax /how/many)
  many
++  poke
  |=  up=?
  :: ~.  +7..
  ^-  _this
  ?:  up  this(many +(many))
  this(many (dec many))
--

:: [l=/vial.100 s=[& %vial]]
:: [l=/one/vial.100 s=[[& &] [<one battery> %vial]]
:: [l=/dec/one/vial.100 s=[[& | & &] <dec battery> ~ <one battery> %vial]
