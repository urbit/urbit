::  File writer
::
::::  /hoon#write#app
  ::
|_  {bowl $~}
++  beak-now  byk(r [%da now])
++  poke-json
  |=  jon+json
  =-  (poke--data (need (- jon.+)))
  =>  jo
  %-  ot  :~
    pax#(cu deft (su fel:stab))
    dat#(of json#some mime#(pe / (cu taco so)) ~)    ::  XX mite
  ==
::
++  poke-write-paste
  |=  {typ+?($hoon $md $txt) txt+@t}
  (poke--data [`typ /pub#paste#(scot %da now)] %mime / (taco txt))
::
++  poke--data
  |=  {{ext+(unit @t) pax+path} dat+$$({$json json} {$mime mime})}
  ?~  ext  $(ext [~ -.dat])
  =+  cay=?-(-.dat %json [-.dat !>(+.dat)], %mime [-.dat !>(+.dat)])
  ?:  =(u.ext -.dat)  (made pax ~ `cay)
  [[ost %exec pax our ~ beak-now %cast u.ext `cay]~ +>.$]  ::  XX defer %nice
::
++  made
  |=  {pax+wire @ res+gage}
  ?.  =(our src)
    ~|(foreign-write#[our=our src=src] !!)
  ?+  -.res  ~|(gage#-.res !!)
    $|  (mean p.res)
    $&  =-  [[ost %info / our -]~ +>.$]
        (foal :(welp (tope beak-now ~) pax /[-.p.res]) p.res)
  ==
--
