::  File writer module
::
::::  /hoon/write/lib
  ::
|%
++  part  ,[%write %0 pith]           :: no state
++  pith  ,~
--
::
::::
  ::
|%
++  data  $%([%json json] [%mime mime])
++  card  $%  [%exec wire @p ~ [beak silk]]
              [%info wire @p toro]
          ==
--
::
::::
  ::
|=  [bowl part]
=*  par  +<+
|_  moz=(list ,[bone card])
++  abet  [(flop moz) `part`par]
++  emit  |=(a=card %_(+> moz :_(moz [ost a])))
++  beak-now  byk(r [%da now])
:: ++  poke-json
::   |=  jon=json
::   =-  (poke--data (need (- jon.+)))
::   =>  jo
::   %-  ot  :~
::     pax/(cu deft (su fel:stab))
::     dat/(of json/some mime/(pe / (cu taco so)) ~)    ::  XX mite
::   ==
++  poke-tree
  |=  [sup=spur mim=mime]  ^+  abet
  (poke--data [`%md (flop sup)] %mime mim)  ::  XX determine extension
::
++  poke-paste
  |=  [typ=?(%hoon %md %txt) txt=@t]  ^+  abet
  (poke--data [`typ /web/paste/(scot %da now)] %mime / (taco txt))
::
++  poke--data
  |=  [[ext=(unit ,@t) pax=path] dat=data]  ^+  abet
  ?~  ext  $(ext [~ -.dat])
  =+  cay=?-(-.dat %json [-.dat !>(+.dat)], %mime [-.dat !>(+.dat)])
  ?:  =(u.ext -.dat)  (made pax ~ `cay)
  =<  abet
  %^  emit  %exec  write/pax                ::  XX defer %nice
  [our ~ beak-now %cast u.ext `cay]
::
++  made
  |=  [pax=wire @ res=gage]  ^+  abet
  ?.  =(our src)
    ~|(foreign-write/[our=our src=src] !!)
  ?+  -.res  ~|(gage/-.res !!)
    %|  (mean p.res)
    %&  =-  abet:(emit %info write/~ our -)
        (foal :(welp (tope beak-now ~) pax /[-.p.res]) p.res)
  ==
--
