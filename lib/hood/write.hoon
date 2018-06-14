::  File writer module
::
::::  /hoon/write/hood/lib
  ::
/?    310
/-    profile
=,  format
=*  as-octs  as-octs:mimes:html
=,  space:userlib
|%
++  part  {$write $0 pith}           :: no state
++  pith  $~
--
::::
::::
  ::
|%
++  data  $%({$json json} {$mime mime})
++  card  $%  {$exec wire @p $~ {beak silk:ford}}
              {$info wire @p toro:clay}
          ==
--
::
::::
  ::
|=  {bowl:gall part}
=*  par  +<+
|_  moz/(list {bone card})
++  abet  [(flop moz) `part`par]
++  emit  |=(a/card %_(+> moz :_(moz [ost a])))
++  beak-now  byk(r [%da now])
++  poke-wipe
  |=  sup/path  ^+  abet                ::  XX determine extension, beak
  =+  ext=%md
  ?~  (file (en-beam beak-now [ext sup]))
    ~|(not-found+[ext `path`(flop sup)] !!)
  =-  abet:(emit %info write+~ our -)
  (fray (en-beam beak-now [ext sup]))
::
++  poke-tree
  |=  {sup/path mim/mime}  ^+  abet     ::  XX determine extension, beak
  (poke--data [`%md (flop sup)] %mime mim)
::
++  poke-profile
  |=  pro=profile:profile
  ^+  abet
  ?.  =(our src)
    ~|(foreign-write+[our=our src=src] !!)
  =/  serving=path  .^(path %e /(scot %p our)/serv/(scot %da now))
  ?>  ?=([@ @ *] serving)
  =/  latest   .^(cass:clay %cw /(scot %p our)/[i.t.serving]/(scot %da now))  ::
  =/  pax  /(scot %p our)/(scot %tas i.t.serving)/(scot %ud ud.latest)/web/profile
  =<  abet
  %-  emit
  =/  fol  (foal pax [%profile !>(pro)])
  [%info /write-profile our fol]
::
++  poke-paste
  |=  {typ/?($hoon $md $txt) txt/@t}  ^+  abet
  (poke--data [`typ /web/paste/(scot %da now)] %mime / (as-octs txt))
::
++  poke-comment
  |=  {sup/path him/ship txt/@t}  ^+  abet
  =+  pax=(welp (flop sup) /comments/(scot %da now))
  =.  txt
    %+  rap  3  :~
      '## `'  (scot %p him)  '`'
      '\0a'  txt
    ==
  (poke--data [`%md pax] %mime / (as-octs txt))
::
++  poke-fora-post
  |=  {sup/path him/ship hed/@t txt/@t}  ^+  abet
  =+  pax=(welp (flop sup) /posts/(cat 3 (scot %da now) '~'))
  =.  txt
    %-  crip
    """
    ---
    type: post
    date: {<now>}
    title: {(trip hed)}
    author: {<him>}
    navsort: bump
    navuptwo: true
    comments: reverse
    ---

    {(trip txt)}
    """
  (poke--data [`%md pax] %mime / (as-octs txt))
::
++  ames-secret
  ^-  @t
  =-  (crip +:<.^(@p %a pax)>)
  pax=/(scot %p our)/code/(scot %da now)/(scot %p our)
::
++  poke-sec-atom
  |=  {hot/host:eyre dat/@}
  ?>  ?=($& -.hot)
  =.  p.hot  (scag 2 p.hot)      :: ignore subdomain
  =.  dat  (scot %uw (en:crua:crypto ames-secret dat))
  (poke--data [`%atom [%sec p.hot]] %mime / (as-octs dat))
::
++  poke--data
  |=  {{ext/(unit @t) pax/path} dat/data}  ^+  abet
  ?~  ext  $(ext [~ -.dat])
  =+  cay=?-(-.dat $json [-.dat !>(+.dat)], $mime [-.dat !>(+.dat)])
  ?:  =(u.ext -.dat)
    (made pax ~ &+cay)
  =<  abet
  %^  emit  %exec  write+pax                ::  XX defer %nice
  [our ~ beak-now %cast u.ext $+cay]
::
++  made
  |=  {pax/wire @ res/gage:ford}  ^+  abet
  :: ?.  =(our src)
  ::   ~|(foreign-write/[our=our src=src] !!)
  ?+  -.res  ~|(gage+-.res !!)
    $|  (mean p.res)
    $&  =-  abet:(emit %info write+~ our -)
        (foal :(welp (en-beam beak-now ~) pax /[-.p.res]) p.res)
  ==
--
