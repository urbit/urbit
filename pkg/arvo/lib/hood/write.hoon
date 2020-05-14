::  File writer module
::
::::  /hoon/write/hood/lib
  ::
/?    310
=,  format
=*  as-octs  as-octs:mimes:html
=,  space:userlib
|%
+$  part  {$write $0 pith}           :: no state
+$  pith  ~
--
::
::::
  ::
|%
++  data  $%({$json json} {$mime mime})
--
::
::::
  ::
|=  {bowl:gall part}
=*  par  +<+
|_  moz/(list card:agent:gall)
++  abet  [(flop moz) `part`par]
++  emit
  |=  =card:agent:gall
  %_(+> moz :_(moz card))
::
++  beak-now  byk(r [%da now])
++  poke-wipe
  |=  sup/path  ^+  abet                ::  XX determine extension, beak
  =+  ext=%md
  ?~  (file (en-beam beak-now [ext sup]))
    ~|(not-found+[ext `path`(flop sup)] !!)
  =-  abet:(emit %pass /write %arvo %c %info -)
  (fray (en-beam beak-now [ext sup]))
::
++  poke-tree
  |=  {sup/path mim/mime}  ^+  abet     ::  XX determine extension, beak
  (poke--data [`%md (flop sup)] %mime mim)
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
  =-  (crip +:<.^(@p %j pax)>)
  pax=/(scot %p our)/code/(scot %da now)/(scot %p our)
::
++  poke-sec-atom
  |=  {hot/host:eyre dat/@}
  ?>  ?=(%& -.hot)
  =.  p.hot  (scag 2 p.hot)      :: ignore subdomain
  =.  dat  (scot %uw (en:crub:crypto ames-secret dat))
  (poke--data [`%atom [%sec p.hot]] %mime / (as-octs dat))
::
++  poke--data
  |=  {{ext/(unit @t) pax/path} dat/data}  ^+  abet
  ?~  ext  $(ext [~ -.dat])
  =/  cay=cage  ?-(-.dat $json [-.dat !>(+.dat)], $mime [-.dat !>(+.dat)])
  =/  =cage
    ?:  =(u.ext -.dat)
      cay
    =+  .^(=tube:clay %cc /(scot %p our)/home/(scot %da now)/[p.cay]/[u.ext])
    [u.ext (tube q.cay)]
  =-  abet:(emit %pass /write %arvo %c %info -)
  (foal :(welp (en-beam beak-now ~) pax /[-.cage]) cage)
::
++  poke
  |=  [=mark =vase]
  ?+  mark  ~|([%poke-write-bad-mark mark] !!)
    %write-sec-atom  =;(f (f !<(_+<.f vase)) poke-sec-atom)
    %write-paste     =;(f (f !<(_+<.f vase)) poke-paste)
    %write-tree      =;(f (f !<(_+<.f vase)) poke-tree)
    %write-wipe      =;(f (f !<(_+<.f vase)) poke-wipe)
  ==
::
++  take
  |=  [=wire =sign-arvo]
  !!
::
++  take-agent
  |=  [=wire =sign:agent:gall]
  ~|([%write-bad-take-agent wire -.sign] !!)
--
