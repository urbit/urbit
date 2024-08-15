/@  mime
^-  kook:neo
=<
|%
++  state  pro/%eyre-task
++  poke   (sy %rely ~)
++  kids  *kids:neo
++  deps
  %-  ~(gas by *band:neo)
  :~  :-  %src
      :-  req=|
      :-  [pro/%mime ~]
      ~
  ==
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    [~ pail]
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =+  !<(=task:eyre:neo vase)
    =/  [eyre-id=@ta req=inbound-request:eyre]  task
    ::
    ::  XX  make secure
    ::
    ::  ?.  authenticated.req
    ::    :_  [stud vase]
    ::    (unauth-cards [bowl task])
    ?+    method.request.req  ~|(%unsupported-http-method !!)
        %'GET'
      :_  [stud vase]
      (eyre-cards [bowl task])
    ==
  --
--
::
|%
++  unauth-cards
  |=  [=bowl:neo [eyre-id=@ta req=inbound-request:eyre]]
  =/  =pith:neo  #/[p/our.bowl]/$/eyre
  =/  loc
    %-  crip
    %+  welp  "/~/login?redirect="
    (trip url.request.req)
  :~
    [pith %poke eyre-sign/!>([eyre-id %head 307 ['Location' loc]~])]
    [pith %poke eyre-sign/!>([eyre-id %data ~])]
    [pith %poke eyre-sign/!>([eyre-id %done ~])]
  ==
++  eyre-cards
  |=  [=bowl:neo [eyre-id=@ta req=inbound-request:eyre]]
  =/  =pith:neo  #/[p/our.bowl]/$/eyre
  =/  mume=(unit mime)  (get-mime bowl)
  ?~  mume
    =/  headers  ['content-type' 'text/plain']~
    =/  data  `(as-octs:mimes:html 'not found')
    :~
      [pith %poke eyre-sign/!>([eyre-id %head 404 headers])]
      [pith %poke eyre-sign/!>([eyre-id %data data])]
      [pith %poke eyre-sign/!>([eyre-id %done ~])]
    ==
  =/  =mime  u.mume
  =/  headers  ['content-type' (crip (join '/' `(list @t)`p.mime))]~
  :~
    [pith %poke eyre-sign/!>([eyre-id %head 200 headers])]
    [pith %poke eyre-sign/!>([eyre-id %data `q.mime])]
    [pith %poke eyre-sign/!>([eyre-id %done ~])]
    [here.bowl %cull ~]
    [here.bowl %tomb ~]
  ==
::
++  get-mime
  |=  =bowl:neo
  ^-  (unit mime)
  =/  src  (~(get by deps.bowl) %src)
  ?~  src  ~
  =/  udea=(unit idea:neo)  (~(get of:neo q.u.src) /)
  ?~  udea  ~
  %-  mole
  |.  `mime`!<(mime q.pail.u.udea)
--
