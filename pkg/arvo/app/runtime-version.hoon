/+  default-agent, dbug, verb
=>
|%
+$  app-state    [%0 latest=@t]
+$  card         card:agent:gall
+$  vere-update  [cur=vere next=(unit vere)]
::
++  set-timer
  |=  now=@da
  ^-  card
  :*  %pass
      /timer
      %arvo
      %b
      %wait
      (add now ~m30)
  ==
++  request-latest-vere
  ^-  card
  :*  %pass
      /vere
      %arvo
      %i
      %request
      [%'GET' 'https://bootstrap.urbit.org/vere/live/last' ~ ~]
      *outbound-config:iris
  ==
:: parse out the commit suffix for people on pre-release vere
:: these revisions look like /vere/~.2.7-de2d39b
:: we will have better pre-release (pace) handling later
++  get-current-version
  |=  current=vere
  ^-  @t
  =/  v
    %+  rush
      (slav %ta (rear rev.current))
    ;~((glue hep) (star ;~(pose nud dot)) (star aln))
  ?~  v  (slav %ta (rear rev.current))
  (crip -.u.v)
::
++  is-equal-version
  |=  [latest=@t current=vere]
  =(latest (get-current-version current))
--
^-  agent:gall
=|  app-state
=*  state  -
%-  agent:dbug
%+  verb  |
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  =/  cur  .^(vere %$ /(scot %p our.bowl)//(scot %da now.bowl)/zen/ver)
  :-  [request-latest-vere ~]
  this(latest (get-current-version cur))
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =/  old  !<(app-state vase)
  `this(state old)
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  ?+  path  (on-watch:def path)
      [%version ~]
    =/  cur  .^(vere %$ /(scot %p our.bowl)//(scot %da now.bowl)/zen/ver)
    :_  this
    :_  ~
    ?:  (is-equal-version latest cur)
      [%give %fact ~ %vere-update !>([cur=cur next=~])]
    =|  next=vere
    :*  %give  %fact  ~  %vere-update
        !>(`vere-update`[cur=cur next=`next(rev /vere/(scot %ta latest))])
    ==
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+  wire  (on-arvo:def wire sign-arvo)
    ::
      [%timer ~]
    ?>  ?=([%behn %wake *] sign-arvo)
    ?<  ?=(^ error.sign-arvo)
    [[request-latest-vere (set-timer now.bowl) ~] this]
    ::
      [%vere ~]
    ?>  ?=([%iris %http-response %finished *] sign-arvo)
    ?>  =(200 status-code.response-header.client-response.sign-arvo)
    =/  res  full-file.client-response.sign-arvo
    ?<  ?=(~ res)
    =/  cur  .^(vere %$ /(scot %p our.bowl)//(scot %da now.bowl)/zen/ver)
    :_  this(latest q.data.u.res)
    ?:  =(q.data.u.res latest)  ~
    =|  next=vere
    :_  ~
    :*  %give  %fact  ~[/version]  %vere-update
        !>(`vere-update`[cur=cur next=`next(rev /vere/(scot %ta latest))])
    ==
  ==
::
++  on-poke   on-poke:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
