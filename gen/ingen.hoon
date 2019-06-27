::  internal wallet generation
::
::  in dojo: */wallet/txt +ingen
::  .txt instead of .json to preverse the required ordering
::  do put the result through a beautifier
::
/+  keygen
=,  keygen
=,  ethe
=,  dejs:format
::
=>  |%
    +$  request
      $:  seed=@ux
          all=(list ship)
          transfer=(list ship)
          spawn=(list ship)
      ==
    ::
    +$  addresses
      $:  owner=address
          transfer=(unit address)
          spawn=(unit address)
          manage=address
          delegate=address
          network=[auth=@ux crypt=@ux]
      ==
    --
::
:-  %say
|=  $:  [now=@da eny=@uvJ byk=beak]
        [~ ~]
    ==
:-  %txt
:_  ~
::
::  parsing the input json
::
%-  crip
=+  jin=.^(json %cx (en-beam:format byk /json/wgr))
=/  lin=(list request)
  =-  ((ar (ot -)) jin)
  :~  'seed'^(cu |=(a=tape (scan (slag 2 a) hex)) sa)
      'ships'^(ar ni)
      'get_transfer'^(ar ni)
      'get_spawn'^(ar ni)
  ==
::
::  generating the output data
::
=/  out
  =-  %+  sort
        ^-  (list (pair ship addresses))
        (zing -)
      |=([[a=ship *] [b=ship *]] (lth a b))
  =+  dr=~(. sd ~)
  %+  turn  lin
  |=  request
  ^-  (list (pair ship addresses))
  =/  cn
    |=  [s=@ m=meta]
    (child-node-from-seed [(met 3 seed) s] m ~)
  ::
  =+  owner=public.keys:(wallet:dr `byts`[(met 3 seed) seed])
  =+  delegate=public.keys:(cn seed "delegate" 0 ~)
  =+  manage=(cn seed "manage" 0 ~)
  ::
  =/  transfers
    %-  ~(gas in *(map ship address))
    %+  turn  transfer
    |=  who=ship
    :-  who
    public.keys:(cn seed "transfer" 0 `who)
  ::
  =/  spawns
    %-  ~(gas in *(map ship address))
    %+  turn  spawn
    |=  who=ship
    :-  who
    public.keys:(cn seed "spawn" 0 `who)
  ::
  =/  netkeys
    %-  ~(gas in *(map ship [@ux @ux]))
    %+  turn  all
    |=  who=ship
    :-  who
    =<  [public.auth public.crypt]
    %-  urbit:dr
    %+  seed:dr
      [(met 3 seed) seed.manage]
    ["network" 0 `who]
  ::
  %+  turn  all
  |=  who=ship
  :-  who
  ^-  addresses
  :*  owner
      (~(get by transfers) who)
      (~(get by spawns) who)
      public.keys:manage
      delegate
      (~(got by netkeys) who)
  ==
::
::  turning output data into json
::
%.  ^-  json
    :-  %o
    %-  ~(gas in *(map @t json))
    =+  hex=|=(a=@ux s+(crip ['0' 'x' ((x-co:co 1) a)]))
    %+  turn  out
    |=  $:  who=ship
            addresses
        ==
    :-  (crip ((d-co:co 1) who))
    %-  pairs:enjs
    ^-  (list [@t json])
    :~  'ownership_address'^(hex owner)
        'transfer_address'^?~(transfer ~ (hex u.transfer))
        'spawn_address'^?~(spawn ~ (hex u.spawn))
        'mgmt_address'^(hex manage)
        'delegate_address'^(hex delegate)
        'authentication_key'^(hex auth.network)
        'encryption_key'^(hex crypt.network)
    ==
::
::NOTE  customized en-json logic because muh requirements
=|  rez=tape
|=  val=json
^-  tape
?~  val  (weld "null" rez)
?-    -.val
    $a
  :-  '['
  =.  rez  [']' rez]
  !.
  ?~  p.val  rez
  |-
  ?~  t.p.val  ^$(val i.p.val)
  ^$(val i.p.val, rez [',' $(p.val t.p.val)])
::
    $b  (weld ?:(p.val "true" "false") rez)
    $n  (weld (trip p.val) rez)
    $s
  :-  '"'
  =.  rez  ['"' rez]
  =+  viz=(trip p.val)
  !.
  |-  ^-  tape
  ?~  viz  rez
  =/  hed
    %.  i.viz
    =+  utf=|=(a/@ ['\\' 'u' ((x-co 4):co a)])
    |=  a/@  ^-  tape
    ?+  a  ?:((gth a 0x1f) [a ~] (utf a))
      $10  "\\n"
      $34  "\\\""
      $92  "\\\\"
    ==
  ?:  ?=({@ ~} hed)
    [i.hed $(viz t.viz)]
  (weld hed $(viz t.viz))
::
    $o
  :-  '{'
  =.  rez  ['}' rez]
  =/  viz
    %+  sort  ~(tap by p.val)
    |=  [[a=@t *] [b=@t *]]
    =+  aa=(rush a dem)
    =+  bb=(rush b dem)
    ?^  aa
      ?>  ?=(^ bb)
      (lth u.aa u.bb)
    =/  val
      |=  x=@t
      ?:  =(x 'ownership_address')    1
      ?:  =(x 'transfer_address')     2
      ?:  =(x 'spawn_address')        3
      ?:  =(x 'mgmt_address')         4
      ?:  =(x 'delegate_address')     5
      ?:  =(x 'authentication_key')   6
      ?:  =(x 'encryption_key')       7
      8
    (lth (val a) (val b))
  ?~  viz  rez
  !.
  |-  ^+  rez
  ?~  t.viz  ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
  =.  rez  [',' $(viz t.viz)]
  ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
==
