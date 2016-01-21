/?    314
=>  |%
    ++  move  (pair bone card)
    ++  sub-result
      $%  [%json json]
      ==
    ++  card  
      $%  [%diff sub-result]
          [%them wire (unit hiss)]
          [%hiss wire %httr [%hiss hiss]]
      ==
    --
|_  [hid=bowl cnt=@ hook=(unit ,@t)]
++  gh
  |_  [style=@tas pax=path]
  ++  req-url  `purl`[[& ~ `/com/github/api] [~ pax] ~]
  ++  scry
    ^-  [? hiss]
    =-  ~&  [%requesting -]  -
    ?+  style  ~|(%invalid-style !!)
      %read-unauth  [| read]
      %read-auth    [& read]
      %listen       [& listen]
    ==
  ++  read  `hiss`[req-url %get ~ ~]
  ++  listen
    ^-  hiss
    ?~  hook
      create-hook
    update-hook
  ++  create-hook
    ?>  ?=([@ @ @ *] pax)
    :*  req-url(pax /[i.pax]/[i.t.pax]/hooks)
        %post  ~  ~
        %-  taco  %-  crip  %-  pojo  %-  jobe  :~
          name/s/%web
          active/b/&
          events/a/(turn `(list ,@t)`t.t.pax |=(a=@t s/a))
          :-  %config
          %-  jobe  :~
            [%url s/'http://107.170.195.5:8445/~/to/gh/json.json?anon&wire=/']
            [%'content_type' s/%json]
          ==
        ==
    ==
  ++  update-hook
    ?>  ?=([@ @ @ *] pax)
    :*  req-url(pax /[i.pax]/[i.t.pax]/hooks/(need hook))
        %post  ~  ~
        %-  taco  %-  crip  %-  pojo  %-  jobe  :~
          [%'add_events' a/(turn `(list ,@t)`t.t.pax |=(a=@t s/a))]
        ==
    ==
  --
::
++  poke-json
  |=  jon=json
  ^-  [(list move) _+>.$]
  =+  ^-  [repo=json sender=json hok=json hook-id=@t zen=json]
      %-  need
      %.  jon
      =>  jo
      (ot repository/some sender/some hook/some 'hook_id'^no zen/some ~)
  ~&  [%id hook-id]
  [~ +>.$(hook `hook-id)]
::
++  peer-scry-x
  |=  pax=path
  ^-  [(list move) _+>.$]
  :_  +>.$(cnt now.hid)  :_  ~
  ?>  ?=(^ pax)
  =-  ~&  [%peered -]  -
  =+  wir=[%x (scot %ud cnt) pax]
  =+  [aut hiz]=~(scry gh i.pax t.pax)
  ?.  aut  [ost.hid %them wir ~ hiz]
  [ost.hid %hiss wir %httr [%hiss hiz]]
::
++  sigh-httr-x  thou-x
++  thou-x
  |=  [way=wire res=httr]
  ^-  [(list move) _+>.$]
  ?>  ?=([@ *] way)
  :_  +>.$  :_  ~
  :^  ost.hid  %diff  %json
  ?~  r.res
    (jobe err/s/%empty-response code/(jone p.res) ~)
  =+  (rush q.u.r.res apex:poja)
  ?~  -
    (jobe err/s/%bad-json code/(jone p.res) body/s/q.u.r.res ~)
  ?.  =(2 (div p.res 100))
    (jobe err/s/%request-rejected code/(jone p.res) msg/u.- ~)
  u.-
::
++  peek
  |=  [ren=@tas tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  ~ ::``noun/[ren tyl]
--
