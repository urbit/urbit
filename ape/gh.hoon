/?    314
=>  |%
    ++  move  (pair bone card)
    ++  sub-result
      $%  [%json json]
      ==
    ++  card  
      $%  [%diff sub-result]
          [%them wire (unit hiss)]
      ==
    --
|_  [hid=bowl cnt=@ hook=(unit ,@t)]
++  gh
  |_  [style=@tas pax=path]
  ++  real-pax  (scan "https://api.github.com{<`path`pax>}" auri:epur)
  ++  auth
    ['Authorization' 'Basic cGhpbGlwY21vbmt0ZXN0OjEzMzdwYXNzd29yZA==' ~]
  ++  scry
    ^-  hiss
    =-  ~&  [%requesting -]  -
    ?+  style  ~|(%invalid-style !!)
      %read-unauth  read-unauth
      %read-auth    read-auth
      %listen       listen
    ==
  ++  read-unauth  `hiss`[real-pax %get ~ ~]
  ++  read-auth  `hiss`[real-pax %get [auth ~ ~] ~]
  ++  listen
    ^-  hiss
    ?~  hook
      create-hook
    update-hook
  ++  create-hook
    ?>  ?=([@ @ @ *] pax)
    :*  %+  scan
          =+  [(trip i.pax) (trip i.t.pax)]
          "https://api.github.com/repos/{-<}/{->}/hooks"
        auri:epur
        %post  [auth ~ ~]  ~
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
    :*  %+  scan
          =+  [(trip i.pax) (trip i.t.pax)]
          "https://api.github.com/repos/{-<}/{->}/hooks/{(trip (need hook))}"
        auri:epur
        %post  [auth ~ ~]  ~
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
  [ost.hid %them [%x (scot %ud cnt) pax] ~ ~(scry gh i.pax t.pax)]
::
++  thou-x
  |=  [way=wire res=httr]
  ^-  [(list move) _+>.$]
  ?>  ?=([@ *] way)
  :_  +>.$  :_  ~
  :^  ost.hid  %diff  %json 
  ?.  &((gte p.res 200) (lth p.res 300))
    (jape "bad response {<p.res>} {<r.res>}")
  ?~  r.res
    (jape "empty response {<p.res>}")
  =+  (rush q.u.r.res apex:poja)
  ?~  -
    (jape "bad json {<p.res>} {<u.r.res>}")
  u.-
::
++  peek
  |=  [ren=@tas tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  ~ ::``noun/[ren tyl]
--
