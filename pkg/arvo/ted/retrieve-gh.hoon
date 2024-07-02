::  Retrieve the contents of a /desk folder in a GitHub repository
::  Derived from pkova/deployer/ted/sync.hoon
::
/-  spider
/+  strandio
=>
|%
+$  github
  $:  path=@t
      mode=@t
      type=@t
      url=@t
  ==
++  from-json
  =,  dejs:format
  ^-  $-(json (list github))
  %-  ar
  %-  ot
  :~
    [%path so]
    [%mode so]
    [%type so]
    [%url so]
  ==
::
++  build-tree-request
  |=  [secret=@t commit=@t]
  ^-  card:agent:gall
  =/  url  (cat 3 (cat 3 'https://api.github.com/repos/urbit/urbit/git/trees/' commit) '?recursive=true')
  =/  =request:http  ~[%'GET' url ~[['User-Agent' 'vere-v1.20']]]
  =/  =task:iris  [%request request *outbound-config:iris]
  [%pass /http-req %arvo %i task]
::
++  build-file-request
  |=  [secret=@t commit=@t path=@t]
  ^-  card:agent:gall
  =/  url  (cat 3 (cat 3 'https://raw.githubusercontent.com/urbit/urbit/' commit) (cat 3 '/' path))
  =/  =request:http  [%'GET' url [['User-Agent' 'vere-v1.20'] ['Authorization' (cat 3 'token ' secret)] ~] ~]
  =/  =task:iris  [%request request *outbound-config:iris]
  [%pass /http-req %arvo %i task]
--
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  repo  !<  path  arg
~&  "Loading desk from https://github.com{<repo>}."
;<  ~  bind:m  (send-raw-card:strandio (build-tree-request secret commit))
;<  res=(pair wire sign-arvo)  bind:m  take-sign-arvo:strandio
?.  ?=([%iris %http-response %finished *] q.res)
  (strand-fail:strand %bad-sign ~)
?~  full-file.client-response.q.res
  (strand-fail:strand %no-body ~)
~&  `@t`type.u.full-file.client-response.q.res
=/  res  (need (de-json:html q.data.u.full-file.client-response.q.res))
?>  ?=(%o -.res)
=/  res  (from-json (~(got by p.res) 'tree'))
=/  res
  %+  skim
    res
  |=  g=github
  =/  pkg  =('pkg/arvo/' (cut 3 [0 9] path.g))
  ?&  =('blob' type.g)
      !=('120000' mode.g)
      ?|(=('pkg/arvo/' (cut 3 [0 9] path.g)) =('pkg/base-dev/' (cut 3 [0 13] path.g)))
      !=('.' (cut 3 [0 1] (rear (stab (crip (cass (trip (cat 3 '/' path.g))))))))
  ==
::
=/  sob=soba:clay  ~
|-
?~  res
  ;<  ps=(list path)  bind:m  (scry:strandio (list path) /ct/base)
  =/  ins=(set path)  (silt (turn sob head))
  =/  dif=(set path)  (~(dif in (silt ps)) ins)
  ~&  dif
  =/  sob  (weld (turn ~(tap by dif) |=(p=path [p %del ~])) sob)
  ;<  now=@da  bind:m  get-time:strandio
  ;<  =ship    bind:m  get-our:strandio
  ;<  ~  bind:m  (send-raw-card:strandio [%pass /deployer-commit %arvo %c %info %base %& sob])
  ;<  ~  bind:m  (send-raw-card:strandio [%pass /deployer-merge %arvo %c %merg %kids ship %base da+now %only-that])
  ;<  ~  bind:m  (sleep:strandio ~s0)  ::  wait for merge to complete
  ;<  kids=@uv  bind:m  (scry:strandio @uv /cz/kids)
  ::  =+  .^(kids=@uv %cz /(scot %p ship)/kids/(scot %da after))
  (pure:m !>([commit kids]))
~&  path.i.res
;<  ~  bind:m  (send-raw-card:strandio (build-file-request secret commit path.i.res))
;<  new=(pair wire sign-arvo)  bind:m  take-sign-arvo:strandio
?.  ?=([%iris %http-response %finished *] q.new)
  (strand-fail:strand %bad-sign ~)
?~  full-file.client-response.q.new
  (strand-fail:strand %no-body ~)
=/  t  (trip (cat 3 '/' path.i.res))
=/  i  (need (find "." t))
=/  p  (oust [0 2] (stab (crip (cass (snap t i '/')))))
=/  s  [p %ins %mime !>([/ data.u.full-file.client-response.q.new])]
$(sob [s sob], res t.res)