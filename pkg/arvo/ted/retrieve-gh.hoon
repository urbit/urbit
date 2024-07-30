::  Retrieve the contents of a folder in a GitHub repository
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
  |=  [repo=path commit=cord]
  ^-  card:agent:gall
  =/  url  (cat 3 (cat 3 (crip "https://api.github.com/repos{<repo>}/git/trees/") commit) '?recursive=true')
  =/  =request:http  ~[%'GET' url ~[['User-Agent' 'vere-v1.20'] ['Accept' 'application/vnd.github.v3+json']]]
  =/  =task:iris  [%request request *outbound-config:iris]
  [%pass /http-req %arvo %i task]
::
++  build-file-request
  |=  [repo=path commit=cord path=cord]
  ^-  card:agent:gall
  =/  url  (cat 3 (cat 3 (crip "https://raw.githubusercontent.com{<repo>}/") commit) (cat 3 '/' path))
  =/  =request:http  ~[%'GET' url ~[['User-Agent' 'vere-v1.20']]]
  =/  =task:iris  [%request request *outbound-config:iris]
  [%pass /http-req %arvo %i task]
--
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  =bowl:spider  bind:m  get-bowl:strandio
=/  repo=path    ;;(path +<.q.arg)
=/  branch=cord  ;;(cord +>-.q.arg)
=/  dir=cord     ;;(cord +>+.q.arg)
?<  |(=(0 dir) =('/' dir))                               ::  sanitize dir
=/  dir-len=@  (met 3 dir)
=?  .  =('/' (cut 3 [0 1] dir))                          ::  /dir -> dir
  .(dir (cut 3 [1 dir-len] dir), dir-len (dec dir-len))
=?  dir  =('/' (cut 3 [(dec dir-len) 1] dir))            ::  dir/ -> dir
  (cut 3 [0 (dec dir-len)] dir)
~&  >  "Retrieving latest commit from https://github.com{<repo>}."
=/  tid  `cord`(cat 3 'strand_' (scot %uv (sham %retrieve-latest-commit eny.bowl)))
;<  ~       bind:m  %-  watch-our:strandio
                    :*  /awaiting/[tid]
                        %spider
                        /thread-result/[tid]
                    ==
;<  ~       bind:m  %-  poke-our:strandio
                    :*  %spider
                        %spider-start
                        !>  :*  `tid.bowl
                                `tid
                                byk.bowl(r da+now.bowl)
                                %retrieve-latest-commit
                                !>(`[repo branch])
                    ==      ==
;<  =cage   bind:m  (take-fact:strandio /awaiting/[tid])
;<  ~       bind:m  (take-kick:strandio /awaiting/[tid])
?:  =(%thread-fail p.cage)
  (strand-fail:strandio !<([term tang] q.cage))
?>  ?=(%thread-done p.cage)
=/  commit  ;;(cord q.q.cage)
::
~&  >  "Retrieving file list in /{(trip dir)}."
;<  ~  bind:m  (send-raw-card:strandio (build-tree-request repo commit))
;<  res=(pair wire sign-arvo)  bind:m  take-sign-arvo:strandio
?.  ?=([%iris %http-response %finished *] q.res)
  (strand-fail:strand %bad-sign ~)
?~  full-file.client-response.q.res
  (strand-fail:strand %no-body ~)
=/  res  (need (de:json:html q.data.u.full-file.client-response.q.res))
?>  ?=(%o -.res)
=/  res  (from-json (~(got by p.res) 'tree'))
=/  res
  %+  skim
    res
  |=  g=github
  ?&  =('blob' type.g)                                       :: only files, not trees
      !=('120000' mode.g)                                    :: do not resolve symlinks
      =((cat 3 dir '/') (cut 3 [0 +((met 3 dir))] path.g))   :: only grab from /{<dir>}
      !=('.' (cut 3 [0 1] (rear (stab (crip (cass (trip (cat 3 '/' path.g))))))))
  ==
::
~&  >  "Retrieving file URLs in /{(trip dir)}."
=/  sob=(list (pair path (pair @ud @)))  ~
=/  path-dir=path  (stab (add '/' (lsh [3 1] dir)))
|-
?~  res  (pure:m !>(`(list (pair path (pair @ud @)))`sob))
~&  >>  path.i.res
;<  ~  bind:m  (send-raw-card:strandio (build-file-request repo commit path.i.res))
;<  new=(pair wire sign-arvo)  bind:m  take-sign-arvo:strandio
?.  ?=([%iris %http-response %finished *] q.new)
  (strand-fail:strand %bad-sign ~)
?~  full-file.client-response.q.new
  (strand-fail:strand %no-body ~)
=/  t  (trip (cat 3 '/' path.i.res))
=/  i  (need (find "." t))
=/  p
  =/  p-prefixed  (stab (crip (cass (snap t i '/'))))
  |-  ^-  path
  ?~  path-dir  p-prefixed
  ?>  =(i.path-dir -.p-prefixed)
  $(path-dir t.path-dir, p-prefixed +.p-prefixed)
=/  s  [p data.u.full-file.client-response.q.new]
$(sob [s sob], res t.res)