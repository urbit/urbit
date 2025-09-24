::  Get the latest commit hash in a given branch of a GitHub repository
::
/-  spider
/+  strandio
=>
|%
++  from-json
    =,  dejs:format
    [%sha so]
::
++  build-commit-request
  |=  [repo=path branch=cord]
  ^-  card:agent:gall
  =/  url  (cat 3 (crip "https://api.github.com/repos{<repo>}/commits/") branch)
  =/  =request:http  ~[%'GET' url ~[['User-Agent' 'vere-v1.20'] ['Accept' 'application/vnd.github.v3+json']]]
  =/  =task:iris  [%request request *outbound-config:iris]
  [%pass /http-req %arvo %i task]
--
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  repo  ;;(path +<.q.arg)
=/  branch  ;;(cord +>.q.arg)
;<  ~  bind:m  (send-raw-card:strandio (build-commit-request repo branch))
;<  res=(pair wire sign-arvo)  bind:m  take-sign-arvo:strandio
?.  ?=([%iris %http-response %finished *] q.res)
  (strand-fail:strand %bad-sign ~)
?~  full-file.client-response.q.res
  (strand-fail:strand %no-body ~)
=/  res  (need (de:json:html q.data.u.full-file.client-response.q.res))
?>  ?=(%o -.res)
=/  commit  (from-json (~(got by ;;((map cord json) p.+.res)) 'sha'))
~&  >>  "Latest commit on branch {(trip branch)} is {(trip commit)}."
(pure:m !>(commit))
