/-  openai
^-  kook:neo
|%
++  poke   (sy %openapi-req %iris-res ~)
++  state  [%or pro/%sig pro/%loading pro/%done ~]
++  kids  ~
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =+  !<(keys=@t q.pail)
    ?+    stud  !!
        %iris-res
      =+  !<(=res:iris:neo vax)
      ?.  ?=(%finished -.dat.res)
        `pail
      ?~  full-file.dat.res
        `pail
      =/  body=cord  q.data.u.full-file.dat.res
      =/  jon=json  (need (de:json:html body))
      ~&  jon/jon
      ~&  result/(chat-completion-res:dejs:openai jon)
      `pail
    ::
        %openapi-req
      =+  !<(=req:openai vax)
      ?>  ?=(%chat-create -.q.req)
      :_  txt/!>(keys)
      :_  ~
      =/  =request:http
        :*  %'POST'
            'https://api.openai.com/v1/chat/completions'
            (headers:openai keys)
            `(as-octt:mimes:html (trip (en:json:html (chat-create-req:enjs:openai +.q.req))))
        ==
      ~&  req/request
      =/  =req:iris:neo  [#/foo request]
      [#/[p/our.bowl]/$/iris %poke iris-req/!>(req)]
    ==
  ::
      
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `(need old)
  --
--
