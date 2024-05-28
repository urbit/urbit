/-  openai
^-  kook:neo
|%
++  poke   (sy %openapi-req %iris-res ~)
++  state  pro/%txt
++  kids  ~
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  +*  keys  !<(@t state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =+  !<(keys=@t state-vase)
    ?+    stud  !!
        %iris-res
      %-  (slog (sell vax) ~)
      `txt/!>(keys)
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
