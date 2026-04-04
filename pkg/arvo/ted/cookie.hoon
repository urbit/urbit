/-  spider
/+  io=strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  =bowl:rand  bind:m  get-bowl:io
::
::  get public url for fake/real ship
=/  our-url
  ?:  .^(? %j /(scot %p our.bowl)/fake/(scot %da now.bowl))
    =/  =hart:eyre
      .^(hart:eyre %e /(scot %p our.bowl)/host/(scot %da now.bowl))
    ;:  welp
        "http"
        "://"
        (trip -.p.r.hart)
        ?~(q.hart "" (welp ":" (a-co:co u.q.hart)))
    ==
  =/  eauth-url
    .^((unit @t) %ex /(scot %p our.bowl)//(scot %da now.bowl)/eauth/url)
  ?~  eauth-url
    ~|(%failed-to-get-eauth-url !!)
  ::  remove /~/eauth from the end of .eauth-url
  (flop (swag [8 (lent (trip u.eauth-url))] (flop (trip u.eauth-url))))
::
;<  code=@p  bind:m  (scry:io @p /j/code/(scot %p our.bowl))
=/  body=@t  (cat 3 'password=' (rsh 3 (scot %p code)))
;<  ~  bind:m
  %-  send-request:io
  :*  %'POST'
      (crip (weld our-url "/~/login"))
      [['Content-Type' 'application/x-www-form-urlencoded'] ~]
      `[(met 3 body) body]
  ==
;<  =client-response:iris  bind:m  take-client-response:io
?>  ?=(%finished -.client-response)
=/  cookie=(unit @t)
  (get-header:http 'set-cookie' headers.response-header.client-response)
?~  cookie
  ~|(%failed-to-find-cookie !!)
(pure:m !>((need cookie)))
