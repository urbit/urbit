/-  spider
/+  strandio, dns
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
^-  form:m
::
;<  our=ship  bind:m  get-our:strandio
?.  ?=(%czar (clan:title our))
  ~|  %not-galaxy  !!
::
;<  now=@da  bind:m  get-time:strandio
=/  ames-domains=(list turf)
  .^((list turf) %j /(scot %p our)/turf/(scot %da now))
::
|-  ^-  form:m
=*  loop  $
?~  ames-domains
  (pure:m *vase)
::
=/  =turf
  (weld i.ames-domains /(crip +:(scow %p our)))
;<  good=?   bind:m  (turf-confirm-install:dns turf)
=/  msg=(pair cord tang)
  ?:  good
    [(cat 3 'confirmed access via ' (en-turf:html turf)) ~]
  :-  (cat 3 'unable to access via ' (en-turf:html turf))
  :~  leaf+"XX check via nslookup"
      leaf+"XX confirm port 80"
  ==
;<  ~        bind:m  (app-message:strandio %dns msg)
loop(ames-domains t.ames-domains)
