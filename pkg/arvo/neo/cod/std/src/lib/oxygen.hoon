/-  manx-utils
::  Oxygen - a library for Eyre
|%
++  bind
  |=  =bowl:neo
  ^-  card:neo
  =/  path  (pout here.bowl)
  :*  #/[p/our.bowl]/$/eyre
      %poke 
      eyre-req/!>([%connect [~ path] here.bowl])
  ==
::
++  respond
  |=  [=bowl:neo eyre-id=@ta req=inbound-request:eyre =manx]
  ^-  (list card:neo)
  %+  turn
    :~  !>([eyre-id %head 200 ['content-type' 'text/html']~])
        !>([eyre-id %data `(manx-to-octs manx)])
        !>([eyre-id %done ~])
    ==
  |=  =vase
  :*  #/[p/our.bowl]/$/eyre
      %poke
      %eyre-sign
      vase
  ==
::
++  parse-url
  |=  =request:http
  ^-  [pax=path pam=(map @t @t)]
  =/  parsed
    %+  rash  url.request
    ;~  plug
        ;~(pfix fas (more fas smeg:de-purl:html))
        yque:de-purl:html
    ==
  :-  -.parsed
  (~(uni by (malt +.parsed)) (malt header-list.request))
::
++  parse-body
  |=  =request:http
  ^-  manx
  %+  fall
    (de-xml:html q:(fall body.request [p=0 q='']))
  *manx
::
++  manx-to-octs
  |=  man=manx
  ^-  octs
  (as-octt:mimes:html (en-xml:html man))
--