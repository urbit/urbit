|%
++  parse-url
  |=  =request:http
  ^-  [pax=path pam=(map @t @t)]
  =/  parsed
    %+  rash  url.request
    ;~  plug
        ;~(pfix fas (more fas smeg:de-purl:html))
        yque:de-purl:html
    ==
      :: strip first 2 segments (/neo/hawk)
  :-  (slag 2 -.parsed)
  (malt +.parsed)
++  parse-body
  |=  =request:http
  ^-  manx
  %+  fall
    (de-xml:html q:(fall body.request [p=0 q='']))
  *manx
--
