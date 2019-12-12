::  link: social bookmarking
::
/-  *link
::
|%
++  site-from-url
  |=  =url
  ^-  site
  =/  murl=(unit purl:eyre)
    (de-purl:html url)
  ?~  murl  'http://example.com'
  %^  cat  3
    ::  render protocol
    ::
    =*  sec  p.p.u.murl
    ?:(sec 'https://' 'http://')
  ::  render host
  ::
  =*  host  r.p.u.murl
  ?-  -.host
    %&  (roll (join '.' p.host) (cury cat 3))
    %|  (rsh 3 1 (scot %if p.host))
  ==
::
++  en-json
  =,  enjs:format
  |%
  ++  page
    |=  =^page
    ^-  json
    %-  pairs
    :~  'title'^s+title.page
        'url'^s+url.page
        'timestamp'^(time time.page)
    ==
  --
::
++  de-json
  =,  dejs:format
  |%
  ++  action
    |=  =json
    ^-  ^action
    ?>  ?=([%o [%add *] ~ ~] json)
    :-  %add  ::TODO  +of doesn't please type system?
    %.  q.n.p.json
    (ot 'path'^pa 'title'^so 'url'^so ~)
  --
--
