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
++  split-discussion-path
  |=  =path
  ^-  [=^path =url]
  ~|  [%path-too-short path]
  ?>  (gth (lent path) 1)  ::TODO  ?= would TMI
  =/  end=@ud  (dec (lent path))
  :-  (scag end path)
  (de-base64:mimes:html (snag end path))
::
++  en-json
  =,  enjs:format
  |%
  ++  submission
    |=  sub=^submission
    ^-  json
    =+  p=(page +.sub)
    ?>  ?=([%o *] p)
    o+(~(put by p.p) 'ship' (ship ship.sub))
  ::
  ++  page
    |=  =^page
    ^-  json
    %-  pairs
    :~  'title'^s+title.page
        'url'^s+url.page
        'timestamp'^(time time.page)
    ==
  ::
  ++  comment
    |=  =^comment
    ^-  json
    %-  pairs
    :~  'ship'^(ship ship.comment)
        'time'^(time time.comment)
        'udon'^s+udon.comment  ::TODO  convert?
    ==
  --
::
++  de-json
  =,  dejs:format
  |%
  ::  +action: json into action
  ::
  ::    formats:
  ::    {save: {path: '/path', title: 'title', url: 'url'}}
  ::    {note: {path: '/path', url: 'url', udon: 'text, maybe udon'}}
  ::
  ++  action
    |=  =json
    ^-  ^action
    ::TODO  the type system doesn't like +of here?
    ?+  json  ~|(json !!)
        [%o [%save *] ~ ~]
      :-  %save
      %.  q.n.p.json
      (ot 'path'^pa 'title'^so 'url'^so ~)
    ::
        [%o [%note *] ~ ~]
      :-  %note
      %.  q.n.p.json
      (ot 'path'^pa 'url'^so 'udon'^so ~)
    ==
  --
--
