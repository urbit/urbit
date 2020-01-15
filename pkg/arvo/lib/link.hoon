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
++  build-discussion-path
  |=  args=$@(url [=path =url])
  |^  ^-  path
      ?@  args  ~[(encode-url-for-path args)]
      :_  path.args
      (encode-url-for-path url.args)
  ::
  ++  encode-url-for-path
    |=  =url
    (crip (en-base64:mimes:html url))
  --
::
++  break-discussion-path
  |=  =path
  ^-  [=^path =url]
  ?~  path  [/ '']
  :-  t.path
  ?:  =(~ i.path)  ''
  (de-base64:mimes:html i.path)
::
::  zip sorted a into sorted b, maintaining sort order
::TODO  stdlib
++  merge-sorted
  |*  [sort=$-([* *] ?) a=(list) b=(list)]
  |-  ^-  ?(_a _b)
  ?~  a  b
  ?~  b  a
  ?:  (sort i.a i.b)
    [i.a $(a t.a)]
  [i.b $(b t.b)]
::
++  merge
  |%
  ++  pages
    ::TODO  we would just use +cury here but it don't work
    |=  [a=^pages b=^pages]
    ^+  a
    %+  merge-sorted
      |=  [a=page b=page]
      (gth time.a time.b)
    [a b]
  ::
  ++  submissions
    |=  [a=^submissions b=^submissions]
    ^+  a
    %+  merge-sorted
      |=  [a=submission b=submission]
      (gth time.a time.b)
    [a b]
  ::
  ++  notes
    |=  [a=^notes b=^notes]
    ^+  a
    %+  merge-sorted
      |=  [a=note b=note]
      (gth time.a time.b)
    [a b]
  ::
  ++  comments
    |=  [a=^comments b=^comments]
    ^+  a
    %+  merge-sorted
      |=  [a=comment b=comment]
      (gth time.a time.b)
    [a b]
  --
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
