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
    (scot %ta (wood url))
  --
::
++  break-discussion-path
  |=  =path
  ^-  [=^path =url]
  ?~  path  [/ '']
  :-  t.path
  ?:  =(~ i.path)  ''
  ~|  path
  (woad (slav %ta i.path))
::
::  zip sorted a into sorted b, maintaining sort order, avoiding duplicates
::
++  merge-sorted-unique
  |*  [sort=$-([* *] ?) a=(list) b=(list)]
  |-  ^-  ?(_a _b)
  ?~  a  b
  ?~  b  a
  ?:  =(i.a i.b)
    [i.a $(a t.a, b t.b)]
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
    %+  merge-sorted-unique
      |=  [a=page b=page]
      (gth time.a time.b)
    [a b]
  ::
  ++  submissions
    |=  [a=^submissions b=^submissions]
    ^+  a
    %+  merge-sorted-unique
      |=  [a=submission b=submission]
      (gth time.a time.b)
    [a b]
  ::
  ++  notes
    |=  [a=^notes b=^notes]
    ^+  a
    %+  merge-sorted-unique
      |=  [a=note b=note]
      (gth time.a time.b)
    [a b]
  ::
  ++  comments
    |=  [a=^comments b=^comments]
    ^+  a
    %+  merge-sorted-unique
      |=  [a=comment b=comment]
      (gth time.a time.b)
    [a b]
  --
::
++  en-json
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    ^-  json
    %-  frond
    :-  -.upd
    ?-  -.upd
        %local-pages
      %-  pairs
      :~  'path'^(path path.upd)
          'pages'^a+(turn pages.upd page)
      ==
    ::
        %submissions
      %-  pairs
      :~  'path'^(path path.upd)
          'pages'^a+(turn submissions.upd submission)
      ==
    ::
        %annotations
      %-  pairs
      :~  'path'^(path path.upd)
          'url'^s+url.upd
          'notes'^a+(turn notes.upd note)
      ==
    ::
        %discussions
      %-  pairs
      :~  'path'^(path path.upd)
          'url'^s+url.upd
          'comments'^a+(turn comments.upd comment)
      ==
    ::
        %observation
      %-  pairs
      :~  'path'^(path path.upd)
          'urls'^a+(turn ~(tap in urls.upd) |=(=url s+url))
      ==
    ==
  ::
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
        'time'^(time time.page)
    ==
  ::
  ++  comment
    |=  =^comment
    ^-  json
    =+  n=(note +.comment)
    ?>  ?=([%o *] n)
    o+(~(put by p.n) 'ship' (ship ship.comment))
  ::
  ++  note
    |=  =^note
    ^-  json
    %-  pairs
    :~  'time'^(time time.note)
        'udon'^s+udon.note  ::TODO  convert?
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
    ::
        [%o [%seen *] ~ ~]
      :-  %seen
      %.  q.n.p.json
      (ot 'path'^pa 'url'^(mu so) ~)
    ==
  --
--
