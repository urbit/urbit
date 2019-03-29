/+  *server, collections
/=  index
  /:  /===/app/landscape/index  /!noun/
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/landscape/js/index-min
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/landscape/css/index
  /|  /css/
      /~  ~
  ==
/=  profile
  /:  /===/app/landscape/profile  /!noun/
/=  inbox
  /:  /===/app/landscape/inbox  /!noun/
/=  stream
  /:  /===/app/landscape/stream  /!noun/
/=  coll-elem
  /:  /===/app/landscape/collections/elem  /!noun/
/=  coll-new
  /:  /===/app/landscape/collections/new   /!noun/
::
|%
::
+$  move  [bone card]
::
+$  card
  $%  [%http-response =http-event:http]
      [%connect wire binding:http-server term]
      [%peer wire dock path]
      [%diff diff]
      [%quit ~]
  ==
+$  diff
  $%  [%hymn manx]
      [%json json]
  ==
::
--
::
|_  [bol=bowl:gall ~]
::
++  this  .
::
++  prep
  |=  old=(unit ~)
  ^-  (quip move _this)
  ?~  old
    :_  this
    [ost.bol %connect / [~ /'~landscape'] %landscape]~
  [~ this]
::
++  bound
  |=  [wir=wire success=? binding=binding:http-server]
  ^-  (quip move _this)
  [~ this]
::
++  peer-xship
  |=  wir=wire
  ^-  (quip move _this)
  ?+  wir
    !!    :: XX should we really crash on data sent from another ship?
  ::
      [%top @t *]
    =/  jon=?  =(t.t.wir [%json ~])
    =/  dif=diff  (coll-elem our.bol (slav %da i.t.wir) ~ jon)
    :_  this
    :~  [ost.bol %diff dif]
        [ost.bol %quit ~]
    ==
  ::
      [%new @t *]
    =/  jon=?  =(t.t.wir [%json ~])
    =/  pos  [*@da %new]
    =/  dif=diff  (coll-elem our.bol (slav %da i.t.wir) `pos jon)
    :_  this
    :~  [ost.bol %diff dif]
        [ost.bol %quit ~]
    ==
  ::
      [%view @t @t *]
    =/  jon=?  =(t.t.t.wir [%json ~])
    =/  pos  [(slav %da i.t.t.wir) %default]
    =/  dif=diff  (coll-elem our.bol (slav %da i.t.wir) `pos jon)
    :_  this
    :~  [ost.bol %diff dif]
        [ost.bol %quit ~]
    ==
  ::
      [%edit @t @t *]
    =/  jon=?  =(t.t.t.wir [%json ~])
    =/  pos  [(slav %da i.t.t.wir) %edit]
    =/  dif=diff  (coll-elem our.bol (slav %da i.t.wir) `pos jon)
    :_  this
    :~  [ost.bol %diff dif]
        [ost.bol %quit ~]
    ==
  ==
::
++  diff-hymn
  |=  [wir=wire hym=manx]
  ^-  (quip move _this)
  :_  this
  [ost.bol %http-response (html-response:app (manx-to-octs (index hym)))]~
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  ::
  =+  request-line=(parse-request-line url.request.inbound-request)
  ?+  site.request-line
    :_  this
    [ost.bol %http-response not-found:app]~
  ::
  ::  inbox page
  ::
      [%'~landscape' ~]
    =/  index-html=octs  (manx-to-octs (index inbox))
    :_  this
    [ost.bol %http-response (html-response:app index-html)]~
  ::
  ::  styling
  ::
      [%'~landscape' %css ~]
    :_  this
    [ost.bol %http-response (css-response:app style)]~
  ::
  ::  javascript
  ::
      [%'~landscape' %js ~]
    :_  this
    [ost.bol %http-response (js-response:app script)]~
  ::
  ::  profile page
  ::
      [%'~landscape' %profile @t ~]
    =/  profile-html=octs
      (manx-to-octs (index (profile i.t.t.site.request-line)))
    :_  this
    [ost.bol %http-response (html-response:app profile-html)]~
  ::
  ::  chat page
  ::
      [%'~landscape' %stream ~]
    =/  stream-html=octs  (manx-to-octs (index stream))
    :_  this
    [ost.bol %http-response (html-response:app stream-html)]~
  ::
  ::  collections top level page
  ::
      [%'~landscape' %collections @t @t ~]
    =/  shp=@p    (slav %p i.t.t.site.request-line)
    =/  col=@da   (slav %da i.t.t.t.site.request-line)
    ?:  =(shp our.bol)
      ::  local request
      ::
      =/  jon=?  =(ext.request-line [~ ~.json])
      =/  dif=diff  (coll-elem shp col ~ jon)
      ?-    -.dif
          %hymn
        =/  oct=octs  (manx-to-octs (index +.dif))
        :_  this
        [ost.bol %http-response (html-response:app oct)]~
      ::
          %json
        =/  oct=octs  (json-to-octs +.dif)
        :_  this
        [ost.bol %http-response (json-response:app oct)]~
      ==
    ::  foreign request
    ::
    ?:  =(ext.request-line [~ ~.json])
      ::  json format
      ::
      =/  pax=path  /xship/top/(scot %da col)/json
      :_  this
      [ost.bol %peer pax [shp %landscape] pax]~
    ::
    ::  html format
    =/  pax=path  /xship/top/(scot %da col)
    :_  this
    [ost.bol %peer pax [shp %landscape] pax]~
  ::
  ::  collections new post page
  ::
      [%'~landscape' %collections @t @t %new ~]
    =/  shp=@p    (slav %p i.t.t.site.request-line)
    =/  col=@da   (slav %da i.t.t.t.site.request-line)
    ?:  =(shp our.bol)
      =/  dif=diff  (coll-elem shp col `[*@da %new] |)
      ?>  ?=(%hymn -.dif)
      =/  new-html=octs
        (manx-to-octs (index +.dif))
      :_  this
      [ost.bol %http-response (html-response:app new-html)]~
    =/  pax=path  /xship/new/(scot %da col)
    :_  this
    [ost.bol %peer pax [shp %landscape] pax]~
  ::
  ::  collections view post page
  ::
      [%'~landscape' %collections @t @t @t ~]
    =/  shp=@p    (slav %p i.t.t.site.request-line)
    =/  col=@da   (slav %da i.t.t.t.site.request-line)
    =/  pos=@da   (slav %da i.t.t.t.t.site.request-line)
    ?:  =(shp our.bol)
      =/  jon=?  =(ext.request-line [~ ~.json])
      =/  dif=diff  (coll-elem shp col `[pos %default] jon)
      ?-    -.dif
          %hymn
        =/  oct=octs  (manx-to-octs (index +.dif))
        :_  this
        [ost.bol %http-response (html-response:app oct)]~
      ::
          %json
        =/  oct=octs  (json-to-octs +.dif)
        :_  this
        [ost.bol %http-response (json-response:app oct)]~
      ==
    ::  foreign request
    ::
    ?:  =(ext.request-line [~ ~.json])
      ::  json format
      ::
      =/  pax=path  /xship/view/(scot %da col)/(scot %da pos)/json
      :_  this
      [ost.bol %peer pax [shp %landscape] pax]~
    ::
    ::  html format
    =/  pax=path  /xship/view/(scot %da col)/(scot %da pos)
    :_  this
    [ost.bol %peer pax [shp %landscape] pax]~
  ::
  ::  collections edit post page
  ::
      [%'~landscape' %collections @t @t @t %edit ~]
    =/  shp=@p    (slav %p i.t.t.site.request-line)
    =/  col=@da   (slav %da i.t.t.t.site.request-line)
    =/  pos=@da   (slav %da i.t.t.t.t.site.request-line)
    ?:  =(shp our.bol)
      =/  dif=diff  (coll-elem shp col `[pos %edit] |)
      ?>  ?=(%hymn -.dif)
      =/  edit-html=octs
        (manx-to-octs (index +.dif))
      :_  this
      [ost.bol %http-response (html-response:app edit-html)]~
    =/  pax=path  /xship/edit/(scot %da col)/(scot %da pos)
    :_  this
    [ost.bol %peer pax [shp %landscape] pax]~
  ::
  ==
::
--
