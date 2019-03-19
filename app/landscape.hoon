/+  *server
/=  index
  /:  /===/app/landscape/index  /!noun/
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/landscape/js/index  /js/
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/landscape/css/index  /css/
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
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  card
  $%  [%poke wire dock poke]
      [%http-response =http-event:http]
    ==
+$  poke
  $%  [%modulo-bind app=term]
      [%modulo-unbind app=term]
  ==
--
::
|_  [bol=bowl:gall sta=@t]
::

++  this  .
::
++  prep
  |=  old=(unit @t)
  ^-  (quip move _this)
  :-  [ost.bol %poke / [our.bol %modulo] [%modulo-bind %landscape]]~
  ?~  old
    this
  this(sta u.old)
::
++  poke-handle-http-request
  %-  (require-authorization ost.bol move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  =+  request-line=(parse-request-line url.request.inbound-request)
  =+  back-path=(flop site.request-line)
  =/  name=@t
    ?~  back-path
      !!
    i.back-path
  ?<  ?=(~ site.request-line)
  ?+  t.site.request-line
    =/  index-html=octs  (manx-to-octs (index inbox))
    [[ost.bol %http-response (html-response index-html)]~ this]
    ::
      [%css *]
    [[ost.bol %http-response (css-response style)]~ this]
    ::
      [%js *]
    [[ost.bol %http-response (js-response script)]~ this]
    ::
      [%profile @t *]
    =/  profile-html=octs
      (manx-to-octs (index (profile i.t.t.site.request-line)))
    [[ost.bol %http-response (html-response profile-html)]~ this]
    ::
      [%stream *]
    =/  stream-html=octs  (manx-to-octs (index stream))
    [[ost.bol %http-response (html-response stream-html)]~ this]
    ::
      [%collections @t @t *]
    =/  shp/@p  (slav %p i.t.t.site.request-line)
    =/  col/@da   (slav %da i.t.t.t.site.request-line)
    =*  tal  t.t.t.t.site.request-line
    ::  top level collection
    ::
    ?:  ?=(~ tal)
      =/  top-html=octs  (manx-to-octs (index (coll-elem shp col ~)))
      [[ost.bol %http-response (html-response top-html)]~ this]
    :: make a new post, or view an old one
    ::
    ?:  ?=([@t ~] tal)
      ::  make a new post
      ::
      ?:  =(-.tal 'new')
        =/  new-html=octs  (manx-to-octs (index (coll-new shp col)))
        [[ost.bol %http-response (html-response new-html)]~ this]
      ::  view a post
      ::
      =/  pos=[@da ?(%default %edit)]  
        [(slav %da i.tal) %default]
      =/  post-html=octs  (manx-to-octs (index (coll-elem shp col `pos)))
      [[ost.bol %http-response (html-response post-html)]~ this]
    ::  edit a post
    ::
    ?:  ?=([@t @t ~] tal)
      ?:  =(+<.tal 'edit')
        =/  pos=[@da ?(%default %edit)]  
          [(slav %da i.tal) %edit]
        =/  edit-html=octs  (manx-to-octs (index (coll-elem shp col `pos)))
        [[ost.bol %http-response (html-response edit-html)]~ this]
      [~ this]
    [~ this]
  ==
--
