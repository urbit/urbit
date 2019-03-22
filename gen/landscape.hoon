/+  *server
/=  index
  /:  /===/gen/landscape/index  /!noun/
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/gen/landscape/js/index-min
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/gen/landscape/css/index
  /|  /css/
      /~  ~
  ==
/=  profile
  /:  /===/gen/landscape/profile  /!noun/
/=  inbox
  /:  /===/gen/landscape/inbox  /!noun/
/=  stream
  /:  /===/gen/landscape/stream  /!noun/
/=  coll-elem
  /:  /===/gen/landscape/collections/elem  /!noun/
/=  coll-new
  /:  /===/gen/landscape/collections/new   /!noun/
::
::
|=  [[now=@da eny=@ bek=beak] $~ $~]
::
|=  [authorized=? =request:http]
^-  simple-payload:http
?.  authorized
  (login-redirect:gen request)
::
=+  request-line=(parse-request-line url.request)
?+  site.request-line
  ::
  =/  index-html=octs  (manx-to-octs (index inbox))
  (html-response:gen index-html)
  ::
    [%'~landscape' %css *]
  (css-response:gen style)
  ::
    [%'~landscape' %js *]
  (js-response:gen script)
  ::
    [%'~landscape' %profile @t *]
  =/  profile-html=octs
    (manx-to-octs (index (profile i.t.t.site.request-line)))
  (html-response:gen profile-html)
  ::
    [%'~landscape' %stream *]
  =/  stream-html=octs  (manx-to-octs (index stream))
  (html-response:gen stream-html)
  ::
    [%'~landscape' %collections @t @t *]
  =/  shp/@p  (slav %p i.t.t.site.request-line)
  =/  col/@da   (slav %da i.t.t.t.site.request-line)
  =*  tal  t.t.t.t.site.request-line
  ::  top level collection
  ::
  ?:  ?=(~ tal)
    =/  top-html=octs  (manx-to-octs (index (coll-elem shp col ~)))
    (html-response:gen top-html)
  :: make a new post, or view an old one
  ::
  ?:  ?=([@t ~] tal)
    ::  make a new post
    ::
    ?:  =(-.tal 'new')
      =/  new-html=octs  (manx-to-octs (index (coll-elem shp col `[*@da %new])))
      (html-response:gen new-html)
    ::  view a post
    ::
    =/  pos=[@da ?(%default %edit %new)]
      [(slav %da i.tal) %default]
    =/  post-html=octs  (manx-to-octs (index (coll-elem shp col `pos)))
    (html-response:gen post-html)
  ::  edit a post
  ::
  ?:  ?=([@t @t ~] tal)
    ?:  =(+<.tal 'edit')
      =/  pos=[@da ?(%default %edit %new)]
        [(slav %da i.tal) %edit]
      =/  edit-html=octs  (manx-to-octs (index (coll-elem shp col `pos)))
      (html-response:gen edit-html)
    not-found:gen
  not-found:gen
==
