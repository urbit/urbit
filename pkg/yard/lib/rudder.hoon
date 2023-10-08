::  rudder: framework for routing & serving simple web frontends
::
::      v1.0.2: newborn helmsman
::
::    the primary usage pattern involves your app calling steer:rudder
::    with a configuration, then calling the resulting gate with an
::    incoming request and relevant context.
::
::      %.  [bowl [eyre-id inbound-request] dat]
::      %-  (steer:rudder _dat cmd)
::      [pages route adlib solve]
::
::    dat    is app state passed into and transformed by the frontend code.
::    cmd    is the type of app actions that the frontend may produce.
::    pages  is a (map term (page _dat cmd)), contains per-view frontend logic.
::    route  is a routing function, turning a url query into a $place.
::    adlib  gets called with the full request when no route is found.
::    solve  is a function that applies a cmd resulting from a POST request.
::
::    the library provides some default implementations for route and adlib,
::    which you can construct using +point and +fours respectively.
::
::    for examples and a more detailed description of handling http requests,
::    see /lib/rudder/poke-example.hoon
::
::    pages implement a bundle of view logic, each implementing a door
::    with three arms.
::
::    +build  gets called for GET requests, producing a $reply to render.
::    +argue  gets called for POST requests, turning it into a cmd.
::    +final  gets called after POST requests, producing a $reply to render.
::
::    for examples and a more detailed description of implementing a page,
::    see /lib/rudder/page-example.hoon
::
::TODO
::  - should rudder really be falling back to generic error messages when
::    calling +final after failure? what if apps/pages want to provide
::    their own generic error message?
::  - in the full-default setup, the behavior of +alert is a little bit
::    awkward. because +point forces routes to omit trailing slashes,
::    you cannot refer to "the current page" in a consistent way.
::    you have to either hardcode the page name, or pass the full url
::    from the inbound-request.
::    a router that forces inclusion of trailing slashes would let you
::    use '.', but has unconventional url semantics, and doesn't mesh
::    nicely with single-level routing.
::  - some inconsistency between the expected output of +adlib and +solve.
::    "briefless" +solve results may be common, so it's nice that they're
::    easy to write. for +adlib that probably isn't as relevant, and
::    the current factoring makes for a nice =^ in the lib code, but...
::    on the other hand, they're still different output types semantically,
::    so inconsistency isn't the end of the world. would have to see how
::    this ends up looking in practice.
::  - +argue is awkward because its function signature doesn't really work
::    if the cmd type is an atom.
::  - maybe unsupported methods should go to the fallback too?
::  - currently ambiguous: do you catch would-fail actions during +argue,
::    or in +solve? might be best to catch earlier, but this splits
::    or duplicates business logic between app and pages...
::
|%
+|  %types  ::  outputs, inputs, function signatures
::
+$  reply
  $%  [%page bod=manx]                                  ::  html page
      [%xtra hed=header-list:http bod=manx]             ::  html page w/ heads
      [%next loc=@t msg=brief]                          ::  303, succeeded
      [%move loc=@t]                                    ::  308, use other
      [%auth loc=@t]                                    ::  307, please log in
      [%code cod=@ud msg=brief]                         ::  error code page
      [%full ful=simple-payload:http]                   ::  full payload
  ==
::
+$  place
  $%  [%page ath=? nom=term]                            ::  serve from pages
      [%away loc=(list @t)]                             ::  308, redirect
  ==
::
+$  query
  $:  trail
      args=(list [key=@t value=@t])
  ==
::
+$  trail
  [ext=(unit @ta) site=(list @t)]
::
+$  order  [id=@ta inbound-request:eyre]
+$  route  $-(trail (unit place))
+$  brief  ?(~ @t)
::
++  page
  |*  [dat=mold cmd=mold]
  $_  ^|
  |_  [bowl:gall order dat]
  ++  build  |~([(list [k=@t v=@t]) (unit [? @t])] *reply)
  ++  argue  |~([header-list:http (unit octs)] *$@(brief cmd))
  ++  final  |~([success=? msg=brief] *reply)
  --
::
+$  card  card:agent:gall
::  pilot: core server logic
::
+|  %pilot
::
++  steer  ::  main helper constructor
  |*  [dat=mold cmd=mold]
  |^  serve
  +$  page   (^page dat cmd)
  +$  adlib  $-(order [[(unit reply) (list card)] dat])
  +$  solve  $-(cmd $@(brief [brief (list card) dat]))
  ::
  ++  serve  ::  main helper
    |=  [pages=(map @ta page) =route =adlib =solve]
    |=  [=bowl:gall =order =dat]
    ^-  (quip card _dat)
    =*  id  id.order
    =+  (purse url.request.order)
    =/  target=(unit place)
      (route -<)
    ::  if there is no route, fall back to adlib
    ::
    ?~  target
      =^  [res=(unit reply) caz=(list card)]  dat
        (adlib order)
      :_  dat
      ?~  res  caz
      (weld (spout id (paint u.res)) caz)
    ::  route might be a redirect
    ::
    ?:  ?=(%away -.u.target)
      =+  (rap 3 '/' (join '/' loc.u.target))
      [(spout id (paint %move -)) dat]
    ::  route might require authentication
    ::
    ?:  &(ath.u.target !authenticated.order)
      [(spout id (paint %auth url.request.order)) dat]
    ::  route might have messed up and pointed to nonexistent page
    ::
    ?.  (~(has by pages) nom.u.target)
      [(spout id (issue 404 (cat 3 'no such page: ' nom.u.target))) dat]
    ::
    %.  [bowl order dat]
    (apply (~(got by pages) nom.u.target) solve)
  ::
  ++  apply  ::  page usage helper
    |=  [=page =solve]
    |=  [=bowl:gall =order =dat]
    ^-  (quip card _dat)
    =.  page  ~(. page bowl order dat)
    =*  id    id.order
    ?+  method.request.order
      [(spout id (issue 405 ~)) dat]
    ::
        %'GET'
      :_  dat
      =+  (purse url.request.order)
      =^  msg  args
        ::NOTE  as set by %next replies
        ?~  msg=(get-header:http 'rmsg' args)  [~ args]
        [`[& u.msg] (delete-header:http 'rmsg' args)]
      %+  spout  id
      (paint (build:page args msg))
    ::
        %'POST'
      ?@  act=(argue:page [header-list body]:request.order)
        :_  dat
        =?  act  ?=(~ act)  'failed to parse request'
        (spout id (paint (final:page | act)))
      ?@  res=(solve act)
        :_  dat
        =?  act  ?=(~ act)  'failed to process request'
        (spout id (paint (final:page | res)))
      :_  +>.res
      =.  +<+>.page  +>.res
      (weld (spout id (paint (final:page & -.res))) +<.res)
    ==
  --
::  easy: hands-off steering behavior
::
+|  %easy
::
++  point  ::  simple single-level routing, +route
  |=  [base=(lest @t) auth=? have=(set term)]
  ^-  route
  |=  trail
  ^-  (unit place)
  ?~  site=(decap base site)  ~
  ?-  u.site
    ~           `[%page auth %index]
    [~ ~]       `[%away (snip ^site)]
    [%index ~]  `[%away (snip ^site)]
    [@ ~]       ?:((~(has in have) i.u.site) `[%page auth i.u.site] ~)
    [@ ~ ~]     `[%away (snip ^site)]
    *           ~
  ==
::
++  fours  ::  simple 404 responses, +adlib
  |*  dat=*
  ::  ^-  adlib:(rest * _dat)
  |=  *
  [[`[%code 404 'no route found'] ~] dat]
::
++  alert  ::  simple redirecting +final handler
  |=  [next=@t build=$-([(list [@t @t]) (unit [? @t])] reply)]
  |=  [done=? =brief]
  ^-  reply
  ?:  done  [%next next brief]
  (build ~ `[| `@t`brief])
::  cargo: payload generation
::
+|  %cargo
::
++  paint  ::  render response
  |=  =reply
  ^-  simple-payload:http
  ?-  -.reply
    %page  [[200 ['content-type' 'text/html']~] `(press bod.reply)]
    %xtra  =?  hed.reply  ?=(~ (get-header:http 'content-type' hed.reply))
             ['content-type'^'text/html' hed.reply]
           [[200 hed.reply] `(press bod.reply)]
    %next  =;  loc  [[303 ['location' loc]~] ~]
           ?~  msg.reply  loc.reply
           %+  rap  3
           :~  loc.reply
               ?:(?=(^ (find "?" (trip loc.reply))) '&' '?')
               'rmsg='
               (crip (en-urlt:html (trip msg.reply)))
           ==
    %move  [[308 ['location' loc.reply]~] ~]
    %auth  =/  loc  (crip (en-urlt:html (trip loc.reply)))
           [[307 ['location' (cat 3 '/~/login?redirect=' loc)]~] ~]
    %code  (issue +.reply)
    %full  ful.reply
  ==
::
++  issue  ::  render status code page
  |=  [cod=@ud msg=brief]
  ^-  simple-payload:http
  :-  [cod ~]
  =;  nom=@t
    `(as-octs:mimes:html (rap 3 ~[(scot %ud cod) ': ' nom '\0a' msg]))
  ?+  cod  ''
    %400  'bad request'
    %404  'not found'
    %405  'method not allowed'
    %500  'internal server error'
  ==
::  utils: fidgeting
::
+|  %utils
::
++  decap  ::  strip leading base from full site path
  |=  [base=(list @t) site=(list @t)]
  ^-  (unit (list @t))
  ?~  base  `site
  ?~  site  ~
  ?.  =(i.base i.site)  ~
  $(base t.base, site t.site)
::
++  frisk  ::  parse url-encoded form args
  |=  body=@t
  %-  ~(gas by *(map @t @t))
  (fall (rush body yquy:de-purl:html) ~)
::
::NOTE  the below (and $query) are also available in /lib/server.hoon,
::      but we reimplement them here for independence's sake.
::
++  purse  ::  url cord to query
  |=  url=@t
  ^-  query
  (fall (rush url ;~(plug apat:de-purl:html yque:de-purl:html)) [[~ ~] ~])
::
++  press  ::  manx to octs
  (cork en-xml:html as-octs:mimes:html)
::
++  spout  ::  build full response cards
  |=  [eyre-id=@ta simple-payload:http]
  ^-  (list card)
  =/  =path  /http-response/[eyre-id]
  :~  [%give %fact ~[path] [%http-response-header !>(response-header)]]
      [%give %fact ~[path] [%http-response-data !>(data)]]
      [%give %kick ~[path] ~]
  ==
--

