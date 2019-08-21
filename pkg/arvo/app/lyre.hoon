/-  *lyre
/+  *server
::
/=  index
  /^  $-(json manx)
  /:  /===/app/lyre/index  /!noun/
::
/=  js
  /^  octs
  /;  as-octs:mimes:html
  /|  /:  /===/app/lyre/js/index  /js/
      /~  ~
  ==
::
/=  css
  /^  octs
  /;  as-octs:mimes:html
  /|  /:  /===/app/lyre/css/index  /css/
      /~  ~
  ==
::
|%
+$  move  [bone card]
+$  card
  $%  [%connect wire binding:eyre term]
      [%disconnect wire binding:eyre]
      [%http-response =http-event:http]
      [%diff diff]
  ==
::
+$  diff
  $%  [%json json]
  ==
::
+$  session
  $:  pax=path
  ==
+$  renderer  ~
::
+$  state
  $:  ses=(list session)
      cur=@u
      ren=(map path (unit renderer))
  ==
::
--
::
|_  [bol=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  ~&  prep+act.bol
  ?~  old
    :_  this
    [ost.bol %connect / [~ /'~lyre'] %lyre]~
  ::  XX  todo, populate renderer map, start clay watch
::  [~ this(+<+ *state)]           ::  flush state
  [~ this(+<+ ;;(state u.old))]  ::  keep state
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?+  a
    ~&  poked+a
    [~ this]
  ::
      %print-state
    ~&  +<+.this
    [~ this]
  ::
  ==
::
++  poke-lyre-action
  |=  act=action
  ^-  (quip move _this)
  ?-  -.act
      %new-session
    =/  new-pax
      ?~  pax.act  /  u.pax.act
    =.  this
      %=  this
        ses  (snoc ses new-pax)
        cur  (lent ses)
      ==
    [update-primary this]
  ::
      %delete-session
    =.  this
      %=  this
        ses  (oust [id.act 1] ses)
        cur  ?:((gte cur id.act) (dec cur) cur)
      ==
    [update-primary this]
  ::
      %switch-session
    ?>  (lth id.act (lent ses))
    =.  this
      %=  this
        cur   id.act
      ==
    [update-primary this]
  ::
      %set-path
    =.  this
      %=  this
        ses   ;:(welp (scag cur ses) [pax.act]~ (slag +(cur) ses))
      ==
    [update-primary this]
  ==
::
++  build-session-json
  ^-  json
  :-  %a
  %+  turn  `path`(snag cur ses)
  |=  seg=@t
  [%s seg]
::
++  update-primary
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib /primary bol)
  |=  [b=bone *]
  ^-  move
  [b %diff %json build-session-json]
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  [update-primary this]
::
++  bound
  |=  [wir=wire suc=? bin=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  =/  request-line  (parse-request-line url.request.inbound-request)
  ?+  request-line
    :_  this
    [ost.bol %http-response not-found:app]~
  ::  styling
  ::
      [[[~ %css] [%'~lyre' %index ~]] ~]
    :_  this
    [ost.bol %http-response (css-response:app css)]~
  ::  scripting
  ::
      [[[~ %js] [%'~lyre' %index ~]] ~]
    :_  this
    [ost.bol %http-response (js-response:app js)]~
  ::  home page; redirect to recent
  ::
      [[~ [%'~lyre' ~]] ~]
    =/  jon=json  build-session-json
    :_  this
    [ost.bol %http-response (manx-response:app (index jon))]~
  ==
::
--

