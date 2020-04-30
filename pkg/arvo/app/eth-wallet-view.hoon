/+  *server, default-agent
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/eth-wallet/index
  /|  /html/
      /~  ~
  ==
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/eth-wallet/js/tile
  /|  /js/
      /~  ~
  ==
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/eth-wallet/js/index
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/eth-wallet/css/index
  /|  /css/
      /~  ~
  ==
/=  eth-wallet-png
  /^  (map knot @)
  /:  /===/app/eth-wallet/img  /_  /png/
::
|%
+$  card  card:agent:gall
--
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this       .
      eth-wallet-core  +>
      cc         ~(. eth-wallet-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    =/  launcha
    [%launch-action !>([%add dap.bol /configs '/~eth-wallet/js/tile.js'])]
    :_  this
    :~  [%pass / %arvo %e %connect [~ /'~eth-wallet'] dap.bol]
        [%pass /eth-wallet-view %agent [our.bol %launch] %poke launcha]
    ==
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)
    ?+    mark  (on-poke:def mark vase)
        %handle-http-request
      =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
      :_  this
      %+  give-simple-payload:app  eyre-id
      %+  require-authorization:app  inbound-request
      poke-handle-http-request:cc
    ::
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card:agent:gall _this)
    ?:  ?=([%http-response *] path)
      `this
    ?:  =(/configs path)
      [[%give %fact ~ %json !>(*json)]~ this]
    (on-watch:def path)
  ::
  ++  on-agent  on-agent:def
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=(%bound +<.sign-arvo)
      (on-arvo:def wire sign-arvo)
    [~ this]
  ::
  ++  on-save  on-save:def
  ++  on-load  on-load:def
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-fail   on-fail:def
  --
::
::
|_  bol=bowl:gall
::
++  poke-handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  =+  url=(parse-request-line url.request.inbound-request)
  ?+  site.url  not-found:gen
      [%'~eth-wallet' %css %index ~]  (css-response:gen style)
      [%'~eth-wallet' %js %tile ~]    (js-response:gen tile-js)
      [%'~eth-wallet' %js %index ~]   (js-response:gen script)
  ::
      [%'~eth-wallet' %img @t *]
    =/  name=@t  i.t.t.site.url
    =/  img  (~(get by eth-wallet-png) name)
    ?~  img
      not-found:gen
    (png-response:gen (as-octs:mimes:html u.img))
  ::
      [%'~eth-wallet' *]  (html-response:gen index)
  ==
::
--
