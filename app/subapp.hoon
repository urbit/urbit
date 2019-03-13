/+  *server
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/subapp/index  /html/
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/subapp/script  /js/

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
++  poke-noun
  |=  asd=?(%bind %unbind)
  ^-  (quip move _this)
  :_  this
  ?:  =(%bind asd)
    [ost.bol %poke /subapp [our.bol %modulo] `poke`[%modulo-bind %subapp]]~
  [ost.bol %poke /subapp [our.bol %modulo] `poke`[%modulo-unbind %subapp]]~
++  prep
  |=  old=(unit @t)
  ^-  (quip move _this)
  ~&  %prep
  :-  [ost.bol %poke /subapp [our.bol %modulo] [%modulo-bind %subapp]]~
  ?~  old
    this
  this(sta u.old)
::
++  poke-handle-http-request
  %-  (require-authorization ost.bol move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
 =+  request-line=(parse-request-line url.request.inbound-request)
  =/  name=@t
    =+  back-path=(flop site.request-line)
    ?~  back-path
      'World'
    i.back-path
  ::
  ?:  =(name 'script')
    :_  this
    :~  ^-  move
        :-  ost.bol
        :*  %http-response
            [%start [200 ['content-type' 'application/javascript']~] [~ script] %.y]
        ==
    ==
  :_  this
  :~  ^-  move
      :-  ost.bol
      :*  %http-response
          [%start [200 ['content-type' 'text/html']~] [~ index] %.y]
      ==
  ==
--
