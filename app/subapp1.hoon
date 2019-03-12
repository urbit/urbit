/+  *server
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/subapp1/index  /&html&/!hymn/
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
    [ost.bol %poke /subapp1 [our.bol %modulo] `poke`[%modulo-bind %subapp1]]~
  [ost.bol %poke /subapp1 [our.bol %modulo] `poke`[%modulo-unbind %subapp1]]~
++  prep
  |=  old=(unit @t)
  ^-  (quip move _this)
  ~&  %prep
  :-  [ost.bol %poke /subapp1 [our.bol %modulo] [%modulo-bind %subapp1]]~
  ?~  old
    this
  this(sta u.old)
::
++  poke-handle-http-request
  %-  (require-authorization ost.bol move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  :_  this
  :~  ^-  move
      :-  ost.bol
      :*  %http-response
          [%start [200 ['content-type' 'text/html']~] [~ index] %.y]
      ==
  ==
--
