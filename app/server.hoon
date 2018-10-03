|%
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  card
  $%  [%connect wire [(unit @t) (list @t)] %server]
      [%http-response =raw-http-response:light]
  ==
::
+$  state
  $:  ::  count: sends back the count value
      ::
      count=(map bone @ud)
  ==
--
::  utilities:
::
|%
::
++  parse-request-line
  |=  url=@t
  ^-  [[(unit @ta) site=(list @t)] args=(list [key=@t value=@t])]
  (fall (rush url ;~(plug apat:de-purl:html yque:de-purl:html)) [[~ ~] ~])
::  +hello:
::
++  hello
  |=  name=@t
  ^-  octs
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  ;html
    ;head
      ;title:"Hello, {<(trip name)>}"
    ==
    ;body
      ;h1:"Hello, {<(trip name)>}"
    ==
  ==
--
::
|_  [bow=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ~&  %prep
  :-  [`move`[ost.bow [%connect / [~ /'~server'] %server]] ~]
  ?~  old
    this
  this(+<+ u.old)
::  alerts us that we were bound. we need this because the vane calls back.
::
++  bound
  |=  [wir=wire success=? binding=binding:light]
  ~&  [%bound success]
  [~ this]
::
::  received when we have a 
::
++  poke-handle-http-request
  |=  [authenticated=? secure=? address=address:light req=http-request:light]
  ^-  (quip move _this)
  ::
  =+  request-line=(parse-request-line url.req)
  ~&  [%request-line request-line]
  =/  name=@t
    =+  back-path=(flop site.request-line)
    ?~  back-path
      'World'
    i.back-path
  ~&  [%name name]
  ::
  :_  this
  :~  ^-  move
      :-  ost.bow
      :*  %http-response
          [%start 200 ['content-type' 'text/html']~ [~ (hello name)] %.y]
  ==  ==
--
