/-  lsp=language-server
|%
::  ++  request-by-type
::    |*  type=mold
::    $>(type requester:lsp)
::
++  pos-jon
  ^-  json
  :-  %o
  %:  malt
    ['character' %n '3']
    ['line' %n '5']
    ~
  ==
::
++  range-jon
  ^-  json
  :-  %o
  %:  malt
    ['start' pos-jon]
    ['end' pos-jon]
    ~
  ==
::
++  change-jon
  ^-  json
  :-  %o
  %:  malt
    ['text' `json`[%s `@t`'text']]
    ['rangeLength' [%n '3']]
    ['range' range-jon]
    ~
  ==
::
++  changes-jon
  ^-  json
  :-  %a
  ^-  (list json)
  [change-jon ~]
::
++  text-document-jon
  ^-  json
  :-  %o
  %:  malt
    ['uri' %s 'file://']
    ['version' %s '1']
    ~
  ==
::
++  did-change-jon
  ^-  json
  :-  %o
  %:  malt
    ['contentChanges' changes-jon]
    ['textDocument' text-document-jon]
    ~
  ==
::
++  did-c-event-jon
  ^-  json
  :-  %o
  %:  malt
    ['id' `json`[%s '1']]
    ['method' `json`[%s `@t`'textDocument/didChange']]
    ['params' did-change-jon]
    ~
  ==
::
++  did-open-jon
  ^-  json
  :-  %o
  %:  malt
    ['textDocument' text-document-item-jon]
    ~
  ==
::
++  did-open-event-jon
  ^-  json
  :-  %o
  ^-  (map cord json)
  %:  malt
    ['id' `json`[%s '3']]
    ['method' `json`[%s 'textDocument/didOpen']]
    ['params' did-open-jon]
    ~
  ==
++  text-document-item-jon
  ^-  json
  :-  %o
  ^-  (map cord json)
  %:  malt
    ['text' %s 'text']
    ['uri' %s 'file://uri']
    ['version' %s '1']
    ~
  ==
::
++  downcase
  |=  a=@
  (add a 32)
++  parse-method
  |=  =tape
  ::  TODO: gross
  ^-  cord
  %-  crip
  %-  zing
  %+  join  "--"
  ^-  (list ^tape)
  %+  turn
    ^-  (list (list ^tape))
    %+  scan
      tape
    %+  more
      fas
    ;~  plug
      (star low)
      (star ;~(plug (cook downcase hig) (star low)))
    ==
  |=  words=(list ^tape)
  ^-  ^tape
  (zing (join "-" words))
::    ::
++  parse-request
  =,  dejs-soft:format
  |=  jon=json
  ?>  ?=([%o *] jon)
  =/  method=cord
    =+  `json`(~(got by p.jon) 'method')
    ?>  ?=([%s *] -)  (parse-method (trip p:-))
  =/  id=(unit cord)
    %-  (lift |=(j=json ?>(?=([%s *] j) p.j)))
    `(unit json)`(~(get by p.jon) 'id')
  =/  params=json
    (~(got by p.jon) 'params')
  ::  ^-  request:lsp
  ^-  request-message:lsp
  :-  id
  ?+  method  !!
      %text-document--did-change
    (parse-text-document--did-change params)
      %text-document--did-open
    (parse-text-document--did-open params)
  ==
::
++  parse-text-document-item
  |=  jon=json
  ^-  text-document-item:lsp
  %.  jon
  =,  dejs:format
  %:  ot
    uri+so
    version+so
    text+so
    ~
  ==
::
++  parse-text-document--did-change
  |=  jon=json
  ^-  text-document--did-change:request:lsp ::%text-document--did-change)
  :-  %text-document--did-change
  =,  dejs:format
  %.  jon
  %:  ot
    'textDocument'^parse-text-document-id
    'contentChanges'^parse-text-document-changes
    ~
    ::  'contentChanges'
  ==
++  parse-text-document--did-open
  |=  jon=json
  ^-  text-document--did-open:request:lsp
  =,  dejs:format
  ?>  ?=([%o *] jon)
  :-  %text-document--did-open
  (parse-text-document-item (~(got by p.jon) 'textDocument'))
::
++  parse-text-document-id
  =,  dejs:format
  %:  ot
    uri+so
    version+so
    ~
  ==
++  parse-text-document-changes
  =,  dejs:format
  %-  ar
  %:  ou
      range+(uf ~ (pe ~ range))
      'rangeLength'^(uf ~ (pe ~ ni))
      text+(un so)
      ~
  ==
++  range
 =,  dejs:format
%:  ot
    start+position
    end+position
    ~
==
::
++  position
 =,  dejs:format
%:  ot
    line+ni
    character+ni
    ~
==
--
