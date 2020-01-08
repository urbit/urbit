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
    ['uri' `json`[%s 'file://']]
    ['version' `json`[%n '1']]
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
    ['text' `json`[%s 'text']]
    ['uri' `json`[%s 'file://uri']]
    ['version' `json`[%n '1']]
    ~
  ==
::
++  downcase
  |=  a=@
  (add a 32)
::
:: TODO: fix
++  unparse-method
  |=  =cord
  'textDocument/publishDiagnostics'

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
  ?+  method  [%unknown ~]
      %text-document--did-change
    (parse-text-document--did-change params)
      %text-document--did-open
    (parse-text-document--did-open params)
      %text-document--did-save
    (parse-text-document--did-save params)
      %text-document--did-close
    (parse-text-document--did-save params)
  ==
::
++  parse-text-document--did-save
  |=  jon=json
  ^-  text-document--did-save:request:lsp
  ?>  ?=([%o *] jon)
  =/  doc-id
    (~(got by p.jon) 'textDocument')
  :-  %text-document--did-save
  (parse-text-document-id doc-id)

::
++  parse-text-document--did-close
  |=  jon=json
  ^-  text-document--did-close:request:lsp
  ?>  ?=([%o *] jon)
  =/  doc-id
    (~(got by p.jon) 'textDocument')
  :-  %text-document--did-close
  (parse-text-document-id doc-id)

::
++  parse-text-document-item
  |=  jon=json
  ^-  text-document-item:lsp
  %.  jon
  =,  dejs:format
  %:  ot
    uri+so
    version+ni
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
    version+ni
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
++  enjs
  =,  enjs:format
  |%
  ++  response
    |=  response=response-message:lsp
    ^-  json
    =/  params=json
      ?-  +<.response
          %text-document--publish-diagnostics
        (publish-diagnostics +.response)
      ==
    %:  pairs
     [%method %s (unparse-method +<.response)]
      params+params
      ~
    ==
  ::
  ++  res
    |=  res=*
    ^-  json
    ~

  ++  position
    |=  =position:lsp
    ^-  json
    %:  pairs
      line+(numb row.position)
      character+(numb row.position)
      ~
    ==
  ::
  ++  range
    |=  =range:lsp
    ^-  json
    %:  pairs
      start+(position start.range)
      end+(position end.range)
      ~
    ==
  ::
  ++  diagnostic
    |=  diag=diagnostic:lsp
    ^-  json
    %:  pairs
      range+(range range.diag)
      severity+(numb severity.diag)
      message+s+message.diag
      ~
    ==
  ::
  ++  publish-diagnostics
    |=  pub=text-document--publish-diagnostics:response:lsp
    ^-  json
    %:  pairs
      uri+s+uri.pub
      diagnostics+a+(turn diagnostics.pub diagnostic)
      ~
    ==
  --
--
