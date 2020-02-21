:: tests for lsp JSON parsing
/-  lsp-sur=language-server
/+  *test, *language-server-json
=,  enjs:format
|%
::
++  position
  [5 3]
++  position-jon
  ^-  json
  :-  %o
  %:  malt
    ['character' %n '3']
    ['line' %n '5']
    ~
  ==
::
++  range
  [position position]
::
++  range-jon
  ^-  json
  :-  %o
  %:  malt
    ['start' position-jon]
    ['end' position-jon]
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
++  text-document-item
  ^-  text-document-item:lsp-sur
  ['file://' `1 'text']
::
++  text-document-item-jon
  ^-  json
  :-  %o
  %:  malt
    ['uri' `json`[%s 'file://']]
    ['version' `json`[%n '1']]
    ['text' `json`[%s 'text']]
    ~
  ==
::
++  text-document-id
  ^-  versioned-doc-id:lsp-sur
  ['file://' `1]
::
++  text-document-id-jon
  ^-  json
  :-  %o
  %:  malt
    ['uri' `json`[%s 'file://']]
    ['version' `json`[%n '1']]
    ~
  ==
++  diagnostic
  ^-  diagnostic:lsp-sur
  [range 1 'Syntax Error']
::
++  diagnostic-jon
  ^-  json
  :-  %o
  %:  malt
    ['range' range-jon]
    ['severity' `json`[%n '1']]
    ['message' `json`[%s 'Syntax Error']]
    ~
  ==
::
++  completion-item
  ^-  completion-item:lsp-sur
  ['label' 1 'detail' 'doc' 'snippet' 1]
::
++  completion-item-jon
  ^-  json
  %:  pairs
    label+s+'label'
    detail+s+'detail'
    kind+n+'1'
    documentation+s+'doc'
    'insertText'^s+'snippet'
    'insertTextFormat'^n+'1'
    ~
  ==
::
++  make-notification-jon
  |=  [method=@t params=json]
  ^-  json
  %:  pairs
    ['method' `json`[%s method]]
    params+params
    ~
  ==
++  make-request-jon
  |=  [id=@t method=@t params=json]
  ^-  json
  %:  pairs
    ['id' `json`[%s id]]
    ['method' `json`[%s method]]
    params+params
    ~
  ==
++  make-response-jon
  |=  [id=@t result=json]
  %:  pairs
    id+s+id
    result+result
    ~
  ==
::
:: Notifications
::
++  test-parse-did-change
  %+  expect-eq
    !>  ^-  all:notification:lsp-sur
    [%text-document--did-change text-document-id [[~ [[5 3] [5 3]]] `3 'text']~]
  !>  %-  notification:dejs
  %+  make-notification-jon  'textDocument/didChange'
  :-  %o
  %:  malt
    ['contentChanges' changes-jon]
    ['textDocument' text-document-id-jon]
    ~
  ==
::
++  test-parse-did-save
  %+  expect-eq
    !>  ^-  all:notification:lsp-sur
    [%text-document--did-save text-document-id]
  !>  %-  notification:dejs
  %+  make-notification-jon  'textDocument/didSave'
  :-  %o
  %:  malt
    ['textDocument' text-document-id-jon]
    ~
  ==
::
++  test-parse-did-close
  %+  expect-eq
    !>  ^-  all:notification:lsp-sur
    [%text-document--did-close text-document-id]
  !>  %-  notification:dejs
  %+  make-notification-jon  'textDocument/didClose'
  :-  %o
  %:  malt
    ['textDocument' text-document-id-jon]
    ~
  ==
::
++  test-parse-did-open
  %+  expect-eq
    !>  ^-  all:notification:lsp-sur
    [%text-document--did-open text-document-item]
  !>  %-  notification:dejs
  %+  make-notification-jon  'textDocument/didOpen'
  :-  %o
  %:  malt
    ['textDocument' text-document-item-jon]
    ~
  ==
::
:: Requests
::
++  test-parse-hover
  %+  expect-eq
    !>  ^-  all:request:lsp-sur
    [%text-document--hover '3' position text-document-id]
  !>  %-  request:dejs
  ^-  json
  %^  make-request-jon  '3'  'textDocument/hover'
  :-  %o
  %:  malt
    ['position' position-jon]
    ['textDocument' text-document-id-jon]
    ~
  ==
++  test-parse-completion
  %+  expect-eq
    !>  ^-  all:request:lsp-sur
    [%text-document--completion '3' position text-document-id]
  !>  %-  request:dejs
  ^-  json
  %^  make-request-jon  '3'  'textDocument/completion'
  :-  %o
  %:  malt
    ['position' position-jon]
    ['textDocument' text-document-id-jon]
    ~
  ==
::  to JSON
::
::  notifications
::
++  test-enjs-publish-diagnostics
  %+  expect-eq
    !>  %-  notification:enjs
    [%text-document--publish-diagnostics 'file://' [diagnostic ~]]
  !>  ^-  json
  %+  make-notification-jon  'textDocument/publishDiagnostics'
  :-  %o
  %:  malt
    ['uri' `json`[%s 'file://']]
    ['diagnostics' `json`[%a [diagnostic-jon ~]]]
    ~
  ==
::  responses
++  test-enjs-hover
  %+  expect-eq
    !>   %-  response:enjs
    [%text-document--hover '1' `'text']
  !>  ^-  json
  %+  make-response-jon  '1'
  %+  frond  'contents'
  s+'text'
::
++  test-enjs-completion
  %+  expect-eq
    !>  %-  response:enjs
    [%text-document--completion '1' ~[completion-item]]
  !>  ^-  json
  %+  make-response-jon  '1'
  [%a ~[completion-item-jon]]
--
