:: tests for lsp JSON parsing
/-  lsp-types=language-server
/+  *test, *language-server-json
|%
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
++  test-parse-did-change
  %+  expect-eq
    !>  [%text-document--did-change ['file://' '1'] [[~ [[5 3] [5 3]]] `3 'text']~]
  !>  %-  parse-text-document--did-change
  ^-  json
  :-  %o
  %:  malt
    ['contentChanges' changes-jon]
    ['textDocument' text-document-jon]
    ~
  ==
--
