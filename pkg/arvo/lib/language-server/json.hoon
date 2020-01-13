/-  lsp=language-server
|%
::  ++  request-by-type
::    |*  type=mold
::    $>(type requester:lsp)
::
++  util
  |%
  ++  get-json-string
    |=  [jon=(map @t json) key=@t]
    ^-  (unit cord)
    =/  cord-jon=(unit json)
      (~(get by jon) key)
    ?~  cord-jon
      ~
    ?>  ?=([%s *] u.cord-jon)
    `p.u.cord-jon
  --
::
::
++  dejs
  =,  dejs:format
  |%
  ++  request
    |=  jon=json
    ?>  ?=([%o *] jon)
    =/  method=cord
      %-  method
      (trip (need (get-json-string:util p.jon 'method')))
    =/  id=cord
      (need (get-json-string:util p.jon 'id'))
    =/  params=json
      (~(got by p.jon) 'params')
    ^-  all:request:lsp
    |^
      ?+  method  [%unknown jon]
          %text-document--hover
        (text-document--hover params id)
      ==
      ::
      ++  text-document--hover
        |=  [params=json id=cord]
        ^-  text-document--hover:request:lsp
        :+  %text-document--hover
          id
        %.  params
        %:  ot
          position+position
          'textDocument'^text-document-id
          ~
        ==
      ::
      --
  ++  notification
    |=  jon=json
    ?>  ?=([%o *] jon)
    =/  method=cord
      %-  method
      (trip (need (get-json-string:util p.jon 'method')))
    =/  params=json
      (~(got by p.jon) 'params')
    ^-  all:notification:lsp
    |^
      ?+  method  [%unknown jon]
          %text-document--did-change
        (text-document--did-change params)
          %text-document--did-open
        (text-document--did-open params)
          %text-document--did-save
        (text-document--did-save params)
          %text-document--did-close
        (text-document--did-close params)
      ==
      ::
      ++  text-document--did-save
        |=  jon=json
        ^-  text-document--did-save:notification:lsp
        ?>  ?=([%o *] jon)
        =/  doc-id
          (~(got by p.jon) 'textDocument')
        :-  %text-document--did-save
        (text-document-id doc-id)
      ::
      ++  text-document--did-close
        |=  jon=json
        ^-  text-document--did-close:notification:lsp
        ?>  ?=([%o *] jon)
        =/  doc-id
          (~(got by p.jon) 'textDocument')
        :-  %text-document--did-close
        (text-document-id doc-id)
      ::

      ++  text-document--did-change
        |=  jon=json
        ^-  text-document--did-change:notification:lsp ::%text-document--did-change)
        :-  %text-document--did-change
        =,  dejs:format
        %.  jon
        %:  ot
          'textDocument'^text-document-id
          'contentChanges'^text-document-changes
          ~
          ::  'contentChanges'
        ==
      ::
      ++  text-document--did-open
        |=  jon=json
        ^-  text-document--did-open:notification:lsp
        =,  dejs:format
        ?>  ?=([%o *] jon)
        :-  %text-document--did-open
        (text-document-item (~(got by p.jon) 'textDocument'))
      --
    ::  Utilities
  ++  text-document-item
    |=  jon=json
    ^-  text-document-item:lsp
    %.  jon
    =,  dejs:format
    %:  ot
      uri+so
      version+(mu ni)
      text+so
      ~
    ==
  ++  text-document-id
    %:  ou
      uri+(un so)
      version+(uf ~ (pe ~ ni))
      ~
    ==
  ++  text-document-changes
    =,  dejs:format
    %-  ar
    %:  ou
        range+(uf ~ (pe ~ range))
        'rangeLength'^(uf ~ (pe ~ ni))
        text+(un so)
        ~
    ==
  ++  method
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
 ++  range
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
::
++  downcase
  |=  a=@
  (add a 32)
::
:: TODO: fix
++  unparse-method
  |=  =cord
  'textDocument/publishDiagnostics' 
::
++  enjs
  =,  enjs:format
  |%
  ++  text-document--publish-diagnostics
    |=  pub=text-document--publish-diagnostics:notification:lsp
    ^-  json
    %:  pairs
      uri+s+uri.pub
      diagnostics+a+(turn diagnostics.pub diagnostic)
      ~
    ==
  ++  notification
    |=  notification=all:notification:lsp
    ^-  json
    =/  params=json
      ?+  -.notification  !!
            %text-document--publish-diagnostics
          (text-document--publish-diagnostics notification)
        ==
    %:  pairs
     [%method %s (unparse-method -.notification)]
      params+params
      ~
    ==
  ::
  ++  response
    |=  res=all:response:lsp
    ^-  json
    |^
      ?-  -.res
          %text-document--hover
        (text-document--hover res)
      ==
      ::
      ++  wrap-in-id
        |=  [id=cord res=json]
        %:  pairs
          id+s+id
          result+res
          ~
        ==
      ++  text-document--hover
        |=  hov=text-document--hover:response:lsp
        %+  wrap-in-id  id.hov
        %+  frond  'contents'
        ?~  contents.hov
          ~
        s+u.contents.hov
      --

  ++  position
    |=  =position:lsp
    ^-  json
    %:  pairs
      line+(numb row.position)
      character+(numb col.position)
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

  --
::  examples
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
--
