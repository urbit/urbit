|%
::
+$  versioned-doc-id
  [uri=@t version=@]
::
::  ++  request
::    |%
::    +$  text-document--did-change
::    --
  ::  $%
  ::    text-document--did-change:request
  ::  ==
+$  response-message
  [id=(unit cord) all:response]
::
+$  request-message
  [id=(unit cord) all:request]
::
++  response
  |%
  ::
  +$  all
    $%
      publish-diagnostics
      unknown
    ==
  ::
  +$  unknown
    [%unknown ~]
  ::
  +$  publish-diagnostics
    [%'textDocument/publishDiagnostics' uri=@t diagnostics=(list diagnostic)]
  ::

::
::  ++  notification
::    |*  kind=response-kind
::    kind
+$  diagnostic
  [=range severity=@ud message=@t]
::
+$  position
  [row=@ud col=@ud]
::
+$  text-document-item
  [uri=@t version=@ text=@t]
::
++  request
  |%
  +$  all
    $%
      text-document--did-change
      text-document--did-open
      unknown
    ==
  +$  text-document--did-change
    [%text-document--did-change versioned-doc-id changes=(list change)]
  +$  text-document--did-open
    [%text-document--did-open text-document-item]
  +$  unknown
    [%unknown ~]
  --
::
+$  change
  $:  range=(unit range)
      range-length=(unit @ud)
      text=@t
  ==
::
+$  range
  $:  start=position
      end=position
  ==
::
--
