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
      text-document--publish-diagnostics
    ==
  ::
  ::
  +$  text-document--publish-diagnostics
    [%text-document--publish-diagnostics uri=@t diagnostics=(list diagnostic)]
  ::
  --
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
      text-document--did-save
      text-document--did-close
      exit
      unknown
    ==
  +$  text-document--did-change
    [%text-document--did-change versioned-doc-id changes=(list change)]
  +$  text-document--did-open
    [%text-document--did-open text-document-item]
  +$  text-document--did-save
    [%text-document--did-save versioned-doc-id]
  +$  text-document--did-close
    [%text-document--did-close versioned-doc-id]
  +$  exit
    [%exit ~]
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
