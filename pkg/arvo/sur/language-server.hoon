|%
::
+$  versioned-doc-id
  [uri=@t version=@t]
::
::  ++  request
::    |%
::    +$  text-document--did-change
::    --
  ::  $%
  ::    text-document--did-change:request
  ::  ==
+$  response-kind
  [method=cord result=mold]
::
+$  request-message
  [id=(unit cord) all:request]
::
++  response
  |*  kind=response-kind
  [id=(unit term) kind]
::
::  ++  notification
::    |*  kind=response-kind
::    kind
+$  position
  [row=@ud col=@ud]
::
+$  text-document-item
  [uri=@t version=@t text=@t]
::
++  request
  |%
  +$  all
    $%
      text-document--did-change
      text-document--did-open
    ==
  +$  text-document--did-change
    [%text-document--did-change versioned-doc-id changes=(list change)]
  +$  text-document--did-open
    [%text-document--did-open text-document-item]
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
