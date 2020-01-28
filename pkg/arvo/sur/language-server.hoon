|%
::
+$  versioned-doc-id
  [uri=@t version=(unit @)]
::
++  request
  |%
  +$  all
    $%
      text-document--hover
      unknown
    ==
  +$  text-document--hover
    [%text-document--hover id=cord position versioned-doc-id]
  +$  unknown
    [%unknown json]
  --
++  response
  |%
  +$  all
    $%
      text-document--hover
    ==
  +$  text-document--hover
    [%text-document--hover id=cord contents=(unit @t)]
  --
::
+$  diagnostic
  [=range severity=@ud message=@t]
::
+$  position
  [row=@ud col=@ud]
::
+$  text-document-item
  [uri=@t version=(unit @) text=@t]
::
++  notification
  |%
  ::
  +$  in
    $%
      text-document--did-change
      text-document--did-open
      text-document--did-save
      text-document--did-close
      exit
      unknown
    ==
  ::
  +$  out
    $%
      text-document--publish-diagnostics
    ==
  ::
  +$  all
    $%
      out
      in
    ==
  ::
  +$  text-document--did-change
    [%text-document--did-change versioned-doc-id changes=(list change)]
  ::
  +$  text-document--did-open
    [%text-document--did-open text-document-item]
  ::
  +$  text-document--did-save
    [%text-document--did-save versioned-doc-id]
  ::
  +$  text-document--did-close
    [%text-document--did-close versioned-doc-id]
  ::
  +$  exit
    [%exit ~]
  ::
  +$  unknown
    [%unknown =json]
  ::
  +$  text-document--publish-diagnostics
    [%text-document--publish-diagnostics uri=@t diagnostics=(list diagnostic)]
  ::
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
