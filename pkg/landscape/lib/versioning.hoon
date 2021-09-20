/+  agentio
|_  [=bowl:gall root=mark version=@ud min=@ud]
+*  io  ~(. agentio bowl)
++  is-root
  |=  =mark
  ?~  (rush mark mark-parser)
    %.n
  %.y
::
++  mark-parser
  ;~(pfix (jest root) ;~(pose ;~(pfix hep dum:ag) (easy `@ud`0)))
::
++  read-version
  |=  =mark
  (rash mark mark-parser)
::
++  append-version
  |=  ver=@ud
  :((cury cat 3) root '-' (scot %ud ver))
::
++  current-version
  ^-  mark
  (append-version version)
::
++  supported
  |=  =mark
  =/  ver
    (read-version mark)
  &((gte ver min) (lte ver version))
::
++  convert-to
  |=  [=mark =vase]
  ^-  cage
  :-  current-version
  ?:  =(mark current-version)
    vase
  ((tube-to mark) vase)
::
++  tube-to
  |=  =mark
  .^(tube:clay %cc (scry:io q.byk.bowl /[mark]/[current-version]))
::
++  tube-from
  |=  =mark
  .^(tube:clay %cc (scry:io q.byk.bowl /[current-version]/[mark]))
::
++  convert-from
  |=  [=mark =vase]
  ^-  cage
  :-  mark
  ?:  =(mark current-version)
    vase
  ((tube-from mark) vase)
--

