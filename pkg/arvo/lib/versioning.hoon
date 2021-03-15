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
  (append-version version)
::
++  supported
  |=  =mark
  =/  ver
    (read-version mark)
  &((gte ver min) (lte ver version))
::
++  convert-to
  |=  =cage
  ^-  vase
  ?:  =(p.cage current-version)
    q.cage
  ((tube-to p.cage) q.cage)
::
++  tube-to
  |=  =mark
  .^(tube:clay %cc (scry:io %home /[mark]/[current-version]))
::
++  tube-from
  |=  =mark
  .^(tube:clay %cc (scry:io %home /[current-version]/[mark]))
::
++  convert-from
  |=  =cage
  ^-  vase
  ?:  =(p.cage current-version)
    q.cage
  ((tube-from p.cage) q.cage)
--

