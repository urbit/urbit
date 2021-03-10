/+  agentio
|_  [=bowl:gall root=mark version=@ud min=@ud]
+*  io  ~(. agentio bowl)
++  is-root
  |=  =mark
  =/  tape-mark
    (trip mark)
  =/  tape-version
    (trip root)
  =((scag (lent tape-version) tape-mark) tape-version)
::
++  parse
  |=  =mark
  %+  slav  %ud
  %-  crip
  (rash (swp 3 mark) (star nud))
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
    (parse mark)
  &((gte min ver) (lte version ver))
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

