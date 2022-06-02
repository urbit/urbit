=>
|%
++  list
  ^-  $
  $%  [%nil $~]
      [%con @ list]
  ==
++  even-list
  ^-  $
  $%  [%nil $~]
      [%con @ $con @ even-list]  :: FIXME allow %con
  ==
--
`list``even-list`[%nil ~]
