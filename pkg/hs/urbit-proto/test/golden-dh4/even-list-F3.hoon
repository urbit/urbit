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
`even-list``list`[%nil ~]
