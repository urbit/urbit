::
::::  /hoon/frontmatter/lib
  ::
/?    310
::
=,  format
|%
++  atr-lines
  |=  atr/(map cord cord)
  %+  turn  (sort ~(tap by atr) |=({{a/@ @} {b/@ @}} (aor a b)))
  |=  {k/cord v/cord}
  (rap 3 k ': ' v ~)
::
++  atr-key  ;~(sfix (star ;~(less col prn)) col ace)
++  print
  |=  {atr/(map cord cord) src/wain}
  ?~  atr  src
  ['---' (welp (atr-lines atr) '---' src)]
++  parse
  =|  atr/(map cord cord)
  |=  wan/wain  ^+  [atr mud='']
  ?~  wan  [~ '']
  ?^  (rush i.wan (star ace))
    $(wan t.wan)
  ?.  =('---' i.wan)  [~ (of-wain wan)]
  |-  ^+  [atr mud='']
  ?~  t.wan  ~|(%unclosed-metadata !!)
  ?:  =('---' i.t.wan)  [atr (of-wain t.t.wan)]
  ?^  (rush i.t.wan (star ace))
    $(wan t.wan)
  =-  $(wan t.wan, atr (~(put by atr) (crip key) (crip val)))
  ~|  malformed-attribute+i.t.wan
  ^-  {key/tape ^ val/tape}
  +>:(atr-key 1^1 (trip i.t.wan))
--
