=/  id
  ^-  $-  arg|{t|$ x|t}
      t.arg
  |=  a
  x.arg.a
:*  (id arg=[t=@ x=1])
    ::  (id * 2)
    (id arg=[t={@ @} x=[3 4]])
    ::  (id ^ [5 6])
    (id arg=[t=? x=%&])
    ::  (id $-({t|$ t} t) id)
==
