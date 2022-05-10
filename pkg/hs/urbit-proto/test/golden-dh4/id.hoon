=/  id
  |=  {t=$ x=t}
  x
:*  (id @ 1)
    (id * 2)
    (id {@ @} [3 4])
    (id ^ [5 6])
    (id ? &)
    (id $-({t=$ t} t) id)
    ((id $-({t=$ t} t) id) $-({t=$ t} t) id)
    (id $-({t=$ t} t) (id $-({t=$ t} t) id))
==
