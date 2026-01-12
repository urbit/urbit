/-  spider
/+  strandio
=,  strand=strand:spider
=>
|%
++  print-quacs
  |=  quz=(list quac:dill)
  ^-  (list tank)
  (zing (turn quz print-quac))
::
++  print-quac
  |=  qua=quac:dill
  ^-  (list tank)
  =|  den=@ud
  |-  ^-  (list tank)
  ?:  =(~ quacs.qua)
    ~[(rap 3 (fil 3 den ' ') name.qua ': ' (print-size size.qua) ~)]
  :-  (rap 3 (fil 3 den ' ') name.qua ':' ~)
  %+  weld
    ^-  (list tank)
    (zing (turn quacs.qua |=(q=quac:dill ^$(qua q, den (add den 2)))))
  ^-  (list tank)
  ~[(rap 3 (fil 3 den ' ') '--' (print-size size.qua) ~)]
::
++  print-size
  |=  size=@ud
  |^  ^-  @t
  ?:  (lte size 1.024)
    (crip "{(a-co:co size)} B")
  ?:  (lte size (pow 1.024 2))
    =+  (dvr size 1.024)
    (crip "{(a-co:co p)}.{(y-co:co (round q))} KiB")
  ?:  (lte size (pow 1.024 3))
    =+  (dvr size (pow 1.024 2))
    (crip "{(a-co:co p)}.{(y-co:co (round q))} MiB")
  =+  (dvr size (pow 1.024 3))
  (crip "{(a-co:co p)}.{(y-co:co (round q))} GiB")
  ++  round
    |=  n=@
    ?:  (lth n 100)
      n
    =+  (dvr n 10)
    ?:  (lth q 5)
      $(n p)
    $(n +(p))
  --
--
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  quz=(list quac:dill)  bind:m  mass:strandio
%-  (slog (print-quacs quz))
(pure:m !>(~))
