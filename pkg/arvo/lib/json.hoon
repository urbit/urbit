^?
|%
++  enjs
  =,  enjs:format
  |%
  ++  frond
    |=  [p=@t q=json]
    ^-  json
    (frond:enjs:format (dekebab p) q)
  ++  pairs
    |=  a=(list [p=@t q=json])
    ^-  json
    %-  pairs:enjs:format
    %+  turn  a
    |=  [p=@t q=json]
    ^-  [@t json]
    [(dekebab p) q]
  --
::
++  dejs
  =,  dejs:format
  |%
  ++  of
    |*  wer/(pole {cord fist})
    |=  jon/json
    ?>  ?=({$o {@ *} $~ $~} jon)
    |-
    ?-    wer                                         
        :: {{key/@t wit/*} t/*}
        {{key/@t *} t/*}
      =>  .(wer [[* wit] *]=wer)
      ?:  =(key.wer (enkebab p.n.p.jon))
        [key.wer ~|(val+q.n.p.jon (wit.wer q.n.p.jon))]
      ?~  t.wer  ~|(bad-key+p.n.p.jon !!)
      ((of t.wer) jon)
    ==
  ++  ot
    |*  wer=(pole [cord fist])
    |=  jon=json
    ~|  jon
    %-  (ot-raw:dejs:format wer)
    ?>  ?=(%o -.jon)
    (ruk-jon p.jon enkebab)
  ::
  ++  ruk-jon
    |=  [a=(map @t json) b=$-(@t @t)]
    ^+  a
    =-  (malt -)
    |- 
    ^-  (list [@t json])
    ?~  a  ~
    :-  [(b p.n.a) q.n.a]
    %+  weld
      $(a l.a)
    $(a r.a)
  --
::
++  dekebab
  |=  str=cord
  ^-  cord
  =-  (fall - str)
  %+  rush  str
  =/  name
    %+  cook
     |=  part=tape
     ^-  tape
     ?~  part  part
     :-  (sub i.part 32)
     t.part
    (star low)
  %+  cook
    (cork (bake zing (list tape)) crip)
  ;~(plug (star low) (more hep name))
::
++  enkebab
  |=  str=cord
  ^-  cord
  ~|  str
  =-  (fall - str)
  %+  rush  str
  =/  name
    %+  cook
      |=  part=tape
      ^-  tape
      ?~  part  part
      :-  (add i.part 32)
      t.part
    ;~(plug hig (star low))
  %+  cook
    |=(a=(list tape) (crip (zing (join "-" a))))
  ;~(plug (star low) (star name))
--


