!:
::  /=main=/bin/ticket/hoon
::
=>  .(-< `who=@p`-<)
|=  [est=time *]
|=  arg=*
^-  bowl
:_  ~
=+  ^=  fir  ^-  [p=@p q=@ud]
    ?:  ?=([@ ~] arg)
      [-.arg 1]
    ?:  ?=([@ @ ~] arg)
      [-.arg +<.arg]
    ~|(%ticket-arg !!)
?>  &(=(who (sein p.fir)) =(who (sein (add p.fir (dec q.fir)))))
=+  ^=  sep  ^-  @
    =+  mir=(clan p.fir)
    ?+  mir  ~|(%ticket-clan !!)
      %king  (bex 8)
      %duke  (bex 16)
      %earl  (bex 32)
    ==
|-  ^-  (list gift)
?:  =(0 q.fir)  ~
=+  tic=(,@p .^(%a (scot %p who) %tick (scot %da est) (scot %p p.fir) ~))
:-  [%la %leaf "{<p.fir>}: {<tic>}"]
$(p.fir (add sep p.fir), q.fir (dec q.fir))
