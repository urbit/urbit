::  /mar/collections/config/hoon
::
|_  $=  con
    $:  desc/cord                                       ::  description
        publ/?                                          ::  public or private
        visi/?                                          ::  visioverable
        mems/(set ship)                                 ::  ships on list
    ==                                                  ::
::
++  grow
  |%
  ++  mime
    :-  /text/x-collections-config
    %-  as-octs:mimes:html
    (of-wain:format txt)
  ++  txt
    ^-  (list @t)
    :^    desc.con
        ?:(publ.con 'public' 'private')
      ?:(visi.con 'visible' 'hidden')
    %+  turn  ~(tap in mems.con)
    (cury scot %p)
  --
::
++  grab
  |%
  ++  mime
    |=  {p/mite:eyre q/octs:eyre}
    (txt (to-wain:format q.q))
  ++  txt
    |=  txs/(list @t)
    ?>  (gte (lent txs) 3)
    :^    (snag 0 txs)
        =((snag 1 txs) 'public')
      =((snag 2 txs) 'visible')
    %-  ~(gas in *(set ship))
    %+  murn  (slag 3 txs)
    (cury slaw %p)
  --
++  grad  %txt
--
