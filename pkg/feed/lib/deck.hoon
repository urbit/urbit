/-  column, deck
/+  col-lib=column
|%
++  enjs
  =,  enjs:format
  |%
  ++  columns
    |=  cs=(map id:column column)
    %-  pairs
    %+  turn  ~(tap by cs)
    |=  [i=id:column c=column]
    [(scot %uv i) (column:enjs:col-lib c)]
  ::
  ++  deck
    |=  d=^deck
    %-  pairs
    :~  ordering+a+(turn ordering.d id:enjs:col-lib)
        columns+(columns columns.d)
    ==
  ::
  ++  diff
    |=  d=diff:^deck
    %+  frond  -.d
    ?-  -.d  
      %ord  a+(turn p.d id:enjs:col-lib)
      %add-col  (ref:enjs:col-lib p.d)
      %del-col  (id:enjs:col-lib p.d)
    ==
  --
++  dejs
  =,  dejs:format
  |%
  ++  diff
    %-  of
    :~  ord+(ar id:dejs:col-lib)
        add-col+ref:dejs:col-lib
        del-col+id:dejs:col-lib
    ==
  ::
  ++  deck
    %-  ot
    :~  ordering+(ar id:dejs:col-lib)
        columns+(op ;~(pfix (jest '0v') viz:ag) column:dejs:col-lib)
    ==
  --

--

