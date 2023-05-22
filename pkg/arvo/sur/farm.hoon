/-  shrub
=<  farm
|% 
+$  farm
  $%  [%sing =plot:shrub kids=$~(~ (map term farm))]
      [%mult =plat:shrub kids=$~(~ (map iota farm))]
  ==
++  fo
  |_  =farm
  ++  get
    |=  =pith
    ^-  (unit ^farm)
    ?~  pith
      `farm
    ?:  ?=(%sing -.farm)
      ?^  i.pith
        ~
      (~(get by kids.farm) i.pith)
    (~(get by kids.farm) i.pith)
  ++  got
    |=  =pith
    (need (get pith))
  ++  got-plot
    |=  =pith
    =+  res=(got pith)
    ?>  ?=(%sing -.res)
    plot.res
  ++  put-plot
    |=  [=pith =plot:shrub]
    =+  res=(got pith)
    ?>  ?=(%sing -.res)
    (put pith res(plot plot))
  ++  put
    |=  [=pith new=^farm]
    ?~  pith  new
    ?:  =(1 (lent pith))
      ?:  ?=(%sing -.farm)
        ?>  ?=(@ i.pith)
        farm(kids (~(put by kids.farm) i.pith new))
      farm(kids (~(put by kids.farm) i.pith new))
    ?:  ?=(%sing -.farm)
      ?>  ?=(@ i.pith)
      =/  kid  $(pith t.pith, farm (~(got by kids.farm) i.pith))
      farm(kids (~(put by kids.farm) i.pith kid))
    =/  kid  $(pith t.pith, farm (~(got by kids.farm) i.pith))
    farm(kids (~(put by kids.farm) i.pith kid))
  ::
  ++  keys-helper
    |=  [curr=pith kids=(map iota ^farm)]
    ^-  (list iota)
    =/  ks=(list iota)  ~(tap in ~(key by kids))
    %+  welp  ks
    %-  zing
    %+  turn  ks
    |=  k=iota
    ^-  (list iota)
    =/  grandkids
      =/  tmp  (~(got by kids) k)
      ?:  ?=(%sing -.tmp)
        kids.tmp
      kids.tmp
    $(curr (snoc curr k), kids grandkids)
  ++  keys
    %-  ~(gas in *(set pith))
    ?:  ?=(%sing -.farm)
    ~ :: (keys-helper / kids.farm)
    ~  ::(keys-helper / kids.farm)

  --
--
