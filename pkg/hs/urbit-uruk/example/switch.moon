=/  four
  ?-  (lef 3)
    x  (inc x)
    y  (fec y)
  ==

=/  id-and-unpack
  |=  v
  ?-  v
    x  [v x]
    x  [v x]
  ==

(id-and-unpack (lef four))
