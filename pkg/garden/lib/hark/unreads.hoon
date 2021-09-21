/+  store=hark-store
|_  =archive:store
++  orm  ((on @da timebox:store) gth)
++  del
  |=  [=time =bin:store]
  ?~  box=(get:orm archive time)  archive
  (put:orm archive time (~(del by u.box) bin))
++  put
  |=  [=time =bin:store =notification:store]
  =/  box=timebox:store  (fall (get:orm archive time) ~)
  =.  box  (~(put by box) bin notification)
  (put:orm archive time box)
::
++  get
  |=  [=time =bin:store]
  ^-  (unit notification:store)
  ?~  box=(get:orm archive time)  ~
  (~(get by u.box) bin)
::
++  got
  |=  [=time =bin:store]
  (need (get time bin))
::
++  has
  |=  [=time =bin:store]
  ?~((get time bin) %.n %.y)
::
++  jab
  |=  [=time =bin:store f=$-(notification:store notification:store)]
  (put time bin (f (got time bin)))
::
++  job
  |=  [=time =bin:store f=$-((unit notification:store) notification:store)]
  (put time bin (f (get time bin)))
--
