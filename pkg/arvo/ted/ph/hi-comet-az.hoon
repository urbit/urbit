/-  spider
/+  *ph-io, vere
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=/  comet=ship  ~londeg-tirlys-somlyd-poltus--pintyn-tarbyl-bicnux-marbud
=/  =feed:jael
  :*  [%2 ~]
      who=comet
      ryf=0
      :_  ~
      :-  lyf=1
      key=0wfm.lBEWM.08gfy.AxYjy.8-tBQ.uq-aa.LZt9c.CVQqd.XBJIs.
          CoG90.BNNGV.1ZmVi.ZbAhY.LuhwC.idNnU.lCVkt.Z4qug.7iY92
  ==
::
?>  ?=(^ (veri:dawn:vere comet feed *point:azimuth-types ~))
::
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~bud)
;<  ~  bind:m  (init-ship ~bud |)
;<  ~  bind:m  (spawn ~marbud)
;<  ~  bind:m  (init-ship ~marbud |)
;<  ~  bind:m  (init-comet comet feed)
:: ;<  ~  bind:m  (send-hi comet ~bud) ::  XX this crashes, sending the |hi before the attestation
                                       ::  and we endup blocking the queue
;<  ~  bind:m  (send-hi ~bud comet)
;<  ~  bind:m  (spawn ~linnup-torsyx)
;<  ~  bind:m  (init-ship ~linnup-torsyx |)
::
:: ;<  ~  bind:m  (send-hi comet ~linnup-torsyx)  :: XX same as above
;<  ~  bind:m  (send-hi ~linnup-torsyx comet)
:: ::
;<  ~  bind:m  end
(pure:m *vase)
