/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=/  comet=ship  ~londeg-tirlys-somlyd-poltus--pintyn-tarbyl-bicnux-marbud
::
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~bud)
;<  ~  bind:m  (init-ship ~bud |)
;<  ~  bind:m  (spawn ~marbud)
;<  ~  bind:m  (init-ship ~marbud |)
;<  ~  bind:m  (init-comet comet)
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
