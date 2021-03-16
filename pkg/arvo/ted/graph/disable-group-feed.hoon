/-  spider,
    met=metadata-store
/+  strandio, resource, graph-view
::
=*  strand     strand:spider
=*  poke-our   poke-our:strandio
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =action:graph-view] arg)
?>  ?=(%disable-group-feed -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
?.  =(our.bowl entity.group.action)
  (strand-fail:strandio %bad-request ~)
;<  association=(unit association:met)  bind:m
  %+  scry:strandio  (unit association:met)
  %-  zing
  :~  /gx/metadata-store/metadata/groups
      (en-path:resource group.action)
      /noun
  ==
?~  association
  ~|('No group exists, cannot make group feed.' !!)
=*  metadatum  metadatum.u.association
?>  ?=(%group -.config.metadatum)
?>  ?|  ?=(~ feed.config.metadatum)
        ?=([~ ^] feed.config.metadatum)
    ==
;<  ~  bind:m
  %+  poke-our  %metadata-push-hook
  :-  %metadata-update
  !>  ^-  action:met
  :^  %add
      group.action
    groups+group.action
  metadatum(feed.config [~ ~])
(pure:m !>(~))
