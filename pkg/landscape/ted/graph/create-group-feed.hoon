/-  spider,
    graph=graph-store,
    met=metadata-store,
    push-hook
/+  strandio, resource, graph-view
::
=*  strand     strand:spider
=*  poke       poke:strandio
=*  poke-our   poke-our:strandio
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =action:graph-view] arg)
?>  ?=(%create-group-feed -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
?.  =(our.bowl entity.group.action)
  (strand-fail:strandio %bad-request ~)
=/  feed-rid=resource
  :-  entity.group.action
  %-  crip
  %+  weld  (trip name.group.action)
  %+  weld  "-"
  (trip (scot %ud (mod eny.bowl 10.000)))
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
        ?=([~ ~] feed.config.metadatum)
    ==
;<  ~  bind:m
  %+  poke-our  %graph-store
  :-  %graph-update-3
  !>  ^-  update:graph
  [now.bowl %add-graph feed-rid *graph:graph `%graph-validator-post %&]
;<  ~  bind:m
  (poke-our %graph-push-hook %push-hook-action !>([%add feed-rid]))
;<  ~  bind:m
  %+  poke-our  %metadata-push-hook
  :-  %metadata-update-2
  !>  ^-  action:met
  :^  %add
      group.action
    groups+group.action
  metadatum(feed.config ``[%graph feed-rid])
;<  ~  bind:m
  %+  poke-our  %metadata-push-hook
  :-  %metadata-update-2
  !>  ^-  action:met
  :^  %add
      group.action
    graph+feed-rid
  %*  .  *metadatum:met
    title         'Group Feed'
    date-created  now.bowl
    creator       our.bowl
    config        [%graph %post]
    preview       %.n
    hidden        %.y
    vip           vip.action
  ==
(pure:m !>(feed-rid))
