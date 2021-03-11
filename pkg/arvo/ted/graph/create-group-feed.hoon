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
;<  metadatum=(unit metadatum:met)  bind:m
  %+  scry:strandio  (unit metadatum:met)
  %-  zing
  :~  /gx/metadata-store/metadata/group
      (en-path:resource group.action)
      /noun
  ==
?~  metadatum
  ~|('No group exists, cannot make group feed.' !!)
?>  ?=(%group -.config.u.metadatum)
?>  ?|  ?=(~ feed.config.u.metadatum)
        ?=([~ ~] feed.config.u.metadatum)
    ==
;<  ~  bind:m
  %+  poke-our  %graph-store
  :-  %graph-update
  !>  ^-  update:graph
  [%0 now.bowl %add-graph feed-rid *graph:graph `%graph-validator-post %&]
;<  ~  bind:m
  (poke-our %graph-push-hook %push-hook-action !>([%add feed-rid]))
;<  ~  bind:m
  %+  poke-our  %metadata-push-hook
  :-  %metadata-update
  !>  ^-  action:met
  :^  %add
      group.action
    group+group.action
  u.metadatum(feed.config ``[%graph feed-rid])
;<  ~  bind:m
  %+  poke-our  %metadata-push-hook
  :-  %metadata-update
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
  ==
(pure:m !>(~))
