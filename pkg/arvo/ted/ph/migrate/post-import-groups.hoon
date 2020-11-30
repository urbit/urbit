/-  spider,
    contact-view,
    *resource,
    group-store
/+  *ph-io, strandio
=,  strand=strand:spider
::
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  bol=bowl:spider  bind:m  get-bowl:strandio
::
=/  join-2=contact-view-action:contact-view  [%join ~zod %group-2]
=/  add-members-1=action:group-store
  [%add-members [~zod %group-1] (sy ~def ~ten ~)]
=/  add-members-2=action:group-store
  [%add-members [~zod %group-2] (sy ~def ~ten ~)]
;<  ~  bind:m  (poke-app ~bus %contact-view %contact-view-action join-2)
;<  ~  bind:m  (poke-app ~web %contact-view %contact-view-action join-2)
;<  ~  bind:m  (poke-app ~zod %group-store %group-action add-members-1)
;<  ~  bind:m  (poke-app ~zod %group-store %group-action add-members-2)
::
(pure:m *vase)
