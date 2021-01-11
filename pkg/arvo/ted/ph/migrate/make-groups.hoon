/-  spider,
    contact-view,
    *resource
/+  *ph-io, strandio
=,  strand=strand:spider
::
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  bol=bowl:spider  bind:m  get-bowl:strandio
::
::  group setup
::  - ~zod creates an open group
::  - ~zod creates and invite-only group, and invites ~bus and ~web
::  - ~bus and ~web join the first, but not the second group, to keep
::    invite-store populated
::
=/  group-1=contact-view-action:contact-view
  :*  %create
      %group-1
      [%open ~ ~]
      'Group 1'
      'this is group 1'
  ==
=/  group-2=contact-view-action:contact-view
  :*  %create
      %group-2
      [%invite (sy ~bus ~web ~)]
      'Group 2'
      'this is group 2'
  ==
=/  join=contact-view-action:contact-view  [%join ~zod %group-1]
;<  ~  bind:m  (poke-app ~zod %contact-view %contact-view-action group-1)
;<  ~  bind:m  (wait-for-output ~zod ">=")
;<  ~  bind:m  (poke-app ~zod %contact-view %contact-view-action group-2)
;<  ~  bind:m  (wait-for-output ~zod ">=")
;<  ~  bind:m  (sleep ~s10)
;<  ~  bind:m  (poke-app ~bus %contact-view %contact-view-action join)
;<  ~  bind:m  (wait-for-output ~bus ">=")
;<  ~  bind:m  (poke-app ~web %contact-view %contact-view-action join)
;<  ~  bind:m  (wait-for-output ~web ">=")
;<  ~  bind:m  (send-hi ~bus ~zod)
;<  ~  bind:m  (send-hi ~web ~zod)
;<  ~  bind:m  (sleep ~s2)
(pure:m *vase)
