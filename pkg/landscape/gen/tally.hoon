/-  gr=group, md=metadata-store, ga=graph-store
/+  re=resource, graph=graph-store
!:
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        args=?(~ [shy=? ~])
        ~
    ==
::
=/  shy=?   ?~(args & shy.args)
=*  our=@p  p.beak
::
|^
=;  out=(list @t)
  :-  %tang
  %-  flop  ::NOTE  tang is bottom-up
  :*  ''
      'tallied your activity score! find the results below.'
    ::
      ?:  shy
        'to show non-anonymized resource identifiers, +tally |'
      'showing plain resource identifiers, share with care.'
    ::
      'counted from groups and channels that you are hosting.'
      'groups are listed with their member count.'
      'channels are listed with activity from the past week:'
      '  - amount of top-level content'
      '  - amount of unique authors'
      ''
      (snoc out '')
  ==
::  gather local non-dm groups, sorted by size
::
=/  groups=(list [local=? resource:re members=@ud])
  %+  murn
    %~  tap  in
    (scry (set resource:re) %y %group-store /groups)
  |=  r=resource:re
  =/  g=(unit group:gr)
    %+  scry  (unit group:gr)
    [%x %group-store [%groups (snoc (en-path:re r) %noun)]]
  ?:  |(?=(~ g) hidden.u.g)
    ~
  `[=(our entity.r) r ~(wyt in members.u.g)]
=/  crowds=(list [resource:re @ud])
  %+  sort  (turn (skim groups head) tail)
  |=  [[* a=@ud] [* b=@ud]]
  (gth a b)
::  gather local per-group channels
::
=/  channels=(map resource:re (list [module=term =resource:re]))
  %-  ~(gas by *(map resource:re (list [module=term =resource:re])))
  %+  turn  crowds
  |=  [r=resource:re *]
  :-  r
  %+  murn
    %~  tap   by
    %+  scry  associations:md
    [%x %metadata-store [%group (snoc (en-path:re r) %noun)]]
  |=  [m=md-resource:md association:md]
  ::NOTE  we only count graphs for now
  ?.  &(=(%graph app-name.m) =(our creator.metadatum))  ~
  ?.  ?=(%graph -.config.metadatum)  ~
  `[module.config.metadatum resource.m]
::  for sanity checks
::
=/  real=(set resource:re)
  =/  upd=update:ga
    %+  scry  update:ga
    [%x %graph-store /keys/graph-update-3]
  ?>  ?=(%keys -.q.upd)
  resources.q.upd
::  count activity per channel
::
=/  activity=(list [resource:re members=@ud (list [resource:re mod=term week=@ud authors=@ud])])
  %+  turn  crowds
  |=  [g=resource:re m=@ud]
  :+  g  m
  %+  murn  (~(got by channels) g)
  |=  [m=term r=resource:re]
  ?.  (~(has in real) r)  ~
  %-  some
  :+  r  m
  ::NOTE  graph-store doesn't use the full resource-style path here!
  =/  upd=update:ga
    %+  scry  update:ga
    [%x %graph-store /graph/(scot %p entity.r)/[name.r]/noun]
  ?>  ?=(%add-graph -.q.upd)
  =*  mo  orm:graph
  =/  week=(list [@da node:ga])
    (tap:mo (lot:mo graph.q.upd ~ `(sub now ~d7)))
  :-  (lent week)
  %~  wyt  in
  %+  roll  week
  |=  [[* mp=maybe-post:ga *] a=(set ship)]
  ?-  -.mp
    %|  a
    %&  (~(put in a) author.p.mp)
  ==
::  render results
::
:-  (tac 'the date is ' (scot %da now))
:-  :(tac 'you are in ' (render-number (lent groups)) ' group(s):')
:-  =-  (roll - tac)
    %+  join  ', '
    %+  turn  groups
    |=([* r=resource:re *] (render-resource r))
:-  :(tac 'you are hosting ' (render-number (lent crowds)) ' group(s):')
%-  zing
%+  turn  activity
|=  [g=resource:re m=@ud chans=(list [resource:re term @ud @ud])]
^-  (list @t)
:-  :(tac 'group, ' (render-resource g) ', ' (render-number m))
%+  turn  chans
|=  [c=resource:re m=term w=@ud a=@ud]
;:  tac  ' chan, '
  (render-resource c)  ', '
  m                    ', '
  (render-number w)    ', '
  (render-number a)
==
::
++  scry
  |*  [=mold care=term app=term =path]
  .^(mold (tac %g care) (scot %p our) app (scot %da now) path)
::
++  tac  (cury cat 3)
::
++  render-resource
  |=  r=resource:re
  ?:  shy
    (crip ((x-co:co 8) (mug r)))
  :(tac (scot %p entity.r) '/' name.r)
::
++  render-number
  |=  n=@ud
  (crip ((d-co:co 1) n))
--
