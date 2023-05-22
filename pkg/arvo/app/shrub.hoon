/-  shrub, farm
/+  default-agent, verb, dbug
|%
++  fo   fo:farm
:: stolen from clay
++  with-face  |=([face=@tas =vase] vase(p [%face face p.vase]))
++  with-faces
  =|  res=(unit vase)
  |=  vaz=(list [face=@tas =vase])
  ^-  vase
  ?~  vaz  (need res)
  =/  faz  (with-face i.vaz)
  =.  res  `?~(res faz (slop faz u.res))
  $(vaz t.vaz)
::
+$  card  card:agent:gall
+$  state-0  
  $:  =farm
      ~
  ==
--
^-  agent:gall
=|  state-0
=*  state  -
%+  verb  |
%-  agent:dbug
=<
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
    main  ~(. +> [bowl ~])
++  on-init  `this
++  on-save  !>(state)
++  on-load
  |=  =vase
  =+  !<(old=state-0 vase)
  `this(state old)
++  on-poke
  |=  [=mark =vase]
  =^  cards  state
    abet:(poke:main mark vase)
  [cards this]
++  on-watch
  |=  =path
  =^  cards  state
    abet:(watch:main path)
  [cards this]
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  [~ ~]
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo  on-arvo:def
++  on-fail  on-fail:def
--
|_  [=bowl:gall cards=(list card)]
++  abet  [(flop cards) state]
++  main  .
++  emit  |=(=card main(cards [card cards]))
++  watch
  |=  =path
  ^+  main
  !!
++  poke
  |=  [=mark =vase]
  ^+  main
  ?+  mark  ~|(bad-mark/mark !!)
    %shrub-bush  (on-bush !<(bush:shrub vase))
    %noun        (on-noun q.vase)
  ==
++  on-noun
  |=  non=*
  ^+  main
  =+  ;;([%dbug =pith] non)
  ?:  =(~ pith)
    ::  TODO: revive~&  ~(key by plots)
    main
  =/  goal=^farm  (~(got fo farm) pith)
  ?:  ?=(%sing -.goal)
    =.  vax.plot.goal  (serve-plot pith plot.goal)
    ~&  >>>  dbug/(crip (noah (slot 6 vax.plot.goal)))
    main
  =.  vax.plat.goal  (serve-plat pith plat.goal)
  ~&  >>>  kids/~(key by kids.goal)
  ~&  >>>  dbug/(crip (noah (slot 6 vax.plat.goal)))
  main
++  on-bush
  |=  =bush:shrub
  ^+  main
  ?+  -.bush  ~|(bad-bush/-.bush !!)
    %grow  (on-grow +.bush)
    %poke  (on-shrub-poke +.bush)
  ==
++  on-grow
  |=  [=twig:shrub =soil:shrub]
  ^+  main
  ~&  grow/[twig soil]
  =+  .^(=vase %ca twig)
  =/  arms
    (sy (sloe -:vase))
  =/  sub=^farm
    ?:  (~(has in arms) %mult)
      [%mult [vase 1 twig ~] ~]
    [%sing [vase 1 twig] ~]
  =.  farm  (~(put fo farm) soil sub)
  main
++  serve-plat
  |=  [=pith =plat:shrub]
  ^-  vase
  =/  =bowl:shrub
    :*  [our.bowl pith]
        now.bowl
        src.plat
    ==
  %+  slap  (slop !>(..zuse) (with-faces vax/vax.plat data/!>(data.plat) bowl/!>(bowl) ~))
  !,  *hoon
  =/  typ  -:!>(*rock:vax)
  ~(. vax [bowl (~(run by data) |=(p=* !<(rock:vax [typ p])))])
++  serve-plot
  |=  [=pith =plot:shrub]
  ~&  bowl/bowl
  ^-  vase
  =/  =bowl:shrub
    :*  [our.bowl pith]
        now.bowl
        src.plot
    ==
  ~&  shrub-bowl/bowl
  %+  slap  (slop !>(..zuse) (with-faces vax/vax.plot bowl/!>(bowl) ~))
  !,(*hoon =-(~&(+<:- -) vax(+<- bowl)))
++  on-shrub-poke
  |=  [=twig:shrub =area:shrub data=*]
  ?>  =(our.bowl p.area)
  :: TODO: actually use schema
  =/  goal=^farm  (~(got fo farm) q.area)
  ~&  found/[q.area -.goal]
  ?:  ?=(%sing -.goal)
    =/  =plot:shrub  plot.goal
    =.  vax.plot  (serve-plot q.area plot)
    =/  vax=vase
      (slym (slap vax.plot limb/%poke) data)
    =.  vax.plot  (slot 3 vax)
    =+  !<(bushes=(list bush:shrub) (slot 2 vax))
    =.  farm  (~(put fo farm) q.area goal(plot plot))
    (plant bushes)
  =/  =plat:shrub  plat.goal
  =.  vax.plat  (serve-plat q.area plat)
  %-  (slog leaf/"before" (cain (slot 6 vax.plat)) ~)
  %-  (slog leaf/"before" >(sloe -:vax.plat)< ~)
  ~&  data/data
  =/  res   (slym (slap vax.plat limb/%poke) data)
  %-  (slog leaf/"after" (cain res) ~)
  %-  (slog leaf/"after" (cain res) ~)
  (deal q.area !<((list card:shrub) res))
  ::  TODO: deal cards
++  on-give
  |=  [=soil:shrub =gift:shrub]
  ?-    -.q.gift
      %add
    (on-grow twig.q.gift (snoc soil p.gift))
      %edit
    !!
      %del
    !! :: TODO: implement alongside +on-trim
  ==
++  deal
  |=  [=soil:shrub cards=(list card:shrub)]
  ?~  cards
    main
  ?:  ?=(%tend -.i.cards)
    =.  main  (on-bush p.i.cards)
    $(cards t.cards)
  =.  main  (on-give soil p.i.cards)
  $(cards t.cards)
++  plant
  |=  bushes=(list bush:shrub)
  ?~  bushes
    main
  =.  main  (on-bush i.bushes)
  $(bushes t.bushes)
--
