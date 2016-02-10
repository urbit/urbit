::
::::  /hoon+hello+gen
  ::
/?  314
::
::::
  !:
:-  %say
|=  *
:-  %noun
=-  %+  turn  -
    |=  {a/@tas b/@tas}
    ?:  |
      ^-  @ta
      %-  crip
      ;:  weld
        "s/\\$"
        (trip a)
        "/"
        "?($"
        (trip a)
        " $"
        (trip b) 
        ")"
        "/g"
      ==
    ?:  |
      ^-  @ta
      %-  crip
      ;:  weld
        "s/%"
        (trip a)
        "/"
        "%"
        (trip b)
        "/g"
      ==
    ?:  &
      ^-  @ta
      %-  crip
      ;:  weld
        "s/\\?(\\$"
        (trip a)
        " \\$"
        (trip b) 
        ")/$"
        (trip b)
        "/g"
      ==
    !!
^-  (list (pair @tas @tas))
:~  [%flap %claw]  ::  used in ames
    [%slug %shoe] 
    [%rack %bank]
    [%gate %lamb]
    [%lock %gill]
    ::  [%lamp %gate]     reused
    [%bud %scon]
    [%qua %conq]
    [%dub %cons]
    [%tri %cont]  :: collides with %trip
    [%ray %conl]
    [%all %conp]

    [%cold %bunt]   :: parser jet
    [%quid %calq]
    [%quip %calt]
    [%with %open]
    ::  [%kick %nock]   reused; used in ames
    [%live %poll]  :: also a hint
    [%show %dump]  :: used in %ames

    ::  [%fate %show]   reused

    [%germ %ddup]  :: also a hint
    [%type %peep]
    [%fly %fix]
    [%ram %rev]   :: also %ramp
    [%eat %sip]     
    [%has %pin]
    [%saw %nip]
    [%dig %ifcl]    :: %digitalocean in ape/cloud
    [%nay %deny]
    [%aye %sure]
    [%deal %deft]   :: used in all vanes
    [%dab %ifat]
    [%non %ifno]    :: also %none
    [%fit %fits]    :: also %fitz
    [%nock %code]   :: reused 
==
