!:
::  /=main=/fun/apply/hoon
::
=>  ^^/===/pony
|=  [who=seat est=time eny=@uw was=path]
|=  *
|=  [mig=@p ~]
=+  bos=(sein mig)
?>  !=(bos mig)
^-  bowl
%+  pomp  ""
%+  pomp  "            If I did not build for myself"
%+  pomp  "            for whom should I build?"
%+  pomp  ""
%+  pomp  "                  -- Bunting, _Chomei at Toyama_"
%+  pomp  ""
=<  main
|%
++  main
  ^-  bowl
  %+  (polo %text "ticket: ~" ~)
    fed:ag
  |=  tic=@p
  %+  (polo %pass "entropy: " ~)
    (boss 256 (more gon qit))
  |=  tey=@
  =.  tey  (shax tey)
  %+  pomp  "entropy check: {<`@p`(mug tey)>}"
  (moor tic tey)
::
++  moor
  |=  [tic=@p tey=@]
  ^-  bowl
  =+  ran=(clan mig)
  ?.  |(=(%duke ran) =(%jack ran))
    %+  (polo %text "name: " ~)
      (boss 256 (more gon qit))
    |=  nam=@
    (moss tic tey (gcos [ran nam]))
  %+  (polo %text "first name: " ~)
    (boss 256 (more gon qit))
  |=  fin=@
  %+  (polo %text "last name: " ~)
    (boss 256 (more gon qit))
  |=  sur=@
  %+  (polo %text "birth year: " ~)
    dim:ag
  |=  yar=@
  (moss tic tey (gcos [ran %lord [yar %us-ca [fin ~ ~ sur]]]))
::
++  moss
  |=  [tic=@p tey=@ gec=gcos]
  ^-  bowl
  =+  bur=(shax :(mix (jam gec) tey))
  ~&  "generating 2048-bit RSA key..."
  =+  loy=(brew 2.048 bur)
  =+  msg=[mig tic gec pub:ex:loy]
  ^-  bowl
  :-  ~  :-  ~
  :-  ^-  (list slip)
      :~  [/request [%yo bos %ta msg]]
          [/response [%oy %to]]
      ==
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  ?+    -.nut  !!
      %yo  
    ?.  =(%good q.nut)
      [[[%la %leaf "request failed"] ~] ~]
    :-  [[%la %leaf "request succeeded"] ~]
    :-  ~
    :_  ..$
    ^-  (list slip)
    [[~ [%oy %to]] ~]
  ::
      %oy
    :_  ~
    ^-  (list gift)
    ?~  s.nut
      :~  [%la %leaf "request rejected"]
      ==
    =+  wul=((hard (unit will)) u.s.nut)
    ?~  wul
      :~  [%la %leaf "request refused"]
      ==
    =+  mac=`mace`[[0 sec:ex:loy] ~]
    =+  wil=u.wul
    :~  [%la %leaf "request approved"]
        [%xy /a `card`[%cash mig mac wil]]
    ==
  ==
--
