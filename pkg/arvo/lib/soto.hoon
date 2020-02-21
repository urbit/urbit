/+  *sole
::
::TODO  revert after #1946
::
|%
::
++  json-to-action
  |=  jon=json  ^-  sole-action
  %-  need  %.  jon
  =>  [dejs-soft:format ..sole-action]
  |^  (ot id+so dat+(fo %ret (of det+change tab+ni ~)) ~)
  ++  fo
    |*  [a=term b=fist]
    |=(c=json ?.(=([%s a] c) (b c) (some [a ~])))
  ::
  ++  ra
    |*  [a=[term fist] b=fist]
    |=  c=json  %.  c
    ?.(=(%a -.c) b (pe -.a (ar +.a)))
  ::
  ++  ke                                              ::  callbacks
    |*  [gar=* sef=(trap fist)]
    |=  jon=json  ^-  (unit _gar)
    =-  ~!  gar  ~!  (need -)  -
    ((sef) jon)
  ::
  ++  change  (ot ler+(at ni ni ~) ted+(pe 0v0 edit) ~)
  ++  char  (cu taft so)
  ++  edit
    %+  ke  *sole-edit  |.  ~+
    %+  fo  %nop
    %+  ra  mor+edit
    (of del+ni set+(cu tuba sa) ins+(ot at+ni cha+char ~) ~)
  --
::
++  effect-to-json
  |=  sef=sole-effect
  |^  ^-  json
      =,  enjs:format
      ?+    -.sef
              ~|(unsupported-effect+-.sef !!)
          $mor  [%a (turn p.sef |=(a/sole-effect ^$(sef a)))]
          $err  (frond %hop (numb p.sef))
          $txt  (frond %txt (tape p.sef))
          $tan  (frond %tan (tape (wush 160 p.sef)))
          $det  (frond %det json:~(grow mar-sole-change +.sef))
      ::
          $pro
        %+  frond  %pro
        (pairs vis+b+vis.sef tag+s+tag.sef cad+(tape (purge cad.sef)) ~)
      ::
          $tab
        :-  %a
        %+  turn  p.sef
        |=  [=cord =^tank]
        %+  frond  %tab
        %-  pairs
        :~  match+s+cord
            info+(tape ~(ram re tank))
        ==
      ::
          ?($bel $clr $nex)
        (frond %act %s -.sef)
      ==
  ++  mar-sole-change
    |_  cha=sole-change
    ++  grow
      |%  ++  json
        ^-  ^json
        =,  enjs:format
        =;  edi
          =,(cha (pairs ted+(edi ted) ler+a+~[(numb own.ler) (numb his.ler)] ~))
        |=  det=sole-edit
        ?-  -.det
          $nop  [%s 'nop']
          $mor  [%a (turn p.det ..$)]
          $del  (frond %del (numb p.det))
          $set  (frond %set (tape (tufa p.det)))
          $ins  (frond %ins (pairs at+(numb p.det) cha+s+(tuft q.det) ~))
        ==
      --
    --
  ++  wush
    |=  [wid=@u tan=tang]
    ^-  tape
    %-  of-wall:format
    %+  turn  (flop tan)
    |=  =tank
    ~!  wid
    ~!  tank
    (of-wall:format (wash 0^wid tank))
  ::
  ++  purge
    |=  a=styx  ^-  tape
    %-  zing  %+  turn  a
    |=  a=_?>(?=(^ a) i.a)
    ?@(a (trip a) ^$(a q.a))
  --
--
