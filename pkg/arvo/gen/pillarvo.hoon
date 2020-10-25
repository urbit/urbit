=+  [our=~sampel now=~2000.1.1]
=<  pillgen
|%
::  fake-arvo defs
+$  curd  [term *]
+$  wire  path
+$  mite  path
+$  octs  (pair @u @t)
+$  mime  (pair mite octs)
::
++  soul  .
++  pillgen
  |=  a=$@(@t [t=@da wire b=[%pill @t]])  ^-  @
  ?@  a  $(a [now /pill [%pill a]])
  543.210:(pill our t.a b.a)
::
++  unix
  |=  [@p @da a=$%([%$ %sync ~] [%into @tas ? fil=(list [path ~ mime])])]
  ?~  -.a  soul
  ~&  %+  turn  fil.a
      |=  [pax=path ~ mim=mime]
      [pax p.mim p.q.mim `@t`(end 3 60 q.q.mim) `@t`(rsh 3 (sub (max (met 3 q.q.mim) 10) 10) q.q.mim)]
  soul
::
++  pill
  |=  [who=ship now=@da fav=curd]
  ^+  soul
  =>  .(fav ;;([%pill txt=@t] fav))
  |^  =+  [ver int]=(parse 0 txt.fav)
      |-  ^+  soul
      ?^  ver  $(ver t.ver, soul (veer-hard our now i.ver))
      =.  soul  (unix who now [%$ %sync ~])
      (unix who now [%into %$ & int])
  ::
  :: ' ~' = improperly terminated, last newline isn't part of file
  ++  vane  ;~(pose (cold %$ buc) low)
  ++  filetyp  ;~(pose ;~(pfix ace cen vane) (jest ' ~'))
  ++  parse
    =|  [ver=(list [%veer lal=@tas pax=path txt=@t]) hav=(list [path ~ mime])]
    |=  [idx=@u txt=@t]  ^+  [ver hav]
    =^  hed  idx  (get-line:bootstrap.tide idx txt)
    =/  [pax=path van=(unit term)]
      ~|  hed
      (rash hed ;~(plug fel:stab (punt filetyp)))
    =^  imp  van  ?.(=([~ ' ~'] van) [| van] [& ~])
    ?:  =(/~ pax)
      ~|  [%post-terminator `@t`(cut 3 [+(idx) 100] txt)]
      ?>  =(+(idx) (met 3 txt))  :: no bytes after terminator line
      [(flop ver) hav]
    ::
    =^  bod  idx  (get-body imp idx txt)
    =/  oct=octs  [(roll (turn bod head) add) (can 3 bod)]
    %_  $
      ver  ?~(van ver :_(ver [%veer u.van (unhoon pax) q.oct]))
      hav  :_(hav [pax `[/text/plain oct]])
    ==
  ::
  ++  unhoon                                           :: strip trailng /hoon
    |=(a=path ^+(a =.(a (flop a) ?+(a !! [%hoon *] (flop t.+.a)))))
  ::
  ++  get-body
    |=  [imp=? idx=@u txt=@t]  ^-  [(list octs) @u]
    =/  sep
      %.  (next-sep:bootstrap.tide idx txt)
      (bond |.(~|([%unclosed-file at=idx `@t`(cut 3 [+(idx) 100] txt)] !!)))
    =/  len  (sub sep +(idx))
    ?:  =('\0A/\0A' (cut 3 [sep 3] txt))
      =+  [ocs fin]=$(idx (add sep 2))
      [[[len (cut 3 [+(idx) len] txt)] ocs] fin]
    =?  len  imp  (dec len)
    [[len (cut 3 [+(idx) len] txt)]~ sep]
  --
::
::
++  veer
  |=  [who=ship now=@da fav=curd]
  ^+  soul
  =>  .(fav ;;([%veer lal=@tas pax=path txt=@t] fav))
  =/  res  (mule |.((veer-hard who now fav)))
  ?-(-.res %& p.res, %| (mean leaf+"veer: {<lal.fav>}" p.res))
::
++  veer-hard
  |=  [who=ship now=@da [%veer lal=@tas pax=path txt=@t]]
  :: ...
  :: ?:  =(%$ lal.fav)
  ~&  veer/[lal pax `@p`(mug txt)]
  soul
::
--
