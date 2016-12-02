::
::::  /hoon/effect/sole/mar
  ::
/?    310
/-    sole
::
::::
  ::
=,  sole
|%
++  mar-sole-change                       ::  XX  dependency
  |_  cha/sole-change
  ++  grow
    |%  ++  json
      ^-  ^json
      =+  cha
      =<  (jobe ted+(. ted) ler+a+~[(jone own.ler) (jone his.ler)] ~)
      |=  det/sole-edit
      ?-  -.det
        $nop  [%s 'nop']
        $mor  [%a (turn p.det ..$)]
        $del  (joba %del (jone p.det))
        $set  (joba %set (jape (tufa p.det)))
        $ins  (joba %ins (jobe at+(jone p.det) cha+s+(tuft q.det) ~))
      ==
    --  
  --
++  wush
  |=  {wid/@u tan/tang}
  ^-  tape
  (of-wall (turn (flop tan) |=(a/tank (of-wall (wash 0^wid a)))))
::
++  purge                                               ::  discard ++styx style
  |=  a/styx  ^-  tape
  %-  zing  %+  turn  a
  |=  a/_?>(?=(^ a) i.a)
  ?@(a (trip a) ^$(a q.a))
--
::
=,  js:eyre
|_  sef/sole-effect
::
++  grab                                                ::  convert from
  |%
  ++  noun  sole-effect                                 ::  clam from %noun
  --
++  grow
  |%
  ++  lens-json                       :: json for cli client
    ^-  ?($~ ^json)                   :: null = ignore
    ?+    -.sef  ~
        $tan  (jape (of-wall (turn (flop p.sef) ~(ram re a))))
        $txt  s+(crip p.sef)
        $sav
      (jobe file+s+(crip <`path`p.sef>) data+s+(crip (sifo q.sef)) ~)
    ::
        $mor
      =+  all=(turn p.sef |=(a/sole-effect lens-json(sef a)))
      =.  all  (skip all |=(a/^json ?=($~ a)))
      ?~  all  ~
      ?~  t.all  i.all
      ~|(multiple-effects+`(list ^json)`all !!)
    ==
  ::
  ++  json
    ^-  ^json
    ?+    -.sef  
              ~|(unsupported-effect+-.sef !!)
        $mor  [%a (turn p.sef |=(a/sole-effect json(sef a)))]
        $err  (joba %hop (jone p.sef))
        $txt  (joba %txt (jape p.sef))
        $tan  (joba %tan (jape (wush 160 p.sef)))
        $det  (joba %det json:~(grow mar-sole-change +.sef))
    ::
        $pro
      %+  joba  %pro
      (jobe vis+b+vis.sef tag+s+tag.sef cad+(jape (purge cad.sef)) ~)
    ::
        ?($bel $clr $nex)
      (joba %act %s -.sef)
    ==
  --
--
