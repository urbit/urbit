::
::::  /hoon#sole-effect#mar
  ::
/?    314
/-    sole
!:
::::
  ::
[sole .]
|%
++  mar-sole-change                       ::  XX  dependency
  |_  cha+sole-change
  ++  grow
    |%  ++  json
      ^-  ^json
      =+  cha
      =<  (jobe ted#(. ted) ler#a#~[(jone own.ler) (jone his.ler)] ~)
      |=  det+sole-edit
      ?-  -.det
        $nop  [%s 'nop']
        $mor  [%a (turn p.det ..$)]
        $del  (joba %del (jone p.det))
        $set  (joba %set (jape (tufa p.det)))
        $ins  (joba %ins (jobe at#(jone p.det) cha#s#(tuft q.det) ~))
      ==
    --  
  --
++  wush
  |=  {wid+@u tan+tang}
  ^-  tape
  =+  rolt=|=(a+wall `tape`?~(a ~ ?~(t.a i.a :(weld i.a "\0a" $(a t.a)))))
  (rolt (turn (flop tan) |=(a+tank (rolt (wash 0^wid a)))))
::
--
!:
|_  sef+sole-effect
::
++  grab                                                ::  convert from
  |%
  ++  noun  sole-effect                                 ::  clam from %noun
  --
++  grow
  |%
  ++  json
    ^-  ^json
    ?+    -.sef  
              ~|(unsupported-effect#-.sef !!)
        $mor  [%a (turn p.sef |=(a+sole-effect json(sef a)))]
        $err  (joba %hop (jone p.sef))
        $txt  (joba %txt (jape p.sef))
        $tan  (joba %tan (jape (wush 160 p.sef)))
        $det  (joba %det json:~(grow mar-sole-change +.sef))
        $pro   
      (joba %pro (jobe vis#b#vis.sef tag#s#tag.sef cad#(jape cad.sef) ~))
    ::
        ?($bel $clr $nex)  
      (joba %act %s -.sef)
    ==
  --
--
