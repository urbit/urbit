::
::::  /hoon/effect/sole/mar
  ::
/?    310
/-    sole
/+    base64
!:
::
::::
  ::
=,  sole
=,  format
|%
++  mar-sole-change                       ::  XX  dependency
  |_  cha/sole-change
  ++  grow
    |%  ++  json
      ^-  ^json
      =,  enjs
      =;  edi
        =,(cha (pairs ted+(edi ted) ler+a+~[(numb own.ler) (numb his.ler)] ~))
      |=  det/sole-edit
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
|_  sef/sole-effect
::
++  grab                                                ::  convert from
  |%
  ++  noun  sole-effect                                 ::  clam from %noun
  --
++  grow
  =,  enjs
  |%
  ++  lens-json                       :: json for cli client
    ^-  ?(~ ^json)                   :: null = ignore
    ?+    -.sef  ~
        $tan  (wall (turn (flop p.sef) |=(a/tank ~(ram re a))))
        $txt  s+(crip p.sef)
        $sag
      =/  =atom  (jam q.sef)
      =/  =octs  [(met 3 atom) atom]
      =/  enc  (en:base64 octs)
      (pairs file+s+(crip <`path`p.sef>) data+s+enc ~)
        $sav
      (pairs file+s+(crip <`path`p.sef>) data+s+(crip (en-base64:mimes:html q.sef)) ~)
    ::
        $mor
      =+  all=(turn p.sef |=(a/sole-effect lens-json(sef a)))
      =.  all  (skip all |=(a/^json ?=(~ a)))
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
        $err  (frond %hop (numb p.sef))
        $txt  (frond %txt (tape p.sef))
        $tan  (frond %tan (tape (wush 160 p.sef)))
        $det  (frond %det json:~(grow mar-sole-change +.sef))
    ::
        $pro
      %+  frond  %pro
      (pairs vis+b+vis.sef tag+s+tag.sef cad+(tape (purge cad.sef)) ~)
    ::
        ?($bel $clr $nex)
      (frond %act %s -.sef)
    ==
  --
--
