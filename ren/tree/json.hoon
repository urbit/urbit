::
::::  /hoon/json/tree/ren
  ::
/?    310
/-    tree-include
/+    tree
/=    gas    /$    fuel:html
/=    dat    /^    tree-include    /tree-include/
/=    kid    /^    (map knot tree-include)
             /_    /tree-include/
=,  format
=,  mimes:html
::
::::
  ::
|%
++  schema  (dict {term $@(mark schema)})
++  dict    |*(a/_* $^({a (dict a)} a))
++  plist   (list {term $@(mark plist)})
++  query
  $%  {$kids p/(list query)}
      {$name $t}
      {$path $t}
      {$spur $t}
  ::
      {$bump $t}
      {$beak $t}
      {$comt $j}
      {$plan $j}
      {$head $r}
      {$sect $j}
      {$snip $r}
      {$body $r}
      {$meta $j}
      {$mime $m}
  ==
++  schema-to-plist                   :: pad improper list
  |=  a/schema  ^-  plist
  ?@(-.a [(to-item a) ~] [(to-item -.a) $(a +.a)])
::
++  to-item
  |=  b/{term $@(mark schema)}  ^-  {term $@(mark plist)}
  ?@(+.b b [-.b (schema-to-plist +.b)])
::
++  from-type                         ::  XX holding out for noun odors
  |=  a/$%({$t p/cord} {$r p/json} {$j p/json} {$m mime})
  ?-  -.a
    $t  [%s p.a]
    $m  (pairs:enjs mite+[%s (en-mite p.a)] octs+(tape:enjs (en-base64 q.q.a)) ~)
    $r  p.a
    $j  p.a
  ==
++  from-queries
  |=  {bem/beam quy/(list query)}
  =<  (pairs:enjs (turn quy .))
  |=  a/query
  :-  -.a
  ?-  -.a
    $name  (from-type +.a ?^(s.bem i.s.bem q.bem))
    $beak  (from-type +.a (crip (spud (en-beam bem(s /)))))
    $path  (from-type +.a (crip (spud (flop s.bem))))
    $spur  (from-type +.a (crip (spud s.bem)))
    $bump  (from-type +.a bump.dat)
    $plan  (from-type +.a plan.dat)
    $comt  (from-type +.a comt.dat)
    $head  (from-type +.a head.dat)
    $snip  (from-type +.a snip.dat)
    $sect  (from-type +.a sect.dat)
    $meta  (from-type +.a meta.dat)
    $mime  (from-type +.a mime.dat)
    $body  (from-type +.a body.dat)
    $kids  ?<  (~(has by (malt p.a)) %kids)  ::  XX recursion? 
           =<  o+(~(urn by kid) .)
           |=  {dir/knot dak/tree-include}  ^-  json
           ^^$(quy p.a, s.bem [dir s.bem], dat dak, kid ~)
  ==
--
::
::::
  ::
=,  tree
^-  json
=+  default='spur.t_body.r_comt.j_plan.j_beak.t_meta.j_kids_meta.j_head.r_bump.t'
=+  ^=  schem
    =+  seh=(fall (~(get by qix.gas) 'q') default)
    ~|(bad-noun+seh ;;(schema (rash seh read-schem)))
%+  from-queries  bem.gas
~|  invalid-query+schem
;;((list query) (schema-to-plist schem))
