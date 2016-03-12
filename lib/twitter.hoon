::    A Twitter API library.
::
::::  /hoon/twitter/lib
  ::
/?    314
/-    twitter
=+  sur-twit:^twitter  :: XX
!:
::::  functions
  ::
|%
++  fass                                                ::  rewrite path
  |=  a/path
  %+  turn  a
  |=(b/@t (gsub '-' '_' b))
::
++  gsub                                                ::  replace chars
  |=  {a/@t b/@t t/@t}
  ^-  @t
  ?:  =('' t)  t
  %+  mix  (lsh 3 1 $(t (rsh 3 1 t)))
  =+  c=(end 3 1 t)
  ?:(=(a c) b c)
::
++  join
  |=  {a/char b/(list @t)}  ^-  @t
  %+  rap  3
  ?~  b  ~
  |-(?~(t.b b [i.b a $(b t.b)])) 
::
++  interpolate-some  ::  [/a/:b/c [d+'bar' b+'foo']~] -> [/a/foo/c [d+'bar']~]
  |=  {pax/path quy/quay}  ^-  {path quay}
  =+  ^=  inline                                        ::  required names
      %-  ~(gas in *(set term))
      (murn pax replacable:interpolate-path)
  =^  inter  quy
    (skid quy |=({a/knot @} (~(has in inline) a)))
  [(interpolate-path pax inter) quy]
::
++  interpolate-path  ::  [/a/:b/c [%b 'foo']~] -> /a/foo/c
  =+  replacable=|=(a/knot `(unit term)`(rush a ;~(pfix col sym)))
  |=  {a/path b/(list (pair term knot))}  ^-  path
  ?~  a  ?~(b ~ ~|(unused-values+b !!))
  =+  (replacable i.a)
  ?~  -  [i.a $(a t.a)]  ::  literal value
  ?~  b  ~|(no-value+u !!)
  ?.  =(u p.i.b)  ~|(mismatch+[u p.i.b] !!)
  [q.i.b $(a t.a, b t.b)]
::
++  valve                                               ::  produce request
  |=  {med/?($get $post) pax/path quy/quay}
  ^-  hiss
  =+  url=(scan "https://api.twitter.com/1.1/.json" auri:epur)  :: base path
  =.  q.q.url  (welp q.q.url pax)
  =.  r.url  quy
  ^-  hiss
  ?-  med
    $get  [url med *math ~]
    $post
      =+  hed=(my content-type+['application/x-www-form-urlencoded']~ ~)
      [url(r ~) med hed (some (tact +:(tail:earn r.url)))]
  ==
::
++  find-req
  =+  all=doc-data-dry:reqs
  |=  a/_-:*endpoint:reqs  ^-  {?($get $post) path}
  ?~  all  ~|(endpoint-lost+a !!)     :: type error, should never happen
  ?:  =(a -:*typ.i.all)
    +.i.all
  $(all t.all)
--
!:
::::  library
  ::
|%
++  render                                                ::  response printers
  |%
  ++  mean
    |=  {msg/@t num/@ud}  ^-  tank
    rose+[": " `~]^~[leaf+"Error {<num>}" leaf+(trip msg)]
  --
++  parse                                                ::  json reparsers
  |%
  ++  ce  |*({a/_* b/fist:jo} (cu:jo |=(c/a c) b))       ::  output type
  ++  fasp  |*(a/{@tas *} [(gsub '-' '_' -.a) +.a])     ::  XX usable electroplating
  ++  user  (cook crip (plus ;~(pose aln cab)))
  ++  mean  (ot errors+(ar (ot message+so code+ni ~)) ~):jo
  ++  stat
    =+  jo
    %+  ce  {id+@u who+@ta now+@da txt+@t}
    %-  ot
    :~  id+ni
        user+(ot (fasp screen-name+(su user)) ~)
        (fasp created-at+da)
        text+so
    ==
  ++  usel 
    =+  jo
    %+  ce  (list who/@ta)
    =-  (ot users+(ar -) ~)
    (ot (fasp screen-name+(su user)) ~)
  --
++  request
  =<  apex
  =+  args:reqs
  |%
  ++  apex
    |=  {a/endpoint b/quay}
    =+  [med pax]=(find-req -.a)
    (valve med (cowl pax +.a b))
  ::
  ++  lutt  |=(@u `@t`(rsh 3 2 (scot %ui +<)))
  ++  llsc 
    :: =>  args:reqs
    |=  a/$@(scr (list scr))  ^-  @t
    ?@(a `@t`a (join ',' a))
  ::
  ++  llst  
    |=  a/$@(@t (list @t))  ^-  @t
    ?@(a `@t`a (join ',' a))
  ::
  ++  llid
    :: =+  args:reqs
    |=  a/$@(tid (list tid))  ^-  @t
    ?~  a  ~|(%nil-id !!)
    ?@(a (lutt a) (join ',' (turn `(list tid)`a lutt)))
  ::
  ++  cowl                                        ::  handle parameters
    |=  $:  pax/path 
            ban/(list param)
            quy/quay
        ==
    ^-  {path quay}
    %+  interpolate-some  (fass pax)
    =-  (weld - quy)
    %+  turn  ban
    |=  p/param
    ^-  {@t @t}
    :-  (gsub '-' '_' -.p)
    ?+  -.p  p.p  :: usually plain text
      ?($source-id $target-id)       (lutt p.p)
      ?($follow $id $name $user-id)  (llid p.p)
      $track                         (llst p.p)
      $screen-name                   (llsc p.p)
    ==
  --
--
