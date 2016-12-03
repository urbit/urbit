::    A Twitter API library.
::
::::  /hoon/twitter/lib
  ::
/?    314
/-    twitter
/+    interpolate, hep-to-cab
=+  sur-twit:^twitter  :: XX
=,  ^eyre
=,  mimes:html
=,  html
=,  format
=,  url:eyre
=,  chrono:userlib
::
::::  functions
  ::
|%
++  join
  |=  {a/char b/(list @t)}  ^-  @t
  %+  rap  3
  ?~  b  ~
  |-(?~(t.b b [i.b a $(b t.b)])) 
::
++  valve                                               ::  produce request
  |=  {med/?($get $post) pax/path quy/quay}
  ^-  hiss
  =+  url=(scan "https://api.twitter.com/1.1/.json" auri:urlp)  :: base path
  =.  q.q.url  (welp q.q.url pax)
  =.  r.url  quy
  ^-  hiss
  ?-  med
    $get  [url med *math ~]
    $post
      =+  hed=(my content-type+['application/x-www-form-urlencoded']~ ~)
      [url(r ~) med hed ?~(r.url ~ (some (as-octt +:(tail:earn r.url))))]
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
::
::::  library
  ::
|%
++  render                                                ::  response printers
  =+  args:reqs
  |%
  ++  mean
    |=  {msg/@t num/@ud}  ^-  tank
    rose+[": " `~]^~[leaf+"Error {<num>}" leaf+(trip msg)]
  ::
  ++  user-url
    |=  a/scr  ^-  purf
    :_  ~
    %^  into-url:interpolate  'https://twitter.com/:scr'
      ~
    ~[scr+a]
  ::
  ++  post-url
    |=  {a/scr b/tid}  ^-  purf
    :_  ~
    %^  into-url:interpolate  'https://twitter.com/:scr/status/:tid'
      ~
    ~[scr+a tid+(tid:print b)]
  --
++  parse  ^?                                            ::  text parsers
  |%
  ++  user  (cook crip (plus ;~(pose aln cab)))
  --
::
++  reparse                                              ::  json reparsers
  =,  parse
  |%
  ++  ce  =>(dejs |*({a/_* b/fist} (cu |=(c/a c) b))) ::  output type
  ++  fasp  |*(a/{@tas *} [(hep-to-cab -.a) +.a])
  ++  mean  (ot errors+(ar (ot message+so code+ni ~)) ~):dejs
  ++  post
    =,  ^?(dejs)
    %+  ce  post:sur-twit
    %-  ot
    :~  id+ni
        user+(ot (fasp screen-name+(su user)) ~)
        (fasp created-at+(cu year (ci stud so)))
        text+(cu crip (su (star escp:de-xml)))  :: parse html escapes
    ==
  ++  usel 
    =,  ^?(dejs)
    %+  ce  (list who/@ta)
    =-  (ot users+(ar -) ~)
    (ot (fasp screen-name+(su user)) ~)
  --
++  print
  =+  args:reqs
  |%
  ++  tid  |=(@u `@t`(rsh 3 2 (scot %ui +<)))
  ++  scr  |=(@t +<)
  ++  lsc
    |=  a/$@(^scr ^lsc)  ^-  @t
    ?@(a `@t`a (join ',' a))
  ::
  ++  lid
    |=  a/$@(^tid (list ^tid))  ^-  @t
    ?~  a  ~|(%nil-id !!)
    ?@(a (tid a) (join ',' (turn `(list ^tid)`a tid)))
  --
++  request
  =<  apex
  =+  args:reqs
  |%
  ++  apex
    |=  {a/endpoint b/quay}  ^-  hiss
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
    %+  into-path-partial:interpolate
      (path:hep-to-cab pax)
    =-  (weld - quy)
    %+  turn  ban
    |=  p/param
    ^-  {@t @t}
    :-  (hep-to-cab -.p)
    ?+  -.p  p.p  :: usually plain text
      ?($source-id $target-id)       (tid:print p.p)
      ?($id $name $user-id)  (lid:print p.p)
      $screen-name                   (lsc:print p.p)
    ==
  --
--
