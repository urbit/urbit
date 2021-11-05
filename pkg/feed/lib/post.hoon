/-  sur=post
/+  res=resource
^? 
=<  [sur .]
=,  sur
|%
++  enjs
  =,  enjs:format
  |%
  ++  gid
    |=  g=^gid
    %-  pairs
    :~  ship+s+(scot %p ship.g)
        time+s+(scot %da time.g)
    ==
  ::
  ++  reference
    |=  ref=reference:contents-0
    |^
    %+  frond  -.ref
    ?-  -.ref
      %graph  (graph +.ref)
      %group  (group +.ref)
      %app    (app +.ref)
      %feed   (gid +.ref)
    ==
    ::
    ++  graph
      |=  [grp=res gra=res idx=^index]
      %-  pairs
      :~  graph+s+(enjs-path:res gra)
          group+s+(enjs-path:res grp)
          index+(index idx)
      ==
    ::
    ++  group
      |=  grp=res
      s+(enjs-path:res grp)
    ::
    ++  app
      |=  [=^^ship =desk p=^path]
      %-  pairs
      :~  ship+s+(scot %p ship)
          desk+s+desk
          path+(path p)
      ==
    --
  ::
  ++  index
    |=  ind=^index
    ^-  json
    :-  %s
    ?:  =(~ ind)
      '/'
    %+  roll  ind
    |=  [cur=@ acc=@t]
    ^-  @t
    =/  num  (numb cur)
    ?>  ?=(%n -.num)
    (rap 3 acc '/' p.num ~) 
  ::
  ++  contents
    |=  c=^contents
    a+(turn c content)
  ::
  ++  content
    |=  c=content:contents-0
    %+  frond  -.c
    ?-  -.c   
      %text       s+text.c
      %mention    s+(scot %p ship.c)
      %url        s+url.c
      %reference  (reference reference.c)
    ::
        %code
      %-  pairs
      :~  expression+s+expression.c
          :-  %output
          ::  virtualize output rendering, +tank:enjs:format might crash
          ::
          =/  result=(each (list json) tang)
            (mule |.((turn output.c tank)))
          ?-  -.result
            %&  a+p.result
            %|  a+[a+[%s '[[output rendering error]]']~]~
          ==
      ==
    ==
  ::
  ++  merge
    |=  [a=json b=json]
    ^-  json
    ?>  &(?=(%o -.a) ?=(%o -.b))
    [%o (~(uni by p.a) p.b)]
  ::
  ++  id
    |=  i=^id
    ^-  json
    s/(scot %da i)
  ::
  ++  envelope
    |=  e=^envelope
    %-  pairs
    :~  id+(id id.e)
        children+a+(turn ~(tap in children.e) id)
    ==
  ::
  ++  missive
    |=  m=^missive
    (merge (envelope -.m) (letter +.m))
  ::
  ++  post
    |=  p=^post
    (merge (missive p.p) (stamps q.p))
  ::
  ++  ship
    |=  s=^^ship
    `json`s+(scot %p s)
  ::
  ++  stamps
    |=  s=^stamps
    %-  pairs
    :~  likes+a+(turn ~(tap in likes.s) ship)
    ::
        :-  %reacts  
        %-  pairs
        %+  turn  ~(tap by reacts.s)
        |=  [s=^^ship =react:^stamps]
        [(scot %p s) s+react]
    ==
  ::
  ++  letter
    |=  l=^letter
    %-  pairs
    :~  parent+?~(parent.l ~ (id u.parent.l))
        author+s+(scot %p author.l)
        contents+(contents contents.l)
        time+(time time-sent.l)
    ==
  ::
  ++  stamps-update
    |=  u=update:^stamps
    %+  frond  -.u
    ?-  -.u
      %ini    (stamps p.u)
      %react   s+react.u
      ?(%unreact %unlike %like)  ~
    ==
  ::
  ++  update
    |=  u=^update
    %+  frond  -.u
    ?-  -.u
      %add-post  (post post.u)
      %del-post  ~
      %stamps    (stamps-update update.u)
    ==
  --
++  dejs
  =,  dejs:format
  |%
  ++  contents  contents-0
  ::
  ++  content-0
    ^-  $-(json content:^contents-0)
    %-  of
    :~  [%mention (su ;~(pfix sig fed:ag))]
        [%text so]
        [%url so]
        [%reference reference]
        [%code eval]
    ==
  ::
  ++  eval
    %-  ot
    :~  expression+so
        output+tang
    ==
  ::
  ++  tang 
    |=  jon=^json
    ^-  ^tang
    ?>  ?=(%a -.jon)
    %-  zing
    %+  turn
      p.jon
    |=  jo=^json
    ^-  (list tank)
    ?>  ?=(%a -.jo)
    %+  turn
      p.jo
    |=  j=^json
    ?>  ?=(%s -.j)
    ^-  tank
    leaf+(trip p.j)
  ::

  ::
  ++  reference
    |^
    %-  of
    :~  graph+graph
        group+dejs-path:res
        app+app
    ==
    ::
    ++  graph
      %-  ot
      :~  group+dejs-path:res
          graph+dejs-path:res
          index+index
      ==
    ::
    ++  app
      %-  ot
      :~  ship+(su ;~(pfix sig fed:ag))
          desk+so
          path+pa
      ==
    --
  ::
  ++  contents-0  
    ^-  $-(json contents:^contents-0)
    (ar content-0)
  --
--
