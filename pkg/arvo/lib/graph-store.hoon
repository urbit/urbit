/-  sur=graph-store, pos=post
/+  res=resource, *or-map
=<  [sur .]
=<  [pos .]
=<  [res .]
=,  sur
=,  pos
|%
++  orm      ((or-map atom node) lth)
++  orm-log  ((or-map time action) lth)
::
++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    ^-  json
    ?>  ?=(%0 -.upd)
    |^  (frond %graph-update (pairs ~[(encode +.upd)]))
    ::
    ++  encode
      |=  upd=update-0
      ^-  [cord json]
      ?-  -.upd
          %keys
        [%keys [%a (turn ~(tap in resources.upd) enjs:res)]]
      ::
          %add-graph
        :-  %add-graph
        %-  pairs
        :~  [%resource (enjs:res resource.upd)]
            [%graph (graph graph.upd)]
        ==
      ::
          %remove-graph
        [%remove-graph (enjs:res resource.upd)]
      ::
          %add-nodes
        :-  %add-nodes
        %-  pairs
        :~  [%resource (enjs:res resource.upd)]
            [%nodes (nodes nodes.upd)]
        ==
      ::
          %remove-nodes
        :-  %remove-nodes
        %-  pairs
        :~  [%resource (enjs:res resource.upd)]
            [%indices (indices indices.upd)]
        ==
      ::
          %add-signatures
        :-  %add-signatures
        %-  pairs
        :~  [%uid (uid uid.upd)]
            [%signatures (signatures signatures.upd)]
        ==
      ::
          %remove-signatures
        :-  %remove-signatures
        %-  pairs
        :~  [%uid (uid uid.upd)]
            [%signatures (signatures signatures.upd)]
        ==
      ::
          %add-tag
        :-  %add-tag
        %-  pairs
        :~  [%term s+term.upd]
            [%resource (enjs:res resource.upd)]
        ==
      ::
          %remove-tag
        :-  %remove-tag
        %-  pairs
        :~  [%term s+term.upd]
            [%resource (enjs:res resource.upd)]
        ==
      ==
    ::
    ++  graph
      |=  g=^graph
      ^-  json
      :-  %a
      %+  turn  (tap:orm g)
      |=  [a=atom n=^node]
      ^-  json
      %-  pairs
      :~  [%key (numb a)]
          [%node (node n)]
      ==
    ::
    ++  index
      |=  i=^index
      ^-  json
      =/  j=^tape  ""
      |-
      ?~  i
        [%s (crip j)]
      =/  k=json  (numb i.i)
      ?>  ?=(%n -.k)
      %_  $
          i  t.i
          j  (weld j (weld "/" (trip +.k)))
      ==
    ::
    ++  node
      |=  n=^node
      ^-  json
      %-  pairs
      :~  [%post (post post.n)]
          :-  %children
          ?-  -.children.n
              %empty  ~
              %graph  (graph +.children.n)
          ==
      ==
    ::
    ++  post
      |=  p=^post
      ^-  json
      %-  pairs
      :~  [%author (ship author.p)]
          [%index (index index.p)]
          [%time-sent (time time-sent.p)]
          [%contents [%a (turn contents.p content)]]
          [%hash ?~(hash.p ~ s+(scot %ux u.hash.p))]
          [%signatures (signatures signatures.p)]
      ==
    ::
    ++  content
      |=  c=^content
      ^-  json
      ?-  -.c
          %text       (frond %text s+text.c)
          %url        (frond %url s+url.c)
          %reference  (frond %reference (uid uid.c))
          %code
        %+  frond  %code
        %-  pairs
        :-  [%expression s+expression.c]
        :_  ~
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
    ::
    ++  nodes
      |=  m=(map ^index ^node)
      ^-  json
      :-  %a
      %+  turn  ~(tap by m)
      |=  [n=^index o=^node]
      ^-  json
      %-  pairs
      :~  [%index (index n)]
          [%node (node o)]
      ==
    ::
    ++  indices
      |=  i=(set ^index)
      ^-  json
      [%a (turn ~(tap in i) index)]
    ::
    ++  uid
      |=  u=^uid
      ^-  json
      %-  pairs
      :~  [%resource (enjs:res resource.u)]
          [%index (index index.u)]
      ==
    ::
    ++  signatures
      |=  s=^signatures
      ^-  json
      [%a (turn ~(tap in s) signature)]
    ::
    ++  signature
      |=  s=^signature
      ^-  json
      %-  pairs
      :~  [%signature s+(scot %ux p.s)]
          [%ship (ship q.s)]
          [%life (numb r.s)]
      ==
    --
  --
::
++  dejs
  =,  dejs:format
  |%
  ++  action
    |=  =json
    ^-  ^action
    !!
  --
::
++  create
  |_  [our=ship now=time]
  ++  post
    |=  [=index contents=(list content)]
    ^-  ^post
    :*  our
        index
        now
        contents
        ~
        *signatures
    ==
  --
--
