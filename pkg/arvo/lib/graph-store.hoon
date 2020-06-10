/-  sur=graph-store, pos=post
/+  res=resource
=<  [sur .]
=<  [pos .]
=<  [res .]
=,  sur
=,  pos
|%
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
      *json
    ::
    ++  index
      |=  i=^index
      ^-  json
      *json
    ::
    ++  nodes
      |=  n=(map ^index node)
      ^-  json
      *json
    ::
    ++  indices
      |=  i=(set ^index)
      ^-  json
      *json
    ::
    ++  uid
      |=  u=^uid
      ^-  json
      *json
    ::
    ++  signatures
      |=  s=^signatures
      ^-  json
      *json
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
