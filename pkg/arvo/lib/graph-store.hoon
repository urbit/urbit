/-  sur=graph-store, pos=post
/+  res=resource, *or-map
=<  [sur .]
=<  [pos .]
=,  sur
=,  pos
|%
::  NOTE: move these functions to zuse
++  nu                                              ::  parse number as hex
  |=  jon/json
  ?>  ?=({$s *} jon)
  (rash p.jon hex)
::
++  re                                                ::  recursive reparsers
  |*  {gar/* sef/_|.(fist:dejs-soft:format)}
  |=  jon/json
  ^-  (unit _gar)
  =-  ~!  gar  ~!  (need -)  -
  ((sef) jon)
::
++  dank                                              ::  tank
  ^-  $-(json (unit tank))
  =,  ^?  dejs-soft:format
  %+  re  *tank  |.  ~+
  %-  of  :~
    leaf+sa
    palm+(ot style+(ot mid+sa cap+sa open+sa close+sa ~) lines+(ar dank) ~)
    rose+(ot style+(ot mid+sa open+sa close+sa ~) lines+(ar dank) ~)
  ==
::
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
      ::
          %archive-graph
        [%archive-graph (enjs:res resource.upd)]
      ::
          %unarchive-graph
        [%unarchive-graph (enjs:res resource.upd)]
      ==
    ::
    ++  graph
      |=  g=^graph
      ^-  json
      :-  %a
      %+  turn  (tap:orm g)
      |=  [a=atom n=^node]
      ^-  json
      :-  %a
      :~  (index [a]~)
          (node n)
      ==
    ::
    ++  index
      |=  i=^index
      ^-  json
      =/  j=^tape  ""
      |-
      ?~  i  [%s (crip j)]
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
      :-  %a
      :~  (index n)
          (node o)
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
    |=  jon=json
    ^-  ^action
    :-  %0
    ^-  action-0
    =<  (decode jon)
    |%
    ++  decode
      %-  of
      :~  [%add-graph add-graph]
          [%remove-graph remove-graph]
          [%add-nodes add-nodes]
          [%remove-nodes remove-nodes]
          [%add-signatures add-signatures]
          [%remove-signatures remove-signatures]
          [%add-tag add-tag]
          [%remove-tag remove-tag]
          [%archive-graph archive-graph]
          [%unarchive-graph unarchive-graph]
      ==
    ::
    ++  add-graph
      %-  ot
      :~  [%resource dejs:res]
          [%graph graph]
      ==
    ::
    ++  graph
      |=  a=json
      ^-  ^graph
      =/  or-mp  ((or-map atom ^node) lth)
      %+  gas:or-mp  ~
      %+  turn  ~(tap by ((om node) a))
      |*  [b=cord c=*]
      ^-  [atom ^node]
      =>  .(+< [b c]=+<)
      [(rash b dem) c]
    ::
    ++  remove-graph  (ot [%resource dejs:res]~)
    ++  archive-graph  (ot [%resource dejs:res]~)
    ++  unarchive-graph  (ot [%resource dejs:res]~)
    ::
    ++  add-nodes
      %-  ot
      :~  [%resource dejs:res]
          [%nodes nodes]
      ==
    ::
    ++  nodes  (op ;~(pfix net (more net dem)) node)
    ::
    ++  node
      %-  ot
      :~  [%post post]
          ::  TODO: support adding nodes with children by supporting the
          ::  graph key
          [%children (of [%empty ul]~)]
      ==
    ::
    ++  post
      %-  ot
      :~  [%author (su ;~(pfix sig fed:ag))]
          [%index index] 
          [%time-sent di]
          [%contents (ar content)]
          [%hash (mu nu)]
          [%signatures (as signature)]
      ==
    ::
    ++  content
      %-  of
      :~  [%text so]
          [%url so]
          [%reference uid]
          [%code eval]
      ==
    ::
    ++  eval
      |=  a=^json
      ^-  [cord (list tank)]
      =,  ^?  dejs-soft:format
      =+  exp=((ot expression+so ~) a)
      %-  need
      ?~  exp  [~ '' ~]
      :+  ~  u.exp
      ::  NOTE: when sending, if output is an empty list,
      ::  graph-store will evaluate
      (fall ((ot output+(ar dank) ~) a) ~)
    ::
    ++  remove-nodes
      %-  ot
      :~  [%resource dejs:res]
          [%indices (as index)]
      ==
    ::
    ++  add-signatures
      %-  ot
      :~  [%uid uid]
          [%signatures (as signature)]
      ==
    ::
    ++  remove-signatures
      %-  ot
      :~  [%uid uid]
          [%signatures (as signature)]
      ==
    ::
    ++  signature
      %-  ot
      :~  [%hash nu]
          [%ship (su ;~(pfix sig fed:ag))]
          [%life ni]
      ==
    ::
    ++  uid
      %-  ot
      :~  [%resource dejs:res]
          [%index index]          
      ==
    ::
    ++  index  (su ;~(pfix net (more net dem)))
    ::
    ++  add-tag
      %-  ot
      :~  [%term so]
          [%resource dejs:res]
      ==
    ::
    ++  remove-tag
      %-  ot
      :~  [%term so]
          [%resource dejs:res]
      ==
    --
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
