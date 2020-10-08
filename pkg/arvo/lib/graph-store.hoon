/-  sur=graph-store, pos=post
/+  res=resource
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
++  orm      ((ordered-map atom node) gth)
++  orm-log  ((ordered-map time logged-update) gth)
::
++  util
  |_  =bowl:gall
  ++  peek-post
    |=  [=app=resource =index]
    ^-  post
    =/  her=cord  (scot %p entity.app-resource)
    =/  nam=cord  name.app-resource
    =/  inx=path  (turn index |=(@ (scot %ud +<)))
    =<  post
    .^  node
      %gx  (scot %p our.bowl)  %graph-store  (scot %da now.bowl)
      %post  her  nam  inx
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
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
  ++  update
    |=  upd=^update
    ^-  json
    ?>  ?=(%0 -.upd)
    |^  (frond %graph-update (pairs ~[(encode q.upd)]))
    ::
    ++  encode
      |=  upd=update-0
      ^-  [cord json]
      ?-  -.upd
          %add-graph
        :-  %add-graph
        %-  pairs
        :~  [%resource (enjs:res resource.upd)]
            [%graph (graph graph.upd)]
            [%mark ?~(mark.upd ~ s+u.mark.upd)]
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
      ::
          %keys
        [%keys [%a (turn ~(tap in resources.upd) enjs:res)]]
      ::
          %tags
        [%tags [%a (turn ~(tap in tags.upd) |=(=term s+term))]]
      ::
          %run-updates
        [%run-updates ~]
      ::
          %tag-queries
        :-  %tag-queries
        %-  pairs
        %+  turn  ~(tap by tag-queries.upd)
        |=  [=term =resources]
        ^-  [cord json]
        [term [%a (turn ~(tap in resources) enjs:res)]]
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
  ::
  ++  index  (su ;~(pfix net (more net dem)))
  ::
  ++  update
    |=  jon=json
    ^-  ^update
    :-  %0
    :-  *time
    ^-  update-0
    =<  (decode jon)
    |%
    ++  decode
      %-  of
      :~  [%add-nodes add-nodes]
          [%remove-nodes remove-nodes]
          [%add-signatures add-signatures]
          [%remove-signatures remove-signatures]
        ::
          [%add-graph add-graph]
          [%remove-graph remove-graph]
        ::
          [%add-tag add-tag]
          [%remove-tag remove-tag]
        ::
          [%archive-graph archive-graph]
          [%unarchive-graph unarchive-graph]
          [%run-updates run-updates]
        ::
          [%keys keys]
          [%tags tags]
          [%tag-queries tag-queries]
      ==
    ::
    ++  add-graph
      %-  ot
      :~  [%resource dejs:res]
          [%graph graph]
          [%mark (mu so)]
      ==
    ::
    ++  graph
      |=  a=json
      ^-  ^graph
      =/  or-mp  ((ordered-map atom ^node) gth)
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
    ::
    ++  keys
      |=  =json
      *resources
    ::
    ++  tags
      |=  =json
      *(set term)
    ::
    ++  tag-queries
      |=  =json
      *^tag-queries
    ::
    ++  run-updates
      |=  a=json
      ^-  [resource update-log]
      [*resource *update-log]
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
