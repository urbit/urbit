/@  tree
/@  tree-diff
^-  kook:neo
|%
++  state  pro/%tree
++  poke  (sy %tree-diff %ack ~)
++  kids  *kids:neo
++  deps  *deps:neo
++  form  
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
  |=  pal=(unit pail:neo)
  ^-  (quip card:neo pail:neo)
  :_  tree/!>(~)
  ~
  ++  poke
    |=  [=stud:neo =vase]
    ^-  (quip card:neo pail:neo)
    ~&  >  stud/stud
    =/  this  !<(=tree q.pail)
    ?+  stud  !!
    :: ::
    ::     %ack-nack
    ::   ~&  >>>  ack-nack/!<((unit tang) vase)
    ::   [~ tree/!>(~)]
    ::
        %ack
      ~&  >>  ack-vase-type/+.vase
      ~&  >>  ack/!<((unit quit:neo) vase)
      [~ tree/!>(~)]
      ::
        %tree-diff
      =/  diff  !<(tree-diff vase)
      ~&  >>>  diff-tree-imp/diff
      ?+  -.diff  !!
          %send-tomb
        =/  =pith:neo  +.diff
        ~&  >>>  pith-tomb/pith
        :_   tree/!>(~)
        :~  
            [pith %cull ~]
            [pith %tomb ~]
        ==
      ==
    ==
  --
--