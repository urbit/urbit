/@  tree
/@  tree-diff
^-  kook:neo
|%
++  state  pro/%tree
++  poke  (sy %tree-diff ~)
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
      ::   %tree  
      :: [~ tree/vase]
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
            [pith %tomb ~]
            [pith %cull ~]
        ==
      ==
    ==
  --
--