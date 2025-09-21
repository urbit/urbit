=<
|%  
++  diff
  |=  [old=* new=*]
  ^-  patch 
  =/  del  (extract-del (oracle old new) old)
  =/  ins  (extract-ins (oracle old new) new)
  =/  allowed-holes  (~(int in (find-del-holes del)) (find-ins-holes ins))
  =.  del  (filter-del-holes allowed-holes del)
  =/  ins  (filter-ins-holes allowed-holes ins)
  =/  closed-patch  (closure (gcp [del ins]))
  ?>  =(& +.closed-patch)
  -.closed-patch
++  apply  
  |=  [patch=_id noun=*]
  ?-    -.patch
      %diff
    =/  var-map  (del del.patch noun)
    (ins ins.patch var-map)
  ::
      %cell
    ?>  ?=(^ noun)
    [$(patch lhs.patch, noun -.noun) $(patch rhs.patch, noun +.noun)]
  ==
++  id
  ^-  patch
  [%diff [%hole ~] [%hole ~]]
+$  patch
  $%  [%cell lhs=patch rhs=patch]
      [%diff ^diff]
  ==
--
::
|%
+$  del-diff
  $%  [%hole @]
      [%cell lhs=del-diff rhs=del-diff]
      [%ignore ~]
  ==
+$  ins-diff
  $%  [%hole @ original=*]
      [%cell lhs=ins-diff rhs=ins-diff]
      [%atom @]
  ==
+$  final-ins-diff
  $%  [%hole @]
      [%cell lhs=final-ins-diff rhs=final-ins-diff]
      [%atom @]
  ==
+$  diff  [del=del-diff ins=final-ins-diff]
+$  patch
  $%  [%cell lhs=patch rhs=patch]
      [%diff diff]
  ==
++  insify-noun
  |=  noun=*
  ^-  final-ins-diff
  ?-  noun
    ^  [%cell $(noun -.noun) $(noun +.noun)]
    @  [%atom noun]
  ==
++  empty-set  (silt `(list @)`~)
++  find-del-holes
  |=  diff=del-diff
  ~+
  ^-  (set @)
  ?-  -.diff
    %hole  (silt ~[+.diff])
    %cell  (~(uni in $(diff lhs.diff)) $(diff rhs.diff))
    %ignore  empty-set
  ==
++  find-final-ins-holes
  |=  diff=final-ins-diff
  ~+
  ^-  (set @)
  ?-  -.diff
    %hole  (silt ~[+.diff])
    %cell  (~(uni in $(diff lhs.diff)) $(diff rhs.diff))
    %atom  empty-set
  ==
++  find-ins-holes
  |=  diff=ins-diff
  ~+
  ^-  (set @)
  ?-  -.diff
    %hole  (silt ~[+<.diff])
    %cell  (~(uni in $(diff lhs.diff)) $(diff rhs.diff))
    %atom  empty-set
  ==
++  filter-del-holes
  |=  [allowed-holes=(set @) diff=del-diff]
  ^-  del-diff
  ?:  ?=(%ignore -.diff)  diff
  ?-  -.diff
    %hole  ?:((~(has in allowed-holes) +.diff) diff [%ignore ~])
    %cell  [%cell $(diff +<.diff) $(diff +>.diff)]
  ==
++  filter-ins-holes
  |=  [allowed-holes=(set @) diff=ins-diff]
  ^-  final-ins-diff
  ?-    -.diff
      %cell  [%cell $(diff +<.diff) $(diff +>.diff)]
      %atom  diff
      %hole
    ?:  (~(has in allowed-holes) +<.diff)  [%hole +<.diff]
    (insify-noun original:diff)
  ==
++  gcp
  |=  diff=diff
  ^-  patch
  ?-    -.ins.diff
      %atom  [%diff diff]
      %hole  [%diff diff]
      %cell
    ?.  ?=(%cell -.del.diff)
      [%diff diff]
    [%cell $(diff [+<.del.diff +<.ins.diff]) $(diff [+>.del.diff +>.ins.diff])]
  ==
++  closure
  |=  =patch
  ^-  [^patch ?]
  ?-    -.patch
      %diff
    =/  del-holes  (find-del-holes del:patch)
    =/  ins-holes  (find-final-ins-holes ins:patch)
    =/  difference  (~(dif in ins-holes) del-holes)
    [patch =(difference empty-set)]
  ::
      %cell
    =/  lhs  $(patch lhs:patch)
    =/  rhs  $(patch rhs:patch)
    ?:  ?&(+.lhs +.rhs)  [[%cell -.lhs -.rhs] &]
    $(patch (pull-diff [%cell -.lhs -.rhs]))
  ==
++  pull-diff
  |=  =patch
  ^-  [%diff del=del-diff ins=final-ins-diff]
  ?-    -.patch
      %diff  patch
      %cell
    =/  pulled-lhs  $(patch lhs:patch)
    =/  pulled-rhs  $(patch rhs:patch)
    :+  %diff 
      [%cell del:pulled-lhs del:pulled-rhs]
    [%cell ins:pulled-lhs ins:pulled-rhs]
  ==
++  is-subtree  
  |=  [tree=* subtree=*]
  ~+
  ^-  ?
  ?:  =(tree subtree)  &
  ?@  tree  |
  ?|  (is-subtree -.tree subtree)
      (is-subtree +.tree subtree)
  ==
++  oracle
  |=  [a=* b=*]
  |=  subtree=*
  ^-  (unit @)
  ?:  ?&  (is-subtree a subtree)
          (is-subtree b subtree)
      ==
    `(mug subtree)
  ~
++  extract-del
  |=  [oracle=$-(* (unit @)) subtree=*]
  ~+
  ^-  del-diff
  =/  hash  (oracle subtree)
  ?^  hash  [%hole +.hash]
  ?@  subtree  [%ignore ~]
  [%cell (extract-del oracle -.subtree) (extract-del oracle +.subtree)]
++  extract-ins
  |=  [oracle=$-(* (unit @)) subtree=*]
  ~+
  ^-  ins-diff
  =/  hash  (oracle subtree)
  ?^  hash  [%hole +.hash subtree]
  ?@  subtree  [%atom subtree]
  [%cell (extract-ins oracle -.subtree) (extract-ins oracle +.subtree)]
++  ins 
  |=  [diff=final-ins-diff var-map=(map @ *)]
  ^-  *
  ?-  -.diff
    %atom  +.diff
    %cell  [$(diff +<.diff) $(diff +>.diff)]
    %hole  (~(got by var-map) +.diff)
  ==
++  del  
  |=  [diff=del-diff noun=*]  
  ^-  (map @ *)
  |^  (go diff noun ((map @ *) ~))
  ++  go  
    |=  [diff=del-diff noun=* var-map=(map @ *)]
    ^-  (map @ *)
    ?-    -.diff
        %ignore
      var-map
    ::
        %hole
      =/  subtree  (~(get by var-map) +.diff)
      ?~  subtree  (~(put by var-map) +.diff noun)
      ?>  =(+.subtree noun)
      var-map
    ::
        %cell
      ?>  ?=(^ noun)
      =/  lhs-var-map  $(diff +<.diff, noun -.noun)
      =/  rhs-var-map  $(diff +>.diff, noun +.noun, var-map lhs-var-map)
      rhs-var-map
    ==
  --
--
