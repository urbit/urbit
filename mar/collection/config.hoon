::
::::  /hoon/config/collection/mar
  ::
/+  collections
|_  con=config:collections
::
::
++  grow
  |%
  ++  mime
    :-  /text/x-collection-config 
    (as-octs:mimes:html (jam con))
::  ++  txt
::    |^
::    ^-  (list @t)
::    ;:  weld
::    ::
::      :~  (cat 3 'name: ' goodname.con)
::          (cat 3 'owner: ' (scot %p owner.con))
::          (cat 3 'date-created: ' (scot %da date-created.con))
::          (cat 3 'last-modified: ' (scot %da last-modified.con))
::          (cat 3 'comments: ' ?:(comments.con 'y' 'n'))
::      ==
::    ::
::      ?~  sort-key.con  ~ 
::      [(cat 3 'sort-key: ' (scot %ud u.sortkey.con)) ~]
::    ::
::      :-  'config-permissions:'
::      (rules-to-txt r.config-permissions.con w.config-permissions.con)
::    ::
::      :-  'content-permissions:'
::      (rules-to-txt r.content-permissions.con w.content-permissions.con)
::    ::
::    ==
::    ++  rules-to-txt
::      |=  [r=rule:clay w=rule:clay]
::      ^-  (list @t)
::      ;:  weld
::      ::
::        :*  (cat 3 '  read: ' mod.r)
::            %+  turn  ~(tap by who.r)
::            |=  hoe=whom:clay
::            ?-  -.hoe
::              %&  (cat 3 '    ' (scot %p +.hoe))
::              %|  (cat 3 '    ' +.hoe)
::            ==
::        ==
::      ::
::        ['  ==' ~]
::      ::
::        :*  (cat 3 '  write: ' mod.w)
::            %+  turn  ~(tap by who.w)
::            |=  hoe=whom:clay
::            ?-  -.hoe
::              %&  (cat 3 '    ' (scot %p +.hoe))
::              %|  (cat 3 '    ' +.hoe)
::            ==
::        ==
::      ::
::        ['  ==' ~]
::      ==
::    --
  --
++  grab
  |%
  ++  mime
    |=  [mite:eyre p=octs:eyre]
    ((hard config:collections) (cue q.p))
::  ++  txt
::    |=  txs=(list @t) 
::    ?>  ?=  $:  name=@t
::                owner=@t
::                dc=@t
::                lm=@t
::                com=@t
::                res=(lest @t)
::            ==
::        txs
::    ::
::    =/  top
::    :*  (rash name.txs ;~(pfix (jest 'name: ') (cook crip (star next))))
::        (rash owner.txs ;~(pfix (jest 'owner: ~') (fed:ag)))
::        (rash dc.txs ;~(pfix (jest 'date-created: ~') (cook |=(a=^ +.a) crub:so)))
::        (rash lm.txs ;~(pfix (jest 'last-modified: ~') (cook |=(a=^ +.a) crub:so)))
::        (rash com.txs ;~(pfix (jest 'comments: ') (flag %y %n)))
::    ==

     




  ++  noun  config:collections
  --
++  grad  %mime
--
