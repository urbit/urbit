::    ++down rendering arms
!:
::::
  ::
|%
++  into-inner
  |=  [a=marl b=manx]
  ?~  c.b  b(c a)
  $(b i.c.b)
::
++  flat
  |=  a=marl
  ^-  tape
  ?~  a  ~
  %-  weld  :_  $(a t.a)
  ^-  tape
  ?~  n.g.i.a
    ?>(?=(_:/(**) i.a) v.i.a.g.i.a)
  ?+    n.g.i.a  $(a c.i.a)
      %img
    %-  zing  ^-  wall
    %+  murn  a.g.i.a   |=  [a=mane b=tape]
    ^-  (unit tape)
    ?+  a  ~
      %alt  [~ b]
    ==
  ==
++  sanitize
 |=  a=marl  ^-  tape
 =-  (zing `wall`(scan (flat a) fel))
 =<  fel=;~(sfix (star ;~(plug (cold '-' -) (plus +))) (star next)) 
 [(star ;~(less aln prn)) ;~(pose nud low (cook |=(a=@ (add a ' ')) hig))]
::
++  sang                                                ::  tight item children
  |=  a=(list elem)
  ^-  marl
  ?~  a  ~
  %-  weld  :_  $(a t.a)
  ?.  ?=(%para -.i.a)
    (sing i.a ~)
  (sung p.i.a)
::
++  sing                                                ::  elem to manx
  =>  |%
      ++  first-word
        |=  a=tape
        =.  a  (trip (crip a))                          ::  XX valid tapes
        ^-  (unit tape)
        =.  a  q.q:(need q:((star ace) [1 1] a))
        =+  vex=((plus ;~(less ace prn)) [1 1] a)
        ?~  q.vex  ~
        (some (wonk vex))
      --
  =+  [tig=| had=*(unit mane)]
  |=  lum=(list elem)
  |^  ^-  marl
      =+  a=apex
      ?~  q.a
        p.a
      (weld p.a $(lum q.a))
  ::
  ++  apex
    ^-  [p=marl q=_lum]
    ?~  lum  
      ?~  had  [~ ~]
      (lose "unclosed {<u.had>}")
    =>  [ele=i.lum .(lum t.lum)]
    ?.  ?=(%html -.ele)
      (push (reso ele) ~)
    =+  tex=(trip (role p.ele))
    =^  mar  tex  [p q.q]:(need q:(many:poxa 1^1 tex))
    ?^  mar
      (push mar)
    =^  hed  lum  (chomp tex head:poxa)
    ?^  hed
      =+  max=`marx`u.hed
      (push(lum q) [max p] ~):[apex(had `n.max) .]
    =^  tal  lum  (chomp tex tail:poxa)
    ?~  tal
      (push ;lost:"{tex}" ~)
    ?:  =(had tal)
      [~ lum]
    ?^  had
      =.  lum  [ele lum]
      (lose "unclosed {<u.had>}")
    (lose "close {<u.tal>}")
  ::
  ++  lose  |=(a=tape [[;lost:"{a}"]~ lum])
  ++  chomp
    |*  [tap=tape fel=_rule]
    ^-  [(unit ,_(wonk *fel)) _lum]
    =+  vex=(fel 1^1 tap)
    ?~  q.vex  [~ lum]
    :-  [~ (wonk vex)]
    ?~(q.q.u.q.vex lum [[%html (crip q.q.u.q.vex) ~] lum])
  ::
  ++  push
    |=  a=marl
    ^+  apex
    ?~  a  apex
    [[b p] q]:[b=i.a (push t.a)]
  ::
  ++  reso
    |=  a=elem
    ?^  -.a
      =.  tig  ?.(?=(%list -.p.a) tig p.p.a)
      ?:  &(tig ?=(%item -.p.a))
        [/li (sang q.a)]
      %+  into-inner  ^$(lum q.a)
      ?-  -.p.a
        %bloq  ;blockquote;
        %item  ;li;
        %list  ?@  q.p.a  ;ul;
               ?:  =(1 p.q.p.a)  ;ol;
               =+  num=(pojo (jone p.q.p.a))
               ;ol(start num);
      ==
    ?-  -.a  ::  :/("unimplemented {<p.a>}")
      %html  !!                       ::  handled earlier   XX do type stuff
      %para  [/p (sung p.a)]
      %head  
        =+  [hed=(add %h0 (lsh 3 1 p.a)) kid=(sung q.a)]
        [[hed id/(sanitize kid) ~] kid]
    ::
      %hrul  ;hr;
  ::     %html  
             ::=+  tex=(role (turn p.a crip))
             ::=+  (poxa tex)
             ::?^  -  u.-
             ::=+  (rush tex (star ;~(pose gah comt:poxa)))
             ::?^  -  :/(~)
             ::;lost: {<p.a>}
        :: :/([(role (turn p.a crip))]~)                ::  XX  haaaaaaack
      %defn  :/(~)
      %code  =+  lan=?~(p.a ~ (first-word r.u.p.a))
             =+  tex=(trip (role q.a))
             ?~  lan  ;pre:code:"{tex}"
             ;pre:code(class "language-{u.lan}"):"{tex}"

    ==
  --
::
++  sung
  |=  lim=kids
  =+  had=*(unit mane)
  |^  ^-  marl
      =+  a=apex
      ?~  q.a
        p.a
      (weld p.a $(lim q.a))
  ::
  ++  apex
    ^-  [p=marl q=_lim]
    ?~  lim  
      ?~  had  [~ ~]
      (lose "unclosed {<u.had>}")
    =>  [ele=i.lim .(lim t.lim)]
    ?.  ?=(%htmt -.ele)
      ?:  &(?=(%$ -.ele) ?=([[%$ *] *] lim))
        apex(p.i.lim (weld p.ele p.i.lim))
      (push (reso ele) ~)
    =+  tex=(trip p.ele)
    =^  emo  lim  (chomp tex empt:poxa)
    ?^  emo
      =+  man=`manx`u.emo
      (push man ~)
    =^  hed  lim  (chomp tex head:poxa)
    ?^  hed
      =+  max=`marx`u.hed
      (push(lim q) [max p] ~):[apex(had `n.max) .]
    =^  tal  lim  (chomp tex tail:poxa)
    ?~  tal
      (push ;lost:"{tex}" ~)
    ?:  =(had tal)
      [~ lim]
    ?^  had
      =.  lim  [ele lim]
      (lose "unclosed {<u.had>}")
    (lose "unopened {<u.tal>}")
  ::
  ++  lose  |=(a=tape [[;lost:"{a}"]~ lim])
  ++  chomp
    |*  [tap=tape fel=_rule]
    ^-  [(unit ,_(wonk *fel)) _lim]
    =+  vex=(fel 1^1 tap)
    ?~  q.vex  [~ lim]
    :-  [~ (wonk vex)]
    ?~(q.q.u.q.vex lim [[%htmt (crip q.q.u.q.vex)] lim])
  ::
  ++  push
    |=  a=marl
    ^+  apex
    ?~  a  apex
    [[b p] q]:[b=i.a (push t.a)]
  ::
  ++  urly
    |=  a=tape  ^-  tape
    ?~  a  ~
    :: ?:  (gth i.a 0xff)  "XX"                       ::  XX
    ?:  ?|  [?=(^ q)]:(alp 1^1 a)
            (~(has in (sa "!*'();:@&=+$,/?#%.~_")) i.a) ::  XX  reparse
        ==
      [i.a $(a t.a)]
    (weld (urle (trip i.a)) $(a t.a))
  ::
  ++  reso
    |=  b=inline
    ^-  manx
    ?@  -.b
      ?-  -.b
        %$     :/(p.b)
        %line  ;br;
        %code  ;code:"{p.b}"
        %htmt  !!  ::p.b              ::  handled earlier ::  XX  do type stuff
      ==
    ?:  ?=(%blot -.p.b)
      =+  res=`manx`;img(src (urly p.p.b), alt (flat (turn q.b ..$)));
        :: ;img@"{p.p.b}";
      ?~  q.p.b  res
      res(a.g (welp a.g.res title/u.q.p.b ~))
    =+  kid=(sung q.b)
    %+  into-inner  kid
    ?-  p.b
      [%emph ?]  ?.(p.p.b ;em; ;strong;)
      [%delt ~]  ;del;
      [%link ^]  =+  url=(urly p.p.b)
                 =.  url  ?^(url url "#{(sanitize kid)}")
                 ?~  q.p.b  ;a/"{url}";
                 ;a/"{url}"(title u.q.p.b);
    ==
  --
--
