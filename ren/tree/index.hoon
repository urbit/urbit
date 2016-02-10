::
::::  /hoon/index/tree/ren
  ::
/+    tree
/,        /
    /;  (getall:tree /h1/h2/h3/h4/h5/h6)  /tree-elem/
::
          /pub/docs/dev/hoon/runes
    /;  |=  [tip=marl sub=(map span marl) ~]
        (zing `(list marl)`[tip (turn (~(tap by sub)) tail)])
    /.    /;    (getall:tree %h1 ~)    /tree-elem/
          /_    /;    (getall:tree %h1 ~)    /tree-elem/
==  ==
::
::::
  ::
`(map path marl)`[[/ -.-] ~ ~]
