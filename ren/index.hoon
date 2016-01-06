/+    tree
/,        /
    /;  (getall:tree /h1/h2/h3/h4/h5/h6)  /elem/
::
          /pub/docs/dev/hoon/runes
    /;  |=  [tip=marl sub=(map span marl) ~]
        (zing `(list marl)`[tip (turn (~(tap by sub)) tail)])
    /.    /;    (getall:tree %h1 ~)    /elem/
          /_    /;    (getall:tree %h1 ~)    /elem/
==  ==
::
::::
  ::
`(map path marl)`[[/ -.-] ~ ~]
