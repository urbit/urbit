/+    tree, react
/=    mime    /mime/
/=    body    /tree-elem/
/=    sect    /tree-index/
/=    meta    /|(/front/ /~[~])
/=    snip    /&snip&elem&/tree-elem/
!:
^-    tree-include
=+  rj=react-to-json:react
=+  fj=|=(atr=(map span span) [%o (~(run by atr) |=(a=span s/a))])
=+  ij=(map-to-json:tree |=(a=path (crip (spud a))) |=(a=marl [%a (turn a rj)]))
[mime (rj body) (rj /h1 hed.snip) (rj /div tal.snip) (fj meta) (ij sect)]
