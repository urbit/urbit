/+    tree, react
/=    mime    /mime/
/=    body    /tree-elem/
/=    sect    /tree-index/
/=    snip    /&snip&elem&/tree-elem/
/=    meta    /^  (map span cord)  /|(/front/ /~[~])
/=    comt    /^  (list (pair time manx))
              /@    /&elem&md&mime&/comment-md/
!:
^-    tree-include
=+  rj=react-to-json:react
=+  ij=(map-to-json:tree |=(a=path (crip (spud a))) |=(a=marl [%a (turn a rj)]))
=+  cj=|=([a=time b=manx] (jobe time/(jode a) body/(rj b) ~))
:*  mime
    (rj body)
    (rj /h1 hed.snip)   :: head
    (rj /div tal.snip)  :: snip
    [%o (~(run by meta) |=(a=span s/a))]
    (ij sect)
    [%a (turn (sort comt lor) cj)]
==
