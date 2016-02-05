::
::::  /hoon/combine/tree/ren
  ::
/-    tree-include
/+    react
/=    mime    /mime/
/=    body    /tree-elem/
/=    sect    /&json&/tree-index/
/=    snip    /&snip&elem&/tree-elem/
/=    meta    /&json&front&/|(/front/ /~[~])
/=    comt    /^  (list (pair time manx))
              /@    /&elem&md&mime&/comment-md/
!:
^-    tree-include
=+  rj=react-to-json:react
=+  cj=|=([a=time b=manx] (jobe time/(jode a) body/(rj b) ~))
:*  mime
    (rj body)
    (rj /h1 hed.snip)   :: head
    (rj /div tal.snip)  :: snip
    meta
    sect
    [%a (turn (sort comt lor) cj)]
==
