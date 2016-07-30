::
::::  /hoon/combine/tree/ren
  ::
/?    310
/-    tree-include
/+    react
/=    mime    /mime/
/=    body    /tree-elem/
/=    sect    /&json&/tree-index/
/=    snip    /&snip&elem&/tree-elem/
/=    meta    /&json&front&/|(/front/ /~[~])
/=    plan    /^  json  /|(/plan-json/ /~[~])
/=    comt    /&json&/tree-comments/
/=    bump    /;  |=(a/(list {p/time *}) =.(a (flop a) ?~(a '' (scot %da p.i.a))))
              /tree-comments/
!:
^-    tree-include
=+  rj=react-to-json:react
:*  mime
    (rj body)
    (rj /h1 hed.snip)   :: head
    (rj /div tal.snip)  :: snip
    meta
    sect
    comt
    plan
    bump
==
