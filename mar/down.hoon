::
::::  /hoon/down/mar
  ::
/?    310
/-    markdown
/+    down-jet, frontmatter
::
::::
  ::
=,  format
=,  markdown
|_  don/down
++  grab                                                ::  convert from
  |%
  ++  noun  down                                        ::  clam from %noun
  ++  md
    |=  src/@t
    =+  [atr mud]=(parse:frontmatter (to-wain src))
    [[%meta atr] (mark:down-jet mud)]
  --
::
++  grow                                                ::  convert into
  |%
  ++  front  ?~(don ~ ?:(?=($meta -.i.don) p.i.don front(don t.don)))
  ++  hymn                                          ::  convert to %hymn
      ;html
        ;head:title:"Untitled"
        ;body
          ;*  (print:down-jet don)
        ==
      ==
  ++  elem                                          ::  convert to %elem
    ;div
      ;*  (print:down-jet don)
    ==
  ::  ++  react  elem
  --
--
