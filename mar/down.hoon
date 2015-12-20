::
::::  /hoon#down#mar
  ::
/?    314
/-    markdown
/+    down-jet, frontmatter
::
::::
  ::
[markdown .]
|_  don/down
++  grab                                                ::  convert from
  |%
  ++  noun  down                                        ::  clam from %noun
  ++  md
    |=  src/@t
    =+  [atr mud]=(parse:frontmatter (lore src))
    [[%meta atr] (mark:down-jet mud)]
  --
::
++  grow                                                ::  convert into
  |%
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
