::
::::  /hoon/down/mar
  ::
/?    314
/-    markdown
/+    down-jet
::
::::
  ::
[markdown .]
|_  don=down
++  grab                                                ::  convert from
  |%
  ++  md  |=(src=@t (mark:down-jet src))
  ++  noun  down                                        ::  clam from %noun
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
