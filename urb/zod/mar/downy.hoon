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
|_  [atr=(map cord cord) don=down]
++  grab                                                ::  convert from
  |%
  ++  mdy  |=([atr=_atr src=@t] [atr (mark:down-jet src)])
  ++  noun  ,[(map cord cord) down]                     ::  clam from %noun
  --
::
++  grow                                                ::  convert into
  |%
  ++  hymn                                          ::  convert to %hymn
    ;html
      ;head:title:"Untitled"
      ;body  ;*  +:elem
      ==
    ==
  ++  elem                                          ::  convert to %elem
    =+  jon=`json`o/(~(run by atr) |=(cord s/+<))
    ;div
      ;*  :-  ;meta(value "{(pojo jon)}", name "frontmatter", urb_front "");
          (print:down-jet don)
    ==
  ::  ++  react  elem
  --
--
