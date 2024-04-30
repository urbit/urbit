/@  accel
/@  accel-cell
:-  [%accel %htmx]
|=  =accel
|=  =bowl:neo
|^
;table
  ;*
  ::=/  size  (wyt by kids.bowl)
  =/  size  10
  %+  turn
    (gulf 1 size)
  |=  n=@ud
  ^-  manx  :: row
  ;tr  ;*
    %+  turn
      (gulf 1 size)
    |=  m=@ud
    ^-  manx :: cell
    ;td  ;+
      ::  get vase of [n][m] kid
      =/  p  ~[ud/n ud/m]
      =/  pail
        (~(gut by kids.bowl) p `pail:neo`[%empty !>(['' ~])])
     (input [!<(accel-cell q.pail) n m])
    ==
  ==
==
++  input
  |=  [cell=accel-cell row=@ud column=@ud]
  ^-  manx
  ;input.border.p1
    =type  "text"
    =value  (trip code.cell)
    =row  (scow %ud row)
    =column  (scow %ud column)
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=accel-diff"
    =hx-trigger  "input changed delay:0.4s"
    =hx-swap  "none"
    =oninput  "this.setAttribute('value', this.value);"
    ;
  ==
--