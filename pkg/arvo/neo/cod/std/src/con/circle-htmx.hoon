/@  circle
:-  [%circle %$ %htmx]
|=  circle
|=  =bowl:neo
|^
;div.p3
  ;div.p2.mw-page.ma
    ;+  title
    ;+  form-new-ship
    ;+  friends-list
  ==
==
::  sur
+$  circle  (set @p)
::  manxes
++  friends-list
  ^-  manx
  ;div.fc.g2
    ;*
    %+  turn
      %+  murn
        ~(tap in ~(key by ~(tar of:neo kids.bowl)))
      |=  =road:neo  
      ?.  ?=([[%p ship=@] *] road)
        ~
      `ship.road
    |=  =ship
    ^-  manx
    ;div.border.p2.mono.fr.jb
      ;div: {<ship>}
      ;button.b1.border.hover.br1:  x
    ==
  ==
++  my-address
  (en-tape:pith:neo :(weld /hawk here.bowl))
++  form-new-ship
  ^-  manx
  ;form.fr.jc.p3
    =hx-post  "{my-address}?stud=circle-diff" :: where to send poke
    =hx-swap  "afterend"                      :: where to put resp
    =hx-target  "this"                        :: where to put resp
    =head  "add"
    ;input.border
      =name  "ship"
      =type  "text"
      =autocomplete  "off"
      =placeholder  "~hansum-tiller"
      =oninput  "this.setAttribute('value', this.value)"
      ;
    ==
    ;button.border.b1
      ; add
    ==
  ==
++  title
  ^-  manx
  ;div.fr.jc.ac.prose
    ;h1.tc.border.wfc.fr.jc.ac
      =style
        """
        width:100px;
        height:100px;
        border-radius:99%
        """
      ; circle
    ==
  ==
--

