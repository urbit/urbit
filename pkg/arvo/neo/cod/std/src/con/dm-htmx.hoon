/@  message
/-  feather-icons
/-  messages
:-  [%ship %$ %htmx]
|=  =ship
|=  =bowl:neo
^-  manx
;div.p2
  =label  "Chat"
    ;+  script:messages
    ;+  style:messages
    ;div.ma.fe.g2.wf
    =style  "max-width: 650px;"
      ;div.fc.g2.wf
        =id  "children"
        ;+  (render-messages:messages bowl)
      ==
      ;+  (render-sender:messages [bowl /pub])
  ==
==