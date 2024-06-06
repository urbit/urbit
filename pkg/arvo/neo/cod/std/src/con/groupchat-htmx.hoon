/@  message
/@  groupchat
/-  feather-icons
/-  messages
:-  [%groupchat %$ %htmx]
|=  =groupchat
|=  =bowl:neo
^-  manx
;div.p2
  =label  "Chat"
  ;div.ma.fc.g2
    =style  "max-width: 650px;"
    ;div.fc.g2
      =id  "children"
      ;+  (render-messages:messages bowl)
    ==
    ;+  (render-sender:messages [bowl /pub])
  ==
==