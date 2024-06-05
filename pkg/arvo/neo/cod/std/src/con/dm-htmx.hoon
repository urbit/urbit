/@  message
/-  feather-icons
/-  messages
:-  [%ship %$ %htmx]
|=  =ship
|=  =bowl:neo
^-  manx
::~&  >  bowl  :: this sigpam results in endless http spinning
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