/@  message
/-  feather-icons
/-  messages
:-  [%ship %$ %htmx]
|=  =ship
|=  =bowl:neo
^-  manx
;div.p2.wf.hf
  =hx-on-message-sent  "$(this).find('.refresher').emit('refresh');"
  ;div.mw-page.ma.wf.hf
    =style
      """
      display: grid;
      grid-template-rows: 1fr auto;
      grid-template-columns: auto;
      grid-template-areas:
        "messages"
        "sender"
      """
    ;+  script:messages
    ;+  style:messages
    ;+  (render-messages:messages bowl)
    ;+  (render-sender:messages [bowl /pub])
  ==
==
