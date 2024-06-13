/@  groupchat
/@  ship
/-  feather-icons
/-  messages
:-  [%messenger %$ %htmx]
|=  ~
|=  =bowl:neo
|^
    view
::
++  view
^-  manx
;div.p2.fc.ac.view.g2.ma
  ;style:  {style}
  ;+  make-chat
  ;+  all-chats
==
::
++  pith-tape
  |=  =pith
  ^-  tape
  (en-tape:pith:neo pith)
::
++  style

  ^~
  %-  trip
    '''
    .view {
        max-width: 650px;
        padding-bottom: 50vh;
        padding-top: 30px;
    }
    input[type="text"]:hover {
        cursor: text;
    }
    input:focus {
        outline: none;
    }
    .w70{
        width: 70%;
    }
    .bc1{
        border: solid var(--b2) 1px;
    }
    '''
::
++  make-chat
  =/  oninput
    """
    this.setAttribute("value", this.value); this.nextElementSibling.nextElementSibling.setAttribute('hx-get', '/neo/hawk{(pith-tape here.bowl)}/dms/' + this.value); htmx.process(document.body);
    """
  ;form.fr.jc.g1.w70
    =hx-post  "/neo/hawk{(pith-tape here.bowl)}?stud=messenger-diff"
    =head  "new-dm"
    =hx-swap  "none"
    =hx-on-htmx-after-request  "$(this).find('.redirect').emit('messenger-created')"
    ;input.grow.bc1.p2.br1
      =name  "invites"
      =type  "text"
      =required  ""
      =placeholder  "Start chat (e.g. ~sampel-palnet)"
      =oninput  oninput
      =autocomplete  "off"
      ;
    ==
    ;input.hidden.grow.bc1.p2.br1
      =type  "text"
      =name  "name"
      =placeholder  "chat name"
      =oninput  (trip 'this.setAttribute("value", this.value);')
      ;
    ==
    ;div.redirect.hidden
      =hx-target  "closest .hawk"
      =hx-swap  "outerHTML"
      =hx-trigger  "messenger-created"
      ;
    ==
    ;button.loader.br1.hover.p2.b0.bc1
      ;span.loaded;  >
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
::
++  all-chats
  =/  kids
    %+  skid  ~(tap of:neo kids.bowl)
        |=  [=pith =idea:neo]
        =(p.q.saga.idea %groupchat)
  ^-  manx
  ;div.fc.as.g1.w70
    ;div.fc.ac.g1.wf
    ;*  %+  turn  q.kids
        |=  [=pith =idea:neo]
        ?~  pith
          ;span.hidden:  no dms
        ?:  (lte 3 (lent pith))
          ;h1.hidden:  {<pith>}
        ~&  >  pith
        =/  ship
          ?.  ?=([%p @p] (rear pith))  *@p
          +:;;([%p @p] (rear pith))
        ~&  >  ship
        ^-   manx
        ;a.br1.hover.b0.fr.jb.wf.bc1
        =href  "/neo/hawk{(pith-tape here.bowl)}{(pith-tape pith)}"
        =hx-swap  "innerHTML"
          ;h3.s-1.p2:  {<ship>}
        ==
    ==
    ;div.fc.ac.g1.wf
      ;*  %+  turn  p.kids
          |=  [=pith =idea:neo]
          ?~  pith  ;span.hidden:  no groupchats
          (chat pith idea)
    ==
  ==
::
++  chat
  |=  [=pith =idea:neo]
  =/  org=@p  +:;;([%p @p] (snag 1 `(list iota)`pith))
  =/  members=(list ship)  ~(tap in members:!<(groupchat q.q.saga.idea))
  =/  chat  +:;;([%t @t] (rear pith))
  ~&  >  ['members' members]
  ^-  manx
  ;div.wf.br1.fc.g1
    ;div.fr.g1
      ;a.br1.hover.fr.jb.g2.wf.bc1.b0
      =href  "/neo/hawk{(pith-tape here.bowl)}{(pith-tape pith)}"
      =hx-swap  "innerHTML"
        ;h3.s-1.p2:  {(trip chat)}
        ;h3.s-1.p2:  {<org>}
      ==
      ;button.br1.hover.bc1.b0.hidden
      =onclick  (weld (trip 'this.parentNode.parentNode.classList.toggle("border"); this.parentNode.parentNode.classList.toggle("p2"); this.previousSibling.classList.toggle("border"); this.classList.toggle("border"); ') (span-toggle "v" "^"))
        ;span: v
      ==
    ==
    ;div.fc.hidden
      ;div.fc.g2
        ;div.fr.je.g2
          ;+  ?.  =(our.bowl org)
            ;span.hidden:  ~
          ;button.hover.br1.b0.bc1
            =onclick  (span-toggle "+" "x")
            ;span:  +
          ==
        ==
        ;+  (add-member chat)
      ==
      ;*  %+  turn  `(list ship)`members
          |=  =ship
          ?:  =(our.bowl ship)
            ;div.fr.jb.g1
            ;span.wf.br1.p2:  {<ship>}
            ==
          ;div.fr.jb.g1
          ;span.wf.br1.p1:  {<ship>}
          ;button.hover.b0.br1.loader.bc1
            =hx-post  "/neo/hawk{(pith-tape here.bowl)}?stud=groupchat-diff"
            =hx-target  "find .loading"
            =hx-swap  "outerHTML"
            =head  "remove"
            =ship  (scow %p ship)
            =hx-on--after-request  "this.parentNode.remove();"
            =onmouseover  "this.previousSibling.classList.add('b1');"
            =onmouseout   "this.previousSibling.classList.remove('b1');"
            ;span.loaded:  x
            ;span.loading
              ;+  loading.feather-icons
            ==
          ==
          ==
    ==
  ==
++  add-member
  |=  chat=cord
  ;form.fr.gb.g1.hidden
  =hx-post  "/neo/hawk{(pith-tape here.bowl)}?stud=messenger-diff"
  =head  "invite-to-groupchat"
  =hx-target  "find button .loading"
  =hx-swap  "outerHTML"
    ;input.hidden
    =type  "text"
    =name  "name"
    =value  (trip chat)
    ;
    ==
    ;input.grow.bc1.p2.br1
    =name  "ship"
    =type  "text"
    =placeholder  "~zod, ~bus"
    =oninput  "this.setAttribute('value', this.value);"
    =autocomplete  "off"
    ;
    ==
    ;button.hidden
    =onclick  "this.parentNode.appendChild(document.createElement('input')) "
    ;span:  more
    ==
    ;button.loader.hover.br1.b0.bc1
      ;span.loaded:  +
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
::
++  span-toggle
|=  [from=tape to=tape]
^-  tape
"this.parentNode.nextElementSibling.classList.toggle('hidden'); if (this.parentNode.nextElementSibling.classList.contains(\"hidden\"))\{this.innerHTML=\"<span>{from}</span>\"} else \{this.innerHTML=\"<span>{to}</span>\"}"
--
