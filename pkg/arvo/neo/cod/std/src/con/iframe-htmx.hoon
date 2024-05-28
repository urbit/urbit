/@  iframe
:-  [%iframe %$ %htmx]
|=  =iframe
|=  =bowl:neo
^-  manx
|^  shell
  ::
++  shell
  ::
  =/  url  (trip iframe)
  ;div.wf.hf.fc
    =label  "iframe"
    =here  (en-tape:pith:neo here.bowl)
    ;form.fr
      =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=iframe"
      =hx-swap  "none"
      =style  "border-bottom: 1px solid #777;"
      ;input.p2.grow
        =type  "text"
        =name  "text"
        =value  url
        =is  "atom-input"
        =oninput  "this.parentNode.nextElementSibling.src = this.value;"
        ;
      ==
      ;button.p2.b1
        ; Save
      ==
    ==
    ;iframe.wf.hf.bd0
      =style  "background-color: #eee;"
      =id  "iframe-{url}"
      =src  url
      ;
    ==
  ==
--
