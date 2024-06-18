/@  htmx
:-  [%hoon %$ %htmx]
|=  hon=@t
|=  =bowl:neo
=/  =name:neo
  [our here]:bowl
=/  fool=(each file:ford:neo tang)
  %-  mule
  |.
  (scan (trip hon) (rein:ford:neo name))
=/  src=wain
  (to-wain:format hon)
^-  manx
=<  apex
|%
::  XX: non-std
++  post-href
  |=  =post:neo
  ^-  path
  ?>  ?=(@ q.post)
  =/  dsk
    /neo/hawk/src/std
  %+  welp  dsk
  /[p.post]/[q.post]
::
++  apex
  ^-  manx
  ;div.wf.hf.hoon.fc
    ;+  imports
    ;+  contents
    ;+  script
  ==
++  imports
  ;div.frw.g2.p2
    ;*
    ?-    -.fool
        %.n
      :_  ~
      (error p.fool)
    ::
        %.y
      %+  turn  pro.p.fool
      |=  =pro:ford:neo
      ^-  manx
      ;a.p2.br1.b1.hover.s-1
        =href  (spud (post-href %pro stud.pro))
        =hx-target  "closest .hawk"
        =hx-swap  "innerHTML"
        ; {<stud.pro>}
      ==
    ==
  ==
++  error
  |=  =tang
  =/  =wall   (zing (turn tang |=(t=tank (~(win re t) [0 80]))))
  =/  =tape   (zing (join "\0a" wall))
  ;details.wf.br1.bd1(open "")
    =style  "max-height: 220px;"
    ;summary.p2.br1.b-3.f0: error
    ;div.pre.mono.p2.wf.scroll-y
      ;+  ;/  tape
    ==
  ==
++  contents
  ;form.fc.g2.wf.relative.grow.scroll-y
    =style  "border-top: 1px solid var(--b3);"
    =hx-put  "{(en-tape:pith:neo (welp /neo/hawk here.bowl))}?stud=hoon"
    =hx-trigger  "click from:find button, keydown[metaKey&&key=='Enter']"
    =hx-target  "closest .hoon"
    =hx-swap  "morph"
    ;div.relative.grow.fc
      ;div.wfc.z1.absolute
        =style  "top: 15px; right: 15px;"
        ;button.p2.br2.b1.bd1.hover.loader
          ;span.loaded.fr.ac.g1
            ;span.bold: save
            ;span.f2.s-2: cmd+enter
          ==
          ;span.loading: ...
        ==
      ==
      ;div.fr.grow
        ;div.hf.f3.numbered
          =style  "min-width: 10px; padding: 8px 5px; border-right: 1px solid var(--b3);"
          ;*
          %+  turn  (gulf 1 (lent src))
          |=  n=@
          ;div.mono(style "line-height: 1.1;");
        ==
        ;textarea.p2.pre.mono.wf.grow
          =id   "bingo-bango"
          =style  "outline:none; line-height: 1.1; overflow-y: hidden;"
          =autocomplete  "off"
          =rows  "1"
          =spellcheck  "false"
          =name  "text"
          =oninput  "this.setAttribute('value', this.value); skyHoonRenumber(this);"
          =value  (trip hon)
          ;*
          %+  turn  src
          |=  lin=@t
          ;/  "{(trip lin)}\0a"
        ==
      ==
    ==
  ==
++  script
  ;script
    ;+  ;/  %-  trip
    '''
    function skyHoonRenumber(el) {
      //  corrects the line numbers
      let lines = 1 + el.value.split('\n').length;
      let nums = $(el).parent().find('.numbered').get()[0];
      let kids = nums.children.length + 1;
      while (kids < lines) {
        let div = document.createElement('div');
        div.classList.add('mono');
        div.style = 'line-height: 1.1;';
        nums.appendChild(div);
        kids = kids + 1;
      }
      while (kids > lines) {
        nums.children[0].remove();
        kids = kids - 1;
      }
    }
    '''
  ==
--
