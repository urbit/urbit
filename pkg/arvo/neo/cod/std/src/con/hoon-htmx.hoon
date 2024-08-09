/@  htmx
/-  feather-icons
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
    /hawk/src/std
  %+  welp  dsk
  /[p.post]/[q.post]
::
++  apex
  ^-  manx
  ;form.wf.hf.hoon.fc
    =hx-put  "{(en-tape:pith:neo (welp /hawk here.bowl))}?stud=hoon"
    =hx-trigger  "click from:find button, keydown[metaKey&&key=='Enter']"
    =hx-target  "closest .hoon"
    =hx-swap  "morph"
    ;+  imports
    ;+  contents
    ;+  script
  ==
++  imports
  ;div.frw.jb.ac.g2.p1
    ;+
    ?-    -.fool
        %.n
      (error p.fool)
    ::
        %.y
      ~&  >>>  far-fool/far.p.fool
      ~&  >>>  fal-fool/fal.p.fool
      ;div.frw.g2.p1.wfc
        ;*
        %+  turn  pro.p.fool
        |=  =pro:ford:neo
        ^-  manx
        ;a.p2.br1.bd1.b1.hover.s-1
          =href  (spud (post-href %pro stud.pro))
          =hx-target  "closest .hawk"
          =hx-swap  "innerHTML"
          ; {<stud.pro>}
        ==
      ==
    ==
    ;+  saver
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
++  saver
  ;button.p2.br2.b1.bd1.hover.loader
    ;span.loaded.fr.ac.g1
      ;span.bold: save
      ;span.f2.s-2: cmd+enter
    ==
    ;span.loading
      ;+  loading.feather-icons
    ==
  ==
++  contents
  ;div.fc.g2.wf.relative.grow.scroll-y
    =style  "border-top: 1px solid var(--b3); padding-bottom: 250px;"
    ;div.fr.grow
      ;div.hf.f3.numbered
        =style  "min-width: 10px; padding: 8px 5px; border-right: 1px solid var(--b3);"
        ;*
        %+  turn  (gulf 1 (lent src))
        |=  n=@
        ;div.mono(style "line-height: 1.1;");
      ==
      ;textarea.p2.pre.mono.wf.grow.scroll-hidden
        =style  "outline:none; line-height: 1.1;"
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
