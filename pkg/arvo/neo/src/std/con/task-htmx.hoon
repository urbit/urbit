/@  task
:-  [%task %htmx]
|=  t=task
|=  =bowl:neo
|^
  shell
++  kids  ~(tap by kids.bowl)
++  id
  ^-  tape
  %-  zing
  %+  turn  (pout here.bowl)
  |=  smeg=@ta
  %+  weld  "--"
  (trip smeg)
++  form-edit
  ^-  manx
  ;form.fc.g2.br1
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=task-diff"
    =hx-trigger  "input changed delay:0.4s from:find textarea, input from:find input"
    =hx-swap  "none"
    =head  "edit"
    ;label.fr.g2.ac.js
      ;+  =-  ?.  done.t:+  -
        -(a.g [[%checked ""] a.g.-])
      ^-  manx
      ;input
        =type  "checkbox"
        =name  "done"
        =onclick  (trip 'if (this.checked) { this.setAttribute("checked", "")} else {this.removeAttribute("checked")}')
        ;
      ==
      ;span.grow: Done
      ;div.htmx-indicator
        ; ---
      ==
    ==
    ;textarea.wf.p2.border.br1.ma.mono
      =name  "text"
      =autocomplete  "off"
      =spellcheck  "false"
      =rows  "{<(add 2 (lent (fand ~[10] (trip text.t))))>}"
      =oninput  "this.setAttribute('value', this.value); this.rows = this.value.split('\\n').length"
      =value  (trip (@t text.t))
      ; {(trip (@t text.t))}
    ==
  ==
++  form-create
  |=  [head=@tas label=tape]
  ^-  manx
  ;div.fc.g1
    ;button.b0.br1.hover.p2.wfc.mono.f3
      =onclick
        """
        this.classList.toggle('toggled');
        this.nextElementSibling.classList.toggle('hidden');
        this.nextElementSibling.firstChild.focus();
        """
      ; {label}
    ==
    ;form.fr.g1.hidden
      =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=task-diff"
      =head  (trip head)
      =hx-swap  "outerHTML"
      =hx-target  "find button .loading"
      ;input.wf.p1.border.br1.grow
        =name  "name"
        =autocomplete  "off"
        =type  "text"
        =pattern  (trip '[a-z]{1}[a-z0-9\\-]+')
        =title  "lowercase and heps"
        =required  ""
        =placeholder  "name"
        =oninput  "this.setAttribute('value', this.value);"
        ;
      ==
      ;button.b1.br1.hover.p1.wfc.loader
        ;span.loaded: create
        ;span.loading: ---
      ==
    ==
  ==
++  part-kid
  |=  [=pith =pail:neo]
  =/  t=task  !<(task q.pail)
  =-  ?.  done.t  -
    -(a.g [[%done ""] a.g.-])
  ^-  manx
  ;div.fc.g1
     =here  (en-tape:pith:neo (welp here.bowl pith))
    ;div.fr.g1
      ;button
        =class  "b0 br1 hover p1 tl action mono fr g3 f2"
        =style  "padding: 4px 9px;"
        =type  "button"
        =onclick
          """
          this.textContent = this.textContent === "=" ? "|" : "="; toggleChildren(this);
          """
        ;span.bold: =
      ==
      ;+  =-
        =/  that  -
        =/  classes
          %+  weld
            "b0 br1 hover p1 grow tl action mono fr g3"
          ?:(done.t " strike f3" "")
        that(a.g [[%class classes] a.g.that])
      ^-  manx
      ;button
        =type  "button"
        =onclick
          """
          this.classList.toggle('toggled'); this.parentNode.nextElementSibling.classList.toggle('hidden');
          """
        ;span.bold: {(trip -:(pout pith))}
        ;span.break.f2: {(scag (fall (find [10 ~] (trip text.t)) 30) (trip text.t))}
      ==
      ;a
        =class  "b0 br1 hover p1 loader f3"
        =hx-indicator  "this"
        =href  "/neo/hawk{(en-tape:pith:neo here.bowl)}{(en-tape:pith:neo pith)}"
        =hx-swap  "innerHTML"
        ;span.loaded: →
        ;span.loading: .
      ==
    ==
    ;div.border.p2.br1.frw.g2.hidden
      =hx-disinherit  "hx-indicator"
      =style  "margin-left: 20px;"
      ;button.b1.br1.p2.hover
        =onclick  "this.parentNode.parentNode.parentNode?.insertAdjacentElement('beforeend', this.parentNode.parentNode); center(this);"
        ; ↧
      ==
      ;button.b1.br1.p2.hover
        =onclick  "this.parentNode.parentNode.nextElementSibling?.insertAdjacentElement('afterend', this.parentNode.parentNode);  center(this);"
        ; ↓
      ==
      ;button.b1.br1.p2.hover
        =onclick  "this.parentNode.parentNode.previousElementSibling?.insertAdjacentElement('beforebegin', this.parentNode.parentNode); center(this);"
        ; ↑
      ==
      ;button.b1.br1.p2.hover
        =onclick  "this.parentNode.parentNode.parentNode?.insertAdjacentElement('afterbegin', this.parentNode.parentNode);  center(this);"
        ; ↥
      ==
      ;div.htmx-indicator.reorder-indicator.p2.f2
        ; ---
      ==
      ;div.basis-full;
      ;button.b1.br1.p2.hover.loader
        =type  "button"
        =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=task-diff"
        =hx-swap  "outerHTML"
        =hx-target  "find .loading"
        =head  "kid-done"
        =pith  (en-tape:pith:neo pith)
        ;span.loaded: toggle
        ;span.loading: ---
      ==
      ;button.b1.br1.p2.hover.loader
        =type  "button"
        =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=task-diff"
        =hx-swap  "none"
        =hx-on--after-request  "this.parentNode.parentNode.remove();"
        =head  "oust"
        =pith  (en-tape:pith:neo pith)
        ;span.loaded: delete
        ;span.loading: ---
      ==
      ;button.b1.br1.p2.hover
        =type  "button"
        =onclick  "this.nextElementSibling.classList.toggle('hidden'); this.classList.toggle('toggled');"
        ; become
      ==
      ;div.basis-full.hidden.fr.g1
        =hx-post  "/neo/hawk{(en-tape:pith:neo (welp here.bowl pith))}?stud=task-diff"
        =hx-trigger  "become"
        =hx-target  "find .loading"
        =hx-swap  "outerHTML"
        =head  "become"
        ;input.grow.p2.br1.border
          =type  "text"
          =name  "path"
          =value  (en-tape:pith:neo (welp here.bowl pith))
          =oninput  "this.setAttribute('value', this.value);"
          ;
        ==
        ;button.p2.b1.hover.br1.loader
          =type  "button"
          =onclick  "this.dispatchEvent(new CustomEvent('become', \{composed:true, bubbles: true}))"
          ;span.loaded: become
          ;span.loading: ---
        ==
      ==
      ;div.basis-full.p2.pre.mono.scroll-x
        ; {(trip text.t)}
      ==
    ==
    ;div.fc.g2.hidden
      =hx-disinherit  "hx-indicator"
      =style  "margin-left: 20px;"
      ;
    ==
  ==
  ::
++  script
  ;script
    ;+  ;/  %-  trip
    '''
    function center(el) {
      el.scrollIntoView({
        block: "center",
        inline: "start",
        behavior: "instant"
      })
    }
    function toggleChildren(el) {
      let kidsDiv = el.parentNode.nextElementSibling.nextElementSibling;
      if (!kidsDiv.children.length) {
        let here = el.parentNode.parentNode.getAttribute('here');
        let stub = document.createElement("div");
        stub.setAttribute("hx-get", `/neo/hawk${here}`);
        stub.setAttribute("hx-trigger", 'load');
        stub.setAttribute("hx-target", 'this');
        stub.setAttribute("hx-select", '.kids');
        stub.textContent = "+ + +"
        stub.className = "fc as jc"
        stub.style = "padding:10px; padding-left: 20px;"
        stub.setAttribute("hx-swap", "outerHTML");
        kidsDiv.appendChild(stub);
        htmx.process(kidsDiv);
      }
      kidsDiv.classList.toggle('hidden');
    }
    '''
  ==
++  form-ordered-kids
  ;form.fc.g1
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=task-diff"
    =head  "reorder"
    =hx-indicator  ".reorder-indicator"
    =hx-swap  "none"
    ;*
    %+  turn  order.t
      |=  =pith
      =/  kid  (~(get by kids.bowl) pith)
      ?~  kid
        ;div: does not exist {(en-tape:pith:neo pith)}
      (part-kid [pith (need kid)])
    ::;*
    ::=/  orphans
    ::  %+  skim  kids
    ::  |=  [=pith *]
    ::  =(~ (find [pith ~] order.t))
    ::%+  turn
    ::  %+  sort  orphans
    ::  |=  [a=[=pith *] b=[=pith *]]
    ::  (lth ->.pith.a ->.pith.b)
    ::part-kid
  ==
++  shell
  ;div.fc.js.af.p2.wf.p1.g5.ma
    =here  (en-tape:pith:neo here.bowl)
    =style  "max-width: 650px; padding-bottom: 50vh; padding-top: 30px;"
    ;+  script
    ;+  form-edit
    ;div.fc.g1.kids
      ::;+  ?~  (lent order.t)  ;/("")  (form-create %prep "+")
      ::;+  ?~  (lent order.t)  (form-create %nest "+")  ;/("")
      ;+  form-ordered-kids
      ::;+  ?~  (lent order.t)  ;/("")  (form-create %nest "+")
      ;+  (form-create %nest "+")
    ==
  ==
--
