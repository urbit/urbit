/@  task
/-  feather-icons
:-  [%task %$ %htmx]
|=  t=task
|=  =bowl:neo
|^
  shell
++  task-title
  ^-  manx
  ;div.fc.g2.br1
    ;h2.bold.s2.tc
      ; {(trip (@t text.t))}
    ==
  ==
++  form-create
  =/  placeholder  ?:(?=([%ud @ud] (rear here.bowl)) "subtask" "task")
  ^-  manx
  ;div.fc.g1.p4
    ;form.fr.g1
      =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=task-diff"
      =head  "new"  
      =hx-swap  "outerHTML"
      =hx-target  "find button .loading"
      ;input.hidden
        =name  "prepend"
        =value  "no"
        ;
      ==
      ;input.wf.p2.border.br2.grow
        =name  "text"
        =autocomplete  "off"
        =type  "text"
        =required  ""
        =placeholder  placeholder
        =oninput  "this.setAttribute('value', this.value);"
        ;
      ==
      ;button.b1.br1.hover.p1.wfc.loader
        ;span.loaded: create
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  ==
++  part-kid
  |=  [=pith =idea:neo]
  =/  =pail:neo  pail.idea
  =/  t=task  !<(task q.pail)
  =/  pith-tape  (en-tape:pith:neo (welp here.bowl pith))
  =-  ?.  done.t  -
    -(a.g [[%done ""] a.g.-])
  ^-  manx
  ;div.fc.g1
    =here  pith-tape
    ;div.fr.g1
      ;div.fr.ac.g1.grow
        =hx-post  "/neo/hawk{pith-tape}?stud=task-diff"
        =hx-trigger  "input changed delay:0.4s from:find .text, input from:find .done"
        =hx-swap  "none"
        =head  "edit"
        ;+
          =/  classnames  "p2 br1 border done s3"
          =;  m
              =/  class  class/classnames
              =/  class-hidden  [%class (weld "hidden " classnames)]
            ?.  done.t  
            ::not done; has kids?
              ?~  order.t
                m(a.g [class a.g.m])
              m(a.g [class-hidden a.g.m])
            ::  done; has kids ?
            ?~  order.t
              ::  no kids
              m(a.g [[%checked ""] class a.g.m])
            ::  has kids and done
            m(a.g [[%checked ""] class-hidden a.g.m])
          ^-  manx
          ;input
            =type  "checkbox"
            =name  "done"
            =onclick  (trip 'if (this.checked) { this.setAttribute("checked", "");$(this.nextElementSibling).addClass("strike f3")} else {this.removeAttribute("checked");$(this.nextElementSibling).removeClass("strike f3")}')
            ;
          ==
        ::
        ;+  =-
          =/  that  -
          =/  classes
            %+  weld
              "grow p2 br2 text bold"
            ?:(done.t " strike f3" "")
          that(a.g [[%class classes] a.g.that])
        ^-  manx
        ;input
          =type  "text"
          =name  "text"
          =value  (trip text.t)
          =onclick  "$(this).addClass('border');$(this).removeClass('bold')"
          =onblur  "$(this).addClass('bold');$(this).removeClass('border')"
          =oninput  "this.setAttribute('value', this.value);"
          ;
        ==
      ==
      ;+  =-
        =/  that  -
        =/  classes
            "b0 br1 hover p1 tl action mono fr g3"
        that(a.g [[%class classes] a.g.that])
      ^-  manx
      ;button
        =type  "button"
        =onclick  (trip 'this.classList.toggle("toggled"); this.parentNode.nextElementSibling.classList.toggle("hidden"); if (this.parentNode.nextElementSibling.classList.contains("hidden")){this.innerHTML="<span class=\'p1\'>V</span>"} else {this.innerHTML="<span class=\'p1\'>⋀</span>"};')
        ;span.p1: V
      ==
      ;a.loader.b0.br1.hover.p1.tl.actio.mono.fr.g3
        =hx-indicator  "this"
        =href  "/neo/hawk{(en-tape:pith:neo here.bowl)}{(en-tape:pith:neo pith)}"
        =hx-swap  "innerHTML"
        ;span.p1.loaded: →
        ;span.p1.loading
          ;+  loading.feather-icons
        ==
      ==  
    ==     
    ;+  (dropdown-menu pith)
  ==
++  dropdown-menu
  |=  =pith
  ^-  manx
  ;div.p2.br1.fr.g2.hidden
    =hx-disinherit  "hx-indicator"
    =style  "margin-left: 20px;"
    ;button.b1.br1.p2.hover.hfc
      =onclick  "this.parentNode.parentNode.parentNode?.insertAdjacentElement('beforeend', this.parentNode.parentNode); center(this);"
      ; ↧
    ==
    ;button.b1.br1.p2.hover.hfc
      =onclick  "this.parentNode.parentNode.nextElementSibling?.insertAdjacentElement('afterend', this.parentNode.parentNode);  center(this);"
      ; ↓
    ==
    ;button.b1.br1.p2.hover.hfc
      =onclick  "this.parentNode.parentNode.previousElementSibling?.insertAdjacentElement('beforebegin', this.parentNode.parentNode); center(this);"
      ; ↑
    ==
    ;button.b1.br1.p2.hover.hfc
      =onclick  "this.parentNode.parentNode.parentNode?.insertAdjacentElement('afterbegin', this.parentNode.parentNode);  center(this);"
      ; ↥
    ==
    ;div.basis-full.hidden;
    ;button.b1.br1.p2.hover.loader.hfc
      =type  "button"
      =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=task-diff"
      =hx-target  "find .loading"
      =hx-swap  "outerHTML"
      =hx-on--after-request  "this.parentNode.parentNode.remove();"
      =head  "oust"
      =pith  (en-tape:pith:neo pith)
      ;span.loaded: delete
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
    ;div.htmx-indicator.reorder-indicator.p2.f2.hfc
      ; ---
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
      =/  kid  (~(get of:neo kids.bowl) pith)
      ?~  kid
        ;div: does not exist {(en-tape:pith:neo pith)}
      (part-kid [pith (need kid)])
  ==
++  shell
  ;div.shell.fc.js.af.p2.wf.p1.g5.ma
    =here  (en-tape:pith:neo here.bowl)
    ;style
    ;+  ;/  %-  trip
    '''
    .shell {
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
    input:checked {
      outline: none;
      accent-color: black;  
    }
    '''
    ==
    ;+  script
    ;+  task-title
    ;div.fc.g1.kids
      ;+  form-ordered-kids
      ;+  form-create
    ==
  ==
--
