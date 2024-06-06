/@  task     ::  [text=cord done=? order=(list pith)]
:: import /lib/feather-icons
/-  feather-icons
:: declare that this is a conversion from task to HTMX
:-  [%task %$ %htmx]
::  gate takes a task and a bowl:neo,
::  so we can access here.bowl and kids.bowl
|=  t=task
|=  =bowl:neo
:: construct Sail using helper arms
:: calls shell arm
|^
  shell
::
++  pith-tape
  ::  pith to tape conversion
  |=  =pith 
  ^-  tape
  (en-tape:pith:neo pith)
::
++  shell
  ::  shell arm contains task manx
  ::  think about it as a body-tag
  ::  <div class="shell fc js af p2 wf p1 g5 ma" here=(pith-tape here.bowl)> 
  ;div.shell.fc.js.af.p2.wf.p1.g5.ma
    =here  (pith-tape here.bowl)
    ::  one of the ways to apply arbitrary CSS
    ;style:  {style}
    ::  script will embed a bit of JavaScript code for a more responsive frontend experience 
    ;+  script
    ::  task-title arm is displaying parent task title
    ;+  task-title
    ::  <div class="fc g1 kids">
    ;div.fc.g1.kids
      ::  maps through kids task in the order of list of piths(ordered tasks)
      ;+  form-ordered-kids
      ::  sends request to make a new kid task 
      ;+  form-create
    ==  ::  </div>
  ==  ::  </div>
::
++  style 
  ::  CSS arm 
  ^~
  ::  turning fenced cord block into tape
  %-  trip
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
::
++  script
  ::  <script>
  ;script
    ::  turning fenced cord block into tape
    ::  function center(el), applies scrollIntoView() methos to provided element
    ::  so the element is visible to the user as specified below
    ::  block: "center" - vertical alignment: center
    ::  inline: "start" - horizontal alignment: start
    ::  behavior: "instant" - scrolling is instant
    ;+  ;/  %-  trip
    '''
    function center(el) {
      el.scrollIntoView({
        block: "center",
        inline: "start",
        behavior: "instant"
      })
    }
    '''
  ==  ::</script>
::
++  task-title
::  displays task's title
  ^-  manx
  :: <div class="fc g2 br1">
  ;div.fc.g2.br1
    ::  <h2 class="bold.s2.tc">{(trip (@t text.t))}
    ;h2.bold.s2.tc
      ; {(trip (@t text.t))}
    ==  ::</h2>
  ==  ::</div>
::
++  form-ordered-kids
  ::  form that keeps track of tasks order 
  ::  sends %reorder poke if order been ajusted
  ;form.fc.g1
    =hx-post  "/neo/hawk{(pith-tape here.bowl)}?stud=task-diff"
    =head  "reorder"
    =hx-indicator  ".reorder-indicator"
    =hx-swap  "none"
    ;*
    ::  iterates over the list of pith (maps through it)
    %+  turn  order.t
      |=  =pith
      ::  extract kid inforamtion at pith from kids.bowl
      ::  and calls part-kid arm with pith and kid data
      =/  kid  (~(get of:neo kids.bowl) pith)
      ?~  kid  
        ;div: does not exist {(pith-tape pith)}  
      (part-kid [pith (need kid)])
  ==
::
++  part-kid
::  part-kid arm maps through kid data
::  sends %edit poke on input change
  |=  [=pith =idea:neo]
  ::  extracts information from idea:neo to task
  =/  =pail:neo  pail.idea
  =/  t=task  !<(task q.pail)
  ::  converts pith to tape
  =/  pt  (pith-tape (welp here.bowl pith))
  ::  checks if task is done
  ::  if it's done assigns attribute done to manx below
  =-  ?.  done.t  -
    -(a.g [[%done ""] a.g.-])
  ^-  manx
  ::  <div class="fc g1" here="pt" done="">
  ;div.fc.g1
    =here  pt
    ::  <div class="fr g1" done="">
    ;div.fr.g1  
      ::  div with %edit poke functionality
      ::  sends POST request when input with class="text" 
      ::  or input with class "done" are being changed
      ;div.fr.ac.g1.grow
        =hx-post  "/neo/hawk{pt}?stud=task-diff"
        =hx-trigger  "input changed delay:0.4s from:find .text, input from:find .done"
        =hx-swap  "none"
        =head  "edit"
        ;+
          =/  classnames  "p2 br1 border done s3"
          ::  Below is a checkbox logic:
          ::  if task is toggled checkbox will appear as checked 
          ::  if task have kids input will be hidden,
          ::  even tho we have logic for handling toggle of parent task
          ::  for better user experience we prevent toggling task if it has kids
          ::
          ::  combining m named noun and attribute logic with manx below 
          =-  =/  m  -
              ::  predefining class="class name" attribute for input
              =/  class  class/classnames
              =/  class-hidden  [%class (weld "hidden " classnames)]
            ::  checks if task toggled as done
            ?.  done.t  
              ::  not done, has kids?
              ?~  order.t
                ::  a.g.m is a part of the manx that contains input checkbox attributes
                ::  we assign class attribute to the rest of it's data
                m(a.g [class a.g.m])
              m(a.g [class-hidden a.g.m])
            ::  done; has kids ?
            ?~  order.t
              ::  assigning checked and class attribute to the rest of manx data
              m(a.g [[%checked ""] class a.g.m])
            m(a.g [[%checked ""] class-hidden a.g.m])
          ^-  manx
          ::  onclick logic depending if input has been checked or unchecked
          ::  if checked adds attribute checked="" to local input
          ::  and assigns classes "strike f3" to sibling input text
          ::  if it's been undone removes attribute checked=""
          ::  and removes "strike f3" classes from sibling input
          ;input
            =type  "checkbox"
            =name  "done"
            =onclick  (trip 'if (this.checked) { this.setAttribute("checked", "");$(this.nextElementSibling).addClass("strike f3")} else {this.removeAttribute("checked");$(this.nextElementSibling).removeClass("strike f3")}')
            ;
          ==
        ::  combining that named noun and class name logic with manx below 
        ::  and make it in to an XML node
        ;+  =-
          =/  that  -
          =/  classes
            %+  weld
              "grow p2 br2 text bold"
            ?:(done.t " strike f3" "")
          ::  assigning classes attribute to the manx below
          that(a.g [[%class classes] a.g.that])
        ^-  manx
        ::  input below has a task text value 
        ::  on input change it changes value attribute to update div POST request form
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
      ;+  
      ^-  manx
      ::  drop down menu button with logic for it's span change
      ::  and assigns classes to the manx of dropdown-menu arm
      ;button.b0.br1.hover.p1.tl.action.mono.fr.g3
        =type  "button"
        =onclick  (trip 'this.classList.toggle("toggled"); this.parentNode.nextElementSibling.classList.toggle("hidden"); if (this.parentNode.nextElementSibling.classList.contains("hidden")){this.innerHTML="<span class=\'p1\'>V</span>"} else {this.innerHTML="<span class=\'p1\'>⋀</span>"};')
        ;span.p1: V
      ==
      ::  a-tag, opens subtask view with spinner logic on loading 
      ;a.loader.b0.br1.hover.p1.tl.actio.mono.fr.g3
        =hx-indicator  "this"
        =href  "/neo/hawk{(pith-tape here.bowl)}{(pith-tape pith)}"
        =hx-swap  "innerHTML"
        ;span.p1.loaded: →
        ;span.p1.loading
          ;+  loading.feather-icons
        ==
      ==  
    == 
    ::  drop down menu arm called with pith 
    ;+  (dropdown-menu pith)
  ==
::
++  dropdown-menu
  ::  dropdown-menu arm provides reorder and oust(delete) kid logic 
  |=  =pith
  ^-  manx
  ::  
  ;div.p2.br1.fr.g2.hidden
    =hx-disinherit  "hx-indicator"
    =style  "margin-left: 20px;"
    ::  moves whole kid div to the end of the task list 
    ::  and uses script logic to center user view on moved task 
    ;button.b1.br1.p2.hover.hfc
      =onclick  "this.parentNode.parentNode.parentNode?.insertAdjacentElement('beforeend', this.parentNode.parentNode); center(this);"
      ; ↧
    ==
    ::  moves kid div one task down and centers view 
    ;button.b1.br1.p2.hover.hfc
      =onclick  "this.parentNode.parentNode.nextElementSibling?.insertAdjacentElement('afterend', this.parentNode.parentNode);  center(this);"
      ; ↓
    ==
    ::  moves kid div one task up and centers view 
    ;button.b1.br1.p2.hover.hfc
      =onclick  "this.parentNode.parentNode.previousElementSibling?.insertAdjacentElement('beforebegin', this.parentNode.parentNode); center(this);"
      ; ↑
    ==
    ::  moves kid div to the beggining of the task list and centers view
    ;button.b1.br1.p2.hover.hfc
      =onclick  "this.parentNode.parentNode.parentNode?.insertAdjacentElement('afterbegin', this.parentNode.parentNode);  center(this);"
      ; ↥
    ==
    ;div.basis-full.hidden;
    ::  delete button that sends POST request with %oust poke to parent task
    ::  and after request been sent removes subtask div from DOM
    ;button.b1.br1.p2.hover.loader.hfc
      =type  "button"
      =hx-post  "/neo/hawk{(pith-tape here.bowl)}?stud=task-diff"
      =hx-target  "find .loading"
      =hx-swap  "outerHTML"
      =hx-on--after-request  "this.parentNode.parentNode.remove();"
      =head  "oust"
      =pith  (pith-tape pith)
      ;span.loaded: delete
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
    ::  indicator that %reorder POST request is in progress 
    ;div.reorder-indicator.p2.loader
      ;span.loading.p1
        ;+  loading.feather-icons
      ==
    ==
  ==
::
++  form-create
  ::  form-create arm send POST request with data for %new poke
  ::  depending on whether it's a parent task or a kid, it specifies a placeholder accordingly
  =/  placeholder  ?:(?=([%ud @ud] (rear here.bowl)) "subtask" "task")
  ^-  manx
  ;div.fc.g1.p4r
  =style  "padding-top:8px;"
    ::  form for %new poke POST request 
    ;form.fr.g1
      =hx-post  "/neo/hawk{(pith-tape here.bowl)}?stud=task-diff"
      =head  "new"  
      =hx-swap  "outerHTML"
      =hx-target  "find button .loading"
      ::  by default will append new task to the ordered kid list
      ;input.hidden
        =name  "prepend"
        =value  "no"
        ;
      ==
      ::  input for task text data
      ;input.wf.p2.border.br2.grow
        =name  "text"
        =autocomplete  "off"
        =type  "text"
        =required  ""
        =placeholder  placeholder
        =oninput  "this.setAttribute('value', this.value);"
        ;
      ==
      ::  button that triggers form to send request 
      ;button.b1.br1.hover.p1.wfc.loader
        ;span.loaded: create
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  ==
--
