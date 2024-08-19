/@  task     ::  [text=cord done=? kids-done=? order=(list pith)]
:: import /lib/feather-icons
/-  feather-icons
:: declare that this is a conversion from task to HTMX
:-  [%task %$ %htmx]
::  outer gate takes a task, inner gate takes a bowl:neo,
::  so we can access here.bowl and kids.bowl in the ui
|=  t=task
|=  =bowl:neo
::
::  in this case, all sail rendering
::  happens in helper arms
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
  ::
  ::  this "shell" div is a wrapper for the whole interface
  ::  <div class="shell fc js af p2 wf p1 g5 ma" here="{<(pith-tape here.bowl)>}">
  ;div.shell.fc.js.af.p2.wf.p1.g5.ma
    =here  (pith-tape here.bowl)
    ::
    ::  return a <style> element with
    ::  the styling in the +css arm
    ;style:  {style}
    ::
    ::  embed JavaScript code from +script for
    ::  a more responsive frontend experience
    ;+  script
    ::
    ::  render from the +task-title arm, which
    ::  returns a header with the task's title
    ;+  task-title
    ::  <div class="fc g1 kids">
    ;div.fc.g1.kids
      ::
      ::  render a <form> element with an ordered list of
      ::  the child shrubs, which are all the top-level tasks
      ;+  form-ordered-kids
      ::  render the <form> that sends a
      ::  poke to create a new task
      ;+  form-create
    ==  :: </div>
  ==  :: </div>
::
++  style
  ^~
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
    ;+  ;/  %-  trip
    '''
    //  applies scrollIntoView() methods to the provided
    //  to display it as specified
    function center(el) {
      el.scrollIntoView({
        block: "center",
        inline: "start",
        behavior: "instant"
      })
    };

    //  tell the user why a clicked checkbox
    //  can't be marked as checked
    document.querySelectorAll(".alert").forEach(function(element) {
      element.addEventListener('click', function(e) {
        if (element.hasAttribute("readonly")){
        e.preventDefault();
        alert("Subtasks are not completed");
          }
        })
      })
    '''
  ==  :: </script>
::
++  task-title
  ^-  manx
  :: <div class="fc g2 br1">
  ;div.fc.g2.br1
    :: <h2 class="bold.s2.tc">
    ;h2.bold.s2.tc
      ; {(trip (@t text.t))}
    ==  :: </h2>
  ==  :: </div>
::
++  kids-check
  ::
  ::  check if all subtasks are completed
  |=  =pith
  ~&  >  pith
  ^-  ?
  =/  t
    !<  task
    q.q.saga:(~(got of:neo kids.bowl) pith)
  ~&  >>  t
  kids-done.t
::
::
++  form-ordered-kids
  ::
  ::  <form> that keeps track of tasks order, sends %reorder
  ::  poke if tasks are manually reordered by the user
  ;form.fc.g1
    =hx-post       "/hawk{(pith-tape here.bowl)}?stud=task-diff"
    =head          "reorder"
    =hx-indicator  ".reorder-indicator"
    =hx-swap       "none"
    ;*
    ::
    ::  iterates over the list of piths in order.task
    %+  turn
      order.t
    |=  =pith
    ::  extract kid information at pith from kids.bowl
    ::  and runs +part-kid on pith and kid data
    =/  kid  (~(get of:neo kids.bowl) pith)
    ?~  kid
      ;div: does not exist {(pith-tape pith)}
    (part-kid [pith (need kid)])
  ==
::
++  part-kid
  ::
  ::  sends %edit poke on input change
  |=  [=pith =idea:neo]
  ::  extracts information from idea:neo to task
  =/  =pail:neo  q.saga.idea
  =/  t=task  !<(task q.pail)
  ::  converts pith to tape
  =/  pt  (pith-tape (welp here.bowl pith))
  ::  checks if task is done; if so, assigns
  ::  attribute "done" to the manx created below
  =-  ?.  done.t  -
    -(a.g [[%done ""] a.g.-])
  ^-  manx
  ::  <div class="fc g1" here="{<pt>}" done="" ...>
  ::  toggle hidden attribute on buttons-menu div
  ;div.fr.g1.p1
    =here  pt
    =onmouseover  "this.childNodes[1].classList.remove('hidden');$(this).addClass('b1 br2');"
    =onmouseout   "this.childNodes[1].classList.add('hidden');$(this).removeClass('b1 br2');"
      ::
      ::  div with %edit poke functionality
      ::  div that sends %edit poke when input with class="text"
      ::  or input with class "done" are being changed
      ;div.fr.ac.g1.grow
        =hx-post     "/hawk{pt}?stud=task-diff"
        =hx-trigger  "input changed delay:0.4s from:find .text, input from:find .done"
        =hx-swap     "none"
        =head        "edit"
          ;+
          ::  defines class attribute with class names
          =/  class  [%class "p2 br1 border done s3"]
          =/  class-alert  [%class "p2 br1 border done s3 alert"]
          ::
          ::  checkbox logic:
          ::  - if task is toggled, checkbox will
          ::    appear as checked
          ::  - if task has kids and all kids are done,
          ::    user will be free to toggle the task
          ::  - if task have kids and they are not done,
          ::    checkbox will have readonly attribute and
          ::    will show alert onclick
          ::  - even though we have logic for handling
          ::    toggling of a parent task, we prevent
          ::    a task from being marked as done if it
          ::    has untoggled kids
          ::
          ::  combining attribute logic with manx below
          =;  m
            ::  checks if the task toggled as done
            ?.  done.t
              ::  if it's not done, does it have kids?
              ?~  order.t
                ::
                ::  a.g.m is a part of the manx that
                ::  contains input checkbox attributes;
                ::  we assign the class attribute to
                ::  the rest of its data
                m(a.g [class a.g.m])
              =/  kc
                (kids-check pith)
              ~&  >>>  kc
              ?:  kc
                ::  assigning class attribute to
                ::  the rest of manx data
                m(a.g [class a.g.m])
              ::
              ::  assigns readonly and class
              ::  attributes to checkbox; 'alert' class will trigger
              ::  alert script functionality
              m(a.g [[%readonly ""] class-alert a.g.m])
            ::
            ::  assigning checked and class attributes
            ::  to the rest of manx data
            m(a.g [[%checked ""] class a.g.m])
          ^-  manx
          ::
          ::  onclick logic depending if input
          ::  has been checked or unchecked:
          ::  - if checked and doesn't have attribute
          ::    readonly, adds attribute checked="" to
          ::    local input and assigns classes "strike f3"
          ::    to sibling input text
          ::  - if it's been undone removes attribute checked=""
          ::    and removes "strike f3" classes from sibling input
          ::
            ;input
              =type     "checkbox"
              =name     "done"
              =onclick  (trip 'if (this.checked && !this.hasAttribute("readonly")){ this.setAttribute("checked", "");$(this.nextElementSibling).addClass("strike f3")} else {this.removeAttribute("checked");$(this.nextElementSibling).removeClass("strike f3")}')
              ;
            ==
        ::
        ::  combining class logic with
        ::  manx below and make it in to an XML node
        ;+  =;  that
          =/  classes
            %+  weld
              "grow p2 br2 text bold"
            ?:(done.t " strike f3" "")
          ::  assigning classes attribute to the manx below
          that(a.g [[%class classes] a.g.that])
        ^-  manx
        ::
        ::  input has a task text value
        ::  on input change, it will change the value
        ::  attribute to update the div's POST request form
        ;input
          =type         "text"
          =name         "text"
          =value        (trip text.t)
          =onclick      "$(this).addClass('border br2');$(this).removeClass('bold')"
          =onblur       "$(this).addClass('bold');$(this).removeClass('border')"
          =oninput      "this.setAttribute('value', this.value);"
          =onmouseover  "$(this).addClass('b2 br2');"
          =onmouseout   "$(this).removeClass('b2');"
          ;
        ==
      ==
    ::  buttons menu arm is called with pith as input
    ;+  (buttons-menu pith)
    ::  a-tag, opens subtask view with spinner logic on loading
    ;a.p2.br1.hover.action.mono.fr.g2.loader
      =hx-indicator  "this"
      =href          "/hawk{(pith-tape here.bowl)}{(pith-tape pith)}"
      =hx-swap       "innerHTML"
      ;span.b1.br1.p2.hfc.loaded:  →
      ;span.b1.br1.p2.hfc.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
::
++  buttons-menu
  ::  dropdown-menu arm provides reorder
  ::  and oust(delete) logic for kids
  |=  =pith
  ^-  manx
  ::
  ;div.p2.br1.fr.g2.hidden
    =hx-disinherit  "hx-indicator"
    =style          "padding-right:0px;"
    ::
    ::  indicator that %reorder POST request is in progress
    ;div.reorder-indicator.p2.loader
      ;span.loading.p1
        ;+  loading.feather-icons
      ==
    ==
    ::  moves kid div one task down and centers view
    ::  and uses script logic to center user view on moved task
    ;button.b1.br1.p2.hover.hfc
      =onclick  "this.parentNode.parentNode.nextElementSibling?.insertAdjacentElement('afterend', this.parentNode.parentNode);  center(this);"
      ; ↓
    ==
    ::  moves kid div one task up and centers view
    ;button.b1.br1.p2.hover.hfc
      =onclick  "this.parentNode.parentNode.previousElementSibling?.insertAdjacentElement('beforebegin', this.parentNode.parentNode); center(this);"
      ; ↑
    ==
    ::  delete button that sends POST request with %oust poke to parent task
    ::  and after request been sent removes subtask div from DOM
    ;button.b1.br1.p2.hover.loader.hfc
      =type                  "button"
      =hx-post               "/hawk{(pith-tape here.bowl)}?stud=task-diff"
      =hx-target             "find .loading"
      =hx-swap               "outerHTML"
      =hx-on--after-request  "this.parentNode.parentNode.remove();"
      =head                  "oust"
      =pith                  (pith-tape pith)
      ;span.loaded: delete
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
::
++  form-create
  ::
  ::  form-create arm send POST request with data for %new poke
  ::  depending on whether it's a parent task or a kid,
  ::  it specifies a placeholder accordingly
  =/  placeholder  ?:(?=([%ud @ud] (rear here.bowl)) "subtask" "task")
  ^-  manx
  ;div.fc.g1.p4r
  =style  "padding-top:8px;"
    ::  form for %new poke POST request
    ;form.fr.g1
      =hx-post    "/hawk{(pith-tape here.bowl)}?stud=task-diff"
      =head       "new"
      =hx-swap    "outerHTML"
      =hx-target  "find button .loading"
      ::  by default will append new task to the ordered kid list
      ;input.hidden
        =name   "prepend"
        =value  "no"
        ;
      ==
      ::  input for task text data
      ;input.wf.p2.border.br2.grow
        =name          "text"
        =autocomplete  "off"
        =type          "text"
        =required      ""
        =placeholder   placeholder
        =oninput       "this.setAttribute('value', this.value);"
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
