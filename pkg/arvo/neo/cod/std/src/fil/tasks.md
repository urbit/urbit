# Tutorial 4: Tasks
Let’s take a look at the Tasks shrub. From within the Sky frontend, you can create, edit, reorder, and delete nested tasks and subtasks. Checking off all subtasks will mark the parent as complete, which involves some interaction between parent and child shrubs.

This time there are no new concepts on the backend, but there’s much more going on in the UI than we’ve looked at in previous tutorials.

In this lesson we’ll see how shrubs keep their parents informed of state changes using the `%gift` poke. This is the most complex UI we’ve looked at yet, so we’ll also focus on the `/con` files.

## /imp/task.hoon
Tasks only needs one `/imp` file: `task.hoon`. The Tasks frontend shows you some tasks that may or may not have other tasks as children.

```hoon
/@  task  ::  [text=cord done=? order=(list path]
::
::  $task-diff
::  $%  [%new =task prepend=?]
::      [%edit text=cord done=?]
::      [%oust =pith]
::      [%reorder order=(list pith)]
::  ==
/@  task-diff
=>
::
::  helper core
|%
::
::  check if all kids are completed
++  check-kids
  |=  =bowl:neo
  ^-  ?
  ?:  =([~ ~] kids.bowl)
    %.y
  =/  piths  ~(tap in ~(key by ~(tar of:neo kids.bowl)))
  %+  levy  piths
  |=  =pith
  =/  =task
    !<  task
    q.pail:(need (~(get by ~(tar of:neo kids.bowl)) pith))
  done.task
::
::  assign a unique numerical ID to a new subtask
++  assign-name
  |=  =bowl:neo
  ^-  @ud
  ?:  =([~ ~] kids.bowl)  1
  =/  sorted-names=(list @ud)
    %-  sort  :_  lth
    %+  turn  ~(tap by ~(tar of:neo kids.bowl))
      |=  [=pith =idea:neo]
      +:(,[%ud @ud] (rear pith))
  =/  last-name=@ud  (rear sorted-names)
  =/  name-missing=(list @ud)
    %+  skim  (gulf 1 last-name)
    |=  n=@ud
    =(~ (find ~[n] sorted-names))
  ?~  name-missing  +(last-name)
  (rear name-missing)
--
::
::  outer core
^-  kook:neo
|%
::
::  state is a %task
++  state  pro/%task
::
::  we accept %gift and %task-diff pokes
++  poke   (sy %task-diff %gift ~)
::
::  we define one generation of kids at
::  /path/to/this/task/<@ud> which are
::  also tasks like their parent, which
::  means those kids also define one
::  generation of kids below them, and so on
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%ud |]
    [pro/%task (sy %task-diff %gift ~)]
  ==
++  deps
  ::
  ::  we don't need dependencies to negotiate
  ::  state between parent and children tasks,
  ::  but it would be valid to do so
  *deps:neo
++  form
  ::
  ::  inner core
  ^-  form:neo
  ::  XX refactor sample to same as other tutorials
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  ::  XX refactor this to default +init from counter
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `(need pal)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =/  this  !<(task state-vase)
    ?+    stud  !!
        %gift
      ?:  (check-kids bowl)
        [~ task/!>(this(done %.y))]
      [~ task/!>(this(done %.n))]
    ::
        %task-diff
      =/  diff  !<(task-diff vax)
      ?-    -.diff
          %new
        =/  name=@ud  (assign-name bowl)
        =.  order.this
          ?:  prepend.diff
            ::  XX do we really need to do this? bad look if so
            `(list pith)`[~[ud/name] order.this]
          `(list pith)`(snoc order.this `pith`[ud/name ~])
        =.  done.this  |
        :_  task/!>(this)
        :~  :-  (welp here.bowl ~[ud/name])
            [%make %task `task/!>(task.diff) ~]
        ==
      ::
          %edit
        :-  ~
        :-  %task
        !>
        %=  this
          text  text.diff
          done  ?:  (check-kids bowl)
                  done.diff
                %.n
        ==
      ::
          %oust
        =/  i  (find [pith.diff ~] order.this)
        ?~  i  `task/!>(this)
        :_  task/!>(this(order (oust [(need i) 1] order.this)))
        :~  [(welp here.bowl pith.diff) [%tomb ~]]
        ==
      ::
          %reorder
        `task/!>(this(order order.diff))
      ==
    ==
  --
--
```

## The %gift poke
In the `+poke` arm we declare this shrub takes a `%gift` as well as a `%task-diff`, but we don’t have to import `%gift` with a `/@` rune. This is a special poke like `%rely` that `/app/neo` gives us when another shrub’s state changes.

Our shrub receives a `%gift` poke every time the state of one of its {kids / descendants} changes. Unlike `%rely`, we have to declare that we accept `%gift` pokes in the `+poke` arm.

```hoon
++  state  pro/%task
++  poke   (sy %task-diff %gift ~)
```

Then we can handle the poke like any other. In this case, when `/imp/task` receives word that one of its kids’ state has changed, it checks to see if all of its kids are completed, then marks its own state as completed or uncompleted accordingly.

```hoon
?+    stud  !!
    %gift
  ?:  (check-kids bowl)
    [~ task/!>(this(done %.y))]
  [~ task/!>(this(done %.n))]
::
```

## Frontend
Let’s look at the Tasks frontend in detail.

### Converting tasks to HTMX
```hoon
/@  task     ::  [text=cord done=? order=(list pith)]
:: import /lib/feather-icons
/-  feather-icons
:: declare that this is a conversion from task to HTMX
:-  [%task %$ %htmx]
::  gate takes a task and a bowl:neo,
::  so we can access here.bowl and kids.bowl
|=  t=task
|=  =bowl:neo
::
::  all sail rendering happens in helper arms
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
    document.getElementById("alert").addEventListener("click", function(e){
      if (document.getElementById("alert").hasAttribute("readonly")){
      e.preventDefault();
      alert("Subtasks are not completed");
      }
    });
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
  |=  [parent-pith=pith order=(list pith)]
  ^-  ?
    %+  levy  order
    |=  p=pith
    =/  =task
      !<  task
      q.pail:(need (~(get by ~(tar of:neo kids.bowl)) (weld parent-pith p)))
    done.task
::
::
++  form-ordered-kids
  ::
  ::  <form> that keeps track of tasks order, sends
  ::  %reorder poke if tasks are manually reordered
  ;form.fc.g1
    =hx-post       "/neo/hawk{(pith-tape here.bowl)}?stud=task-diff"
    =head          "reorder"
    =hx-indicator  ".reorder-indicator"
    =hx-swap       "none"
    ;*
    ::
    ::  iterates over the list of piths,
    ::  turning through the kids' data
    %+  turn  order.t
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
  =/  =pail:neo  pail.idea
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
        =hx-post     "/neo/hawk{pt}?stud=task-diff"
        =hx-trigger  "input changed delay:0.4s from:find .text, input from:find .done"
        =hx-swap     "none"
        =head        "edit"
          ;+
          ::  defines class attribute with class names
          =/  class  [%class "p2 br1 border done s3"]
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
          ::  combining m named noun and
          ::  attribute logic with manx below
          =-  =/  m  -
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
                (kids-check pith order.t)
              ?:  kc
                ::  assigning class attribute to
                ::  the rest of manx data
                m(a.g [class a.g.m])
              ::
              ::  assigns readonly, id and class
              ::  attributes to checkbox; id will trigger
              ::  alert script functionality
              m(a.g [[%readonly ""] [%id "alert"] class a.g.m])
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
        ::  combining that named noun and class name
        ::  logic with manx below and make it in to an XML node
        ;+  =-
          =/  that  -
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
      =href          "/neo/hawk{(pith-tape here.bowl)}{(pith-tape pith)}"
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
      =hx-post               "/neo/hawk{(pith-tape here.bowl)}?stud=task-diff"
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
      =hx-post    "/neo/hawk{(pith-tape here.bowl)}?stud=task-diff"
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
```

### Converting %task-diffs to HTMX
```hoon
/@  task-diff
/-  feather-icons
:-  [%task-diff %$ %htmx]
|=  t=task-diff
|=  =bowl:neo
^-  manx
;div.loading
  =hx-get        "/neo/hawk{(en-tape:pith:neo here.bowl)}"
  =hx-target     "closest .hawk"
  =hx-indicator  "closest .loader"
  =hx-swap       "innerHTML"
  =hx-trigger    "load"
  ;span.loading
  ;+  loading.feather-icons
  ==
==
```

### Converting HTMX to %task-diffs
```hoon
/@  node  ::  manx
::
::  $task-diff
::  $%  [%new =task prepend=?]
::      [%edit text=cord done=?]
::      [%oust =pith]
::      [%reorder order=(list pith)]
::  ==
/@  task-diff
::  import lib/manx-utils
/-  manx-utils
::
::  declare that this is a conversion from a
::  dynamic XML node to task-diff
:-  [%node %$ %task-diff]
|=  nod=node
^-  task-diff
=/  mu  ~(. manx-utils nod)
::  extract head attribute from XML node
=/  head  (@tas (got:mu %head))
%-  task-diff
?+  head
  ~|  [%unknown-head head]
  !!
    %new
  ::  extract text and prepend attributes from XML node
  =/  text     (vol:mu "text")
  =/  prepend  (vol:mu "prepend")
  ?:  =(prepend 'prepend')
  ::  construct the task-diff
    [head [text | ~] &]
  [head [text | ~] |]
::
    %edit
  ::  extract text attribute from XML node
  =/  text     (vol:mu "text")
  ::  extract checked attribute from done element in XML node
  =/  done-el  (need (named:mu "done"))
  =/  done     (~(has by (malt a.g.done-el)) %checked)
  ::  construct the task-diff
  [head text done]
::
    %oust
  ::  extract pith attribute from XML node
  =/  path  (stab (got:mu %pith))
  ::  construct the task-diff
  [head (pave:neo path)]
::
    %reorder
  =/  piths
    ::
    ::  extracting here attribute from each node in XML
    ::  list and return as last element of here path
    %+  turn  c.nod
    |=  =manx
    =/  mu-reorder  ~(. manx-utils manx)
    =/  here        (get:mu-reorder %here)
    ?~  here
      ~&  >>>  [%bad-here manx]
      !!
    (pave:neo /[(rear (stab (crip (need here))))])
  ::  construct the task-diff
  [head piths]
==
```
