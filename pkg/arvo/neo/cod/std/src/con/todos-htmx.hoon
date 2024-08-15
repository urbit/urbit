/@  todos
/-  feather-icons
:-  [%todos %$ %htmx]
|=  todo=todos
|=  =bowl:neo
|^
  =/  hide-hid  ?:(show-done.todo "" "hide-hidden")
  ;div
    =class  "p-page todo-top mw-page ma {hide-hid}"
    =hx-post  "/hawk{(en-tape:pith:neo here.bowl)}?stud=todos"
    =hx-swap  "none"
    =hx-target  "this"
    =hx-trigger  "todo-save, todo-save-name delay:0.4s"
    ;div.fr.ac.jb.p2.sticky.z1.b0
      =style  "top: 0;"
      ;+  indicator
      ;+  (show-done-toggle show-done.todo)
    ==
    ;div.fc.mt1.todo-list(name "todos")
      ;*  (turn todos.todo render-todo)
    ==
    ;+  form-new
    ;div.hidden.template
      ;+  (render-todo [%.n ''])
    ==
    ;+  style
  ==
++  form-new
  %^  add-attr  %onsubmit
    """
    event.preventDefault();
    let template = $(this).closest('.todo-top').find('.template');
    let list = $(this).closest('.todo-top').find('.todo-list');
    let dolly = template.clone().children().first();
    let input = $(this).find('input');
    let name = input.val();
    input.val('');
    dolly.find('.todo-name').text(name);
    dolly.find('.edit').val(name);
    dolly.find('.edit').attr('value', name);
    dolly.appendTo(list);
    $(this).emit('todo-save');
    """
  ;form.fr.g1.mt1.p2
    ;input.grow.br1.bd1.p2
      =required  ""
      =placeholder  "new task"
      ;
    ==
    ;button.p2.b1.br1.bd1.hover
      ;+  add.feather-icons
    ==
  ==
++  indicator
  ;div.loader.p2
    ;span.loaded;
    ;span.loading
      ;+  loading.feather-icons
    ==
  ==
++  show-done-toggle
  |=  =flag
  ;label.fr.g2.ac.je.f3.p2
    ;span.s-1: show done
    ;+
    =;  m
      ?.  flag  m
      m(a.g [[%checked ""] a.g.m])
    %^  add-attr  %onclick
        """
        if($(this).is(':checked')) \{ this.setAttribute('checked', '') } else \{ this.removeAttribute('checked') }
        $(this).closest('.todo-top').toggleClass('hide-hidden');
        $(this).emit('todo-save');
        """
    ;input
      =type  "checkbox"
      =name  "show-done"
      ;
    ==
  ==
::
++  render-todo
  |=  [done=? name=@t]
  =/  dun  ?:(done "done" "")
  ;div
    =class  "fc p2 g2 todo {dun}"
    ;div.fr.ac.jb.g1
      ;label.fr.g2.ac.js
        ;+  (checkbox done)
        ;div.p2.todo-name: {(trip name)}
      ==
      ;+  %^  add-attr  %oninput
          """
          $(this).closest('.todo').find('.todo-name').text(this.value);
          $(this).attr('value', this.value);
          $(this).emit('todo-save-name');
          """
      ;input.edit.p2.br1.bd1.grow.hidden
        =name  "todo-name"
        =value  (trip name)
        ;
      ==
      ;+  %^  add-attr  %onclick
          """
          $(this).closest('.todo').find('.tray').toggleClass('hidden');
          $(this).closest('.todo').find('.edit').toggleClass('hidden');
          $(this).closest('.todo').find('label').toggleClass('hidden');
          $(this).toggleClass('toggled');
          """
      ;button.p2.b0.br1.hover
        =type  "button"
        ; ⋮
      ==
    ==
    ;div.fr.ac.je.g2.tray.hidden
      ;+  %^  add-attr  %onclick
          """
          let todo = $(this).closest('.todo');
          let par = todo.parent();
          todo.remove();
          par.emit('todo-save');
          """
      ;button.p-1.b1.br1.bd1.hover
        ; delete
      ==
      ;div.grow;
      ;+  %^  add-attr  %onclick
          """
          let todo = $(this).closest('.todo');
          todo.parent().prepend(todo);
          $(this).emit('todo-save');
          """
      ;button.p-1.b1.br1.bd1.hover
        ; ⇈
      ==
      ;+  %^  add-attr  %onclick
          """
          let todo = $(this).closest('.todo');
          let top = $(this).closest('.todo-top');
          if (top.hasClass('hide-hidden')) \{
            todo.prevAll(':not(.done)').first().before(todo);
          } else \{
            todo.prevAll().first().before(todo);
          }
          $(this).emit('todo-save');
          """
      ;button.p-1.b1.br1.bd1.hover
        ; ↑
      ==
      ;+  %^  add-attr  %onclick
          """
          let todo = $(this).closest('.todo');
          let top = $(this).closest('.todo-top');
          if (top.hasClass('hide-hidden')) \{
            todo.nextAll(':not(.done)').first().after(todo);
          } else \{
            todo.nextAll().first().after(todo);
          }
          $(this).emit('todo-save');
          """
      ;button.p-1.b1.br1.bd1.hover
        ; ↓
      ==
      ;+  %^  add-attr  %onclick
        """
        let todo = $(this).closest('.todo');
        todo.parent().append(todo);
        $(this).emit('todo-save');
        """
      ;button.p-1.b1.br1.bd1.hover
        ; ⇊
      ==
    ==
  ==
::
++  checkbox
  |=  =flag
  =;  m
    ?.  flag  m
    m(a.g [[%checked ""] a.g.m])
  %^  add-attr  %onclick
    """
    if($(this).is(':checked')) \{ this.setAttribute('checked', '') } else \{ this.removeAttribute('checked') };
    $(this).closest('.todo').toggleClass('done');
    $(this).emit('todo-save');
    """
  ;input.checkbox-round
    =type  "checkbox"
    =name  "done"
    ;
  ==
++  style
  ;style
    ;+  ;/  %-  trip
    '''
    .checkbox-round {
        width: 1.1em;
        height: 1.1em;
        background-color: var(--b0);
        border-radius: 50%;
        vertical-align: middle;
        border: 1px solid var(--b1);
        appearance: none;
        -webkit-appearance: none;
        outline: none;
        cursor: pointer;
    }
    label {
      cursor: pointer;
    }
    .todo {
      border-bottom: 1px solid var(--b1);
    }
    .todo:last-child {
      border: none;
    }
    .checkbox-round:checked {
      background-color: var(--f2);
    }
    .hide-hidden .done {
      display: none;
    }
    .done .todo-name {
      color: var(--f4);
    }
    '''
  ==
::  collapses newlines to spaces
++  unline
  |=  =tape
  %+  turn  tape
  |=  =cord
  ?:(=(cord '\0a') ' ' cord)
++  add-attr
  |=  [=term =tape =manx]
  ^-  ^manx
  %=  manx
    a.g
      :-  [term (unline tape)]
      a.g.manx
  ==
--
