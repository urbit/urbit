# Tutorial 1: Counter

One of the simplest shrubs imaginable is a counter that stores one number and takes one poke: increment the number.

By the end of this tutorial you'll understand the structure of a shrub, and how to write a trivial one of your own. This won't explain shrubbery from first principles — you don’t need to understand it from first principles — but you'll see how similar a shrub is to a Gall agent and where they differ.

You’ll also get a glimpse of how one shrub can accomodate various frontend interfaces. We’ll make a simple frontend for Sky, a prototype namespace browser.

## Counter in Gall and Shrubbery
Here's the Gall agent we'll reimplement in shrubbery. It stores one number and takes one poke, `%inc`, to increment the number.

```hoon
/+  dbug, default-agent, verb
|%
+$  versioned-state
$%  state-0
==
+$  state-0
$:  %0
    value=@ud
==
+$  counter-action
  $%  [%inc ~]
  ==
+$  card  card:agent:gall
--
::
%+  verb  &
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init   on-init:def
++  on-peek   on-peek:def
++  on-watch  on-watch:def
++  on-arvo   on-arvo:def
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
++  on-save
  !>(state)
::
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  :-  ~
  %=  this
    state  !<(state-0 old)
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+  mark
    (on-poke:def mark vase)
  ::
      %noun
    =/  act
      !<(counter-action vase)
    ?+  -.act
      (on-poke:def mark vase)
    ::
        %inc
      :-  ~
      %=  this
        value  +(value)
      ==
    ==
  ==
--
```

Here's the same thing in shrubbery.

```hoon
/@  number
/@  counter-diff
^-  kook:neo
|%
++  state  pro/%number
++  poke   (sy %counter-diff ~)
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
    +*  state  !<(number state-vase)
    ++  init
      |=  old=(unit pail:neo)
      ^-  (quip card:neo pail:neo)
      `(need old)
    ++  poke
      |=  [=stud:neo vaz=vase]
      ^-  (quip card:neo pail:neo)
      ?>  =(%counter-diff stud)
      =/  act  !<(counter-diff vaz)
      ?>  =(-.act %inc)  ::  XX can we remove this line?
      `number/!>(+(state))
  --
--
```

Let’s set up a fakeship that has `/base/app/neo.hoon`, the prototype “shrub runner”, then go over the code piece by piece.

## Counter shrub, explained
Now let’s take a closer look at the counter shrub. You’ll find a version of `counter.hoon` with comments in your `/imp` folder.

```hoon
::  /imp/counter.hoon
/@  number
/@  counter-diff
```

These lines import two files from our `/pro` folder: `number.hoon` and `counter-diff.hoon`.

```hoon
::  /pro/number.hoon
,@ud
```

```hoon
::  /pro/counter-diff.hoon
,[%inc ~]
```

The folder structure you have to work with right now is messier than it will be in the final product. This is an artefact of prototyping shrubbery in a Gall agent in the `%base` desk.

The only folders you need to understand for this tutorial are `/pro`, `/imp`, and `/con`.
- `/pro` for protocols. Like `/sur`, this is where your custom types live.
- `/imp` for implementations. Like `/app`, this is where your Gall agent-like shrubs live.
- `/con` for conversions. Like `/mar`, this is where you define rules for transforming nouns in your desk.

Let’s look at the rest of the `/imp` file.

```hoon
/@  number
/@  counter-diff
::
::  outer core of a shrub
^-  kook:neo
|%
::
::  the state of counter is a %number, just a @ud
++  state
  ^-  curb:neo
  [%pro %number]
::
::  the set of pokes that counter
::  takes only contains %counter-diff
++  poke
  ^-  (set stud:neo)
  (sy %counter-diff ~)
::
::
::  counter does not "constrain" its children;
::  any shrub can be made below this shrub in the
::  namespace, they can have any state and any kids
++  kids
  ^-  kids:neo
  *kids:neo
::
::  counter has no other shrubs as dependencies
++  deps
  ^-  deps:neo
  *deps:neo
::
::  inner core of a shrub
++  form
  ^-  form:neo
  ::  treat this door's sample as boilerplate
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
    ::
    ::  de-vase the state; we don't know what it is,
    ::  in most cases it will be counter's old state
    +*  state  !<(number state-vase)
    ::
    ::  +init, like +on-init
    ++  init
      ::
      ::  minimal +init, just returns the
      ::  initial state passed in on %make
      |=  old=(unit pail:neo)
      ^-  (quip card:neo pail:neo)
      [~ (need old)]
    ::
    ::  +poke, like +on-poke
    ++  poke
      ::
      ::  a stud (e.g. %number or %counter-diff) is kind
      ::  of like a mark, it only gets more complicated
      ::  than that with types from other desks/ships
      |=  [=stud:neo vaz=vase]
      ::
      ::  return a (list card:neo) and a
      ::  pail, which is a (pair stud vase)
      ^-  (quip card:neo pail:neo)
      ::
      ::  assert that the poke's stud is %counter-diff,
      ::  which protects counter from evil vases
      ?>  =(%counter-diff stud)
      =/  act
        !<(counter-diff vaz)
      ?>  =(-.act %inc)
      ::
      ::  return no cards and a pail
      [~ [%number !>(+(state))]]
  --
--
```

Once you’ve saved `/imp/counter.hoon`, run `|commit %base` and Neo will add it to its state. We can now interact with this shrub in the Dojo.

## Poking the shrub
A `card:neo` is a `(pair pith note)`.

A `pith` is a list of head-tagged cells forming a typed path. This is the location of the shrub to which your card will be sent.
* The path `/examples/counter/one` will be a pith `~[%examples %counter %one]`.
* The path `/~sampel/examples/counter/one` will be a pith `~[[%p ~sampel] %examples %counter %one]`.
* The path `/~sampel/examples/counter/1` will be a pith `~[[%p ~sampel] %examples %counter [%ud 1]]`.

A `note` is one of the four types of command any shrub will accept.

```hoon
+$  note
  $%  [%make made]             ::  create a shrub
      [%poke =pail]            ::  poke a shrub
      [%tomb cas=(unit case)]  ::  tombstone a shrub
      [%cull ~]                ::  ???
  ==
```

If the `pith` doesn’t correspond to the location of an existing shrub, you’ll have to make a shrub there before doing anything else.

Let’s `%make` a shrub at path `/examples/counters/one` from the Dojo, giving it an initial state of `0`. We’ll explain the structure of the `%make` note in more detail in the Diary tutorial.

```
:neo &neo-card [~[[%p our] %examples %counters %one] [%make %counter `[%number !>(0)] ~]]
```

You should see `>> %make /examples/counters/one` in the Dojo if successful.

Now we can now send a `%poke` to the counter shrub at this path.

```
:neo &neo-card [~[[%p our] %examples %counters %one] [%poke [%counter-diff !>([%inc ~])]]]
```

At time of writing there is no easy way to inspect the state of a shrub from the Dojo. We’ll just have to build a frontend and hope it all just works.

## Counter frontend in Sky
Shrubbery aims to be interface-agnostic. One part of that vision is `/con` files, which make it possible to convert data from one backend type to any frontend type, and one frontend type to any backend type. Here are Counter’s `/con` files.

### Converting number to HTMX
```hoon
::  /con/number-htmx.hoon
/@  number  ::  @ud
/-  feather-icons
:-  [%number %$ %htmx]
|=  =number
|=  =bowl:neo
^-  manx
;div.p3.fc.g2.ac.br2
  ;h1:  Counter
  ;p:  {<number>}
  ;form
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=counter-diff"
    =hx-target  "find .loading"
    =hx-swap  "outerHTML"
    =head  "inc"
    ;button.bd1.br1.p2.b1.hover.loader
      ;span.loaded:  Increment
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
==
```

This “converts” the `number` type to a `manx`, specifically targeting a frontend that uses the [HTMX](https://htmx.org/) library. You don’t need to know HTMX to build shrubbery frontends or to follow the rest of this tutorial. If you want to understand the HTMX above in more detail, see `/con/number-htmx.hoon` for line-by-line comments.

This isn’t a 1:1 conversion from one data type to another; we’re not converting Hoon `number=1` to JSON `{ "number": 1 }`. If a frontend asks for a `number` in the form of HTMX, we return some [Sail](https://docs.urbit.org/language/hoon/guides/sail) that interpolates the `number` in a basic interface consisting of a heading, the number, and one button to send an `%inc` poke to the Counter shrub.

### Converting Node to %counter-diff
```hoon
::  /con/node-counter-diff.hoon
/@  node          ::  manx
/@  counter-diff  ::  [%inc ~]
/-  manx-utils
:-  [%node %$ %counter-diff]
|=  =node
^-  counter-diff
=/  mu  ~(. manx-utils node)
=/  head  (?(%inc) (got:mu %head))
[head ~]
```

This is a more straightforward conversion from a dynamic XML node (in this case, HTMX), to a `%counter-diff`. Using the [manx-utils](https://github.com/tinnus-napbus/manx-utils) Hoon library for brevity, we extract the XML node’s `head` attribute (which has been converted to the term `%inc` on its way here) and use that to form the `%counter-diff`, which is `[%inc ~]`. See `/con/node-counter-diff.hoon` for line-by-line comments.

## Testing the Counter in Sky
The Sky homepage shows you one tile for all of the shrubs who are the immediate children of your `/home` shurb, which was made for you upon booting `%neo` for the first time. You won’t see a Counter tile there because there is no `/counter` shrub beneath `/home`, so let’s make one.

```
:neo &neo-card [~[[%p our] %home %counter] [%make %counter `[%number !>(0)] ~]]
```

If you refresh your browser you should now see a tile labelled “counter”. Click there to see the Counter frontend from the `/con` file and increment the state of the `/counter` shrub.

## Building on the Counter
If you know your way around Gall, you should now be able to make some minor changes to the counter example above. Try the following:

* Initialize the shrub with a default state if the given `(unit vase)` in `+init` is empty.
* Add more pokes like `%dec`, `%add`, and `%sub` on the backend.
* Add those pokes to the frontend interface, with one button per poke.
