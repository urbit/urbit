# Ford changes for non-dummies.

You're used to .hoon files being parsed into a ++twig.  This
stays true for vanes, hoon.hoon, and %batz apps; but for 
everything else we get a ++hood.  

It happens that every old .hoon file is also a ++hood. but there
are some extra runes at the top which do interesting cool things. 


# Top-level file stuff.

The first, simplest thing that the new build system gives you: 
without using any other features at all, we treat your file as a
list of nested twigs, ie `=~`, not a single twig.  So instead of

    =>  |%
        ++  library
        --
    |%
    ++  program
    --

you can write - using my new, official, stylized-tilde divider:

    ::    One line that describes this source file.
    ::::  
      ::  /hoon/file/fab/pub
    |%
    ++  library
    --
    ::
    ::::
      ::
    |%
    ++  program
    --

The conventional way of doing `!:` has also changed.  The trouble
is that `!:` is a twig and it cannot come above ++hood runes.
Actually, the right place to apply !: is per core, and it should
stand out visually because it indicates immature code:

    !:  |%
    ++  library
    --

But you should always use at least one ++hood directive, namely 
`/?`.  This is tied to the version of Urbit as a whole, which in
practice means the version of `%zuse` - also defined as ++zuse.
Thus, `zuse` is 314 and `hoon` is 164.

The assumption, currently shaky, is that backward compatibility
in both ++zuse and ++hoon will never be broken.  This is
basically what it means to do Kelvin versioning.  With 

    /?    314

you state that this code should work iff `(lte zuse 314)`.

So this is what the start of a normal .hoon file looks like:

    ::    At least one line that describes this source file.
    ::
    ::::  /hoon/file/fab/pub
      ::
    /?    314
    /-    html, hymn, down, tlon-radio-a
    /+    tlon-radio, tlon-twitter
    ::
    ::::  ~tasfyn-partyv, ~tomsyt-balsen, ~talsur-todres
      ::
    |%
    ++  code
      !!
    --

Note that hood runes by convention use *four* spaces, making
them visually distinct from twig runes - and preserving the
Rastersysteme.

Formal comments: the path is the `++spur` (path after /===/) of
the file, which is generally stored inverted.  The list of ships
is the list of people who have changed the code, oldest first.

Anyone whose (new) .hoon files do not match this template is
eligible for a yellow card!  I mean it!


# Mad hood runes.

Let's go through these runes one by one, creating a wacky test
program as we go.

There are three main sections of a hood: hooves, horns, hoops.
They must be in the file in that order.  Let's talk about them 
in the *opposite* order, though.


# Indirect hoops (simple).

Hoops are the actual code of your program.  You can buy two kinds:

    $%  [%& p=twig]                               ::  direct twig
        [%| p=beam]                               ::  resource location   
    ==                                            ::

The program produced by a hood is a stack of twigs which are
composed by `=~`, ie, connected with `=>`.  You can specify these
twigs directly, as in the above, or you can load them from Urbit.

Let's build a toy program, `mad`, that uses mad hood skillz.
First, let's load it with an indirect hoop.  In

    ~/urb/pub/src/mad/fib.hoon

we put

    |%
    ++  fib  |=(x=@ ~+(?:((lth x 2) 1 (add $(x (dec x)) $(x (sub x 2))))))
    --

and in

    ~/urb/pub/fab/mad/one/hymn.hook

we put

    ::    Our first experiment with major hood runes.
    ::
    ::::  /hoon/one/mad/fab/pub
      ::
    /=    gas  /$  fuel
    //    /===/pub/src/mad/fib
    ::
    ::::  ~tasfyn-partyv
      ::
    =+  arg=(biff (~(get by qix.gas) %number) (slat %ud))
    ::
    ;html
      ;head
        ;title: Mad Experiment One
      ==
      ;body
        ;+  ?~  arg
              ;p: Usage: ?number=x
            ;p  ; This is an ;{i "HTML file"} which 
                ; computes the Fibonacci number
                ; of {<u.arg>}: {<(fib u.arg)>}.
            ==
      ==
    ==


# Indirect hoops (advanced).

Indirect hoops have a still more amazing power - they can make
cores out of directories themselves.   

Let's `cp -r` `fab/mad/one` to `fab/mad/two` and put, in

    ~/urb/pub/src/mad/tools/fib.hoon

the single line

    |=(x=@ ~+(?:((lth x 2) 1 (add $(x (dec x)) $(x (sub x 2)))))) 

Then, in

    ~/urb/pub/fab/mad/two/hymn.hook

we put:

    ::    Our second experiment with major hood runes.
    ::
    ::::  /hoon/two/mad/fab/pub
      ::
    /=    gas  /$  fuel
    //    /===/pub/src/mad/tools
    ::
    ::::  ~tasfyn-partyv
      ::
    =+  arg=(biff (~(get by qix.gas) %number) (slat %ud))
    ;html
      ;head
        ;title: Mad Experiment Two
      ==
      ;body
        ;+  ?~  arg
              ;p: Usage: ?number=x
            ;p  ; This is an ;{i "HTML file"} which 
                ; computes the Fibonacci number
                ; of {<u.arg>}: {<(fib u.arg)>}.
            ==
      ==
    ==

Note that our function name *does not exist anywhere in the
source code*, except where it is called.  Rather, it's implicit
from the filename.

It would be superfluous to observe that this mechanism is
recursive - you can create arbitrarily deep hierarchies of cores
whose symbolic namespace follows the source code tree.

Don't mistake indirect hoops, however, for a library loading
mechanism.  Hoops may be used to *build* libraries, but `/+`
is used to load them.  Indirect hoops are for splitting up your
program into separate parts, not for sharing code between
programs.


# Horns.

Another thing we like to do is compile data into our application.
No, really - one of the stupidest things in many programming 
environment is dynamically loading static resources.  We really
try not to do that in Urbit.  (At least, this week we try!)

Before her hoops, the programmer can load a set of referentially
transparent `horns`, or resources.  

In
    ~/urb/pub/fab/mad/res/hello/hymn.hook

we put:

    ;p: Hello, world.

and in 

    ~/urb/pub/fab/mad/three/hymn.hook

we put:

    ::    This third experiment wears the horns.
    ::
    ::::  /hoon/three/mad/fab/pub
      ::
    /=    hello  /:  /===/pub/fab/mad/res/hello  /hymn/
    :
    ::::  ~tasfyn-partyv
      ::
    ::
    ;html
      ;head
        ;title: Mad Experiment Three
      ==
      ;body
        ;+  hello
      ==
    ==

What did we do?  We loaded a typed, dynamically built resource
from the global namespace onto our reef.

Note the runic syntax above.  You might suspect that this is a
tall mode, and you might be right.  It condenses:

    /=    hello  /:/======/res/hello:/hymn/

Or even:

Note also how we avoid repeating shared path spans.

Also, unlike twig runes, hood runes can be replaced with a 
semantic codename:

    /dub  hello  /see  /======/res/hello  /hymn/


# More horns.

Cool as it is, this example barely scratches the awesome that is
Urbit resource loading.  For instance, in 

    ~/urb/pub/fab/mad/res/bible
  
put, in `1.html`, `2.html`, and `3.html` respectively,

    <p>The earth was without form, and void.</p>

    <p>Then Cain slew Abel.</p>

    <p>I bring not peace, but a sword.</p>

then, in

    ~/urb/pub/fab/mad/four/hymn.hook

put 

    ::    This fourth experiment is profoundly biblical.
    ::
    ::::  /hoon/four/mad/fab/pub
      ::
    /=   bible  /:  /======/res/bible   
                /;  |=  a=(list (pair ,@ manx))
                    (turn a |=([* b=manx] b))
                /@  
                /hymn/
    ::
    ::::  ~tasfyn-partyv
      ::
    !:
    ;html
      ;head
        ;title: Mad Experiment Four
      ==
      ;body
        ;*  bible
      ==
    ==

What happened here?  The `/@` rune loads a list of files named by
decimals in ascending order, with the protocol `/hymn/` - the
result of parsing HTML.  This produces not a list of hymns
(actually just `++manx`, the XML node type), but a list of pairs 
`[number manx]`, so we need to strip the numbers with `/;`.

We can also understand `/:` a little better.  It doesn't mean
"load the horn at this beam" - it means "set the beam at which
horns within this one are loaded."  There is also its cousin
`/,`, which just descends within the current beam.  The beam
starts as the path to the hook file itself.

Nothing about this system restricts the tree to a fixed depth -
we could have not just `/hymn/` in our bible directory,
but another `/@` or other arbitrary resource directives.

`/@` has several cousins for different naming schemes.  `/|`
reads a folder named by `@dr`, ie, relative date.  `/&` reads
one named by `@da`, ie, absolute date.  An excellent way to
organize random events named only by time is to nest `/|` within
`/&`, eg, a tree of days with that day's events in it.

You can also use `/%` to load a map of unparsed names.  Finally,
`/*` handles multiple heterogeneous subtrees with different
horns.  You can cast the product of `/*` to a map with `/^`,
or you can leave it as a tuple.  You can also build a simple list
of horns with `/.`.

Finally, we've already seen `/$`, which calls a parsing function
(`++fuel` for web remainders) on the location and remainder of
the file.  And `/~` lets you stick a simple twig directly in your
resource tree, for computing "resources" entirely by hand.

Here is the structure of horns:

    ++  horn                                ::  resource tree
      $%  [%ape p=twig]                     ::  /~  twig by hand
          [%arg p=twig]                     ::  /$  argument
          [%day p=horn]                     ::  /|  list by @dr
          [%dub p=term q=horn]              ::  /=  apply face
          [%fan p=(list horn)]              ::  /.  list
          [%for p=path q=horn]              ::  /,  descend
          [%hub p=horn]                     ::  /@  list by @ud
          [%man p=(map span horn)]          ::  /*  hetero map
          [%nap p=horn]                     ::  /%  homo map
          [%now p=horn]                     ::  /&  list by @da 
          [%saw p=twig q=horn]              ::  /;  operate on
          [%see p=beam q=horn]              ::  /:  relative to
          [%sic p=tile q=horn]              ::  /^  cast
          [%toy p=mark]                     ::  /mark/  static
      ==                                    ::

See `%ford` for the structure of horns, and `++fair` in ford for
the exact syntax and semantics.


# Hooves: structures.

At the top of your hood comes the proper way of sharing code
between programs: `/-` and `/+`, for structure and library
loading respectively.

Let's try `/-` by reusing one of our earlier examples - this
time, with an actual type cast.  In 

    ~/urb/pub/fab/mad/five/down.hook

put 

    /-  down
    ^-  down
    :~  :-  %par 
        :~  tex/"This is a "
            emp/bent/~[tex/"fun experiment "]
            tex/"in markdown; 2 + 2 is {<(add 2 2)>}."
    ==  ==

In plain English, we loaded the `++down` structure and cast our
hand-rolled markdown to it.  Where did we get this code, exactly?
It came from `/===/sur/down/gate/hook`:

    /?  314
    /-  *markdown
    down

The `*` tells us that we are dealing not with a single gate, but
a core containing multiple gates.  `/===/sur/markdown/core/hook`:

    |%
    ++  down  (list barb)                             ::  markdown structure
    ++  barb                                          ::  block elements
      $%  [%had p=@ud q=(list shin) r=(unit tape)]    ::  depth, contents, id
          [%hem p=manx]                               ::  html tag
          [%hot ~]                                    ::  horizontal rule
          [%lie p=down]                               ::  list element
          [%lit p=? q=down]                           ::  list
          [%par p=(list shin)]                        ::  paragraph
          [%pre p=wall]                               ::  preformatted text
          [%quo p=down]                               ::  blockquote
      ==                                              ::
    ++  shin                                          ::  span elements
      $%  [%cod p=tape]                               ::  inline code
          [%cut ~]                                    ::  break
          [%emp p=?(%bent %bold %both) q=(list shin)] ::  emphasis
          [%ike p=(list shin)]                        ::  strikethrough
          [%lin p=(list shin) q=tape r=(unit tape)]   ::  link
          [%tex p=tape]                               ::  text
      ==                                              ::
    -- 

We see that markdown isn't a trivial single structure.  With `*`
in a hoof we do two things.  One, we load a core not a gate.
Two, we implicitly stick an

    =+  markdown

on your reef, so that you can use `down` where otherwise you'd
have to say `down:markdown`.  `down/gate/hook` uses this to 
export the naked `down` gate into the structure space.

What actually happens to all the resources you load with `/-`?
They go into a single core above the libraries, horns and hoops.
Moreover, when libraries, horns and hoops contain `/-` runes of
their own, indicating structure dependencies, *all* these
dependencies are collected into the single structure core.


# Hooves: global names and conflicts.

When we use a single symbol, like `down`, we indicate that we're
looking for a structure in the same ship, desk, and case as our
own.  In other words, we're loading a local structure which is
tightly coupled to the system loading it.

We can also use global structures.  Here the syntax is 

    name/label/~ship

The desk in the global case is bound to `%main`, because it's
assumed that if you're publishing your code to the world it
belongs on your `%main`.

But this still becomes an arm named `structure` in your structure
core.  What happens if we depend in two places on two hooves,
with the same name but different labels or ships?

For now, we are conservative - all hooves in a hood must match
exactly.  Eventually we'll parse conventional labels (for Kelvin
and semantic versioning) and perform automagic upgrades.


# Hooves: libraries.

Libraries are loaded much the same way as structures, except with
`/+`.  The syntax is the same, except that there is no equivalent 
of `*`.  Library dependencies are also collected across the
entire build, like structure dependencies.

Libraries are assumed to be cores, and they are stacked one after
another in dependency order - if a depends on b, a obviously must
come after b.

Let's try using a library.  In

    ~/urb/lib/example/core.hook

put
  
    |%
    ++  fib  |=(x=@ ~+(?:((lth x 2) 1 (add $(x (dec x)) $(x (sub x 2))))))
    --

Then, in

    ~/urb/pub/fab/mad/six/hymn.hook

we put:
    ::    Our sixth experiment with major hood runes.
    ::
    ::::  /hoon/six/mad/fab/pub
      ::
    /+    example 
    /=    gas  /$  fuel
    ::
    ::::  ~tasfyn-partyv
      ::
    =+  arg=(biff (~(get by qix.gas) %number) (slat %ud))
    ;html
      ;head
        ;title: Mad Experiment Two
      ==
      ;body
        ;+  ?~  arg
              ;p: Usage: ?number=x
            ;p  ; This is an ;{i "HTML file"} which 
                ; computes the Fibonacci number
                ; of {<u.arg>}: {<(fib u.arg)>}.
            ==
      ==
    ==

