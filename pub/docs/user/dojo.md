---
sort: 8
title: Dojo manual
---

# `:dojo` manual

The dojo is a typed functional shell.  Assuming our default
plot `~fintud-macrep`,  

Its prompt is:

    ~fintud-macrep:dojo>

## Quickstart

To print a Hoon expression or other recipe:

    ~fintud-macrep:dojo> (add 2 2)

To save a recipe as a variable `foo`:

    ~fintud-macrep:dojo> =foo (add 2 2)

To save as a unix file (`$pier/.urb/put/foo/bar.baz`):

    ~fintud-macrep:dojo> .foo/bar/baz (add 2 2)

To save as an urbit file (`/===/foo/bar/baz`):

    ~fintud-macrep:dojo> *foo/bar/baz (add 2 2)

A noun generator with ordered and named arguments:

    ~fintud-macrep:dojo> +make one two three, =foo (add 2 2), =bar 42

A poke message to an urbit daemon:

    ~fintud-macrep:dojo> :~fintud-macrep/talk (add 2 2)

A system command to `:hood`:

    ~fintud-macrep:dojo> |reload %vane

## Manual

An Urbit value is called a "noun."  A noun is either an unsigned
integer ("atom") or an ordered pair of nouns ("cell").  Nouns
are just values, with no cyclic structure or pointer identity.

The dojo is your safe space for hand-to-hand combat with nouns.
Every dojo command builds a "product" noun functionally, then
applies this product in a side effect -- show, save, or send.

### Theory

In the quickstart we learned a crude interpretation of the dojo
in terms of "expressions, generators and operations."  While
nothing in the quickstart section is inaccurate, it's not the way
the system works internally.

*All* dojo lines are commands.  An operation uses a *recipe*
to create a noun, which the command uses in its side effect.
Just printing the noun is a trivial case of a command.

Recipes are *conceptually* functional, but often use concrete,
stateful action sequences.  A simple Hoon expression (*twig*) is
purely functional, but it's only one kind of recipe.

A recipe can get data from an HTTP GET request or an interactive
input dialog.  It can also query, even block on, the Urbit
namespace.  These operations are *conceptually* functional, but
not *actually* functional.  They don't belong in a pure Hoon
expression, but they do belong in a dojo recipe.  A recipe is "FP
in the large," more like Unix pipes than Haskell monads.

The dojo is "single-threaded" in each session.  One session can
work on one command at a time.  The session does not accept user
input while processing a command, even when it blocks over the
network.  And each session's state is independent.  (If you want
to work on two things at a time, connect two console sessions to
your dojo.)

Once you've built the product of your recipe, you show, save, 
or send it.

You can pretty-print the product to the console.  You can save it
-- as a dojo variable, as a revision to the Urbit filesystem, or
as an export to a file in the Unix filesystem.  Or you can
send it -- staying native with an Urbit poke, or going retro
with an HTTP PUT/POST.

All these operations are typed.  Hoon is a statically typed
language, but the dojo is a dynamic interpreter.  The nouns you
build in the dojo are dynamically typed nouns, or "cages".

A cage actually has two layers of type: "mark," a network label
(like a MIME type), and "span," a Hoon language type.  When a
cage is sent across the Urbit network, the receiving daemon
validates the noun against its own version of the mark, and
regenerates the span.

Of course, sometimes a recipe produces a noun with mark `%noun`,
meaning "any noun," and span `*`, the set of all nouns.  We have
no choice but to do the best we can with mystery nouns, but we 
prefer a formal description.

A mark is also called a "format."  Marks let us perform a variety
of formal typed operations on nouns: validation, translation,
even patch and diff for revision control.

### Other resources

An excellent way to understand `:dojo` is to read the source,
which is in `/===/ape/dojo/hoon`.

Unfortunately, you may or may not know Hoon.  We'll use some Hoon
snippets here for defining structures and grammars.  Just think
of it as pseudocode -- the meaning should be clear from context.

### Syntax and semantics

To use the dojo, type a complete command at the dojo prompt.
The simplest command just prints a Hoon expression:

    ~fintud-macrep:dojo> (add 2 2)

Hit return.  You'll see:

    > (add 2 2)
    4
    ~fintud-macrep:dojo> 

Similarly in tall form,

    ~fintud-macrep:dojo> %+  add  2  2
    > %+  add  2  2
    4
    ~fintud-macrep:dojo> 

An incomplete command goes into a multiline input buffer.  Use
the up-arrow (see the console history section) to get the last
command back, edit it so it's just `%+  add  2`, and press return.
You'll see:

    > %+  add  2
    ~fintud-macrep/dojo<

Enter `2`.  You'll see:

    > %+  add  2
      2
    4
    ~fintud-macrep/dojo>

The full command that parses and runs is the concatenation of all
the partial lines, with a space inserted between them.  To clear
all multiline input, just hit return on an empty prompt.

### Command structure

Every finished line is parsed into one `++dojo-command`:

    ++  dojo-command                        ::
      $%  [%edit p=path q=dojo-recipe]      ::  modify clay file
          [%http p=? q=purl r=dojo-recipe]  ::  http post or put
          [%poke p=goal q=dojo-recipe]      ::  send request
          [%save p=path q=dojo-recipe]      ::  replace clay file
          [%show p=dojo-recipe]             ::  print to console
          [%unix p=path q=dojo-recipe]      ::  export to unix
          [%verb p=term q=dojo-recipe]      ::  store variable
      ==                                    ::

Each kind of `++dojo-command` is an action that depends on one
noun production, a `++dojo-recipe`.  We describe first the
commands, then the recipes.

##### `[%show p=dojo-recipe]`

To print the product, the command is just the recipe:

    ~fintud-macrep:dojo> (add 2 2)

##### `[%verb p=term q=dojo-recipe]`

To save the product to a variable `foo`:

    ~fintud-macrep:dojo> =foo (add 2 2)

`foo` goes into your Hoon subject (scope) and is available to all
expressions.

To unbind `foo`:

    ~fintud-macrep:dojo> =foo

The dojo has a set of special variables, some read-write and some
read-only: `dir`, `lib`, `arc`, `now`, `our`, `eny`.

The read-write specials are `dir`, `lib` and `arc`. `dir` is the
current working beak (`%clay` branch and revision) and directory. The
current beak is often the invalid "version 0", which is shorthand for
the most recent revision. If you need a valid `%clay` path, say, as an
argument to `+ls`, use the special read-only variable `%`, which
automatically expands this to the current time. If `dir` is unset,
then the current working path is defaulted to the current revision of
the `home` desk on your urbit.

`lib` is a set of libraries, and `arc` a set of structures, to put in
the Hoon subject. Add libraries with the `%ford` rune `/+`, and
structures with `/-`. Libraries will be loaded from the `lib`
directory in the current beak, and structures from the `sur`
directory.

Read-only specials are `now`, the current (128-bit `@da`) time, `our`,
the current urbit, and `eny`, 256 bits of fresh entropy.

##### `[%edit p=path q=dojo-recipe]`
##### `[%save p=path q=dojo-recipe]`

The product is either a new version of, or a modification to,
the Urbit file at the given path.  (See the discussion of Urbit
filesystem paths.)

To save:

    ~fintud-macrep:dojo> *%/numbers/four (add 2 2)

To edit:

    ~fintud-macrep:dojo> -%/numbers/four (add 2 2)

A save (`*`) overwrites the current (if any) version of the file
with a new version of any mark.  The save command above will work
(if you want `/numbers/four` at your current path).

An edit (`-`) applies a diff whose mark has to match the diff
mark for the current version of the file.  The edit command above
will not work, because evaluating a Hoon expression like `(add 2
2)` just produces a `%noun` mark, ie, an arbitrary noun.

For either saves or edits, the current version of the file must
be the same version specified in the write -- in other words,
we can only write to HEAD.  If someone else has sneaked in a
change since the version specified, the command will fail.

##### `[%unix p=path q=dojo-recipe]`

    ~fintud-macrep:dojo> ./numbers/four (add 2 2)

The product is saved as a Unix file (its mark is translated
to MIME, and the MIME type is mapped as the extension).

##### `[%poke p=goal q=dojo-recipe]`

A poke or *order* is a one-way transactional request.  It either
succeeds and returns no information, or fails and produces an
error dump.

Every order is sent to one daemon on one urbit.  The default
urbit is your urbit.  The default daemon is the system daemon,
`:hood`.  The following syntactic forms are equivalent:

    ~fintud-macrep:dojo> :~fintud-macrep/hood (add 2 2)
    ~fintud-macrep:dojo> :hood (add 2 2)
    ~fintud-macrep:dojo> :~fintud-macrep (add 2 2)
    ~fintud-macrep:dojo> : (add 2 2)

Urbit pokes do not have a separate verb.  The mark of the message
defines the semantics of the operation.  You don't call a method
`foo` whose argument is a noun in mark `bar` -- you poke a noun
in mark `bar`.  The mark is the protocol is the method.

If the order succeeds, you'll see an `>=` line.  If not, you'll
see an error report, typically with a stack trace.

It's common (but not necessary) to use a custom generator for the 
daemon you're talking to.  (For generators, see below.)  Hence

    ~fintud-macrep:dojo> :~fintud-macrep/fish +fish/catch (add 2 2)

It's irritating to type "fish" twice, just because we're using a
fish generator to talk to a fish daemon.  Hence a shortcut:

    ~fintud-macrep:dojo> :~fintud-macrep/fish|catch (add 2 2)

If we combine all these defaults, we get the "system command"
shortcut:

    ~fintud-macrep:dojo> :~fintud-macrep/hood +hood/reload %ames
    ~fintud-macrep:dojo> |reload %ames

This is the most common poke, a generated message to your own
hood. 

##### `[%http p=? q=purl r=dojo-recipe]`

The Web has its own poke, unfortunately in two flavors.  To POST,

    ~fintud-macrep:dojo> +http://website.com (add 2 2)

To PUT:

    ~fintud-macrep:dojo> -http://website.com (add 2 2)

As with a poke, you'll get a >= for success, or an error report.

#### Recipes, models and filters

But wait, what's a recipe?  Simplifying the actual code slightly:

    ++  dojo-recipe                          ::  functional build
      $%  [%ex p=twig]                       ::  hoon expression
          [%as p=mark q=dojo-recipe]         ::  format conversion
          [%do p=twig q=dojo-recipe]         ::  apply gate
          [%ge p=dojo-script]                ::  generator
          [%ur p=purl]                       ::  get url
          [%tu p=(list dojo-recipe)]         ::  tuple
      ==                                     ::
    ++  dojo-script                          ::  script
      $:  p=path                             ::  core recipe
          q=dojo-config                      ::  configuration
      ==                                     ::
    ++  dojo-config                          ::  configuration
      $:  p=(list dojo-recipe)               ::  by order
          q=(map term (unit dojo-recipe))    ::  by keyword
      ==                                     ::

##### `[%ex p=twig]`

The twig in an `%ex` recipe is a Hoon expression.  The recipe
syntax is just the Hoon syntax.

The subject of the twig is a core stack: first the Hoon kernel,
then the Arvo standard library, then the structures and libraries
in `lib` and `arc`.  On the very top are the dojo variables.

A twig produces the trivial mark `%noun`, except in two cases
where the dojo can do better.  The dojo analyzes the twig to
detect two trivial cases where direct evaluation gives us a mark:
a variable reference like `foo` that matches a dojo variable, or
an urbitspace dereference like `.^(/cx/~fintud-macrep/main/1/foo)`.
In either case, if we executed these through Hoon, we'd get the
same noun with the same span.

##### `[%tu p=(list dojo-recipe)]`

A is just a tuple of recipes, using the normal Hoon syntax for
a tuple.  `[a]` is `a`, `[a b]` the cell `[a b]`, `[a b c]` the
cell `[a [b c]]`.

A tuple, unless it's a trivial 1-tuple, is always marked `%noun`.

##### `[%ge p=dojo-script]`

A `%ge` is a generator, a configurable script loaded from the
filesystem.

The script recipe `++dojo-script` specifies a script path, a list
of ordered arguments, and a list of keyword arguments.  All the
arguments are recipes.  The path specifies a Hoon source file in
`/===/gen/[path]`.

For the path `/fun/make`, the ordered arguments `1`, `2` and `3`, 
and the named arguments `foo` and `bar`, the syntax is:

    ~fintud-macrep:dojo> +fun/make 1 2 3, =foo (add 2 2), =bar 42

Unless this non-closed form is the end of a command, it needs to
be surrounded by `[]` to make it play well with others.

Generator programming is covered in the dojo developer's guide.
The user doesn't need to know or notice how the generator gets
its input (if any), except in one case: a dialog.

A dialog generator will take over the prompt and ask you
questions.  If this seems terrifying, ^D will abort the dialog,
the recipe, and the command, and take you back to the dojo.

##### `[%as p=mark q=dojo-recipe]`

`%as` is a mark conversion. Since the input to it is another
recipe, we can chain them to make a conversion pipeline.

To convert a recipe, just precede it with the converison form, `&mark`:

    ~fintud-macrep:dojo> &noun (add 2 2)
    ~fintud-macrep:dojo> &md (add 50 7)

##### `[%do p=twig q=dojo-recipe]`

`%do` is a Hoon functino (gate) application. It can also be in a pipeline.

Its syntax is a hoon expression preceeded by `_`:

    ~fintud-macrep:dojo> _lore 'hello\0aworld'
    ~fintud-macrep:dojo> _|=(a=@ (mul 3 a))} (add 2 2)

##### `[%ur p=purl]`

A simple HTTP get produces the result as a `%httr` noun.

### Development

Developing dojo generators is the easiest form of Hoon programming.
Generator scripts are found in the `gen` folder.

#### Configuration

All generator scripts are configured with the same configuration gate:

    |=  $:  [now=@da eny=@ bec=beak]
            [arg=my-arguments opt=my-options]
        ==

We try to minimize boilerplate, but you can't get out of this
one.  The dojo will slam this configuration gate to create your
generator.

The head of the sample is a system context.  `now` is the date of
the call; `eny` is 256 bits of entropy; `bec` is a triple
`[p=ship q=desk r=case]` (ie, the root of a filesystem path).
This beak is the path to the script, not the current path within
the dojo (dojo variables are not, unlike in Unix, visible to
generator scripts).

`arg` and `opt` are whatever you want them to be.  (Use `~` if
you have no arguments or options.)  The dojo will replace `arg`
with the user's ordered arguments, and replace any options in
`opt` specified by the user's named arguments.  (More exactly,
if the user specifies `=foo 42`, your `opt` is replaced with
`opt(foo 42)`.)

Bear in mind that dojo syntax is list-centric, so your `arg` will
always end with a `~`.  For instance,

    ~fintud-macrep/dojo> +fun/make 1 2 3

will generate an `arg` of `[1 2 3 ~]`.  Yes, this is the only
place in Urbit where we do list-centric arguments.

Note also that script configuration is typed.  The user's command
will fail if there's a type mismatch.  But `arg` does not have to
be a homogeneous list -- just a tuple with `~` on the end.  Also,
you can use `arg=*` and sort out the nouns by hand.  Any value
you don't care about can simply be `*`.

#### Generators

There are four kinds of generators: builders (with no special 
I/O), dialogs (which read console input), scrapers (which pull
data from the webs), and synthesizers (which produce another
generator).  Any generator can use `.^` to both read from and
block (wait for remote or delayed results) on the Urbit global
namespace.

A generator produces a cell whose tail is the configuration gate,
and whose head is `%say` for a builder, `%ask` for a dialog, 
`%get` for a scraper, and `%con` for a constructor.

#### Builders

A builder just produces a cask (mark-value cell) directly from
the configuration gate.  Here's the simplest builder, with a 
blank configuration:

    :-  %say  |=  *
    :-  %noun
    "hello, world."

#### Dialogs

A dialog is a console input form.  We recommend using the helpful
`sole` structures, with

    /-  *sole

(If you're interested in building your own dialogs without `sole`
(not that complicated at all), it's easiest to start by
reverse-engineering `sole`.)

Otherwise, a dialog boilerplate (with blank configuration), which
generates a `my-result` result with mark `%my-result-mark`:

    :-  %ask  |=  *
    ^-  (sole-result (cask my-result))
    %+  sole-so  %my-result-mark
    *my-result

Internally, a `++sole-result` is either a final result or a new
dialog core with a new prompt.  The dojo keeps running the dialog
until it produces a final result.

A dialog step can do one of three things: print a message, prompt
for a value, or finish with a result.  These are done with
`sole-yo`, `sole-lo`, and `sole-so` respectively.  Here's a
simple dialog which uses all of them:

    :-  %ask  |=  *
    ^-  (sole-result (cask ,@ud))
    %+  sole-yo  leaf/"let's multiply two numbers..."
    %+  sole-lo  [%& %number "number one: "]
    %+  sole-go  dim:ag
    |=  one=@ud
    %+  sole-lo  [%& %number "number two: "]
    %+  sole-go  dim:ag
    |=  two=@ud
    %+  sole-so  %noun
    (mul one two)

`++sole-yo` prints a tank (prettyprint structure).  See `++tank`
in hoon.hoon.

`++sole-lo` takes a prompt and a new dialog.  In the example,
`[%& %number "your number: "]` is a `++sole-prompt`.  `&` as
opposed to `|` means input is echoed (not a password).
`%number` is a history label; all inputs with the same label
share the same history buffer.

The `++sole-dialog` is generally built with `++sole-go`, as used
above.  This takes a parsing `++rule` (here `dim:ag`, which
parses a decimal), and a gate whose sample is the parsed value,
producing a new dialog.

#### Scrapers

Most stuff on the internets is crap, but there's exceptions.
Sometimes it's nice to get it and compute functions on it.

A scraper is much like a dialog, except instead of `sole-lo` and 
`sole-go` it uses `sole-at`.

    :-  %get  |=  *
    %+  sole-yo  leaf/"Fetching example.com"
    %+  sole-at  [[& ~ `/com/example] `/ ~]
    |=  hit=httr
    %+  sole-yo  leaf/"Fetched."
    %+  sole-so  %httr
    hit

`++sole-at` takes a `purl` request url, and a gate through
which to slam the result `httr`.

#### Synthesizer

A synthesizer simply produces another recipe.  Its 

