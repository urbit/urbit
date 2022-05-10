# TODO

[ ] Dump keikaku into this list.
[ ] Rewrite this very out of date list. Wow.

Consider a subject of type

    $%  [%a ?(%a %b)]
        [%b ?(%a %b)]
    ==

Then in

    ?:  ?=([%a %a] .)
      X
    Y

at X the subject should be of type {$a $a}, but at Y the subject should be of
type {?(%a %b) ?:(?=(%a +3) $b ?(%a %b))}.

Continuing from Y, if we do

    ?:  ?=([%a %b] .)
      Z
    W

then at w, the type should be {$b ?(%a %b)}. How does this occur? When we fuse
%a with the head type ?(%a %b), the result is $a, with a seminoun %a. When we
eval the tail type expression against this, we get $b. Now, cropping from $b the
tail of the pattern %b gets us void, so the %a-head branch is dead here. We
proceed to $b in the head with no change to the tail.


Meanwhile against a subject of type

    [[? ?] ?]

the test

    ?:  ?=([[& &] &] .)
      X
    Y

gives rise to type {{? ?} ?:(?=([& &] +3) $| ?)}, and

   [? ? ?]

with

    [& & &]

gives rise to type

   {? ?:(?=(& +3) +3) {? ?:(?=(& +3) $| ?)} {? ?}}

## Based Hoon

[x] Get rid of shitty special case ideas from pairing with Phil
[x] Make Name eval away
[ ] Make eval take a type and rely on its facial structure in look via find
[ ] Fix all call sites
[ ] Test it
[ ] Ugh review all past tests

## Begin work on subject-oriented prototype in parallel

[ ] Figure out when I want to do this. Maybe just when stuck / bored / want to.
[ ] Start this todo list

## Pattern matching

[ ] Establish mentally a notion of "derms" which are skinlike Codes
[ ] Establish work and play for derms
[ ] Establish "meld" (type refinement) with derms or whatever this was
    - Fix this todo item
[ ] Pushing semantics for facial extraction
[ ] The Cas rule (do we still need?)
[ ] Test

## $@, $^
[ ] Establish syntax for "power types" $(...), $<
[ ] Establish a better name for power types
[ ] Desugar power types
[ ] Implement the proper semantics for power types, possibly cf Cardelli 1980s
[ ] Add syntax for $@, $^
[ ] Add desugaring
[ ] Implement nesting rules
[ ] Implement work/play in types with a view towards the power constraint
[ ] Adjust work/play on pattern matching
[ ] Test

## Recursive types
[ ] Re-find readings.
[ ] Read that reading I found to see if we actually benefit from doing something
    more interesting from the obvious.

## Repl
[ ] Establish a skin that does multiline reading using readline and echos
[ ] Feed through the layers and print type and value
[ ] Add the ability to import a standard library
[ ] BONUS figure out multiline history properly goddamn it

## Other things
[ ] I've wontfixed on variance of higher order types. Don't expect anyone to
    care for a while, but this is a potential research project later, maybe for
    an intern
[ ] The $ syntax has garnered suspicion.
[ ] The reversal of the subject has garnered suspicion.
[ ] We need to understand caching compilers a la ollef but in the applicative
    context.
[ ] We probably also need to understand Jared's work on caching better.
[ ] Do we have a problem equating $: and :-? The problem I have in mind is that
    the right hand side of $: needs to be evaluated relative to an enlarged
    subject, but this is not the case with :-.
[ ] Keep thinking, on an ongoing basis, about unifying $= with ^=.
[ ] Remember whether %= needs to wait for subject orientation. I think so.
[ ] Look over all XXs and FIXMEs
[ ] Should we model a wing as an empty centis?
