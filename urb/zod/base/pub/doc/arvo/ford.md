<div class="short">

`%ford`
=======

Our typed and marked computation engine.

A variety of different services are provided by `%ford`, but they mostly
involve compiling hook files, slapping/slammming code with marked data,
and converting data between marks, including validating data to a mark.
Throughout every computation, `%ford` keeps track of which resources are
dependencies so that the client may be aware when one or more
dependencies are updated.

`%ford` neither accepts unix events nor produces effects. It exists
entirely for the benefit of applications and other vanes, in particular
`%gall`. `%eyre` exposes the functional publishing aspects of `%ford`
while `%gall` uses `%ford` to control the execution of applications.
`%clay` is intended to use `%ford` to managed marked data, but this is
not yet reality.

</div>

------------------------------------------------------------------------

<list></list>

------------------------------------------------------------------------

Cards
=====

`%ford` accepts just one card, `%exec`. This is misleading, however,
since there are fourteen different `silk`s that may be used with it. In
every case, the expected response to a `%exec` card is a `%made` gift
with either an error or the produced result along with its set of
dependencies.

Silks may autocons, so that the product of a cell of silks is a cell of
the product of the two silks.

[`%bake`](#bake)
================

Tries to functionally produce the file at a given beam with the given
mark and heel. It fails if there is no way to translate at this level.

[`%boil`](#boil)
================

Functionally produces the file at a given beam with the given mark and
heel. If there is no way to translate at this beam, we pop levels off
the stack and attempt to bake there until we find a level we can bake.
This should almost always be called instead of `%bake`.

[`%call`](#call)
================

Slams the result of one silk against the result of another.

[`%cast`](#cast)
================

Translates the given silk to the given mark, if possible. This is one of
the critical and fundamental operations of ford.

[`%diff`](#diff)
================

Diffs the two given silks (which must be of the same mark), producing a
cage of the mark specified in `++mark` in `++grad` for the mark of the
two silks.

[`%done`](#done)
================

Produces exactly its input. This is rarely used on its own, but many
silks are recursively defined in terms of other silks, so we often need
a silk that simply produces its input. A monadic return, if you will.

[`%dude`](#dude)
================

Computes the given silk with the given tank as part of the stack trace
if there is an error.

[`%dune`](#dune)
================

Produces an error if the cage is empty. Otherwise, it produces the value
in the unit.

[`%mute`](#mute)
================

Takes a silk and a list of changes to make to the silk. At each wing in
the list we put the value of the associated silk.

[`%pact`](#pact)
================

Applies the second silk as a patch to the first silk. The second silk
must be of the mark specified in `++mark` in `++grad` for the mark of
the first silk.

[`%plan`](#plan)
================

Performs a structured assembly directly. This is not generally directly
useful because several other silks perform supersets of this
functionality. We don't usually have naked hoods outside ford.

[`%reef`](#reef)
================

Produces a core containing the entirety of zuse and hoon, suitable for
running arbitrary code against. The mark is `%noun`.

[`%ride`](#ride)
================

Slaps a twig against a subject silk. The mark of the result is `%noun`.

[`%vale`](#vale)
================

Validates untyped data from a ship against a given mark. This is an
extremely useful function.
