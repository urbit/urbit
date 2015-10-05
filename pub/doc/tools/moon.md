`+ticket`
===================

Connecting your planet to moons.

The Urbit namespace is designed for you to have a planet in the
cloud and your other devices (pc, laptop, phone) are moons.  Once you
have a moon, you can setup 2 way sync of various desks, much like
Dropbox or a reactive git.  If you `|mount` the desks, you can 
have synchronization between directories on your moon and planet's 
systems.

Creating your moon name
-----------------------

First, for creating your moon you need to come up with a moon name.
For this you need a number less than 2^32 (since every planet has 
2^32 moons) in it's phonemic representation or `@p`:

```
> `@p`0x12.3456
~hobdyl-pontyr
```

Take your partial name above and append it with your planet name to
get your full moon name, in this case `~hobdyl-pontyr-matfeb-sablud` for my planet `~matfeb-sablud`.

Creating your moon
------------------

On your planet:

```
> |sync %kids our %base :: only have to do this once
> +ticket ~hobdyl-pontyr-matfeb-sablud  :: has to be done for every moon
~some-ticket
```

On your pc/laptop:

```
> bin/urbit -w hobdyl-pontyr-matfeb-sablud -t some-ticket
```

Wait for it to boot up and there you have it.  You've created your
moon which is tied to your planet.

When you first boot your moon you will have a shell that is connected
to your planet.  This might or might not be what you want.  To get a
local shell, type `^d` at the dojo prompt, which should drop you into
the task manager.  Then type `*dojo` to get a shell connected to your
moon.

Setting up 2-way sync
---------------------

To create a 2 way syncronized desk between your moon and your
planet, simply do the following:

On the moon:

```
> |sync %home ~matfeb-sablud %home
> |mount /=home= %home    :: So you can see the files from Unix
```

On the planet:

```
> |sync %home ~hobdyl-pontyr-matfeb-sablud %home
```

The initial sync takes a little while, but after that you should be
able to create and edit files and have them sync up on the paired
system, much like your own personal Dropbox.

---

[This guide brought to you by `~matfeb-sablud`]