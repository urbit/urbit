`+ticket`
===================

Connecting your planet to moons.

The Urbit namespace is designed for you to have a planet in the
cloud and your other devices (pc, laptop, phone) are moons.  Once you
have a moon, you can setup 2 way sync of various desks, much like
Dropbox or a reactive git.  If you `|mount` the desks, you can 
have synchronization between directories on your moon and planet's 
systems.

Creating your moon
------------------

Each planet can issue 2^32 moons. Moon names look like two planet
names glued together and always end with the signing planet name. So
for the planet `~matfeb-sablud` one could generate a moon `~talsur-
todres-matfeb-sablud`.

### On your planet

All of your moons will sync from your `%kids` desk. Your planet
should come with one by default, but let's check to see:

```
> +ls /=kids=
```

You should get a list of directories. If you just get `~` you can
set one up with the following command:

```
>|sync %kids our %base
```

To generate a random moon you can run the following in the `dojo`:

``` 
+ticket =+(`@p`(cat 5 our (mod eny (pow 2 32))) ~&(- -)) 
```

The output will print your moon name above the line with the
command. It will look something like this:

``` 
~some-moon-some-planet 
> +ticket =+(`@p`(cat 5 our (mod eny (pow 2 32))) ~&(- -))
~some-ticket 
```

You'll use both of these values to create your moon in the next
step.

### On your PC/laptop

```
> bin/urbit -w some-moon-some-planet -t some-ticket
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