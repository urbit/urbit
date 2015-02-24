This guide is intended to get you oriented in the Arvo command prompt
and give you a tour of some basic utilities. The command prompt comes in
two flavors, in a web browser and in a terminal. For the most part
they're the same, except that in a browser you can evaluate tall-form
Hoon expressions but you can't run readline apps, such as `:chat`.

Every Arvo command prompt is also a Hoon REPL. The command line is a
great place to test out your hoon knowledge. In this guide we're just
going to talk about some basic system utilities and get comfortable
moving around in `%clay`. If you'd just like to see a list of
command-line utilities, you can find the Arvo man pages [here]().

This rudimentary tour should work well in both places.

1

Move around `%clay`

After finishing the [setup instructions]() you should have an Arvo
prompt that looks like this:

    ~talsur-todres/try=>

The path at the beginning of your prompt is actually a path in the
global filesystem of Urbit, called `%clay`. Since `%clay` is universal
across all of Urbit, each full path starts with a ship name. `%clay` is
also versioned on a per-desk basis. Desks are the top-level directories
in your pier.

Moving around `%clay` is simple. There is no equivalent of `cd`.
Instead, just type a valid path name at the prompt to move to that
directory. Here we'll move to our starting root path in the try desk,
`/try=` to the `main` desk:

    ~talsur-todres/try=> /=main=
    =% /~talsur-todres/main/0
    ~talsur-todres/main=> 

We have two shortcuts in `%clay` that are worth noting, `=` and `%`.

`=` copies in some corresponding part of our current path. In the second
line above you can see how the `=` in `/=main=` pull in the
`~talsur-todres` and `0` in from our starting directory,
`/~talsur-todres/try/0`. It's important to note that our full prompt to
start is `/~talsur-todres/try=`, where the trailing `=` indicates the
current revision. In the shell, revision `0` never exists — it's used as
a pointer to the head.

`%` is similar to `.` in unix:

    ~talsur-todres/main=> %
    =% /~talsur-todres/main/0
    ~talsur-todres/main=> %%
    [~.~talsur-todres ~.main ~]
    ~talsur-todres/main=> %%%
    [~.~talsur-todres ~]
    ~talsur-todres/main=> %%%%
    ~

When using `%` to move around in `%clay` you need to make sure to use
leading and trailing `/` to ensure your path is interpolted correctly:

    ~talsur-todres/main=> /%%%/try=
    =% /~talsur-todres/try/0
    ~talsur-todres/try=> 

2

Create some revisions

Let's use `:into`, our simple utility for writing text to a file, to
create a new file:

    ~talsur-todres/try=> :into %/helo/txt 'helo mars'
    written
    ~talsur-todres/try=> 

To confirm that our file was written, we can use `:ls`. `:ls` prints a
list of directory contents, but requires that you specify a path. `%`
will suffice for the current path:

    ~talsur-todres/try=> :ls %
    readme helo
    ~talsur-todres/try=> 

Let's quickly switch back to a unix command prompt to see a few things
about both how files are synced between `%clay` and unix, and where your
apps live.

    my-pier/talsur-todres/$ ls try
    helo.txt    readme.md
    my-pier/talsur-todres/$ cat try/helo.txt
    helo mars

Here you can see that our files are synced back to unix as they are
changed in urbit, and vice-versa. As you change files in unix you'll see
those changes appear in `%clay`.

    my-pier/talsur-todres/$ ls main/app/
    bang        grep        poke        solid       unsync
    began       hi          radio       sync        verb
    begin       into        reload      talk        ye
    cat         label       reset       terminal
    chat        ls          rm          test
    cp          mv          shell       ticket
    my-pier/talsur-todres/$ cat main/app/ls/core.hook 
    !:
    |_  [hid=hide ~]
    ++  poke-ls-args
      |=  [ost=bone you=ship arg=path ~]
      =+  lon=((hard arch) .^(%cy arg))
      :_  +>.$
      :*  [ost %pass / %g %cide %$]
          [ost %give %nice ~]
          %+  turn  (~(tap by sup.hid))
          |=  [ost=bone *]
          :^  ost  %give  %rush
          :-  %tang
          :_  ~  
          :+  %rose  [" " ~ ~]
          (turn (~(tap by r.lon) ~) |=([a=@ta b=*] leaf/(trip a)))
      ==
    ++  peer
      |=
        *
      `+>
    --

Here you can see that `/main/app` is the main location where our apps
are stored, and the contents of the `:ls` app. urbit applications are of
course written in hoon, our naitive programming language. Don't worry
about the contents of the file for now. Since changes in unix are synced
back in to urbit, we can develop urbit programs by simply editing them
in our favorite editor and saving them.

For the time being let's switch back to urbit and update our file with
some new content, so we can see how `%clay` stores revisions.

    ~talsur-todres/try=> :into %/helo/txt 'gbye mars'
    written
    ~talsur-todres/try=> :ls /=try/1
    readme helo
    ~talsur-todres/try=> :cat /=try/1/helo/txt
    /~talsur-todres/try/9/helo/txt
    helo mars
    ~talsur-todres/try=> :cat /=try/2/helo/txt
    /~talsur-todres/try/10/helo/txt
    gbye mars
    ~talsur-todres/try=> :cat /=try=/helo/txt
    /~talsur-todres/try/~2014.11.26..01.06.33..c93a/helo/txt
    gbye mars
    ~talsur-todres/try=> 

Here we use `:ls` to investigate the filesystem across versions. You can
see that our `helo` file exists in our first revision. Using the simple
`:cat` command we can print the contents of `/=try/helo/txt` in its two
separate, versioned states.

We can even move to a different version of our desk and look around:

    ~talsur-todres/try=> /=try/1
    =% /~talsur-todres/try/1
    ~talsur-todres/try/1> :ls %
    readme helo
    ~talsur-todres/try/1>

This is sort of like being in a detached HEAD in git.

3

Start a yacht

Each Urbit destroyer can delegate around four billion yachts. Yachts are
also urbit ships, but are pegged to their parent identity, and are set
up to mirror their filesystem. We can generate a `[ship: ticket]` pair
for a yacht by using the `:ticket` utility:

    ~talsur-todres/try=> :ticket ~talsur-todres-talsur-todres
    ~talsur-todres-talsur-todres: ~figpem-fapmyl-wacsud-racwyd

Every yacht for a particular destroyer ends in the same `ship-name`, and
has every possible destroyer prefix. For example,
`~tasfyn-partyv-talsur-todres` is also a valid yacht from
`~talsur-todres`.

Start up a new `vere` process with something like `bin/vere -c yacht`.
Then run `:begin` and enter the `[ship: ticket]` pair you just generated
when prompted. When the process is complete you should get a
`; ~talsur-todres-talsur-todres :y1: is your neighbor` message on your
destroyer. To confirm that everything is working properly, you can use
`:hi` to send a message:

    ~talsur-todres/try=> :hi ~talsur-todres-talsur-todres "whats up"
    hi ~talsur-todres-talsur-todres successful
    ~talsur-todres/try=> 

Which will appear on your new yacht:

    < ~talsur-todres: whats up
    ~talsur-todres-talsur-todres/try=> 

You should also see the contents of your `/try` desk mirrored on your
yacht:

    ~talsur-todres-talsur-todres/try=> :ls %
    readme helo
    ~talsur-todres-talsur-todres/try=>

Making another change on your destroyer should automatically propagate
down to your yacht:

    ~talsur-todres/try=> :into %/helo/txt 'back to mars'
    written
    ~talsur-todres/try=>

    [%merge-fine ~talsur-todres %try]    
    ~talsur-todres-talsur-todres/try=> :cat %/helo/txt
    back to mars
    ~talsur-todres-talsur-todres/try=>

4

Move files around

Another familiar command line utility is `:mv`:

    ~talsur-todres/try=> :mv %/helo/txt %/test/helo/txt
    moved
    ~talsur-todres/try=>

    [%merge-fine ~talsur-todres %try]    
    ~talsur-todres-talsur-todres/try=> :cat %/test/helo/txt
    back to mars
    ~talsur-todres-talsur-todres/try=>

In `%clay` we don't use file extensions or folders. A path either does
or does not have anything in it. There's no need to do the equivalent of
`mkdir` before moving something.

We also implement the familiar `:rm`:

    ~talsur-todres/try=> :rm %/test/helo/txt
    removed
    ~talsur-todres/try=> :cat %/test/helo/txt
    file /~talsur-todres/try/~2014.11.26..16.49.52..3f5e/test/helo/txt not available
    ~talsur-todres/try=> 

    [%merge-fine ~talsur-todres %try]    
    ~talsur-todres-talsur-todres/try=> :cat %/test/helo/txt
    file /~tasfyn-partyv-talsur-todres/try/~2014.11.26..16.50.15..556b/test/helo/txt not available
    ~talsur-todres-talsur-todres/try=>
