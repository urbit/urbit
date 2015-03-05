Doing development can be a messy process. Since Urbit ships are meant to
last forever it can be convenient to development on a disposable ship as
to not permanently destroy any of your own part of the urbit network. In
this short guide we're going to go over how to set up a fake network for
development on a single physical machine.

This guide assumes that you have already followed the [setup
instructions](). Going forward we'll refer to the directory you cloned
the repo in as `/$URB_DIR`.

1.  

Start a fake `vere` for the carrier `~zod`.

In `/$URB_DIR`, run

        $ bin/vere -F -I ~zod -c zod

This will boot `vere` into the carrier `~zod`. Because we're using the
flag `-F` `vere` doesn't check any of the keys to confirm that we are in
fact the owner of `~zod`. We use `-I` here to signal to `vere` that we
want to start an 'imperial' ship, or carrier. `-I` takes a ship name.
You can enter any one of the 256 Urbit carriers. More information on
`vere` and its command line options can be found [here]().

You should see `vere` start as usual, although instead of copying its
files from a parent ship the files are copied from `urb/zod` inside your
Urbit directory.

For most development tasks, using a fake carrier works really well. You
get a very short name, and a safe testing environment. If you need to
test out a collection of ships talking to each other, let's keep going.

2.  

Start a second fake `vere`.

In a new terminal, cd to `/$URB_DIR` and run:

        $ bin/vere -F -c doznec

Since we don't specify a carrier with `-I` here, this should boot into a
submarine, as if you had started vere normally. In your running fake
`~zod` you should see a [`~&`]() alerting you that the sub you just
started is your neighbor. This means that a key exchange has happened,
and your packets are being transmitted directly. Now, let's get a ticket
for a cruiser.

In your fake `~zod`, ticket a cruiser:

        ~zod/try=> :ticket ~doznec

This line should return a `[ship: ticket]` pair of [`@p`](). You can Now
you can return to your submarine and run:

        ~sipmyl-wolmeb-haswel-losmyl--dibten-holdyn-dacdyn-natsep/try=> :begin

Use the `[ship: ticket]` pair you got from your fake `~zod` to complete
the `:begin` process for `~doznec`. When finished you should see
something like `; ~doznec _doz_ is your neighbor` on your fake `~zod`.

You can repeat this process on `~doznec`, ticketing destroyers that are
children of `~doznec` by running `:ticket` with a valid destroyer.
`:ticket` actually takes two arguments, a ship name and a number
indicating how many tickets to generate. `~tasfyn-partyv` is the first
destroyer under `~doznec`, so you can run `:ticket ~tasfyn-partyv 5` to
get five `[ship: ticket]` pairs.

3.  

Add some files, make sure the network if functioning.

You should now have a directory `/$URB_DIR/zod`.

In

    /$URB_DIR/zod/zod/try/test.txt

Put

        hello from mars

You should see a sync event on `~zod` indicated by a `+` with the file
path, and at least one `%merge-fine` messages on `~doznec`. On your
filesystem, you should see the file mirrored in
`/$URB_DIR/doznec/doznec/try/test.txt`.

You can also send a few `:hi` messages over the network. On `~doznec`
try:

        ~doznec/try=> :hi ~zod "just checking in from urth"

You should see the message appear on your `~zod` and get a
`hi ~zod successful` on your `~doznec`.

This is a good way to set up a test environment where you can try
anything out and break anything you want. When working in these test
environments it is usually safest to keep your working files outside of
your pier and copy them in. This way you can quickly
`rm -rf zod/ && bin/vere -F -I ~zod -c zod` to start again.
