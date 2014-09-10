Ames
====

Ames is our networking protocol.

First we give commentary on the code, the algorithms involved, and the
protocol.  We trace through the code touched when a packet is sent, received,
acknowledged, and that acknowledgment applied.  This is fairly comprehensive,
and contains many implementation details, but if you understand this, then you
understand Ames.

If you've scrolled down this page, you may be intimidated by the amount of Hoon
code, especially if you are new to the language.  Don't be afraid of it, you
don't have to read any of it if you don't want to -- every interesting action
the code takes is explained in plain English.  In fact, if you are new to the
language, this may be a good learning opportunity.  Even if you don't
understand every line of Hoon code, you'll hopefully be able to follow most
lines.  By the time you've worked through this, you'll have seen many common
patterns and best practices.  Hoon, much more than other languages, is best
learned by reading and understanding large quantities of existing code.  In
this way, it is similar to learning a natural language.  All of this code is in
`arvo/ames.hoon`.

After the commentary, we have reference documentation for all the data
structures that are specific to Ames.  If you see a data structure or a
variable used that you don't recognize, search for it in the code, and it's
very likely defined in one of these data structures.  We recommend that another
tab is kept open for easy access to the data structure reference documentation.
The code for these is split between `arvo/ames.hoon` and `arvo/zuse.hoon`.

The Lifecycle of a Packet (or, How a Packet Becomes Law)
--------------------------------------------------------

Here, we will trace a packet as it makes its way through ames.  There are
actually two pathways through ames:  the legacy path through `%want`, and the
modern way, entered through `%wont`, with full end-to-end acknowledgments.
Here we will only trace the modern way, though much of the path is the same for
both.

When an app (or a vane) wishes to send a packet to another ship, it must send a
`%wont` card:

```
              [%wont p=sock q=path r=*]                 ::  e2e send message
```

This card takes three arguments.  The `p` is a `sock`, that is, a pair of two
ships, the first of which is the sender and the second is the receiver.  But
wait, you ask, why do I get to decide who is the sender?  Can I fake like I'm
someone else?  The reason is that there are potentially multiple ships on the
same pier, and the kernel can send a message from any of them.  If you attempt
to send a message from a ship not on your pier, then ames will refuse to send
it.  If you hack around in your own copy of ames to go ahead and send it
anyway, then the other ship will reject it because your key is bad.  Only send
messages from yourself.

The `q` is a path, representing the place on the other side that you want to
receive your message.  It is approximately equivalent to a port number.
Messages on the same path are guaranteed to arrive in the same order as they
were sent.  No such guarantees are made across paths.

The `r` is the actual data that you are sending.  As the type implies, this can
be an arbitrary noun, and it will be transferred to the receiver exactly as-is,
in a well-typed way.  Of course, this is data that is sent over the wire, so be
careful not to send anything too massive unless you're willing to wait.

But enough about the interface.  Grepping in ames.hoon for `%wont`, we find
that it appears in exactly two places:  at its definition in `++kiss`, and in
`++knob`, where it is handled.  We see that we go directly into `++wise:am`.

```
    ++  wise                                            ::    wise:am
      |=  [soq=sock hen=duct cha=path val=* ete=?]      ::  send a statement
      ^-  [p=(list boon) q=fort]
      zork:zank:(wool:(ho:(um p.soq) q.soq) hen cha val ete)
```

The inputs to this gate are exactly the sort of thing you'd expect.  In
particular, everything in the `%wont` gate is here plus the calling duct so
that we know where to send the acknowledgment and `ete` to determine if we're
going to do the modern end-to-end acknowledgments.

The actual line of code looks intimidating, but it's really not all that bad.
Working from the inside out, the call to `++um` sets up our domestic server,
and the call to `++ho` sets up our knowledge about the neighbor we're sending
to.  From the outside, `++zork` and `++zank` just apply the changes made to our
`++um` and `++am` cores, respectively.  If you're familiar with the common
idiom of `++abet`, that's all this is.  The code predates the widespread usage
of that name.

The interesting part, then, is in `++wool:ho:um:am`.  Let's look at the code.

```
        ++  wool                                        ::    wool:ho:um:am
          |=  [hen=duct cha=path val=* ete=?]           ::  send a statement
          ^+  +>
          =+  ^=  rol  ^-  rill
              =+  rol=(~(get by ryl.bah) cha)
              ?~(rol *rill u.rol)
          =+  sex=sed.rol
          ::  ~&  [%tx [our her] cha sex]
          =.  ryl.bah
              %+  ~(put by ryl.bah)  cha
              rol(sed +(sed.rol), san (~(put by san.rol) sex hen))
          =+  cov=[p=p:sen:gus q=clon:diz]
          %+  wind  [cha sex]
          ?:  ete
            [%bund q.cov cha sex val]
          [%bond q.cov cha sex val]
```

This is slightly more complicated, but it's still not all that bad.  Our
inputs, at least, are fairly obvious.

If you glance at the code for a second, you'll see that `++wind:ho:um:am` seems
to be able to send a message, or `++meal`, given a `++soup`.  This gate, then,
just sets up the things we need to for `++wind` to do its job.

We first get `rol`, which is a `++rill`, that is, a particular outbound stream.
This stream is specific to the path on which we're sending.  If the path hasn't
been used before, then we create it.  We let `sex` be the number of messages
we've already sent on this path.

Then, we update the outbound stream by incrementing the number of messages sent
and placing an entry in `san.rol` that associates the message number with the
`duct` that sent the message.  This allows us to give the acknowledgment to the
one who sent the message.

We let `cov` be the current life of our crypto and our neighbor's crypto.  At
the moment, we only need our neighbor's life, which we put into the meal.

Finally, we call `++wind:ho:um:am` with the `++soup` of the path and message
number and the `++meal` of the payload itself.  For end-to-end acknowledged
messages, we use `%bund`.

```
              [%bund p=life q=path r=@ud s=*]           ::  e2e message
```

Looking at how we create the `%bund`, we can easily see what each field is for.

Following the trail a little further, we go to `++wind:ho:um:am`.

```
        ++  wind                                        ::    wind:ho:um:am
          |=  [gom=soup ham=meal]
          ::  ~&  [%wind her gom]
          ^+  +>
          =^  wyv  diz  (zuul:diz now ham)
          =^  feh  puz  (whap:puz now gom wyv)
          (busk xong:diz feh)
```

`++wind` does three things:  it (1) encodes the message into a list of
possibly-encrypted packets, (2) puts the message into the packet pump, and (3)
sends any packets that are ready to be sent.  Yes, our nice little linear run
of each gate calling exactly one other interesting gate is over.  We'll go in
order here.

`++zuul:lax:as:go` is the what converts a `++meal` into a list of actual, 1KB
packets.

```
        ++  zuul                                        ::    zuul:lax:as:go
          |=  [now=@da ham=meal]                        ::  encode message
          ^-  [p=(list rock) q=_+>]
          =<  weft
          ++  wasp                                      ::  null security
          ++  weft                                      ::  fragment message
          ++  wisp                                      ::  generate message
```

For organizational purposes, `++zuul` constructs an internal core with three
arms.  `++wasp` encodes the meal into an atom with no encryption.  `++wisp`
encodes a meal with possible encryption (else it simply calls `++wasp`).
`++weft` takes the result of `++wisp` and splits it into actual packets.

```
          ++  wasp                                      ::  null security
            ^-([p=skin q=@] [%none (jam ham)])
```

This simply jams the meal, wrapping it with the `skin` of `%none`, meaning no
encryption.

Since `++wisp` is a little long, we'll go through it line-by-line.

```
          ++  wisp                                      ::  generate message
            ^-  [[p=skin q=@] q=_..wisp]
```

`++wisp` produces a pair of a `skin` and an atom, which is the meal encoded as
a single atom and possibly encrypted.

```
            ?:  =(%carp -.ham)
              [wasp ..wisp]
```

If the meal that we're encoding is a `%carp`, then we don't encrypt it.  A
`%carp` meal is a partial meal, used when a message is more than 1KB.  Since
the entire message is already encrypted, we don't need to encrypt each packet
individually again.

```
            ?:  !=(~ yed.caq.dur)
              ?>  ?=(^ yed.caq.dur)
              :_  ..wisp
              :-  %fast
              %^  cat  7
                p.u.yed.caq.dur
              (en:r:cluy q.u.yed.caq.dur (jam ham))
```

If we have a symmetric key set up with this neighbor, then we simply use it.
The skin `%fast` is used to indicate a symmetric key.

```
            ?:  &(=(~ lew.wod.dur) |(=(%back -.ham) =(%buck -.ham)))
              [wasp ..wisp]
```

If we do not yet have our neighbor's will, then there is no way that we can
seal the message so that only they may read it.  If what we're sending is an
acknowledgment, then we go ahead and just send it in the clear.

```
            =^  tuy  +>.$
              ?:(=(~ lew.wod.dur) [*code +>.$] (griz now))
```

If we don't have our neighbor's will, then we "encrypt" with a key of 0.  If we
do have their will, then we generate a new symmetric key that we will propose.

```
            :_  ..wisp
            =+  yig=sen
            =+  bil=law.saf                             ::  XX send whole will
            =+  hom=(jam ham)
```

`yig` will be the life and engine for our current crypto.  `bil` is our will.
`hom` is the meal encoded as a single atom.

```
            ?:  =(~ lew.wod.dur)
              :-  %open
              %^    jam
                  [~ `life`p.yig]
                bil
              (sign:as:q.yig tuy hom)
```

If we do not have our neighbor's will, then we send our current life along with
our will and the message.  The message itself is "signed" with a key of 0.

```
            :-  %full
              =+  cay=cluy
              %^    jam
                  [`life`p.cay `life`p.yig]
                bil
              (seal:as:q.yig pub:ex:r.cay tuy hom)
          --                                            ::  --zuul:lax:as:go
```

If we do have our neighbor's will, then we send our perception of their current
life, our current life, our will, and the message.  The message is sealed with
their public key so that only they can read our message.

Once we have the message encoded as an atom, `++weft` goes to work.

```
          ++  weft                                      ::  fragment message
            ^-  [p=(list rock) q=_+>.$]
            =^  gim  ..weft  wisp
            :_  +>.$
            ^-  (list rock)
```

We're going to produce a list of the packets to send.  First, we use the
aforementioned `++wisp` to get the message as an atom.

```
            =+  wit=(met 13 q.gim)
            ?<  =(0 wit)
```

`wit` is the number of 1KB (2^13 bit) blocks in the message.  We assert that
there is at least one block.

```
            ?:  =(1 wit)
              =+  yup=(spit [our her] p.gim q.gim)
              [yup ~]
```

If there is exactly one block, then we just call `++spit` to turn the message
into a packet.  We'll explain what `++spit` does momentarily.

```
            =+  ruv=(rip 13 q.gim)
            =+  gom=(shaf %thug q.gim)
            =+  inx=0
```

If there is more than one block, then we rip it into blocks in `ruv`.  `gom` is
a hash of the message, used as an id.  `inx` is the number of packets we've
already made.

```
            |-  ^-  (list rock)
            ?~  ruv  ~
            =+  ^=  vie
                %+  spit
                  [our her]
                wasp(ham [%carp (ksin p.gim) inx wit gom i.ruv])
            :-  vie
            $(ruv t.ruv, inx +(inx))
```

Here we package each block into a packet with `++spit` and produce the list of
packets.

```
  ++  spit                                              ::  cake to packet
    |=  kec=cake  ^-  @
    =+  wim=(met 3 p.p.kec)
    =+  dum=(met 3 q.p.kec)
    =+  yax=?:((lte wim 2) 0 ?:((lte wim 4) 1 ?:((lte wim 8) 2 3)))
    =+  qax=?:((lte dum 2) 0 ?:((lte dum 4) 1 ?:((lte dum 8) 2 3)))
    =+  wix=(bex +(yax))
    =+  vix=(bex +(qax))
    =+  bod=:(mix p.p.kec (lsh 3 wix q.p.kec) (lsh 3 (add wix vix) r.kec))
    =+  tay=(ksin q.kec)
    %+  mix
      %+  can  0
      :~  [3 1]
          [20 (mug bod)]
          [2 yax]
          [2 qax]
          [5 tay]
      ==
    (lsh 5 1 bod)
```

This is how we turn a message into a real packet.  This has the definition of
the packet format.

`wim` is the length of the sending ship, and `dum` is the length of the
receiving ship.  There are only five possibilities for each of those,
corresponding to carriers, cruisers, destroyers, yachts, and submarines.  These
are encoded in `yax` and `qax` as 0, 0, 1, 2, and 3, respectively.  Thus, `wix`
and `vix` are the number of bytes that must be reserved for the ship names in a
packet.

Next, we construct `bod` by simply concatenating the sending ship, the
receiving ship, and the body of the message.  Then, we get the encryption
mechanism from `++skin`, which may be a 0, 1, 2, or 3, and put it in `tay`.

Next, we concatenate together, bit by bit, some final metadata.  We use three
bits for our protocol number, which is incremented modulo eight when there is a
continuity breach or the protocol changes.  We use the final twenty bits of a
hash of the body (which, we suppose, makes it a twenty bit hash) for
error-checking.  We use two bits to tell how much room is used in the body for
the sending ship, and another two bits for the receiving ship.  Finally, we use
five bits to store the encryption type.  Note that since there are only two
bits worth of encryption types, there are three unused bits here.  This adds up
to 32 bits of header data.  Finally, we concatenate this onto the front of the
packet.  Thus, we can summarize the packet header format as follows.

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|Proto|             Hash of Body              |yax|qax| Crypto  |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

After this, there are `yax` bits of the sender name, `qax` bits of the receiver
name, and up to 8192 bits of data.  Thus, the maximum size of a packet is
achieved in a message between two submarines with 8192 bits of data.  This will
require 32+128+128+8192 = 8480 bits, or 1060 bytes.

This concludes our discussion of `++zuul:lax:as:go`.  If you recall from
`++wind:ho:um:am`, the list of packets from `++zuul` is passed into `++whap:pu`
to update the packet pump and get any packets that can be sent immediately.

```
    ++  whap                                            ::    whap:pu
      |=  [now=@da gom=soup wyv=(list rock)]            ::  send a message
      ^-  [(list rock) _+>]
      =.  pyz  (~(put by pyz) gom (lent wyv))
      =.  +>
        |-  ^+  +>.^$
        ?~  wyv  +>.^$
        %=  $
          wyv  t.wyv
          nus  +(nus)
          diq  (~(put by diq) (shaf %flap i.wyv) nus)
          puq  (~(put to puq) [nus `soul`[gom 0 | ~2000.1.1 i.wyv]])
        ==
      (harv now)
```

First, we put into `pyz` the id for this message and the number of its packets
that have not yet been acknowledged, which is of course the total number of
packets since we haven't even sent the packets.

For every packet, we change three things in the state (`++shed`) of our packet
pump:  (1) we increment `nus`, the number of packets sent; (2) we put the
packet number into `diq` keyed by a hash of the packet; and (3) we put the
packet into the packet queue, with the basic metadata of its id `gom`, 0
transmissions, not live yet, last sent in the year 2000, and the packet itself.

Finally, we harvest the packet pump.

```
    ++  harv                                            ::    harv:pu
      |=  now=@da                                       ::  harvest queue
      ^-  [(list rock) _+>]
      ?:  =(~ puq)  [~ +>(rtn ~)]
      ?.  (gth caw nif)  [~ +>]
      =+  wid=(sub caw nif)
      =|  rub=(list rock)
      =<  abet  =<  apse
      |%
```

`++harv` contains a core for most of its work.  The meat is in `++apse`.
First, though, it sets itself up.  If there aren't any packets in the queue,
then we simply do nothing except set `rtn`, our next timeout, to nil because we
don't have any packets that may need to be retransmitted.  If we have more live
(that is, sent and unacknowledged) packets than our window size, then we don't
do anything.

Otherwise, we let `wid` be the width of our remaining packet window, and we
initialize `rub` to nil.  `rub` will be the list of packets that are ready to
be sent.  We then call `++apse` and pass the result to `++abet`.  `++apse`
decides which packets are ready to be sent.

```
      ++  apse
        ^+  .
        ?~  puq  .
        ?:  =(0 wid)  .
        =>  rigt  =<  left
        ?>  ?=(^ puq)
        ?:  =(0 wid)  .
        ?.  =(| liv.q.n.puq)  .
        ::  ~&  [%harv nux.q.n.puq p.n.puq]
        %_    .
          wid          (dec wid)
          rub          [pac.q.n.puq rub]
          nif          +(nif)
          liv.q.n.puq  &
          nux.q.n.puq  +(nux.q.n.puq)
          lys.q.n.puq  now
        ==
```

If there are no remaining packets to send, or if we've filled the packet
window, do nothing.  We call `++rigt` and `++left` to process the left and
right branches of the packet queue.

Now we assert that the queue is not empty, and we again check that we haven't
filled the packet window.  We will operate on the head of the queue.  If the
packet is live, then do nothing.  Otherwise, we go ahead and send it.

To send, we (1) decrement `wid`, our packet window width; (2) cons the packet
onto the `rub`, which will be returned as the list of packets to send; (3)
increment `nif`, the number of live packets; (4) set the packet to be live;
(5) increment the number of transmissions of the packet; and (6) set the last
sent time of the packet to now.

```
      ++  left
        ?>  ?=(^ puq)
        ^+(. =+(lef=apse(puq l.puq) lef(puq [n.puq puq.lef r.puq])))
      ++  rigt
        ?>  ?=(^ puq)
        ^+(. =+(rig=apse(puq r.puq) rig(puq [n.puq l.puq puq.rig])))
```

These do exactly what you would expect:  they traverse the packet queue so that
`++apse` gets called recursively through it.

Finally, `++abet` gets called, which resolves the changes.

```
      ++  abet
        ?~  rub  [~ +>.$]
        [(flop rub) +>.$(rtn [~ (add rto now)])]
```

This returns the packets that we wish to send, and it updates the timeout so
that we know when to try resending unacknowledged packets.

This concludes our discussion of `++whap:pu`.  To finish `++wind:ho:um:am`, we
just need to delve into `++busk:ho:um:am`.  But wait, in the call to `++busk`,
the first argument is `xong:diz`.  What is this?  This, my dear reader, is one
more detour, this time into `++xong:lax:as:go`.

```
        ++  xong                                        ::    xong:lax:as:go
          ^-  (list ship)                               ::  route unto
          =+  [fro=xen too=xeno]
          =+  ^=  oot  ^-  (list ship)
              =|  oot=(list ship)
              |-  ^+  oot
              ?~  too  ~
              ?:  (lien fro |=(a=ship =(a i.too)))  ~
              [i.too $(too t.too)]
          ::  ~&  [%xong-to [our her] (weld oot ?>(?=(^ fro) t.fro))]
          (weld oot ?>(?=(^ fro) t.fro))
```

This gets the list of intermediate ships needed to get a packet from us to our
neighbor.  First, we get `fro` and `too`, the "canons" of ourself and our
neighbor, respectively.

What is this "canon", you ask?  A canon is simply a ship plus its "ancestors",
as defined by `++sein`.  For example, the canon of `~hoclur-bicrel` is:

```
~hoclur-bicrel/try=> (saxo ~hoclur-bicrel)
~[~hoclur-bicrel ~tasruc ~tug]
```

If we follow the algorithm in `++xong`, we see that we are simply creating a
list of ships that form a path from our neighbor to ourself.  Essentially, we
look through the canon of our neighbor until we find something in our own
cannon -- a common ancestor.  Or, if we are from different carriers, then there
is no common ancestor.  We then weld this onto the tail of our own canon.  In
the end, this is simply a list of possible ships to try to route via to get to
our neighbor, ordered by preferability (that is, closeness to our neighbor).
We will end up trying, in order, to find a lane to these.

Now, we can finally get to `++busk:ho:um:am`.

```
        ++  busk                                        ::    busk:ho:um:am
          |=  [waz=(list ship) pax=(list rock)]         ::  send packets
          %_    +>
              bin
            |-  ^+  bin
            ?~  pax  bin
            $(pax t.pax, bin (weld (flop (wist:diz now waz ~ i.pax)) bin))
          ==
```

Thankfully, `++busk` is fairly simple.  We go through the list of packets and
convert them to `++boon`s with `++wist:lax:as:go`.  These boons are placed into
`bin`, and they end up getting processed by `++clop` (this happens in
`++knob`).

```
        ++  wist                                        ::    wist:lax:as:go
          |=  $:  now=@da                               ::  route via
                  waz=(list ,@p)
                  ryn=(unit lane)
                  pac=rock
              ==
          ^-  (list boon)
          ?:  =(our her)  [[%ouzo *lane pac] ~]
          ?~  waz  ~
          =+  dyr=?:(=(her i.waz) dur (gur i.waz))
          ?.  ?&  !=(our i.waz)
                  ?=(^ lun.wod.dyr)
              ==
            $(waz t.waz)
          :_  ?:  ?=(%ix -.u.lun.wod.dyr)
                $(waz t.waz)
              ~
          :+  %ouzo  u.lun.wod.dyr
          ?:  &(=(i.waz her) =(~ ryn))  pac
          =+  mal=(jam `meal`[%fore her ryn pac])
          %-  spit
          ^-  cake
          :*  [our i.waz]
              ?~  yed.caq.dyr  [%none mal]
              :-  %fast
              %^  cat  7
                p.u.yed.caq.dyr
              (en:crua q.u.yed.caq.dyr mal)
          ==
```

This takes a sample of the current time, the list of ships that we just
generated, a lane if we already know it, and the packet itself.

First, if we are sending a message to ourself, then we simply create a `%ouzo`
boon with a bunted lane.  Otherwise, if there are no routing candidates, there
is nothing we can do, so we return nil.

Next, we get the `dore` of the first routing candidate.  If we're looking at
the neighbor to whom we're trying to send the message, then we simply use the
`dore` that we already have.  Otherwise, we get a default `dore`.

If we're the first routing candidate, or if we have don't have a lane to this
candidate, then we skip this candidate and move on to the next one.

If we have only a provisional ip address, then we try to send on it, but we
also try to send on later routing candidates as well.  Otherwise, we only send
on this one candidate.

Finally, we create the actual `%ouzo` boon.  The lane is the one from our
`dore`.  If we're sending it directly to our intended recipient, and we haven't
been told to use a specific lane, then we just send the packet directly.
Otherwise, we wrap it in a little `%fore` meal, telling the intermediary to
whom we wish it to be sent.  If we have already set up a symmetric key with the
intermediary, then we encrypt it with that.  Otherwise, we send it in the
clear.

Now, if you recall, we have traced all the way through from the beginning when,
in `++knob`, the `%wont` card was handled by a call to `++wise`.  There is only
one more step before the packet is finally sent.  Looking in `++knob`, we see
that the resultant list of boons is passed into `++clop`, which will execute
the correct actions and return a list of moves.  In `++clop`, we see the
handling of each specific boon.  The one we are interested in is `%ouzo`, since
that is the only one we have sent thus far.

```
        %ouzo
      ::  ~&  [%send now p.bon `@p`(mug (shaf %flap q.bon))] 
      :_  fox
      [[gad.fox [%give %send p.bon q.bon]] ~]
```

Very simply, we give a `%send` gift along the special duct that goes straight into
the bowels of unix.  This is the last stop before we drop into vere, and later
libuv.  And then... the world.

The packet, after its creation, embarks on a journey across physical time and
space into the great unknown.  Hurtling through fiber-optic cables at hundreds
of thousands of kilometers per second, it finally arrives at our neighbor's
network adapter.  The adapter tells unix, unix tells libuv, libuv tells vere,
and vere sends a `%hear` kiss to ames.  And now we reenter the kernel.

The `%hear` kiss goes straight to `++knob`, just as did the `%wont` kiss
earlier.

```
            %hear
          (~(gnaw am [now fox]) %good p.kyz q.kyz)
```

Here, though, we call `++gnaw:am` to process the packet.  The arguments to
`++gnaw` are the same as those to the `%hear` kiss:  the lane on which the
packet was received and the packet itself.  The other argument is just `%good`,
which is a `++cape` saying that we expect the packet to succeed.  If a formal
error occurs, then since we have a transactional event system, the `%hear`
event will never be considered to have actually happened, and unix will send a
`%hole` kiss so that we may send a negative acknowledgment.

```
    ++  gnaw                                            ::    gnaw:am
      |=  [kay=cape ryn=lane pac=rock]                  ::  process packet
      ^-  [p=(list boon) q=fort]
      ?.  =(2 (end 0 3 pac))  [~ fox]
      =+  kec=(bite pac)
      ?:  (goop p.p.kec)  [~ fox]
      ?.  (~(has by urb.ton.fox) q.p.kec)
        [~ fox]
      =<  zork
      =<  zank
      %-  ~(chew la:(ho:(um q.p.kec) p.p.kec) kay ryn %none (shaf %flap pac))
      [q.kec r.kec]
```

First, we check the protocol number.  If it is not correct, then we simply
ignore the packet entirely.  Otherwise, we parse the packet with `++bite`,
which converts a packet atom into a `cake`, that is, a triple of the `sock`
(pair of sender and receiver), the `skin` (encryption type), and the data.

```
  ++  bite                                              ::  packet to cake
    |=  pac=rock  ^-  cake
    =+  [mag=(end 5 1 pac) bod=(rsh 5 1 pac)]
    =+  :*  vez=(end 0 3 mag)                           ::  protocol version
            chk=(cut 0 [3 20] mag)                      ::  checksum
            wix=(bex +((cut 0 [23 2] mag)))             ::  width of receiver
            vix=(bex +((cut 0 [25 2] mag)))             ::  width of sender
            tay=(cut 0 [27 5] mag)                      ::  message type
        ==
    ?>  =(2 vez)
    ?>  =(chk (end 0 20 (mug bod)))
    :+  [(end 3 wix bod) (cut 3 [wix vix] bod)]
      (kins tay)
    (rsh 3 (add wix vix) bod)
```

This is exactly the inverse of `++spit`.  Note that here we check both the
protocol number and the hash, crashing on error.  Remember that a crash will
result in a negative acknowledgment being sent.

Continuing in `++gnaw`, we see that if the intended recipient is not on our
pier, then we drop the packet.

If we've gotten this far, then we wish to process the packet.  Recall that
`++ho` and `++um` set up the domestic server and foreign client cores,
respectively, and that `++zork` and `++zank` resolve any changes to these
cores.

The new stuff here, then, is the `++la` core and the `++chew` arm.  The `++la`
sets up a core for this particular packet, containing the current
success/failure `cape`, the lane it was sent on, the encryption type, and a
hash of the packet, used as an id.

`++chew` is called with the encryption type and the message itself.  It contains
a little helper core inside of it, which starts immediately with `++apse`.

```
            ++  apse
              ^+  +>.$
              =+  oub=bust:puz
              =+  neg==(~ yed.caq.dur.diz)
              =.  +>.$  east
              =+  eng==(~ yed.caq.dur.diz)
              =+  bou=bust:puz
              =.  bin
                ?.  &(oub !bou)  bin
                :_(bin [%wine [our her] " is ok"])
              =.  bin
                ?.  &(neg !eng)  bin
                :_(bin [%wine [our her] " is your neighbor"])
              +>.$
```

First, we let `oub` be true if our neighbor hasn't been responding to us for
more than sixteen seconds.  Let `neg` be true if we haven't yet proposed a
symmetric key, meaning that we haven't yet corresponded with this ship, so they
are not our neighbor.  Next, we run `++east`, which we'll go into in just a
minute.

We now do the same two checks and store the results in `eng` and `bou`.  If our
neighbor has, like the prodigal son, returned after an extended absense, then
we send a `%wine` boon as the proverbial fatted calf, which is simply printed
out to the console.  Likewise, if we are meeting one with whom we have never
had the pleasure of acquainting ourselves, we send a message to the console to
that effect.

We skipped over `++east`, which contains the meat of the processing.  It first
decrypts the message, then calls `++chow:la:ho:um:am` with the resultant meal.
We'll go through each of the four cases in turn, but first since each one calls
`++bilk:pu`, we'll take a brief detour.

```
    ++  bilk                                            ::    bilk:pu
      |=  now=@da                                       ::  inbound packet
      ^+  +>
      =+  trt=(mul 2 rtt)
      %=  +>.$
        rue  [~ now]
        rto  trt
        rtn  ?~(puq ~ [~ (add now trt)])
      ==
```

This updates the timing information in our packet pump.  `rue`, the last time
we have heard from this neighbor, is set to now.  `rto`, the retransmit timeout
is set to twice the current ping time, and if there is anything in the packet
queue, then we reset the next timeout, since we've just heard a message.

Back to `++east`.

```
                  %none
                =.  puz  (bilk:puz now)
                (chow ((hard meal) (cue msg)))
```

The simplest case is when the encryption type is `%none`.  We first call
`++bilk` to update the packet pump, then we cue (unjam) the message into a
meal.  We hard cast it into a meal -- if the cast fails, then we do want to
crash since someone is sending us malformed data.  Finally, we send the result
to `++chow` for interpretation and handling.

```
                  %fast
                =+  [mag=`hand`(end 7 1 msg) bod=(rsh 7 1 msg)]
                =+  dey=(kuch:diz mag)
                ?~  dey
                  ~&  [%bad-key her mag]
                  +>.$                           ::  ignore unknown key
                =.  puz  (bilk:puz now)
                =^  key  diz  u.dey
                (chow(aut sin) ((hard meal) (cue (dy:q:sen:gus key bod))))
```

For symmetric encryption, we first get the `hand`, which is the hash of the
symmetric key.  We pass it to `++kuch:lax:as:go`, which returns the key if we
either have used it before or we have proposed it.  If we have proposed it,
then we change its status from proposed to real.  If `++kuch` fails, then
we drop the packet and print out a `%bad-key` message.

Otherwise, we call `++bilk` as before to update the packet pump and pass into
`++chow` the decrypted data.

```
                  %full
                =+  mex=((hard ,[p=[p=life q=life] q=will r=@]) (cue msg))
                =.  diz  (deng:diz q.mex)
                =+  wug=cluy:diz
                ?>  =(q.p.mex p.wug)
                =+  gey=(sev:gus p.p.mex)
                =+  mes=(need (tear:as:q.gey pub:ex:r.wug r.mex))
                =.  diz  (wasc:diz p.mes)
                =.  puz  (bilk:puz now)
                (west(msg q.mes))
```

For sealed asymmetric encryption, we first take off the the layer of data that
gives us the life and will of our neighbor, and we apply try to extend their former
will with the new data.  `++deng` will fail if this is impossible.

Next, we get our most current understanding of our neighbor's crypto, and we
verify that it's the same life as what they're sending.  Then, we get our own
crypto from `++sev` and decrypt the message with the public key from our
neighbor's crypto.  We register the proposed symmetric key, update the packet
pump, and call `++west`, which simply casts the message to a meal and calls
`++chow`, reporting any error.

```
                  %open
                =+  mex=((hard ,[p=[~ q=life] q=will r=@]) (cue msg))
                =.  diz  (deng:diz q.mex)
                =+  wug=cluy:diz
                ?>  =(q.p.mex p.wug)
                =+  mes=(need (sure:as:r.wug *code r.mex))
                =.  puz  (bilk:puz now)
                (west(msg mes))
```

Finally, for signed asymmetric encryption, we, as before, take off the layer of
data that gives us the life and will of our neighbor.  This time, of course, we
do not get our own crypto -- only that of our neighbor.

The rest you have seen.  We call `++deng` to extend the will, we verify that
their crypto life is what we think it ought to be, we "decrypt" the data, we
update the packet pump, and we call `++west` to call `++chow`.

```
          ++  chow                                      ::    chow:la:ho:um:am
            |=  fud=meal                                ::  interpret meal
            ^+  +>
            =.  diz  ?:(=(%none aut) diz (wast:diz ryn))
            (dine fud)
```

Here, if the message was encrypted at all, then we call `++wast:lax:as:go`,
which simply updates the lane (route) to our neighbor (unless we're given a
provisional route).  This ensures that we always have the most direct possible
path to them.

We've been handling this meal for so long, we've almost forgotten what we want
to do with it.  The telos is of any meal to be dined on.  We will choose out
the cases here that are important to our current investigation.

```
                %fore
              =+  ^=  lyn  ^-  lane
                  ?~  q.fud  ryn
                  ?.  ?=(%if -.u.q.fud)  u.q.fud
                  [%ix +.u.q.fud]
                  ::  u.q.fud
              ?:  =(our p.fud)
                (emit %mead lyn r.fud)
              =+  zid=(myx:gus p.fud)
              (emir (wist:zid now xong:zid [~ lyn] r.fud))
```

Forwarding is the simplest case, since we've seen all the arms before, except
perhaps `++emit` and `++emir`, which simply take a boon or list of boons
respectively and queue them up to be handled when the core resolves.  If we're
told to forward a packet to ourselves, then we emit a `%mead` boon which simply
sends another `%hear` kiss to ourselves with the data.  Otherwise, we try to
find a route to the recipient, as before.

```
                %carp
              =+  zol=(~(get by olz.weg) s.fud)
              ?^  zol  cock(kay u.zol)
              =^  neb  nys.weg
                  =+  neb=(~(get by nys.weg) s.fud)
                  ?^  neb  [u.neb nys.weg]
                  =+  neb=`bait`[(kins p.fud) 0 r.fud ~]
                  [neb (~(put by nys.weg) s.fud neb)]
              ?>  (lth q.fud p.r.neb)
              ?>  =((kins p.fud) p.neb)
              ?>  =(r.fud p.r.neb)
              =+  doy=`(unit ,@)`(~(get by q.r.neb) q.fud)
              ?^  doy  cock
              =>  ^+  .   %=  .
                    q.r.neb  (~(put by q.r.neb) q.fud t.fud)
                    q.neb    +(q.neb)
                  ==
              ::  ~&  [%carp q.fud s.fud q.neb p.r.neb]
              ?:  =(q.neb p.r.neb)
                =:  nys.weg  (~(del by nys.weg) s.fud)
                    olz.weg  (~(put by olz.weg) s.fud kay)
                  ==
                (golf p.neb r.neb)
              =.  +>.$  cock
              +>.$(nys.weg (~(put by nys.weg) s.fud neb))
```

Here, we have received a partial message, and we're just assembling the
individual packets into a message.  Most of this code is fairly algorithmic, so
we'll just hit the high points.  In the beginning, we check if we've already
received this message, and if so, we resend the acknowledgment.  Remember,
"always ack a dupe, never ack an ack".

In `nys.weg` we keep track of an incoming set of partial packets, indexed by
the `flap` hash that comes with every packet.  We check to see if we have
already received this partial message, and if so we acknowledge it.  Otherwise,
we put it in `nys.weg` unless this is the last message, in which case we ack
the last partial message, move the complete message into `olz.weg`, and call
`++golf`, which assembles the message and calls `++chew`, to start the dance
again with the complete message.

```
                %bund
              ::  ~&  [%bund q.fud r.fud]
              ?>  =(p:sen:gus p.fud)
              (deer q.fud r.fud ?-(kay %dead ~, %good [~ s.fud]))
```

What if we're just receiving a regular old, garden variety message?  We call
`++deer` with the data from the message.  If we already know that the message
processing will fail (that is, if we got a `%hole` card from unix rather than a
`%hear` card), then we don't even send the data at all.  Remember, if a packet
fails to process, it's as if it never even arrived, except that we send a
negative acknowledgment.

```
          ++  deer                                      ::    deer:la:ho:um:am
            |=  [cha=path num=@ud dut=(unit)]           ::  interpret message
            ^+  +>
            =+  rum=(fall (~(get by raz.bah) cha) *race)
            %=    +>.$
                +>
              ?.  (gte num did.rum)                     ::  always ack a dup
                (cook (~(get by bum.rum) num) cha ~ ryn dam)
              ?:  dod.rum
                (coat cha rum(mis (~(put by mis.rum) num [kay ryn dam dut])))
              %=    +>.+>.$
                  raz.bah
                %+  ~(put by raz.bah)  cha
                rum(mis (~(put by mis.rum) num [kay ryn dam dut]))
              ==
            ==
```

First, we get the race for this particular triple of sender, receiver, and
path, creating it if it doesn't exist.  If we've already acked the message,
then we resend the ack.  Note that `did.rum` is the number of packets we
acknowledged, positively or negatively while `bum.rum` is a map of message
numbers to negative acknowledgments.  Thus, if a message number is less than
`did.rum`, then if it's in `bum.rum` then it was negatively acknowledged,
otherwise it's postively acknowledged.  Thus, we are constant in space with the
number of successful messages and linear in the number of failed messages.
We'll document `++cook` later on, but suffice it to say that it sends an
acknowledgment.  It is to end-to-end acknowledgments what `++cock` is to
packet-level acknowledgments.

If we are still processing a message (that is, `dod.rum` is false), then we
simply put this message in the map of misordered packets to be processed when
their time comes.  "Processing a message" in this case means that we've
received the message and notified the correct application, but we're still
waiting for the application-level acknowledgment.

Otherwise, we're ready for a packet, so we process it.

```
        ++  coat                                        ::    coat:ho:um:am
          |=  [cha=path rum=race]                       ::  update input race
          ^+  +>
          =+  cun=(~(get by mis.rum) did.rum)
          ?~  cun
            +>.$(raz.bah (~(put by raz.bah) cha rum))
          ?.  =(%good p.u.cun)  +>.$
          ?>  ?=(^ s.u.cun)
          %=    +>.$
              raz.bah  (~(put by raz.bah) cha rum(dod |))
              bin
            :_  bin
            :^    %mulk
                [our her]
              `soap`[[p:sen:gus clon:diz] cha did.rum]
            u.s.u.cun
          ==
```

First, we grab the message we want to process and store it in `cun`.  If it's a
good packet, then we change `dod.rum` to false, meaning that we're in the
middle of processing a packet and should not start processing another one.  We
also put a `%mulk` boon into the queue so that, when it all resolves, we send a
mesage to the intended recipient application.  The boon contains the sender,
the receiver, the identity of the message, and the message itself.

This bubbles up all the way back to `++knob`, where we were handling the
`%hear` card.  Following the logic in `++knob`, we can see that the boons get
sent into `++clop` to be turned into actual arvo-level moves.  We've been here
before, if you recall, when we handled the `%cake` boon to send a message.
Now, we're handling the `%mulk` boon, which is unfortunately slightly more
complicated.

```
        %mulk
      ::  ~&  [%mulk p.bon q.bon]
      ?>  ?=([@ @ *] q.q.bon)
      ?>  ?=(%q i.q.q.bon)
      ?+  i.t.q.q.bon
        ~&  %mulk-bad
        :_  fox
        :~  :-  (claw p.p.bon)
            [%sick %wart p.bon i.t.q.q.bon t.t.q.q.bon r.bon]
        ==
          %ge                                         ::  %gall request
        ?>  ?=([@ ~] t.t.q.q.bon)
        =+  app=`term`(need ((sand %tas) i.t.t.q.q.bon))
        =+  ^=  pax
            :+  (scot %p p.p.bon)
              (scot %p q.p.bon)
            q.q.bon
        :_  fox  [hen %pass pax %g %rote p.bon app r.bon]~
          %gh                                         ::  %gall response
        ?>  ?=([@ ~] t.t.q.q.bon)
        =+  app=`term`(need ((sand %tas) i.t.t.q.q.bon))
        =+  ^=  pax
            :+  (scot %p p.p.bon)
              (scot %p q.p.bon)
            q.q.bon
        :_  fox  [hen %pass pax %g %roth p.bon app r.bon]~
      ==
```

We're dispatching messages based on the prefix of their path.  Since only
`%gall` apps use end-to-end acknowledgments at the moment, every path must have
at least two elements, and the first one must be `%q`.  Beyond that, we handle
the `/q/ge` and `/q/gh` cases for gall requests and responses, respectively.

In both cases, we require the next term in the path to be the name of the
intended recipient `%gall` app.  Thus, a message to `/q/ge/chat` for example,
will send a message to the chat app.

We then send a message to the app itself.  The message is either a `%rote` or a
`%roth` for a request and a response, respectively.  The content is the `rook`
or `roon` that was sent (stored in `r.bon`), but we don't actually handle that
at all here.  That's completely a `%gall`-level thing.  We're just the
messenger.

Notice the path we send this over.  We encode the sender, the receiver, and the
path over which it was sent.  This fully specifies the `race` so that when the
app gives us the acknowledgment we know where to send it.

We now have another interlude.  We have entrusted our precious data, so
carefully guarded and guided from the app on that far-away ship, to our local
app.  It has the ability to do whatever it pleases with it.  It may take a
significant amount of time to process.  When the message has been handled by
this app, though, it must produce an acknowledgment.  Our final task is to
deliver this acknowledgment to the sending app.

We should describe here what exactly these oft-mentioned acknowledgments
actually consist of.  There are two kinds of acknowledgments:  positive and
negative.  A positive acknowledgment contains no data other than its existence.
A negative acknowledgment may optionally include a reason for said negativity.
Formally, a negative acknowledgment is an `ares`, which is a unit pair of a
term and a list of tanks.  If this is null, this is simply a failure with no
associated information.  If the pair exists, the term is a short error code
that is usually both human and computer readable.  For example, if you try to
send a message to a valid `%gall` app that doesn't have any `++poke` to handle
it, then `%gall` will give a negative acknowledgment with error term
`%poke-find-fail`.  The list of tanks is a human-readable description of the
error.  This often contains a stack trace.  At any rate, all this information
is returned to the sending app on the other end of the wire.

After this brief interlude, our story resumes in `++knap`, where we receive
responses.  In particular, a `%mean` indicates a negative acknowledgment while
a `%nice` indicates a positive acknowledgment.

```
        ?(%mean %nice)
      ?>  ?=([@ @ @ *] tea)
      =+  soq=[(slav %p i.tea) (slav %p i.t.tea)]
      =+  pax=t.t.tea
      =+  ^=  fuy
          =<  zork  =<  zank
          %^  ~(rack am [now fox])  soq  pax
          ?-(+<.sih %mean `p.+.sih, %nice ~)
      =>  %_(. fox q.fuy)
      =|  out=(list move)
      |-  ^-  [p=(list move) q=_+>.^$]
      ?~  p.fuy
        [(flop out) +>.^$]
      =^  toe  fox  (clop now hen i.p.fuy)
      $(p.fuy t.p.fuy, out (weld (flop toe) out))
```

Recall the format of the path we sent the message on, and you'll understand why
`soq` and `pax` are the sender/receiver pair and path on which the message was
sent.  The rest of this is structured much like `++knob`, so we call
`++rack:am` and send the resulting boons to `++clop`.  Business as usual.

```
    ++  rack                                            ::    rack:am
      |=  [soq=sock cha=path cop=coop]                  ::  e2e ack
      =+  oh=(ho:(um p.soq) q.soq)
      =.  oh  (cook:oh cop cha ~)
      (cans:oh cha)
```

First, we set up `++um` and `++ho`, as we've done twice before, for our
domestic and foreign servers, respectively.  The other two things are new,
though.  Well, `++cook` is not actually new, but we delayed the explanation
saying only that it sends an acknowledgment.  The time has come.

```
        ++  cook                                        ::    cook:ho:um:am
          |=  [cop=coop cha=path ram=(unit ,[ryn=lane dam=flap])]
          ^+  +>                                        ::  acknowledgment
          =+  rum=(need (~(get by raz.bah) cha))
          =+  lat=(~(get by mis.rum) did.rum)
          ?:  &(?=(~ lat) ?=(~ ram))  ~&(%ack-late-or-redundant +>.$)
          =+  ^-  [ryn=lane dam=flap]
              ?^  ram  [ryn.u.ram dam.u.ram]
              ?<  ?=(~ lat)
              [q r]:u.lat
          =.  raz.bah
            ?^  ram  raz.bah
            %+  ~(put by raz.bah)  cha
            rum(dod &, bum ?~(cop bum.rum (~(put by bum.rum) did.rum u.cop)))
          =^  roc  diz  (zuul:diz now [%buck cop dam ~s0])
          (busk(diz (wast:diz ryn)) xong:diz roc)
```

If we are acknowledging a message that we have already acked, the `ram` will
contain the new lane and flap to send the duplicate ack to.  This happens if we
call `++cook` in `++deer`, but it doesn't happen from `++rack`.  If there is no
message waiting to be acknowledged and we're not given an explicit lane and
flap (that is, we're not sending a duplicate ack), then the app must have sent
us multiple acknowledgments.  We do the only sensible thing we can do and drop
all acknowledgments after the first, printing a message.  This is, in fact, an
error, so it could be argued that we ought to crash.  Whatever you do, don't
depend on this not crashing.

First, we grab the race specified by the given path, and we get the most recent
in-order message, which must be the one which is being acknowledged.

Then, we decide which lane/flap to respond on/to.  Basically, in the usual case
we respond on the lane through which the initial message was sent, which is
stored along with the other packet information in `mis.rum`, since it has to be
remembered across calls to ames.  However, if we receive a duplicate message,
then we must respond to the new message.  It's quite possible the reason the
other acknowledgment didn't get returned was that the lane between the ships
was broken.

At any rate, we update the race by saying that we've finished processing this
packet (unless we're sending a duplicate ack) and, if we're sending a negative
acknowledgment, putting the negative ack into `bum.rum` so that we can resend
it if necessary.

We encode our new message, updating the packet pump, with `++zuul`, as before,
and we send it off with `++busk`, routed via `++wast` to one of the ships in
`++xong`.  Of course, in practice, we don't even look at the ships in `++xong`
because we already have a lane directly to our neighbor (the one over which
they sent their message to us).

We glossed over the actual message we're sending back.  We're sending a `%buck`
meal, which is an acknowledgment.  The `cop` specifies whether this is a
positive or a negative ack, `dam` specifies the message we're acknowledging,
and the `~s0` is a placeholder for the processing time required.  This time is
neither calculated (though it is hopefully obvious how to do so) nor used at
present, but this information may be used in the future for improved congestion
control.  Since the round-trip time for an end-to-end acknowledged packet
includes the processing time on the other end, most common congestion control
algorithms will stumble when some messages take much longer to process than
others.  As noted, though, this is simply an opportunity for improvement -- our
congestion control algorithms are relatively naive at the moment.

Recall that `++busk` calls `++wist` to put the actual `%ouzo` boon in the
queue, which gets handled by `++clop` to actually send the message.  This is
the same pipeline as sending any other message, so we'll refer you to the
explanation above if you've forgotten it.

The last thing we need to do on this ship is move on to the next packet in the
queue if there is one.  If you recall, in `++rack` after the call to `++cook`
there was a call to `++cans:ho:um:am`.

```
        ++  cans                                        ::    cans:ho:um:am
          |=  cha=path
          =+  rum=(need (~(get by raz.bah) cha))
          =.  rum
            %=  rum
              did  +(did.rum)
              mis  (~(del by mis.rum) did.rum)
            ==
          (coat cha rum)
```

This is very simple.  We increment the number of packets that we've
acknowledged on this race and we delete the packet that we just acknowledged
from the set of misordered packets.

Then, we call `++coat` again to process the next packet if we've already
received it.  And that's it for this.

The acknowledgment now travels the same path that its forebearer, the original
message, once tread, but this time not into the great unknown.  The weary
traveler is seeking out its familial roots, finding the app from whom sprung
forth the original message way back in paragraph three.  When it arrives at the
network adapter of its ancestors, the adapter tells unix, unix tells libuv,
libuv tells vere, and vere sends a `%hear` kiss to ames.  Once more into the
kernel.

The `%hear` kiss is handled in `++knob` as before, leading to `++gnaw`, going
over to `++chew`, `++apse`, `++chow`, and eventualy to `++dine`.  We've seen
most of the cases in `++dine`, but we haven't yet looked at the handling of this
`%buck` meal.

```
                %buck
              =.  +>  ?.(=(%full aut) +> cock)          ::  finish key exch
              +>(..la (tock p.fud q.fud r.fud))
```

We send a packet level acknowledgment if we're finishing a key exchange, else
we call `++tock` to process the acknowledgment.

This will get a little involved, so if you don't much care about how exactly an
acknowledgment happens, just know that the result gets gifted as a `%woot` card
back to the app who sent it.  For those brave souls who wish to see this thing
through to the end, it's once more into the breach.

```
        ++  tock                                        ::    tock:ho:um:am
          |=  [cop=coop fap=flap cot=@dr]               ::  e2e ack by hash
          ^+  +>
          =^  yoh  puz  (bick:puz now fap)
          =.  +>.$
            ?~  p.yoh  +>.$
            =^  hud  +>.$
              (done p.u.p.yoh q.u.p.yoh)
            ?~  hud  +>.$
            %=    +>.$
                bin
              :_  bin
              `boon`[%cake [our her] [[p:sen:gus clon:diz] u.p.yoh] cop u.hud]
            ==
          (busk xong:diz q.yoh)
```

We're going to work through this one a little backwards since it's mostly
fairly simple except the call to `++bick:pu`.  In fact, we'll just skip
`++bick` for the moment and finish the rest.

If `++bick` succesfully acks the message, then we call `++done`.

```
        ++  done                                        ::    done:ho:um:am
          |=  [cha=path num=@ud]                        ::  complete outgoing
          ^-  [(unit duct) _+>]
          =+  rol=(need (~(get by ryl.bah) cha))
          =+  rix=(~(get by san.rol) num)
          ?~  rix  [~ +>.$]
          :-  rix
          %_    +>.$
              ryl.bah
            (~(put by ryl.bah) cha rol(san (~(del by san.rol) num)))
          ==
```

This very simply gets the rill (the outgoing counterpart to a race, if you
recall), pulls out of the map of outstanding messages the duct over which the
original message was sent, and produces this duct while deleting that entry
from the map of outstanding messages.

Going back to `++tock`, we now have the duct we need to return the result
over.  We do the very sensible thing and put a `%cake` boon in the queue to be
processed later by `++clop`.

In `q.yoh` we have a list of messages that may need to be sent, which we pass
to `++busk` to send, as usual.  When an acknowledgment arrives, that may
trigger other messages immediately.  This often happens when sending more
messages than the width of the logical window since for congestion control
reasons another message cannot be sent until some of the earlier ones have been
acknowledged.

We'll look at the processing of the `%cake` boon in `++clop` before we get back
to talking about `++bick`.

```
        %cake
      :_  fox
      :~  [s.bon %give %woot q.p.bon r.bon]
      ==
```

We very simply give, along the duct we found above, a `%woot` card with the
ship who sent us the ack and the ack itself.  This allows the application to
decide what to do about the result.  In case of a failure, we usually either
resend the message or display it to the user.  Sometimes, we recognize the
error term and handle it internally.  In any case, the decision of how to
handle the acknowledgment is entirely up to the application.  Our job is done.

Well, except that we skipped `++bick:pu`.  Let's go back to that.

```
    ++  bick                                            ::    bick:pu
      |=  [now=@da fap=flap]                            ::  ack by hash
      ^-  [[p=(unit soup) q=(list rock)] _+>]
      =+  sun=(~(get by diq) fap)
      ?~  sun
        [[~ ~] +>.$]
      =.  diq  (~(del by diq) fap)
      =^  gub  +>.$  (bock now u.sun)
      =^  yop  +>.$  (harv now)
      [[gub yop] +>.$]
```

If you recall, in `++whap:pu` we created the packet pump's representation of
the message, which included putting the message into `diq`, which maps from
packet hashes to packet sequence numbers.  Thus, `u.sun` is the sequence number
of this particular message.

We delete this message from `diq` since we have now received an ack for it.  We
call `++bock` to perform the ack by sequence number.  We call `++harv` to
harvest the packet queue, sending any messages that are now able to be sent.

In `++bock`, there are three arms we haven't seen before:  `++bine`, `+wept`,
and `++beet`.  We'll describe each of these before we get to `++bock`.
`++bine` looks scariest.

```
    ++  bine                                            ::    bine:pu
      |=  [now=@da num=@ud]                             ::  apply ack
      ^-  [(unit soup) _+>]
      ?~  puq  !!
      ?.  =(num p.n.puq)
        ?:  (gth num p.n.puq)
          =+  lef=$(puq l.puq)
          [-.lef +.lef(puq [n.puq puq.lef r.puq])]
        =+  rig=$(puq r.puq)
        [-.rig +.rig(puq [n.puq l.puq puq.rig])]
      =:  rtt  ?.  &(liv.q.n.puq =(1 nux.q.n.puq))  rtt
               =+  gap=(sub now lys.q.n.puq)
               ::  ~&  [%bock-trip num (div gap (div ~s1 1.000))]
               (div (add (mul 2 rtt) gap) 3)
          nif  (sub nif !liv.q.n.puq)
        ==
      =+  lez=(dec (need (~(get by pyz) gom.q.n.puq)))
      =^  gub  pyz
          ?:  =(0 lez)
            [[~ gom.q.n.puq] (~(del by pyz) gom.q.n.puq)]
          [~ (~(put by pyz) gom.q.n.puq lez)]
      :-  gub
      +>.$(puq ~(nap to puq))
```

The first few lines are simply looking through the packet queue until we find
the correct packet to ack.  This is basic queue manipulation that operates
directly on the treap structure of the queue.  If you understand treap queues,
the logic is easy to follow.  Otherwise, just trust us that by the time we get
to the `=:`, the packet with sequence number `num` is on the top of the packet
queue (that is, at `n.puq`).

We first update the round-trip time.  If the packet is either not alive or had
to be transmitted more than once, then we don't have any reliable way of
calculating the round-trip time since we're unsure of exactly which
transmission was acknowledged.  Otherwise, the round-trip time is the
difference between now and when the packet was last sent.  We set `rtt` by a
little weighted average where the previous smoothed RTT is weighted twice as
much as the RTT of the current packet.  Thus, `(2*rtt+gap)/3`.  This gives us a
nice smooth RTT that is somewhat resilient to outlier data while still being
responsive to our ever-changing world.

If the packet wasn't already dead, then we decrement the number of live
packets, which may allow more packets to be sent.

We decrement the number of unacknowledged packets in our `pyz` for this
particular message.  If you recall, this was set in `++whap` to the number of
packets required to send a message.

If that was the last packet in the messge that needed to be acked, then we
delete the messgae reference from `pyz` and produce the id of the message.
Otherwise, we simply update `pyz` with the new number of unacked messages.
In either case, we remove the packet from the packet queue.

```
    ++  wept                                            ::    wept:pu
      |=  [fip=@ud lap=@ud]                             ::  fip thru lap-1
      =<  abet  =<  apse
      |%
      ++  abet  +>.$
      ++  apse
        ^+  .
        ?~  puq  .
        ?:  (lth p.n.puq fip)  ?~(l.puq . left)
        ?:  (gte p.n.puq lap)  ?~(r.puq . rigt)
        =>  rigt  =<  left
        ?>  ?=(^ puq)
        ?.(liv.q.n.puq . .(nif (dec nif), liv.q.n.puq |))
      ::
      ++  left
        ?>  ?=(^ puq)
        ^+(. =+(lef=apse(puq l.puq) lef(puq [n.puq puq.lef r.puq])))
      ++  rigt
        ?>  ?=(^ puq)
        ^+(. =+(rig=apse(puq r.puq) rig(puq [n.puq l.puq puq.rig])))
      --
```

The algorithm is a simple case of traversing the packet queue.  Essentialy, we
mark as dead all packets in the queue between `fip` and `(dec lap)`.  We also
update `nif`, the number of live packets.  Lest you mourn too much the passing
of these packets, know that they shall soon rise again.  Recall that in
`++bick` after the call to `++bock` we call `++harv`.  This will resend the
packets that have just been labeled dead.

```
    ++  beet                                            ::    beet:pu
      ^+  .                                             ::  advance unacked
      =-  +(nep ?~(foh nus u.foh))
      ^=  foh
      |-  ^-  (unit ,@ud)
      ?~  puq  ~
      ?:  (lte p.n.puq nep)  $(puq l.puq)
      =+  rig=$(puq r.puq)
      ?^(rig rig [~ p.n.puq])
```

Here we search for the next expected packet number.  Basically, we search the
queue for the leftmost packet whose number is greater than the current `nep`.
If we don't find any such packet, we just use the total number of packets sent.

We can now dive into `++bock`, our last arm.

```
    ++  bock                                            ::    bock:pu
      |=  [now=@da num=@ud]                             ::  ack by sequence
      ^-  [(unit soup) _+>]
      =^  gym  +>  (bine now num)
      :-  gym
      ?:  (gth num nep)
        =+  cam=(max 2 (div caw 2))
        ::  ~&  [%bock-hole num nep cam]
        beet:(wept(nep num, cag cam, caw cam) nep num)
      =.  caw  ?:  (lth caw cag)  +(caw)
               (add caw !=(0 (mod (mug now) caw)))
      ?:  =(num nep)
        ::  ~&  [%bock-fine num nif caw cag]
        beet
      ::  ~&  [%bock-fill num nif caw cag]
      +>.$
```

First, we call `++bine` to apply the ack to the packet pump information.  We
produce `gym`, which, if it exists, is the id of the packet that was acked.  If
we received an ack for a packet later than the one we expected, then we halve
the logical packet window and kill all the earlier packets so that they may be
resent.

Otherwise, we possibly increase the congestion window.  If the window is less than
the congestion threshold, then we increment the size of the window.  Otherwise,
we only increment one out of every `caw` times.

If we received an ack for the packet we expected, then we simply advance `nep`
with `++beet`.  If we received an ack for a packet earlier than we expected, we
do nothing.

It may be hard to believe, but we are, in fact, done.  The message has been
sent, received, acknowledged, and the acknowledgment has been returned to the
original sender.  We hope it's clear that, while the process has been somewhat
involved, the algorithms are not all that complicated.  If you've read this
far, you know `%ames`.  The only other code involves initialization, timeouts,
and the like.

Below, we give detailed reference documentation for the data models involved.

Data Models
-----------

###`++fort`, formal state

```
++  fort                                                ::  formal state
          $:  %0                                        ::  version
              gad=duct                                  ::  client interface
              hop=@da                                   ::  network boot date
              ton=town                                  ::  security
              zac=(map ship corn)                       ::  flows by server
          ==                                            ::
```

This is the state of our vane.  Anything that must be remembered between
calls to ames must be stored in this state.

`%0` is the version of the ames state model itself.  If the data model `++fort`
changes, then this number needs to be incremented, and an adapter must be
written to upgrade the old state into the new state.  Note that this is the
version number of the model itself, not the contents.  When the data changes,
there is of course no need to change this.

`gad` is a `duct` over which we send `%send` cards to unix.  This card is
initialized when unix sends a `%barn` card as vere starts up.  Vere treats this
duct specially -- don't send anything weird over it.

`hop` is the network boot date.  This is set when the `%kick` card is sent by
vere on start up.

`ton` is a `++town`, where we store all of our security/encryption state.  Note
that this is shared across all ships on a pier.

`zac` is a map of ships to `++corn`.  This stores all the per-ship state.  The
keys to this map are the ships on the current pier.

###`++town`, all security state

```
++  town                                                ::  all security state
          $:  lit=@ud                                   ::  imperial modulus
              any=@                                     ::  entropy
              urb=(map ship sufi)                       ::  all keys and routes
              fak=?                                     ::
          ==                                            ::
```

This is the security state of our pier.

`lit` is unused.

`any` is 256 bits of entropy.  This entropy is used and updated in exactly two
places: when we send a `%junk` card, and when we generate a new symmetric key
in `++griz:lax:as:go`.  When it is updated, it is updated by a SHA-256 hash of
the current time and the old value of the entropy.

`urb` is a map of ships to `++sufi`.  This is where we store all the per-ship
state for the pier.  The keys to this map are the ships on the current pier.

`fak` is true if we are on a fake network.  This disables certain security
checks so that anyone may run a fake `~zod`.  This is used only for development.
To use, run vere with the `-F` option (and the `-I ~zod` option for a fake
`~zod`).

###`++sufi`, domestic host

```
++  sufi                                                ::  domestic host
          $:  hoy=(list ship)                           ::  hierarchy
              val=wund                                  ::  private keys
              law=will                                  ::  server will
              seh=(map hand ,[p=ship q=@da])            ::  key cache
              hoc=(map ship dore)                       ::  neighborhood
          ==                                            ::
```

This is the security state of a domestic server.

`hoy` is a list of the ships directly above us in the hierarchy of ships.  For
example, for `~hoclur-bicrel`, this would be `~tasruc` and `~tug`.  See
`++sein`.

`val` is a list of our private keys.

`law` is our certificate, which is a list of the XXX

`seh`

`hoc` is a map of ships to `++dore`.  The stores all the security informatoin
about foreign ships.  The keys to this map are the neighbors (ships we have
been in contact with) of this domestic server.

###`++wund`, private keys

```
++  wund  (list ,[p=life q=ring r=acru])                ::  mace in action
```

This is a list of our own private keys, indexed by life.  The key itself is
the `++ring`, and the `++acru` is the encryption engine.  We generate the
`++acru` from the private key by calling `++weur`.  Thus, we can at any time
regenerate our `++wund` from a `++mace`.  The current crypto is at the head of
the list and can be accessed with
`++sen:as:go`.

###`++ring`, private key

```
++  ring  ,@                                            ::  private key
```

This is a private key.  The first byte is reserved to identify the type of
cryptography.  Lower-case means public key, upper-case means public key, and
the letter identifies which `++acru` to use.

###`++pass`, public key

```
++  pass  ,@                                            ::  public key
```

This is a public key.  The first byte is reserved to identify the type of
cryptography.  Lower-case means public key, upper-case means public key, and
the letter identifies which `++acru` to use.

###`++mace`, private secrets

```
++  mace  (list ,[p=life q=ring])                       ::  private secrets
```

This is a list of the our private keys, indexed by life.  From this we can
generate a `++wund` for actual use.

###`++skin`, encoding stem

```
++  skin  ?(%none %open %fast %full)                    ::  encoding stem
```

This defines the type of encryption used for each message.  `%none` refers
to messages sent in the clear, `%open` refers to signed messages, `%full`
refers to sealed messages, and `%fast` refers to symmetrically encrypted
messages.  See `++acru` for details.

###`++acru`, asymmetric cryptosuite

```
++  acru                                                ::  asym cryptosuite
          $_  ^?  |%                                    ::  opaque object
          ++  as  ^?                                    ::  asym ops
            |%  ++  seal  |=([a=pass b=@ c=@] _@)       ::  encrypt to a
                ++  sign  |=([a=@ b=@] _@)              ::  certify as us
                ++  sure  |=([a=@ b=@] *(unit ,@))      ::  authenticate from us
                ++  tear  |=  [a=pass b=@]              ::  accept from a 
                          *(unit ,[p=@ q=@])            ::
            --                                          ::
          ++  de  |+([a=@ b=@] *(unit ,@))              ::  symmetric de, soft
          ++  dy  |+([a=@ b=@] _@)                      ::  symmetric de, hard
          ++  en  |+([a=@ b=@] _@)                      ::  symmetric en
          ++  ex  ^?                                    ::  export
            |%  ++  fig  _@uvH                          ::  fingerprint
                ++  pac  _@uvG                          ::  default passcode
                ++  pub  *pass                          ::  public key
                ++  sec  *ring                          ::  private key
            --
          ++  nu  ^?                                    ::  reconstructors
             |%  ++  pit  |=([a=@ b=@] ^?(..nu))        ::  from [width seed]
                 ++  nol  |=(a=@ ^?(..nu))              ::  from naked ring
                 ++  com  |=(a=@ ^?(..nu))              ::  from naked pass
            --
          --
```

This is an opaque interface for a general asymmetric cryptosuite.  Any form
of asymmetric cryptography can be dropped in to be used instead of the default.
Right now, there are two cryptosuites, `++crua`, which is your standard RSA,
and `++crub`, which is elliptic curve crypto but is mostly stubbed out at the
moment.

####`++as:acru`, asymmetric operations

```
          ++  as  ^?                                    ::  asym ops
            |%  ++  seal  |=([a=pass b=@ c=@] _@)       ::  encrypt to a
                ++  sign  |=([a=@ b=@] _@)              ::  certify as us
                ++  sure  |=([a=@ b=@] *(unit ,@))      ::  authenticate from us
                ++  tear  |=  [a=pass b=@]              ::  accept from a 
                          *(unit ,[p=@ q=@])            ::
            --                                          ::
```

This is the core that defines the standard asymmetric cryptography
operations.

`++seal:as:acru` allows us to send a message encrypted with someone's public
key so that only they may read it.  If Alice seals a message with Bob's public
key, then she can be sure that Bob is the only one who can read it.  This is
associated with the `++skin` `%full`.

`++sign:as:acru` allows us to sign a message with our private key so that
others can verify that we sent the message.  If Alice signs a message with her
private key, then Bob can verify with her public key that it was indeed Alice
who sent it.  This is associated with the `++skin` `%open`.

`++sure:as:acru` is the dual to `++sign:as:acru`.  It allows us to verify that
a message we have received is indeed from the claimed sender.  If Alice sends a
message with her private key, then Bob can use this arm to verify that it was
indeed Alice who sent it.  This is associated with the `++skin` `%open`.

`++tear:as:acru` is the dual to `++seal:as:acru`.  It allows us to read a
message that we can be sure is only read by us.  If Alice seals a message with
Bob's public key, then Bob can use this arm to read it.  This is associated
with the `++skin` `%full`.

####`++de:acru`, `++dy:acru`, and `++en:acru`, symmetric encryption/decryption

```
          ++  de  |+([a=@ b=@] *(unit ,@))              ::  symmetric de, soft
          ++  dy  |+([a=@ b=@] _@)                      ::  symmetric de, hard
          ++  en  |+([a=@ b=@] _@)                      ::  symmetric en
```

Symmetric encryption is associated with the `++skin` `%fast`.

`++de:acru` decrypts a message with a symmetric key, returning `~` on failure
and `[~ u=data]` on success.

`++dy:acru` decrypts a message with a symmetric key, crashing on failure.  This
should almost always be defined as, and should always be semantically
equivalent to, `(need (de a b))`.

`++en:acru` encrypts a message with a symmetric key.

####`++ex:acru`, exporting data

```
          ++  ex  ^?                                    ::  export
            |%  ++  fig  _@uvH                          ::  fingerprint
                ++  pac  _@uvG                          ::  default passcode
                ++  pub  *pass                          ::  public key
                ++  sec  *ring                          ::  private key
            --
```

`++fig:ex:acru` is our fingerprint, usually a hash of our public key.  This is
used, for example, in `++zeno`, where every carrier owner's fingerprint is
stored so that we can ensure that carriers are indeed owned by their owners

`++pac:ex:acru` is our default passcode, which is unused at present.

`++pub:ex:acru` is the `++pass` form of our public key.

`++sec:ex:acru` is the `++ring` form of our private key.

####`++nu:acru`, reconstructors

```
          ++  nu  ^?                                    ::  reconstructors
             |%  ++  pit  |=([a=@ b=@] ^?(..nu))        ::  from [width seed]
                 ++  nol  |=(a=@ ^?(..nu))              ::  from naked ring
                 ++  com  |=(a=@ ^?(..nu))              ::  from naked pass
            --
```

These arms allow us to reconstruct a `++acru` from basic data.

`++pit:nu:acru` constructs a `++acru` from the width of our intended key and
seed entropy.  This is usually used in the initial construction of the
`++acru`.

`++nol:nu:acru` constructs a `++acru` from a "naked ring", meaning a `++ring`
without the initial byte identifying the type of crypto.  There is often a
helper arm that that wraps this; see `++weur` for `++crua` and `++wear` for
`++crub`.

`++com:nu:acru` constructs a `++acru` from a "naked pass", meaning a `++ring`
without the initial byte identifying the type of crypto.  There is often a
helper arm that that wraps this; see `++haul` for `++crua` and `++hail` for
`++crub`.

###`++will`, certificate

```
++  will  (list deed)                                   ::  certificate
```

This is a list of deeds associated with the current ship.  There should be
an item in this list for every ship from this point up in the hierarchy times
the number of lives that each ship has had.  For example, ~hoclur-bicrel may
have a will with three items: one for itself, one for ~tasruc (who issued
~hoclur-bicrel's deed) and one for ~tug (who issued ~tasruc's deed).

###`++deed`, identity

```
++  deed  ,[p=@ q=step r=?]                             ::  sig, stage, fake?
```

`p` is the signature of a particular deed, which is a signed copy of `q`.

`q` is the stage in the identity.

`r` is true if we're working on a fake network, where we don't check that the
carrier fingerprints are correct.  This allows us to create fake networks for
development without interfering with the real network.

###`++step`, identity stage

```
++  step  ,[p=bray q=gens r=pass]                       ::  identity stage
```

This is a single stage in our identity.  Thus, this is specific to a single
life in a single ship.  Everything in here may change between lives.

`p`

`q`

`r` is the public key for this stage in the identity.

###`++bray`

```
++  bray  ,[p=life q=(unit life) r=ship s=@da]          ::  our parent us now
```

XXX

###`++gens`, general identity

```
++  gens  ,[p=lang q=gcos]                              ::  general identity
```

`p` is the IETF language code for the preferred language of this identity.
This is unused at the moment, but in the future text should be localized based
on this.

`q` is the description of the ship.

###`++gcos`, identity description

```
++  gcos                                                ::  id description
          $%  [%czar ~]                                 ::  8-bit ship
              [%duke p=what]                            ::  32-bit ship
              [%earl p=@t]                              ::  64-bit ship
              [%king p=@t]                              ::  16-bit ship
              [%pawn p=(unit ,@t)]                      ::  128-bit ship
          ==                                            ::
```

This is the description of the identity of a ship.  Most types of identity have
a `@t` field, which is their human-readable name.  The identity of a `%duke` is
more involved.

A `%czar`, a carrier, is a ship with an 8-bit address.  Thus, there are only
256 carriers.  These are at the top of the namespace hierarchy, and the
fingerprint of each carrier is stored in `++zeno`.  These are the "senators" of
Urbit.

A `%king`, a cruiser, is a ship with a 16-bit address.  Thus, there are 65,536
cruisers.  Each carrier may issue 256 cruisers.  These are the infrastructure
of Urbit.

A `%duke`, a destroyer, is a ship with a 32-bit address.  Thus, there are
4,294,967,296 destroyers.  Each cruiser may issue 65,536 cruisers.  These are
the individuals of Urbit.

A `%earl`, a yacht, is a ship with a 64-bit address.  Thus, there are
18,446,744,073,709,551,616 yachts.  Each destroyer may issue 4,294,967,296
yachts.  These are the devices of Urbit.

A `%pawn`, a submarine, is a ship with a 128-bit address.  Thus, there are a
lot of submarines.  The chance of random name collision is negligible, so
submarines are not issued by any ship.  They must simply assert their presence,
and they are all considered children of ~zod.  This is the underworld of Urbit,
where anonymity reigns supreme.

###`++what`, logical destroyer identity

```
++  what                                                ::  logical identity
          $%  [%anon ~]                                 ::  anonymous
              [%lady p=whom]                            ::  female person ()
              [%lord p=whom]                            ::  male person []
              [%punk p=sect q=@t]                       ::  opaque handle ""
          ==                                            ::
```

This is the logical identity of a destroyer.

A `%anon` is a completely anonymous destroyer.  The difference between this and
a submarine is that a submarine is ephemeral while a `%anon` destroyer is not.
Thus, we may not know who ~hoclur-bicrel is, but we do know that it's always
the same person.

A `%lady` is a female person.  The name used here should be a real name.

A `%lord` is a male person.  The name used here should be a real name.

A `%punk` is a person who is identified only by a handle.

###`++whom`, real person

```
++  whom  ,[p=@ud q=govt r=sect s=name]                 ::  year/govt/id
```

Ths is the information associated with a real person.  It is mostly information
that could be observed with the briefest of interactions.

`p` is the birth year.

`q` is the location of a user, usually of the form "country/zip".

`r` is the sect of the user.

`s` is the real name of the person.


###`++govt`

```
++  govt  path                                          ::  country/postcode
```

This is the location of the user, usually of the form "country/zip".

###`++sect`

```
++  sect  ?(%black %blue %red %orange %white)           ::  banner
```

XXX

###`++name`

```
++  name  ,[p=@t q=(unit ,@t) r=(unit ,@t) s=@t]        ::  first mid/nick last
```

This is the given name, possible middle name/initial, possible nickname, and
surname of a user.

packet format
-------------

`++go`, PKI engine
------------------

###`++as`, per server

####`++born`, register user

#####`++lax`, per client

`++pu`, packet pump
-------------------

`++am`, protocol engine
-----------------------

###`++um`, per server

####`++ho`, per friend

#####`++la`, per packet

protocol vane
-------------

