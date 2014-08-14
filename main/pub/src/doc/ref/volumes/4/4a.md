Ames
====

Ames is our networking protocol.

data models
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

