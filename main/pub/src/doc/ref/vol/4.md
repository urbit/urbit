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

#Batz

Coming soon

#Dill

Coming soon

#Clay

Clay
====

Clay is our filesystem.

data models
-----------

###`++raft`, formal state

```
++  raft                                                ::  filesystem
          $:  fat=(map ship room)                       ::  domestic
              hoy=(map ship rung)                       ::  foreign
              ran=rang                                  ::  hashes
          ==                                            ::
```

This is the state of our vane.  Anything that must be remembered between calls
to clay must be stored in this state.

`fat` is the set of domestic servers.  This stores all the information that is
specfic to a particular ship on this pier.  The keys to this map are the ships
on the current pier.

`hoy` is the set of foreign servers that we know anything about.  This stores
all the information that is specific to a particular foreign ship.  The keys to
this map are all the ships whose filesystems we have attempted to access
through clay.

`ran` is the store of all commits and deltas, keyed by hash.  The is where all
the "real" data we know is stored; the rest is "just bookkeeping".

###`++room`, filesystem per domestic ship

```
++  room                                                ::  fs per ship
          $:  hun=duct                                  ::  terminal duct
              hez=(unit duct)                           ::  sync duch
              dos=(map desk dojo)                       ::  native desk
          ==                                            ::
```

This is the representation of the filesystem of a ship on our pier.

`hun` is the duct that we use to send messages to dill to display notifications
of filesystem changes.  Only `%note` gifts should be produced along this duct.
This is set by the `%init` kiss.

`hez`, if present, is the duct we use to send sync messages to unix so that
they end up in the pier unix directory.  Only `%ergo` gifts should be producd
along this duct.  This is set by `%into` and `%invo` gifts.

`dos` is a well-known operating system released in 1981.  It is also the set of
desks on this ship, mapped to their data.

###`++desk`, filesystem branch

```
++  desk  ,@tas                                         ::  ship desk case spur
```

This is the name of a branch of the filesystem.  The default desks are "arvo",
"main", and "try".  More may be created by simply referencing them.  Desks have
independent histories and states, and they may be merged into each other.

###`++dojo`, domestic desk state

```
++  dojo  ,[p=cult q=dome]                              ::  domestic desk state
```

This is the all the data that is specific to a particular desk on a domestic
ship.  `p` is the set of subscribers to this desk and `q` is the data in the
desk.

###`++cult`, subscriptions

```
++  cult  (map duct rave)                               ::  subscriptions
```

This is the set of subscriptions to a particular desk.  The keys are the ducts
from where the subscriptions requests came.  The results will be produced along
these ducts.  The values are a description of the requested information.

###`++rave`, general subscription request

```
++  rave                                                ::  general request
          $%  [& p=mood]                                ::  single request
              [| p=moat]                                ::  change range
          ==                                            ::
```

This represents a subscription request for a desk.  The request can be for
either a single item in the desk or else for a range of changes on the desk.

###`++mood`, single subscription request

```
++  mood  ,[p=care q=case r=path]                       ::  request in desk
```

This represents a request for the state of the desk at a particular commit,
specfied by `q`.  `p` specifies what kind of information is desired, and `r`
specifies the path we are requesting.

###`++moat`, range subscription request

```
++  moat  ,[p=case q=case]                              ::  change range
```

This represents a request for all changes between `p` and `q`.  Note that there
is currently no way to request to be notified only on changes to particular
paths in the filesystem.  You must subscribe to the entire desk.

###`++care`, clay submode

```
++  care  ?(%u %v %w %x %y %z)                          ::  clay submode
```

This specifies what type of information is requested in a subscription or a
scry.

`%u` requests the `++rang` at the current moment.  Because this information is
not stored for any moment other than the present, we crash if the `++case` is
not a `%da` for now.

`%v` requests the `++dome` at the specified commit.

`%w` requests the current revsion number of the desk.

`%x` requests the file at a specified path at the specified commit.  If there
is no node at that path or if the node has no contents (that is, if `q:ankh` is
null), then this produces null.

`%y` requests a `++arch` of the specfied commit at the specified path.

`%z` requests the `++ankh` of the specified commit at the specfied path.

###`++arch`, shallow filesystem node

```
++  arch  ,[p=@uvI q=(unit ,@uvI) r=(map ,@ta ,~)]      ::  fundamental node
```

This is analogous to `++ankh` except that the we have neither our contents nor
the ankhs of our children.  The other fields are exactly the same, so `p` is a
hash of the associated ankh, `u.q`, if it exists, is a hash of the contents of
this node, and the keys of `r` are the names of our children.  `r` is a map to
null rather than a set so that the ordering of the map will be equivalent to
that of `r:ankh`, allowing efficient conversion.

###`++case`, specifying a commit

```
++  case                                                ::  ship desk case spur
          $%  [%da p=@da]                               ::  date
              [%tas p=@tas]                             ::  label
              [%ud p=@ud]                               ::  number
          ==                                            ::
```

A commit can be referred to in three ways: `%da` refers to the commit that was
at the head on date `p`, `%tas` refers to the commit labeled `p`, and `%ud`
refers to the commit numbered `p`.  Note that since these all can be reduced
down to a `%ud`, only numbered commits may be referenced with a `++case`.

###`++dome`, desk data

```
++  dome                                                ::  project state
          $:  ang=agon                                  ::  pedigree
              ank=ankh                                  ::  state
              let=@ud                                   ::  top id
              hit=(map ,@ud tako)                       ::  changes by id
              lab=(map ,@tas ,@ud)                      ::  labels
          ==                                            ::
```

This is the data that is actually stored in a desk.

`ang` is unused and should be removed.

`ank` is the current state of the desk.  Thus, it is the state of the
filesystem at revison `let`.  The head of a desk is always a numbered commit.

`let` is the number of the most recently numbered commit.  This is also the
total number of numbered commits.

`hit` is a map of numerical ids to hashes of commits.  These hashes are mapped
into their associated commits in `hut:rang`.  In general, the keys of this map
are exactly the numbers from 1 to `let`, with no gaps.  Of course, when there
are no numbered commits, `let` is 0, so `hit` is null.  Additionally, each of
the commits is an ancestor of every commit numbered greater than this one.
Thus, each is a descendant of every commit numbered less than this one.  Since
it is true that the date in each commit (`t:yaki`) is no earlier than that of
each of its parents, the numbered commits are totally ordered in the same way
by both pedigree and date.  Of course, not every commit is numbered.  If that
sounds too complicated to you, don't worry about it.  It basically behaves
exactly as you would expect.

`lab` is a map of textual labels to numbered commits.  Note that labels can
only be applied to numbered commits.  Labels must be unique across a desk.

###`++ankh`, filesystem node

```
++  ankh                                                ::  fs node (new)
          $:  p=cash                                    ::  recursive hash
              q=(unit ,[p=cash q=*])                    ::  file
              r=(map ,@ta ankh)                         ::  folders
          ==                                            ::
```

This is a single node in the filesystem.  This may be file or a directory or
both.  In earth filesystems, a node is a file xor a directory.  On mars, we're
inclusive, so a node is a file ior a directory.

`p` is a recursive hash that depends on the contents of the this file or
directory and on any children.

`q` is the contents of this file, if any.  `p.q` is a hash of the contents
while `q.q` is the data itself.

`r` is the set of children of this node.  In the case of a pure file, this is
empty.  The keys are the names of the children and the values are, recursively,
the nodes themselves.

###`++cash`, ankh hash

```
++  cash  ,@uvH                                         ::  ankh hash
```

This is a 128-bit hash of an ankh.  These are mostly stored within ankhs
themselves, and they are used to check for changes in possibly-deep
hierarchies.

###`++rung`, filesystem per neighbor ship

```
++  rung  $:  rus=(map desk rede)                       ::  neighbor desks
          ==                                            ::
```

This is the filesystem of a neighbor ship.  The keys to this map are all the
desks we know about on their ship.

###`++rede`, desk state

```
++  rede                                                ::  universal project
          $:  lim=@da                                   ::  complete to
              qyx=cult                                  ::  subscribers
              ref=(unit rind)                           ::  outgoing requests
              dom=dome                                  ::  revision state
          ==                                            ::
```

This is our knowledge of the state of a desk, either foreign or domestic.

`lim` is the date of the last full update.  We only respond to requests for
stuff before this time.

`qyx` is the list of subscribers to this desk.  For domestic desks, this is
simply `p:dojo`, all subscribers to the desk, while in foreign desks this is
all the subscribers from our ship to the foreign desk.

`ref` is the request manager for the desk.

`dom` is the actual data in the desk.

###`++rind`, request manager

```
++  rind                                                ::  request manager
          $:  nix=@ud                                   ::  request index
              bom=(map ,@ud ,[p=duct q=rave])           ::  outstanding
              fod=(map duct ,@ud)                       ::  current requests
              haw=(map mood (unit))                     ::  simple cache
          ==                                            ::
```

This is the request manager for a desk.

`nix` is one more than the index of the most recent request.  Thus, it is the
next available request number.

`bom` is the set of outstanding requests.  The keys of this map are some subset
of the numbers between 0 and one less than `nix`.  The members of the map are
exactly those requests that have not yet been fully satisfied.

`fod` is the same set as `bom`, but from a different perspective.  In
particular, the values of `fod` are the same as the values of `bom`, and the
`p` out of the values of `bom` are the same as the keys of `fod`.  Thus, we can
map ducts to their associated request number and `++rave`, and we can map
numbers to their associated duct and `++rave`.

`haw` is a map from simple requests to their values.  This acts as a cache for
requests that have already been made.  Thus, the second request for a
particular `++mood` is nearly instantaneous.

###`++rang`, data store

```
++  rang  $:  hut=(map tako yaki)                       ::
              lat=(map lobe blob)                       ::
          ==                                            ::
```

This is a set of data keyed by hash.  Thus, this is where the "real" data is
stored, but it is only meaningful if we know the hash of what we're looking
for.

`hut` is a map from hashes to commits.  We often get the hashes from
`hit:dome`, which keys them by logical id.  Not every commit has an id.

`lat` is a map from hashes to the actual data.  We often get the hashes from a
`++yaki`, a commit, which references this map to get the data.  There is no
`++blob` in any `++yaki`.  They are only accessible through this map.

###`++tako`, commit reference

```
++  tako  ,@                                            ::  yaki ref
```

This is a hash of a `++yaki`, a commit.  These are most notably used as the
keys in `hut:rang`, where they are associated with the actual `++yaki`, and as
the values in `hit:dome`, where sequential ids are associated with these.

###`++yaki`, commit

```
++  yaki  ,[p=(list tako) q=(map path lobe) r=tako t=@da] ::  commit
```

This is a single commit.

`p` is a list of the hashes of the parents of this commit.  In most cases, this
will be a single commit, but in a merge there may be more parents.  In theory,
there may be an arbitrary number of parents, but in practice merges have
exactly two parents.  This may change in the future.  For commit 1, there is no
parent.

`q` is a map of the paths on a desk to the data at that location.  If you
understand what a `++lobe` and a `++blob` is, then the type signature here
tells the whole story.

`r` is the hash associated with this commit.

`t` is the date at which this commit was made.

###`++lobe`, data reference

```
++  lobe  ,@                                            ::  blob ref
```

This is a hash of a `++blob`.  These are most notably used in `lat:rang`, where
they are associated with the actual `++blob`, and as the values in `q:yaki`,
where paths are associated with their data in a commit.

###`++blob`, data

```
++  blob  $%  [%delta p=lobe q=lobe r=udon]             ::  delta on q
              [%direct p=lobe q=* r=umph]               ::
              [%indirect p=lobe q=* r=udon s=lobe]      ::
          ==                                            ::
```

This is a node of data.  In every case, `p` is the hash of the blob.

`%delta` is the case where we define the data by a delta on other data.  In
practice, the other data is always the previous commit, but nothing depends on
this.  `q` is the hash of the parent blob, and `r` is the delta.

`%direct` is the case where we simply have the data directly.  `q` is the data
itself, and `r` is any preprocessing instructions.  These almost always come
from the creation of a file.

`%indirect` is both of the preceding cases at once.  `q` is the direct data,
`r` is the delta, and `s` is the parent blob.  It should always be the case
that applying `r` to `s` gives the same data as `q` directly (with the
prepreprocessor instructions in `p.r`).  This exists purely for performance
reasons.  This is unused, at the moment, but in general these should be created
when there are a long line of changes so that we do not have to traverse the
delta chain back to the creation of the file.

###`++udon`, abstract delta

```
++  udon                                                ::  abstract delta
          $:  p=umph                                    ::  preprocessor
              $=  q                                     ::  patch
              $%  [%a p=* q=*]                          ::  trivial replace
                  [%b p=udal]                           ::  atomic indel
                  [%c p=(urge)]                         ::  list indel
                  [%d p=upas q=upas]                    ::  tree edit
              ==                                        ::
          ==                                            ::
```

This is an abstract change to a file.  This is a superset of what would
normally be called diffs.  Diffs usually refer to changes in lines of text
while we have the ability to do more interesting deltas on arbitrary data
structures.

`p` is any preprocessor instructions.

`%a` refers to the trival delta of a complete replace of old data with new
data.

`%b` refers to changes in an opaque atom on the block level.  This has very
limited usefulness, and is not used at the moment.

`%c` refers to changes in a list of data.  This is often lines of text, which
is your classic diff.  We, however, will work on any list of data.

`%d` refers to changes in a tree of data.  This is general enough to describe
changes to any hoon noun, but often more special-purpose delta should be
created for different content types.  This is not used at the moment, and may
in fact be unimplemented.

###`++urge`, list change

```
++  urge  |*(a=_,* (list (unce a)))                     ::  list change
```

This is a parametrized type for list changes.  For example, `(urge ,@t)` is a
list change for lines of text.

###`++unce`, change part of a list.

```
++  unce  |*  a=_,*                                     ::  change part
          $%  [%& p=@ud]                                ::  skip[copy]
              [%| p=(list a) q=(list a)]                ::  p -> q[chunk]
          ==                                            ::  
```

This is a single change in a list of elements of type `a`.  For example, `(unce ,@t)` is
a single change in a lines of text.

`%&` means the next `p` lines are unchanged.

`%|` means the lines `p` have changed to `q`.

###`++umph`, preprocessing information

```
++  umph                                                ::  change filter
          $|  $?  %a                                    ::  no filter
                  %b                                    ::  jamfile
                  %c                                    ::  LF text
              ==                                        ::
          $%  [%d p=@ud]                                ::  blocklist
          ==                                            ::
```

This space intentionally left undocumented.  This stuff will change once we get
a well-typed clay.


###`++upas`, tree change

```
++  upas                                                ::  tree change (%d)
          $&  [p=upas q=upas]                           ::  cell
          $%  [%0 p=axis]                               ::  copy old
              [%1 p=*]                                  ::  insert new
              [%2 p=axis q=udon]                        ::  mutate!
          ==                                            ::
```

This space intentionally left undocumented.  This stuff is not known to work,
and will likely change when we get a well-typed clay.  Also, this is not a
complicated type; it is not difficult to work out the meaning.

#Eyre

Coming soon

#Ford

Coming soon

#Gall

Coming soon
