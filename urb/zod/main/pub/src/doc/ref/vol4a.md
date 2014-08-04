Ames 
====

Ames is our networking protocol.

data models
-----------

###`++fort`, our formal state

```
++  fort                                                ::  formal state
          $:  %0                                        ::  version
              gad=duct                                  ::  client interface
              hop=@da                                   ::  network boot date
              ton=town                                  ::  security
              zac=(map ship corn)                       ::  flows by server
          ==                                            ::
```

`++fort` is the state of our vane.  Anything that must be remembered between
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

`++town` is the security state of our pier.

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

`++sufi` is the security state of a domestic server.

`hoy` is a list of the ships directly above us in the hierarchy of ships.  For
example, for `~hoclur-bicrel`, this would be `~tasruc` and `~tug`.  See
`++sein`.

`val` is a list of our private keys.

`law`

`seh`

`hoc` is a map of ships to `++dore`.  The stores all the security informatoin
about foreign ships.  The keys to this map are the neighbors (ships we have
been in contact with) of this domestic server.

packet format
-------------

`++go`, PKI engine
------------------

### `++as`, per server

#### `++born`, register user

##### `++lax`, per client

`++pu`, packet pump
-------------------

`++am`, protocol engine
-----------------------

### `++um`, per server

#### `++ho`, per friend

##### `++la`, per packet

protocol vane
-------------

