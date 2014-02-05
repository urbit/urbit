@0xa727c25c0c7018f2;

struct Raft {
  struct Comd {                           # state machine command
    typ @0 :Type;
    bob @1 :Data;

    enum Type {
      nop @0;                             # no-op
      ova @1;                             # arvo events
      #xek @2;                            # checkpoint
    }
  }

  struct Rent {                           # log entry
    tem @0 :UInt64;                       # term
    cmd @1 :Comd;                         # command
  }

  struct Apen {                           # AppendEntries
    tem @0 :UInt64;                       # term
    cid @1 :Text;                         # leader ID
    lai @2 :UInt64;                       # previous log index
    lat @3 :UInt64;                       # previous log term
    ent @4 :List(Rent);                   # entries
    cit @5 :UInt64;                       # leader commitIndex
  }

  struct Revo {                           # RequestVote
    tem @0 :UInt64;                       # term
    cid @1 :Text;                         # candidate ID
    lai @2 :UInt64;                       # last log index
    lat @3 :UInt64;                       # last log term
  }

  struct Rasp {                           # Raft response
    tem @0 :UInt64;                       # leader's term
    suc @1 :Bool;                         # success
  }
}
