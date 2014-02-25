@0xa727c25c0c7018f2;

struct Raft {
  struct Comd {                           # state machine command
    typ @0 :Type;
    bob @1 :Data;

    enum Type {
      nop @0;                             # no-op
      ova @1;                             # arvo event
      #xek @2;                            # checkpoint
    }
  }

  struct Rent {                           # log entry
    tem @0 :UInt32;                       # term
    cmd @1 :Comd;                         # command
  }

  struct Rest {                           # Raft RPC request
    tem @0 :UInt32;                       # sender term
    cid @1 :Text;                         # sender name
    lai @2 :UInt64;                       # last log index
    lat @3 :UInt32;                       # last log term
    union {
      revo @4 :Void;                      # RequestVote
      apen :group {                       # AppendEntries
        ent @5 :List(Rent);               # log entries
        cit @6 :UInt64;                   # leader commitIndex
      }
    }
  }

  struct Rasp {                           # Raft RPC response
    tem @0 :UInt64;                       # leader's term
    suc @1 :Bool;                         # success
  }
}
