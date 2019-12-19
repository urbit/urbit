let Persist = { collect-fx : Bool }

let FakeMode = < Dry | Wet : Persist >

let Mode = < Online : Persist | Local : Persist | Fake : FakeMode >

let Verbose = < Quiet | Normal | Verbose >

let King = { mode : Mode, log : Verbose }

let Serf =
      { debug-ram :
          Bool
      , debug-cpu :
          Bool
      , check-corrupt :
          Bool
      , check-fatal :
          Bool
      , verbose :
          Bool
      , dry-run :
          Bool
      , quiet :
          Bool
      , hashless :
          Bool
      , trace :
          Bool
      }

let Ship = { addr : Text, serf : Serf, ames-port : Optional Natural }

let Config = { king : King, ships : List Ship }

let KingDefault =
      { mode = Mode.Online { collect-fx = False }, log = Verbose.Normal } : King

let SerfDefault =
        { debug-ram =
            False
        , debug-cpu =
            False
        , check-corrupt =
            False
        , check-fatal =
            False
        , verbose =
            False
        , dry-run =
            False
        , quiet =
            False
        , hashless =
            False
        , trace =
            False
        }
      : Serf

let ShipDefault =
        λ(addr : Text)
      → { addr = addr, serf = SerfDefault, ames-port = None Natural }

let ConfigDefault = { king = KingDefault, ships = [] : List Ship } : Config

let ConfigExample =
      { king = KingDefault, ships = [ ShipDefault "zod" ] } : Config

in  ConfigExample
