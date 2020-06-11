let
  ops = import ../ops/default.nix {};
in
  {
    results = ops.test;
    fakebus = ops.bus;
  }
