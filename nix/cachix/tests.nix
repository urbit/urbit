let
  ops = import ../ops/default.nix {};
in
  {
    urbit-test-results = ops.test;
    urbit-test-pier    = ops.bus;
  }
