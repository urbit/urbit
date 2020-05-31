Stubbed out:

- [x] Handle replacement events (stubbed out now b/c interface can't
  handle unparsed nouns)
- [x] Handle IPC errors by killing serf process.
- [ ] Handle `peek` and `pack` in `swimming` flow.
- [ ] PlayBail should be an exception for now.
- [ ] Write haddock docs for `Urbit.Vere.Serf.IPC`.
- [x] Unstub slog/stder/dead callbacks on serf config.
- [x] Remove GoodParse hack in newRunCompute.
- [ ] Bring back tank printing.
- [ ] Bring back code for handling serf stderr messages.

King-Haskell specific features:

- [ ] Re-implement "collect-fx" flow.

Performance:

- [x] Batching during replay.
- [x] Batching during normal operation.

Polish:

- [x] Cleanup batching flow.
- [ ] Logging for new IPC flow.
- [ ] Logging for boot sequence.
- [ ] Bring back progress bars.
- [ ] Hook up error callbacks to IO Drivers.
- [x] Think through how to shutdown the serf on exception.
- [ ] Better exceptions in Serf error cases.

Unrelated bugs:

- [ ] Handle ^C in connected terminals.
- [ ] terminal driver seems to have a race condition when spinner changed
      too quickly.
- [ ] King should shutdown promptly on ^C. Always takes 2s in practice./
