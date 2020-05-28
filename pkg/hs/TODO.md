Stubbed out:

- [x] Handle replacement events (stubbed out now b/c interface can't
  handle unparsed nouns)
- [x] Handle IPC errors by killing serf process.
- [ ] PlayBail should be an exception.
- [ ] Write haddock docs for `Urbit.Vere.Serf.IPC`.
- [ ] Unstub slog/stder/dead callbacks on serf config.
- [ ] GoodParse hack in newRunCompute.
- [ ] Bring back tank printing.
- [ ] Bring back code for handling serf stderr messages.

King-Haskell specific features:

- [ ] Re-implement "collect-fx" flow.

Performance:

- [ ] Batching during replay and normal operation.

Polish:

- [ ] Logging for new IPC flow.
- [ ] Logging for boot sequence.
- [ ] Bring back progress bars.
- [ ] Hook up error callbacks to IO Drivers.
- [x] Think through how to shutdown the serf on exception.
- [ ] Better exceptions in Serf error cases.
