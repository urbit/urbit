# urb

Unix control of Urbit

# Running and Installing

To run without installing anything:

```bash
nix-shell --pure --command 'python ./urb -d "(add 3 4)"'
```

To install `urb`:

```bash
nix-env -if .
```
