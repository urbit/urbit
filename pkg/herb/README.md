# Herb

Unix control of Urbit

# Running and Installing

To run without installing anything:

```bash
nix-shell --pure --command 'python ./herb -d "(add 3 4)"'
```

To install `herb`:

```bash
nix-env -if .
```
