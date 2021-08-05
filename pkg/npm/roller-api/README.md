# Azimuth L2-Roller API

## Docs

Visit [this link](https://documenter.getpostman.com/view/16338962/Tzm3nx7x) for an overview of the L2-Roller documentation.

## Generate Typescript client

```bash
npm i && npm run build
```

## Build Typescript client and docs

```bash
cd ./client/typescript
npm i && npm run build
```

## Publish

TODO

## Test

The tests expect an urbit listening on `localhost:8080` with `%aggregator` and `%aggregator-rpc` agents started.

```bash
npm run client-test
```

`./tests/roller.tests.ts` shows some examples of using the RPCs.
