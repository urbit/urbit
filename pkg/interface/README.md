## EScape

EScape is Uqbar's web UI, forked from Tlon's Landscape app.

### Contributions and feature requests

For information on how to contribute, see [CONTRIBUTING][cont].

### Build Steps

1. Set up a public S3 bucket or similar to enable http glob downloads
2. Update the package.json `version` number
3. Run `npm run build:prod`
4. Spin up a local fakezod
5. Run `cp -r ~/\*path/to/urbit\*/pkg/interface/dist ~/\*path/to/zod\*/landscape/`
6. Run `cd ~/\*path/to/zod\*/landscape/dist && rm \*.js.map\*`
7. Run `urbit zod`
8. In the dojo run `|commit %landscape`
9. Then `-garden!make-glob %landscape /dist`
10. Upload the glob (located in zod/.urb/put/) to your S3 bucket
11. Add the glob's url and hash to `pkg/escape/desk.docket-0` and update the version number
12. Push the branch to your repo
13. Pull the info to your distro ship
14. Copy to your `escape` desk
15. In the dojo run `|commit %escape`
