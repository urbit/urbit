# Official Urbit Docker Image

This is the official Docker image for [Urbit](https://urbit.org). 

Urbit is a clean-slate OS and network for the 21st century.

## Using

To use this image, you should mount a volume with a keyfile, comet file, or existing pier at `/urbit`, and map ports
as described below.

### Volume Mount
This image expects a volume mounted at `/urbit`. This volume should initially contain one of

- A keyfile `<shipname>.key` for a galaxy, star, planet, or moon. See the setup instructions for Urbit for information on [obtaining a keyfile](https://urbit.org/using/install/). 
  * e.g. `sampel-palnet.key` for the planet `sampel-palnet`.
- An empty file with the extension `.comet`. This will cause Urbit to boot a [comet](https://urbit.org/docs/glossary/comet/) in a pier named for the `.comet` file (less the extension).
  * e.g. starting with an empty file `my-urbit-bot.comet` will result in Urbit booting a comet into the pier
    `my-urbit-bot` under your volume.
- An existing pier as a directory `<shipname>`. You can migrate an existing ship to a new docker container in this way by placing its pier under the volume.
  * e.g. if your ship is `sampel-palnet` then you likely have a directory `sampel-palnet` whose path you pass to `./urbit` when starting. [Move your pier](https://urbit.org/using/operations/using-your-ship/#moving-your-pier) directory to the volume and then start the container.

The first two options result in Urbit attempting to boot either the ship named by the name of the keyfile, or a comet. In both cases, after that boot is successful, the `.key` or `.comet` file will be removed from the volume and the pier will take its place.

In consequence, it is safe to remove the container and start a new container which mounts the same volume, e.g. to upgrade the version of the urbit binary by running a later container version. It is also possible to stop the container and then move the pier away e.g. to a location where you will run it directly with the Urbit binary.

### Ports
The image includes `EXPOSE` directives for TCP port 80 and UDP port 34343. Port `80` is used for Urbit's HTTP interface for both [Landscape](https://urbit.org/docs/glossary/landscape/) and for [API calls](https://urbit.org/using/integrating-api/) to the ship. Port `34343` is used by [Ames](https://urbit.org/docs/glossary/ames/) for ship-to-ship communication.

You can either pass the `-P` flag to docker to map ports directly to the corresponding ports on the host, or map them individually with `-p` flags. For local testing the latter is often convenient, for instance to remap port 80 to an unprivileged port.

## Extending

You likely do not want to extend this image. External applications which interact with Urbit do so primarily via an HTTP API, which should be exposed as described above. For containerized applications using Urbit, it is more appropriate to use a container orchestration service such as Docker Compose or Kubernetes to run Urbit alongside other containers which will interface with its API.

## Development
The docker image is built by a Nix derivation in the [`nix/pkgs/docker-image/default.nix`](https://github.com/urbit/urbit/tree/master/nix/pkgs/docker-image/default.nix) file under the Urbit git repository.
