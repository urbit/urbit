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
The image includes `EXPOSE` directives for TCP port 80 and UDP port 34343. Port `80` is used for Urbit's HTTP interface for both [Landscape](https://urbit.org/docs/glossary/landscape/) and for [API calls](https://urbit.org/using/integrating-api/) to the ship. Port `34343` is set by default to be used by [Ames](https://urbit.org/docs/glossary/ames/) for ship-to-ship communication.

You can either pass the `-P` flag to docker to map ports directly to the corresponding ports on the host, or map them individually with `-p` flags. For local testing the latter is often convenient, for instance to remap port 80 to an unprivileged port. 

For best performance, you must map the Ames UDP port to the *same* port on the host. If you map to a different port Ames will not be able to make direct connections and your network performance may suffer somewhat. Note that using the same port is required for direct connections but is not by itself sufficient for them. If you are behind a NAT router or the host is not on a public IP address or you are firewalled, you may not achive direct connections regardless.

For this purpose you can force Ames to use a custom port. `/bin/start-urbit --port=$AMES_PORT` can be passed as an argument to the `docker start` command. Passing `/bin/start-urbit --port=13436` for example, would use port 13436. You must pass the name of the start script `/bin/start-urbit` in order to also pass arguments, if this is omitted your container will not start.

### Examples
Creating a volume for ~sampel=palnet:
```
docker volume create sampel-palnet
```

Copying key to sampel-palnet's volume (assumes default docker location):
```
sudo cp ~/sampel-palnet.key /var/lib/docker/volumes/sampel-palnet/_data/sampel-palnet.key
```

Using that volume and launching ~sampel-palnet on host port 8080 with Ames talking on the default host port 34343:
```
docker run -d -p 8080:80 -p 34343:34343/udp --name sampel-palnet \
    --mount type=volume,source=sampel-palnet,destination=/urbit \
    tloncorp/urbit
```

Using host port 8088 with Ames talking on host port 23232:
```
docker run -d -p 8088:80 -p 23232:23232/udp --name sampel-palnet \
    --mount type=volume,source=sampel-palnet,destination=/urbit \
    tloncorp/urbit /bin/start-urbit --port=23232
```

### Getting and resetting the Landscape +code
This docker image includes tools for retrieving and resetting the Landscape login code belonging to the planet, for programmatic use so the container does not need a tty. These scripts can be called using `docker container exec`.

Getting the code:
```
$ docker container exec sampel-palnet /bin/get-urbit-code
sampel-sampel-sampel-sampel
```

Resetting the code:
```
$ docker container exec sampel-palnet /bin/reset-urbit-code
OK
```

Once the code has been reset the new code can be obtained from `/bin/get-urbit-code`.

## Extending

You likely do not want to extend this image. External applications which interact with Urbit do so primarily via an HTTP API, which should be exposed as described above. For containerized applications using Urbit, it is more appropriate to use a container orchestration service such as Docker Compose or Kubernetes to run Urbit alongside other containers which will interface with its API.

## Development
The docker image is built by a Nix derivation in the [`nix/pkgs/docker-image/default.nix`](https://github.com/urbit/urbit/tree/master/nix/pkgs/docker-image/default.nix) file under the Urbit git repository.
