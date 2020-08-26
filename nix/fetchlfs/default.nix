{ pkgs }:

{ src
, name ? baseNameOf src
, owner ? "urbit"
, repo ? "urbit"
, preferLocalBuild ? true
}:

assert builtins.isPath src;

let

  # Read the first 7 characters of the supplied `src` path for the required
  # `version` key per https://github.com/git-lfs/git-lfs/blob/master/docs/spec.md
  #
  # If `version` exists we assume we're dealing with a lfs pointer and extract
  # the `oid` and `size` from the metadata and write these into a JSON object .
  #
  # If the first 7 characters are unrecognised we assume the path is a binary
  # file and set both `oid` and `size` to `null`.
  pointer = pkgs.runCommandLocal "lfs-pointer-${name}" { } ''
    oid="null"
    size="null"

    if [[ "$(head -c 7 "${src}")" != "version" ]]; then
      header "lfs ${src} is a binary blob, skipping"
    else
      header "reading lfs pointer from ${src}"

      pointer=($(awk '{print $2}' "${src}"))
      oid="''${pointer[1]#sha256:}"
      size="''${pointer[2]}"
    fi

    cat <<EOF > "$out"
    {"oid": "$oid", "size": $size}
    EOF
  '';

  # Read the `oid` and `size` (which may be null) into an expression so we
  # can use them as the fixed-output derivation's `sha256 = oid`, and as the
  # payload to retrieve the actual lfs binary blob href.
  details = builtins.fromJSON (builtins.readFile pointer);

  # Encode `oid` and `size` into a download operation JSON request body per
  # https://github.com/git-lfs/git-lfs/blob/master/docs/api/batch.md
  #
  # This is done using toJSON to avoid bash quotation issues.
  download = builtins.toJSON {
    operation = "download";
    objects = [ details ];
  };

  # A fixed-output derivation using the previously calculated `oid` as
  # the expected sha256 output hash.
  #
  # - 1. Request the actual url of the binary file from the lfs batch api.
  # - 2. Download the binary file contents to `$out`.
  blob = pkgs.stdenvNoCC.mkDerivation {
    name = "lfs-blob-${name}";
    nativeBuildInputs = [ pkgs.curl pkgs.jq ];
    phases = [ "installPhase" ];
    installPhase = ''
      curl=(
          curl
          --location
          --max-redirs 20
          --retry 3
          --disable-epsv
          --cookie-jar cookies
          $NIX_CURL_FLAGS
        )

      url="https://github.com/${owner}/${repo}.git/info/lfs/objects/batch"
      headers="Accept: application/vnd.git-lfs+json"

      header "reading lfs metadata from $url"

      href=$("''${curl[@]}" -H "$headers" -d '${download}' "$url" \
        | jq -r '.objects[0].actions.download.href')

      header "download lfs blob from remote"

      # Pozor/Achtung: the $href contains credential and signature information.
      "''${curl[@]}" -s --output "$out" "$href"
    '';

    impureEnvVars = pkgs.stdenvNoCC.lib.fetchers.proxyImpureEnvVars;

    SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

    outputHashAlgo = "sha256";
    outputHashMode = "flat";
    outputHash = details.oid;

    inherit preferLocalBuild;
  };

in

# If `details.oid` is null then the `src` has been considered as already
# containing binary contents and so the fixed-output derivation `blob` must
# be skipped since we don't know the `oid` nor expected hash.
if details.oid == null || details.size == null then src else blob
