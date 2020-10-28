{ lib, stdenvNoCC, coreutils, google-cloud-sdk, xxd }:

# Somewhat annoyingly due to needing to use Google Storage's Content-MD5
# to ensure a fixed output derivation - we need an md5sum of the file to
# upload. This is in additional to any sha256sum you might want to actually
# name the object key under.

{ bucket, object, name, file, contentMD5, contentType, serviceAccountKey
, preferLocalBuild ? true }:

assert lib.asserts.assertMsg (builtins.isString serviceAccountKey)
  "`serviceAccountKey` must contain the JSON contents of a service-account key";

let

  uri = "gs://${bucket}/${lib.removePrefix "/" object}";

in stdenvNoCC.mkDerivation {
  name = "push-${lib.strings.sanitizeDerivationName name}";

  nativeBuildInputs = [ coreutils google-cloud-sdk xxd ];

  phases = [ "installPhase" ];

  installPhase = ''
    set -euo pipefail

    export HOME="."

    gcloud auth activate-service-account --key-file=- <<< '${serviceAccountKey}'

    local_md5=$(echo -n '${contentMD5}' | xxd -r -p | base64)
    remote_md5=
       
    stat_uri() {
      header "retrieving md5 for ${uri}"

      remote_md5=$(gsutil stat '${uri}' \
        | sed -n -e '/Hash (md5):/{s/.*: *//p}' \
        | base64 -d \
        | xxd -p)
    }

    if ! stat_uri; then
       header "copying ${file} to ${uri}"

       gsutil -h "Content-MD5:$local_md5" \
              -h "Content-Type:${contentType}" \
         cp '${file}' '${uri}'

       if ! stat_uri; then
         echo "failed calculating remote uri md5" >&2
         exit 1 
       fi
    fi 

    # This is the same format as md5sum (double space separator) and
    # needs to match the .outputHash to ensure a fixed output derivation.
    echo -n "$remote_md5  ${uri}" > $out
  '';

  outputHashAlgo = "sha256";
  outputHashMode = "flat";
  outputHash = builtins.hashString "sha256" "${contentMD5}  ${uri}";

  inherit preferLocalBuild;
}
