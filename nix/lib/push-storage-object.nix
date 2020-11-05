{ lib, makeEffect, python3, google-cloud-sdk }:

{ bucket, object, name, file, contentType, serviceAccountKey }:

assert lib.asserts.assertMsg (builtins.isString serviceAccountKey)
  "`serviceAccountKey` must contain the JSON contents of a service-account key";

let

  uri = "gs://${bucket}/${lib.removePrefix "/" object}";

in makeEffect {
  inherit file uri contentType serviceAccountKey;

  name = "push-${name}";

  inputs = [ (google-cloud-sdk.override { python = python3; }) ];

  dontUnpack = true;

  effectScript = ''
    export HOME="."

    gcloud auth activate-service-account --key-file=- <<< $serviceAccountKey

    stat_uri() {
      gsutil stat $uri
    }

    if ! stat_uri; then
      header "copying $file to $uri"

      gsutil -h "Content-Type:$contentType" cp $file $uri

      if ! stat_uri; then
        header "failed pushing $file to $url"
        exit 1 
      fi
    else
      header "$uri already exists, skipping"
    fi 
  '';
}
