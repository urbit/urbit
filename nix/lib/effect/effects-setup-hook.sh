# ----------------------------------------------------------------------------
# prepare headers file for curl to talk to Hercules CI


initHerculesCIAPI() {
  herculesCIHeaders=$PWD/hercules-ci.headers
  jq </secrets/secrets.json >$herculesCIHeaders -r '"Authorization: Bearer \(."hercules-ci".token)"'
}
preInitHooks+=("initHerculesCIAPI")


# ----------------------------------------------------------------------------
# state crud

getStateFile() {
  local stateName="$1"
  local stateFileName="${2:-$1}"
  echo 1>&2 "fetching state file $stateName"
  while true; do
    http_code=$(curl \
      -H @$herculesCIHeaders \
      --retry-max-time 86400 --retry-connrefused --max-time 1800 \
      --silent --show-error \
      --location \
      "$HERCULES_CI_API_BASE_URL/api/v1/current-task/state/$stateName/data" \
      -o "$stateFileName" \
      -w '%{http_code}'
      );
    case $http_code in
      200|204)
        go_curl="false";
        break ;;
      408|421|429|5*)
        echo 1>&2 "http status $http_code. Retrying..."
        sleep 60
        continue ;;
      404)
        echo 1>&2 "state file does not exist."
        rm -f "$stateFileName"
        break ;;
      *)
        echo 1>&2 "request failed with fatal status $http_code"
        exit 1 ;;
    esac
  done
}

putStateFile() {
  local stateName="$1"
  local stateFileName="${2:-$1}"
  echo "pushing state file $stateName..."
  curl \
    -H @$herculesCIHeaders \
    --retry-max-time 86400 --retry-connrefused --max-time 1800 \
    --silent --show-error \
    --location --fail \
    -XPUT \
    --upload-file "$stateFileName" \
    "$HERCULES_CI_API_BASE_URL/api/v1/current-task/state/$stateName/data" \
    ;
  echo "pushing state successful."
}


# ----------------------------------------------------------------------------
# uploading state on error too

putStatePhaseOnFailure() {
  if [[ -n $putStatePhase ]]; then
    echo 'uploading state files after failure' 1>&2
    eval "$putStatePhase"
  fi
}

registerPutStatePhaseOnFailure() {
  failureHooks=("putStatePhaseOnFailure" "${failureHooks[@]}")
}

# ----------------------------------------------------------------------------
# unpack fix


simpleCopyUnpack() {
  local fn="$1"
  cp --no-preserve=ownership --recursive --reflink=auto \
     -- $fn "$(stripHash "$fn")" \
     ;
}

unpackCmdHooks+=(simpleCopyUnpack)


# ----------------------------------------------------------------------------
# warn if run in wrong environment


if [[ "true" != ${IN_HERCULES_CI_EFFECT:-} ]]; then

  if [[ -n ${NIX_LOG_FD:-} ]]; then
    cat 1>&2 <<EOF
WARNING: You are running a Hercules CI Effect in the Nix sandbox. This is very
         unlikely to work. Effects are described in the derivation format and
         have a lot in common, so you've probably tried to build it by accident.
EOF
  else
    cat 1>&2 <<EOF
WARNING: This effect is not running in the Hercules CI Effect sandbox.
EOF
  fi

fi


# ----------------------------------------------------------------------------
# using secrets


readSecretString() {
  local secretName="$1"
  local dataPath="$2"
  if ! jq -e -r </secrets/secrets.json '.[$secretName] | '"$dataPath" --arg secretName "$secretName"
  then echo 1>&2 "Could not find path $dataPath in secret $secretName"
  fi
}

readSecretJSON() {
  local secretName="$1"
  local dataPath="$2"
  jq -c </secrets/secrets.json '.[$secretName] | '"$dataPath" --arg secretName "$secretName"
}

writeAWSSecret() {
  local secretName="${1:-aws}"
  local profileName="${2:-default}"

  mkdir -p ~/.aws
  cat >>~/.aws/credentials <<EOF

[$profileName]
aws_secret_access_key = $(readSecretString "$secretName" .aws_secret_access_key)
aws_access_key_id = $(readSecretString "$secretName" .aws_access_key_id)

EOF
}

writeSSHKey() {
  local secretName="${1:-ssh}"
  local privateName="${2:-$HOME/.ssh/id_rsa}"
  mkdir -p "$(dirname "$privateName")"
  readSecretString "$secretName" .privateKey >"$privateName"
  chmod 0400 "$privateName"
  ssh-keygen -y -f "$privateName" >"$privateName.pub"
}

writeDockerKey() {
  local secretName="${1:-docker}"
  local directory="${2:-$HOME/.docker}"

  mkdir -p $directory

  readSecretString "$secretName" .clientKey >"$directory/key.pem"
  readSecretString "$secretName" .clientCertificate >"$directory/cert.pem"
  readSecretString "$secretName" .CACertificate >"$directory/ca.pem"

  # Please permission checks if any
  chmod 0400 "$directory"/{key,cert,ca}.pem
}
useDockerHost() {
  local host="${1}"
  local port="${2:-2376}"
  export DOCKER_HOST=tcp://$host:$port
  export DOCKER_TLS_VERIFY=1
}
