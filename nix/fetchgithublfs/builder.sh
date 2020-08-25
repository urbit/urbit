source $stdenv/setup

set -eo pipefail

src=${src:?src not set}
owner=${owner:?owner not set}
repo=${repo:?repo not set}

say() {
  if [[ -z "$QUIET" ]]; then
    echo "$@" >&2
  fi
}

die() {
  echo "$@" >&2
  exit 1
}

# Assert the given src path is a lfs pointer.
if [[ "$(head -c 7 "$src")" != "version" ]]; then
  header "copying lfs contents verbatim from $src to $out"

  cp "$src" "$out"

  stopNest

  exit 0
fi

header "reading lfs pointer metadata from $src"

# Parse the second column of every line from the pointer.
pointer=($(awk '{print $2}' "$src"))

name="lfs-file"
oid="${pointer[1]#sha256:}"
size="${pointer[2]}"

say "oid is $oid"
say "size is $size"

curl=(
    curl
    --location
    --max-redirs 20
    --retry 3
    --disable-epsv
    --cookie-jar cookies
    $NIX_CURL_FLAGS
)

if ! [ -f "$SSL_CERT_FILE" ]; then
    curl+=(--insecure)
fi

url="https://github.com/${owner}/${repo}.git/info/lfs/objects/batch"
headers="Accept: application/vnd.git-lfs+json"
body=$(cat <<EOF 
{
  "operation": "download", 
  "objects": [{"oid": "$oid", "size": $size}]
}
EOF
)

header "get pointer metadata from $url"

href=$("${curl[@]}" -H "$headers" -d "$body" "$url" | jq -r '.objects[0].actions.download.href')

header "get file contents from remote"

# Pozor: the $href contains credential and signature information.
"${curl[@]}" -s --output "$out" "$href"

stopNest
