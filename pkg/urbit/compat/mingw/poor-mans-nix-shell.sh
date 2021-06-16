# ensure required mingw packages are installed
mpkgs=(cmake curl gcc jq libuv make wslay)
pacman -S --needed autoconf automake-wrapper libtool patch ${mpkgs[@]/#/mingw-w64-x86_64-}

declare -a cdirs
declare -a ldirs
declare -A hdeps
sources=(../../nix/sources.json ../../nix/sources-mingw.json)
deriver=urbit-mingw-build
depdirs=
nixpath=${NIX_STORE-../build}

# LDFLAGS doesn't like absolute paths
if [ "${nixpath:0:1}" == "/" ]
then
  mkdir -p $nixpath
  nixpath=$(realpath --relative-to=. $nixpath)
fi

hex2nixbase32 () {
  local digits='0123456789abcdfghijklmnpqrsvwxyz'
  local bits=0
  local left=0 # number of bits left in $bits
  local i=0
  while ((1))
  do
    while ((left>=5))
    do
      echo -n ${digits:$((bits&31)):1}
      bits=$((bits>>5))
      left=$((left-5))
    done
    if ((i == ${#1}))
    then
      break
    fi
    char=0x${1:i:2}
    i=$((i+2))
    bits=$((bits|(char<<(left))))
    left=$((left+8))
  done
  echo -n ${digits:$bits:1}
}

buildnixdep () {
  echo Building dependency $key...
  local cache=https://app.cachix.org/api/v1/cache/${CACHIX_CACHE-}
  local hash=
  if [ -n "$url" ]
  then
    hash=${hdeps[$key]}
    dir=$nixpath/$hash-$key
    if [ -e $dir/.mingw~ ]
    then
      # dependency present, don't reupload
      hash=
    else
      # dependency absent, check the binary cache if configured
      if [ -n "${CACHIX_CACHE-}" ]
      then
        echo Checking binary cache for $hash-$key...
        narinfo="$cache/${hash}.narinfo"
        if curl -fLI "$narinfo"
        then
          url="$cache/$(curl -fL -H "Accept: application/json" "$narinfo"|jq -r '.url')"
          echo Found $url
          strip=0
          hash=
        fi
      fi
      mkdir -p $dir
      pushd $dir
      curl -fL "$url"|(tar --strip $strip -xzf - || true)
      popd
    fi
  else
    # local dependency
    dir=../$key
  fi

  # patch and build the dependency if necessary
  if [ ! -e $dir/.mingw~ ]
  then
    local patch=compat/mingw/$key.patch
    [ -e $patch ] && patch -d $dir -p 1 <$patch
    pushd $dir
    eval "$cmdprep"
    eval make "$cmdmake"
    touch .mingw~
    popd
  fi

  # if configured, upload freshly built dependency to binary cache
  if [ -n "$hash" -a -n "${CACHIX_AUTH_TOKEN-}" ]
  then
    (
    echo Uploading freshly built $hash-$key to binary cache...
    tar -C $dir -czf $hash.tar .
    local size=$(stat -c '%s' $hash.tar)
    read filehash _ < <(sha256sum $hash.tar)
    curl -fL -H "Content-Type: application/gzip" -H "Authorization: Bearer $CACHIX_AUTH_TOKEN" --data-binary @"$hash.tar" "$cache/nar"
    curl -fL -H "Content-Type: application/json" -H "Authorization: Bearer $CACHIX_AUTH_TOKEN" --data-binary @- "$cache/${hash}.narinfo" <<EOF
{
  "cStoreHash": "$hash",
  "cStoreSuffix": "$key",
  "cNarHash": "sha256:$(hex2nixbase32 $filehash)",
  "cNarSize": $size,
  "cFileHash": "$filehash",
  "cFileSize": $size,
  "cReferences": [],
  "cDeriver": "$deriver"
}
EOF
    echo Done. ) || true
    rm $hash.tar || true
  fi
}

# I have to go over the sources files several times
# because jq does not have a way to invoke external programs

# list external dependencies, create hash map and directory replacement regex
# use -j and \u0000 to work around https://github.com/stedolan/jq/issues/1870
while read -rd "" key json
do
  # create 'store hash' from sources.json data and patch
  patch=compat/mingw/$key.patch
  read hash _ < <((
  echo -n $json
  [ -e $patch ] && cat $patch)|sha256sum)
  hash=$(hex2nixbase32 $hash)
  hdeps[$key]=$hash
  # NB: this path substitution works only in local dependencies
  depdirs="$depdirs|gsub(\"\\\\.\\\\./$key\";\"$nixpath/$hash-$key\")"
done < <(jq --arg deriver "$deriver" -Sscrj 'add|to_entries|.[]|select(.value.mingw)|select(.value.url)|.key," ",{($deriver):(.value)},"\u0000"' ${sources[@]})

# build dependencies, create include and library directory arrays
. <(jq -sr 'add|to_entries|.[]|select(.value.mingw)|"
unset dir
key=\(.key|@sh) \\
url=\(.value.url//""|@sh) \\
strip=\(.value.mingw.strip+1) \\
cmdprep=\(.value.mingw.prepare//""'"$depdirs"'|@sh) \\
cmdmake=\(.value.mingw.make//""'"$depdirs"'|@sh) \\
buildnixdep # sets dir
\(.value.mingw.include//"."|if type == "array" then . else [.] end|map("cdirs+=(-I$dir/\(.))")|join("\n"))
\(.value.mingw.lib//"."|if type == "array" then . else [.] end|map("ldirs+=(-L$dir/\(.))")|join("\n"))"' ${sources[@]})
