#!/usr/bin/env bash
# support running off a tarball that doesn't contain binary pills
(( $($1 ../../bin/ivory.pill) > 512 )) || curl -L https://github.com/urbit/urbit/raw/urbit-v$URBIT_VERSION/bin/ivory.pill > ../../bin/ivory.pill

poor_mans_xxd () {
  cch=0
  echo "unsigned char $2[] = {"
  while IFS='' read line
  do
    for i in $line
    do
      echo -n " 0x$i,"
      cch=$((cch+1))
    done
    echo
  done < <(od -An -v -tx1 $1)
  echo "};"
  echo "unsigned int $2_len = $cch;"
}

[ -e include/ca-bundle.h ] || poor_mans_xxd $2 include_ca_bundle_crt >include/ca-bundle.h
[ -e include/ivory.h     ] || poor_mans_xxd ../../bin/ivory.pill u3_Ivory_pill >include/ivory.h
