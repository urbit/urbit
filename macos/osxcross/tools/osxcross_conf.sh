#!/usr/bin/env bash

pushd "${0%/*}" &>/dev/null

OSXCROSS_CONF="../target/bin/osxcross-conf"
[ -f $OSXCROSS_CONF ] || { OSXCROSS_CONF=$(which osxcross-conf 2>/dev/null) || exit 1; }

$OSXCROSS_CONF || exit 1

$(dirname $OSXCROSS_CONF)/osxcross-env

popd &>/dev/null
