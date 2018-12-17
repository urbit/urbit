#!/usr/bin/env bash

if ! [[ -f $MESON_BUILD_ROOT/urbit-worker ]]; then
  ln -s $MESON_BUILD_ROOT/urbit $MESON_BUILD_ROOT/urbit-worker
fi
