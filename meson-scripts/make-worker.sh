#!/bin/bash

if ! [[ -d $MESON_SOURCE_ROOT/bin ]]; then
  mkdir -p $MESON_SOURCE_ROOT/bin
fi

if [[ -d $MESON_BUILD_ROOT ]]; then
  if [[ -f $MESON_BUILD_ROOT/urbit ]]; then
    cp $MESON_BUILD_ROOT/urbit $MESON_SOURCE_ROOT/bin/
    if ! [[ -f $MESON_SOURCE_ROOT/bin/urbit-worker ]]; then
      ln -s $MESON_SOURCE_ROOT/bin/urbit $MESON_SOURCE_ROOT/bin/urbit-worker
	fi
  fi
fi
