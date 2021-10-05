# Keccak-tiny Library
# Options:
#    *  KECCAK_UNROLLED - Enable unrolled implementation (but disable main one).

HEADERS += \
    $$PWD/keccak-tiny.h \
    $$PWD/define-macros.h

contains(DEFINES, KECCAK_UNROLLED) {
    SOURCES += $$PWD/keccak-tiny-unrolled.c
} else {
    SOURCES += $$PWD/keccak-tiny.c
}
