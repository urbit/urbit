#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ent.h"

int main(void) {
    char buf[256] = {0};

    if (0 != ent_getentropy(buf, sizeof(buf))) {
        perror("getentropy");
        exit(1);
    }

    for (int i = 0; i < sizeof buf; i++) {
        printf("%02hhx", buf[i]);
    }

    puts("");
}
