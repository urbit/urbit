#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ent.h>


int
main()
{
	char buf[256];
	int  i;

	memset(buf, 0, sizeof(buf));
	if (0 != ent_getentropy(buf, sizeof(buf))) {
		perror("getentropy");
		exit(1);
	}
	for (i = 0; i < sizeof buf; ++i) {
		printf("%02hhx", buf[i]);
	}
	puts("");
	return 0;
}
