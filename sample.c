#include <stdio.h>
#include <string.h>

#include <ent/ent.h>


int
main()
{
	char buf[256];
	int  i;

	memset(buf, 0, sizeof(buf));
	if (0 != ent_getentropy(buf, sizeof(buf))) {
		perror("getentropy");
	}
	for (i = 0; i < sizeof buf; ++i) {
		printf("%02hhx", buf[i]);
	}
	puts("");
	return 0;
}
