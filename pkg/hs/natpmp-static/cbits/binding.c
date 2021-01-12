/* $Id: natpmpc.c,v 1.13 2012/08/21 17:23:38 nanard Exp $ */
/* libnatpmp
Copyright (c) 2007-2011, Thomas BERNARD
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * The name of the author may not be used to endorse or promote products
	  derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <netinet/in.h>
#include <arpa/inet.h>
#include "natpmp.h"

// Additional binding code in C to make this more convenient to call from
// Haskell. libnatpmp expects that code which uses it to select() on an
// internal socket, which we don't want to expose to the Haskell bindings user.
//
// This is mostly an adaptation of the code in the demo natpmpc.c to use the
// select() loop.
int readNatResponseSynchronously(natpmp_t* natpmp, natpmpresp_t * response)
{
	fd_set fds;
	struct timeval timeout;
	int r;
	int sav_errno;

	do {
		FD_ZERO(&fds);
		FD_SET(natpmp->s, &fds);
		getnatpmprequesttimeout(natpmp, &timeout);
		r = select(FD_SETSIZE, &fds, NULL, NULL, &timeout);
		sav_errno = errno;
		if(r<0) {
			/* fprintf(stderr, "select():  errno=%d '%s'\n", */
			/*         sav_errno, strerror(sav_errno)); */
			return 1;
		}
		r = readnatpmpresponseorretry(natpmp, response);
		sav_errno = errno;
		/* printf("readnatpmpresponseorretry returned %d (%s)\n", */
		/*        r, r==0?"OK":(r==NATPMP_TRYAGAIN?"TRY AGAIN":"FAILED")); */
/* 		if(r<0 && r!=NATPMP_TRYAGAIN) { */
/* #ifdef ENABLE_STRNATPMPERR */
/* 			fprintf(stderr, "readnatpmpresponseorretry() failed : %s\n", */
/* 			        strnatpmperr(r)); */
/* #endif */
/* 			fprintf(stderr, "  errno=%d '%s'\n", */
/* 			        sav_errno, strerror(sav_errno)); */
/* 		} */
	} while(r==NATPMP_TRYAGAIN);

    return r;
}
