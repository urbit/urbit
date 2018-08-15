/* vere/found.c
**
**  This file is in the public domain.
*/

#include "all.h"

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <dirent.h>
#include <stdint.h>
#include <uv.h>
#include <termios.h>
#include <term.h>
#include <errno.h>
#include <libgen.h>
#include <ftw.h>

#include "vere/vere.h"

#define FDB_API_VERSION 520
#include <fdb_c.h>

#if 0
void
_found_test()
{

  /* cluster */
 FDBFuture * cluf = fdb_create_cluster( "clust" );
  
      DLLEXPORT WARN_UNUSED_RESULT fdb_error_t
    fdb_future_get_cluster( FDBFuture* f, FDBCluster** out_cluster );


   FDBFuture * clust = fdb_cluster_create_database( FDBCluster* c, uint8_t const* db_name,
                                 int db_name_length );


  DLLEXPORT WARN_UNUSED_RESULT fdb_error_t
    fdb_future_get_database( FDBFuture* f, FDBDatabase** out_database );


      DLLEXPORT WARN_UNUSED_RESULT fdb_error_t
    fdb_database_create_transaction( FDBDatabase* d,
                                     FDBTransaction** out_transaction );


}

#endif

