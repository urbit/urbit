
#include <lmdb.h>

MDB_env* u3m_lmdb_init(const char* log_path);
void u3m_lmdb_shutdown(MDB_env* env);

void u3m_lmdb_write_identity(MDB_env* environment,
                             u3_noun who,
                             u3_noun is_fake,
                             u3_noun life);
void u3m_lmdb_read_identity(MDB_env* environment,
                            u3_noun* who,
                            u3_noun* is_fake,
                            u3_noun* life);



