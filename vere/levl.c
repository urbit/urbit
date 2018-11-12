
void _test_leveldb()
{
  printf("******************** leveldb\n");

  leveldb_options_t * lop_u = leveldb_options_create();
  char * errstr;
  leveldb_t* ldb_u = leveldb_open(lop_u, "name", &errstr);

  leveldb_writebatch_t*  bat_u = leveldb_writebatch_create();

  char * key = "1";
  char * val = "testing";
  leveldb_writebatch_put(bat_u,
                              key, strlen(key),
                              val, strlen(val));

  
  leveldb_write(ldb_u,
                     lop_u,
                     bat_u,
                     & errstr);

  leveldb_writebatch_destroy(bat_u );
  leveldb_close(ldb_u);

 leveldb_options_destroy(lop);
}

