#include "urcrypt.h"
#include "util.h"
#include <argon2.h>
#include <blake2.h>

// library convention is to have sizes in size_t, but argon2 wants them
// in uint32_t, so here's a helper macro for ensuring equivalence.
#define SZ_32(s) ( sizeof(size_t) <= sizeof(uint32_t) || s <= 0xFFFFFFFF )

const char*
urcrypt_argon2(uint8_t  type,
               uint32_t version,
               uint32_t threads,
               uint32_t memory_cost,
               uint32_t time_cost,
               size_t secret_length,
               uint8_t *secret,
               size_t associated_length,
               uint8_t *associated,
               size_t password_length,
               uint8_t *password,
               size_t salt_length,
               uint8_t *salt,
               size_t out_length,
               uint8_t *out,
               urcrypt_argon2_alloc_t alloc_ptr,
               urcrypt_argon2_free_t free_ptr)
{
  if ( !( SZ_32(secret_length) &&
          SZ_32(associated_length) &&
          SZ_32(password_length) &&
          SZ_32(salt_length) &&
          SZ_32(out_length) ) ) {
    return "length > 32 bits";
  }
  else {
    int (*f)(argon2_context*);
    int result;

    switch ( type ) {
      default:
        return "unknown type";
      case urcrypt_argon2_d:
        f = &argon2d_ctx;
        break;
      case urcrypt_argon2_i:
        f = &argon2i_ctx;
        break;
      case urcrypt_argon2_id:
        f = &argon2id_ctx;
        break;
      case urcrypt_argon2_u:
        f = &argon2u_ctx;
        break;
    }

    urcrypt__reverse(secret_length, secret);
    urcrypt__reverse(associated_length, associated);
    urcrypt__reverse(password_length, password);
    urcrypt__reverse(salt_length, salt);

    argon2_context context = {
      out,                   // output array, at least [digest length] in size
      out_length,            // digest length
      password,              // password array
      password_length,       // password length
      salt,                  // salt array
      salt_length,           // salt length
      secret,                // optional secret data
      secret_length,
      associated,            // optional associated data
      associated_length,
      time_cost,             // performance cost configuration
      memory_cost,
      threads,
      threads,
      version,               // algorithm version
      alloc_ptr,             // custom memory allocation function
      free_ptr,              // custom memory deallocation function
      ARGON2_DEFAULT_FLAGS   // by default only internal memory is cleared
    };

    result = (*f)(&context);

    if ( ARGON2_OK != result ) {
      return argon2_error_message(result);
    }
    else {
      urcrypt__reverse(out_length, out);
      return NULL;
    }
  }
}

int
urcrypt_blake2(size_t message_length,
               uint8_t *message,
               size_t key_length,
               uint8_t key[64],
               size_t out_length,
               uint8_t *out)
{
  if ( key_length > 64 ) {
    return -1;
  }
  else {
    urcrypt__reverse(message_length, message);
    urcrypt__reverse(key_length, key);

    if ( 0 != blake2b(out, out_length,
                      message, message_length,
                      key, key_length)) {
      return -1;
    }
    else {
      urcrypt__reverse(out_length, out);
      return 0;
    }
  }
}
