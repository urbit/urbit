#ifndef ED25519_H
#define ED25519_H

#include <stddef.h>

#if defined(_WIN32)
	#if defined(ED25519_BUILD_DLL)
		#define ED25519_DECLSPEC __declspec(dllexport)
	#elif defined(ED25519_DLL)
		#define ED25519_DECLSPEC __declspec(dllimport)
	#else
		#define ED25519_DECLSPEC
	#endif
#else
	#define ED25519_DECLSPEC
#endif


#ifdef __cplusplus
extern "C" {
#endif

#ifndef ED25519_NO_SEED
int ED25519_DECLSPEC ed25519_create_seed(unsigned char *seed);
#endif

int ED25519_DECLSPEC ed25519_create_keypair(unsigned char *verify_key, unsigned char *sign_key, unsigned char *seed);
int ED25519_DECLSPEC ed25519_sign(unsigned char *signature, const unsigned char *message, size_t message_len, const unsigned char *sign_key);
int ED25519_DECLSPEC ed25519_verify(const unsigned char *signature, const unsigned char *message, size_t message_len, const unsigned char *verify_key);


#ifdef __cplusplus
}
#endif

#endif