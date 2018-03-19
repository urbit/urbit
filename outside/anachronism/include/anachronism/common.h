#ifndef ANACHRONISM_COMMON_H
#define ANACHRONISM_COMMON_H

#include <stdlib.h> /* for size_t */

// Telnet bytes must be unsigned
typedef unsigned char telnet_byte;

// Error codes returned from API functions
// Positive codes are success/notice codes.
// Nonpositive codes are errors.
// ALLOC is 0 for parity with the NULL result from malloc().
typedef enum telnet_error
{
  TELNET_E_NOT_SUBNEGOTIABLE = -4, // The telopt is not open for subnegotiation.
  TELNET_E_BAD_PARSER        = -3, // The telnet_parser* passed is NULL
  TELNET_E_BAD_NVT           = -2, // The telnet_nvt* passed is NULL
  TELNET_E_INVALID_COMMAND   = -1, // The telnet_byte passed is not an allowed command in this API method
  TELNET_E_ALLOC             =  0, // Not enough memory to allocate essential library structures
  TELNET_E_OK                =  1, // Huge Success!
  TELNET_E_INTERRUPT         =  2, // Parser interrupted by user code.
} telnet_error;

#endif // ANACHRONISM_COMMON_H
