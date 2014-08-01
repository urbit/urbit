#ifndef ANACHRONISM_ANACHRONISM_H
#define ANACHRONISM_ANACHRONISM_H

#ifdef __cplusplus
extern "C" {
#endif

#include <anachronism/common.h>

// predefined Telnet commands from 240-255
enum
{
  IAC_SE = 240,
  IAC_NOP,
  IAC_DM,
  IAC_BRK,
  IAC_IP,
  IAC_AO,
  IAC_AYT,
  IAC_EC,
  IAC_EL,
  IAC_GA,
  IAC_SB,
  IAC_WILL,
  IAC_WONT,
  IAC_DO,
  IAC_DONT,
  IAC_IAC,
};

typedef enum telnet_telopt_location
{
  TELNET_LOCAL,
  TELNET_REMOTE,
} telnet_telopt_location;


/**
 * NVT Events
 */
 
typedef enum telnet_event_type
{
  TELNET_EV_DATA,    /* A stretch of plain data was received. (data, length) */
  TELNET_EV_COMMAND, /* A simple IAC comamnd was recevied. (command) */
  TELNET_EV_WARNING, /* A non-fatal invalid sequence was received. (message, position) */
  TELNET_EV_SEND,    /* Outgoing data to be sent. (data, length) */
} telnet_event_type;

typedef struct telnet_event
{
  telnet_event_type type;
} telnet_event;

typedef struct telnet_data_event
{
  telnet_event SUPER_;
  const telnet_byte* data;
  size_t length;
} telnet_data_event;

typedef struct telnet_command_event
{
  telnet_event SUPER_;
  telnet_byte command;
} telnet_command_event;

typedef struct telnet_warning_event
{
  telnet_event SUPER_;
  const char* message;
  size_t position;
} telnet_warning_event;

typedef struct telnet_send_event
{
  telnet_event SUPER_;
  const telnet_byte* data;
  size_t length;
} telnet_send_event;


/**
 * Telopt Events
 */

typedef enum telnet_telopt_event_type
{
  TELNET_EV_TELOPT_TOGGLE,
  TELNET_EV_TELOPT_FOCUS,
  TELNET_EV_TELOPT_DATA,
} telnet_telopt_event_type;

typedef struct telnet_telopt_event
{
  telnet_telopt_event_type type;
} telnet_telopt_event;

typedef struct telnet_telopt_toggle_event
{
  telnet_telopt_event SUPER_;
  telnet_telopt_location where;
  unsigned char status;
} telnet_telopt_toggle_event;

typedef struct telnet_telopt_focus_event
{
  telnet_telopt_event SUPER_;
  unsigned char focus;
} telnet_telopt_focus_event;

typedef struct telnet_telopt_data_event
{
  telnet_telopt_event SUPER_;
  const telnet_byte* data;
  size_t length;
} telnet_telopt_data_event;



typedef struct telnet_nvt telnet_nvt;


typedef void (*telnet_nvt_event_callback)(telnet_nvt* nvt, telnet_event* event);
typedef void (*telnet_telopt_event_callback)(telnet_nvt* nvt, telnet_byte telopt, telnet_telopt_event* event);
typedef unsigned char (*telnet_negotiate_event_callback)(telnet_nvt* nvt, telnet_byte telopt, telnet_telopt_location where);

/**
  Creates a new Telnet NVT.
  
  Errors:
    TELNET_E_ALLOC - Unable to allocate enough memory for the NVT.
 */
telnet_nvt* telnet_nvt_new(void* userdata,
                           telnet_nvt_event_callback nvt_callback,
                           telnet_telopt_event_callback telopt_callback,
                           telnet_negotiate_event_callback negotiate_callback);

void telnet_nvt_free(telnet_nvt* nvt);

/**
  Every NVT can have some user-specific data attached, such as a user-defined struct.
  This can be accessed (primarily by event callbacks) to differentiate between NVTs.
  
  Errors:
    TELNET_E_BAD_NVT - Invalid telnet_nvt* parameter.
  
  Example:
    // assuming a FILE was passed to telnet_nvt_new():
    FILE out = NULL;
    telnet_get_userdata(nvt, (void**)&out);
 */
telnet_error telnet_get_userdata(telnet_nvt* nvt, void** udata);

/**
  Processes incoming data.
  If `bytes_used` is non-NULL, it will be set to the length of the string that
  was read. This is generally only useful if you use telnet_halt() in a callback.
  
  Errors:
    TELNET_E_BAD_NVT   - Invalid telnet_nvt* parameter.
    TELNET_E_ALLOC     - Unable to allocate destination buffer for incoming text.
    TELNET_E_INTERRUPT - User code interrupted the parser.
 */
telnet_error telnet_receive(telnet_nvt* nvt, const telnet_byte* data, size_t length, size_t* bytes_used);

/**
  If currently parsing (i.e. telnet_recv() is running), interrupts the parser.
  This is useful for things such as MCCP, where a Telnet sequence hails the start of
  data that must be decompressed before being parsed.
  
  Errors:
    TELNET_E_BAD_NVT - Invalid telnet_nvt* parameter.
 */
telnet_error telnet_interrupt(telnet_nvt* nvt);


/**
  Sends a string as a stream of escaped Telnet data.
  
  Errors:
    TELNET_E_BAD_NVT - Invalid telnet_nvt* parameter.
    TELNET_E_ALLOC   - Unable to allocate destination buffer for outgoing text.
 */
telnet_error telnet_send_data(telnet_nvt* nvt, const telnet_byte* data, const size_t length);

/**
  Sends a Telnet command.
  
  Errors:
    TELNET_E_BAD_NVT        - Invalid telnet_nvt* parameter.
    TELNET_E_INVALID_COMMAND    - The command cannot be WILL, WONT, DO, DONT, SB, or SE.
 */
telnet_error telnet_send_command(telnet_nvt* nvt, const telnet_byte command);

/**
  Sends a subnegotiation packet.
  
  Errors:
    TELNET_E_BAD_NVT        - Invalid telnet_nvt* parameter.
    TELNET_E_ALLOC          - Unable to allocate destination buffer for outgoing text.
 */
telnet_error telnet_send_subnegotiation(telnet_nvt* nvt, const telnet_byte option, const telnet_byte* data, const size_t length);


telnet_error telnet_telopt_enable(telnet_nvt* nvt, const telnet_byte telopt, telnet_telopt_location where);
telnet_error telnet_telopt_disable(telnet_nvt* nvt, const telnet_byte telopt, telnet_telopt_location where);
telnet_error telnet_telopt_status(telnet_nvt* nvt, const telnet_byte telopt, telnet_telopt_location where,  unsigned char* status);

#ifdef __cplusplus
}
#endif

#endif // ANACHRONISM_ANACHRONISM_H
