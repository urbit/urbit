#ifndef ANACHRONISM_PARSER_H
#define ANACHRONISM_PARSER_H

#include <anachronism/common.h>

typedef enum telnet_parser_event_type
{
  TELNET_EV_PARSER_DATA,
  TELNET_EV_PARSER_COMMAND,
  TELNET_EV_PARSER_OPTION,
  TELNET_EV_PARSER_SUBNEGOTIATION,
  TELNET_EV_PARSER_WARNING,
} telnet_parser_event_type;

typedef struct telnet_parser_event
{
  telnet_parser_event_type type;
} telnet_parser_event;

typedef struct telnet_parser_data_event
{
  telnet_parser_event SUPER_;
  const telnet_byte* data;
  size_t length;
} telnet_parser_data_event;

typedef struct telnet_parser_command_event
{
  telnet_parser_event SUPER_;
  telnet_byte command;
} telnet_parser_command_event;

typedef struct telnet_parser_option_event
{
  telnet_parser_event SUPER_;
  telnet_byte command;
  telnet_byte option;
} telnet_parser_option_event;

typedef struct telnet_parser_subnegotiation_event
{
  telnet_parser_event SUPER_;
  int active;
  telnet_byte option;
} telnet_parser_subnegotiation_event;

typedef struct telnet_parser_warning_event
{
  telnet_parser_event SUPER_;
  const char* message;
  size_t position;
} telnet_parser_warning_event;



typedef struct telnet_parser telnet_parser;

typedef void (*telnet_parser_callback)(telnet_parser* parser, telnet_parser_event* event);


telnet_parser* telnet_parser_new(void* userdata, telnet_parser_callback callback);
void telnet_parser_free(telnet_parser* parser);

telnet_error telnet_parser_get_userdata(telnet_parser* parser, void** userdata);

telnet_error telnet_parser_parse(telnet_parser* parser,
                                 const telnet_byte* data,
                                 size_t length,
                                 size_t* bytes_used);

telnet_error telnet_parser_interrupt(telnet_parser* parser);

#endif // ANACHRONISM_PARSER_H
