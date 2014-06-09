#include <stdlib.h>
#include <string.h>
#include <anachronism/nvt.h>
#include <anachronism/parser.h>


#define TELOPT_TOGGLE_CALLBACK(nvt, telopt, where_, status_) do { \
  if ((nvt)->telopt_callback) { \
    telnet_telopt_toggle_event ev; \
    ev.SUPER_.type = TELNET_EV_TELOPT_TOGGLE; \
    ev.where = (where_); \
    ev.status = (status_); \
    \
    (nvt)->telopt_callback((nvt), (telopt), (telnet_telopt_event*)&ev); \
  } \
} while (0)

#define TELOPT_FOCUS_CALLBACK(nvt, telopt, status_) do { \
  if ((nvt)->telopt_callback) { \
    telnet_telopt_focus_event ev; \
    ev.SUPER_.type = TELNET_EV_TELOPT_FOCUS; \
    ev.status = (status_); \
    \
    (nvt)->telopt_callback((nvt), (telopt), (telnet_telopt_event*)&ev); \
  } \
} while (0)

#define TELOPT_DATA_CALLBACK(nvt, telopt, data_, length_) do { \
  if ((nvt)->telopt_callback) { \
    telnet_telopt_data_event ev; \
    ev.SUPER_.type = TELNET_EV_TELOPT_DATA; \
    ev.data = (data_); \
    ev.length = (length_); \
    \
    (nvt)->telopt_callback((nvt), (telopt), (telnet_telopt_event*)&ev); \
  } \
} while (0)

#define SEND_CALLBACK(nvt, data_, length_) do { \
  if ((nvt)->callback) { \
    telnet_send_event ev; \
    ev.SUPER_.type = TELNET_EV_SEND; \
    ev.data = (data_); \
    ev.length = (length_); \
    \
    (nvt)->callback((nvt), (telnet_event*)&ev); \
  } \
} while (0)


// Q Method of Implementing TELNET Option Negotiation
// ftp://ftp.rfc-editor.org/in-notes/rfc1143.txt
typedef enum qstate {
  Q_NO = 0,  Q_WANTYES, Q_WANTYESNO,
  Q_YES,     Q_WANTNO, Q_WANTNOYES,
} qstate;

typedef struct telnet_qstate
{
  unsigned remote : 3;
  unsigned local : 3;
} telnet_qstate;

struct telnet_nvt
{
  telnet_parser* parser;
  telnet_qstate options[256]; // track the state of each subnegotiation option
  short current_remote;
  
  telnet_nvt_event_callback callback;
  telnet_telopt_event_callback telopt_callback;
  telnet_negotiate_event_callback negotiate_callback;
  
  void* userdata;
};

static unsigned char telopt_status(telnet_nvt* nvt,
                                   telnet_byte telopt,
                                   telnet_telopt_location where)
{
  unsigned int qval = (where == TELNET_LOCAL) ?
    nvt->options[telopt].local :
    nvt->options[telopt].remote;
  
  switch (qval) {
    case Q_YES: case Q_WANTNO: case Q_WANTNOYES:
      return 1;
    default:
      return 0;
  }
}
#define telopt_subnegotiable(nvt, telopt) (telopt_status((nvt), (telopt), TELNET_REMOTE) || telopt_status((nvt), (telopt), TELNET_LOCAL))


static void send_option(telnet_nvt* nvt, telnet_byte command, telnet_byte telopt)
{
  const telnet_byte buf[] = {IAC_IAC, command, telopt};
  SEND_CALLBACK(nvt, buf, 3);
}

static void process_option_event(telnet_nvt* nvt,
                                 telnet_byte command,
                                 telnet_byte telopt)
{
  telnet_qstate* q = &nvt->options[telopt];
  // Every qstate begins zeroed-out, and Q_NO is 0.
  
  switch (command)
  {
    case IAC_WILL:
      switch (q->remote)
      {
        case Q_NO:
          if (nvt->negotiate_callback && nvt->negotiate_callback(nvt, telopt, TELNET_REMOTE)) {
            send_option(nvt, IAC_DO, telopt);
            q->remote = Q_YES;
            TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_REMOTE, 1);
          } else {
            send_option(nvt, IAC_DONT, telopt);
          }
          break;
        case Q_WANTNO:
          // error
          q->remote = Q_NO;
          break;
        case Q_WANTNOYES:
          // error
          q->remote = Q_YES;
          TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_REMOTE, 1);
          break;
        case Q_WANTYES:
          q->remote = Q_YES;
          TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_REMOTE, 1);
          break;
        case Q_WANTYESNO:
          send_option(nvt, IAC_DONT, telopt);
          q->remote = Q_WANTNO;
          TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_REMOTE, 1);
          break;
      }
      break;
    case IAC_WONT:
      switch (q->remote)
      {
        case Q_YES:
          send_option(nvt, IAC_DONT, telopt);
          q->remote = Q_NO;
          TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_REMOTE, 0);
          break;
        case Q_WANTNO:
          q->remote = Q_NO;
          break;
        case Q_WANTNOYES:
          send_option(nvt, IAC_DO, telopt);
          q->remote = Q_WANTYES;
          break;
        case Q_WANTYES:
          q->remote = Q_NO;
          break;
        case Q_WANTYESNO:
          q->remote = Q_NO;
          break;
      }
      break;
    case IAC_DO:
      switch (q->local)
      {
        case Q_NO:
          if (nvt->negotiate_callback && nvt->negotiate_callback(nvt, telopt, TELNET_LOCAL)) {
            send_option(nvt, IAC_WILL, telopt);
            q->local = Q_YES;
            TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_LOCAL, 1);
          } else {
            send_option(nvt, IAC_WONT, telopt);
          }
          break;
        case Q_WANTNO:
          // error
          q->local = Q_NO;
          break;
        case Q_WANTNOYES:
          // error
          q->local = Q_YES;
          TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_LOCAL, 1);
          break;
        case Q_WANTYES:
          q->local = Q_YES;
          TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_LOCAL, 1);
          break;
        case Q_WANTYESNO:
          send_option(nvt, IAC_WONT, telopt);
          q->local = Q_WANTNO;
          TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_LOCAL, 1);
          break;
      }
      break;
    case IAC_DONT:
      switch (q->local)
      {
        case Q_YES:
          send_option(nvt, IAC_DONT, telopt);
          q->local = Q_NO;
          TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_LOCAL, 0);
          break;
        case Q_WANTNO:
          q->local = Q_NO;
          break;
        case Q_WANTNOYES:
          send_option(nvt, IAC_WILL, telopt);
          q->local = Q_WANTYES;
          break;
        case Q_WANTYES:
          q->local = Q_NO;
          break;
        case Q_WANTYESNO:
          q->local = Q_NO;
          break;
      }
      break;
  }
}

static void process_data_event(telnet_nvt* nvt,
                               const telnet_byte* data,
                               size_t length)
{
  if (nvt->current_remote == -1) {
    // Main-line data
    if (nvt->callback) {
      telnet_data_event ev;
      ev.SUPER_.type = TELNET_EV_DATA;
      ev.data = data;
      ev.length = length;
      nvt->callback(nvt, (telnet_event*)&ev);
    }
  } else {
    // Telopt data
    telnet_byte telopt = (telnet_byte)nvt->current_remote;
    
    if (nvt->telopt_callback) {
      // Make sure the telopt is enabled
      if (telopt_subnegotiable(nvt, telopt)) {
        telnet_telopt_data_event ev;
        ev.SUPER_.type = TELNET_EV_TELOPT_DATA;
        ev.data = data;
        ev.length = length;
        nvt->telopt_callback(nvt, telopt, (telnet_telopt_event*)&ev);
      }
    }
  }
}

static void process_subnegotiation_event(telnet_nvt* nvt,
                                         int open,
                                         telnet_byte telopt)
{
  if (open) {
    nvt->current_remote = telopt;
  } else {
    nvt->current_remote = -1;
  }
  
  if (nvt->telopt_callback) {
    // Make sure the telopt is enabled
    if (telopt_subnegotiable(nvt, telopt)) {
      telnet_telopt_focus_event ev;
      ev.SUPER_.type = TELNET_EV_TELOPT_FOCUS;
      ev.focus = open;
      nvt->telopt_callback(nvt, telopt, (telnet_telopt_event*)&ev);
    }
  }
}

static void process_event(telnet_parser* parser, telnet_parser_event* event)
{
  telnet_nvt* nvt = NULL;
  telnet_parser_get_userdata(parser, (void*)&nvt);
    
  switch (event->type)
  {
    case TELNET_EV_PARSER_DATA:
    {
      telnet_parser_data_event* ev = (telnet_parser_data_event*)event;
      process_data_event(nvt, ev->data, ev->length);
      break;
    }
    
    case TELNET_EV_PARSER_OPTION:
    {
      telnet_parser_option_event* ev = (telnet_parser_option_event*)event;
      process_option_event(nvt, ev->command, ev->option);
      break;
    }
    
    case TELNET_EV_PARSER_SUBNEGOTIATION:
    {
      telnet_parser_subnegotiation_event* ev = (telnet_parser_subnegotiation_event*)event;
      process_subnegotiation_event(nvt, ev->active, ev->option);
      break;
    }
    
    case TELNET_EV_PARSER_COMMAND:
    {
      if (nvt->callback) {
        telnet_parser_command_event* parser_ev = (telnet_parser_command_event*) event;
        
        telnet_command_event ev;
        ev.SUPER_.type = TELNET_EV_COMMAND;
        ev.command = parser_ev->command;
        nvt->callback(nvt, (telnet_event*)&ev);
      }
      break;
    }
    
    case TELNET_EV_PARSER_WARNING:
    {
      if (nvt->callback) {
        telnet_parser_warning_event* parser_ev = (telnet_parser_warning_event*) event;
        
        telnet_warning_event ev;
        ev.SUPER_.type = TELNET_EV_WARNING;
        ev.message = parser_ev->message;
        ev.position = parser_ev->position;
        nvt->callback(nvt, (telnet_event*)&ev);
      }
      break;
    }
    
    default:
      break;
  }
}


telnet_nvt* telnet_nvt_new(void* userdata,
                           telnet_nvt_event_callback nvt_callback,
                           telnet_telopt_event_callback telopt_callback,
                           telnet_negotiate_event_callback negotiate_callback)
{
  telnet_nvt* nvt = malloc(sizeof(telnet_nvt));
  if (nvt)
  {
    telnet_parser* parser = telnet_parser_new((void*)nvt, &process_event);
    if (parser)
    {
      memset(nvt, 0, sizeof(*nvt));
      nvt->parser = parser;
      nvt->callback = nvt_callback;
      nvt->telopt_callback = telopt_callback;
      nvt->negotiate_callback = negotiate_callback;
      nvt->userdata = userdata;
      nvt->current_remote = -1;
    }
    else
    {
      free(nvt);
      nvt = NULL;
    }
  }
  return nvt;
}

void telnet_nvt_free(telnet_nvt* nvt)
{
  if (nvt)
  {
    telnet_parser_free(nvt->parser);
    free(nvt);
  }
}

telnet_error telnet_get_userdata(telnet_nvt* nvt, void** userdata)
{
  if (!nvt)
    return TELNET_E_BAD_NVT;
  
  *userdata = nvt->userdata;
  return TELNET_E_OK;
}

telnet_error telnet_receive(telnet_nvt* nvt, const telnet_byte* data, size_t length, size_t* bytes_used)
{
  if (!nvt)
    return TELNET_E_BAD_NVT;
  
  return telnet_parser_parse(nvt->parser, data, length, bytes_used);
}

telnet_error telnet_interrupt(telnet_nvt* nvt)
{
  if (!nvt)
    return TELNET_E_BAD_NVT;
  
  return telnet_parser_interrupt(nvt->parser);
}


static int safe_concat(const telnet_byte* in, size_t inlen, telnet_byte* out, size_t outlen)
{
  // Copy as much as possible into the buffer.
  memcpy(out, in, (outlen < inlen) ? outlen : inlen);
  
  // true if everything could be copied, false otherwise
  return outlen >= inlen;
}

// Escapes any special characters in data, writing the result data to out.
// Returns -1 if not everything could be copied (and out is full).
// Otherwise returns the length of the data in out.
//
// To avoid potential -1 return values, pass in an out buffer double the length of the data buffer.
static size_t telnet_escape(const telnet_byte* data, size_t length, telnet_byte* out, size_t outsize)
{
  if (data == NULL || out == NULL)
    return 0;
  
  size_t outlen = 0;
  size_t left = 0;
  size_t right = 0;
  const char* seq = NULL;
  for (; right < length; ++right)
  {
    switch (data[right])
    {
      case IAC_IAC:
        seq = "\xFF\xFF";
        break;
      case '\r':
        // Only escape \r if it doesn't immediately precede \n.
        if (right + 1 >= length || data[right+1] != '\n')
        {
          seq = "\r\0";
          break;
        }
        // !!FALLTHROUGH!!
      default:
        continue; // Move to the next character
    }
    
    // Add any normal data that hasn't been added yet.
    if (safe_concat(data+left, right-left, out+outlen, outsize-outlen) == 0)
      return -1;
    outlen += right - left;
    left = right + 1;
    
    // Add the escape sequence.
    if (safe_concat((const telnet_byte*)seq, 2, out+outlen, outsize-outlen) == 0)
      return -1;
    outlen += 2;
  }
  
  // Add any leftover normal data.
  if (left < right)
  {
    if (safe_concat(data+left, right-left, out+outlen, outsize-outlen) == 0)
      return -1;
    outlen += right - left;
  }
  
  return outlen;
}

telnet_error telnet_send_data(telnet_nvt* nvt, const telnet_byte* data, const size_t length)
{
  if (!nvt)
    return TELNET_E_BAD_NVT;
  else if (!nvt->callback)
    return TELNET_E_OK; // immediate success since they apparently don't want the data to go anywhere
  
  // Due to the nature of the protocol, the most any one byte can be encoded as is two bytes.
  // Hence, the smallest buffer guaranteed to contain any input is double the length of the source.
  size_t bufsize = sizeof(telnet_byte) * length * 2;
  telnet_byte* buf = malloc(bufsize);
  if (!buf)
    return TELNET_E_ALLOC;
  
  bufsize = telnet_escape(data, length, buf, bufsize);
  
  SEND_CALLBACK(nvt, buf, bufsize);
  
  free(buf);
  buf = NULL;
  
  return TELNET_E_OK;
}

telnet_error telnet_send_command(telnet_nvt* nvt, const telnet_byte command)
{
  if (!nvt)
    return TELNET_E_BAD_NVT;
  else if (command >= IAC_SB || command == IAC_SE) 
    return TELNET_E_INVALID_COMMAND; // Invalid command
  
  const telnet_byte buf[] = {IAC_IAC, command};
  SEND_CALLBACK(nvt, buf, 2);
  
  return TELNET_E_OK;
}

telnet_error telnet_send_subnegotiation(telnet_nvt* nvt, const telnet_byte option, const telnet_byte* data, const size_t length)
{
  if (!nvt)
    return TELNET_E_BAD_NVT;
  else if (!telopt_subnegotiable(nvt, option))
    return TELNET_E_NOT_SUBNEGOTIABLE;
  else if (!nvt->callback)
    return TELNET_E_OK;
  
  // length*2 is the maximum buffer size needed for an escaped string.
  // The extra five bytes are for the IAC, SB, <option>, IAC, and SE frame around the data.
  size_t bufsize = (sizeof(telnet_byte) * length * 2) + 5;
  telnet_byte* buf = malloc(bufsize);
  if (!buf)
    return TELNET_E_ALLOC;
  
  // Begin with IAC SB <option>
  telnet_byte iac[] = {IAC_IAC, IAC_SB, option};
  memcpy(buf, iac, 3);
  
  // Add the subnegotiation body
  size_t escaped_length = telnet_escape(data, length, buf+3, bufsize-3) + 3;
  
  // End with IAC SE
  iac[1] = IAC_SE;
  memcpy(buf+escaped_length, iac, 2);
  escaped_length += 2;
  
  SEND_CALLBACK(nvt, buf, escaped_length);
  
  free(buf);
  buf = NULL;
  
  return TELNET_E_OK;
}


telnet_error telnet_telopt_enable(telnet_nvt* nvt,
                                  telnet_byte telopt,
                                  telnet_telopt_location where)
{
  if (!nvt)
    return TELNET_E_BAD_NVT;
  
  telnet_qstate* q = &nvt->options[telopt];
  if (where == TELNET_LOCAL) {
    switch (q->local)
    {
      case Q_NO:
        q->local = Q_WANTYES;
        send_option(nvt, IAC_WILL, telopt);
        break;
      case Q_WANTNO:
        q->local = Q_WANTNOYES;
        break;
      case Q_WANTYESNO:
        q->local = Q_WANTYES;
        break;
    }
  } else {
    switch (q->remote)
    {
      case Q_NO:
        q->remote = Q_WANTYES;
        send_option(nvt, IAC_DO, telopt);
        break;
      case Q_WANTNO:
        q->remote = Q_WANTNOYES;
        break;
      case Q_WANTYESNO:
        q->remote = Q_WANTYES;
        break;
    }
  }
  
  return TELNET_E_OK;
}

telnet_error telnet_telopt_disable(telnet_nvt* nvt,
                                   telnet_byte telopt,
                                   telnet_telopt_location where)
{
  if (!nvt)
    return TELNET_E_BAD_NVT;
  
  telnet_qstate* q = &nvt->options[telopt];
  if (where == TELNET_LOCAL) {
    switch (q->local)
    {
      case Q_YES:
        send_option(nvt, IAC_WONT, telopt);
        q->local = Q_WANTNO;
        TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_LOCAL, 0);
        break;
      case Q_WANTNOYES:
        q->local = Q_WANTNO;
        break;
      case Q_WANTYES:
        q->local = Q_WANTYESNO;
        break;
    }
  } else {
    switch (q->remote)
    {
      case Q_YES:
        send_option(nvt, IAC_DONT, telopt);
        q->remote = Q_WANTNO;
        TELOPT_TOGGLE_CALLBACK(nvt, telopt, TELNET_REMOTE, 0);
        break;
      case Q_WANTNOYES:
        q->remote = Q_WANTNO;
        break;
      case Q_WANTYES:
        q->remote = Q_WANTYESNO;
        break;
    }
  }
  
  return TELNET_E_OK;
}

telnet_error telnet_telopt_status(telnet_nvt* nvt,
                                  telnet_byte telopt,
                                  telnet_telopt_location where,
                                  unsigned char* status)
{
  if (!nvt)
    return TELNET_E_BAD_NVT;
  
  *status = telopt_status(nvt, telopt, where);
  return TELNET_E_OK;
}
