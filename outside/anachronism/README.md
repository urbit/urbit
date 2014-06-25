# Anachronism
Anachronism is a fully-compliant implementation of [the Telnet protocol][wiki-telnet]. Fallen
out of favor in this day and age, most people only know it as a command-line
tool for debugging HTTP. Today, Telnet is most commonly used in the realm of
[MUDs][wiki-muds], though there are still a few other niches filled by Telnet.

Anachronism offers a simple API for translating between streams of data and
events, and is completely network-agnostic. Anachronism also offers **channels**, an
abstraction layer which treats Telnet as a data multiplexer. Channels make it
extremely easy to build reusable modules for Telnet sub-protocols such
as MCCP (MUD Client Compression Protocol), which can be written once and plugged
into any application that wants to include support.

[wiki-telnet]: http://en.wikipedia.org/wiki/Telnet (Telnet at Wikipedia)
[wiki-muds]: http://en.wikipedia.org/wiki/MUD (MUDs at Wikipedia)

## Installation
While Anachronism has no dependencies and is theoretically cross-platform, I've
only written a Makefile for Linux. Help would be appreciated for making this
work across more platforms.

    make
    sudo make install

This will install Anachronism's shared and static libraries to /usr/local/lib,
and its header files to /usr/local/include/anachronism/. You may also need to
run `ldconfig` to make Anachronism available to your project's compiler/linker.

## Usage
The anachronism/nvt.h header can be consulted for more complete documentation.

### Basic usage
The core type exposed by Anachronism is the telnet\_nvt, which represents the
Telnet RFC's "Network Virtual Terminal". An NVT is created using
telnet\_nvt\_new(). When creating an NVT, you must provide it with a set of
callbacks to send events to, and an optional void\* to store as the event
handler's context. You can use telnet\_recv() to process incoming data, and
the telnet\_send\_\*() set of functions to emit outgoing data.

    #include <stdio.h>
    #include <anachronism/nvt.h>
    
    void on_event(telnet_nvt* nvt, telnet_event* event)
    {
      switch (event->type)
      {
        // A data event (normal text received)
        case TELNET_EV_DATA:
        {
          telnet_data_event* ev = (telnet_data_event*)event;
          printf("[IN]: %.*s\n", ev->length, ev->data);
          break;
        }
        
        // Outgoing data emitted by the NVT
        case TELNET_EV_SEND:
        {
          telnet_send_event* ev = (telnet_send_event*)event;
          printf("[OUT]: %.*s\n", ev->length, ev->data);
          break;
        }
      }
    }
    
    int main()
    {
      // Create an NVT
      telnet_nvt* nvt = telnet_nvt_new(NULL, &on_event, NULL, NULL);
      
      // Process some incoming data
      const char* data = "foo bar baz";
      telnet_receive(nvt, (const telnet_byte*)data, strlen(data), NULL);
      
      // Free the NVT
      telnet_nvt_free(nvt);
      return 0;
    }

### Telopts
Anachronism provides an easy-to-use interface to Telnet's "telopt" functionality
via the telnet\_telopt\_*() set of functions. As telopts are negotiated and
utilized, events are sent to the telopt callback provided to telnet_nvt_new().

    #include <stdio.h>
    #include <anachronism/nvt.h>
    
    void on_event(telnet_nvt* nvt, telnet_event* event)
    {
      switch (event->type)
      {
        // Outgoing data emitted by the NVT
        case TELNET_EV_SEND:
        {
          telnet_send_event* ev = (telnet_send_event*)event;
          printf("[OUT]: %.*s\n", ev->length, ev->data);
          break;
        }
      }
    }
    
    void on_telopt_event(telnet_nvt* nvt, telnet_byte telopt, telnet_telopt_event* event)
    {
      // telopt is the telopt this event was triggered for
      
      switch (event->type)
      {
        case TELNET_EV_TELOPT_TOGGLE:
          telnet_telopt_toggle_event* ev = (telnet_telopt_toggle_event*)event;
          // ev->where is TELNET_TELOPT_LOCAL or TELNET_TELOPT_REMOTE,
          //     corresponding to Telnet's WILL/WONT and DO/DONT commands.
          // ev->status is TELNET_TELOPT_ON or TELNET_TELOPT_OFF.
          break;
        case TELNET_EV_TELOPT_FOCUS:
          telnet_telopt_focus_event* ev = (telnet_telopt_focus_event*)event;
          // ev->focus is 1 or 0 depending on if a subnegotiation packet has
          //     begun or ended.
          break;
        case TELNET_EV_TELOPT_DATA:
          telnet_telopt_data_event* ev = (telnet_telopt_data_event*)event;
          // ev->data is a pointer to the received data.
          // ev->length is the length of the data buffer.
          break;
      }
    }
    
    int main()
    {
      // Create an NVT
      telnet_nvt* nvt = telnet_nvt_new(NULL, &on_event, &on_telopt_event, NULL);
      
      // Ask to enable a telopt locally (a WILL command)
      telnet_request_enable(nvt, 230, TELNET_LOCAL);
      
      // Process some incoming data
      const char* data = "\xFF\xFD\xE6" // IAC DO 230  (turn channel on)
                         "\xFF\xFA\xE6" // IAC SB 230  (switch to channel)
                         "foo bar baz"                 (send data)
                         "\xFF\xF0";    // IAC SE      (switch to main)
      telnet_receive(nvt, (const telnet_byte*)data, strlen(data), NULL);
      
      // Free the NVT
      telnet_nvt_free(nvt);
      return 0;
    }

### Interrupting
    TODO: Explain how to interrupt the parser.

## Alternatives
* [libtelnet][github-libtelnet], by Elanthis<br>
  It incorporates a number of (rather MUD-specific) protocols by default,
  though its API is quite different.

[github-libtelnet]: https://github.com/elanthis/libtelnet (libtelnet on GitHub)

## Credits
Someone from #startups on Freenode IRC suggested the name (I'm sure as a joke).
If you read this, remind me who you are so I can credit you properly!
