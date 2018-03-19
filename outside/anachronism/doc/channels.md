# Telnet

## Channels
Telnet supports data multiplexing by way of 256 built-in sub-channels, each
identified by a byte in the interval [\x00-\xFF]. By switching between
channels, you can send completely separate streams of data through the same
connection.

All channels start out closed by default. To open a channel, one host must
request or offer a channel using IAC WILL &lt;id> or IAC DO &lt;id>. The remote host
then responds with IAC DO &lt;id> or IAC WILL &lt;id>, respectively. Alternatively,
the request may be denied using IAC DONT &lt;id> or IAC WONT &lt;id>, respectively.

In order to switch to a specific channel, the IAC SB &lt;id> sequence must
be used. All data sent afterwards will be routed through that specific channel.
To switch back to the main channel, IAC SE must be used. Note that subchannels
do not support any IAC sequences except IAC IAC (an escaped \xFF byte) and
IAC SE (return to the main channel). In particular, you cannot switch directly
from one subchannel to another: you must revert to the main channel first.

Due to the unbiased nature of Telnet, neither side of the connection is
automatically recognized as the server or the client. However, a host may either
request a channel (as a client) or offer a channel (as a server). The WILL/WONT
commands are used in the role of server ("I will", "I wont"), while DO/DONT
are used in the role of client ("You do", "You do not"). As such, a channel
may be opened twice (even simultaneously).

As an example, lets assume a terminal is connected to a server using Telnet. The
server offers MCCP (data compression), but wants to know what the terminal's
window size is. The following communication might occur:

    <server> IAC DO NAWS
    <server> IAC WILL MCCP
    <client> IAC WILL NAWS
    <client> IAC SB NAWS \x50 \x00 \x50 \x00 IAC SE
    <client> IAC DO MCCP
    <server> IAC SB MCCP IAC SE
    <server> (compressed data)

Notice that MCCP was negotiated such that the server offers the compression.
Only the server-to-client flow of data is compressed; the client would not
compress its data unless the channel was negotiated in the other direction as
well.

In general, a specific subchannel is tied to a specific Telnet subprotocol. For
example, the EXOPL subprotocol is assigned to channel 255, so that channel
should be avoided for any other purpose. A full list of registered subprotocols
can be found on the [IANA website][1].

[1]: http://www.iana.org/assignments/telnet-options
