<div class="short">

`%ames`
=======

`%ames` is our networking protocol.

`%ames` is the name of both our network and the vane that communicates
over it. When Unix receives a packet over the correct UDP port, it pipes
it straight into `%ames` for handling. Also, all packets sent over the
`%ames` network are sent by the `%ames` vane. Apps and vanes may use
`%ames` to directly send messages to other ships. In general, apps use
gall and clay to communicate with other ships rather than using `%ames`
directly, but this isn't a requirement. Of course, gall and clay use
`%ames` behind the scenes to communicate across the network. These are
the only two vanes that use `%ames`.

`%ames` includes several significant components. Although the actual
crypto algorithms are defined in zuse, they're used extensively in
`%ames` for encrypting and decrypting packets. Congestion control and
routing is handled entirely in `%ames`. Finally, the actual `%ames`
protocol itself, including how to route incoming packets to the correct
vane or app, is defined in `%ames`.

</div>

<hr>
</hr>
<list></list>
