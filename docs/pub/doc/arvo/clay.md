<div class="short">

`%clay`
=======

`%clay` is our filesystem.

`%clay` is version-controlled, referentially-transparent, and global.
While this filesystem is stored in `%clay`, it is mirrored to Unix for
convenience. Unix tells `%clay`s whenever a file changes in the Unix
copy of the filesystem so that the change may be applied. `%clay` tells
unix whenever an app or vane changes the filesystem so that the change
can be effected in Unix. Apps and vanes may use `%clay` to write to the
filesystem, query it, and subscribe to changes in it. Ford and gall use
`%clay` to serve up apps and web pages.

`%clay` includes three components. First is the filesystem/version
control algorithms, which are mostly defined in `++ze` and `++zu` in
zuse. Second is the write, query, and subscription logic. Finally, there
is the logic for communicating requests to, and receiving requests from,
foreign ships.

</div>

------------------------------------------------------------------------

<list></list>
