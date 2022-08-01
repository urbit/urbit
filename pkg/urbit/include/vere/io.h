/// @file io.h
///
/// Bridge header file between the Rust-implemented IO drivers and Vere.
///
/// Any comment that references a Rust file (i.e. a file with a `.rs` extension)
/// refers to the [`io_drivers` repo](https://github.com/mcevoypeter/io_drivers)
/// unless otherwise noted.

#ifndef U3_VERE_IO_H
#define U3_VERE_IO_H

/// The return status of a driver. See the `Status` enum in `src/lib.rs`.
typedef enum {
  Success,
  BadSource,
  BadChannel,
  BadSink,
  NoRuntime,
  NoDriver,
} Status;

/// Launch the HTTP client IO driver that is implemented in Rust. See
/// `http_client_run()` in `src/http/client.rs` for more.
c3_y
http_client_run(void);

#endif /* ifndef U3_VERE_IO_H */
