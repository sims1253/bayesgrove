# Start the experimental local IPC server

Starts a local WebSocket server that exposes the bounded remote IPC
contract for external clients. On connection the server emits a full
`GraphSnapshot` message, then streams lightweight `ProtocolEvent`
updates as workflow state changes. The exposed command surface is
limited to the query/update operations marked `remote_accessible` in
[`bg_api_boundary()`](https://sims1253.github.io/bayesgrove/reference/bg_api_boundary.md);
lifecycle management, unrestricted filesystem access, and other
non-remote package entry points remain outside the protocol.

## Usage

``` r
bg_serve(project, host = "127.0.0.1", port = NULL, poll_interval = 0.2)
```

## Arguments

- project:

  A `bg_handle`.

- host:

  Host interface to bind (default `"127.0.0.1"`).

- port:

  TCP port to bind. If `NULL`, a random high port is selected. Explicit
  ports must be whole numbers between 1 and 65535.

- poll_interval:

  Poll interval in seconds for detecting out-of-band state changes,
  meaning updates made by other sessions or external processes outside
  the current websocket request/response path.

## Value

A `bg_server` handle with `url`, `host`, `port`, `protocol_version`,
`service()`, and [`stop()`](https://rdrr.io/r/base/stop.html) members.
Call `service(timeout = 0.05)` to process pending server events inside
the current R process; it delegates to
[`httpuv::service()`](https://rdrr.io/pkg/httpuv/man/service.html) and
invisibly returns `TRUE`. Call
[`stop()`](https://rdrr.io/r/base/stop.html) for a synchronous shutdown
that closes the listener, drops connected clients, and invalidates
future polling work.

## Details

Heavy execution should be coordinated through existing async pathways,
not by running long synchronous handlers in the foreground protocol
thread. Remote submission therefore routes async execution through
`mirai`.

## Examples

``` r
if (FALSE) { # \dontrun{
project <- bg_init(tempfile("bayesgrove-server-"))
server <- bg_serve(project)
server$url
server$service()
server$stop()
} # }
```
