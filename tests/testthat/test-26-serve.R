serve_ns <- function(name) {
  getFromNamespace(name, "bayesgrove")
}

serve_pump <- function(timeout = 0.05) {
  httpuv::service(timeout = timeout)
  later::run_now(timeoutSecs = 0)
  invisible(TRUE)
}

serve_wait_until <- function(predicate, timeout = 3) {
  deadline <- Sys.time() + timeout

  repeat {
    serve_pump()
    if (isTRUE(predicate())) {
      return(invisible(TRUE))
    }
    if (Sys.time() > deadline) {
      stop("Timed out waiting for websocket condition.", call. = FALSE)
    }
  }
}

serve_collect_messages <- function(url) {
  messages <- list()
  opened <- FALSE
  closed <- FALSE
  errors <- character()

  client <- websocket::WebSocket$new(url, autoConnect = FALSE)
  client$onOpen(function(event) {
    opened <<- TRUE
    invisible(NULL)
  })
  client$onMessage(function(event) {
    messages[[length(messages) + 1L]] <<- jsonlite::fromJSON(
      event$data,
      simplifyVector = FALSE
    )
    invisible(NULL)
  })
  client$onError(function(event) {
    errors <<- c(
      errors,
      if (!is.null(event$message)) event$message else "websocket error"
    )
    invisible(NULL)
  })
  client$onClose(function(event) {
    closed <<- TRUE
    invisible(NULL)
  })
  client$connect()

  list(
    client = client,
    opened = function() opened,
    closed = function() closed,
    messages = function() messages,
    errors = function() errors
  )
}

serve_skip_if_socket_unavailable <- function() {
  server <- tryCatch(
    httpuv::startServer("127.0.0.1", 39491L, app = list()),
    error = function(e) e
  )

  if (inherits(server, "error")) {
    testthat::skip(
      "Local TCP server sockets are unavailable in this environment."
    )
  }

  httpuv::stopServer(server)
  invisible(TRUE)
}

describe("bg_serve()", {
  it("rejects unexpected command arguments before dispatch", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    messages <- list()
    fake_ws <- list(
      send = function(payload) {
        messages[[length(messages) + 1L]] <<- jsonlite::fromJSON(
          payload,
          simplifyVector = FALSE
        )
        invisible(TRUE)
      }
    )

    state <- new.env(parent = emptyenv())
    state$project <- handle
    state$protocol_version <- serve_ns("bg_protocol_version")()
    state$clients <- list(client_1 = fake_ws)
    state$stopped <- FALSE
    state$poll_interval <- 0.05
    state$signature <- serve_ns("bg_serve_state_signature")(handle)

    payload <- jsonlite::toJSON(
      list(
        protocol_version = serve_ns("bg_protocol_version")(),
        message_type = "Command",
        command_id = "cmd_status_bad",
        command = "bg_status",
        args = list(auto_advance = TRUE)
      ),
      auto_unbox = TRUE,
      null = "null"
    )

    serve_ns("bg_serve_handle_message")(state, fake_ws, FALSE, payload)

    expect_length(messages, 1)
    expect_equal(messages[[1]]$message_type, "CommandResult")
    expect_false(messages[[1]]$ok)
    expect_equal(messages[[1]]$error$code, "invalid_command")
    expect_match(messages[[1]]$error$message, "allowed schema variant")
  })

  it("uses cheap file-change tokens for poll signatures", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    testthat::local_mocked_bindings(
      bg_status = function(...) {
        stop("bg_status should not be called for poll signatures")
      },
      bg_pending_gates = function(...) {
        stop("bg_pending_gates should not be called for poll signatures")
      },
      bg_jobs = function(...) {
        stop("bg_jobs should not be called for poll signatures")
      },
      bg_list_branches = function(...) {
        stop("bg_list_branches should not be called for poll signatures")
      },
      .package = "bayesgrove"
    )

    signature <- serve_ns("bg_serve_state_signature")(handle)

    expect_type(signature$signature, "character")
    expect_equal(length(signature$signature), 1)
  })

  it("detects same-size rewrites in file-change tokens", {
    token <- serve_ns("bg_serve_file_change_token")
    path <- tempfile()

    writeLines("abc", path)
    original_mtime <- file.info(path)$mtime[[1]]
    first <- token(path)

    writeLines("def", path)
    Sys.setFileTime(path, original_mtime)
    second <- token(path)

    expect_equal(first$size, second$size)
    expect_equal(first$mtime, second$mtime)
    expect_false(identical(first$hash, second$hash))
  })

  it("defers valid command execution off the websocket callback", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    messages <- list()
    fake_ws <- list(
      send = function(payload) {
        messages[[length(messages) + 1L]] <<- jsonlite::fromJSON(
          payload,
          simplifyVector = FALSE
        )
        invisible(TRUE)
      }
    )

    state <- new.env(parent = emptyenv())
    state$project <- handle
    state$protocol_version <- serve_ns("bg_protocol_version")()
    state$clients <- list(client_1 = fake_ws)
    state$stopped <- FALSE
    state$poll_interval <- 0.05
    state$signature <- serve_ns("bg_serve_state_signature")(handle)

    payload <- jsonlite::toJSON(
      list(
        protocol_version = serve_ns("bg_protocol_version")(),
        message_type = "Command",
        command_id = "cmd_status",
        command = "bg_status",
        args = stats::setNames(list(), character())
      ),
      auto_unbox = TRUE,
      null = "null"
    )

    serve_ns("bg_serve_handle_message")(state, fake_ws, FALSE, payload)
    expect_length(messages, 0)

    later::run_now(timeoutSecs = 0)
    expect_length(messages, 1)
    expect_equal(messages[[1]]$message_type, "CommandResult")
    expect_true(messages[[1]]$ok)
  })

  it("queues a ProtocolEvent after a mutating command handler runs", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(result = TRUE)
    })

    messages <- list()
    fake_ws <- list(
      send = function(payload) {
        messages[[length(messages) + 1L]] <<- jsonlite::fromJSON(
          payload,
          simplifyVector = FALSE
        )
        invisible(TRUE)
      }
    )

    state <- new.env(parent = emptyenv())
    state$project <- handle
    state$protocol_version <- serve_ns("bg_protocol_version")()
    state$clients <- list(client_1 = fake_ws)
    state$stopped <- FALSE
    state$poll_interval <- 0.05
    state$signature <- serve_ns("bg_serve_state_signature")(handle)

    payload <- jsonlite::toJSON(
      list(
        protocol_version = serve_ns("bg_protocol_version")(),
        message_type = "Command",
        command_id = "cmd_add",
        command = "bg_add_node",
        args = list(kind = "source", label = "Queued event")
      ),
      auto_unbox = TRUE,
      null = "null"
    )

    serve_ns("bg_serve_handle_message")(state, fake_ws, FALSE, payload)
    later::run_now(timeoutSecs = 0)
    later::run_now(timeoutSecs = 0)

    types <- vapply(messages, `[[`, character(1), "message_type")
    expect_true("CommandResult" %in% types)
    expect_true("ProtocolEvent" %in% types)
  })

  it("starts and sends a GraphSnapshot on connect", {
    skip_if_not_installed("websocket")
    serve_skip_if_socket_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    server <- bg_serve(handle, poll_interval = 0.05)
    withr::defer(server$stop())

    client <- serve_collect_messages(server$url)
    withr::defer(try(client$client$close(), silent = TRUE))

    serve_wait_until(function() length(client$messages()) >= 1L)

    first <- client$messages()[[1]]
    expect_equal(first$message_type, "GraphSnapshot")
    expect_true(bg_validate_protocol_object(first, "bg_graph_snapshot"))
    expect_equal(client$errors(), character())
  })

  it("returns CommandResult and ProtocolEvent for an allowed mutating command", {
    skip_if_not_installed("websocket")
    serve_skip_if_socket_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(result = TRUE)
    })
    server <- bg_serve(handle, poll_interval = 0.05)
    withr::defer(server$stop())

    client <- serve_collect_messages(server$url)
    withr::defer(try(client$client$close(), silent = TRUE))

    serve_wait_until(function() length(client$messages()) >= 1L)

    command <- list(
      protocol_version = serve_ns("bg_protocol_version")(),
      message_type = "Command",
      command_id = "cmd_add",
      command = "bg_add_node",
      args = list(kind = "source", label = "Remote source")
    )
    expect_true(bg_validate_protocol_object(command, "bg_command"))

    client$client$send(jsonlite::toJSON(
      command,
      auto_unbox = TRUE,
      null = "null"
    ))

    serve_wait_until(function() {
      msgs <- client$messages()
      sum(
        vapply(msgs, `[[`, character(1), "message_type") == "CommandResult"
      ) >=
        1L &&
        sum(
          vapply(msgs, `[[`, character(1), "message_type") == "ProtocolEvent"
        ) >=
          1L
    })

    msgs <- client$messages()
    result <- Filter(
      function(msg) identical(msg$message_type, "CommandResult"),
      msgs
    )[[1]]
    event <- Filter(
      function(msg) identical(msg$message_type, "ProtocolEvent"),
      msgs
    )[[1]]

    expect_true(result$ok)
    expect_true(bg_validate_protocol_object(result, "bg_command_result"))
    expect_true(bg_validate_protocol_object(event, "bg_protocol_event"))
  })

  it("returns the descriptive extension registry over the wire", {
    skip_if_not_installed("websocket")
    serve_skip_if_socket_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "source")
    server <- bg_serve(handle, poll_interval = 0.05)
    withr::defer(server$stop())

    client <- serve_collect_messages(server$url)
    withr::defer(try(client$client$close(), silent = TRUE))

    serve_wait_until(function() length(client$messages()) >= 1L)

    client$client$send(jsonlite::toJSON(
      list(
        protocol_version = serve_ns("bg_protocol_version")(),
        message_type = "Command",
        command_id = "cmd_extension_registry",
        command = "bg_extension_registry",
        args = stats::setNames(list(), character())
      ),
      auto_unbox = TRUE,
      null = "null"
    ))

    serve_wait_until(function() {
      any(vapply(
        client$messages(),
        function(msg) {
          identical(msg$message_type, "CommandResult") &&
            identical(msg$command_id, "cmd_extension_registry")
        },
        logical(1)
      ))
    })

    result <- Filter(
      function(msg) {
        identical(msg$message_type, "CommandResult") &&
          identical(msg$command_id, "cmd_extension_registry")
      },
      client$messages()
    )[[1]]

    expect_true(result$ok)
    expect_equal(result$result$policy$registry_mode, "descriptive")
    expect_false(result$result$policy$gui_extension_api)
  })

  it("does not emit a duplicate ProtocolEvent for one mutating command", {
    skip_if_not_installed("websocket")
    serve_skip_if_socket_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      list(result = TRUE)
    })
    server <- bg_serve(handle, poll_interval = 0.05)
    withr::defer(server$stop())

    client <- serve_collect_messages(server$url)
    withr::defer(try(client$client$close(), silent = TRUE))

    serve_wait_until(function() length(client$messages()) >= 1L)

    client$client$send(jsonlite::toJSON(
      list(
        protocol_version = serve_ns("bg_protocol_version")(),
        message_type = "Command",
        command_id = "cmd_add_once",
        command = "bg_add_node",
        args = list(kind = "source", label = "Only one event")
      ),
      auto_unbox = TRUE,
      null = "null"
    ))

    serve_wait_until(function() {
      msgs <- client$messages()
      any(vapply(
        msgs,
        function(msg) {
          identical(msg$message_type, "CommandResult") &&
            identical(msg$command_id, "cmd_add_once")
        },
        logical(1)
      ))
    })

    Sys.sleep(0.25)
    for (i in seq_len(5)) {
      serve_pump()
    }

    protocol_events <- Filter(
      function(msg) identical(msg$message_type, "ProtocolEvent"),
      client$messages()
    )
    expect_length(protocol_events, 1L)
    expect_equal(protocol_events[[1]]$command_id, "cmd_add_once")
  })

  it("surfaces protocol version mismatch clearly", {
    skip_if_not_installed("websocket")
    serve_skip_if_socket_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    server <- bg_serve(handle, poll_interval = 0.05)
    withr::defer(server$stop())

    client <- serve_collect_messages(server$url)
    withr::defer(try(client$client$close(), silent = TRUE))

    serve_wait_until(function() length(client$messages()) >= 1L)

    client$client$send(jsonlite::toJSON(
      list(
        protocol_version = "999.0.0",
        message_type = "Command",
        command_id = "cmd_bad_version",
        command = "bg_status",
        args = stats::setNames(list(), character())
      ),
      auto_unbox = TRUE,
      null = "null"
    ))

    serve_wait_until(function() {
      any(vapply(
        client$messages(),
        function(msg) {
          identical(msg$message_type, "CommandResult") &&
            identical(msg$command_id, "cmd_bad_version")
        },
        logical(1)
      ))
    })

    result <- Filter(
      function(msg) {
        identical(msg$message_type, "CommandResult") &&
          identical(msg$command_id, "cmd_bad_version")
      },
      client$messages()
    )[[1]]

    expect_false(result$ok)
    expect_equal(result$error$code, "version_mismatch")
    expect_match(
      result$error$message,
      paste("expected", serve_ns("bg_protocol_version")()),
      fixed = TRUE
    )
  })

  it("supports disconnect and reconnect without restarting the R session", {
    skip_if_not_installed("websocket")
    serve_skip_if_socket_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    server <- bg_serve(handle, poll_interval = 0.05)
    withr::defer(server$stop())

    client1 <- serve_collect_messages(server$url)
    withr::defer(try(client1$client$close(), silent = TRUE))
    serve_wait_until(function() length(client1$messages()) >= 1L)

    client1$client$close()
    serve_wait_until(client1$closed)

    client2 <- serve_collect_messages(server$url)
    withr::defer(try(client2$client$close(), silent = TRUE))
    serve_wait_until(function() length(client2$messages()) >= 1L)

    expect_equal(client2$messages()[[1]]$message_type, "GraphSnapshot")
    expect_equal(client2$errors(), character())
  })

  it("remains responsive while async work is coordinated through mirai", {
    skip_if_not_installed("websocket")
    skip_if_not_installed("mirai")
    serve_skip_if_socket_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "slow", executor = function(node, inputs) {
      Sys.sleep(0.5)
      list(result = "ok")
    })
    bg_add_node(handle, kind = "slow", label = "Slow node")

    server <- bg_serve(handle, poll_interval = 0.05)
    withr::defer(server$stop())

    client <- serve_collect_messages(server$url)
    withr::defer(try(client$client$close(), silent = TRUE))
    serve_wait_until(function() length(client$messages()) >= 1L)

    client$client$send(jsonlite::toJSON(
      list(
        protocol_version = serve_ns("bg_protocol_version")(),
        message_type = "Command",
        command_id = "cmd_submit",
        command = "bg_submit",
        args = stats::setNames(list(), character())
      ),
      auto_unbox = TRUE,
      null = "null"
    ))

    serve_wait_until(function() {
      any(vapply(
        client$messages(),
        function(msg) {
          identical(msg$message_type, "CommandResult") &&
            identical(msg$command_id, "cmd_submit")
        },
        logical(1)
      ))
    })

    client$client$send(jsonlite::toJSON(
      list(
        protocol_version = serve_ns("bg_protocol_version")(),
        message_type = "Command",
        command_id = "cmd_status",
        command = "bg_status",
        args = stats::setNames(list(), character())
      ),
      auto_unbox = TRUE,
      null = "null"
    ))

    serve_wait_until(
      function() {
        any(vapply(
          client$messages(),
          function(msg) {
            identical(msg$message_type, "CommandResult") &&
              identical(msg$command_id, "cmd_status")
          },
          logical(1)
        ))
      },
      timeout = 2
    )

    status_result <- Filter(
      function(msg) {
        identical(msg$message_type, "CommandResult") &&
          identical(msg$command_id, "cmd_status")
      },
      client$messages()
    )[[1]]

    expect_true(status_result$ok)
    expect_true(bg_validate_protocol_object(status_result, "bg_command_result"))
  })

  it("answers protocol queries without reopening stored artifacts", {
    skip_if_not_installed("websocket")
    serve_skip_if_socket_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(result = list(value = 1L))
    })
    bg_add_node(handle, kind = "data", label = "Data")
    bg_run(handle, mode = "sync")

    testthat::local_mocked_bindings(
      bg_fetch_artifact = function(...) {
        stop("artifact fetch should not be used for snapshot queries")
      },
      .package = "bayesgrove"
    )

    server <- bg_serve(handle, poll_interval = 0.05)
    withr::defer(server$stop())

    client <- serve_collect_messages(server$url)
    withr::defer(try(client$client$close(), silent = TRUE))
    serve_wait_until(function() length(client$messages()) >= 1L)

    client$client$send(jsonlite::toJSON(
      list(
        protocol_version = serve_ns("bg_protocol_version")(),
        message_type = "Command",
        command_id = "cmd_snapshot",
        command = "bg_snapshot",
        args = stats::setNames(list(), character())
      ),
      auto_unbox = TRUE,
      null = "null"
    ))

    serve_wait_until(function() {
      any(vapply(
        client$messages(),
        function(msg) {
          identical(msg$message_type, "CommandResult") &&
            identical(msg$command_id, "cmd_snapshot")
        },
        logical(1)
      ))
    })

    result <- Filter(
      function(msg) {
        identical(msg$message_type, "CommandResult") &&
          identical(msg$command_id, "cmd_snapshot")
      },
      client$messages()
    )[[1]]

    expect_true(result$ok)
    expect_equal(result$result$message_type, "GraphSnapshot")
    expect_true(bg_validate_protocol_object(result$result, "bg_graph_snapshot"))
    expect_true("bg_execute_action" %in% names(result$result$command_surface))
  })

  it("executes protocol actions through bg_execute_action over the wire", {
    skip_if_not_installed("websocket")
    serve_skip_if_socket_unavailable()

    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )
    bg_register_node_kind(handle, "fit")
    seed_id <- bg_add_node(handle, kind = "fit", label = "Seed")
    branch <- bg_branch(handle, seed_id, label = "Alternative")

    action_id <- bg_next_actions(
      handle,
      scope = "branch",
      branch_id = branch$branch_id
    )$actions[[1]]$action_id

    server <- bg_serve(handle, poll_interval = 0.05)
    withr::defer(server$stop())

    client <- serve_collect_messages(server$url)
    withr::defer(try(client$client$close(), silent = TRUE))
    serve_wait_until(function() length(client$messages()) >= 1L)

    client$client$send(jsonlite::toJSON(
      list(
        protocol_version = serve_ns("bg_protocol_version")(),
        message_type = "Command",
        command_id = "cmd_execute_action",
        command = "bg_execute_action",
        args = list(
          action_id = action_id,
          overrides = list(
            choice = "observable_prediction",
            choice_label = "Predict y",
            rationale = "Set the branch goal from the GUI surface."
          )
        )
      ),
      auto_unbox = TRUE,
      null = "null"
    ))

    serve_wait_until(function() {
      msgs <- client$messages()
      any(vapply(
        msgs,
        function(msg) {
          identical(msg$message_type, "CommandResult") &&
            identical(msg$command_id, "cmd_execute_action")
        },
        logical(1)
      ))
    })

    result <- Filter(
      function(msg) {
        identical(msg$message_type, "CommandResult") &&
          identical(msg$command_id, "cmd_execute_action")
      },
      client$messages()
    )[[1]]

    expect_true(result$ok)
    expect_equal(result$result$kind, "record_decision")
    expect_equal(
      bg_get_goal(handle, branch$branch_id)$kind,
      "observable_prediction"
    )
  })
})
