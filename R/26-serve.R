#' Start the experimental local IPC server
#'
#' Starts a local WebSocket server that exposes the bounded remote IPC contract
#' for external clients. On connection the server emits a full
#' `GraphSnapshot` message, then streams lightweight `ProtocolEvent` updates as
#' workflow state changes. The exposed command surface is limited to the
#' query/update operations marked `remote_accessible` in `bg_api_boundary()`;
#' lifecycle management, unrestricted filesystem access, and other non-remote
#' package entry points remain outside the protocol.
#'
#' Heavy execution should be coordinated through existing async pathways, not by
#' running long synchronous handlers in the foreground protocol thread. Remote
#' submission therefore routes async execution through `mirai`.
#'
#' @param project A `bg_handle`.
#' @param host Host interface to bind (default `"127.0.0.1"`).
#' @param port TCP port to bind. If `NULL`, a random high port is selected.
#'   Explicit ports must be whole numbers between 1 and 65535.
#' @param poll_interval Poll interval in seconds for detecting out-of-band state
#'   changes, meaning updates made by other sessions or external processes
#'   outside the current websocket request/response path.
#'
#' @return A `bg_server` handle with `url`, `host`, `port`,
#'   `protocol_version`, `service()`, and `stop()` members. Call
#'   `service(timeout = 0.05)` to process pending server events inside the
#'   current R process; it delegates to `httpuv::service()` and invisibly
#'   returns `TRUE`. Call `stop()` for a synchronous shutdown that closes the
#'   listener, drops connected clients, and invalidates future polling work.
#'
#' @examples
#' \dontrun{
#' project <- bg_init(tempfile("bayesgrove-server-"))
#' server <- bg_serve(project)
#' server$url
#' server$service()
#' server$stop()
#' }
#' @export
bg_serve <- function(
  project,
  host = "127.0.0.1",
  port = NULL,
  poll_interval = 0.2
) {
  S7::check_is_S7(project, bg_handle)
  bg_validate_remote_command_boundary()

  if (!is.character(host) || length(host) != 1L || !nzchar(host)) {
    cli::cli_abort("{.arg host} must be a single non-empty string.")
  }
  if (!is.null(port)) {
    if (!is.numeric(port) || length(port) != 1L || is.na(port)) {
      cli::cli_abort("{.arg port} must be NULL or a single numeric value.")
    }
    if (port != as.integer(port)) {
      cli::cli_abort("{.arg port} must be a whole number.")
    }
    if (port < 1L || port > 65535L) {
      cli::cli_abort("{.arg port} must be between 1 and 65535.")
    }
  }
  if (!is.numeric(poll_interval) || length(poll_interval) != 1L) {
    cli::cli_abort("{.arg poll_interval} must be a single numeric value.")
  }
  if (poll_interval <= 0) {
    cli::cli_abort("{.arg poll_interval} must be greater than 0.")
  }

  state <- new.env(parent = emptyenv())
  state$project <- project
  state$host <- host
  state$protocol_version <- bg_protocol_version()
  state$clients <- list()
  state$server <- NULL
  state$stopped <- FALSE
  state$poll_interval <- as.numeric(poll_interval)
  state$signature <- bg_serve_state_signature(project)

  app <- list(
    onWSOpen = function(ws) {
      client_id <- bg_new_id("client")
      state$clients[[client_id]] <- ws

      ws$onClose(function(...) {
        state$clients[[client_id]] <- NULL
        invisible(NULL)
      })

      ws$onMessage(function(binary, message) {
        bg_serve_handle_message(state, ws, binary, message)
      })

      bg_schedule_graph_snapshot_send(state, ws)
      invisible(NULL)
    }
  )

  server_binding <- bg_start_server_binding(state$host, port, app)
  state$server <- server_binding$server
  state$port <- server_binding$port
  bg_schedule_server_poll(state)

  reg.finalizer(
    state,
    function(e) {
      bg_stop_server_state(e)
      invisible(NULL)
    },
    onexit = TRUE
  )

  server <- list(
    host = state$host,
    port = state$port,
    url = sprintf("ws://%s:%s/websocket", state$host, state$port),
    protocol_version = state$protocol_version,
    service = function(timeout = 0.05) {
      httpuv::service(timeout = timeout)
      invisible(TRUE)
    },
    stop = function() {
      bg_stop_server_state(state)
      invisible(TRUE)
    }
  )
  class(server) <- "bg_server"
  server
}

#' @export
print.bg_server <- function(x, ...) {
  cli::cli_text(cli::col_grey("<bg_server>"))
  cli::cli_bullets(c(
    "*" = "URL: {.val {x$url}}",
    "*" = "Protocol version: {.val {x$protocol_version}}"
  ))
  invisible(x)
}

#' @keywords internal
bg_protocol_version <- function() {
  "0.1.0"
}

#' @keywords internal
bg_drop_null_fields <- function(x) {
  if (!is.list(x)) {
    return(x)
  }

  keep <- !vapply(x, is.null, logical(1))
  x <- x[keep]

  lapply(x, bg_drop_null_fields)
}

bg_remote_command_registry <- function() {
  list(
    bg_snapshot = list(
      fn = "bg_remote_graph_snapshot",
      mutates_state = FALSE,
      allowed_args = character(),
      required_args = character()
    ),
    bg_status = list(
      fn = "bg_status",
      mutates_state = FALSE,
      allowed_args = character(),
      required_args = character(),
      normalize_args = function(args) {
        args$auto_advance <- FALSE
        args
      }
    ),
    bg_next_actions = list(
      fn = "bg_next_actions",
      mutates_state = FALSE,
      allowed_args = c("scope", "branch_id"),
      required_args = character()
    ),
    bg_list_branches = list(
      fn = "bg_list_branches",
      mutates_state = FALSE,
      allowed_args = character(),
      required_args = character()
    ),
    bg_extension_registry = list(
      fn = "bg_extension_registry",
      mutates_state = FALSE,
      allowed_args = character(),
      required_args = character()
    ),
    bg_branch_lineage = list(
      fn = "bg_branch_lineage",
      mutates_state = FALSE,
      allowed_args = "branch_id",
      required_args = "branch_id"
    ),
    bg_add_node = list(
      fn = "bg_add_node",
      mutates_state = TRUE,
      allowed_args = c("kind", "label", "params", "inputs", "metadata"),
      required_args = "kind"
    ),
    bg_connect = list(
      fn = "bg_connect",
      mutates_state = TRUE,
      allowed_args = c("from", "to", "edge_type", "metadata"),
      required_args = c("from", "to")
    ),
    bg_update_node = list(
      fn = "bg_update_node",
      mutates_state = TRUE,
      allowed_args = c("node_id", "label", "params", "metadata"),
      required_args = "node_id"
    ),
    bg_remove_node = list(
      fn = "bg_remove_node",
      mutates_state = TRUE,
      allowed_args = "node_id",
      required_args = "node_id"
    ),
    bg_answer_gate = list(
      fn = "bg_answer_gate",
      mutates_state = TRUE,
      allowed_args = c("id", "choice", "rationale", "refs", "evidence"),
      required_args = c("id", "choice", "rationale")
    ),
    bg_record_decision = list(
      fn = "bg_record_decision",
      mutates_state = TRUE,
      allowed_args = c(
        "scope",
        "prompt",
        "choice",
        "alternatives",
        "rationale",
        "refs",
        "evidence",
        "kind",
        "metadata"
      ),
      required_args = c("scope", "prompt", "choice", "rationale")
    ),
    bg_execute_action = list(
      fn = "bg_execute_action",
      mutates_state = TRUE,
      allowed_args = c("action_id", "overrides"),
      required_args = "action_id"
    ),
    bg_submit = list(
      fn = "bg_submit",
      mutates_state = TRUE,
      allowed_args = "targets",
      required_args = character(),
      normalize_args = function(args) {
        args$backend <- "mirai"
        args
      }
    ),
    bg_cancel = list(
      fn = "bg_cancel",
      mutates_state = TRUE,
      allowed_args = "run_id",
      required_args = "run_id"
    )
  )
}

#' @keywords internal
bg_remote_accessible_functions <- function() {
  registry <- bg_api_boundary()
  sort(registry$fn[registry$remote_accessible])
}

#' @keywords internal
bg_validate_remote_command_boundary <- function() {
  registry <- bg_remote_command_registry()
  remote_registry <- sort(names(registry))
  remote_api <- bg_remote_accessible_functions()

  missing <- setdiff(remote_api, remote_registry)
  if (length(missing) > 0) {
    cli::cli_abort(
      "Remote-accessible API entries missing from server registry: {.val {missing}}"
    )
  }

  extra <- setdiff(remote_registry, remote_api)
  if (length(extra) > 0) {
    cli::cli_abort(
      "Server registry exposes functions not marked remote_accessible: {.val {extra}}"
    )
  }

  missing_arg_specs <- names(Filter(
    function(spec) is.null(spec$allowed_args) || is.null(spec$required_args),
    registry
  ))
  if (length(missing_arg_specs) > 0) {
    cli::cli_abort(
      paste0(
        "Server registry entries missing explicit arg declarations: ",
        paste(missing_arg_specs, collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}

#' @keywords internal
bg_build_graph_snapshot_state <- function(project) {
  protocol <- bg_partition_protocol_by_scope(
    bg_next_actions(project),
    project
  )

  list(
    project_id = project@project_id,
    project_name = bg_read_project_config(project)$project_name %||%
      basename(project@path),
    graph = bg_read_graph(project),
    status = bg_drop_null_fields(bg_status(project, auto_advance = FALSE)),
    pending_gates = bg_protocol_named_list(bg_pending_gates(project)),
    branches = bg_protocol_named_list(bg_list_branches(project)),
    branch_goals = bg_protocol_named_list(
      bg_read_goal_registry(project)$branch_goals %||% list()
    ),
    protocol = protocol,
    command_surface = bg_protocol_command_surface(),
    extension_registry = bg_extension_registry(project)
  )
}

#' @keywords internal
bg_remote_graph_snapshot <- function(project) {
  bg_build_graph_snapshot_message(project)
}

#' @keywords internal
bg_build_graph_snapshot_message <- function(project) {
  utils::modifyList(
    list(
      protocol_version = bg_protocol_version(),
      message_type = "GraphSnapshot",
      emitted_at = bg_now_timestamp()
    ),
    bg_build_graph_snapshot_state(project)
  )
}

#' @keywords internal
bg_build_protocol_event_message <- function(
  project,
  event_kind = "state_changed",
  command_id = NULL,
  source = "poll"
) {
  message <- list(
    protocol_version = bg_protocol_version(),
    message_type = "ProtocolEvent",
    event_id = bg_new_id("evt"),
    event_kind = event_kind,
    source = source,
    emitted_at = bg_now_timestamp(),
    status = bg_drop_null_fields(bg_status(project, auto_advance = FALSE)),
    graph_version = as.integer(bg_read_graph(project)$version %||% 0L)
  )

  if (!is.null(command_id)) {
    message$command_id <- command_id
  }

  message
}

#' @keywords internal
bg_build_command_result_message <- function(
  command_id,
  ok,
  result = NULL,
  error = NULL
) {
  message <- list(
    protocol_version = bg_protocol_version(),
    message_type = "CommandResult",
    command_id = command_id,
    ok = isTRUE(ok),
    emitted_at = bg_now_timestamp()
  )

  if (isTRUE(ok)) {
    message$result <- result
  } else {
    message$error <- error %||%
      list(
        code = "command_failed",
        message = "Command failed."
      )
  }

  message
}

#' @keywords internal
bg_parse_command_message <- function(message) {
  payload <- jsonlite::fromJSON(message, simplifyVector = FALSE)
  if (!is.list(payload)) {
    cli::cli_abort("Command payload must be a JSON object.")
  }

  payload$args <- bg_protocol_named_list(payload$args %||% list())
  bg_validate_protocol_object(payload, "bg_command")
  payload
}

#' @keywords internal
bg_validate_remote_command_args <- function(command_name, args, spec) {
  args <- args %||% list()
  if (!is.list(args)) {
    cli::cli_abort(
      "Arguments for remote command {.val {command_name}} must be a JSON object."
    )
  }

  arg_names <- names(args)
  if (length(args) > 0 && is.null(arg_names)) {
    cli::cli_abort(
      "Arguments for remote command {.val {command_name}} must be named."
    )
  }

  blank_names <- arg_names[!nzchar(arg_names %||% character())]
  if (length(blank_names) > 0) {
    cli::cli_abort(
      "Arguments for remote command {.val {command_name}} must use non-empty names."
    )
  }

  extra <- setdiff(
    arg_names %||% character(),
    spec$allowed_args %||% character()
  )
  if (length(extra) > 0) {
    cli::cli_abort(
      "Unexpected arguments for remote command {.val {command_name}}: {.val {extra}}."
    )
  }

  missing <- setdiff(
    spec$required_args %||% character(),
    arg_names %||% character()
  )
  if (length(missing) > 0) {
    cli::cli_abort(
      "Missing required arguments for remote command {.val {command_name}}: {.val {missing}}."
    )
  }

  invisible(TRUE)
}

#' @keywords internal
bg_dispatch_remote_command <- function(project, command_name, args = list()) {
  registry <- bg_remote_command_registry()
  spec <- registry[[command_name]] %||% NULL
  if (is.null(spec)) {
    cli::cli_abort("Unknown or non-remote command {.val {command_name}}.")
  }

  bg_validate_remote_command_args(command_name, args, spec)

  if (is.function(spec$normalize_args)) {
    args <- spec$normalize_args(args %||% list())
  }

  fn <- get(spec$fn, envir = asNamespace("bayesgrove"))
  list(
    result = do.call(fn, c(list(project = project), args %||% list())),
    spec = spec
  )
}

#' @keywords internal
bg_serve_state_signature <- function(project) {
  payload <- list(
    graph = bg_serve_file_change_token(bg_storage_path(
      project,
      "graph",
      "graph.json"
    )),
    gates = bg_serve_file_change_token(bg_gate_specs_path(project)),
    branches = bg_serve_file_change_token(bg_branch_registry_path(project)),
    goals = bg_serve_file_change_token(bg_goal_registry_path(project)),
    decisions = bg_serve_file_change_token(
      bg_storage_path(project, "decisions", "decisions.jsonl")
    ),
    jobs = bg_serve_file_change_token(bg_storage_path(
      project,
      "runs",
      "jobs.jsonl"
    )),
    config = bg_serve_file_change_token(bg_storage_path(
      project,
      "config.json"
    )),
    summaries = bg_serve_file_change_token(bg_summary_log_path(project)),
    artifacts = bg_serve_file_change_token(bg_artifact_index_path(project))
  )

  list(
    signature = digest::digest(
      jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
      algo = "xxhash32"
    )
  )
}

#' @keywords internal
bg_serve_file_change_token <- function(path) {
  if (!file.exists(path)) {
    return(list(exists = FALSE))
  }

  info <- file.info(path)
  list(
    exists = TRUE,
    size = unname(info$size[[1]]),
    mtime = unname(as.numeric(info$mtime[[1]])),
    hash = digest::digest(
      path,
      algo = "xxhash32",
      serialize = FALSE,
      file = TRUE
    )
  )
}

#' @keywords internal
bg_serve_send <- function(ws, payload) {
  ws$send(
    jsonlite::toJSON(
      bg_sort_persisted_value(payload),
      auto_unbox = TRUE,
      null = "null"
    )
  )
}

#' @keywords internal
bg_serve_broadcast <- function(state, payload) {
  client_ids <- names(state$clients)
  if (length(client_ids) == 0) {
    return(invisible(FALSE))
  }

  for (client_id in client_ids) {
    ws <- state$clients[[client_id]] %||% NULL
    if (is.null(ws)) {
      next
    }

    tryCatch(
      bg_serve_send(ws, payload),
      error = function(...) {
        state$clients[[client_id]] <- NULL
        invisible(NULL)
      }
    )
  }

  invisible(TRUE)
}

#' @keywords internal
bg_schedule_graph_snapshot_send <- function(state, ws) {
  later::later(
    function() {
      if (isTRUE(state$stopped)) {
        return(invisible(FALSE))
      }

      tryCatch(
        bg_serve_send(ws, bg_build_graph_snapshot_message(state$project)),
        error = function(...) invisible(NULL)
      )

      invisible(TRUE)
    },
    delay = 0
  )
}

#' @keywords internal
bg_queue_protocol_event <- function(
  state,
  event_kind = "state_changed",
  command_id = NULL,
  source = "poll"
) {
  later::later(
    function() {
      if (isTRUE(state$stopped)) {
        return(invisible(FALSE))
      }

      bg_serve_broadcast(
        state,
        bg_build_protocol_event_message(
          project = state$project,
          event_kind = event_kind,
          command_id = command_id,
          source = source
        )
      )
      invisible(TRUE)
    },
    delay = 0
  )
}

#' @keywords internal
bg_schedule_remote_command <- function(state, ws, parsed) {
  later::later(
    function() {
      if (isTRUE(state$stopped)) {
        return(invisible(FALSE))
      }

      dispatched <- tryCatch(
        bg_dispatch_remote_command(
          project = state$project,
          command_name = parsed$command,
          args = parsed$args
        ),
        error = function(e) e
      )

      if (inherits(dispatched, "error")) {
        bg_serve_send(
          ws,
          bg_build_command_result_message(
            command_id = parsed$command_id,
            ok = FALSE,
            error = list(
              code = "command_failed",
              message = conditionMessage(dispatched)
            )
          )
        )
        return(invisible(FALSE))
      }

      if (isTRUE(dispatched$spec$mutates_state)) {
        # Record the post-command state immediately so the poll loop does not
        # rediscover the same mutation and emit a duplicate event.
        state$signature <- bg_serve_state_signature(state$project)
      }

      bg_serve_send(
        ws,
        bg_build_command_result_message(
          command_id = parsed$command_id,
          ok = TRUE,
          result = dispatched$result
        )
      )

      if (isTRUE(dispatched$spec$mutates_state)) {
        bg_queue_protocol_event(
          state = state,
          event_kind = "state_changed",
          command_id = parsed$command_id,
          source = parsed$command
        )
      }

      invisible(TRUE)
    },
    delay = 0
  )
}

#' @keywords internal
bg_serve_handle_message <- function(state, ws, binary, message) {
  if (isTRUE(binary)) {
    bg_serve_send(
      ws,
      bg_build_command_result_message(
        command_id = bg_new_id("cmd"),
        ok = FALSE,
        error = list(
          code = "binary_not_supported",
          message = "Binary websocket messages are not supported."
        )
      )
    )
    return(invisible(FALSE))
  }

  parsed <- tryCatch(
    bg_parse_command_message(message),
    error = function(e) e
  )
  if (inherits(parsed, "error")) {
    bg_serve_send(
      ws,
      bg_build_command_result_message(
        command_id = bg_new_id("cmd"),
        ok = FALSE,
        error = list(
          code = "invalid_command",
          message = conditionMessage(parsed)
        )
      )
    )
    return(invisible(FALSE))
  }

  if (!identical(parsed$protocol_version, state$protocol_version)) {
    bg_serve_send(
      ws,
      bg_build_command_result_message(
        command_id = parsed$command_id,
        ok = FALSE,
        error = list(
          code = "version_mismatch",
          message = sprintf(
            "Protocol version %s is not supported; expected %s.",
            parsed$protocol_version,
            state$protocol_version
          ),
          expected_protocol_version = state$protocol_version
        )
      )
    )
    return(invisible(FALSE))
  }

  bg_schedule_remote_command(state, ws, parsed)
  invisible(TRUE)
}

#' @keywords internal
bg_schedule_server_poll <- function(state) {
  later::later(
    function() {
      if (isTRUE(state$stopped)) {
        return(invisible(FALSE))
      }

      current <- bg_serve_state_signature(state$project)
      if (!identical(current$signature, state$signature$signature)) {
        state$signature <- current
        bg_serve_broadcast(
          state,
          bg_build_protocol_event_message(
            project = state$project,
            event_kind = "state_changed",
            source = "poll"
          )
        )
      }

      bg_schedule_server_poll(state)
      invisible(TRUE)
    },
    delay = state$poll_interval
  )
}

#' @keywords internal
bg_stop_server_state <- function(state) {
  if (is.null(state) || isTRUE(state$stopped)) {
    return(invisible(TRUE))
  }

  state$stopped <- TRUE

  if (!is.null(state$server)) {
    tryCatch(
      httpuv::stopServer(state$server),
      error = function(...) {
        invisible(NULL)
      }
    )
  }

  state$clients <- list()
  invisible(TRUE)
}

#' @keywords internal
bg_start_server_binding <- function(host, port, app) {
  candidates <- if (is.null(port)) {
    sample(seq.int(32000L, 42000L), size = 50L)
  } else {
    as.integer(port)
  }

  last_error <- NULL

  for (candidate in candidates) {
    server <- tryCatch(
      httpuv::startServer(host, candidate, app = app),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(server)) {
      return(list(server = server, port = as.integer(candidate)))
    }
  }

  cli::cli_abort(
    "Unable to bind bg_serve() on {.val {host}}: {conditionMessage(last_error)}"
  )
}
