# Persistence primitives: timestamps, storage paths, atomic JSON, JSONL
# appends with seq stamping, file locks, and artifact-index IO +
# normalization. Pure moves from workflow-context.R (M9 item 1).

# --- Timestamps and Storage Paths ---

#' @keywords internal
bg_now_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' @keywords internal
bg_storage_path <- function(project, ...) {
  file.path(project@path, ".bayesgrove", ...)
}

#' @keywords internal
bg_sort_persisted_value <- function(x) {
  if (is.null(x) || is.atomic(x)) {
    return(x)
  }

  if (is.list(x)) {
    if (!is.null(names(x))) {
      nm <- names(x)
      ord <- order(nm)
      x <- x[ord]
      names(x) <- nm[ord]
    }

    return(lapply(x, bg_sort_persisted_value))
  }

  x
}

# --- JSON IO Primitives ---

#' @keywords internal
bg_write_json_atomic <- function(path, data, sort_keys = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  payload <- if (isTRUE(sort_keys)) bg_sort_persisted_value(data) else data

  jsonlite::write_json(
    payload,
    tmp,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    force = TRUE
  )

  file.rename(tmp, path)
  invisible(TRUE)
}

#' @keywords internal
bg_with_file_lock <- function(lock_path, code, timeout = 10, poll = 0.05) {
  dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)
  acquired <- FALSE
  start <- Sys.time()

  while (!acquired) {
    acquired <- dir.create(lock_path, recursive = FALSE, showWarnings = FALSE)
    if (acquired) {
      break
    }

    if (as.numeric(Sys.time() - start, units = "secs") >= timeout) {
      cli::cli_abort(
        "Timed out waiting for project lock at {.path {lock_path}}."
      )
    }

    Sys.sleep(poll)
  }

  on.exit(unlink(lock_path, recursive = TRUE, force = TRUE), add = TRUE)
  eval.parent(substitute(code))
}

#' @keywords internal
bg_append_jsonl <- function(path, record, known_line_count = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock_path <- paste0(path, ".lock")

  bg_with_file_lock(lock_path, {
    # Stamp a monotonic sequence number: current line count + 1. This gives
    # every record a total order independent of clock resolution, so two
    # records written within the same second are still distinguishable.
    # The caller may pass a known line count (e.g. from a jobs cache) to skip
    # the O(lines) recount; under the lock the file is authoritative, so when
    # a count is supplied we trust it, otherwise count from disk.
    # readLines, not count.fields: JSON escapes quotes as \", which
    # field-based counting misparses (an unbalanced-looking quote merges
    # lines and undercounts, corrupting the seq order).
    existing_lines <- if (!is.null(known_line_count)) {
      known_line_count
    } else if (file.exists(path)) {
      length(readLines(path, warn = FALSE))
    } else {
      0L
    }
    record$seq <- existing_lines + 1L

    json_line <- jsonlite::toJSON(
      bg_sort_persisted_value(record),
      auto_unbox = TRUE,
      null = "null"
    )
    cat(paste0(json_line, "\n"), file = path, append = TRUE)
    invisible(record$seq)
  })
}

#' Select the index of the latest record by monotonic `seq`, falling back to
#' `created_at` string comparison for legacy records that lack `seq`.
#'
#' @param records A list of record lists.
#' @param timestamp_field The field to read the tiebreak timestamp from
#'   (default `"created_at"`); falls back to `updated_at` when missing.
#' @return An integer index into `records`, or `integer(0)` if empty.
#' @keywords internal
#' @noRd
bg_latest_index_by_seq <- function(
  records,
  timestamp_field = "created_at"
) {
  if (length(records) == 0) {
    return(integer(0))
  }

  seqs <- vapply(records, function(r) r$seq %||% NA_integer_, integer(1))
  timestamps <- vapply(
    records,
    function(r) {
      r[[timestamp_field]] %||% r$updated_at %||% ""
    },
    character(1)
  )

  has_seq <- !is.na(seqs)
  if (all(has_seq)) {
    return(which.max(seqs))
  }
  if (any(has_seq)) {
    return(which(seqs == max(seqs[has_seq], na.rm = TRUE))[[1]])
  }

  order(timestamps, decreasing = TRUE, na.last = TRUE)[[1]]
}

# --- Artifact Index IO ---

#' @keywords internal
bg_artifact_index_path <- function(project) {
  file.path(project@path, ".bayesgrove", "cache", "index.json")
}

#' @keywords internal
bg_modify_artifact_index <- function(project, code, timeout = 10, poll = 0.05) {
  bg_with_file_lock(
    paste0(bg_artifact_index_path(project), ".lock"),
    code,
    timeout = timeout,
    poll = poll
  )
}

# --- Artifact Normalization ---

#' @keywords internal
bg_normalize_artifact_entry <- function(fingerprint, entry) {
  entry <- entry %||% list()
  bindings <- entry$bindings %||% list()

  if (length(bindings) == 0 && !is.null(entry$node_id)) {
    bindings[[entry$node_id]] <- list(
      node_id = entry$node_id,
      status = entry$status %||% "active",
      updated_at = entry$updated_at %||% entry$created_at %||% NULL
    )
  }

  if (length(bindings) > 0) {
    for (node_id in names(bindings)) {
      bindings[[node_id]] <- utils::modifyList(
        list(
          node_id = node_id,
          status = "active",
          updated_at = entry$updated_at %||% entry$created_at %||% NULL
        ),
        bindings[[node_id]]
      )
    }
  }

  active_bindings <- names(Filter(
    function(x) identical(x$status, "active"),
    bindings
  ))
  first_active <- if (length(active_bindings) > 0) {
    active_bindings[[1]]
  } else {
    NULL
  }
  first_binding <- if (length(bindings) > 0) names(bindings)[[1]] else NULL
  entry$status <- entry$status %||%
    if (length(active_bindings) > 0) "active" else "superseded"
  entry$node_id <- entry$node_id %||% first_active %||% first_binding %||% NULL
  entry$artifact_ref <- entry$artifact_ref %||% NULL
  entry$created_at <- entry$created_at %||% NULL
  entry$updated_at <- entry$updated_at %||% entry$created_at %||% NULL
  entry$bindings <- bindings
  entry$fingerprint <- fingerprint
  entry
}

#' @keywords internal
bg_normalize_artifact_index <- function(index) {
  index <- index %||% list()
  if (
    identical(index$schema_name %||% NULL, "bg_artifact_index") &&
      !is.null(index$entries)
  ) {
    entries <- index$entries %||% list()
    normalized <- list()

    for (node_id in names(entries)) {
      node_entries <- entries[[node_id]] %||% list()
      for (fingerprint in names(node_entries)) {
        leaf <- node_entries[[fingerprint]] %||% list()
        entry <- normalized[[fingerprint]] %||%
          list(
            artifact_ref = leaf$artifact_ref %||% NULL,
            created_at = leaf$created_at %||% NULL,
            updated_at = leaf$updated_at %||% leaf$created_at %||% NULL,
            metadata = leaf$metadata %||% list(),
            bindings = list()
          )
        entry$artifact_ref <- entry$artifact_ref %||%
          leaf$artifact_ref %||%
          NULL
        entry$created_at <- entry$created_at %||% leaf$created_at %||% NULL
        entry$updated_at <- leaf$updated_at %||%
          leaf$created_at %||%
          entry$updated_at %||%
          NULL
        entry$metadata <- utils::modifyList(
          entry$metadata %||% list(),
          leaf$metadata %||% list()
        )
        entry$bindings[[node_id]] <- list(
          node_id = node_id,
          status = leaf$status %||% "active",
          updated_at = leaf$updated_at %||% leaf$created_at %||% NULL
        )
        normalized[[fingerprint]] <- entry
      }
    }

    index <- normalized
  }

  stats::setNames(
    lapply(names(index), function(fingerprint) {
      bg_normalize_artifact_entry(fingerprint, index[[fingerprint]])
    }),
    names(index)
  )
}

#' @keywords internal
bg_write_artifact_index <- function(project, index) {
  entries <- list()

  for (fingerprint in names(index)) {
    entry <- index[[fingerprint]]
    bindings <- entry$bindings %||% list()
    for (node_id in names(bindings)) {
      binding <- bindings[[node_id]]
      entries[[node_id]] <- entries[[node_id]] %||% list()
      entries[[node_id]][[fingerprint]] <- list(
        artifact_ref = entry$artifact_ref %||% NULL,
        status = binding$status %||% "active",
        created_at = entry$created_at %||% NULL,
        updated_at = binding$updated_at %||% entry$updated_at %||% NULL,
        metadata = entry$metadata %||% list()
      )
    }
  }

  persisted <- list(
    schema_name = "bg_artifact_index",
    schema_version = 1L,
    project_id = project@project_id,
    entries = entries
  )

  bg_write_json_atomic(
    bg_storage_path(project, "cache", "index.json"),
    persisted
  )
}

#' @keywords internal
bg_artifact_binding <- function(index, fingerprint, node_id) {
  entry <- index[[fingerprint]]
  if (is.null(entry)) {
    return(NULL)
  }
  entry$bindings[[node_id]] %||% NULL
}

#' @keywords internal
bg_any_active_artifact_binding <- function(entry) {
  any(vapply(
    entry$bindings %||% list(),
    function(binding) identical(binding$status, "active"),
    logical(1)
  ))
}

#' @keywords internal
bg_refresh_artifact_entry <- function(entry) {
  active_nodes <- names(Filter(
    function(binding) identical(binding$status, "active"),
    entry$bindings %||% list()
  ))
  first_active <- if (length(active_nodes) > 0) active_nodes[[1]] else NULL
  first_binding <- if (length(entry$bindings) > 0) {
    names(entry$bindings)[[1]]
  } else {
    NULL
  }

  entry$status <- if (length(active_nodes) > 0) "active" else "superseded"
  entry$node_id <- first_active %||% first_binding %||% entry$node_id %||% NULL
  entry$updated_at <- bg_now_timestamp()
  entry
}

#' @keywords internal
bg_supersede_artifacts_for_node <- function(
  index,
  node_id,
  except_fingerprint = NULL
) {
  changed <- FALSE
  count <- 0L

  for (fingerprint in names(index)) {
    if (
      !is.null(except_fingerprint) && identical(fingerprint, except_fingerprint)
    ) {
      next
    }

    binding <- index[[fingerprint]]$bindings[[node_id]] %||% NULL
    if (!is.null(binding) && identical(binding$status, "active")) {
      index[[fingerprint]]$bindings[[node_id]]$status <- "superseded"
      index[[fingerprint]]$bindings[[node_id]]$updated_at <- bg_now_timestamp()
      index[[fingerprint]] <- bg_refresh_artifact_entry(index[[fingerprint]])
      changed <- TRUE
      count <- count + 1L
    }
  }

  list(index = index, changed = changed, count = count)
}
