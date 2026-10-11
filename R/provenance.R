# Execution provenance ------------------------------------------------------
#
# Every successful execution records what it consumed and ran in its final
# job record, as `provenance`. The main process is the single writer: the
# sequential path records it in bg_execute_node(), the parallel path after a
# worker returns its executor metadata and environment manifest. Identity is
# carried by content addresses (artifact refs and file hashes), never by the
# current graph. See architecture/rules-and-evidence.md, section 2.

# --- P/A/D declarations ----------------------------------------------------

#' The parameter categories of a node-kind `pad` declaration.
#' @keywords internal
#' @noRd
bg_pad_categories <- function() {
  c("P", "A", "D", "locator", "other")
}

#' Validate and normalize a node-kind `pad` declaration.
#'
#' Accepts a named list whose names are a subset of `bg_pad_categories()` and
#' whose values are character vectors of parameter names (or, as read back
#' from JSON, lists of strings). Returns every category as a character vector
#' so callers never special-case missing categories. A parameter may appear
#' in only one category. `NULL` means the kind declares no classification.
#' @keywords internal
#' @noRd
bg_normalize_pad_declaration <- function(pad) {
  if (is.null(pad)) {
    return(NULL)
  }

  categories <- bg_pad_categories()
  if (!is.list(pad) || (length(pad) > 0L && is.null(names(pad)))) {
    cli::cli_abort(
      "{.arg pad} must be a named list with entries among {.val {categories}}."
    )
  }
  unknown <- setdiff(names(pad), categories)
  if (length(unknown) > 0L || anyDuplicated(names(pad)) > 0L) {
    cli::cli_abort(c(
      "{.arg pad} must name each category at most once.",
      "x" = if (length(unknown) > 0L) {
        "Unknown categor{?y/ies}: {.val {unknown}}."
      },
      "i" = "Valid categories are {.val {categories}}."
    ))
  }

  normalized <- lapply(categories, function(category) {
    values <- pad[[category]] %||% character()
    if (is.list(values)) {
      scalar_strings <- vapply(
        values,
        function(value) is.character(value) && length(value) == 1L,
        logical(1)
      )
      if (!all(scalar_strings)) {
        cli::cli_abort(
          "{.arg pad${category}} must be a character vector of parameter names."
        )
      }
      values <- as.character(unlist(values, use.names = FALSE))
    }
    if (!is.character(values) || anyNA(values) || !all(nzchar(values))) {
      cli::cli_abort(
        "{.arg pad${category}} must be a character vector of parameter names."
      )
    }
    unique(values)
  })
  names(normalized) <- categories

  classified <- unlist(normalized, use.names = FALSE)
  repeated <- unique(classified[duplicated(classified)])
  if (length(repeated) > 0L) {
    cli::cli_abort(
      "Parameter{?s} {.val {repeated}} appear{?s/} in more than one {.arg pad} category."
    )
  }

  normalized
}

#' Persistable form of a normalized `pad` declaration.
#'
#' Categories are stored as JSON arrays (lists) so a one-element category does
#' not unbox to a bare string in the runtime manifest.
#' @keywords internal
#' @noRd
bg_pad_declaration_manifest <- function(pad) {
  if (is.null(pad)) {
    return(NULL)
  }
  lapply(pad, as.list)
}

#' Built-in P/A/D declarations for the fit kinds.
#'
#' Parameters an executor ignores are deliberately unclassified, so a node
#' carrying them records no `pad`. Settings an executor hard-codes appear in
#' the execution's `effective` settings instead.
#' @keywords internal
#' @noRd
bg_builtin_pad_declarations <- function() {
  list(
    cmdstanr_fit = list(
      P = "stan_file",
      A = c(
        "chains",
        "iter_warmup",
        "iter_sampling",
        "adapt_delta",
        "max_treedepth",
        "seed"
      ),
      locator = "data_input",
      other = "thresholds"
    ),
    prior_fit = list(
      P = "stan_file",
      A = c("iter_warmup", "iter_sampling", "seed"),
      locator = "data_input"
    ),
    brms_fit = list(
      P = c("formula", "family", "priors"),
      A = c(
        "chains",
        "iter_warmup",
        "iter_sampling",
        "adapt_delta",
        "max_treedepth",
        "seed",
        "backend",
        "algorithm"
      ),
      locator = "data_input",
      other = "thresholds"
    ),
    brms_prior_fit = list(
      P = "formula",
      A = c(
        "chains",
        "iter_warmup",
        "iter_sampling",
        "seed",
        "backend",
        "algorithm"
      ),
      locator = "data_input"
    )
  )
}

# --- Executor reports ------------------------------------------------------

#' An empty named list, which serializes as a JSON object (`{}`).
#' @keywords internal
#' @noRd
bg_empty_named_list <- function() {
  stats::setNames(list(), character())
}

#' Coerce a value to a named list, or an empty named list when it is not one.
#' @keywords internal
#' @noRd
bg_as_named_list <- function(x) {
  if (!is.list(x) || length(x) == 0L || is.null(names(x))) {
    return(bg_empty_named_list())
  }
  if (!all(nzchar(names(x)))) {
    return(bg_empty_named_list())
  }
  x
}

#' Pass a value through canonical JSON and back.
#'
#' Provenance is persisted as JSON and digests must be recomputable from the
#' persisted record, so it is normalized to its JSON-read shape before any
#' digest is taken. This also turns a non-serializable report into an error
#' at capture time instead of at the job-log write.
#' @keywords internal
#' @noRd
bg_json_roundtrip <- function(x) {
  jsonlite::fromJSON(bg_canonical_json(x), simplifyVector = FALSE)
}

#' Build an executor's `metadata$provenance` report.
#'
#' Built-in executors use this so every report has the same shape. Any field
#' left `NULL` falls back to the engine default (see
#' `bg_normalize_executor_report()`).
#' @param D Character vector of consumed data input ids.
#' @param subject Input id of the fit a check evaluated.
#' @param data_files List of `list(node_id, path)` entries naming the
#'   file-path inputs the executor read. The engine supplies their hashes.
#' @param effective `list(P, A)` of settings read back from the result.
#' @keywords internal
#' @noRd
bg_executor_report <- function(
  D = NULL,
  subject = NULL,
  data_files = NULL,
  effective = NULL
) {
  consumed <- list()
  if (!is.null(D)) {
    consumed$D <- as.character(D)
  }
  if (!is.null(subject)) {
    consumed$subject <- subject
  }
  if (!is.null(data_files)) {
    consumed$data_files <- data_files
  }
  report <- list(consumed = consumed)
  if (!is.null(effective)) {
    report$effective <- effective
  }
  list(provenance = report)
}

#' Whether a value is a single path naming an existing regular file.
#' @keywords internal
#' @noRd
bg_is_file_path <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value) &&
    file.exists(value) &&
    !dir.exists(value)
}

#' Report a selected data input that is a file path.
#'
#' Names the input and path only. The engine hashes every file-path input in
#' the executing process before the executor runs, and supplies those hashes,
#' so a report never certifies bytes read after execution. Returns an empty
#' list when the data is not a single existing file path.
#' @keywords internal
#' @noRd
bg_data_file_report <- function(input_id, data) {
  if (!bg_is_file_path(data)) {
    return(list())
  }
  list(list(
    node_id = input_id,
    path = normalizePath(data, mustWork = FALSE)
  ))
}

# --- File identities -------------------------------------------------------
#
# File-backed data is supported only as a regular file hashed before the run
# at the identical path, resolved in the executing process. That process
# (bg_execute_node() sequentially, the worker in parallel) snapshots, before
# and after the executor runs, the Stan source closure of the node's
# `stan_file` param and every input whose value is a character scalar, and
# normalizes the paths the executor reports. The main process then compares
# normalized absolute paths only (see `bg_reconcile_data_files()`).

#' SHA-256 of a file, or `NA` when it cannot be read.
#' @keywords internal
#' @noRd
bg_hash_file_or_na <- function(path) {
  tryCatch(
    digest::digest(file = path, algo = "sha256"),
    error = function(e) NA_character_
  )
}

#' Snapshot one input value as a filesystem reference.
#'
#' @return `list(path, kind, sha256)`: `path` normalized in this process;
#'   `kind` one of `"file"`, `"directory"`, `"missing"`; `sha256` the content
#'   hash of a regular file, or `NA` (also for an unreadable file).
#' @keywords internal
#' @noRd
bg_snapshot_path <- function(value) {
  if (!nzchar(value)) {
    return(list(path = value, kind = "missing", sha256 = NA_character_))
  }
  path <- normalizePath(value, mustWork = FALSE)
  kind <- if (!file.exists(path)) {
    "missing"
  } else if (dir.exists(path)) {
    "directory"
  } else {
    "file"
  }
  list(
    path = path,
    kind = kind,
    sha256 = if (identical(kind, "file")) {
      bg_hash_file_or_na(path)
    } else {
      NA_character_
    }
  )
}

#' Snapshot the files an execution may read.
#'
#' @param node The node as passed to the executor (resolved params).
#' @param inputs The executor inputs, named by upstream node id.
#' @param project_path Project root for resolving a relative `stan_file`.
#' @return `list(stan_file = list(path, sha256) or NULL, data_files = named
#'   list of list(node_id, path, kind, sha256))` with one `data_files` entry
#'   per input whose value is a character scalar.
#' @keywords internal
#' @noRd
bg_execution_file_identities <- function(node, inputs, project_path = NULL) {
  stan_file <- node$params$stan_file %||% NULL
  stan <- NULL
  if (is.character(stan_file) && length(stan_file) == 1L) {
    stan <- list(
      path = stan_file,
      sha256 = tryCatch(
        bg_source_hash_component(node, project_path),
        error = function(e) NA_character_
      )
    )
  }

  data_files <- list()
  for (id in names(inputs) %||% character()) {
    value <- inputs[[id]]
    if (
      nzchar(id) &&
        is.character(value) &&
        length(value) == 1L &&
        !is.na(value)
    ) {
      data_files[[id]] <- c(list(node_id = id), bg_snapshot_path(value))
    }
  }

  list(stan_file = stan, data_files = data_files)
}

#' Normalize the paths an executor reported, in the executing process.
#'
#' Relative paths resolve against this process's working directory, which
#' is the one the executor read them from. The main process never resolves
#' a reported path itself.
#' @param result A normalized execution result (`artifact`, `summaries`,
#'   `metadata`), or anything else, which is returned unchanged.
#' @keywords internal
#' @noRd
bg_localize_report_paths <- function(result) {
  files <- if (is.list(result) && is.list(result$metadata)) {
    tryCatch(
      result$metadata$provenance$consumed$data_files,
      error = function(e) NULL
    )
  }
  if (!is.list(files) || length(files) == 0L) {
    return(result)
  }
  result$metadata$provenance$consumed$data_files <- lapply(
    files,
    function(entry) {
      path <- if (is.list(entry)) entry$path else NULL
      if (is.character(path) && length(path) == 1L && !is.na(path)) {
        entry$path <- normalizePath(path, mustWork = FALSE)
      }
      entry
    }
  )
  result
}

#' Run an executor between two file-identity snapshots.
#'
#' Snapshots before invocation, calls `run()`, normalizes the paths the
#' executor reported, and snapshots again. Never fails on hashing. Each
#' snapshotted file carries `changed`: `TRUE` unless it was a readable
#' regular file before the run with the same content afterwards. Whether a
#' change matters is decided once the executor's report is known.
#' @param run A zero-argument function invoking the executor.
#' @return `list(value, files = list(stan_file, data_files, error))`;
#'   `error` is non-`NULL` when the snapshot itself failed.
#' @keywords internal
#' @noRd
bg_with_file_identities <- function(node, inputs, project_path, run) {
  snapshot <- function() {
    tryCatch(
      bg_execution_file_identities(node, inputs, project_path),
      error = function(e) list(error = conditionMessage(e))
    )
  }
  before <- snapshot()
  # Force the executor before localizing: an executor error must propagate
  # once, not through a lazily evaluated argument.
  value <- run()
  value <- bg_localize_report_paths(value)
  after <- snapshot()

  differs <- function(before_hash, after_hash) {
    is.na(before_hash) ||
      !identical(before_hash, after_hash %||% NA_character_)
  }

  stan <- before$stan_file
  if (!is.null(stan)) {
    stan$changed <- differs(stan$sha256, after$stan_file$sha256)
  }
  data_files <- before$data_files %||% list()
  for (id in names(data_files)) {
    data_files[[id]]$changed <- differs(
      data_files[[id]]$sha256,
      after$data_files[[id]]$sha256
    )
  }

  list(
    value = value,
    files = list(
      stan_file = stan,
      data_files = data_files,
      error = before$error %||% after$error
    )
  )
}

#' Normalize an executor's provenance report against the execution inputs.
#'
#' Applies the defaults for anything not reported: `consumed$D` is every
#' input, `consumed$subject` is `NULL`, and `effective` is empty. Reported ids
#' that are not inputs of this execution are dropped, so a report can never
#' claim consumption of something the engine did not pass in.
#' @param metadata The executor's normalized `metadata`.
#' @param input_ids Input node ids in executor order.
#' @keywords internal
#' @noRd
bg_normalize_executor_report <- function(metadata, input_ids) {
  report <- if (is.list(metadata)) metadata$provenance else NULL
  if (!is.list(report)) {
    report <- list()
  }
  consumed <- if (is.list(report$consumed)) report$consumed else list()
  effective <- if (is.list(report$effective)) report$effective else list()

  reported_d <- consumed$D
  d_ids <- if (is.null(reported_d)) {
    input_ids
  } else {
    ids <- unlist(reported_d, use.names = FALSE)
    ids <- if (is.character(ids)) ids else character()
    ids[ids %in% input_ids]
  }

  subject <- consumed$subject
  if (
    !(is.character(subject) &&
      length(subject) == 1L &&
      subject %in% input_ids)
  ) {
    subject <- NULL
  }

  is_scalar_string <- function(x) is.character(x) && length(x) == 1L
  data_files <- Filter(
    function(entry) {
      is.list(entry) &&
        (is_scalar_string(entry$node_id) || is_scalar_string(entry$path))
    },
    if (is.list(consumed$data_files)) consumed$data_files else list()
  )
  # Executors only name the files they read; hashes come from the engine's
  # pre-invocation snapshot (see bg_resolve_data_files()).
  data_files <- lapply(data_files, function(entry) {
    list(
      node_id = if (is_scalar_string(entry$node_id)) entry$node_id else NULL,
      path = if (is_scalar_string(entry$path)) entry$path else NULL
    )
  })

  list(
    consumed = list(
      D = as.list(unique(d_ids)),
      subject = subject,
      data_files = data_files
    ),
    effective = list(
      P = bg_as_named_list(effective$P),
      A = bg_as_named_list(effective$A)
    )
  )
}

# --- P/A/D snapshots -------------------------------------------------------

#' Identity fields hashed into each P/A/D component digest.
#'
#' Everything else stored on a component (locators, producer node ids, file
#' paths) is recorded for provenance but excluded from the digest.
#' @keywords internal
#' @noRd
bg_pad_identity_fields <- function() {
  list(
    P = c("kind", "params", "stan_source", "effective"),
    A = c("executor", "params", "effective", "environment"),
    D = c("params", "data")
  )
}

#' Digest the identity fields of one P/A/D component.
#' @keywords internal
#' @noRd
bg_pad_component_digest <- function(component, category) {
  fields <- bg_pad_identity_fields()[[category]]
  identity <- stats::setNames(
    lapply(fields, function(field) component[[field]]),
    fields
  )
  bg_canonical_digest(identity)
}

#' Identify the executor that ran a node, for the A component.
#'
#' Built-in executors are identified by reference (their version is in the
#' environment manifest); user executors by the same source hash the
#' fingerprint uses.
#' @keywords internal
#' @noRd
bg_executor_identity <- function(project, kind_reg, node) {
  executor_ref <- kind_reg$executor_ref %||% NULL
  if (!is.null(executor_ref) && startsWith(executor_ref, "builtin:")) {
    return(executor_ref)
  }
  hash <- bg_executor_fingerprint_component(project, node)
  if (!nzchar(hash)) {
    return(NULL)
  }
  paste0("sha256:", hash)
}

#' Compute the P/A/D snapshot of one execution.
#'
#' Returns `NULL` when the kind declares no `pad` or when any parameter on
#' the node is unclassified: such an execution cannot be bound.
#' @param node The graph node as persisted (unresolved params).
#' @param declaration Normalized `pad` declaration of the node's kind.
#' @param provenance The execution provenance so far (JSON-normalized).
#' @param executor Executor identity string.
#' @param stan_source Pre-invocation hash of the `stan_file` source closure,
#'   from the executing process.
#' @keywords internal
#' @noRd
bg_pad_snapshot <- function(
  node,
  declaration,
  provenance,
  executor,
  stan_source = NULL
) {
  if (is.null(declaration)) {
    return(NULL)
  }

  params <- node$params %||% list()
  param_names <- names(params) %||% character()
  classified <- unlist(declaration, use.names = FALSE)
  if (length(setdiff(param_names, classified)) > 0L) {
    return(NULL)
  }

  pick <- function(category) {
    keep <- intersect(param_names, declaration[[category]])
    if (length(keep) == 0L) {
      return(bg_empty_named_list())
    }
    params[keep]
  }

  # A Stan program is identified by the contents of its source closure, as
  # hashed before the executor ran. The path only locates it, so bundling and
  # relocation keep the same identity.
  p_params <- pick("P")
  p_locator <- bg_empty_named_list()
  if (!"stan_file" %in% names(p_params)) {
    stan_source <- NULL
  } else {
    p_locator$stan_file <- p_params$stan_file
    p_params <- p_params[setdiff(names(p_params), "stan_file")]
    if (length(p_params) == 0L) {
      p_params <- bg_empty_named_list()
    }
  }

  consumed <- provenance$consumed %||% list()
  d_ids <- unlist(consumed$D, use.names = FALSE) %||% character()
  input_refs <- list()
  for (input in provenance$inputs %||% list()) {
    input_refs[[input$node_id]] <- input$artifact_ref
  }
  file_hashes <- list()
  for (entry in consumed$data_files %||% list()) {
    if (is.character(entry$node_id) && length(entry$node_id) == 1L) {
      file_hashes[[entry$node_id]] <- entry$sha256
    }
  }
  # One data identity per consumed input, in consumed order: the file content
  # hash when the input was a data file, otherwise the consumed artifact ref
  # (the content address of the realized data). Producer node ids are recorded
  # alongside but are not identity, so identical data from different producer
  # nodes has the same D.
  data_identities <- lapply(d_ids, function(id) {
    if (!is.null(file_hashes[[id]])) {
      paste0("sha256:", file_hashes[[id]])
    } else {
      input_refs[[id]] %||% NULL
    }
  })

  effective <- provenance$effective %||% list()
  pad <- list(
    P = list(
      kind = node$kind,
      params = p_params,
      stan_source = stan_source,
      effective = effective$P %||% bg_empty_named_list(),
      locator = p_locator
    ),
    A = list(
      executor = executor,
      params = pick("A"),
      effective = effective$A %||% bg_empty_named_list(),
      environment = provenance$environment %||% bg_empty_named_list()
    ),
    D = list(
      params = pick("D"),
      data = data_identities,
      producers = as.list(d_ids),
      locator = pick("locator")
    ),
    other = pick("other")
  )
  pad <- bg_json_roundtrip(pad)

  for (category in c("P", "A", "D")) {
    pad[[category]]$digest <- bg_pad_component_digest(pad[[category]], category)
  }
  pad
}

# --- Provenance capture ----------------------------------------------------

#' Reconcile consumed files with the engine's pre-run file identities.
#'
#' File-backed data is supported only as a regular file hashed before the
#' run at the identical path, resolved in the executing process. Paths here
#' are already normalized there, so they are compared as strings. Fails
#' closed: any violation is an error, which leaves the execution without a
#' pad.
#'
#' - Every reported `data_files` entry must name an input in consumed D
#'   (by `node_id`, or by a `path` equal to that input's snapshot path), and
#'   a reported `path` must equal the snapshot path.
#' - Every consumed D input whose value is a character scalar is a
#'   filesystem reference: it must have been a readable regular file before
#'   the run, unchanged afterwards. Its pre-run hash is its data identity;
#'   the CAS ref of the path string is never used.
#' - Inputs outside consumed D are never checked.
#' @param reported `consumed$data_files` entries from the normalized report.
#' @param snapshot Named list of `list(node_id, path, kind, sha256, changed)`
#'   by input id, one per character-scalar input.
#' @param d_ids Consumed D input ids, in consumed order.
#' @return `list(data_files, errors)`: `data_files` are
#'   `list(node_id, path, sha256)` entries, one per file-backed D input;
#'   `errors` is a character vector.
#' @keywords internal
#' @noRd
bg_reconcile_data_files <- function(reported, snapshot, d_ids = character()) {
  snapshot <- snapshot %||% list()
  errors <- character()

  for (entry in reported %||% list()) {
    id <- entry$node_id %||% NULL
    path <- entry$path %||% NULL
    if (is.null(id)) {
      in_d <- Filter(
        function(file) identical(file$path, path),
        snapshot[intersect(d_ids, names(snapshot))]
      )
      id <- if (length(in_d) > 0L) names(in_d)[[1]] else NULL
    }
    if (is.null(id) || !id %in% d_ids) {
      errors <- c(
        errors,
        sprintf(
          "reported data file %s is not a consumed D input",
          path %||% id %||% "<unnamed>"
        )
      )
      next
    }
    file <- snapshot[[id]] %||% NULL
    if (is.null(file)) {
      errors <- c(
        errors,
        sprintf("input %s is reported as a data file but is not a path", id)
      )
      next
    }
    if (!is.null(path) && !identical(path, file$path)) {
      errors <- c(
        errors,
        sprintf(
          "reported file %s does not match input %s (%s)",
          path,
          id,
          file$path
        )
      )
    }
  }

  data_files <- list()
  for (id in intersect(d_ids, names(snapshot))) {
    file <- snapshot[[id]]
    if (!identical(file$kind, "file") || is.na(file$sha256)) {
      errors <- c(
        errors,
        sprintf(
          "input %s (%s) was not a readable regular file before the run (%s)",
          id,
          file$path,
          if (identical(file$kind, "file")) "unreadable" else file$kind
        )
      )
      next
    }
    if (isTRUE(file$changed)) {
      errors <- c(
        errors,
        sprintf(
          "data file %s (input %s) changed during execution",
          file$path,
          id
        )
      )
    }
    data_files[[length(data_files) + 1L]] <- list(
      node_id = id,
      path = file$path,
      sha256 = file$sha256
    )
  }

  list(data_files = data_files, errors = errors)
}

#' Build the provenance record of one successful execution.
#'
#' @param project A `bg_handle` (main process).
#' @param node The graph node as persisted.
#' @param kind_reg The node-kind runtime registry entry.
#' @param fingerprint The execution fingerprint.
#' @param artifact_ref The execution's own output ref.
#' @param input_bindings The run-plan input bindings used to assemble the
#'   executor's inputs, in executor order.
#' @param metadata The executor's normalized `metadata`.
#' @param environment Environment manifest of the process that ran the node.
#' @param files File identities captured around the executor in the process
#'   that ran it (see `bg_with_file_identities()`).
#' @keywords internal
#' @noRd
bg_build_execution_provenance <- function(
  project,
  node,
  kind_reg,
  fingerprint,
  artifact_ref,
  input_bindings,
  metadata,
  environment,
  files = NULL
) {
  inputs <- lapply(input_bindings %||% list(), function(binding) {
    list(node_id = binding$from_node_id, artifact_ref = binding$artifact_ref)
  })
  input_ids <- unique(vapply(inputs, `[[`, character(1), "node_id"))
  report <- bg_normalize_executor_report(metadata, input_ids)
  reconciled <- bg_reconcile_data_files(
    report$consumed$data_files,
    files$data_files,
    d_ids = unlist(report$consumed$D, use.names = FALSE) %||% character()
  )
  report$consumed$data_files <- reconciled$data_files

  provenance <- bg_json_roundtrip(list(
    artifact_ref = artifact_ref,
    fingerprint = fingerprint,
    inputs = inputs,
    consumed = report$consumed,
    effective = report$effective,
    environment = bg_as_named_list(environment)
  ))

  # File identities must come from before the executor ran, for exactly the
  # files it consumed. Otherwise the record stays but carries no pad, so it
  # cannot be bound.
  errors <- reconciled$errors
  if (is.null(files)) {
    errors <- c("file identities were not captured for this execution", errors)
  } else {
    if (!is.null(files$error)) {
      errors <- c(
        sprintf("file identities could not be captured (%s)", files$error),
        errors
      )
    }
    stan <- files$stan_file
    if (!is.null(stan) && (isTRUE(stan$changed) || is.na(stan$sha256))) {
      errors <- c(
        sprintf(
          "stan_file %s (source closure) changed during execution",
          stan$path
        ),
        errors
      )
    }
  }
  if (length(errors) > 0L) {
    provenance["pad"] <- list(NULL)
    provenance$capture_error <- paste(errors, collapse = "; ")
    return(provenance)
  }

  provenance["pad"] <- list(bg_pad_snapshot(
    node = node,
    declaration = bg_normalize_pad_declaration(kind_reg$pad %||% NULL),
    provenance = provenance,
    executor = bg_executor_identity(project, kind_reg, node),
    stan_source = files$stan_file$sha256 %||% NULL
  ))
  provenance
}

#' Capture execution provenance without ever failing the run.
#'
#' Read-back and snapshot code is defensive, but provenance is a record, not
#' a precondition: any unexpected error warns and records no provenance. The
#' error message is returned so the job record can keep it durably.
#' @return `list(provenance, error)`; exactly one is non-`NULL`.
#' @keywords internal
#' @noRd
bg_capture_execution_provenance <- function(project, node_id, graph, ...) {
  node <- graph$nodes[[node_id]]
  tryCatch(
    list(
      provenance = bg_build_execution_provenance(project, node, ...),
      error = NULL
    ),
    error = function(e) {
      cli::cli_warn(c(
        "Could not record execution provenance for node {.val {node_id}}.",
        "x" = conditionMessage(e)
      ))
      list(provenance = NULL, error = conditionMessage(e))
    }
  )
}

#' The environment manifest of the current process, never failing.
#' @keywords internal
#' @noRd
bg_execution_environment <- function() {
  tryCatch(
    bg_default_environment_manifest(),
    error = function(e) list(r = as.character(getRversion()))
  )
}

#' Record a successful execution: summaries, provenance, and the final job.
#'
#' Shared by the sequential path and the main-process finalization of the
#' parallel path, so both write the same records. Summaries are stamped with
#' the execution's `job_id`. When provenance capture fails, the succeeded job
#' carries `capture_error` instead of `provenance`, which distinguishes it
#' from executions recorded before provenance existed.
#' @return The persisted summaries.
#' @keywords internal
#' @noRd
bg_record_execution <- function(
  project,
  node_id,
  graph,
  kind_reg,
  fingerprint,
  artifact_ref,
  input_bindings,
  job_id,
  summaries,
  metadata,
  environment,
  files
) {
  written <- bg_persist_summaries(
    project = project,
    node_id = node_id,
    artifact_ref = artifact_ref,
    execution_fingerprint = fingerprint,
    summaries = summaries,
    job_id = job_id
  )

  captured <- bg_capture_execution_provenance(
    project,
    node_id,
    graph,
    kind_reg = kind_reg,
    fingerprint = fingerprint,
    artifact_ref = artifact_ref,
    input_bindings = input_bindings,
    metadata = metadata,
    environment = environment,
    files = files
  )

  bg_update_job(
    project,
    job_id,
    status = "succeeded",
    result_ref = artifact_ref,
    finished_at = bg_now_timestamp(),
    provenance = captured$provenance,
    capture_error = captured$error
  )

  written
}

# --- Resolution ------------------------------------------------------------

#' Check that an artifact exists in the CAS with a valid digest.
#' @keywords internal
#' @noRd
bg_cas_blob_is_valid <- function(project, artifact_ref) {
  if (
    !is.character(artifact_ref) ||
      length(artifact_ref) != 1L ||
      !startsWith(artifact_ref, "cas:sha256:")
  ) {
    return(FALSE)
  }
  path <- bg_cas_blob_path(project, artifact_ref)
  if (!file.exists(path)) {
    return(FALSE)
  }
  hash <- sub("^cas:sha256:", "", artifact_ref)
  identical(digest::digest(file = path, algo = "sha256"), hash)
}

#' List the succeeded executions that produced an artifact at a fingerprint.
#'
#' Eligible jobs are succeeded, carry provenance whose `artifact_ref` and
#' `fingerprint` match (and whose `result_ref` is that artifact), and their
#' artifact exists in the CAS with a valid digest. Several executions can
#' produce byte-identical output at one fingerprint, so this returns all of
#' them in log order (by final-record `seq`).
#' @param jobs Optional pre-read jobs list.
#' @return A list of job records, possibly empty.
#' @keywords internal
#' @noRd
bg_execution_jobs <- function(project, artifact_ref, fingerprint, jobs = NULL) {
  jobs <- jobs %||% bg_jobs(project)
  eligible <- Filter(
    function(job) {
      provenance <- job[["provenance"]]
      identical(job$status, "succeeded") &&
        is.list(provenance) &&
        identical(provenance$artifact_ref, artifact_ref) &&
        identical(provenance$fingerprint, fingerprint) &&
        identical(job$result_ref, artifact_ref)
    },
    jobs
  )
  if (length(eligible) == 0L || !bg_cas_blob_is_valid(project, artifact_ref)) {
    return(list())
  }

  seqs <- vapply(
    eligible,
    function(job) as.numeric(job$seq %||% 0),
    numeric(1)
  )
  unname(eligible[order(seqs)])
}

#' Resolve a node to the execution that produced its current artifact.
#'
#' 1. Predict the node's fingerprint.
#' 2. Take the artifact the runner would use for it (`bg_check_artifact()`).
#' 3. Pick a representative producing execution: the most recent eligible
#'    job in log order (see `bg_execution_jobs()`). It may belong to another
#'    node with the same fingerprint, which is truthful because the node's
#'    artifact is that output.
#' @return `list(node_id, fingerprint, artifact_ref, job, jobs)` or `NULL`.
#' @keywords internal
#' @noRd
bg_resolve_node_execution <- function(
  project,
  node_id,
  predicted_fingerprints = NULL,
  artifact_index = NULL,
  jobs = NULL
) {
  predicted_fingerprints <- predicted_fingerprints %||%
    bg_predicted_fingerprints(project)
  fingerprint <- predicted_fingerprints[[node_id]] %||% NULL
  if (is.null(fingerprint)) {
    return(NULL)
  }

  artifact_ref <- bg_check_artifact(
    project,
    fingerprint,
    node_id = node_id,
    artifact_index = artifact_index
  )
  if (is.null(artifact_ref)) {
    return(NULL)
  }

  eligible <- bg_execution_jobs(project, artifact_ref, fingerprint, jobs = jobs)
  if (length(eligible) == 0L) {
    return(NULL)
  }

  list(
    node_id = node_id,
    fingerprint = fingerprint,
    artifact_ref = artifact_ref,
    job = eligible[[length(eligible)]],
    jobs = eligible
  )
}

#' Read the provenance of a node's current artifact
#'
#' Resolves `node_id` to the execution that produced the artifact it currently
#' uses: its predicted fingerprint selects the cached artifact, and the most
#' recent succeeded job that produced that artifact at that fingerprint is the
#' representative execution. The representative can belong to another node
#' that shares the fingerprint (cache reuse), because the node's artifact is
#' that execution's output.
#'
#' The returned provenance records the execution's own output
#' (`artifact_ref`), its `fingerprint`, the `inputs` passed to the executor
#' (in order), what it `consumed` (`D` input ids, the check `subject`, and
#' `data_files` with SHA-256 hashes taken before the executor ran), the
#' `effective` settings read back from the result, the `environment` of the
#' process that ran it, and the `pad` snapshot (P/A/D components with SHA-256
#' digests, or `NULL` when the node kind has no complete
#' [bg_register_node_kind()] `pad` declaration). File-backed data counts only
#' as a regular file hashed before the run at the identical path; when that
#' rule is violated, or the Stan source or a consumed file changed while the
#' executor ran, `pad` is `NULL` and `capture_error` says why.
#'
#' @param project A `bg_handle`.
#' @param node_id A node id.
#' @return The execution's provenance list with its `job_id`, or `NULL` when
#'   the node has no fresh artifact or the artifact has no recorded
#'   provenance: executions from before provenance was recorded, and
#'   executions whose capture failed (their job records `capture_error`).
#'   Run the node, or re-execute it with `bg_run(project, rerun = node_id)`,
#'   to record one.
#' @export
bg_node_pad <- function(project, node_id) {
  S7::check_is_S7(project, bg_handle)
  if (!is.character(node_id) || length(node_id) != 1L || is.na(node_id)) {
    cli::cli_abort("{.arg node_id} must be a single node id string.")
  }
  graph <- bg_read_graph(project)
  if (!node_id %in% names(graph$nodes)) {
    cli::cli_abort("Node {.val {node_id}} not found in graph.")
  }

  execution <- bg_resolve_node_execution(project, node_id)
  if (is.null(execution)) {
    return(NULL)
  }
  c(list(job_id = execution$job$job_id), execution$job$provenance)
}
