# Portable bundle sources -----------------------------------------------------
#
# Policy for external files referenced by node params when bundling a project
# (the "referenced-sources" policy). Only params keys explicitly approved here
# may pull files into the archive; everything else (secrets, unreferenced
# siblings, executor source, whole directories) stays out. Relocation happens
# at BUNDLE time: files are copied to a project-relative location inside the
# archive, the staged graph is rewritten to the archived paths, and a
# relocation table lands in the bundle manifest. Restoring therefore never
# sources or evaluates project code.

#' Approved node-param keys treated as portable source files.
#'
#' The list is grounded in what the code actually reads: `stan_file` is the
#' only file-path param consumed by executors (checked with `file.exists()`
#' and passed to `cmdstanr::cmdstan_model()` by the `cmdstanr_fit`,
#' `prior_fit`, and `sbc` executors) and hashed by
#' `bg_source_hash_component()`. Adding a key here opts its values into
#' bundling, project-root resolution at run time, and the bundle relocation
#' table.
#' @return Character vector of approved params keys.
#' @keywords internal
#' @noRd
bg_bundle_source_keys <- function() {
  "stan_file"
}

#' Detect absolute filesystem paths (POSIX, Windows drive letters, UNC).
#' @keywords internal
#' @noRd
bg_is_absolute_path <- function(path) {
  startsWith(path, "/") ||
    startsWith(path, "\\") ||
    grepl("^[A-Za-z]:[\\\\/]", path)
}

#' Normalize path separators to forward slashes.
#' @keywords internal
#' @noRd
bg_path_with_slashes <- function(path) {
  gsub("\\", "/", path, fixed = TRUE)
}

#' Resolve one source-param value against a project root.
#'
#' Relative values resolve against the project root (the directory containing
#' `.bayesgrove`) when the file exists there; otherwise the value is returned
#' unchanged so prior working-directory behavior still applies. This is what
#' makes bundle-archived references (persisted as project-relative paths) work
#' from any working directory after a restore.
#' @param project_path Absolute path of the project root.
#' @param value A param value (single string).
#' @return The resolved path (absolute when found under the project root).
#' @keywords internal
#' @noRd
bg_resolve_project_source <- function(project_path, value) {
  if (bg_is_absolute_path(value)) {
    return(value)
  }

  candidate <- file.path(project_path, value)
  if (file.exists(candidate)) {
    normalizePath(candidate, mustWork = FALSE)
  } else {
    value
  }
}

#' Resolve every approved source param for execution.
#'
#' Returns a params list for the in-memory node handed to an executor; the
#' persisted graph is never rewritten by this helper. Executors receive only
#' `(node, inputs)` and cannot resolve project-relative paths themselves, so
#' both dispatch paths (sequential `bg_execute_node`, parallel
#' `bg_build_worker_task`) call this immediately before invoking the executor,
#' mirroring how `data_ref` values are resolved into `node$resolved$data`.
#' @param params A node params list.
#' @param project_path Absolute path of the project root.
#' @return The params list with approved keys resolved where possible.
#' @keywords internal
#' @noRd
bg_resolve_source_params <- function(params, project_path) {
  for (key in bg_bundle_source_keys()) {
    value <- params[[key]] %||% NULL
    if (is.character(value) && length(value) == 1L && !is.na(value)) {
      params[[key]] <- bg_resolve_project_source(project_path, value)
    }
  }
  params
}

#' Extract quoted `#include` targets from Stan program lines.
#'
#' stanc3 resolves `#include "rel/path.stan"` relative to the directory of the
#' file containing the directive, so the bundler archives includes at the same
#' relative position. Angle-bracket includes are compiler-internal and never
#' bundled.
#' @param lines Character vector of Stan program text.
#' @return Character vector of targets (NA where a line carries none).
#' @keywords internal
#' @noRd
bg_stan_include_targets <- function(lines) {
  pattern <- "^[[:space:]]*#include[[:space:]]+\"([^\"]+)\""
  hits <- regmatches(lines, regexec(pattern, lines))
  vapply(
    hits,
    function(m) if (length(m) > 0L) m[[2]] else NA_character_,
    character(1)
  )
}

#' Collect a Stan program plus its on-disk `#include` closure.
#'
#' Directives naming files that do not exist are reported in `missing` (the
#' bundler warns on them) rather than silently dropped. Cycles terminate via
#' the seen set.
#' @param stan_file Normalized path to an existing Stan program.
#' @return `list(files = <normalized paths>, missing = <normalized paths>)`.
#' @keywords internal
#' @noRd
bg_stan_source_closure <- function(stan_file) {
  files <- character()
  missing <- character()
  seen <- character()
  queue <- stan_file
  while (length(queue) > 0L) {
    current <- queue[[1]]
    queue <- queue[-1]
    if (current %in% seen) {
      next
    }
    seen <- c(seen, current)
    files <- c(files, current)

    targets <- Filter(
      Negate(is.na),
      bg_stan_include_targets(readLines(current, warn = FALSE))
    )
    for (target in targets) {
      resolved <- normalizePath(
        file.path(dirname(current), target),
        mustWork = FALSE
      )
      if (file.exists(resolved) && !dir.exists(resolved)) {
        queue <- c(queue, resolved)
      } else {
        missing <- c(missing, resolved)
      }
    }
  }

  list(files = unique(files), missing = unique(missing))
}

#' Longest common ancestor directory of a set of normalized paths.
#' @param paths Normalized absolute paths (forward slashes not required).
#' @return The deepest directory containing every path.
#' @keywords internal
#' @noRd
bg_common_root_dir <- function(paths) {
  splits <- strsplit(bg_path_with_slashes(paths), "/", fixed = TRUE)
  depth <- min(lengths(splits))
  common <- character()
  for (i in seq_len(depth)) {
    part <- splits[[1]][[i]]
    if (all(vapply(splits, function(s) identical(s[[i]], part), logical(1)))) {
      common <- c(common, part)
    } else {
      break
    }
  }

  # Drop the file component of the deepest common prefix. Absolute paths
  # always share the leading "" component, so "/" is the floor.
  if (length(common) <= 1L) {
    "/"
  } else {
    paste(common[-length(common)], collapse = "/")
  }
}

#' Path of `path` relative to an ancestor directory `root`.
#' @keywords internal
#' @noRd
bg_rel_within_root <- function(path, root) {
  path <- bg_path_with_slashes(path)
  root <- bg_path_with_slashes(root)
  prefix <- if (identical(root, "/")) "" else root
  rel <- sub("^/+", "", substring(path, nchar(prefix) + 1L))
  if (!nzchar(rel)) "." else rel
}

#' Copy approved source files into a bundle staging directory.
#'
#' Implements the "referenced-sources" policy for [bg_bundle()]: every value
#' held under an approved params key that names an existing file outside the
#' installed bayesgrove package is copied, together with its Stan `#include`
#' closure, into `<project>/bundle_sources/<key>/src_<n>/...`. The tree under
#' each slot is rooted at the closure's common ancestor so relative includes
#' keep resolving after the move. The staged `graph.json` is rewritten in
#' place so every relocated param points at its archived, project-relative
#' path.
#'
#' Security boundary: only files reachable from approved param values are
#' copied — never directories wholesale, never unreferenced siblings.
#' References that are missing at bundle time produce a warning and keep
#' their original value. References into the installed bayesgrove package are
#' left untouched (portable wherever the package is installed).
#'
#' @param graph The project graph (from a snapshot).
#' @param dest_proj_dir Staged project directory inside the bundle temp dir.
#' @return The `source_policy` manifest section: policy name and version, the
#'   approved keys, and a relocation table keyed by the original param value,
#'   each entry carrying the archived path and a SHA-256 per copied file.
#' @keywords internal
#' @noRd
bg_copy_bundle_sources <- function(graph, dest_proj_dir) {
  policy <- list(
    name = "referenced-sources",
    version = 1L,
    approved_keys = bg_bundle_source_keys(),
    relocation = list()
  )

  # Map each referenced value to the approved key that referenced it (first
  # key wins on duplicates; the same file archives to the same slot either
  # way). Only character scalar values are reference-shaped.
  key_by_value <- c()
  for (node_id in names(graph$nodes %||% list())) {
    params <- graph$nodes[[node_id]]$params %||% NULL
    for (key in bg_bundle_source_keys()) {
      value <- params[[key]] %||% NULL
      if (
        is.character(value) &&
          length(value) == 1L &&
          !is.na(value) &&
          is.null(key_by_value[[value]])
      ) {
        key_by_value[[value]] <- key
      }
    }
  }
  references <- sort(names(key_by_value))
  if (length(references) == 0L) {
    return(policy)
  }

  package_root <- bg_path_with_slashes(system.file(package = "bayesgrove"))

  closures <- list()
  roots <- character()
  for (value in references) {
    # Directories are never bundled (policy: no wholesale copies) and cannot
    # be Stan programs; treat them like missing references.
    if (!file.exists(value) || dir.exists(value)) {
      cli::cli_warn(
        "Bundle is missing referenced source file {.path {value}}."
      )
      next
    }
    normalized <- normalizePath(value, mustWork = FALSE)
    if (startsWith(bg_path_with_slashes(normalized), package_root)) {
      # Package-shipped sources are portable wherever bayesgrove is
      # installed; freezing copies would only invalidate fingerprints.
      next
    }

    closure <- bg_stan_source_closure(normalized)
    for (missing_path in closure$missing) {
      cli::cli_warn(c(
        "Bundle is missing included Stan file {.path {missing_path}}.",
        "i" = paste0(
          "Referenced by {.path {normalized}}; the restored model will not ",
          "compile without it."
        )
      ))
    }
    closures[[value]] <- closure
    roots[[value]] <- bg_common_root_dir(closure$files)
  }

  # One archive slot per distinct closure root, in deterministic order.
  slot_of_root <- setNames(
    paste0("src_", seq_along(sort(unique(roots)))),
    sort(unique(roots))
  )

  for (value in names(closures)) {
    closure <- closures[[value]]
    root <- roots[[value]]
    slot <- slot_of_root[[root]]

    hashes <- c()
    archived_paths <- c()
    main_normalized <- normalizePath(value, mustWork = FALSE)
    main_archived <- NULL
    for (src in closure$files) {
      rel <- bg_rel_within_root(src, root)
      archived <- paste(
        c("bundle_sources", key_by_value[[value]], slot, rel),
        collapse = "/"
      )
      if (identical(src, main_normalized)) {
        main_archived <- archived
      }
      dest <- file.path(dest_proj_dir, archived)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(src, dest, overwrite = TRUE)

      archived_paths <- c(archived_paths, archived)
      hashes <- c(
        hashes,
        digest::digest(file = src, algo = "sha256")
      )
    }

    ord <- order(archived_paths)
    policy$relocation[[value]] <- list(
      archived = main_archived,
      files = stats::setNames(as.list(hashes[ord]), archived_paths[ord])
    )
  }

  bg_rewrite_staged_graph_sources(dest_proj_dir, policy$relocation)

  policy
}

#' Rewrite approved source params in the staged bundle graph.
#'
#' Reads the staged `graph.json` with `simplifyVector = FALSE` (structure is
#' preserved exactly as persisted), replaces every param value that has a
#' relocation entry with its archived project-relative path, and writes the
#' file back atomically. The graph version is unchanged: the staged copy is
#' the same graph with relocated references, and the relocation table in the
#' bundle manifest records what moved.
#' @param dest_proj_dir Staged project directory inside the bundle temp dir.
#' @param relocation Relocation table keyed by original param value.
#' @return `TRUE` when the graph was rewritten, `FALSE` when skipped.
#' @keywords internal
#' @noRd
bg_rewrite_staged_graph_sources <- function(dest_proj_dir, relocation) {
  graph_path <- file.path(dest_proj_dir, ".bayesgrove", "graph", "graph.json")
  if (!file.exists(graph_path) || length(relocation) == 0L) {
    return(FALSE)
  }

  raw <- jsonlite::read_json(graph_path, simplifyVector = FALSE)
  for (node_id in names(raw$nodes %||% list())) {
    params <- raw$nodes[[node_id]]$params %||% NULL
    for (key in bg_bundle_source_keys()) {
      value <- params[[key]] %||% NULL
      if (
        is.character(value) &&
          length(value) == 1L &&
          !is.na(value) &&
          !is.null(relocation[[value]])
      ) {
        raw$nodes[[node_id]]$params[[key]] <- relocation[[value]]$archived
      }
    }
  }

  bg_write_json_atomic(graph_path, raw, sort_keys = FALSE)
  TRUE
}
