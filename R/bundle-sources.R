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

#' Prefix marking a package-relative source reference.
#'
#' Bundles rewrite `system.file()`-based references into
#' `bayesgrove:<package-relative path>` sentinels: the bytes are not copied
#' (the installed package already carries them) and the path string becomes
#' machine-stable, unlike the absolute library path it replaces.
#' `bg_resolve_project_source()` expands these through `system.file()`.
#' @return The sentinel prefix (single string).
#' @keywords internal
#' @noRd
bg_package_source_prefix <- function() {
  "bayesgrove:"
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
#' Resolution order, shared by bundling, fingerprinting, and both executor
#' dispatch paths:
#'
#' - Package references (`bayesgrove:<rel>` sentinels, written when a bundle
#'   rewrites a `system.file()`-based `stan_file`) expand through
#'   `system.file(<rel>, package = "bayesgrove")`, so they resolve on any
#'   machine with the package installed. An unresolvable sentinel falls
#'   through to the remaining rules and downstream code treats it like any
#'   other missing file.
#' - Absolute paths are used as-is.
#' - Relative values resolve against the project root (the directory
#'   containing `.bayesgrove`) when the file exists there; otherwise the
#'   value is returned unchanged so prior working-directory behavior still
#'   applies. This is what makes bundle-archived references (persisted as
#'   project-relative paths) work from any working directory after a
#'   restore.
#'
#' @param project_path Absolute path of the project root.
#' @param value A param value (single string).
#' @return The resolved path (absolute when a target was found).
#' @keywords internal
#' @noRd
bg_resolve_project_source <- function(project_path, value) {
  if (startsWith(value, bg_package_source_prefix())) {
    expanded <- system.file(
      sub(paste0("^", bg_package_source_prefix()), "", value),
      package = "bayesgrove",
      mustWork = FALSE
    )
    if (nzchar(expanded)) {
      return(normalizePath(expanded, mustWork = FALSE))
    }
    # An unresolvable sentinel falls through to path resolution, so the
    # eventual file.exists() treats it like any other missing file.
  }

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
#'
#' When the paths diverge, the deepest shared components are directories, so
#' the common prefix IS the root. When there is only one distinct path, the
#' prefix is the file itself and its directory is the root.
#' @param paths Normalized absolute paths (forward slashes not required).
#' @return The deepest directory containing every path.
#' @keywords internal
#' @noRd
bg_common_root_dir <- function(paths) {
  slashed <- unique(bg_path_with_slashes(paths))
  splits <- strsplit(slashed, "/", fixed = TRUE)
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

  if (length(slashed) == 1L && length(common) > 0L) {
    common <- common[-length(common)]
  }
  # Absolute paths share the leading "" component, so "/" is the floor.
  if (length(common) <= 1L) {
    "/"
  } else {
    paste(common, collapse = "/")
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
#' Each raw param value is resolved with `bg_resolve_project_source()` first,
#' so relative references bundle identically from any working directory —
#' the same resolution the executor and the fingerprint use at run time.
#'
#' Security boundary: only files reachable from approved param values are
#' copied — never directories wholesale, never unreferenced siblings.
#' References that are missing at bundle time produce a warning and keep
#' their original value. References into the installed bayesgrove package are
#' not copied; they are rewritten to `bayesgrove:` package sentinels
#' (see `bg_package_source_prefix()`), which resolve through `system.file()`
#' on every machine with the package installed.
#'
#' @param graph The project graph (from a snapshot).
#' @param dest_proj_dir Staged project directory inside the bundle temp dir.
#' @param project_path Absolute path of the project root, used to resolve
#'   relative references exactly like at run time.
#' @return The `source_policy` manifest section: policy name and version, the
#'   approved keys, and a relocation table keyed by the raw param value (what
#'   the graph rewrite matches). Copied entries carry the resolved original
#'   path, the archived path, and a SHA-256 per copied file; package-shipped
#'   entries carry the resolved original path and the `package_ref` sentinel
#'   they were rewritten to (no bytes copied).
#' @keywords internal
#' @noRd
bg_copy_bundle_sources <- function(graph, dest_proj_dir, project_path) {
  policy <- list(
    name = "referenced-sources",
    version = 1L,
    approved_keys = bg_bundle_source_keys(),
    relocation = list()
  )

  # Map each referenced value to the approved key that referenced it (first
  # key wins on duplicates; the same file archives to the same slot either
  # way). Only character scalar values are reference-shaped.
  key_by_value <- list()
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
  originals <- character()
  for (value in references) {
    # Resolve exactly like the executor and the fingerprint do, so a
    # project-relative reference bundles from any working directory.
    resolved <- bg_resolve_project_source(project_path, value)
    # Directories are never bundled (policy: no wholesale copies) and cannot
    # be Stan programs; treat them like missing references.
    if (!file.exists(resolved) || dir.exists(resolved)) {
      cli::cli_warn(
        "Bundle is missing referenced source file {.path {value}}."
      )
      next
    }
    normalized <- normalizePath(resolved, mustWork = FALSE)
    if (startsWith(bg_path_with_slashes(normalized), package_root)) {
      # Package-shipped sources are not copied: the installed package
      # already carries the bytes on every machine. Instead the staged graph
      # reference becomes a machine-stable package sentinel, so a restore
      # under a different library path still resolves (and the params hash
      # stops depending on where the library lives).
      policy$relocation[[value]] <- list(
        original = normalized,
        package_ref = paste0(
          bg_package_source_prefix(),
          bg_rel_within_root(normalized, package_root)
        ),
        files = list()
      )
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
    originals[[value]] <- normalized
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
    original <- originals[[value]]

    hashes <- c()
    archived_paths <- c()
    main_archived <- NULL
    for (src in closure$files) {
      rel <- bg_rel_within_root(src, root)
      archived <- paste(
        c("bundle_sources", key_by_value[[value]], slot, rel),
        collapse = "/"
      )
      if (identical(src, original)) {
        main_archived <- archived
      }
      dest <- file.path(dest_proj_dir, archived)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      if (!isTRUE(file.copy(src, dest, overwrite = TRUE))) {
        cli::cli_abort(c(
          "Failed to copy {.path {src}} into the bundle at {.path {dest}}.",
          "i" = "No relocation was recorded for this reference."
        ))
      }

      archived_paths <- c(archived_paths, archived)
      # Hash the copied destination so the manifest always reflects the
      # archive bytes, not the source after later edits.
      hashes <- c(
        hashes,
        digest::digest(file = dest, algo = "sha256")
      )
    }

    ord <- order(archived_paths)
    policy$relocation[[value]] <- list(
      original = original,
      archived = main_archived,
      files = stats::setNames(as.list(hashes[ord]), archived_paths[ord])
    )
  }

  bg_rewrite_staged_graph_sources(dest_proj_dir, policy$relocation)

  policy
}

#' The value a relocated reference takes in the staged graph.
#'
#' Copied references point at their archived project-relative path
#' (`archived`); package-shipped references point at a machine-stable
#' `bayesgrove:` sentinel (`package_ref`). Exactly one is set per entry.
#' @param entry One relocation table entry.
#' @return The replacement param value (single string), or NULL.
#' @keywords internal
#' @noRd
bg_relocation_target <- function(entry) {
  entry$archived %||% entry$package_ref %||% NULL
}

#' Rewrite approved source params in the staged bundle graph.
#'
#' Reads the staged `graph.json` with `simplifyVector = FALSE` (structure is
#' preserved exactly as persisted), replaces every param value that has a
#' relocation entry with its replacement (archived path for copied files,
#' package sentinel for package-shipped sources), and writes the file back
#' atomically. The graph version is unchanged: the staged copy is the same
#' graph with relocated references, and the relocation table in the bundle
#' manifest records what moved.
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
      replacement <- if (
        is.character(value) &&
          length(value) == 1L &&
          !is.na(value)
      ) {
        bg_relocation_target(relocation[[value]] %||% NULL)
      } else {
        NULL
      }
      if (!is.null(replacement)) {
        raw$nodes[[node_id]]$params[[key]] <- replacement
      }
    }
  }

  bg_write_json_atomic(graph_path, raw, sort_keys = FALSE)
  TRUE
}
