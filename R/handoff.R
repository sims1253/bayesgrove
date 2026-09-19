#' Capture a reproducibility manifest for bundling.
#'
#' Captures R/platform session info plus CmdStan/Stan versions (when cmdstanr
#' is installed) so a bundle records the environment that produced it. On
#' cross-machine restore, a caller compares manifests and warns on mismatch
#' rather than silently rerunning everything.
#'
#' Uses `utils::sessionInfo()` to avoid a hard `sessioninfo` dependency; the
#' output is a structured list (not the print-formatted string) so it
#' round-trips through JSON.
#' @return A list with `$r_version`, `$platform`, `$packages` (named versions),
#'   and `$cmdstan` (version string or NULL).
#' @keywords internal
#' @noRd
bg_reproducibility_manifest <- function() {
  si <- utils::sessionInfo()
  pkgs <- c()
  op <- si$loadedOnly
  if (is.list(op)) {
    pkgs <- vapply(op, function(p) p$Version %||% NA_character_, character(1))
  }
  ap <- si$otherPkgs
  if (is.list(ap)) {
    pkgs <- c(
      pkgs,
      vapply(ap, function(p) p$Version %||% NA_character_, character(1))
    )
  }

  cmdstan <- NULL
  if (requireNamespace("cmdstanr", quietly = TRUE)) {
    cmdstan <- tryCatch(
      as.character(cmdstanr::cmdstan_version()),
      error = function(e) NA_character_
    )
  }

  list(
    captured_at = bg_now_timestamp(),
    r_version = si$R.version$version.string %||% NA_character_,
    platform = si$platform %||% NA_character_,
    packages = pkgs[order(names(pkgs))],
    cmdstan = cmdstan
  )
}

#' Bundle a bayesgrove project for reproducible handoff
#'
#' Creates a portable `.tar.gz` archive of the project, including the structural
#' graph, decision log, job history, configuration, attached data, and cached
#' artifacts. Model fit artifacts are optional.
#'
#' **Portable source policy**
#'
#' External files referenced by approved node params are copied into the
#' archive and their references relocated, so a restored project works after
#' the original files (and machine) are gone:
#'
#' * Approved keys: `stan_file`. The referenced Stan program is copied together
#'   with every file reachable through `#include "..."` directives (resolved
#'   relative to the including file, as stanc3 does). The key list is grounded
#'   in what the executors actually read.
#' * References into the installed bayesgrove package (e.g.
#'   `system.file("stan", ...)` models) are rewritten to a machine-stable
#'   package reference (`bayesgrove:stan/<model>.stan`) instead of being
#'   copied: the installed package already carries the bytes on every
#'   machine, and the sentinel path no longer depends on where the library
#'   lives, so restored projects resolve it anywhere and their fingerprints
#'   are comparable across machines. Only a package upgrade (recorded in the
#'   environment and reproducibility manifests) can change these sources.
#' * Nothing else is copied. Executor dependencies (persisted separately as
#'   inert source and restored only through the trust-gated
#'   [bg_restore_executors()]), external data (attach it with
#'   [bg_set_node_data()] so it travels through the CAS), secrets, and
#'   unreferenced siblings of a referenced file all stay outside the archive.
#'   Directories are never copied wholesale.
#' * A referenced file that is missing at bundle time produces a warning and
#'   the reference is left as-is.
#'
#' Relocation happens at bundle time: files are archived under
#' `bundle_sources/` inside the project directory, the archived graph stores
#' the archived paths relative to the project root, and
#' `.bayesgrove/bundle_manifest.json` records a `source_policy` section with
#' the policy name and version, the approved keys, and a relocation table
#' keyed by the raw param value, each entry carrying the resolved original
#' path, the archived path, and a SHA-256 per copied file. Restoring
#' therefore executes no project code: untar and [bg_open()] as usual.
#' Relative `stan_file` params resolve against the project root at bundling,
#' fingerprinting, and execution time, so a project bundles and runs
#' identically from any working directory.
#'
#' Because the params hash covers the path string, relocated references change
#' fingerprints and restored fit nodes rerun by default (`include_fits` is
#' `FALSE` by default, so cached fits do not travel anyway).
#'
#' @param project A `bg_handle`.
#' @param path File path where the bundle should be saved (default: a temp file).
#'   Relative paths are resolved from the current working directory.
#' @param include_fits Logical, whether to package the large model fit artifacts.
#'
#' @return The file path to the generated bundle.
#' @export
bg_bundle <- function(
  project,
  path = NULL,
  include_fits = FALSE
) {
  S7::check_is_S7(project, bg_handle)

  if (is.null(path)) {
    path <- file.path(
      tempdir(),
      sprintf(
        "%s_bundle_%s.tar.gz",
        project@project_id,
        format(Sys.time(), "%Y%m%d%H%M%S")
      )
    )
  }
  path <- file.path(
    normalizePath(dirname(path), mustWork = TRUE),
    basename(path)
  )

  # Ensure the project state is up to date
  snap <- bg_snapshot(project)

  tmp_dir <- tempfile("bgb")
  unlink(tmp_dir, recursive = TRUE)
  dir.create(tmp_dir, recursive = TRUE)

  dest_proj_dir <- file.path(tmp_dir, basename(project@path))

  # 1. Copy core project files (.bayesgrove metadata directory)
  # Copy files selectively to exclude model fit artifacts when requested.
  dir.create(file.path(dest_proj_dir, ".bayesgrove"), recursive = TRUE)

  # Copy graph, decisions, runs, workflow state (summaries JSONL + branch and
  # goal registries; without these a restored bundle loses its protocol
  # state: obligations, holds, and branches), and config.
  for (sub in c("graph", "decisions", "runs", "workflow", "config.json")) {
    src_path <- file.path(project@path, ".bayesgrove", sub)
    if (file.exists(src_path)) {
      file.copy(
        src_path,
        file.path(dest_proj_dir, ".bayesgrove"),
        recursive = TRUE
      )
    }
  }

  # 2. Handle Artifacts and Fits
  cache_src <- file.path(project@path, ".bayesgrove", "cache")
  cache_dest <- file.path(dest_proj_dir, ".bayesgrove", "cache")

  if (file.exists(cache_src)) {
    dir.create(cache_dest, recursive = TRUE)

    # Always copy the index
    idx_src <- file.path(cache_src, "index.json")
    if (file.exists(idx_src)) {
      file.copy(idx_src, cache_dest)
    }

    # Attached data are not indexed execution results, but must travel with
    # the graph even if their nodes have never run.
    artifact_refs <- unlist(
      lapply(snap$graph$nodes, function(node) {
        node$params$data_ref
      }),
      use.names = FALSE
    )

    # Include indexed results unless their bindings identify a model fit.
    idx <- bg_read_artifact_index(project)

    for (fp in names(idx)) {
      meta <- idx[[fp]]
      binding_nodes <- names(Filter(
        function(binding) identical(binding$status, "active"),
        meta$bindings %||% list()
      ))
      bound_graph_nodes <- Filter(
        Negate(is.null),
        lapply(binding_nodes, function(node_id) {
          snap$graph$nodes[[node_id]] %||% NULL
        })
      )

      # Kinds whose artifacts are large model fits: the generic "fit"/"compile"
      # kinds from bg_use_default_workflow plus the real backend fit kinds.
      fit_kinds <- c(
        "fit",
        "compile",
        "cmdstanr_fit",
        "brms_fit",
        "prior_fit",
        "brms_prior_fit"
      )
      should_include <- TRUE
      if (
        !include_fits &&
          any(vapply(
            bound_graph_nodes,
            function(node) node$kind %in% fit_kinds,
            logical(1)
          ))
      ) {
        should_include <- FALSE
      }

      if (!should_include || is.null(meta$artifact_ref)) {
        next
      }

      artifact_refs <- c(artifact_refs, meta$artifact_ref)
    }

    for (ref in unique(artifact_refs)) {
      if (
        !is.character(ref) ||
          is.na(ref) ||
          !grepl("^cas:sha256:[0-9a-f]{64}$", ref)
      ) {
        next
      }
      hash <- sub("^cas:sha256:", "", ref)
      prefix <- substr(hash, 1, 2)
      cas_file <- file.path(cache_src, "sha256", prefix, paste0(hash, ".rds"))

      if (file.exists(cas_file)) {
        dir.create(
          file.path(cache_dest, "sha256", prefix),
          recursive = TRUE,
          showWarnings = FALSE
        )
        file.copy(cas_file, file.path(cache_dest, "sha256", prefix))
      } else {
        cli::cli_warn("Bundle is missing referenced artifact {.val {ref}}.")
      }
    }
  }

  # 3. Portable sources (the "referenced-sources" policy): copy approved
  # external files referenced by node params (Stan programs plus their
  # include closure) into the archive and rewrite the staged graph to
  # project-relative archived paths, so a restored project works after the
  # original paths are gone and restoring runs no project code. References
  # resolve against the project root, matching run-time resolution.
  source_policy <- bg_copy_bundle_sources(
    graph = snap$graph,
    dest_proj_dir = dest_proj_dir,
    project_path = project@path
  )

  # 4. Write bundle manifest, including a reproducibility manifest (session
  # info + CmdStan/Stan versions) so cross-machine restores surface
  # environment mismatches as a warning rather than silently rerunning.
  manifest <- list(
    schema_name = "bg_bundle_manifest",
    schema_version = 1,
    bundle_id = bg_new_id("bundle"),
    project_id = project@project_id,
    created_at = bg_now_timestamp(),
    include_fits = include_fits,
    source_policy = source_policy,
    reproducibility = bg_reproducibility_manifest()
  )

  jsonlite::write_json(
    manifest,
    file.path(dest_proj_dir, ".bayesgrove", "bundle_manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = I(17)
  )

  # 4. Create the tar archive
  tar_bin <- Sys.which("tar")
  tar_tool <- if (nzchar(tar_bin)) tar_bin else "internal"
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp_dir)
  utils::tar(
    path,
    files = basename(project@path),
    compression = "gzip",
    tar = tar_tool
  )

  cli::cli_inform("Project bundled successfully: {.path {path}}")

  path
}

#' Generate a reproducible markdown report of the workflow
#'
#' @param project A `bg_handle`.
#' @param path Optional output path. Defaults to `workflow_report.md` or
#'   `workflow_report.html` in the project root depending on `format`.
#' @param format Output format: markdown (`"md"`) or html (`"html"`).
#'
#' @return The path to the written report file.
#' @export
bg_export_report <- function(
  project,
  path = NULL,
  format = c("html", "md")
) {
  S7::check_is_S7(project, bg_handle)
  format <- match.arg(format)

  if (is.null(path)) {
    path <- file.path(
      project@path,
      if (identical(format, "html")) {
        "workflow_report.html"
      } else {
        "workflow_report.md"
      }
    )
  } else if (!grepl("^(/|[A-Za-z]:[\\\\/])", path)) {
    path <- file.path(project@path, path)
  }

  snap <- bg_snapshot(project)

  # Build a simple markdown document
  lines <- c(
    sprintf("# bayesgrove Workflow Report: %s", snap$name),
    sprintf("**Project ID:** `%s`", snap$project_id),
    sprintf("**Generated:** %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("**Workflow state:** `%s`", snap$status$workflow_state),
    "",
    "## Graph Topology"
  )

  # Add nodes
  plan <- bg_plan(project)
  for (id in plan$graph_plan$topo_order) {
    node <- snap$graph$nodes[[id]]
    lines <- c(
      lines,
      sprintf("- **%s** (`%s`): %s", id, node$kind, node$label %||% "No label")
    )

    # Show upstream connections
    upstreams <- bg_dagri_incoming_edges(snap$graph, id)
    if (length(upstreams) > 0) {
      lines <- c(
        lines,
        sprintf(
          "  - *Inputs:* %s",
          paste(sapply(upstreams, function(e) e$from), collapse = ", ")
        )
      )
    }
  }

  # Embed the Mermaid flowchart. In Markdown a ```mermaid fence
  # renders on GitHub (and in Quarto). Append the HTML embed after the
  # escaped <pre> block at write time below. It must not go through
  # bg_escape_html, or the <div>/<script> tags render as literal text.
  mermaid <- tryCatch(
    as.character(bg_graph_mermaid(project)),
    error = function(e) NULL
  )
  if (!is.null(mermaid) && !identical(format, "html")) {
    lines <- c(lines, "", "## DAG (Mermaid)", "", "```mermaid", mermaid, "```")
  }

  # Add Decisions
  lines <- c(lines, "", "## Decision Provenance")
  if (length(snap$decisions) == 0) {
    lines <- c(lines, "No decisions recorded.")
  } else {
    for (dec in snap$decisions) {
      lines <- c(
        lines,
        sprintf("### %s", dec$prompt),
        sprintf("- **Choice:** %s", dec$choice),
        sprintf("- **Rationale:** %s", dec$rationale),
        sprintf("- **Scope:** %s", dec$scope),
        sprintf("- **Timestamp:** %s", dec$created_at)
      )

      if (length(dec$alternatives) > 0) {
        lines <- c(
          lines,
          sprintf(
            "- **Alternatives considered:** %s",
            paste(dec$alternatives, collapse = ", ")
          )
        )
      }

      if (length(dec$evidence) > 0) {
        lines <- c(
          lines,
          sprintf(
            "- **Evidence nodes:** %s",
            paste(dec$evidence, collapse = ", ")
          )
        )
      }

      if (length(dec$refs) > 0) {
        lines <- c(
          lines,
          sprintf("- **References:** %s", bg_format_report_refs(dec$refs))
        )
      }

      if (!is.null(dec$metadata$edge_id)) {
        lines <- c(
          lines,
          sprintf(
            "- **Gate edge:** %s (%s -> %s)",
            dec$metadata$edge_id,
            dec$metadata$from_node_id,
            dec$metadata$to_node_id
          )
        )
      }

      lines <- c(lines, "")
    }
  }

  lines <- c(lines, bg_research_report(snap$research))

  lines <- c(lines, "## Artifact Index")
  if (length(snap$artifacts) == 0) {
    lines <- c(lines, "No cached artifacts recorded.")
  } else {
    for (fp in names(snap$artifacts)) {
      meta <- snap$artifacts[[fp]]
      lines <- c(
        lines,
        sprintf(
          "- `%s`: %s (%s)",
          meta$node_id,
          fp,
          meta$artifact_ref
        )
      )
    }
  }

  if (identical(format, "html")) {
    mermaid_html <- if (!is.null(mermaid)) {
      c(
        "<h2>DAG (Mermaid)</h2>",
        '<div class="mermaid">',
        mermaid,
        "</div>",
        '<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>',
        "<script>mermaid.initialize({startOnLoad: true});</script>"
      )
    } else {
      character()
    }
    html_lines <- c(
      "<!DOCTYPE html>",
      "<html><head><meta charset=\"utf-8\"><title>bayesgrove Workflow Report</title></head><body><pre>",
      bg_escape_html(paste(lines, collapse = "\n")),
      "</pre>",
      mermaid_html,
      "</body></html>"
    )
    writeLines(html_lines, path)
  } else {
    writeLines(lines, path)
  }

  cli::cli_inform("Report exported to {.path {path}}")
  path
}

bg_format_report_refs <- function(refs) {
  rendered <- vapply(
    refs,
    function(ref) {
      if (is.list(ref)) {
        paste(
          Filter(
            nzchar,
            c(
              ref$citekey %||% "",
              ref$note %||% "",
              ref$url %||% ""
            )
          ),
          collapse = " | "
        )
      } else {
        as.character(ref)
      }
    },
    character(1)
  )

  paste(rendered, collapse = "; ")
}

bg_escape_html <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  gsub(">", "&gt;", text, fixed = TRUE)
}
