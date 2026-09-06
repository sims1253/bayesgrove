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
#' graph, decision log, job history, configuration, and optionally cached artifacts
#' or raw data files.
#'
#' @param project A `bg_handle`.
#' @param path File path where the bundle should be saved (default: a temp file).
#' @param include_data How to handle external data sources: 'omit',
#'   'freeze_source', or 'copy'. The legacy alias 'recipe_only' is accepted and
#'   treated as 'omit'.
#' @param include_fits Logical, whether to package the large model fit artifacts.
#'
#' @return The file path to the generated bundle.
#' @export
bg_bundle <- function(
  project,
  path = NULL,
  include_data = c("omit", "freeze_source", "copy", "recipe_only"),
  include_fits = FALSE
) {
  include_data <- match.arg(include_data)
  S7::check_is_S7(project, bg_handle)

  if (identical(include_data, "recipe_only")) {
    cli::cli_warn(
      "include_data = 'recipe_only' is deprecated; using 'omit' instead."
    )
    include_data <- "omit"
  }

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

  # Ensure the project state is up to date
  snap <- bg_snapshot(project)

  tmp_dir <- tempfile("bgb")
  unlink(tmp_dir, recursive = TRUE)
  dir.create(tmp_dir, recursive = TRUE)

  dest_proj_dir <- file.path(tmp_dir, basename(project@path))

  # 1. Copy core project files (.bayesgrove metadata directory)
  # We do NOT just copy the whole folder because we want to filter large artifacts
  dir.create(file.path(dest_proj_dir, ".bayesgrove"), recursive = TRUE)

  # Copy graph, decisions, runs, workflow state (summaries JSONL + branch and
  # goal registries — without these a restored bundle loses its protocol
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

    # Conditionally copy actual RDS artifacts
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

      hash <- sub("^cas:sha256:", "", meta$artifact_ref)
      prefix <- substr(hash, 1, 2)
      cas_file <- file.path(cache_src, "sha256", prefix, paste0(hash, ".rds"))

      if (file.exists(cas_file)) {
        dir.create(
          file.path(cache_dest, "sha256", prefix),
          recursive = TRUE,
          showWarnings = FALSE
        )
        file.copy(cas_file, file.path(cache_dest, "sha256", prefix))
      }
    }
  }

  # 3. Write bundle manifest, including a reproducibility manifest (session
  # info + CmdStan/Stan versions) so cross-machine restores surface
  # environment mismatches as a warning rather than silently rerunning.
  manifest <- list(
    schema_name = "bg_bundle_manifest",
    schema_version = 1,
    bundle_id = bg_new_id("bundle"),
    project_id = project@project_id,
    created_at = bg_now_timestamp(),
    data_policy = include_data,
    include_fits = include_fits,
    reproducibility = bg_reproducibility_manifest()
  )

  jsonlite::write_json(
    manifest,
    file.path(dest_proj_dir, ".bayesgrove", "bundle_manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = I(17)
  )

  # 4. Tar it up
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
#' @param out_file Deprecated alias for `path`.
#'
#' @return The path to the written report file.
#' @export
bg_export_report <- function(
  project,
  path = NULL,
  format = c("html", "md"),
  out_file = NULL
) {
  S7::check_is_S7(project, bg_handle)
  format <- match.arg(format)

  if (!is.null(out_file)) {
    cli::cli_warn(
      "`out_file` is deprecated; use `path` and `format` instead."
    )
    path <- out_file
  }

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

  # Embed the Mermaid flowchart (Milestone 7). In Markdown a ```mermaid fence
  # renders on GitHub (and in Quarto). The HTML embed is appended AFTER the
  # escaped <pre> block at write time below — it must not go through
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
