# Verify captured documentation output without CmdStan. Companion to
# tools/precompute-vignettes.R, which REFRESHES the three precomputed sampler
# vignettes with a real CmdStan run. This script only VERIFIES output that is
# deterministic, so it runs in CI. From the repository root:
#
#   Rscript tools/check-captured-output.R            verify; exit 1 on drift
#   Rscript tools/check-captured-output.R --refresh  rewrite deterministic
#                                                    output for the maintainer
#
# Policy (the lists below are the single source of truth):
# - README.md is re-rendered from README.Rmd and compared at the content
#   level (whitespace- and pandoc-version-insensitive).
# - The chunks listed in `checkable_chunks` are re-evaluated against the
#   installed checkout and compared with their pasted output blocks.
# - The three sampler vignettes in `precomputed_docs` are NEVER compared or
#   rewritten here: their pasted blocks mix random run/node/decision ids,
#   sampler-dependent diagnostics, and timings, and re-executing them needs
#   CmdStan. Refresh them only with tools/precompute-vignettes.R.
# - Documents in `live_docs` evaluate their chunks at build time (R CMD check
#   exercises them), so pasted output there is forbidden; the guard below
#   fails if any appears.

args <- commandArgs(trailingOnly = TRUE)
refresh <- identical(args, "--refresh")
if (length(args) > 1L || (length(args) == 1L && !refresh)) {
  stop("Usage: Rscript tools/check-captured-output.R [--refresh]")
}

source_dir <- normalizePath(".")

# --------------------------------------------------------------------------
# Policy configuration
# --------------------------------------------------------------------------

live_docs <- c(
  "concepts",
  "extensions",
  "research-search",
  "simulation-study",
  "articles/teaching",
  "articles/reproducible-research"
)

precomputed_docs <- c(
  "eight-schools" = paste(
    "MCMC fits via cmdstanr; pasted blocks carry random run/node ids and",
    "sampler-dependent diagnostics; re-execution requires CmdStan."
  ),
  "getting-started" = paste(
    "MCMC fits and posterior predictive draws; pasted blocks carry random",
    "ids and sampler-dependent p-values; re-execution requires CmdStan."
  ),
  "case-study-roaches" = paste(
    "Three MCMC fits, PPC p-values, and LOO comparisons; pasted blocks",
    "carry random ids, timings, and sampler-dependent values; re-execution",
    "requires CmdStan."
  )
)

# Deterministic exceptions inside the precomputed vignettes. Each entry must
# print plain values only (no plots, no random ids, no sampler state) and must
# not depend on earlier chunks of its document.
checkable_chunks <- list(
  list(
    doc = "case-study-roaches",
    label = "data",
    reason = paste(
      "Summaries of the shipped roaches.csv; platform-stable and",
      "independent of the sampler."
    )
  )
)

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

doc_path <- function(doc) {
  file.path(source_dir, "vignettes", paste0(doc, ".Rmd"))
}

read_doc <- function(doc) {
  readLines(doc_path(doc), warn = FALSE)
}

# Locate an R chunk by label; returns c(start, end) line indices of the whole
# chunk (fences included), or NULL.
find_chunk <- function(lines, label) {
  starts <- grep("^```\\{r(?:[ ,}])", lines, perl = TRUE)
  for (start in starts) {
    end <- start + which(lines[(start + 1L):length(lines)] == "```")[1L]
    chunk_label <- sub(
      "^```\\{r[[:space:]]*([^,}]*)[^{]*$",
      "\\1",
      lines[[start]]
    )
    if (identical(trimws(chunk_label), label)) {
      return(c(start, end))
    }
  }
  NULL
}

# The fenced output block directly after a chunk, or NULL when the chunk is
# followed by prose. Returns c(start, end) of the whole block.
block_after_chunk <- function(lines, chunk) {
  i <- chunk[[2]] + 1L
  while (i <= length(lines) && !nzchar(trimws(lines[[i]]))) {
    i <- i + 1L
  }
  if (
    i > length(lines) ||
      !grepl("^```", lines[[i]]) ||
      grepl("^```\\{r", lines[[i]])
  ) {
    return(NULL)
  }
  end <- i + which(lines[(i + 1L):length(lines)] == "```")[1L]
  c(i, end)
}

# Conservative normalization applied to both sides of every output
# comparison. Values that cannot be normalized reliably (for example
# sampler-dependent numbers) are excluded by policy instead, so no stochastic
# value is ever blurred into a deterministic-looking one.
normalize_lines <- function(lines) {
  lines <- sub("[[:space:]]+$", "", lines)
  # knitr's comment prefix is a display convention, not content.
  lines <- sub("^[[:space:]]*#[>]'?[[:space:]]?", "", lines)
  # Random content hashes: node/run/decision/obligation/action/gate ids.
  lines <- gsub(
    "\\b(node|run|edge|dec|obl|act|gate|branch)_[0-9a-f]{6,}\\b",
    "\\1_<id>",
    lines
  )
  lines <- gsub("\\bbranch:[0-9a-f]{6,}\\b", "branch:<id>", lines)
  # ISO 8601 and common report timestamps.
  lines <- gsub(
    "[0-9]{4}-[0-9]{2}-[0-9]{2}[T ][0-9]{2}:[0-9]{2}:[0-9]{2}(Z|[A-Z]+)?",
    "<timestamp>",
    lines
  )
  # Toolchain versions.
  lines <- gsub("\\bR version [0-9.]+.*", "R version <version>", lines)
  lines <- gsub("\\bCmdStan [0-9.]+", "CmdStan <version>", lines)
  lines <- gsub("\\bcmdstanr [0-9.]+", "cmdstanr <version>", lines)
  # Execution timings.
  lines <- gsub(
    "[0-9]+([.][0-9]+)? (seconds|secs|ms)\\b",
    "<seconds>",
    lines
  )
  # Session temporary paths.
  lines <- gsub("(/tmp/|/var/folders/)[^[:space:]]+", "<tmp-path>", lines)
  lines <- gsub("[A-Za-z0-9_/]*Rtmp[A-Za-z0-9]+", "<tmp-path>", lines)
  trimws(lines)
}

# Canonical logical lines for markdown comparison: paragraphs reflowed,
# code fences kept verbatim with their info string normalized, table rows
# whitespace-collapsed. This makes the README comparison insensitive to
# pandoc's line wrapping, fence spelling, and table padding while still
# catching any content drift.
canonicalize_markdown <- function(lines) {
  lines <- sub("[[:space:]]+$", "", lines)
  out <- character()
  buffer <- character()
  flush_buffer <- function() {
    if (length(buffer)) {
      joined <- trimws(gsub("[[:space:]]+", " ", paste(buffer, collapse = " ")))
      if (nzchar(joined)) {
        out <<- c(out, joined)
      }
      buffer <<- character()
    }
  }
  in_fence <- FALSE
  for (line in lines) {
    if (in_fence) {
      if (grepl("^```", line)) {
        in_fence <- FALSE
        out <- c(out, "```")
      } else {
        out <- c(out, line)
      }
      next
    }
    if (grepl("^```", line)) {
      flush_buffer()
      info <- tolower(trimws(sub(
        "^```[[:space:]]*([^[:space:]]*)",
        "\\1",
        line
      )))
      if (identical(info, "text")) {
        info <- ""
      }
      out <- c(out, paste0("```", info))
      in_fence <- TRUE
      next
    }
    if (grepl("^[[:space:]]*\\|", line)) {
      flush_buffer()
      row <- trimws(gsub("[[:space:]]+", " ", line))
      if (grepl("^[[:space:]:|-]+$", row) && grepl("-", row, fixed = TRUE)) {
        row <- gsub("[[:space:]]", "", gsub("-{2,}", "-", row))
      }
      out <- c(out, row)
      next
    }
    if (!nzchar(trimws(line))) {
      flush_buffer()
      next
    }
    buffer <- c(buffer, line)
  }
  flush_buffer()
  out
}

# Small unified diff over lines (Myers not required at these sizes).
unified_diff <- function(a, b, context = 3L) {
  n <- length(a)
  m <- length(b)
  lcs <- matrix(0L, nrow = n + 1L, ncol = m + 1L)
  for (i in seq(n, 1)) {
    for (j in seq(m, 1)) {
      lcs[i, j] <- if (identical(a[[i]], b[[j]])) {
        lcs[i + 1L, j + 1L] + 1L
      } else {
        max(lcs[i + 1L, j], lcs[i, j + 1L])
      }
    }
  }
  ops <- list()
  i <- 1L
  j <- 1L
  while (i <= n && j <= m) {
    if (identical(a[[i]], b[[j]])) {
      ops[[length(ops) + 1L]] <- c(eq = 0L, i, j)
      i <- i + 1L
      j <- j + 1L
    } else if (lcs[i + 1L, j] >= lcs[i, j + 1L]) {
      ops[[length(ops) + 1L]] <- c(del = -1L, i, j)
      i <- i + 1L
    } else {
      ops[[length(ops) + 1L]] <- c(add = 1L, i, j)
      j <- j + 1L
    }
  }
  while (i <= n) {
    ops[[length(ops) + 1L]] <- c(del = -1L, i, j)
    i <- i + 1L
  }
  while (j <= m) {
    ops[[length(ops) + 1L]] <- c(add = 1L, i, j)
    j <- j + 1L
  }
  changed <- vapply(ops, function(op) op[[1]] != 0L, logical(1))
  if (!any(changed)) {
    return(character())
  }
  first <- min(which(changed))
  last <- max(which(changed))
  lo <- max(1L, first - context)
  hi <- min(length(ops), last + context)
  window <- ops[lo:hi]
  header_a <- window[[1]][[2]]
  header_b <- window[[1]][[3]]
  count_a <- sum(vapply(window, function(op) op[[1]] != 1L, logical(1)))
  count_b <- sum(vapply(window, function(op) op[[1]] != -1L, logical(1)))
  out <- c(
    "```diff",
    sprintf("@@ -%d,%d +%d,%d @@", header_a, count_a, header_b, count_b)
  )
  for (op in ops[lo:hi]) {
    kind <- op[[1]]
    text <- if (kind == -1L) a[[op[[2]]]] else b[[op[[3]]]]
    prefix <- if (kind == 0L) {
      " "
    } else if (kind == -1L) {
      "-"
    } else {
      "+"
    }
    out <- c(out, paste0(prefix, text))
  }
  c(out, "```")
}

failures <- character()
stale <- character()
record_failure <- function(...) {
  failures <<- c(failures, ...)
}
record_stale <- function(...) {
  stale <<- c(stale, ...)
}

# --------------------------------------------------------------------------
# Install the checkout into a temporary library, mirroring
# tools/precompute-vignettes.R, so chunk verification runs against this
# checkout's data and code.
# --------------------------------------------------------------------------

temp_lib <- tempfile("bg-check-lib-")
dir.create(temp_lib)
parent_libs <- .libPaths()
status <- system2(
  file.path(R.home("bin"), "R"),
  c(
    "CMD",
    "INSTALL",
    "--no-multiarch",
    "--with-keep.source",
    "-l",
    shQuote(temp_lib),
    "."
  ),
  env = paste0(
    "R_LIBS=",
    paste(parent_libs, collapse = .Platform$path.sep)
  )
)
if (!identical(status, 0L)) {
  stop("Temporary package installation failed")
}
.libPaths(c(temp_lib, .libPaths()))
suppressPackageStartupMessages(library(bayesgrove))
cat("bayesgrove:", as.character(packageVersion("bayesgrove")), "\n\n")

# --------------------------------------------------------------------------
# 1. README.md freshness
# --------------------------------------------------------------------------

cat("README.md: ")
if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("rmarkdown is required to verify README.md")
}
render_dir <- tempfile("bg-readme-")
dir.create(render_dir)
rendered <- rmarkdown::render(
  file.path(source_dir, "README.Rmd"),
  output_format = rmarkdown::github_document(html_preview = FALSE),
  output_file = "README.md",
  output_dir = render_dir,
  intermediates_dir = render_dir,
  quiet = TRUE
)
checked <- canonicalize_markdown(readLines(
  file.path(source_dir, "README.md"),
  warn = FALSE
))
fresh <- canonicalize_markdown(readLines(rendered, warn = FALSE))
if (identical(checked, fresh)) {
  cat("matches a fresh render\n")
} else {
  cat("STALE (content differs from a fresh render)\n")
  record_stale(
    "README.md is stale: re-render with",
    "  Rscript tools/check-captured-output.R --refresh",
    unified_diff(checked, fresh)
  )
}
if (refresh) {
  file.copy(rendered, file.path(source_dir, "README.md"), overwrite = TRUE)
  cat("README.md refreshed from README.Rmd\n")
}

# --------------------------------------------------------------------------
# 2. Deterministic chunks inside the precomputed vignettes
# --------------------------------------------------------------------------

for (entry in checkable_chunks) {
  doc <- entry$doc
  cat(sprintf("%s.Rmd chunk %s: ", doc, entry$label))
  lines <- read_doc(doc)
  chunk <- find_chunk(lines, entry$label)
  if (is.null(chunk)) {
    stop(sprintf("Chunk %s not found in %s.Rmd", entry$label, doc))
  }
  block <- block_after_chunk(lines, chunk)
  if (is.null(block)) {
    stop(sprintf(
      "No captured output block follows chunk %s in %s.Rmd",
      entry$label,
      doc
    ))
  }
  code <- lines[seq.int(chunk[[1]] + 1L, chunk[[2]] - 1L)]
  checked_block <- normalize_lines(lines[seq.int(
    block[[1]] + 1L,
    block[[2]] - 1L
  )])
  env <- new.env(parent = globalenv())
  fresh_block <- character()
  tryCatch(
    {
      fresh_block <- capture.output({
        for (expression in parse(text = code)) {
          value <- withVisible(eval(expression, envir = env))
          if (value$visible) {
            print(value$value)
          }
        }
      })
    },
    error = function(e) {
      stop(sprintf("Chunk %s failed: %s", entry$label, conditionMessage(e)))
    }
  )
  fresh_block <- normalize_lines(fresh_block)
  if (identical(checked_block, fresh_block)) {
    cat("matches re-evaluated output\n")
  } else {
    cat("STALE (captured output differs from re-evaluation)\n")
    record_stale(
      sprintf("%s.Rmd chunk %s is stale:", doc, entry$label),
      "  refresh with Rscript tools/check-captured-output.R --refresh",
      unified_diff(checked_block, fresh_block)
    )
  }
  if (refresh) {
    lines <- c(
      lines[seq_len(block[[1]])],
      fresh_block,
      lines[seq.int(block[[2]], length(lines))]
    )
    writeLines(lines, doc_path(doc))
    cat(sprintf("refreshed chunk %s in %s.Rmd\n", entry$label, doc))
  }
}

# --------------------------------------------------------------------------
# 3. Policy guards
# --------------------------------------------------------------------------

# Every vignette must be classified, so new documents join the policy
# deliberately instead of silently.
known <- c(live_docs, names(precomputed_docs))
found <- list.files(
  file.path(source_dir, "vignettes"),
  pattern = "[.]Rmd$",
  recursive = TRUE
)
found <- sub("[.]Rmd$", "", found)
unclassified <- setdiff(found, known)
if (length(unclassified)) {
  record_failure(
    "Unclassified vignettes (add them to live_docs or precomputed_docs in",
    "tools/check-captured-output.R):",
    paste0("  - ", unclassified)
  )
}

# Precomputed vignettes must point readers at the manual runner.
for (doc in names(precomputed_docs)) {
  if (!any(grepl("tools/precompute-vignettes[.]R", read_doc(doc)))) {
    record_failure(
      sprintf(
        "%s.Rmd is precomputed but its marker comment does not mention",
        doc
      ),
      "tools/precompute-vignettes.R; add the comment so the refresh path",
      "stays discoverable."
    )
  }
}

# Live documents must not accumulate pasted output; their chunks evaluate at
# build time, so pasted blocks there would be silent drift.
captured_pattern <- "^[[:space:]]*([#][>]|\\[[0-9]+\\]|named (list|character)\\(|<bg_)"
for (doc in live_docs) {
  lines <- read_doc(doc)
  starts <- grep("^```\\{r(?:[ ,}])", lines, perl = TRUE)
  for (start in starts) {
    end <- start + which(lines[(start + 1L):length(lines)] == "```")[1L]
    block <- block_after_chunk(lines, c(start, end))
    if (
      !is.null(block) &&
        grepl(captured_pattern, lines[[block[[1]] + 1L]])
    ) {
      record_failure(
        sprintf(
          "%s.Rmd chunk at line %d is followed by pasted output, but the",
          doc,
          start
        ),
        "document is live-evaluated. Either let the chunk print its own",
        "output or move the document to precomputed_docs (manual refresh",
        "via tools/precompute-vignettes.R)."
      )
    }
  }
}

# --------------------------------------------------------------------------
# Ledger and exit
# --------------------------------------------------------------------------

cat("\nPolicy ledger\n")
cat(
  "  verified: README.md; deterministic chunks:",
  paste0(
    vapply(
      checkable_chunks,
      function(e) sprintf("%s#%s", e$doc, e$label),
      character(1)
    ),
    collapse = ", "
  ),
  "\n"
)
cat("  excluded (manual refresh via tools/precompute-vignettes.R):\n")
for (doc in names(precomputed_docs)) {
  cat(sprintf("    - %s: %s\n", doc, precomputed_docs[[doc]]))
}

if (refresh) {
  cat(
    "\nRefresh mode: deterministic output rewritten; stochastic",
    "precomputed blocks were not touched.\n"
  )
}

if (length(stale)) {
  if (refresh) {
    cat("\nRefreshed stale deterministic output (see above).\n")
  } else {
    failures <- c(failures, stale)
  }
}

if (length(failures)) {
  cat("\nCaptured output is stale or the policy is violated:\n\n")
  cat(paste0(failures, collapse = "\n"), "\n\n")
  quit(status = 1L)
}

cat("\nAll captured documentation output is current.\n")
