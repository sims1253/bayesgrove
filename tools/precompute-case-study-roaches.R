# Precompute the roaches case-study vignette outputs.
#
# Runs every R chunk from vignettes/case-study-roaches.Rmd, in order, and
# emits output under `<<<CHUNK name>>>` markers. The package is installed into
# a temporary library first so system.file() sees inst/ content and CmdStan
# binaries are compiled outside the source tree.

source_dir <- normalizePath(".")
lib <- tempfile("bg-roaches-lib-")
dir.create(lib)

status <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-multiarch", "--with-keep.source", "-l", lib, ".")
)
if (!identical(status, 0L)) {
  stop("Temporary package installation failed")
}

.libPaths(c(lib, .libPaths()))

cat(
  "bayesgrove package version:",
  as.character(packageVersion("bayesgrove")),
  "\n"
)
cat("cmdstanr version:", as.character(packageVersion("cmdstanr")), "\n")
cat("CmdStan version:", as.character(cmdstanr::cmdstan_version()), "\n")

lines <- readLines(file.path(source_dir, "vignettes", "case-study-roaches.Rmd"))
starts <- grep("^```\\{r(?:[ ,}])", lines)
headers <- lines[starts]
labels <- sub("^```\\{r[[:space:]]*([^,}]*)[^{]*$", "\\1", headers)
required_sequence <- c(
  "loo-review",
  "loo-review-branches",
  "comparison-decision",
  "dispositions"
)
positions <- match(required_sequence, labels)
if (anyNA(positions) || is.unsorted(positions, strictly = TRUE)) {
  stop("Roaches precompute chunk sequence is out of sync with the vignette")
}

vignette_text <- paste(lines, collapse = "\n")
required_code <- c(
  "print(bg_result(handle, n_compare))",
  'bg_next_actions(handle, scope = "project")$obligations',
  'bg_next_actions(handle, scope = "branch", branch_id = nb$branch_id)$obligations',
  'bg_next_actions(handle, scope = "branch", branch_id = zinb$branch_id)$obligations'
)
if (
  !all(vapply(
    required_code,
    grepl,
    logical(1),
    x = vignette_text,
    fixed = TRUE
  ))
) {
  stop("Roaches precompute output contract is out of sync with the vignette")
}
env <- new.env(parent = globalenv())
report_path <- NULL

for (start in starts) {
  end <- start + which(lines[(start + 1L):length(lines)] == "```")[1L]
  header <- lines[[start]]
  label <- sub("^```\\{r[[:space:]]*([^,}]*)[^{]*$", "\\1", header)
  if (!nzchar(label)) {
    label <- "unnamed"
  }
  code <- lines[(start + 1L):(end - 1L)]

  cat(sprintf("<<<CHUNK %s>>>\n", label))
  figure <- label %in% c("plot-poisson", "plot-nb")
  if (figure) {
    figure_path <- file.path(
      source_dir,
      "vignettes",
      "figures",
      sprintf("case-study-roaches-ppc-%s.png", sub("plot-", "", label))
    )
    dir.create(dirname(figure_path), recursive = TRUE, showWarnings = FALSE)
    png(figure_path, width = 700, height = 500)
  }

  tryCatch(
    {
      last_value <- NULL
      for (expression in parse(text = code)) {
        value <- withVisible(eval(expression, envir = env))
        last_value <- value$value
        if (value$visible) {
          print(value$value)
        }
      }
    },
    error = function(e) {
      if (figure && dev.cur() > 1L) {
        dev.off()
      }
      stop(sprintf("Chunk %s failed: %s", label, conditionMessage(e)))
    }
  )
  if (figure) {
    dev.off()
  }

  if (identical(label, "report")) {
    report_path <- as.character(last_value)
  }
}

if (!is.null(report_path) && file.exists(report_path)) {
  report <- readLines(report_path)
  decision_start <- grep("^##+ Decision", report, ignore.case = TRUE)[1L]
  if (is.na(decision_start)) {
    stop("Could not find decision-trail section in exported report")
  }
  next_section <- grep("^## ", report)
  next_section <- next_section[next_section > decision_start][1L]
  decision_end <- if (is.na(next_section)) length(report) else next_section - 1L
  excerpt_end <- min(decision_start + 14L, decision_end)
  cat("<<<CHUNK report-excerpt>>>\n")
  cat(report[decision_start:excerpt_end], sep = "\n")
  cat("\n")
}
