# Run a recorded tutorial from its Rmd source, including eval = FALSE chunks.
# From the repository root:
#   Rscript tools/precompute-vignettes.R eight-schools
#   Rscript tools/precompute-vignettes.R getting-started
#   Rscript tools/precompute-vignettes.R case-study-roaches
#
# Requires cmdstanr and CmdStan. Refresh pasted output from the CHUNK markers,
# check the regenerated figures, and update the vignette's version stamp.
# Output varies with package and sampler versions; this is not a snapshot test.

args <- commandArgs(trailingOnly = TRUE)
vignettes <- c("eight-schools", "getting-started", "case-study-roaches")
if (length(args) != 1L || !args[[1]] %in% vignettes) {
  stop("Choose one vignette: ", paste(vignettes, collapse = ", "))
}
vignette <- args[[1]]
source_dir <- normalizePath(".")

# Install this checkout so system.file() finds its data and Stan programs.
# Compilation stays in the session's temporary library, not the source tree.
lib <- tempfile("bg-vignette-lib-")
dir.create(lib)
status <- system2(
  file.path(R.home("bin"), "R"),
  c(
    "CMD",
    "INSTALL",
    "--no-multiarch",
    "--with-keep.source",
    "-l",
    shQuote(lib),
    "."
  )
)
if (!identical(status, 0L)) {
  stop("Temporary package installation failed")
}
.libPaths(c(lib, .libPaths()))

cat("bayesgrove version:", as.character(packageVersion("bayesgrove")), "\n")
cat("cmdstanr version:", as.character(packageVersion("cmdstanr")), "\n")
cat("CmdStan version:", as.character(cmdstanr::cmdstan_version()), "\n")

lines <- readLines(file.path(source_dir, "vignettes", paste0(vignette, ".Rmd")))
starts <- grep("^```\\{r(?:[ ,}])", lines)
env <- new.env(parent = globalenv())
report_path <- NULL
figures <- switch(
  vignette,
  "getting-started" = c("ppc-plot" = "getting-started-ppc-normal.png"),
  "case-study-roaches" = c(
    "plot-poisson" = "case-study-roaches-ppc-poisson.png",
    "plot-nb" = "case-study-roaches-ppc-nb.png"
  ),
  character()
)

for (start in starts) {
  end <- start + which(lines[(start + 1L):length(lines)] == "```")[1L]
  label <- sub("^```\\{r[[:space:]]*([^,}]*)[^{]*$", "\\1", lines[[start]])
  if (!nzchar(label)) {
    label <- "unnamed"
  }
  code <- lines[seq.int(start + 1L, end - 1L)]

  cat(sprintf("<<<CHUNK %s>>>\n", label))
  figure <- label %in% names(figures)
  if (figure) {
    figure_path <- file.path(
      source_dir,
      "vignettes",
      "figures",
      figures[[label]]
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
      stop(sprintf("Chunk %s failed: %s", label, conditionMessage(e)))
    },
    finally = {
      if (figure) {
        dev.off()
      }
    }
  )
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
