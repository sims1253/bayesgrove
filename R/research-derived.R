# Candidate bindings and run-derived evidence --------------------------------
#
# A research candidate can be bound to the execution that produced a node's
# current artifact, and evidence can be derived from executions instead of
# asserted. Everything here is engine-owned: it is derived during propose and
# again during apply, under the research lock, and a different derivation
# makes the proposal stale. Identity is carried by content addresses and the
# execution records of R/provenance.R, never by the current graph. See
# architecture/rules-and-evidence.md, section 2.

#' Field names a public research action may never supply.
#' @keywords internal
#' @noRd
bg_research_engine_owned <- function() {
  c(
    "binding",
    "provenance",
    "pad",
    "digest",
    "digests",
    "fingerprint",
    "fingerprints",
    "execution_fingerprint",
    "artifact_ref",
    "artifact_refs",
    "subject_artifact_ref",
    "job_id",
    "job_ids",
    "summary_id",
    "summary_ids",
    "research_version"
  )
}

#' Reject engine-owned fields in a public action.
#'
#' Checks the action's own fields and the fields of each supplied P/A/D
#' component, where a copied snapshot would carry its `digest`.
#' @keywords internal
#' @noRd
bg_research_reject_engine_owned <- function(action) {
  owned <- bg_research_engine_owned()
  found <- intersect(names(action), owned)
  if (is.list(action$components)) {
    for (key in names(action$components)) {
      component <- action$components[[key]]
      hits <- if (is.list(component)) intersect(names(component), owned)
      if (length(hits)) {
        found <- c(found, paste0("components.", key, ".", hits))
      }
    }
  }
  if (length(found)) {
    cli::cli_abort(c(
      "Research actions cannot supply engine-owned field{?s} {.val {found}}.",
      "i" = "Supply {.field node_id}; the engine derives bindings, provenance, and digests from the execution."
    ))
  }
  invisible(action)
}

#' Node kinds whose executions report no subject and are not yet supported
#' for derived evidence.
#' @keywords internal
#' @noRd
bg_research_underived_kinds <- function() c("sbc", "compare")

#' Resolve a node to its representative execution for research.
#'
#' Wraps `bg_resolve_node_execution()` with the errors a research action
#' reports. Pre-read `fingerprints`, `index`, and `jobs` may be passed so one
#' derivation reads each source once.
#' @return The resolution (see `bg_resolve_node_execution()`), plus `kind`.
#' @keywords internal
#' @noRd
bg_research_resolve_node <- function(
  project,
  node_id,
  fingerprints = NULL,
  index = NULL,
  jobs = NULL
) {
  if (is.null(project)) {
    cli::cli_abort("Resolving {.field node_id} requires the project handle.")
  }
  bg_research_string(node_id, "node_id")
  graph <- bg_read_graph(project)
  node <- graph$nodes[[node_id]]
  if (is.null(node)) {
    cli::cli_abort("Node {.val {node_id}} not found in graph.")
  }
  execution <- bg_resolve_node_execution(
    project,
    node_id,
    predicted_fingerprints = fingerprints,
    artifact_index = index,
    jobs = jobs
  )
  if (is.null(execution)) {
    cli::cli_abort(c(
      "Node {.val {node_id}} has no fresh artifact with recorded execution provenance.",
      "i" = "Run it with {.fn bg_run}, or invalidate and re-run it with {.code bg_run(project, rerun = \"{node_id}\")}."
    ))
  }
  execution$kind <- node$kind
  execution
}

#' Derive a candidate binding from a node.
#'
#' The node resolves to its representative execution, which must carry a
#' P/A/D snapshot. `research_version` is the version the action creates, so
#' the binding refers to the history entry that records it.
#' @keywords internal
#' @noRd
bg_research_binding <- function(project, state, node_id) {
  execution <- bg_research_resolve_node(project, node_id)
  provenance <- execution$job$provenance
  if (is.null(provenance$pad)) {
    reason <- provenance$capture_error %||%
      sprintf(
        paste(
          "kind '%s' declares no complete P/A/D classification of the node's",
          "parameters"
        ),
        execution$kind
      )
    cli::cli_abort(c(
      "The execution behind node {.val {node_id}} recorded no P/A/D snapshot, so a candidate cannot be bound to it.",
      "x" = "{reason}",
      "i" = "Declare {.arg pad} for the kind with {.fn bg_register_node_kind}, then re-run the node."
    ))
  }
  list(
    node_id = node_id,
    job_id = execution$job$job_id,
    artifact_ref = execution$artifact_ref,
    pad = provenance$pad,
    research_version = state$version + 1L
  )
}

#' The P/A/D components a bound create defaults to: the snapshot without
#' digests.
#' @keywords internal
#' @noRd
bg_research_pad_components <- function(pad) {
  bg_research_data(lapply(pad[c("P", "A", "D")], function(component) {
    component[setdiff(names(component), "digest")]
  }))
}

#' Components whose snapshot digests differ between two P/A/D snapshots.
#' @keywords internal
#' @noRd
bg_research_pad_diff <- function(pad, other) {
  Filter(
    function(key) !identical(pad[[key]]$digest, other[[key]]$digest),
    c("P", "A", "D")
  )
}

#' Derive the components and `changed` of a prospective candidate.
#'
#' Implements the transition table of create and revise. `parent` is `NULL`
#' for a create; `binding` is `NULL` when the action supplies no `node_id`.
#' Bindings are never inherited from the parent.
#' @return `list(components, changed)`.
#' @keywords internal
#' @noRd
bg_research_revision <- function(parent, action, binding) {
  supplied <- action$components
  if (is.null(supplied)) {
    if (is.null(binding)) {
      cli::cli_abort(
        "Supply {.field components}, or {.field node_id} to bind the candidate to a run."
      )
    }
    if (is.null(parent)) {
      supplied <- bg_research_pad_components(binding$pad)
    }
  }
  prior <- bg_research_data(parent$components %||% list())
  components <- if (is.null(supplied)) {
    prior
  } else {
    bg_research_components(supplied, prior)
  }
  described <- Filter(
    function(key) !identical(components[[key]], prior[[key]]),
    names(supplied)
  )
  if (is.null(parent)) {
    return(list(components = components, changed = as.list(described)))
  }
  if (is.null(binding)) {
    if (identical(components, prior)) {
      cli::cli_abort(
        "A revision must change P, A, or D. Record a note or gather evidence instead."
      )
    }
    return(list(components = components, changed = as.list(described)))
  }
  if (is.null(parent$binding)) {
    # Adding a binding is itself the change, so the description may be equal.
    return(list(components = components, changed = as.list(described)))
  }
  changed <- union(
    bg_research_pad_diff(binding$pad, parent$binding$pad),
    described
  )
  if (length(changed) == 0L) {
    cli::cli_abort(c(
      "A revision must change P, A, or D.",
      "x" = "Node {.val {binding$node_id}} has the same P, A, and D snapshot digests as the parent's binding, and the description is unchanged.",
      "i" = "Record a note or gather evidence instead."
    ))
  }
  list(
    components = components,
    changed = as.list(sort(changed, method = "radix"))
  )
}

#' The consumed artifact of an execution's reported subject, or `NULL`.
#' @keywords internal
#' @noRd
bg_research_subject_ref <- function(provenance) {
  subject <- provenance$consumed$subject
  if (!is.character(subject) || length(subject) != 1L) {
    return(NULL)
  }
  for (input in provenance$inputs %||% list()) {
    if (identical(input$node_id, subject)) {
      return(input$artifact_ref)
    }
  }
  NULL
}

#' The P/A/D snapshot of the latest execution that produced an artifact.
#' @keywords internal
#' @noRd
bg_research_artifact_pad <- function(jobs, artifact_ref) {
  producing <- Filter(
    function(job) {
      identical(job$status, "succeeded") &&
        identical(job[["provenance"]]$artifact_ref, artifact_ref)
    },
    jobs
  )
  if (length(producing) == 0L) {
    return(NULL)
  }
  seqs <- vapply(
    producing,
    function(job) as.numeric(job$seq %||% 0),
    numeric(1)
  )
  producing[[order(seqs)[[length(seqs)]]]]$provenance$pad
}

#' Explain how an unrelated execution differs from the bound one.
#' @keywords internal
#' @noRd
bg_research_mismatch_detail <- function(jobs, artifact_ref, binding) {
  pad <- bg_research_artifact_pad(jobs, artifact_ref)
  if (is.null(pad)) {
    return(NULL)
  }
  differing <- bg_research_pad_diff(pad, binding$pad)
  if (length(differing) == 0L) {
    return(
      "Its P, A, and D digests equal the binding's, but its output differs."
    )
  }
  sprintf(
    "It differs from the bound execution in %s.",
    paste(differing, collapse = ", ")
  )
}

#' Derive evidence from a node for a bound candidate.
#'
#' Eligibility is decided by content identity, in order:
#' - check: the representative execution reports a subject whose consumed
#'   artifact is the binding's artifact; only that execution is eligible;
#' - self: the node's resolved artifact is the binding's artifact; the bound
#'   job and every succeeded job with the same output and fingerprint are
#'   eligible.
#' Summaries are matched by their engine-stamped `job_id`.
#' @return `list(check, result, provenance)`.
#' @keywords internal
#' @noRd
bg_research_derive_evidence <- function(project, candidate, action) {
  binding <- candidate$binding
  if (is.null(binding)) {
    cli::cli_abort(c(
      "Candidate {.val {candidate$id}} is not bound to a run, so evidence cannot be derived for it.",
      "i" = "Bind a candidate with {.field node_id} on {.val create} or {.val revise}, or record asserted evidence with {.field result}."
    ))
  }
  node_id <- action$node_id
  if (is.null(project)) {
    cli::cli_abort("Resolving {.field node_id} requires the project handle.")
  }
  bg_research_string(node_id, "node_id")
  kind <- bg_read_graph(project)$nodes[[node_id]]$kind
  if (!is.null(kind) && kind %in% bg_research_underived_kinds()) {
    cli::cli_abort(c(
      "Derived evidence from {.val {kind}} nodes is not supported yet.",
      "i" = "An {.val sbc} node fits its own model to simulated data and a {.val compare} node covers several models, so neither reports a subject. Record asserted evidence with {.field result} instead."
    ))
  }
  fingerprints <- bg_predicted_fingerprints(project)
  index <- bg_read_artifact_index(project)
  jobs <- bg_jobs(project)
  execution <- bg_research_resolve_node(
    project,
    node_id,
    fingerprints = fingerprints,
    index = index,
    jobs = jobs
  )
  representative <- execution$job
  subject_ref <- bg_research_subject_ref(representative$provenance)
  if (identical(subject_ref, binding$artifact_ref)) {
    eligible <- representative$job_id
  } else if (identical(execution$artifact_ref, binding$artifact_ref)) {
    bound <- jobs[[binding$job_id]]
    fingerprint <- bound[["provenance"]]$fingerprint
    same <- if (is.null(fingerprint)) {
      list()
    } else {
      bg_execution_jobs(project, binding$artifact_ref, fingerprint, jobs = jobs)
    }
    eligible <- unique(c(
      binding$job_id,
      vapply(same, `[[`, character(1), "job_id")
    ))
  } else {
    detail <- if (is.null(subject_ref)) {
      bg_research_mismatch_detail(jobs, execution$artifact_ref, binding)
    } else {
      bg_research_mismatch_detail(jobs, subject_ref, binding)
    }
    cli::cli_abort(c(
      if (is.null(subject_ref)) {
        "Node {.val {node_id}} neither evaluated nor produced the fit bound to candidate {.val {candidate$id}}."
      } else {
        "Node {.val {node_id}} evaluated a different fit than the one bound to candidate {.val {candidate$id}}."
      },
      "x" = if (is.null(subject_ref)) {
        "It resolves to {.val {execution$artifact_ref}}; the binding is {.val {binding$artifact_ref}} from node {.val {binding$node_id}}."
      } else {
        "Its subject was {.val {subject_ref}}; the binding is {.val {binding$artifact_ref}} from node {.val {binding$node_id}}."
      },
      "i" = detail
    ))
  }

  summaries <- Filter(
    function(summary) {
      is.character(summary$job_id) && summary$job_id %in% eligible
    },
    bg_read_summaries(
      project,
      predicted_fingerprints = fingerprints,
      artifact_index = index
    )
  )
  kinds <- unique(vapply(summaries, `[[`, character(1), "summary_kind"))
  check <- action$check
  if (is.null(check)) {
    if (length(kinds) == 0L) {
      cli::cli_abort(
        "The eligible execution{?s} of node {.val {node_id}} wrote no summaries."
      )
    }
    if (length(kinds) > 1L) {
      cli::cli_abort(c(
        "Supply {.field check}: the eligible executions of node {.val {node_id}} wrote several summary kinds.",
        "i" = "Summary kinds: {.val {kinds}}."
      ))
    }
    check <- kinds
  }
  bg_research_string(check, "check")
  selected <- Filter(
    function(summary) identical(summary$summary_kind, check),
    summaries
  )
  if (length(selected) == 0L) {
    cli::cli_abort(c(
      "The eligible executions of node {.val {node_id}} wrote no {.val {check}} summary.",
      "i" = if (length(kinds)) {
        "Summary kinds: {.val {kinds}}."
      } else {
        "They wrote no summaries."
      }
    ))
  }
  # Summaries are read in log order, so the last match is the most recent.
  summary <- selected[[length(selected)]]
  list(
    check = check,
    result = list(
      summary_kind = summary$summary_kind,
      severity = summary$severity,
      passed = summary$passed,
      metrics = summary$metrics %||% bg_empty_named_list()
    ),
    provenance = list(
      kind = "derived",
      node_id = node_id,
      job_id = summary$job_id,
      artifact_ref = execution$artifact_ref,
      subject_artifact_ref = binding$artifact_ref,
      summary_ids = list(summary$summary_id)
    )
  )
}

#' Validate the stored binding of a candidate.
#' @keywords internal
#' @noRd
bg_research_validate_binding <- function(binding) {
  if (is.null(binding)) {
    return(invisible(NULL))
  }
  fields <- c("node_id", "job_id", "artifact_ref", "pad", "research_version")
  bg_research_fields(binding, fields, fields, "binding")
  for (field in c("node_id", "job_id", "artifact_ref")) {
    bg_research_string(binding[[field]], paste0("binding.", field))
  }
  if (
    !is.list(binding$pad) ||
      !is.numeric(binding$research_version) ||
      length(binding$research_version) != 1L
  ) {
    cli::cli_abort("Candidate binding is malformed.")
  }
  invisible(NULL)
}

#' Validate the structure of stored derived evidence.
#'
#' Evidence without `provenance` is asserted and is not inspected. Derived
#' evidence must have the stored shape, belong to a bound candidate, and name
#' that binding's artifact as its subject. This catches malformed state on
#' read, independent of the graph and registries. It does not authenticate a
#' fabricated file.
#' @keywords internal
#' @noRd
bg_research_validate_evidence <- function(state) {
  evidence <- state$evidence
  if (length(evidence) && is.null(names(evidence))) {
    cli::cli_abort("Evidence must be named by its IDs.")
  }
  scalar_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  provenance_fields <- c(
    "kind",
    "node_id",
    "job_id",
    "artifact_ref",
    "subject_artifact_ref",
    "summary_ids"
  )
  result_fields <- c("summary_kind", "severity", "passed", "metrics")
  for (id in names(evidence)) {
    record <- evidence[[id]]
    provenance <- if (is.list(record)) record$provenance
    if (is.null(provenance)) {
      next
    }
    bg_research_fields(
      provenance,
      provenance_fields,
      provenance_fields,
      "provenance"
    )
    if (
      !identical(provenance$kind, "derived") ||
        !all(vapply(
          provenance[setdiff(provenance_fields, "summary_ids")],
          scalar_string,
          logical(1)
        )) ||
        !is.list(provenance$summary_ids) ||
        length(provenance$summary_ids) == 0L ||
        !all(vapply(provenance$summary_ids, scalar_string, logical(1)))
    ) {
      cli::cli_abort("Derived provenance of evidence {.val {id}} is malformed.")
    }
    result <- record$result
    bg_research_fields(result, result_fields, result_fields, "result")
    passed <- result$passed
    if (
      !scalar_string(result$summary_kind) ||
        !scalar_string(result$severity) ||
        !(is.null(passed) || (is.logical(passed) && length(passed) == 1L)) ||
        !is.list(result$metrics) ||
        !identical(record$check, result$summary_kind)
    ) {
      cli::cli_abort("Derived result of evidence {.val {id}} is malformed.")
    }
    candidate_id <- record$candidate_id
    binding <- if (scalar_string(candidate_id)) {
      state$candidates[[candidate_id]]$binding
    }
    if (
      is.null(binding) ||
        !identical(provenance$subject_artifact_ref, binding$artifact_ref)
    ) {
      cli::cli_abort(
        "Derived evidence {.val {id}} must name the artifact its candidate is bound to."
      )
    }
  }
  invisible(NULL)
}
