# Shared Workflow Pack Obligation and Action Constructors
# -------------------------------------------------------
# Shared obligation and action constructors for all workflow packs.

# Packs only see structural node kinds, so fit-likeness follows the `*_fit`
# naming convention; prior-predictive fits are excluded from inference candidacy.
bg_pack_is_fit_node <- function(node) {
  kind <- node$kind %||% ""
  grepl("(^|_)fit$", kind) && !grepl("prior", kind, fixed = TRUE)
}

bg_pack_obligation <- function(
  context,
  kind,
  title,
  why,
  severity = "blocking",
  basis = list(),
  metadata = list()
) {
  metadata <- utils::modifyList(
    list(hold_node_ids = character(), source_keys = character()),
    metadata %||% list()
  )

  basis <- bg_protocol_normalize_value(utils::modifyList(
    list(
      node_ids = character(),
      summary_ids = character(),
      decision_ids = character(),
      branch_ids = character()
    ),
    basis
  ))

  list(
    kind = kind,
    scope = context$scope,
    severity = severity,
    title = title,
    basis = basis,
    explanation = list(
      why = why,
      references = bg_workflow_references(
        source_keys = metadata$source_keys,
        references = metadata$references
      )
    ),
    metadata = metadata[setdiff(names(metadata), "references")]
  )
}

bg_pack_action <- function(
  context,
  kind,
  title,
  why_now,
  basis = list(),
  payload = list(),
  metadata = list()
) {
  metadata <- utils::modifyList(
    list(source_keys = character()),
    metadata %||% list()
  )

  list(
    kind = kind,
    scope = context$scope,
    title = title,
    basis = bg_protocol_normalize_value(basis %||% list()),
    payload = bg_protocol_normalize_value(payload %||% list()),
    explanation = list(
      why_now = why_now,
      references = bg_workflow_references(
        source_keys = metadata$source_keys,
        references = metadata$references
      )
    ),
    metadata = metadata[setdiff(names(metadata), "references")]
  )
}

bg_pack_check_action <- function(
  context,
  obligation,
  title,
  source_node_id = NULL,
  node_kind = "check",
  default_label_prefix,
  why_now,
  metadata = list()
) {
  metadata <- utils::modifyList(
    metadata %||% list(),
    list(
      source_keys = unique(c(
        obligation$metadata$source_keys %||% character(),
        (metadata %||% list())$source_keys %||% character()
      )),
      references = unique(c(
        obligation$explanation$references %||% character(),
        (metadata %||% list())$references %||% character()
      ))
    )
  )

  source_node_id <- source_node_id %||%
    bg_pack_source_node_id(context, obligation$basis$node_ids)
  if (is.null(source_node_id)) {
    return(NULL)
  }

  bg_pack_action(
    context = context,
    kind = "create_node_from_template",
    title = title,
    why_now = why_now,
    basis = list(
      obligation_refs = list(list(
        kind = obligation$kind,
        scope = obligation$scope
      )),
      node_ids = obligation$basis$node_ids %||% source_node_id,
      summary_ids = obligation$basis$summary_ids %||% character()
    ),
    payload = list(
      template_ref = "diagnostic_check",
      source_node_id = source_node_id,
      node_kind = node_kind,
      default_label = paste(
        default_label_prefix,
        bg_pack_node_label(context, source_node_id)
      )
    ),
    metadata = metadata
  )
}
