#' @title bayesgrove Project Handle
#' @description A reference-semantic handle for an open bayesgrove project.
#'
#' # Decision record: S7 + environment-backed state
#'
#' `bg_handle` is an S7 class with an environment-backed `.state` slot for
#' the jobs cache and registries. This provides reference semantics with S7
#' validation. Keep this design: replacing it with R6 or plain environments
#' would remove the S7 validation checks.
#'
#' @param project_id Unique project identifier.
#' @param path File path to the project root.
#' @param readonly Logical, whether the handle is readonly.
#' @param closed Logical, whether the handle is closed.
#' @param loaded_graph_version Integer, current loaded graph version.
#' @param lock_token Character or NA, current lock token.
#' @param registries List of runtime registries.
#' @param metadata List of project metadata.
#' @export
bg_handle <- S7::new_class(
  "bg_handle",
  properties = list(
    .state = S7::class_environment,
    project_id = S7::new_property(
      S7::class_character,
      getter = function(self) self@.state$project_id,
      setter = function(self, value) {
        self@.state$project_id <- value
        self
      }
    ),
    path = S7::new_property(
      S7::class_character,
      getter = function(self) self@.state$path,
      setter = function(self, value) {
        self@.state$path <- value
        self
      }
    ),
    readonly = S7::new_property(
      S7::class_logical,
      getter = function(self) self@.state$readonly,
      setter = function(self, value) {
        self@.state$readonly <- value
        self
      }
    ),
    closed = S7::new_property(
      S7::class_logical,
      getter = function(self) self@.state$closed,
      setter = function(self, value) {
        self@.state$closed <- value
        self
      }
    ),
    loaded_graph_version = S7::new_property(
      S7::class_integer,
      getter = function(self) self@.state$loaded_graph_version,
      setter = function(self, value) {
        self@.state$loaded_graph_version <- value
        self
      }
    ),
    lock_token = S7::new_property(
      S7::class_character,
      getter = function(self) self@.state$lock_token,
      setter = function(self, value) {
        self@.state$lock_token <- value
        self
      }
    ),
    registries = S7::new_property(
      S7::class_list,
      getter = function(self) self@.state$registries,
      setter = function(self, value) {
        self@.state$registries <- value
        self
      }
    ),
    metadata = S7::new_property(
      S7::class_list,
      getter = function(self) self@.state$metadata,
      setter = function(self, value) {
        self@.state$metadata <- value
        self
      }
    )
  ),
  constructor = function(
    project_id,
    path,
    readonly = FALSE,
    closed = FALSE,
    loaded_graph_version = 0L,
    lock_token = NA_character_,
    registries = list(),
    metadata = list()
  ) {
    env <- new.env(parent = emptyenv())
    env$project_id <- project_id
    env$path <- path
    env$readonly <- readonly
    env$closed <- closed
    env$loaded_graph_version <- as.integer(loaded_graph_version)
    env$lock_token <- lock_token
    env$registries <- registries
    env$metadata <- metadata

    S7::new_object(S7::S7_object(), .state = env)
  }
)

#' @export
print.bg_handle <- function(x, ...) {
  cli::cli_text(cli::col_grey("<bg_handle>"))
  cli::cli_bullets(c(
    "*" = "Project ID: {.val {x@project_id}}",
    "*" = "Path: {.path {x@path}}",
    "*" = "Readonly: {.val {x@readonly}}",
    "*" = "Closed: {.val {x@closed}}",
    "*" = "Graph Version: {.val {x@loaded_graph_version}}"
  ))
  invisible(x)
}
