# Protocol schema validation helpers ---------------------------------------------

#' @keywords internal
bg_protocol_named_list <- function(x = list()) {
  if (!is.null(names(x)) || length(x) > 0) {
    return(x)
  }

  stats::setNames(list(), character())
}

#' @keywords internal
bg_protocol_asset_path <- function(...) {
  installed_path <- system.file(..., package = "bayesgrove")
  if (
    nzchar(installed_path) &&
      (file.exists(installed_path) || dir.exists(installed_path))
  ) {
    return(installed_path)
  }

  namespace_path <- tryCatch(
    getNamespaceInfo(asNamespace("bayesgrove"), "path"),
    error = function(...) NULL
  )
  if (is.null(namespace_path) || !nzchar(namespace_path)) {
    return("")
  }

  source_path <- file.path(namespace_path, "inst", ...)
  if (file.exists(source_path) || dir.exists(source_path)) {
    return(source_path)
  }

  ""
}

#' Load the protocol schema index
#'
#' @return Parsed schema index as a list, or NULL if not found.
#' @keywords internal
bg_load_protocol_schema_index <- function() {
  index_path <- bg_protocol_asset_path(
    "protocol",
    "schema-index.json"
  )

  if (!nzchar(index_path) || !file.exists(index_path)) {
    return(NULL)
  }

  jsonlite::read_json(index_path, simplifyVector = FALSE)
}

#' Load a specific protocol schema by name
#'
#' @param schema_name Name of the schema (without .json extension)
#'
#' @return Parsed schema as a list, or NULL if not found.
#' @keywords internal
bg_load_protocol_schema <- function(schema_name) {
  schema_path <- bg_protocol_asset_path(
    "protocol",
    "schemas",
    paste0(schema_name, ".json")
  )

  if (!nzchar(schema_path) || !file.exists(schema_path)) {
    return(NULL)
  }

  jsonlite::read_json(schema_path, simplifyVector = FALSE)
}

#' List available protocol schemas
#'
#' @return Character vector of available schema names.
#' @keywords internal
bg_list_protocol_schemas <- function() {
  schemas_dir <- bg_protocol_asset_path("protocol", "schemas")

  if (!nzchar(schemas_dir) || !dir.exists(schemas_dir)) {
    return(character())
  }

  schema_files <- list.files(
    schemas_dir,
    pattern = "\\.json$",
    full.names = FALSE
  )
  sub("\\.json$", "", schema_files)
}

#' Load the protocol fixture index
#'
#' @return Parsed fixture index as a list, or NULL if not found.
#' @keywords internal
#' @noRd
bg_load_protocol_fixture_index <- function() {
  index_path <- bg_protocol_asset_path(
    "protocol",
    "fixtures",
    "fixture-index.json"
  )

  if (!nzchar(index_path) || !file.exists(index_path)) {
    return(NULL)
  }

  jsonlite::read_json(index_path, simplifyVector = FALSE)
}

#' List available protocol fixtures
#'
#' @param stability Optional stability filter.
#'
#' @return Character vector of fixture names.
#' @keywords internal
#' @noRd
bg_list_protocol_fixtures <- function(stability = NULL) {
  index <- bg_load_protocol_fixture_index()
  if (is.null(index)) {
    return(character())
  }

  fixtures <- index$fixtures %||% list()
  names_out <- names(fixtures)
  if (is.null(stability)) {
    return(names_out %||% character())
  }

  keep <- vapply(
    fixtures,
    function(fixture) identical(fixture$stability %||% NULL, stability),
    logical(1)
  )
  names_out[keep]
}

#' Load a specific protocol fixture by name
#'
#' @param fixture_name Fixture name from `fixture-index.json`.
#'
#' @return Parsed fixture payload, or NULL if not found.
#' @keywords internal
#' @noRd
bg_load_protocol_fixture <- function(fixture_name) {
  index <- bg_load_protocol_fixture_index()
  if (is.null(index)) {
    return(NULL)
  }

  fixture <- (index$fixtures %||% list())[[fixture_name]] %||% NULL
  if (is.null(fixture)) {
    return(NULL)
  }

  fixture_path <- bg_protocol_asset_path(
    "protocol",
    "fixtures",
    fixture$path
  )
  if (!nzchar(fixture_path) || !file.exists(fixture_path)) {
    return(NULL)
  }

  jsonlite::read_json(fixture_path, simplifyVector = FALSE)
}

#' Validate a protocol object against its schema
#'
#' Performs structural validation of a protocol object against
#' expected field names, scalar types, nested arrays, object maps,
#' and local schema references. This remains lighter than a full
#' JSON Schema validator, but it is strong enough to catch drift
#' in the checked-in protocol surfaces.
#'
#' @param object The R object to validate
#' @param schema_name Name of the schema to validate against
#'
#' @return TRUE if valid, otherwise raises an error.
#' @keywords internal
bg_validate_protocol_object <- function(object, schema_name) {
  schema <- bg_load_protocol_schema(schema_name)
  if (is.null(schema)) {
    cli::cli_abort("Schema {.val {schema_name}} not found.")
  }

  bg_validate_protocol_value(
    object = object,
    schema = schema,
    schema_name = schema_name,
    path = schema_name
  )

  invisible(TRUE)
}

#' @keywords internal
bg_validate_protocol_value <- function(object, schema, schema_name, path) {
  ref <- schema$`$ref` %||% NULL
  if (!is.null(ref)) {
    ref_name <- sub("\\.json$", "", basename(ref))
    ref_schema <- bg_load_protocol_schema(ref_name)

    if (is.null(ref_schema)) {
      cli::cli_abort(
        "Schema {.val {schema_name}} references missing schema {.val {ref_name}} at {.val {path}}."
      )
    }

    return(bg_validate_protocol_value(
      object = object,
      schema = ref_schema,
      schema_name = ref_name,
      path = paste0(path, "->$ref(", ref_name, ")")
    ))
  }

  one_of <- schema$oneOf %||% NULL
  if (!is.null(one_of)) {
    matched <- vapply(
      one_of,
      function(option) {
        tryCatch(
          {
            bg_validate_protocol_value(object, option, schema_name, path)
            TRUE
          },
          error = function(...) FALSE
        )
      },
      logical(1)
    )

    if (!any(matched)) {
      cli::cli_abort(
        "Value at {.val {path}} does not match any allowed schema variant in {.val {schema_name}}."
      )
    }

    return(invisible(TRUE))
  }

  expected_type <- schema$type %||% NULL
  if (!is.null(expected_type)) {
    bg_validate_protocol_scalar_type(object, expected_type, path, schema_name)
  }

  enum_values <- schema$enum %||% NULL
  if (!is.null(enum_values) && !object %in% enum_values) {
    cli::cli_abort(
      "Value at {.val {path}} must be one of {.val {enum_values}} for schema {.val {schema_name}}."
    )
  }

  if (identical(expected_type, "string")) {
    min_length <- schema$minLength %||% NULL
    if (!is.null(min_length) && nchar(object) < min_length) {
      cli::cli_abort(
        "String at {.val {path}} is shorter than {.val {min_length}} for schema {.val {schema_name}}."
      )
    }

    pattern <- schema$pattern %||% NULL
    if (!is.null(pattern) && !grepl(pattern, object, perl = TRUE)) {
      cli::cli_abort(
        "String at {.val {path}} must match pattern {.val {pattern}} for schema {.val {schema_name}}."
      )
    }

    string_format <- schema$format %||% NULL
    if (!is.null(string_format)) {
      bg_validate_protocol_string_format(
        object = object,
        string_format = string_format,
        path = path,
        schema_name = schema_name
      )
    }
  }

  if (identical(expected_type, "integer")) {
    minimum <- schema$minimum %||% NULL
    if (!is.null(minimum) && object < minimum) {
      cli::cli_abort(
        "Integer at {.val {path}} is smaller than {.val {minimum}} for schema {.val {schema_name}}."
      )
    }
  }

  properties <- schema$properties %||% list()
  required <- schema$required %||% character()
  additional_properties <- schema$additionalProperties %||% NULL

  if (
    identical(expected_type, "object") ||
      length(properties) > 0 ||
      length(required) > 0 ||
      !is.null(additional_properties)
  ) {
    bg_validate_protocol_object_value(
      object = object,
      properties = properties,
      required = required,
      additional_properties = additional_properties,
      schema_name = schema_name,
      path = path
    )
  }

  items <- schema$items %||% NULL
  if (identical(expected_type, "array") || !is.null(items)) {
    bg_validate_protocol_array_value(
      object = object,
      items = items,
      schema_name = schema_name,
      path = path
    )
  }

  invisible(TRUE)
}

#' @keywords internal
bg_validate_protocol_string_format <- function(
  object,
  string_format,
  path,
  schema_name
) {
  valid <- switch(
    string_format,
    "date-time" = grepl(
      "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?(?:Z|[+-]\\d{2}:\\d{2})$",
      object,
      perl = TRUE
    ),
    TRUE
  )

  if (!valid) {
    cli::cli_abort(
      "String at {.val {path}} must match format {.val {string_format}} for schema {.val {schema_name}}."
    )
  }

  invisible(TRUE)
}

#' @keywords internal
bg_validate_protocol_scalar_type <- function(
  object,
  expected_type,
  path,
  schema_name
) {
  is_scalar <- length(object) == 1L

  type_ok <- switch(
    expected_type,
    "string" = is.character(object) && is_scalar,
    "integer" = is_scalar &&
      (is.integer(object) ||
        (is.numeric(object) && !is.na(object) && object == floor(object))),
    "number" = is.numeric(object) && is_scalar,
    "boolean" = is.logical(object) && is_scalar,
    "array" = is.atomic(object) || is.list(object),
    "object" = is.list(object),
    TRUE
  )

  if (!type_ok) {
    cli::cli_abort(
      paste0(
        "Value at {.val {path}} has unexpected type {.val {typeof(object)}} ",
        "for {.val {expected_type}} in schema {.val {schema_name}}."
      )
    )
  }
}

#' @keywords internal
bg_validate_protocol_object_value <- function(
  object,
  properties,
  required,
  additional_properties,
  schema_name,
  path
) {
  if (!is.list(object)) {
    cli::cli_abort(
      "Value at {.val {path}} must be an object-like list for schema {.val {schema_name}}."
    )
  }

  object_names <- names(object)
  if (is.null(object_names)) {
    cli::cli_abort(
      "Value at {.val {path}} must be a named list for schema {.val {schema_name}}."
    )
  }

  for (field in required) {
    if (is.null(object[[field]])) {
      cli::cli_abort(
        "Protocol object missing required field {.val {field}} at {.val {path}} for schema {.val {schema_name}}."
      )
    }
  }

  for (field in intersect(names(properties), object_names)) {
    bg_validate_protocol_value(
      object = object[[field]],
      schema = properties[[field]],
      schema_name = schema_name,
      path = paste0(path, "$", field)
    )
  }

  if (is.null(additional_properties)) {
    return(invisible(TRUE))
  }

  if (!is.logical(additional_properties) && !is.list(additional_properties)) {
    cli::cli_abort(
      "Schema {.val {schema_name}} has invalid {.field additionalProperties} at {.val {path}}."
    )
  }

  extra_fields <- setdiff(object_names, names(properties))
  if (identical(additional_properties, FALSE) && length(extra_fields) > 0) {
    cli::cli_abort(
      "Unexpected fields {.val {extra_fields}} at {.val {path}} for schema {.val {schema_name}}."
    )
  }

  if (is.list(additional_properties)) {
    for (field in extra_fields) {
      bg_validate_protocol_value(
        object = object[[field]],
        schema = additional_properties,
        schema_name = schema_name,
        path = paste0(path, "$", field)
      )
    }
  }

  invisible(TRUE)
}

#' @keywords internal
bg_validate_protocol_array_value <- function(object, items, schema_name, path) {
  if (is.null(items)) {
    return(invisible(TRUE))
  }

  values <- if (is.list(object)) {
    unname(object)
  } else {
    as.list(object)
  }

  for (idx in seq_along(values)) {
    bg_validate_protocol_value(
      object = values[[idx]],
      schema = items,
      schema_name = schema_name,
      path = paste0(path, "[", idx, "]")
    )
  }

  invisible(TRUE)
}
