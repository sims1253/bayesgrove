#' @keywords internal
bg_print_graph_tree <- function(graph) {
  nodes <- graph$nodes %||% list()
  edges <- graph$edges %||% list()

  if (length(nodes) == 0) {
    cli::cli_inform("Empty graph.")
    return(invisible(NULL))
  }

  # Find roots (nodes with no incoming edges)
  has_incoming <- if (length(edges) > 0) {
    unique(vapply(
      edges,
      function(edge) edge$to %||% "",
      character(1)
    ))
  } else {
    character(0)
  }
  has_incoming <- has_incoming[nzchar(has_incoming)]
  roots <- setdiff(names(nodes), has_incoming)

  if (length(roots) == 0) {
    roots <- names(nodes)[1]
  }

  # Build adjacency list
  adj <- list()
  for (e in edges) {
    if (is.null(e$from) || is.null(e$to)) {
      next
    }
    adj[[e$from]] <- c(adj[[e$from]], e$to)
  }

  printed <- new.env(parent = emptyenv())

  print_node <- function(node_id, prefix = "", is_last = TRUE) {
    if (exists(node_id, envir = printed)) {
      label <- (nodes[[node_id]] %||% list())$label %||% node_id
      connector <- if (is_last) "\u2514\u2500\u2500 " else "\u251c\u2500\u2500 "
      cli::cli_verbatim(paste0(
        prefix,
        connector,
        cli::col_yellow("[already shown: ", label, "]")
      ))
      return()
    }

    assign(node_id, TRUE, envir = printed)
    node <- nodes[[node_id]] %||% list()

    connector <- if (is_last) "\u2514\u2500\u2500 " else "\u251c\u2500\u2500 "
    label <- node$label %||% node_id

    state_color <- switch(
      node$state %||% "new",
      "cached" = cli::col_green,
      "ready" = cli::col_cyan,
      "blocked" = cli::col_red,
      "running" = cli::col_yellow,
      cli::col_grey
    )

    state_str <- node$state %||% "new"
    kind_str <- node$kind %||% "unknown"
    lbl <- cli::col_white(label)
    knd <- cli::col_grey("<", kind_str, ">")
    cli::cli_verbatim(paste0(
      prefix,
      connector,
      lbl,
      " ",
      knd,
      " ",
      state_color("[", state_str, "]")
    ))

    children <- adj[[node_id]] %||% character(0)
    if (length(children) > 0) {
      new_prefix <- paste0(prefix, if (is_last) "    " else "\u2502   ")
      for (i in seq_along(children)) {
        print_node(children[i], new_prefix, i == length(children))
      }
    }
  }

  cli::cli_h2("Execution Graph")
  for (i in seq_along(roots)) {
    print_node(roots[i], "", i == length(roots))
  }

  invisible(NULL)
}

#' @method print dagriculture_graph
#' @export
print.dagriculture_graph <- function(x, ...) {
  bg_print_graph_tree(x)
  invisible(x)
}
