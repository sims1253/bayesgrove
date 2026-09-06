# Mermaid flowchart of the active project graph

Emits Mermaid flowchart text for the active graph: node label + kind,
state coloring (cached/ready/held/blocked/failed), and branch grouping
via subgraphs. Pure string manipulation — zero new dependencies —
renders on GitHub and in Quarto. Embed the output in
[bg_export_report](https://sims1253.github.io/bayesgrove/dev/reference/bg_export_report.md).

## Usage

``` r
bg_graph_mermaid(project, direction = "TD")
```

## Arguments

- project:

  A `bg_handle`.

- direction:

  Mermaid graph direction (default `"TD"`; also `"LR"`).

## Value

A character scalar of Mermaid flowchart text with class `bg_mermaid`,
whose print method renders the raw text (so the result is copy-pasteable
at the console and embeddable as a string).
