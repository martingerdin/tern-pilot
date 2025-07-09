create_consort_diagram <- function(results) {
  str_replace <- stringr::str_replace
  coll <- stringr::coll
  n.control <- results$n.control |> as.character()
  n.atls <- results$n.atls |> as.character()
  n.atls.residents <- results$n.atls.residents |> as.character()
  n.ptc <- results$n.ptc |> as.character()
  n.ptc.residents <- results$n.ptc.residents |> as.character()
  n.control.complete.m30d <- results$n.control.complete.m30d |> as.character()
  n.control.complete.hd <- results$n.control.complete.hd |> as.character()
  n.atls.complete.m30d <- results$n.atls.complete.m30d |> as.character()
  n.atls.complete.hd <- results$n.atls.complete.hd |> as.character()
  n.ptc.complete.m30d <- results$n.ptc.complete.m30d |> as.character()
  n.ptc.complete.hd <- results$n.ptc.complete.hd |> as.character()
  consort.svg <- readLines("consort-flow-diagram.svg") |>
    str_replace(coll("n.control"), n.control) |>
    str_replace(coll("n.atls.residents"), n.atls.residents) |>
    str_replace(coll("n.atls"), n.atls) |>
    str_replace(coll("n.ptc.residents"), n.ptc.residents) |>
    str_replace(coll("n.ptc"), n.ptc) |>
    str_replace(coll("n.control.complete.m30d"), n.control.complete.m30d) |>
    str_replace(coll("n.control.complete.hd"), n.control.complete.hd) |>
    str_replace(coll("n.atls.complete.m30d"), n.atls.complete.m30d) |>
    str_replace(coll("n.atls.complete.hd"), n.atls.complete.hd) |>
    str_replace(coll("n.ptc.complete.m30d"), n.ptc.complete.m30d) |>
    str_replace(coll("n.ptc.complete.hd"), n.ptc.complete.hd)
  writeLines(consort.svg, "modified-consort-flow-diagram.svg")
  system("svgexport modified-consort-flow-diagram.svg consort-flow-diagram.png 2x")
}
