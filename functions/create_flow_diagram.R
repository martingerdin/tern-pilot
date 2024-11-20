create_flow_diagram <- function() {
    flow.diagram <- paste0(
        c(
            "digraph flow_diagram {",
            "node [shape=rect]",
            "edge [splines=ortho]",
            "A[label=\"Clusters assessed for eligibility and randomised (n = 7)\"]",
            "B[label=\"Clusters randomised to standard care (n = 3)\"]",
            "C[label=\"Clusters randomised to ATLS (n = 2)\"]",
            "D[label=\"Clusters randomised to PTC (n = 2)\"]",
            paste0("E[label=\"Patients included (n = ", n.control, ")\"]"),
            paste0("F[label=\"Patients included (n = ", n.atls, ")\\nResidents included (n = ", n.atls.residents, ")\"]"),
            paste0("G[label=\"Patients included (n = ", n.ptc, ")\\nResidents included (n = ", n.ptc.residents, ")\"]"),
            "A -> B",
            "A -> C",
            "A -> D",
            "B -> E",
            "C -> F",
            "D -> G",
            "}"
        ),
        collapse = "\n"
    )
    write(flow.diagram, file = "flow-diagram.dot")
    system("dot -Tpng flow-diagram.dot -o flow-diagram.png")
}
