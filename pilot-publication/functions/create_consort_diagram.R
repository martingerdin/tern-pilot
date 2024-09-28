create_consort_diagram <- function(data) {
    library(DiagrammeR)

    # Create the flow diagram using grViz
    grViz("
digraph consort {

  # Set node shape and font attributes
  node [shape=square, fontname=Helvetica, fontsize=10]

  # Nodes
  A [label='Total: Clusters: 30 | Residents: 90 | Patients: 270']
  B [label='Standard Care\nClusters: 10\nResidents: 30\nPatients: 90']
  C [label='ATLS\nClusters: 10\nResidents: 30\nPatients: 90']
  D [label='PTC\nClusters: 10\nResidents: 30\nPatients: 90']

  # Mid-point for the split
  mid [shape=point, width=0, label='']

  # Edges
  A -> mid [arrowhead=none]  # Straight line down, no arrowhead
  mid -> B [tailport=s, headport=n]  # Connector to Standard Care
  mid -> C [tailport=s, headport=n]  # Connector to ATLS
  mid -> D [tailport=s, headport=n]  # Connector to PTC
}
")
}
