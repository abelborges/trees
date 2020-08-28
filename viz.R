library(data.tree)
library(DiagrammeR)

function(tree) {
  nodes = as.Node(tree)
  
  nodes$Do(function(node) {
    label = if (node$name == 'then') ({
      n = node$parent
      if (!is.null(n$cut)) paste(n$feature, '>', round(n$cut, 2))
      else n$feature
    }) else as.character(node$parentval)
    
    SetNodeStyle(
      node,
      arrowhead = 'none',
      label = label,
      tooltip = if (node$isLeaf) length(node$ids) else '',
      style = 'filled,rounded',
      penwidth = 2
    )
  })
  
  plot(nodes)
}
