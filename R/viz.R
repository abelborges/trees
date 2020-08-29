library(data.tree)
library(DiagrammeR)

function(tree, splitnode = 'split') {
  nodes = as.Node(tree)
  
  nodes$Do(function(node) {
    label = if (node$name == splitnode) ({
      p = node$parent
      if (!is.numeric(p$cut))
        paste0(p$feature, ' in {', paste(p$cut, collapse = ','), '}')
      else
        paste(p$feature, '>', round(p$cut, 2))
    }) else as.character(node$parentval)
    
    tooltip = if (node$name == splitnode) ({
      node$parent$n
    }) else ({
      node$n
    })
    
    SetNodeStyle(
      node,
      label = label,
      tooltip = tooltip,
      style = 'filled,rounded',
      penwidth = 2
    )
  })
  
  plot(nodes)
}
