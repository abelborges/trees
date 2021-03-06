
entropy = function(p, k = length(p)) {
  p = p[p > 0]
  -sum(p * log(p, k))
}

expected_entropy = source('expected_entropy.R')$value

best_split = function(y, x, k, minsize = 30, minentropy = .1) {
  n = length(y)
  k = length(unique(y))
  feats = names(x)
  
  if (n < minsize | entropy(table(y)/n, k) < minentropy)
    return(list(feature = NULL, n = n))
  
  entropies = lapply(feats, function(feat) {
    expected_entropy(y, x[[feat]], n, k)
  })
  
  best = which.min(sapply(entropies, function(e) e$entropy))
  list(
    n = n,
    feature = feats[best],
    cut = entropies[[best]]$cut
  )
}

build = function(y, x, ids, parentval = NULL) {
  split = best_split(y, x)
  
  if (is.null(split$feature) | length(x) == 1) {
    if (is.null(parentval)) # nothing interesting
      NULL
    else # leaf
      list(
        n = split$n,
        parentval = parentval,
        ids = ids
      )
  } else { # split again
    cols = setdiff(names(x), split$feature)
    
    feat = x[[split$feature]]
    if (is.numeric(split$cut))
      feat = feat > split$cut
    else
      feat = feat %in% split$cut
    featvals = unique(feat)
    
    children = lapply(featvals, function(featval) {
      rows = which(feat == featval)
      build(y[rows], x[rows,][cols], ids[rows], featval)
    })
    
    list(
      n = split$n,
      parentval = if (is.null(parentval)) 'root' else parentval,
      feature = split$feature,
      cut = split$cut,
      split = setNames(children, featvals)
    )
  }
}

function(data, target) {
  y = data[[target]]
  x = data[,setdiff(names(data), target)]
  build(y, x, ids = 1:length(y))
}
