
entropy = function(p, k = length(p)) {
  p = p[p > 0]
  -sum(p * log(p, k))
}

best_split = function(y, x, minsize = 30, minentropy = .1) {
  n = length(y)
  features = names(x)
  k = length(unique(y))
  prior = entropy(table(y)/n, k)
  
  if (n < minsize | prior < minentropy)
    return(list(feature = NULL, n = n))
  
  expected_entropy = function(given_feature) {
    xx = x[[given_feature]]
    distribution = table(xx) / n
    cond_entropies = sapply(names(distribution), function(val) {
      entropy(table(y[xx == val])/n, k)
    })
    sum(distribution * cond_entropies)
  }
  
  entropies = sapply(features, expected_entropy)
  features[which.min(entropies)]
}

build = function(y, x, ids, parentval = NULL) {
  n = length(y)
  feature = best_split(y, x)
  
  if (is.null(feature) | length(x) == 1) {
    if (is.null(parentval)) # nothing interesting
      NULL
    else # leaf
      list(
        n = n,
        parentval = parentval,
        ids = ids
      )
  } else { # split again
    feat = x[[feature]]
    cols = setdiff(names(x), feature)
    featvals = unique(feat)
    
    children = lapply(featvals, function(featval) {
      rows = which(feat == featval)
      build(y[rows], x[rows,][cols], ids[rows], featval)
    })
    
    list(
      n = n,
      parentval = if (is.null(parentval)) 'root' else parentval,
      feature = feature,
      children = setNames(children, featvals)
    )
  }
}

decision_tree = function(data, target) {
  y = data[[target]]
  x = data[,setdiff(names(data), target)]
  build(y, x, ids = 1:length(y))
}

fake_it = function(n = 1000) {
  data.frame(
    x1 = sample(letters[1:2], n, T),
    x2 = sample(letters[1:2], n, T),
    x3 = sample(letters[1:2], n, T),
    y = sample(c(F,T), n, T)
  )
}

data = fake_it()
tree = decision_tree(data, target = 'y')
tree
