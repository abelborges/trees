
is_ordinal = function(x) {
  is_not = is.character(x) | (is.factor(x) & !is.ordered(x))
  !is_not
}

function(of_y, given_x, n, k) {
  xlevels = unique(given_x)
  ordinal = is_ordinal(given_x)
  
  if (!ordinal) {
    probs_class1 = sapply(xlevels, function(x) mean(of_y[given_x == x]))
    xlevels = xlevels[order(probs_class1)]
    given_x = factor(given_x, levels = xlevels, ordered = TRUE)
  } else {
    xlevels = sort(xlevels)
  }
  
  entropy_by_cut = sapply(xlevels[-length(xlevels)], function(x) {
    x_given = which(given_x > x)
    e1 = entropy(table(of_y[ x_given ])/n, k)
    e2 = entropy(table(of_y[-x_given ])/n, k)
    p = length(x_given) / n
    p*e1 + (1-p)*e2
  })
  
  best = which.min(entropy_by_cut)
  
  list(
    cut = if (ordinal) xlevels[best] else xlevels[seq_along(xlevels) > best],
    entropy = entropy_by_cut[best]
  )
}
