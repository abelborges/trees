
expected_entropy = function(given_numeric_feature) {
  xx = x[[given_numeric_feature]]
  thresholds = quantile(xx, seq(.01, .99, by = .01))
  potential_entropies = sapply(thresholds, function(threshold) {
    i = which(xx > threshold)
    e1 = entropy(table(y[ i ])/n, k)
    e2 = entropy(table(y[-i ])/n, k)
    p = length(i)/n
    p*e1 + (1-p)*e2
  })
  best_cut = which.min(entropies)
  list(
    best_cut = qs[best_cut],
    entropy = potential_entropies[best_cut]
  )
}
