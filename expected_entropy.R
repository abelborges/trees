# TODO: use only binary splits, see ESLII section 9.2.4, page 310

is_ordinal = function(x) {
  not_ordinal = is.character(x) | (is.factor(x) & !is.ordered(x))
  !not_ordinal
}

expected_entropy_categoric = function(of_y, given_x, n, k) {
  distribution = table(given_x) / n
  cond_entropies = sapply(names(distribution), function(val) {
    entropy(table(of_y[given_x == val])/n, k)
  })
  
  list(
    cut = NULL,
    entropy = sum(distribution * cond_entropies)
  )
}

expected_entropy_numeric = function(of_y, given_x, n, k) {
  ps = seq(.01, .99, by = .01)
  qtype = if (is.factor(given_x)) 3 else 7 # see 'Types' in ?quantile
  cuts = quantile(given_x, probs = ps, names = FALSE, type = qtype)
  
  entropy_by_cut = sapply(seq_along(cuts), function(i) {
    idxs = which(given_x > cuts[i])
    e1 = entropy(table(of_y[ idxs ])/n, k = 2)
    e2 = entropy(table(of_y[-idxs ])/n, k = 2)
    p = 1 - ps[i] # approximately length(idxs)/n
    p*e1 + (1-p)*e2
  })
  
  # which.min returns first match
  # which.min(sample(entropy_by_cut)) would randomize in case there's a tie
  best = which.min(entropy_by_cut)
  
  list(
    cut = cuts[best],
    entropy = entropy_by_cut[best]
  )
}

function(of_y, given_x, n, k) {
  if (!is_ordinal(given_x))
    expected_entropy_categoric(of_y, given_x, n, k)
  else
    expected_entropy_numeric(of_y, given_x, n, k)
}
