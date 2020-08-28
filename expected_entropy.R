
is_ordinal = function(x) {
  not_ordinal = is.character(x) | (is.factor(x) & !is.ordered(x))
  !not_ordinal
}

function(of, given, n, k) {
  if (!is_ordinal(given)) {
    distribution = table(given)/n
    cond_entropies = sapply(names(distribution), function(val) {
      entropy(table(of[given == val])/n, k)
    })
    
    list(
      cut = NULL,
      entropy = sum(distribution * cond_entropies)
    )
  } else {
    ps = seq(.01, .99, by = .01)
    qtype = if (is.factor(given)) 3 else 7 # see 'Types' in ?quantile
    cuts = quantile(given, probs = ps, names = FALSE, type = qtype)
    
    entropy_by_cut = sapply(seq_along(cuts), function(i) {
      idxs = which(given > cuts[i])
      e1 = entropy(table(of[ idxs ])/n, k = 2)
      e2 = entropy(table(of[-idxs ])/n, k = 2)
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
}
