
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
