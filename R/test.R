decision_tree = source('trees.R')$value
viz = source('viz.R')$value

fake_it = function(n = 1000) {
  data.frame(
    x1 = sample(letters[1:2], n, T),
    x2 = sample(letters[1:2], n, T),
    x3 = rnorm(n),
    y = sample(c(F,T), n, T)
  )
}

data = fake_it()
head(data)
tree = decision_tree(data, target = 'y')

viz(tree)
