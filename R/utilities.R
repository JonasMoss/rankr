#' Convert Table Object to Vectors
#'
#' @param tab A table object.
#' @return A list of two vectors `x, y` satisfying `table(x, y) == tab`.

table_to_vectors = function(tab) {

  n = sum(tab)
  x_names = as.numeric(rownames(tab))
  y_names = as.numeric(colnames(tab))
  x = rep(NA, n)
  y = rep(NA, n)

  index = 1
  for (i in seq_len(nrow(tab))) {
    for (j in seq_len(ncol(tab))) {
      x[index:(index + tab[i, j] - 1)] = x_names[i]
      y[index:(index + tab[i, j] - 1)] = y_names[j]
      index = index + tab[i, j]
    }
  }

  list(x = x, y = y)

}
