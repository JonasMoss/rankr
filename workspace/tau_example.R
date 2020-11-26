tab = data.frame(matrix(c(102, 35, 68, 34,
191, 80, 215, 122,
110, 90, 168, 223), nrow = 3, by = TRUE))

tab = cbind(c("more", "average", "less"), tab)
colnames(tab) = c("school", "A", "B", "C", "D")


countsToCases(as.data.frame(table(tab)))

(MyTable <- table(tab))

(MyTable <- table(state.division, state.region))



# Convert from data frame of counts to data frame of cases.
# `countcol` is the name of the column containing the counts
countsToCases <- function(x, countcol = "Freq") {
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
  
  # Drop count column
  x[[countcol]] <- NULL
  
  # Get the rows from x
  x[idx, ]
}

countsToCases(tab)