add <- function(x, y){
  x+y
}

above10 <- function(vec){
  subset <- vec>10
  vec[subset]
}

subset_lower_limit <- function(vec, limit=5){
  subset <- vec>limit
  vec[subset]
}

column_mean <- function(mat, remove_na=TRUE){
  means <- numeric(ncol(mat))
  for(i in 1:ncol(mat)){
    means[i] <- mean(mat[, i], na.rm=remove_na)
  }
  means
}