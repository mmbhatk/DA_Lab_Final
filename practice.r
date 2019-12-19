stream <- c(1, 5, 3, 4, 1, 3, 4, 5, 1, 3, 4, 5, 1, 1, 5, 4, 3, 4)
stream <- as.numeric(as.factor(stream))

b <- function(x) {
  num = as.numeric(intToBits(x))
  index = match(1, num) - 1
  return(index)
}

h <- c()
for(i in 1:length(stream)) {
  h[i] <- ((6 * stream[i] + 1) %% 5)
}

B <- c()
for(i in 1:length(stream)) {
  B[i] <- b(h[i])
}

B[is.na(B)] <- 0
print(2 ^ max(B))