stream <- c(1, 2, 3, 5, 9, 3, 5, 2, 1, 3)
stream <- as.numeric(as.factor(stream))
print(stream)

# Returns the number of trailing zeroes
b <- function(x) {
  num = as.numeric(intToBits(x))
  index <- match(1, num) - 1
  return(index)
}

# Step 1: Calculate the hash values
H <- c()
for(i in 1:length(stream)){
  H[i] <- ((6 * i + 1) %% 5)
}

# Step 2: Count number of trailing zeroes in binary representation
B <- c()
for(i in 1:length(H)){
  B[i] <- b(H[i])
}

B[is.na(B)] <- 0
print(B)
print(2 ^ max(B))