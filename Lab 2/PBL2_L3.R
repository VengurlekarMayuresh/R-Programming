# Lab 3: Control Flow for Data Cleaning
getwd()

data <- read.csv("processed.cleveland.data", header = F, na.strings = "?")

cat(readLines("heart-disease.names"), sep = "\n")

data

colnames(data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", 
                          "restecg", "thalach", "exang", "oldpeak", 
                          "slope", "ca", "thal", "num")
# above were taken from o/p of heart-disease.names
# purposeful anomalies as stated
{
  set.seed(47)
  
  n <- nrow(data)
  
  n # num of rows we got
  
  data$trestbps[sample(1:n, 5)] <- -150
  data$trestbps[sample(1:n, 5)] <- NA    
  data$trestbps[sample(1:n, 5)] <- 340
}

# task 1 bp cleaning func

bpClean <- function(bp) {
  if(is.na(bp))
    return(NA)
  else if(bp<0)
    return(NA)
  else if(bp > 250)
    return(250)
  else
    return(bp)
}

# task 2 error handling using try catch

calcBP <- function(chol, trestbps)  {
  tryCatch(
    expr = {
      if(is.na(trestbps) | is.na(chol)) stop("Input Value is NA")
      if(trestbps <= 0) stop("Denminator is <= 0")
      return(chol / trestbps)
    },
    error = function(e) {
      warning(paste("Handled Error: ", e$message))
      return(NA)
    }
  )
}

meanBP <- 
  tryCatch(
    expr = mean(data$trestbps, na.rm = T),
    error = function(e) {message("Error calculating mean:", e$message); return(NA)}
)
# task 3 loop based vs vectorized data cleaning

loop <- function(bp) {
  result <- numeric(length(bp))
  for (i in seq_along(bp))
    result[i] <- bpClean(bp[i])
  return(result)
}


vectorized <- function(bp) {
  bp[bp < 0] <- NA
  bp[bp > 250] <- 250
  return(bp)
}


if (!require(microbenchmark)) install.packages("microbenchmark")
library(microbenchmark)

benchmark_results <- microbenchmark(
  Loop = loop(data$trestbps),
  Vectorized = vectorized(data$trestbps),
  times = 100
)
print(benchmark_results)

# task 4 validation of cleaned data after above steps

cat("Negative BP:", sum(data$trestbps < 0, na.rm = TRUE), "\n")
cat("BP > 250:", sum(data$trestbps > 250, na.rm = TRUE), "\n")

data$trestbps <- vectorized(data$trestbps)

cat("Missing BP Count:", sum(is.na(data$trestbps)), "\n")
cat("Min BP:", min(data$trestbps, na.rm = TRUE), "\n")
cat("Max BP:", max(data$trestbps, na.rm = TRUE), "\n")
cat("Mean BP:", mean(data$trestbps, na.rm = TRUE), "\n")
cat("Median BP:", median(data$trestbps, na.rm = TRUE), "\n")

cat("Remaining Negative BP:", sum(data$trestbps < 0, na.rm = TRUE), "\n")
cat("Remaining BP > 250:", sum(data$trestbps > 250, na.rm = TRUE), "\n")

write.csv(data, "cleaned_heart_data.csv", row.names = FALSE)


print("Vectorized cleaning is much faster than loop 
      (approximately 7.5x faster by mean of time taken)")
print("This is because a for loop iterates line by line to perform 
      calculations for each entry in R's interpreted env. 
      Meanwhile, vectorized computation offloads this task to 
      pre-compiled hardware optimized code specifically meant 
      for this task. It skips the time overhead of interpretation by 
      directly sending the data to the underlying language.")