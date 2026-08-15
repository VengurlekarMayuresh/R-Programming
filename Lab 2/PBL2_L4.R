# Lab 4 Advanced missing data handling

#' ---
#' title: "Lab 4: Advanced Missing Data Handling"
#' output: 
#'   pdf_document:
#'     latex_engine: xelatex
#' ---

required_packages <- c("naniar", "skimr", "ggplot2")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}


url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
adult_cols <- c("age", "workclass", "fnlwgt", "education", "education_num", 
                "marital_status", "occupation", "relationship", "race", "sex", 
                "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

data <- read.csv(url, header = FALSE, col.names = adult_cols, strip.white = TRUE, stringsAsFactors = FALSE)

set.seed(47)
n <- nrow(data)

data$age[sample(1:n, 1500)] <- 999
data$age[sample(1:n, 2000)] <- NA
data$workclass[sample(1:n, 3500)] <- ""
data$hours_per_week[sample(1:n, 800)] <- NaN

cat("Count of NA in age:", sum(is.na(data$age)), "\n")
cat("Count of NaN in hours_per_week:", sum(is.nan(data$hours_per_week)), "\n")
cat("Count of blank strings (\"\") in workclass:", sum(data$workclass == ""), "\n")
cat("Count of impossible age values (age == 999):", sum(data$age == 999, na.rm = TRUE), "\n")
dummy_obj <- NULL
cat("is.null(dummy_obj):", is.null(dummy_obj), "\n")
cat("is.null(data$age[1]):", is.null(data$age[1]), "\n")

cat("\nVariable-wise missing summary:\n")
print(miss_var_summary(data))

impute_median <- function(x) {
  if (!is.numeric(x)) stop("Input must be a numeric vector.")
  valid_median <- median(x[!is.na(x) & !is.nan(x)], na.rm = TRUE)
  x[is.na(x) | is.nan(x)] <- valid_median
  return(x)
}

cleaned_data <- data
cleaned_data$age[cleaned_data$age == 999] <- NA
cleaned_data$workclass[cleaned_data$workclass == ""] <- "Unknown"
cleaned_data$occupation[cleaned_data$occupation == "?"] <- "Unknown"
cleaned_data <- cleaned_data[!is.nan(cleaned_data$hours_per_week), ]
cleaned_data$age <- impute_median(cleaned_data$age)
cleaned_data$hours_per_week <- impute_median(cleaned_data$hours_per_week)
complete_rows <- sum(complete.cases(cleaned_data))
cat("\nNumber of completely intact rows:", complete_rows, "out of", nrow(cleaned_data), "\n")

cat("\nVariable-wise missing summary (After Cleaning):\n")
print(miss_var_summary(cleaned_data))

# task 5
skim_summary <- skim_without_charts(cleaned_data)
print(skim_summary)
cat("Impossible age (999) remaining:", sum(cleaned_data$age == 999, na.rm = TRUE), "\n")
cat("Untreated NAs in age:", sum(is.na(cleaned_data$age)), "\n")
cat("Blank strings remaining in workclass:", sum(cleaned_data$workclass == ""), "\n")

write.csv(cleaned_data, "cleaned_data.csv", row.names = FALSE)
cat("\nSaved output file as 'cleaned_data.csv'\n")


library(naniar)
library(gridExtra)

p_before <- vis_miss(data) + ggtitle("Data Missingness Matrix (Before)")
p_after  <- vis_miss(cleaned_data) + ggtitle("Data Missingness Matrix (After)")

png("missingness_matrix_comparison.png", width = 1000, height = 500)
grid.arrange(p_before, p_after, ncol = 2)
dev.off()