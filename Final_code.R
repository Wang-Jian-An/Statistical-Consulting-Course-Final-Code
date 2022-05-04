# Import Library
###########################
library("openxlsx")
library("ggplot2")
library("GGally")
library("dplyr")
library("multcomp")
library("nnet")
library("h2o") # AutoML
###########################

# Set path
setwd("D://OneDrive - gs.ncku.edu.tw//1102課程//統計諮詢")

# Load Data
###########################
row_data <- read.xlsx("Dataset//DryBeanDataset//Dry_Bean_Dataset.xlsx")
###########################

# Data Info
###########################
str(row_data)

###########################

# EDA
###########################
unique_class <- unique(row_data[, target])
one_column <- numerical_features[16]
for (one_class in unique_class){
  print(one_class)
  select_class_value <- row_data %>% filter(Class == one_class)
  select_class_value <- select_class_value[, one_column]
  max_value <- max(select_class_value, na.rm = TRUE)
  min_value <- min(select_class_value, na.rm = TRUE)
  median_value <- median(select_class_value, na.rm = TRUE)
  print(max_value)
  print(min_value)
  print(median_value)
}

###########################

# Preprocessing

# Define Features
###########################
target <- "Class"
target_BOMBAY_or_not <- "BOMBAY_or_not"
all_columns <- colnames(row_data)
numerical_features <- all_columns[which(all_columns != target)]
unique_class <- unique(row_data[, target])
###########################

# Preprocessing
###########################
row_data[, target] <- as.factor(row_data[, target])
row_data[, "BOMBAY_or_not"] <- ifelse()
###########################

# Handle Outliers - 使用 Box plot 概念排除掉 Outliers 後重新定義新的資料
###########################
preprocess_outlier_data <- row_data

detect_outlier_via_boxplot <- function(input_data){
  Q3_value <- quantile(input_data, 3 / 4)
  Q1_value <- quantile(input_data, 1 / 4)
  IQR_value <- Q3_value - Q1_value
  lower_bound <- Q1_value - 1.5 * IQR_value
  upper_bound <- Q3_value + 1.5 * IQR_value
  return (c(lower_bound, upper_bound))
}


Q3_value <- quantile(row_data[, one_column], 3 / 4)
Q1_value <- quantile(row_data[, one_column], 1 / 4)
IQR_value <- Q3_value - Q1_value
lower_bound <- Q1_value - 1.5 * IQR_value

# one_column <- numerical_features[1]
for (one_column in numerical_features){
  lower_upper_bound <- detect_outlier_via_boxplot(row_data[, one_column])
  preprocess_outlier_data[, one_column] <- ifelse(lower_upper_bound[1] > row_data[, one_column] | row_data[, one_column] > lower_upper_bound[2], NA, row_data[, one_column])
}

###########################


## Descriptive Statistics
###########################

# 把每個變數的欄位、平均數、標準差與檢定結果變成一個 Row
content_column <- vector(mode = "list", length = length(unique_class)+1)
names(content_column) <- c("Column Name", unique_class)

for (one_column in numerical_features){
  for (one_class in unique_class){
    select_class_value <- preprocess_outlier_data %>% filter(Class == one_class)
    select_class_value <- select_class_value[, one_column]
    mean_value <- mean(select_class_value, na.rm = TRUE)
    sd_value <- sd(select_class_value, na.rm = TRUE)
    content_column[[one_class]] <- c(content_column[[one_class]], 
                                     paste(round(mean_value, 4), " (", round(sd_value, 4), ")", sep = ""))
  }
  content_column[["Column Name"]] <- c(content_column[["Column Name"]], one_column)
}

content_column <- data.frame(content_column)
write.csv(content_column, "preprocess_outlier_Descriptive_Statistics.csv")
###########################

## Plot
###########################
# 繪製每個變數的 Histogram
one_column <- numerical_features[1]
for (one_column in numerical_features){
  temp_plot <- ggplot(mapping = aes(x = row_data[, one_column])) +
    geom_histogram(mapping = aes(y = ..density..)) +
    geom_density(alpha = .2, color = "red") +
    scale_x_continuous(one_column) +
    ggtitle(paste("The histogram of", one_column))
  ggsave(temp_plot, file = paste("histogram//histogram_", one_column, ".png", sep = ""))
}

# 繪製主要變數之間的散佈圖
ggpairs(row_data[, c("Area", "Perimeter", "MajorAxisLength", "MinorAxisLength", "Eccentricity", "ConvexArea")],
        mapping = aes(color = row_data[, "Class"]))

# 繪製每個變數各別在七種類別之間的Boxplot
one_column <- numerical_features[1]
for (one_column in numerical_features){
  temp_plot <- ggplot(mapping = aes(x = row_data[, target], y = row_data[, one_column])) +
    geom_boxplot() +
    xlab(target) +
    ylab(one_column) +
    ggtitle(paste("The boxplot of ", one_column, " between ", target, sep = ""))
  ggsave(temp_plot, file = paste("boxplot//boxplot_", one_column, ".png", sep = ""))
}


###########################

# Hypothesis
###########################
one_column <- numerical_features[2]
aov_formula <- as.formula(paste(one_column, "~", target))
bartlett_test_result <- bartlett.test(aov_formula, data = row_data)
bartlett_test_result
aov_model <- aov(formula = aov_formula, data = row_data)
summary(aov_model)
KW_model <- kruskal.test(aov_formula, data = row_data)
KW_model

post_test <- TukeyHSD(aov_model)
post_test$Class
plot(post_test)

###########################

# Correlation
###########################

content_column <- vector(mode = "list", length = length(numerical_features)+1)
names(content_column) <- c("Column_Name", numerical_features)

for (one_column in numerical_features){
  content_column[["Column_Name"]] <- c(content_column[["Column_Name"]], one_column)
  for (two_column in numerical_features){
    corr_value <- cor(row_data[, c(one_column, two_column)])[one_column, two_column]
    content_column[[one_column]] <- c(content_column[[one_column]], corr_value)
  }
}

content_column <- data.frame(content_column)
write.csv(content_column, "row_report//Pearson_Correlation.csv")
###########################


# Multinomial Logistic Regression
###########################
unique_class <- unique_class[c(3, 1, 2, 4, 5, 6, 7)]

# 建立 Column Name 在 List 的標頭
result_list <- vector(mode = "list", length = length(unique_class)+1)
names(result_list) <- c("Column_Name", unique_class)
for (one_class in unique_class){
  result_list[[one_class]] <- vector(mode = "list", length = 4)
  names(result_list[[one_class]]) <- c("Coefficient (Stderr)", "Odds", "95% CI", "p-value")
}

row_data[, target] <- relevel(row_data[, target], ref = "BOMBAY")
# one_column <- numerical_features[1]
for (one_column in numerical_features){
  model_formula <- as.formula(paste(target, "~", one_column))
  model <- multinom(formula = model_formula, data = row_data)
  ctable <- data.frame(summary(model)$coefficients)
  ctable[, "Stderr"] <- summary(model)$standard.errors[, "Area"]
  ctable[, "Odds"] <- exp(ctable[, "Area"])
  lower_coef <- ctable[, "Area"] - qnorm(0.975) * ctable[, "Stderr"]
  upper_coef <- ctable[, "Area"] + qnorm(0.975) * ctable[, "Stderr"]
  ctable[, "lower_bound"] <- exp(lower_coef)
  ctable[, "upper_bound"] <- exp(upper_coef)
  ctable[, "zvalue"] <- ctable[, "Area"] / ctable[, "Stderr"]
  ctable[, "p-value"] <- (1 - pnorm(abs(ctable[, "zvalue"]), 0, 1)) * 2
  
  for (one_result in c("Coefficient (Stderr)", "Odds", "95% CI", "p-value")){
    result_list[[unique_class[1]]][[one_result]] <- c(result_list[[unique_class[1]]][[one_result]], "ref")
  }
  
  for (one_class in unique_class[2:7]){
    result_list[[one_class]][["Coefficient (Stderr)"]] <- c(result_list[[one_class]][["Coefficient (Stderr)"]], ctable[one_class, one_column])
    result_list[[one_class]][["Odds"]] <- c(result_list[[one_class]][["Odds"]], ctable[one_class, "Odds"])
    result_list[[one_class]][["95% CI"]] <- c(result_list[[one_class]][["95% CI"]],
                                              paste("[", round(ctable[one_class, "lower_bound"], 4), " ,", round(ctable[one_class, "upper_bound"], 4), "]", sep = ""))
    result_list[[one_class]][["p-value"]] <- c(result_list[[one_class]][["p-value"]], ctable[one_class, "p-value"])
  }
  
  result_list[["Column_Name"]] <- c(result_list[["Column_Name"]], one_column)
}
###########################

# PCA
###########################
# Covariance
cov_numerical_features <- cov(row_data[, numerical_features])
cov_pca_result <- prcomp(cov_numerical_features)
plot(cov_pca_result, type = "line")
summary(cov_pca_result)
cov_pca_result$rotation

# Correlation
cor_numerical_features <- cor(row_data[, numerical_features])
cor_pca_result <- prcomp(cor_numerical_features)
plot(cor_pca_result)
summary(cor_pca_result)
###########################

# Data Transformation via PCA
###########################


###########################

# Data Split
###########################


###########################

# AutoML via h2o
###########################



###########################

# Mixed Model
###########################




###########################


