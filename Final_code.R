# Import Library
###########################
library("openxlsx")
library("ggplot2")
library("dplyr")
library("multcomp")
library("nnet")
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


###########################

# Preprocessing

# Define Features
###########################
target <- "Class"
all_columns <- colnames(row_data)
numerical_features <- all_columns[which(all_columns != target)]

###########################

# Preprocessing
###########################
row_data[, target] <- as.factor(row_data[, target])

###########################

# Handle Outliers - 把 Outliers 轉換成 missing data
###########################



###########################


## Descriptive Statistics
###########################
unique_class <- unique(row_data[, target])

# 1. 建立第一列表示出每個類別名稱
first_colunm_names <- c("")
for (one_column in unique_class){
  first_colunm_names <- c(first_colunm_names, 
                          paste(one_column, " (n = ", 
                                as.character( dim(row_data %>% filter(Class == one_column))[1] ), ")", sep = ""))
}
first_colunm_names <- c(first_colunm_names, "p-value")

# 2. 定義「mean (sd)」的欄位名稱
second_column_names <- c("", rep("mean (sd)", length(unique_class)), "")

# 3. 把每個變數的欄位、平均數、標準差與檢定結果變成一個 Row
content_column <- vector(mode = "list", length = length(unique_class)+1)
names(content_column) <- c("Column Name", unique_class)

for (one_column in numerical_features){
  for (one_class in unique_class){
    select_class_value <- row_data %>% filter(Class == one_class)
    select_class_value <- select_class_value[, one_column]
    mean_value <- mean(select_class_value)
    sd_value <- sd(select_class_value)
    content_column[[one_class]] <- c(content_column[[one_class]], 
                                   paste(round(mean_value, 4), " (", round(sd_value, 4), ")", sep = ""))
  }
  content_column[["Column Name"]] <- c(content_column[["Column Name"]], one_column)
}

content_column <- data.frame(content_column)
write.csv(content_column, "Descriptive_Statistics.csv")
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
par(mfrow = c(1, 1))
plot(row_data[, c("Area", "Perimeter", "MajorAxisLength", "MinorAxisLength", "Eccentricity", "ConvexArea")])

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



###########################


# Logistic Regression
###########################




###########################






