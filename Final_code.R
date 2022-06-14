# Import Library
###########################
library("openxlsx")
library("ggplot2")
library("GGally")
library("dplyr")
library("multcomp")
library("nnet")
library("lares") # AutoML
library("caret")
library("mixtools") # Use Mixture Model with EM Algorithm
library("UBL") # SMOTE
library("MASS")
###########################

# Set path
setwd("D://OneDrive - gs.ncku.edu.tw//1102½Òµ{//²Î­p¿Ô¸ß")

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
###########################

## Descriptive Statistics
###########################
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
# 
one_column <- numerical_features[1]
for (one_column in numerical_features){
  temp_plot <- ggplot(mapping = aes(x = row_data[, one_column])) +
    geom_histogram(mapping = aes(y = ..density..)) +
    geom_density(alpha = .2, color = "red") +
    scale_x_continuous(one_column) +
    ggtitle(paste("The histogram of", one_column))
  ggsave(temp_plot, file = paste("histogram//histogram_", one_column, ".png", sep = ""))
}

# 
ggpairs(row_data[, c("Area", "Perimeter", "MajorAxisLength", "MinorAxisLength", "Eccentricity", "ConvexArea")],
        mapping = aes(color = row_data[, "Class"]))

# 
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

# 
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

# Plot with Mixture Models
###########################
draw_mixture_model_plot_1d_2 <- function(mixture_model,
                                       one_column){
  comp.norm <- c()
  for (one_k in 1:2){
    comp.norm <- cbind(comp.norm, rnorm(n = 1000, 
                                        mean = mixture_model$mu[one_k],
                                        sd = mixture_model$sigma[one_k]))
  }
  ggplot(mapping = aes(x = row_data[, one_column]))+
    geom_histogram(mapping = aes(y = ..density..))+
    geom_density(alpha = .2, color = "black", lwd = 1.5)+
    geom_density(mapping = aes(x = comp.norm[, 1]), color = "red", lwd = 1.5)+
    geom_density(mapping = aes(x = comp.norm[, 2]), color = "green", lwd = 1.5)+
    scale_x_continuous(one_column)+
    ggtitle(paste("The Mixture Model Result of", one_column, "with K = 2"))
  ggsave(paste("MixtureModelPlot//", one_column, "_1D_K=2.png", sep = ""))
}

draw_mixture_model_plot_1d_3 <- function(mixture_model,
                                         one_column){
  comp.norm <- c()
  for (one_k in 1:3){
    comp.norm <- cbind(comp.norm, rnorm(n = 1000, 
                                        mean = mixture_model$mu[one_k],
                                        sd = mixture_model$sigma[one_k]))
  }
  ggplot(mapping = aes(x = row_data[, one_column]))+
    geom_histogram(mapping = aes(y = ..density..))+
    geom_density(alpha = .2, color = "black", lwd = 1.5)+
    geom_density(mapping = aes(x = comp.norm[, 1]), color = "red", lwd = 1.5)+
    geom_density(mapping = aes(x = comp.norm[, 2]), color = "green", lwd = 1.5)+
    geom_density(mapping = aes(x = comp.norm[, 3]), color = "blue", lwd = 1.5)+
    scale_x_continuous(one_column)+
    ggtitle(paste("The Mixture Model Result of", one_column, "with K = 3"))
  ggsave(paste("MixtureModelPlot//", one_column, "_1D_K=3.png", sep = ""))
}

draw_mixture_model_plot_1d_4 <- function(mixture_model,
                                         one_column){
  comp.norm <- c()
  for (one_k in 1:4){
    comp.norm <- cbind(comp.norm, rnorm(n = 1000, 
                                        mean = mixture_model$mu[one_k],
                                        sd = mixture_model$sigma[one_k]))
  }
  ggplot(mapping = aes(x = row_data[, one_column]))+
    geom_histogram(mapping = aes(y = ..density..))+
    geom_density(alpha = .2, color = "black", lwd = 1.5)+
    geom_density(mapping = aes(x = comp.norm[, 1]), color = "red", lwd = 1.5)+
    geom_density(mapping = aes(x = comp.norm[, 2]), color = "green", lwd = 1.5)+
    geom_density(mapping = aes(x = comp.norm[, 3]), color = "blue", lwd = 1.5)+
    geom_density(mapping = aes(x = comp.norm[, 4]), color = "purple", lwd = 1.5)+
    scale_x_continuous(one_column)+
    ggtitle(paste("The Mixture Model Result of", one_column, "with K = 4"))
  ggsave(paste("MixtureModelPlot//", one_column, "_1D_K=4.png", sep = ""))
}

draw_mixture_model_plot_2d_2 <- function(mixture_model,
                                       two_column){
  comp.norm <- vector(mode = "list", length = 0)
  for (one_k in 1:2){
    comp.norm[[one_k]] <- mvrnorm(n = 1000, 
                                  mu = gm$mu[[one_k]], 
                                  Sigma = gm$sigma[[one_k]])
  }
  ggplot()+
    geom_density_2d(mapping = aes(x = row_data[, two_column[1]],
                                  y = row_data[, two_column[2]]), 
                    color = "black")+
    geom_density_2d(mapping = aes(x = unlist(comp.norm[[1]][, 1]),
                                  y = unlist(comp.norm[[1]][, 2])), 
                    color = "red")+
    geom_density_2d(mapping = aes(x = unlist(comp.norm[[2]][, 1]),
                                  y = unlist(comp.norm[[2]][, 2])), 
                    color = "green")+
    scale_x_continuous(two_column[1])+
    scale_y_continuous(two_column[2])
    ggtitle(paste("The Mixture Model Result of", paste(two_column[1], two_column[2], sep = "-"), "with K = 2"))
  ggsave(paste("MixtureModelPlot//", paste(two_column[1], two_column[2], sep = "-"), "_2D_K=2.png", sep = ""))
}

draw_mixture_model_plot_2d_3 <- function(mixture_model,
                                         two_column){
  comp.norm <- vector(mode = "list", length = 0)
  for (one_k in 1:3){
    comp.norm[[one_k]] <- mvrnorm(n = 1000, 
                                  mu = gm$mu[[one_k]], 
                                  Sigma = gm$sigma[[one_k]])
  }
  ggplot()+
    geom_density_2d(mapping = aes(x = row_data[, two_column[1]],
                                  y = row_data[, two_column[2]]), 
                    color = "black")+
    geom_density_2d(mapping = aes(x = unlist(comp.norm[[1]][, 1]),
                                  y = unlist(comp.norm[[1]][, 2])), 
                    color = "red")+
    geom_density_2d(mapping = aes(x = unlist(comp.norm[[2]][, 1]),
                                  y = unlist(comp.norm[[2]][, 2])), 
                    color = "green")+
    geom_density_2d(mapping = aes(x = unlist(comp.norm[[3]][, 1]),
                                  y = unlist(comp.norm[[3]][, 2])), 
                    color = "green")
    scale_x_continuous(two_column[1])+
    scale_y_continuous(two_column[2])
  ggtitle(paste("The Mixture Model Result of", paste(two_column[1], two_column[2], sep = "-"), "with K = 3"))
  ggsave(paste("MixtureModelPlot//", paste(two_column[1], two_column[2], sep = "-"), "_2D_K=3.png", sep = ""))
}
###########################

# Mixture Model via mixtools
###########################

gm <- mvnormalmixEM(x = row_data[, numerical_features[1:2]], 
                  k = 2, 
                  maxit = 100000)
plot.mixEM(gm, whichplots = 2)
draw_mixture_model_plot_2d_2(mixture_model = gm,
                             two_column = numerical_features[1:2])

# Mixture Model for 1D
mixture_model_result_2 <- vector(mode = "list", length = 0)
mixture_model_result_3 <- vector(mode = "list", length = 0)
mixture_model_result_4 <- vector(mode = "list", length = 0)
for (one_numerical_feature in numerical_features){
  print(paste("Start for", one_numerical_feature, "K = 2"))
  gm <- normalmixEM(x = row_data[, one_numerical_feature], 
                    k = 2, 
                    maxit = 100000)
  draw_mixture_model_plot_1d_2(mixture_model = gm,
                             one_column = one_numerical_feature)
  mixture_model_result_2[[one_numerical_feature]] <- 
    c(gm$mu[1], gm$sigma[1], gm$lambda[1], gm$mu[2], gm$sigma[2], gm$lambda[2])
  
  print(paste("Start for", one_numerical_feature, "K = 3"))
  gm <- normalmixEM(x = row_data[, one_numerical_feature], 
                    k = 3, 
                    maxit = 100000)
  draw_mixture_model_plot_1d_3(mixture_model = gm,
                             one_column = one_numerical_feature)
  mixture_model_result_3[[one_numerical_feature]] <- 
    c(gm$mu[1], gm$sigma[1], gm$lambda[1], 
      gm$mu[2], gm$sigma[2], gm$lambda[2],
      gm$mu[3], gm$sigma[3], gm$lambda[3])
  
  print(paste("Start for", one_numerical_feature, "K = 4"))
  gm <- normalmixEM(x = row_data[, one_numerical_feature], 
                    k = 4, 
                    maxit = 100000)
  draw_mixture_model_plot_1d_4(mixture_model = gm,
                             one_column = one_numerical_feature)
  mixture_model_result_4[[one_numerical_feature]] <- 
    c(gm$mu[1], gm$sigma[1], gm$lambda[1], 
      gm$mu[2], gm$sigma[2], gm$lambda[2],
      gm$mu[3], gm$sigma[3], gm$lambda[3],
      gm$mu[4], gm$sigma[4], gm$lambda[4])
}

# Mixture Model for 2D
mixture_model_result_2D_2 <- vector(mode = "list", length = 0)
mixture_model_result_2D_3 <- vector(mode = "list", length = 0)
mixture_model_result_2D_4 <- vector(mode = "list", length = 0)
for (one_numerical_feature in numerical_features){
  for (two_numerical_feature in numerical_features){
    if (one_numerical_feature != two_numerical_feature){
      print(paste("Start for", paste(one_numerical_feature, two_numerical_feature, sep = "-"), "K = 2"))
      ErrorResult <- tryCatch({
        gm <- mvnormalmixEM(x = row_data[, c(one_numerical_feature, two_numerical_feature)],
                            k = 2,
                            maxit = 100000)

        draw_mixture_model_plot_2d_2(mixture_model = gm,
                                     two_column = c(one_numerical_feature, two_numerical_feature))
        mixture_model_result_2D_2[[paste(one_numerical_feature, two_numerical_feature, sep = "-")]] <-
          c(gm$mu[[1]][1], gm$mu[[1]][2], gm$sigma[[1]][1], gm$sigma[[1]][2], gm$lambda[1],
            gm$mu[[2]][1], gm$mu[[2]][2], gm$sigma[[2]][1], gm$sigma[[2]][2], gm$lambda[2])
      }, error = function(err){
        print(paste(paste(one_numerical_feature, two_numerical_feature, sep = "-"), "is Error"))
      })
      
      print(paste("Start for", paste(one_numerical_feature, two_numerical_feature, sep = "-"), "K = 3"))
      ErrorResult <- tryCatch({
        gm <- mvnormalmixEM(x = row_data[, c(one_numerical_feature, two_numerical_feature)], 
                            k = 3, 
                            maxit = 100000)
        
        draw_mixture_model_plot_2d_2(mixture_model = gm,
                                     two_column = c(one_numerical_feature, two_numerical_feature))
        mixture_model_result_2D_2[[paste(one_numerical_feature, two_numerical_feature, sep = "-")]] <- 
          c(gm$mu[[1]][1], gm$mu[[1]][2], gm$sigma[[1]][1], gm$sigma[[1]][2], gm$lambda[1],
            gm$mu[[2]][1], gm$mu[[2]][2], gm$sigma[[2]][1], gm$sigma[[2]][2], gm$lambda[2],
            gm$mu[[3]][1], gm$mu[[3]][2], gm$sigma[[3]][1], gm$sigma[[3]][2], gm$lambda[3])
      }, error = function(err){
        print(paste(paste(one_numerical_feature, two_numerical_feature, sep = "-"), "K = 3 is Error"))
      })
    }
  }
}

###########################

