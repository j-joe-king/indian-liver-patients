################################
# HarvardX Data Science Capstone Course
# Indian Liver Patient Data Analysis R Script
# Author: Jonathan King
# Date: 7 September 2020
#################################

# SECTION 1 : Prerequisites

# Ensure required R packages are installed
# tidyverse, caret, data.table, lubridate, ggplot

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

# Create subdirectories for .rda files and figures
if (!file.exists("./rdas")){
  dir.create("./rdas")
}

if (!file.exists("./figs")){
  dir.create("./figs")
}

# Function declarations


# SECTION 2 : DOWNLOAD AND CLEAN DATA

# Download data direct from UCI archive and label columns
indianliverpatients <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv",
                                header = FALSE,
                                col.names = c("Age", "Gender",
                                              "Total_Billirubin",
                                              "Direct_Billirubin",
                                              "Alkaline_Phosphotase",
                                              "Alamine_Aminotransferase",
                                              "Aspartate_Aminotransferase",
                                              "Total_Proteins",
                                              "Albumin",
                                              "Albumin_Globulin_Ratio",
                                              "Diagnosis_Liver_Disease"))

# Clean Data

# Inspection of data reveals four patients with missing data: no Albumin/Globulin Ratio present.
# For the missing data, we estimate using the formula for A/G Ratio of Albumin/(Total_Proteins-Albumin)
# Create flag for liver disease Diagnosis_Liver_Disease

ilp <- indianliverpatients %>% mutate(Diagnosis_Liver_Disease = factor((Diagnosis_Liver_Disease == 1)),
                                      Gender = as.factor(Gender),
                                      Albumin_Globulin_Ratio = round(
                                        ifelse(is.na(Albumin_Globulin_Ratio),
                                               Albumin / (Total_Proteins - Albumin),
                                               Albumin_Globulin_Ratio), 2),
                                      AST_ALT_Ratio = round(Aspartate_Aminotransferase / Alamine_Aminotransferase,2))

no_of_patients_with_liver_disease <- sum(ilp$Diagnosis_Liver_Disease == TRUE)
no_of_patients_without_liver_disease <- sum(ilp$Diagnosis_Liver_Disease == FALSE)

# SECTION 3: Split Data into training and test sets ilptrain and ilptest respectively

set.seed(71020, sample.kind = "Rounding")
#if using R3.5 or earlier, use 'set.seed(71020)' instead

test_index <- createDataPartition(y = ilp$Diagnosis_Liver_Disease, times = 1, p = 0.1, list = FALSE)
ilptrain <- ilp[-test_index,]
ilptest <- ilp[test_index,]

# Summarise the data in original dataset (ilp)

data_summary <- ilp %>% summarise(dataset = "Original",
                          n_patients = n(),
                          n_males = sum(Gender == "Male"),
                          n_females = sum(Gender == "Female"),
                          n_males_with_liver_disease = sum(Gender == "Male" & Diagnosis_Liver_Disease == TRUE),
                          n_females_with_liver_disease = sum(Gender == "Female" & Diagnosis_Liver_Disease == TRUE))

# Summarise the data in ilptrain and ilptest
data_summary <- data_summary %>% 
  rbind(ilptrain %>% summarize(dataset = "Training", 
                          n_patients = n(),
                          n_males = sum(Gender == "Male"),
                          n_females = sum(Gender == "Female"),
                          n_males_with_liver_disease = sum(Gender == "Male" & Diagnosis_Liver_Disease == TRUE),
                          n_females_with_liver_disease = sum(Gender == "Female" & Diagnosis_Liver_Disease == TRUE)))  %>%
  rbind(ilptest %>% summarize(dataset = "Testing", 
                              n_patients = n(),
                              n_males = sum(Gender == "Male"),
                              n_females = sum(Gender == "Female"),
                              n_males_with_liver_disease = sum(Gender == "Male" & Diagnosis_Liver_Disease == TRUE),
                              n_females_with_liver_disease = sum(Gender == "Female" & Diagnosis_Liver_Disease == TRUE)))

save(data_summary, file = "./rdas/data_summary.rda")

# We now standarise the data using the mean and standard deviation for non-liver disease patients to
# give the base levels for each sex

base <- ilptrain %>% group_by(Gender) %>% summarise( 
  Avg_ALT = mean(Alamine_Aminotransferase[Diagnosis_Liver_Disease == FALSE]),
  Sd_ALT = sd(Alamine_Aminotransferase[Diagnosis_Liver_Disease == FALSE]),
  Avg_AST = mean(Aspartate_Aminotransferase[Diagnosis_Liver_Disease == FALSE]),
  Sd_AST = sd(Aspartate_Aminotransferase[Diagnosis_Liver_Disease == FALSE]),
  Avg_APT = mean(Alkaline_Phosphotase[Diagnosis_Liver_Disease == FALSE]),
  Sd_APT = sd(Alkaline_Phosphotase[Diagnosis_Liver_Disease == FALSE]),
  Avg_AST_ALT_Ratio = mean(AST_ALT_Ratio[Diagnosis_Liver_Disease == FALSE]),
  Sd_AST_ALT_Ratio = sd(AST_ALT_Ratio[Diagnosis_Liver_Disease == FALSE]),
  Avg_TP = mean(Total_Proteins[Diagnosis_Liver_Disease == FALSE]),
  Sd_TP = sd(Total_Proteins[Diagnosis_Liver_Disease == FALSE]),
  Avg_ALB = mean(Albumin[Diagnosis_Liver_Disease == FALSE]),
  Sd_ALB = sd(Albumin[Diagnosis_Liver_Disease == FALSE]),
  Avg_AGR = mean(Albumin_Globulin_Ratio[Diagnosis_Liver_Disease == FALSE]),
  Sd_AGR = sd(Albumin_Globulin_Ratio[Diagnosis_Liver_Disease == FALSE]),
  Avg_TBIL = mean(Total_Billirubin[Diagnosis_Liver_Disease == FALSE]),
  Sd_TBIL = sd(Total_Billirubin[Diagnosis_Liver_Disease == FALSE]),
  Avg_DBI = mean(Direct_Billirubin[Diagnosis_Liver_Disease == FALSE]),
  sd_DBI = sd(Direct_Billirubin[Diagnosis_Liver_Disease == FALSE]))

save(base, file = "./rdas/base_levels.rda")

z_ilptrain <- ilptrain %>% mutate(
  Z_ALT = (Alamine_Aminotransferase - base$Avg_ALT[Gender]) / base$Sd_ALT[Gender],
  Z_AST = (Aspartate_Aminotransferase - base$Avg_AST[Gender]) / base$Sd_AST[Gender],
  Z_APT = (Alkaline_Phosphotase - base$Avg_APT[Gender]) / base$Sd_APT[Gender],
  Z_AST_ALT_R = (AST_ALT_Ratio - base$Avg_AST_ALT_Ratio[Gender]) / base$Sd_AST_ALT_Ratio[Gender],
  Z_TP = (Total_Proteins - base$Avg_TP[Gender]) / base$Sd_TP[Gender],
  Z_ALB = (Albumin - base$Avg_ALB[Gender]) / base$Sd_ALB[Gender],
  Z_AGR = (Albumin_Globulin_Ratio - base$Avg_AGR[Gender]) / base$Sd_AGR[Gender],
  Z_TBIL = (Total_Billirubin - base$Avg_TBIL[Gender]) / base$Sd_TBIL[Gender],
  Z_DBI = (Direct_Billirubin - base$Avg_DBI[Gender]) / base$sd_DBI[Gender])

# Apply the same standardisation to ilptest (note we are using the base average/sd from ilptrain 

z_ilptest <- ilptest %>% mutate(
  Z_ALT = (Alamine_Aminotransferase - base$Avg_ALT[Gender]) / base$Sd_ALT[Gender],
  Z_AST = (Aspartate_Aminotransferase - base$Avg_AST[Gender]) / base$Sd_AST[Gender],
  Z_APT = (Alkaline_Phosphotase - base$Avg_APT[Gender]) / base$Sd_APT[Gender],
  Z_AST_ALT_R = (AST_ALT_Ratio - base$Avg_AST_ALT_Ratio[Gender]) / base$Sd_AST_ALT_Ratio[Gender],
  Z_TP = (Total_Proteins - base$Avg_TP[Gender]) / base$Sd_TP[Gender],
  Z_ALB = (Albumin - base$Avg_ALB[Gender]) / base$Sd_ALB[Gender],
  Z_AGR = (Albumin_Globulin_Ratio - base$Avg_AGR[Gender]) / base$Sd_AGR[Gender],
  Z_TBIL = (Total_Billirubin - base$Avg_TBIL[Gender]) / base$Sd_TBIL[Gender],
  Z_DBI = (Direct_Billirubin - base$Avg_DBI[Gender]) / base$sd_DBI[Gender])


# to do this)
# Age distribution of data

age_distribution <- ilptrain %>% ggplot(aes(Age, colour = Gender)) + geom_histogram(binwidth = 5) + 
  scale_x_continuous(breaks = seq(0,90,5)) +
  scale_y_continuous(breaks = seq(0,60,5)) + theme(legend.position = "bottom")
age_distribution
ggsave("./figs/age_distribution.png", width = 4, height = 3)

# Exclude from modelling the five-year age bins where the count is less than 15.
# This will set the age range for investigation from 18 to 77.

ilptrain_age_limited <- z_ilptrain %>% filter(Age > 17 & Age < 78)
ilptrain_healthy <- ilptrain_age_limited %>% filter(Diagnosis_Liver_Disease == FALSE)
ilptrain_diagnosed <- ilptrain_age_limited %>% filter(Diagnosis_Liver_Disease == TRUE)

# Plot all variables against Age and Diagnosis_Liver_Disease
alt_age_plot <- ilptrain_age_limited %>% ggplot(aes(Age,Z_ALT, color = Diagnosis_Liver_Disease)) +
  geom_rect(aes(ymin=-1.96,ymax=1.96,xmin=15, xmax=80), color="grey", fill="grey") +
  geom_line(y=0,color="black")+
  geom_jitter() + facet_grid(Gender~.) + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(15,80,5))
alt_age_plot
ggsave("./figs/alt_age_plot.png", width = 4, height = 3)

ast_age_plot <- ilptrain_age_limited %>% ggplot(aes(Age,Z_AST, color = Diagnosis_Liver_Disease)) +
  geom_rect(aes(ymin=-1.96,ymax=1.96,xmin=15, xmax=80), color="grey", fill="grey") +
  geom_line(y=0,color="black")+
  geom_jitter() + facet_grid(Gender~.) + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(15,80,5))
ast_age_plot
ggsave("./figs/ast_age_plot.png", width = 4, height = 3)

ast_alt_ratio_age_plot <- ilptrain_age_limited %>% ggplot(aes(Age,Z_AST_ALT_R, color = Diagnosis_Liver_Disease)) +
  geom_rect(aes(ymin=-1.96,ymax=1.96,xmin=15, xmax=80), color="grey", fill="grey") +
  geom_line(y=0,color="black")+
  geom_jitter() + facet_grid(Gender~.) + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(15,80,5))
ast_alt_ratio_age_plot
ggsave("./figs/ast_alt_ratio_age_plot.png", width = 4, height = 3)

apt_age_plot <- ilptrain_age_limited %>% ggplot(aes(Age,Z_APT, color = Diagnosis_Liver_Disease)) +
  geom_rect(aes(ymin=-1.96,ymax=1.96,xmin=15, xmax=80), color="grey", fill="grey") +
  geom_line(y=0,color="black")+
  geom_jitter() + facet_grid(Gender~.) + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(15,80,5))
apt_age_plot
ggsave("./figs/apt_age_plot.png", width = 4, height = 3)

tp_age_plot <- ilptrain_age_limited %>% ggplot(aes(Age,Z_TP, color = Diagnosis_Liver_Disease)) +
  geom_rect(aes(ymin=-1.96,ymax=1.96,xmin=15, xmax=80), color="grey", fill="grey") +
  geom_line(y=0,color="black")+
  geom_jitter() + facet_grid(Gender~.) + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(15,80,5))
tp_age_plot
ggsave("./figs/tp_age_plot.png", width = 4, height = 3)

alb_age_plot <- ilptrain_age_limited %>% ggplot(aes(Age,Z_ALB, color = Diagnosis_Liver_Disease)) +
  geom_rect(aes(ymin=-1.96,ymax=1.96,xmin=15, xmax=80), color="grey", fill="grey") +
  geom_line(y=0,color="black")+
  geom_jitter() + facet_grid(Gender~.) + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(15,80,5))
alb_age_plot
ggsave("./figs/alb_age_plot.png", width = 4, height = 3)

agr_age_plot <- ilptrain_age_limited %>% ggplot(aes(Age,Z_AGR, color = Diagnosis_Liver_Disease)) +
  geom_rect(aes(ymin=-1.96,ymax=1.96,xmin=15, xmax=80), color="grey", fill="grey") +
  geom_line(y=0,color="black")+
  geom_jitter() + facet_grid(Gender~.) + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(15,80,5))
agr_age_plot
ggsave("./figs/agr_age_plot.png", width = 4, height = 3)

tbil_age_plot <- ilptrain_age_limited %>% ggplot(aes(Age,Z_TBIL, color = Diagnosis_Liver_Disease)) +
  geom_rect(aes(ymin=-1.96,ymax=1.96,xmin=15, xmax=80), color="grey", fill="grey") +
  geom_line(y=0,color="black")+
  geom_jitter() + facet_grid(Gender~.) + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(15,80,5))
tbil_age_plot
ggsave("./figs/tbil_age_plot.png", width = 4, height = 3)

dbi_age_plot <- ilptrain_age_limited %>% ggplot(aes(Age,Z_DBI, color = Diagnosis_Liver_Disease)) +
  geom_rect(aes(ymin=-1.96,ymax=1.96,xmin=15, xmax=80), color="grey", fill="grey") +
  geom_line(y=0,color="black")+
  geom_jitter() + facet_grid(Gender~.) + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(15,80,5))
dbi_age_plot
ggsave("./figs/dbi_age_plot.png", width = 4, height = 3)

# SECTION 4 DEVELOPING THE PREDICTION MODEL

set.seed(301220, sample.kind = "Rounding")

#knn: K Nearest Neighbours model: All Markers

train_knn_all_markers <- train(Diagnosis_Liver_Disease ~ Age+Gender+Z_ALT+Z_AST+Z_APT+Z_TP+Z_ALB+Z_AGR+Z_TBIL+Z_DBI, method = "knn", 
                   data = z_ilptrain,
                   tuneGrid = data.frame(k = seq(3,33,1)))

knn_all_marker_best_k <- ggplot(train_knn_all_markers, highlight = TRUE)
knn_all_marker_best_k
ggsave("./figs/knn_all_marker_best_k.png", width = 3, height = 3)

predictions <- predict(train_knn_all_markers, z_ilptest)
z_ilptest <- z_ilptest %>% mutate(KNN_All_Prediction=predictions)
cf_knn_all <- confusionMatrix(predictions,z_ilptest$Diagnosis_Liver_Disease)
save(train_knn_all_markers, cf_knn_all, file="./rdas/train_knn_all_markers.rda")

#knn: K Nearest Neighbours model: Non-Protein Markers

train_knn_non_protein_markers <- train(Diagnosis_Liver_Disease ~ Age+Gender+Z_ALT+Z_AST+Z_APT+Z_TBIL+Z_DBI, method = "knn", 
                               data = z_ilptrain,
                               tuneGrid = data.frame(k = seq(3,33,1)))

knn_non_protein_marker_best_k <- ggplot(train_knn_non_protein_markers, highlight = TRUE)
knn_non_protein_marker_best_k
ggsave("./figs/knn_non_protein_marker_best_k.png", width = 3, height = 3)

predictions <- predict(train_knn_non_protein_markers, z_ilptest)
z_ilptest <- z_ilptest %>% mutate(KNN_Non_Protein_Prediction=predictions)
cf_knn_non_protein <- confusionMatrix(predictions,z_ilptest$Diagnosis_Liver_Disease)
save(train_knn_non_protein_markers, cf_knn_non_protein, file="./rdas/train_knn_non_protein_markers.rda")

# rf: Random Forest: All Markers
train_rf_all_markers <- train(Diagnosis_Liver_Disease ~ Age+Gender+Z_ALT+Z_AST+Z_APT+Z_TP+Z_ALB+Z_AGR+Z_TBIL+Z_DBI, 
                  method = "rf",                   
                  tuneGrid = data.frame(mtry = seq(2,10)),
                  nodesize = 14, 
                  data = z_ilptrain)
rf_all_markers_best_no_predictors <- ggplot(train_rf_all_markers, highlight = TRUE)
rf_all_markers_best_no_predictors
ggsave("./figs/rf_all_markers_best_no_predictors.png", width = 3, height = 3)

predictions <- predict(train_rf_all_markers,z_ilptest)
z_ilptest <- z_ilptest %>% mutate(RF_All_Prediction=predictions)
cf_rf_all <- confusionMatrix(predictions,z_ilptest$Diagnosis_Liver_Disease)
save(train_rf_all_markers, cf_rf_all, file="./rdas/train_rf_all_markers.rda")

# rf: Random Forest: Non-protein Markers

train_rf_non_protein_markers <- train(Diagnosis_Liver_Disease ~ Age+Gender+Z_ALT+Z_AST+Z_APT+Z_TBIL+Z_DBI, 
                              method = "rf",                   
                              tuneGrid = data.frame(mtry = seq(2,7)), 
                              nodesize = 14,
                              data = z_ilptrain)
rf_non_protein_markers_best_no_predictors <- ggplot(train_rf_non_protein_markers, highlight = TRUE)
rf_non_protein_markers_best_no_predictors
ggsave("./figs/rf_non_protein_markers_best_no_predictors.png", width = 3, height = 3)

predictions <- predict(train_rf_non_protein_markers,z_ilptest)
z_ilptest <- z_ilptest %>% mutate(RF_Non_Protein_Prediction=predictions)
cf_rf_non_protein <- confusionMatrix(predictions,z_ilptest$Diagnosis_Liver_Disease)
save(train_rf_non_protein_markers, cf_rf_non_protein, file="./rdas/train_rf_non_protein_markers.rda")


