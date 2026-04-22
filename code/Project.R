#Load main libraries for data handling and cleaning
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
library(caret)
library(car)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)
library(tidytext)
# library(tm)
library(textclean)
library(e1071)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)

#Import datasets
cust_credit<-read_csv("Downloads/0. Data Science In Practice/4.PROJECT BANK CREDIT SCORING/DATASETS/customer credit details.csv")
View(cust_credit)
cust_info<-read_csv("Downloads/0. Data Science In Practice/4.PROJECT BANK CREDIT SCORING/DATASETS/customer info.csv")
View(cust_info)
cust_profiles<-read_csv("Downloads/0. Data Science In Practice/4.PROJECT BANK CREDIT SCORING/DATASETS/customer profiles.csv")
View(cust_profiles)

#Clean column names
names(cust_credit)<-tolower(gsub(" ", "_", names(cust_credit)))
names(cust_info)<-tolower(gsub(" ", "_", names(cust_info)))
names(cust_profiles) <- tolower(gsub(" ", "_", names(cust_profiles)))

nrow(cust_credit)
nrow(cust_info)
nrow(cust_profiles)

sum(duplicated(cust_credit$custid))
sum(duplicated(cust_info$custid))
sum(duplicated(cust_profiles$custid))

cust_credit<-cust_credit[!duplicated(cust_credit$custid), ]

#Merge datasets by custid
master_data<-cust_credit %>%
  inner_join(cust_info, by = "custid") %>%
  inner_join(cust_profiles, by = "custid")
View(master_data)

#Summary of numeric variables
colSums(is.na(master_data))
master_data <- master_data %>%
  filter(
    !is.na(debtinc) & !is.na(othdebt) & !is.na(preloan),
    !(veh %in% c(11, 22)),
    !(selfemp == 11),
    !(deposit == 11),
    !(preloan == 11)
  )
summary(master_data)

#Decoding categorical variables
master_data<-master_data %>%
  mutate(
    gender = dplyr::recode(as.factor(gender), `1` = "Male", `2` = "Female"),
    ms = dplyr::recode(as.factor(ms), `1` = "Single", `2` = "Married"),
    child = dplyr::recode(as.factor(child), `1` = "No Child", `2` = "With Child"),
    house = dplyr::recode(as.factor(house), `1` = "No House", `2` = "Own House"),
    account = dplyr::recode(as.factor(account), `1` = "No Account", `2` = "Has Account"),
    deposit = dplyr::recode(as.factor(deposit), `1` = "No Deposit", `2` = "Has Deposit"),
    preloan = dplyr::recode(as.factor(preloan), `1` = "No Prior Loan", `2` = "Prior Loan"),
    selfemp = dplyr::recode(as.factor(selfemp), `1` = "No", `2` = "Yes"),
    veh = dplyr::recode(as.factor(veh), `1` = "No Vehicle", `2` = "Has Vehicle")
  )

#Categorize debtinc
master_data<-master_data %>%
  mutate(debtinc_cat = case_when(
    debtinc < 10 ~ "Low",
    debtinc >= 10 & debtinc < 20 ~ "Medium",
    debtinc >= 20 ~ "High",
    TRUE ~ NA_character_
  ))
table(master_data$debtinc_cat)

#Compute default rate
master_data$bad<-as.factor(master_data$bad)
bad_rate<-mean(master_data$bad == 1) * 100
paste("Bad Rate:", round(bad_rate, 2), "%")

#Choose numeric variables
numeric_vars<-c("debtinc", "creddebt", "othdebt", "emp", "address")

#Outlier detection
long_data<-master_data %>%
  select(debtinc, creddebt, othdebt) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(long_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "debtinc" = "#4A77FF",
    "creddebt" = "#FF5733",
    "othdebt" = "#FDE9C9"
  )) +
  labs(title = "Boxplot of key debt variables", x = "Variable", y = "Value") +
  theme_minimal()

#Convert to long format
long_numeric<-master_data %>%
  select(all_of(numeric_vars), bad) %>%
  pivot_longer(cols = -bad, names_to = "variable", values_to = "value")

#Building a boxplot
ggplot(long_numeric, aes(x = as.factor(bad), y = value, fill = as.factor(bad))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("0" = "#FF5733", "1" = "#4A77FF")) +
  labs(
    title = "Boxplots of Key Numeric Variables by Loan Default Status",
    x = "Loan Default (Bad: 0 = No, 1 = Yes)",
    y = "Value"
  ) +
  theme_minimal()

#Table of “Bad Rate” for categorical variables
categ_vars<-c("gender", "zone", "ms", "child", "house", "veh", "selfemp", "account", "deposit", "preloan")
categ_vars[!categ_vars %in% names(master_data)]
get_bad_rate<-function(var) {
  master_data %>%
    group_by(across(all_of(var))) %>%
    summarise(
      Total = n(),
      Bad = sum(bad == 1),
      Bad_Rate = round(Bad / Total * 100, 2)
    ) %>%
    rename(!!var := 1)
}

bad_rate_tables<-lapply(categ_vars, get_bad_rate)
names(bad_rate_tables)<-categ_vars
for (v in categ_vars) {
  cat("\n=== Bad Rate by", v, "===\n")
  print(bad_rate_tables[[v]])
}

#Constructing a barplot for each variable
for (v in categ_vars) {
  data_plot<-bad_rate_tables[[v]]
  
  ggplot(data_plot, aes(x = factor(!!sym(v)), y = Bad_Rate, fill = factor(!!sym(v)))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(title = paste("Bad Rate by", v),
         x = v, y = "Bad Rate (%)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold")) +
    scale_fill_viridis_d() -> p
  
  print(p)
}

#Heatmap zone vs ms
heat_data1<-master_data %>%
  group_by(zone, ms) %>%
  summarise(
    Total = n(),
    Bad = sum(bad == 1, na.rm = TRUE),
    Bad_Rate = round(Bad / Total * 100, 2)
  ) %>%
  ungroup()
ggplot(heat_data1, aes(x = factor(zone), y = ms, fill = Bad_Rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Bad_Rate),
            color = "black", size = 4,
            angle = 90, vjust = 0.5, hjust = 0.5) +
  scale_fill_gradientn(
    colours = c("#3F75FF", "#FDE9C9", "#FF6D7A")
  ) +
  labs(
    title = "Heatmap of Bad Rate by Zone and Marital Status (ms)",
    x = "Zone",
    y = "Marital Status",
    fill = "Bad Rate (%)"
  ) +
  theme_minimal()

#Heatmap zone vs child
heat_data2<-master_data %>%
  group_by(zone, child) %>%
  summarise(
    Total = n(),
    Bad = sum(bad == 1, na.rm = TRUE),
    Bad_Rate = round(Bad / Total * 100, 2)
  ) %>%
  ungroup()
ggplot(heat_data2, aes(x = factor(zone), y = child, fill = Bad_Rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Bad_Rate),
            color = "black", size = 4,
            angle = 90, vjust = 0.5, hjust = 0.5) +
  scale_fill_gradientn(
    colours = c("#3F75FF", "#FDE9C9", "#FF6D7A")
  ) +
  labs(
    title = "Heatmap of Bad Rate by Zone and Child",
    x = "Zone",
    y = "Child",
    fill = "Bad Rate (%)"
  ) +
  theme_minimal()

#Create data partition into train and test data sets
set.seed(123)
train_index<-createDataPartition(master_data$bad, p = 0.8, list = FALSE)
train_data<-master_data[train_index, ]
test_data<-master_data[-train_index, ]

#Constructing logistic regression Model 1
model1<-glm(bad ~ ., data = train_data %>% 
        select(-custid), 
        family = "binomial")
summary(model1)

#Constructing logistic regression Model 2
model2<-glm(bad ~ creddebt + emp + address + branch + debtinc_cat, 
              data = train_data, 
              family = "binomial")
summary(model2)

#Check multicollinearity
vif(model2)

#Obtain ROC curve and AUC for train data Model 2
train_pred_prob<-predict(model2, newdata = train_data, type = "response")
roc_obj<-roc(train_data$bad, train_pred_prob)
plot(roc_obj, 
     main = "ROC Curve - Train Data (Model 2)", 
     col = "#4A77FF", 
     lwd = 2)
auc_train<-auc(roc_obj)
print(paste("AUC (Train, Model 2):", round(auc_train, 4), "\n"))

#Obtain classification table and accuracy (%) (Train Data, Model 2)
opt_threshold<-as.numeric(coords(roc_obj, 
                                 x = "best", 
                                 best.method = "closest.topleft")["threshold"])
train_pred_class_opt<-ifelse(train_pred_prob > opt_threshold, 1, 0)
conf_matrix_opt<-table(Predicted = train_pred_class_opt, Actual = train_data$bad)
print(conf_matrix_opt)

#True Positives (TP)
TP_train<-conf_matrix_opt["1", "1"]
#True Negatives (TN)
TN_train<-conf_matrix_opt["0", "0"]
#False Positives (FP)
FP_train<-conf_matrix_opt["1", "0"]
#False Negatives (FN)
FN_train<-conf_matrix_opt["0", "1"]

accuracy_train<-(TP_train + TN_train)/sum(conf_matrix_opt)
cat("Accuracy (Train, Model 2):", round(accuracy_train * 100, 2), "%\n")

sensitivity_train<-TP_train/(TP_train + FN_train)
cat("Sensitivity (Train, Model 2):", round(sensitivity_train * 100, 2), "%\n")

specificity_train<-TN_train/(TN_train + FP_train)
cat("Specificity (Train, Model 2):", round(specificity_train * 100, 2), "%\n")

#Obtain ROC curve and AUC for test data Model 2
test_pred_prob<-predict(model2, newdata = test_data, type = "response")
roc_obj_test<-roc(test_data$bad, test_pred_prob)
plot(roc_obj_test, 
     main = "ROC Curve - Test Data (Model 2)", 
     col = "#FF6D7A", 
     lwd = 2)
auc_test<-auc(roc_obj_test)
cat("AUC (Test, Model 2):", round(auc_test, 4), "\n")

#Obtain accuracy, sensitivity and specificity for test data Model 2
test_pred_class<-ifelse(test_pred_prob > opt_threshold, 1, 0)
conf_matrix_test<-table(Predicted = test_pred_class, Actual = test_data$bad)
print(conf_matrix_test)

TP_test<-conf_matrix_test["1", "1"]
TN_test<-conf_matrix_test["0", "0"]
FP_test<-conf_matrix_test["1", "0"]
FN_test<-conf_matrix_test["0", "1"]

accuracy_test<-(TP_test + TN_test)/sum(conf_matrix_test)
cat("Accuracy (Test, Model 2):", round(accuracy_test * 100, 2), "%\n")

sensitivity_test<-TP_test/(TP_test + FN_test)
cat("Sensitivity (Test, Model 2):", round(sensitivity_test * 100, 2), "%\n")

specificity_test<-TN_test/(TN_test + FP_test)
cat("Specificity (Test, Model 2):", round(specificity_test * 100, 2), "%\n")

#Alternative model
#Constructing logistic regression Model 3 (numeric debtinc)
model3<-glm(bad ~ debtinc + creddebt + emp + address + branch, 
              data = train_data, 
              family = "binomial")
summary(model3)

#Check multicollinearity
vif(model3)

#Obtain ROC curve and AUC for train data Model 3
train_pred_prob3<-predict(model3, newdata = train_data, type = "response")
roc_obj3<-roc(train_data$bad, train_pred_prob3)
plot(roc_obj3, 
     main = "ROC Curve - Train Data (Model 3)", 
     col = "#4A77FF", 
     lwd = 2)
auc_train3<-auc(roc_obj3)
cat("AUC (Train, Model 3):", round(auc_train3, 4), "\n")

#Obtain classification table and accuracy (%) (Train Data, Model 3)
opt_threshold3<-as.numeric(coords(roc_obj3, 
                                    x = "best", 
                                    best.method = "closest.topleft")[["threshold"]])

train_pred_class3<-ifelse(train_pred_prob3 > opt_threshold3, 1, 0)
conf_matrix_train3<-table(Predicted = train_pred_class3, Actual = train_data$bad)
print(conf_matrix_train3)

TP_train3<-conf_matrix_train3["1", "1"]
TN_train3<-conf_matrix_train3["0", "0"]
FP_train3<-conf_matrix_train3["1", "0"]
FN_train3<-conf_matrix_train3["0", "1"]

accuracy_train3<-(TP_train3 + TN_train3)/sum(conf_matrix_train3)
cat("Accuracy (Train, Model 3):", round(accuracy_train3 * 100, 2), "%\n")

sensitivity_train3<-TP_train3/(TP_train3 + FN_train3)
cat("Sensitivity (Train, Model 3):", round(sensitivity_train3 * 100, 2), "%\n")

specificity_train3<-TN_train3/(TN_train3 + FP_train3)
cat("Specificity (Train, Model 3):", round(specificity_train3 * 100, 2), "%\n")

#Obtain ROC curve and AUC for test data Model 3
test_pred_prob3<-predict(model3, newdata = test_data, type = "response")
roc_obj_test3<-roc(test_data$bad, test_pred_prob3)
plot(roc_obj_test3, 
     main = "ROC Curve - Test Data (Model 3)", 
     col = "#FF6D7A", 
     lwd = 2)
auc_test3<-auc(roc_obj_test3)
cat("AUC (Test, Model 3):", round(auc_test3, 4), "\n")

#Obtain accuracy, sensitivity and specificity for test data Model 3
test_pred_class3<-ifelse(test_pred_prob3 > opt_threshold3, 1, 0)
conf_matrix_test3<-table(Predicted = test_pred_class3, Actual = test_data$bad)
print(conf_matrix_test3)

TP_test3<-conf_matrix_test3["1", "1"]
TN_test3<-conf_matrix_test3["0", "0"]
FP_test3<-conf_matrix_test3["1", "0"]
FN_test3<-conf_matrix_test3["0", "1"]

accuracy_test3<-(TP_test3 + TN_test3)/sum(conf_matrix_test3)
cat("Accuracy (Test, Model 3):", round(accuracy_test3 * 100, 2), "%\n")

sensitivity_test3<-TP_test3/(TP_test3 + FN_test3)
cat("Sensitivity (Test, Model 3):", round(sensitivity_test3 * 100, 2), "%\n")

specificity_test3<-TN_test3/(TN_test3 + FP_test3)
cat("Specificity (Test, Model 3):", round(specificity_test3 * 100, 2), "%\n")

#Naive Bayes model
nb_model<-naiveBayes(bad ~ ., data = train_data %>% select(-custid))

#Obtain ROC curve and AUC for train data (Naive Bayes)
nb_pred_prob_train<-predict(nb_model, train_data, type = "raw")[, 2]
nb_roc_train<-roc(train_data$bad, nb_pred_prob_train)
plot(nb_roc_train, 
     main = "ROC Curve – Naive Bayes (Train Data)", 
     col = "#4A77FF", 
     lwd = 2)
auc_nb_train<-auc(nb_roc_train)
cat("AUC (Train, Naive Bayes):", round(auc_nb_train, 4), "\n")

#Obtain Confusion Matrix for train data (Naive Bayes)
opt_threshold_nb<-as.numeric(coords(nb_roc_train, 
                                      x = "best", 
                                      best.method = "closest.topleft")["threshold"])
nb_pred_class_train<-ifelse(nb_pred_prob_train > opt_threshold_nb, 1, 0)

conf_matrix_nb_train<-table(Predicted = nb_pred_class_train, Actual = train_data$bad)
print(conf_matrix_nb_train)

TP<-conf_matrix_nb_train["1", "1"]
TN<-conf_matrix_nb_train["0", "0"]
FP<-conf_matrix_nb_train["1", "0"]
FN<-conf_matrix_nb_train["0", "1"]

accuracy<-(TP + TN)/sum(conf_matrix_nb_train)
cat("Accuracy (Train, Naive Bayes):", round(accuracy * 100, 2), "%\n")

sensitivity<-TP/(TP + FN)
cat("Sensitivity (Train, Naive Bayes):", round(sensitivity * 100, 2), "%\n")

specificity<-TN/(TN + FP)
cat("Specificity (Train, Naive Bayes):", round(specificity * 100, 2), "%\n")

#Obtain ROC curve and AUC for test data (Naive Bayes)
nb_pred_prob_test<-predict(nb_model, test_data, type = "raw")[, 2]
roc_nb_test<-roc(test_data$bad, nb_pred_prob_test)
plot(roc_nb_test, 
     main = "ROC Curve - Test Data (Naive Bayes)", 
     col = "#FF6D7A", 
     lwd = 2)
auc_nb_test<-auc(roc_nb_test)
cat("AUC (Test, Naive Bayes):", round(auc_nb_test, 4), "\n")

#Obtain Confusion Matrix for test data (Naive Bayes)
nb_threshold<-as.numeric(
  coords(nb_roc_train, x = "best", best.method = "closest.topleft")[["threshold"]]
)
nb_pred_class_test<-ifelse(nb_pred_prob_test > nb_threshold, 1, 0)

conf_matrix_nb_test<-table(Predicted = nb_pred_class_test, Actual = test_data$bad)
print(conf_matrix_nb_test)

TP<-conf_matrix_nb_test["1", "1"]
TN<-conf_matrix_nb_test["0", "0"]
FP<-conf_matrix_nb_test["1", "0"]
FN<-conf_matrix_nb_test["0", "1"]

accuracy<-(TP + TN)/sum(conf_matrix_nb_test)
cat("Accuracy (Test, Naive Bayes):", round(accuracy * 100, 2), "%\n")

sensitivity<-TP/(TP + FN)
cat("Sensitivity (Test, Naive Bayes):", round(sensitivity * 100, 2), "%\n")

specificity<-TN/(TN + FP)
cat("Specificity (Test, Naive Bayes):", round(specificity * 100, 2), "%\n")

#Obtain ROC curve and AUC for train data (Decision Tree)
dt_model<-rpart(
  bad ~ ., 
  data = train_data %>% select(-custid),
  method = "class",
  parms = list(prior = c(0.5, 0.5)),
  control = rpart.control(cp = 0.01, minsplit = 20)
)
rpart.plot(
  dt_model,
  main = "Decision Tree",
  box.palette = c("#4A77FF", "#FF6D7A"),
  shadow.col = "gray80",
  nn = TRUE,
  type = 4,
  extra = 104,
  fallen.leaves = TRUE
)

dt_pred_prob_train<-predict(dt_model, newdata = train_data, type = "prob")[, "1"]
roc_dt_train<-roc(train_data$bad, dt_pred_prob_train)
plot(roc_dt_train, 
     main = "ROC Curve – Decision Tree (Train Data)", 
     col = "#4A77FF", 
     lwd = 2)
auc_dt_train<-auc(roc_dt_train)
cat("AUC (Train, Decision Tree):", round(auc_dt_train, 4), "\n")

#Obtain Confusion Matrix for Decision Tree (Train Data)
dt_pred_class_train<-predict(dt_model, newdata = train_data, type = "class")
conf_matrix_dt_train<-table(Predicted = dt_pred_class_train, Actual = train_data$bad)
print(conf_matrix_dt_train)

TP_train<-conf_matrix_dt_train["1", "1"]
TN_train<-conf_matrix_dt_train["0", "0"]
FP_train<-conf_matrix_dt_train["1", "0"]
FN_train<-conf_matrix_dt_train["0", "1"]

accuracy_train<-(TP_train + TN_train)/sum(conf_matrix_dt_train)
cat("Accuracy (Train, Decision Tree):", round(accuracy_train * 100, 2), "%\n")

sensitivity_train<-TP_train/(TP_train + FN_train)
cat("Sensitivity (Train, Decision Tree):", round(sensitivity_train * 100, 2), "%\n")

specificity_train<-TN_train/(TN_train + FP_train)
cat("Specificity (Train, Decision Tree):", round(specificity_train * 100, 2), "%\n")

#Obtain ROC curve and AUC for Decision Tree (Test Data)
dt_pred_prob_test<-predict(dt_model, newdata = test_data, type = "prob")[, "1"]
roc_dt_test<-roc(test_data$bad, dt_pred_prob_test)
plot(roc_dt_test, 
     main = "ROC Curve – Decision Tree (Test Data)", 
     col = "#FF6D7A", 
     lwd = 2)
auc_dt_test<-auc(roc_dt_test)
cat("AUC (Test, Decision Tree):", round(auc_dt_test, 4), "\n")

#Obtain Confusion Matrix for Decision Tree (Test Data)
dt_pred_class_test<-predict(dt_model, newdata = test_data, type = "class")
conf_matrix_dt_test<-table(Predicted = dt_pred_class_test, Actual = test_data$bad)
print(conf_matrix_dt_test)

TP_test<-conf_matrix_dt_test["1", "1"]
TN_test<-conf_matrix_dt_test["0", "0"]
FP_test<-conf_matrix_dt_test["1", "0"]
FN_test<-conf_matrix_dt_test["0", "1"]

accuracy_test<-(TP_test + TN_test)/sum(conf_matrix_dt_test)
cat("Accuracy (Test, Decision Tree):", round(accuracy_test * 100, 2), "%\n")

sensitivity_test<-TP_test/(TP_test + FN_test)
cat("Sensitivity (Test, Decision Tree):", round(sensitivity_test * 100, 2), "%\n")

specificity_test<-TN_test/(TN_test + FP_test)
cat("Specificity (Test, Decision Tree):", round(specificity_test * 100, 2), "%\n")


#Obtain ROC curve and AUC for Random Forest model (Train Data)
set.seed(123)
rf_model<-randomForest(
  as.factor(bad) ~ .,
  data = train_data %>% select(-custid),
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

rf_pred_prob_train<-predict(rf_model, newdata = train_data, type = "prob")[, "1"]
roc_rf_train<-roc(train_data$bad, rf_pred_prob_train)
plot(roc_rf_train, 
     main = "ROC Curve – Random Forest (Train Data)", 
     col = "#7B4F9D", 
     lwd = 2)
auc_rf_train<-auc(roc_rf_train)
cat("AUC (Train, Random Forest):", round(auc_rf_train, 4), "\n")

#Obtain Confusion Matrix for Random Forest model (Train Data)
rf_pred_class_train<-predict(rf_model, newdata = train_data, type = "response")
conf_matrix_rf_train<-table(Predicted = rf_pred_class_train, Actual = train_data$bad)
print(conf_matrix_rf_train)

TP_train<-conf_matrix_rf_train["1", "1"]
TN_train<-conf_matrix_rf_train["0", "0"]
FP_train<-conf_matrix_rf_train["1", "0"]
FN_train<-conf_matrix_rf_train["0", "1"]

accuracy_train<-(TP_train + TN_train)/sum(conf_matrix_rf_train)
cat("Accuracy (Train, Random Forest):", round(accuracy_train * 100, 2), "%\n")

sensitivity_train<-TP_train/(TP_train + FN_train)
cat("Sensitivity (Train, Random Forest):", round(sensitivity_train * 100, 2), "%\n")

specificity_train<-TN_train/(TN_train + FP_train)
cat("Specificity (Train, Random Forest):", round(specificity_train * 100, 2), "%\n")

#Obtain ROC curve and AUC for Random Forest model (Test Data)
rf_pred_prob_test<-predict(rf_model, newdata = test_data, type = "prob")[, "1"]
roc_rf_test<-roc(test_data$bad, rf_pred_prob_test)
plot(roc_rf_test, 
     main = "ROC Curve – Random Forest (Test Data)", 
     col = "#FB6A4A", 
     lwd = 2)
auc_rf_test<-auc(roc_rf_test)
cat("AUC (Test, Random Forest):", round(auc_rf_test, 4), "\n")

#Obtain Confusion Matrix for Random Forest model (Test Data)
rf_pred_class_test<-predict(rf_model, newdata = test_data, type = "response")
conf_matrix_rf_test<-table(Predicted = rf_pred_class_test, Actual = test_data$bad)
print(conf_matrix_rf_test)

TP_test<-conf_matrix_rf_test["1", "1"]
TN_test<-conf_matrix_rf_test["0", "0"]
FP_test<-conf_matrix_rf_test["1", "0"]
FN_test<-conf_matrix_rf_test["0", "1"]

accuracy_test<-(TP_test + TN_test)/sum(conf_matrix_rf_test)
cat("Accuracy (Test, Random Forest):", round(accuracy_test * 100, 2), "%\n")

sensitivity_test<-TP_test/(TP_test + FN_test)
cat("Sensitivity (Test, Random Forest):", round(sensitivity_test * 100, 2), "%\n")

specificity_test<-TN_test/(TN_test + FP_test)
cat("Specificity (Test, Random Forest):", round(specificity_test * 100, 2), "%\n")

#Text Pre-processing
feedback_data<-read_lines("Downloads/0. Data Science In Practice/4.PROJECT BANK CREDIT SCORING/DATASETS/Customer Feedback.txt")

feedback_df<-data.frame(text = feedback_data, stringsAsFactors = FALSE)

clean_feedback<-feedback_df %>%
  mutate(text = replace_contraction(text)) %>%
  mutate(text = replace_symbol(text)) %>%
  mutate(text = tolower(text)) %>%
  mutate(text = removePunctuation(text)) %>%
  mutate(text = removeNumbers(text)) %>%
  mutate(text = removeWords(text, stopwords("en")))

data("stop_words")
tokens<-clean_feedback %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
View(tokens)

tokens %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#4A77FF") +
  coord_flip() +
  labs(title = "Top 20 Most Frequent Words in Customer Feedback",
       x = "Word", 
       y = "Frequency") +
  theme_minimal()

#Word Cloud
word_freq<-tokens %>%
  count(word, sort = TRUE)

set.seed(123)
wordcloud(
  words = word_freq$word,
  freq = word_freq$n,
  min.freq = 3,
  max.words = 100,
  scale = c(5, 0.8),
  random.order = FALSE,
  rot.per = 0.2,
  family = "Helvetica",
  colors = viridis::viridis(8)
)

#Sentiment Analysis
sentiment_scores<-get_nrc_sentiment(feedback_df$text)
sentiment_df<-cbind(feedback_df, sentiment_scores)
avg_sentiment<-colSums(sentiment_scores[, 1:8])

#Visualizing Sentiment Scores
sentiment_plot<-data.frame(
  sentiment = names(avg_sentiment),
  score = as.numeric(avg_sentiment)
)

positive_emotions<-c("joy", "trust", "anticipation", "surprise")
negative_emotions<-c("anger", "disgust", "fear", "sadness")

sentiment_plot<-sentiment_plot %>%
  mutate(type = ifelse(sentiment %in% positive_emotions, "Positive", "Negative"))

ggplot(sentiment_plot, aes(x = reorder(sentiment, score), y = score, fill = type)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("Positive" = "#4A77FF", "Negative" = "#FF6D7A"),
    name = "Emotion Type"
  ) +
  labs(
    title = "Sentiment Scores from Customer Feedback",
    x = "Sentiment", y = "Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )