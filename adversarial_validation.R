library(pROC)

train <- read.csv("C:/D/New_folder/personal/prithvi/2016/DataScience/Numerai/17_12_2016/numerai_training_data.csv")
test <- read.csv("C:/D/New_folder/personal/prithvi/2016/DataScience/Numerai/17_12_2016/numerai_tournament_data.csv")

train_col_names <- colnames(train)
test_col_names <- colnames(test)
common_attr_train <- intersect(train_col_names, test_col_names)

modified_train <- train[, common_attr_train]
modified_train['TARGET'] <- 0
modified_test <- test[, common_attr_train]
modified_test['TARGET'] <- 1

all_data <- rbind(modified_train, modified_test)

model <- glm(TARGET~., family = binomial(link='logit'), data = all_data)


predicted_data <- predict(model, type = 'response')
actual_data <- all_data$TARGET

roccurve <- roc(actual_data, predicted_data)
plot(roccurve, col="dark red")
