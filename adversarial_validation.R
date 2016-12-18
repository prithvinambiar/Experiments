#library(pROC)

adversarial_validation = function(train, test){

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
  
  #roccurve <- roc(actual_data, predicted_data)
  #plot(roccurve, col="dark red")
  
  modified_train['Predicted_TARGET'] <- predicted_data[1:nrow(modified_train)]
  
  train_ordered <- order(modified_train['Predicted_TARGET'], decreasing=TRUE)
  validation_train_index <- train_ordered[1:(length(train_ordered)*.2)]
  train_index <- train_ordered[((length(train_ordered)*.2)+1):length(train_ordered)]
  list(train[train_index, ], train[validation_train_index, ])
}


#train <- read.csv("C:/D/New_folder/personal/prithvi/2016/DataScience/Numerai/17_12_2016/numerai_training_data.csv")
#test <- read.csv("C:/D/New_folder/personal/prithvi/2016/DataScience/Numerai/17_12_2016/numerai_tournament_data.csv")
# USAGE -
#  res <- adversarial_validation(train, test)
#  train <- res[0]
#  validation <- res[1]

