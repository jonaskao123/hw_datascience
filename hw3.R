library(randomForest)

preprocess <- function(input_data){
  data <- read.csv(input_data, header = FALSE)
  data[is.na(data)] <- 0
  return(data)
}

k_fold <- function(k, input_file, output_file){
  
  data <- preprocess(input_file)
  
  labels <- factor(data$V2)
  features <- data[, (3:5603)] 
  
  train_accuracy <- vector("numeric", length = k)
  val_accuracy <- vector("numeric", length = k)
  test_accuracy <- vector("numeric", length = k)
  
  fold_size <- floor(nrow(features) / k)
  
  for (i in 1 : k) {
    
    test_indices <- ((i - 1) * fold_size + 1):(i * fold_size) 
    val_indices <- ((i %% k) * fold_size + 1):(((i %% k) + 1) * fold_size)  
    train_indices <- setdiff(1:nrow(data), c(test_indices, val_indices))  
    
    train_data <- data[train_indices, 3:5603]
    val_data <- data[val_indices, 3:5603]
    test_data <- data[test_indices, 3:5603]
    
    train_labels <- labels[train_indices]
    val_labels <- labels[val_indices]
    test_labels <- labels[test_indices]
    
    missing_labels <- setdiff(c("CP", "CW", "EC", "IM"), unique(train_labels))
    if (length(missing_labels) > 0) {
      missing_data <- data[labels %in% missing_labels, 3:5603]
      missing_labels <- labels[labels %in% missing_labels]
      train_data <- rbind(train_data, missing_data)
      train_labels <- c(train_labels, missing_labels)
    }
    
    model <- randomForest(x = train_data, y = train_labels, ntree = 500, mtry = 65, maxnodes = 20)
    
    train_pred <- predict(model, newdata = train_data)
    train_accuracy[i] <- mean(train_pred == train_labels)
    
    val_pred <- predict(model, newdata = val_data)
    val_accuracy[i] <- mean(val_pred == val_labels)
    
    test_pred <- predict(model, newdata = test_data)
    test_accuracy[i] <- mean(test_pred == test_labels)
  }
  
  
  avg_train_accuracy <- mean(train_accuracy)
  avg_val_accuracy <- mean(val_accuracy)
  avg_test_accuracy <- mean(test_accuracy)
  
  performance <- data.frame(
    set = c(paste0("fold", 1:k), "ave."),
    training = c(train_accuracy, avg_train_accuracy),
    validation = c(val_accuracy, avg_val_accuracy),
    test = c(test_accuracy, avg_test_accuracy)
  )
  
  write.csv(performance, file = output_file, row.names = FALSE, quote = F)
  return (model)
}

# Example usage:
model <- k_fold(5, 'HW3_DATA/Archaeal_tfpssm.csv', 'HW3_DATA/performance.csv')
