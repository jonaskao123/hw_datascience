calculate <- function(target, badthre, input, output) {
  read_multiple_files <- function(methods) {
    methods <- unlist(strsplit(methods, " "))
    data <- lapply(methods, function(method) read.csv(method))
    names(data) <- methods
    return(data)
  }
  
  method_data <- read_multiple_files(input)
  
  results <- lapply(method_data, function(data) {
    if (target == "bad") {
      filtered_data <- subset(data, pred.score >= badthre)
      nonfiltered_data <- subset(data, pred.score < badthre)
      TP <- subset(filtered_data, reference == target)
      FN <- subset(filtered_data, reference != target)
      TN <- subset(nonfiltered_data, reference != target)
      FP <- subset(nonfiltered_data, reference == target)
      null_P <- sum(data$reference == "bad") / nrow(data)
      
      sensitivity <- round(sum(filtered_data$reference == target) / sum(data$reference == target), 2)
      specificity <- 1 - round(sum(filtered_data$reference != target) / sum(data$reference != target), 2)
      precision <- round(sum(filtered_data$reference == target) / nrow(filtered_data), 2)
      F1 <- round(2 * sensitivity * precision / (sensitivity + precision), 2)
      logLikelihood <- round(sum(log(c(TP$pred.score, 1 - FN$pred.score, 1 - TN$pred.score, FP$pred.score))), 2)
      logLikelihood_null <- with(data, sum(ifelse(reference == "bad", log(null_P), log(1-null_P))))
      pseudoR2 <- round(1 -  logLikelihood / logLikelihood_null, 2)
      return(c(sensitivity = sensitivity, specificity = specificity, F1 = F1, logLikelihood = logLikelihood, pseudoRsquared = pseudoR2))
    } else {
      filtered_data <- subset(data, pred.score < badthre)
      nonfiltered_data <- subset(data, pred.score >= badthre)
      TP <- subset(filtered_data, reference == target)
      FN <- subset(filtered_data, reference != target)
      TN <- subset(nonfiltered_data, reference != target)
      FP <- subset(nonfiltered_data, reference == target)
      null_P <- sum(data$reference == "bad") / nrow(data)
      
      sensitivity <- round(sum(filtered_data$reference == target) / sum(data$reference == target), 2)
      specificity <- 1 - round(sum(filtered_data$reference != target) / sum(data$reference != target), 2)
      precision <- round(sum(filtered_data$reference == target) / nrow(filtered_data), 2)
      F1 <- round(round(2 * sensitivity * precision / (sensitivity + precision), 3), 2)
      logLikelihood <- round(sum(log(c(1 - TP$pred.score, FN$pred.score, TN$pred.score, 1 - FP$pred.score))), 2)
      logLikelihood_null <- with(data, sum(ifelse(reference == "bad", log(null_P), log(1-null_P))))
      pseudoR2 <- round(1 -  logLikelihood / logLikelihood_null, 2)
      return(c(sensitivity = sensitivity, specificity = specificity, F1 = F1, logLikelihood = logLikelihood, pseudoRsquared = pseudoR2))
    }
    
  })
  
  results_df <- do.call(rbind, results)
  method <- gsub("\\.csv", "", names(method_data)) 
  results_df <- cbind(method, results_df)
  results_df <- data.frame(results_df)

  results_df <- rbind(results_df, c("best", results_df$method[sapply(results_df[, -1], which.max)]))
  write.csv(results_df, file = output, quote = F, row.names = F)
}

# calculate("bad", 0.5, "method1.csv method2.csv", "output1.csv")
# calculate("bad", 0.4, "method1.csv method3.csv method5.csv", "output2.csv")
# calculate("good", 0.6, "method2.csv method4.csv method6.csv", "output3.csv")
                    