summary <- function(input_path, output_path) {
  df <- read.csv(input_path)
  max_height <- which.max(df$height)
  max_weight <- which.max(df$weight)
  max_height <- df[max_height, ]
  max_weight <- df[max_weight, ]
  
  summary_data <- data.frame(set = basename(input_path), 
                             weight = round(max_weight$weight, 2), 
                             height = round(max_height$height, 2))
  
  write.csv(summary_data, output_path, row.names = FALSE)
}

# summary('./example/input1.csv', './example/output1_test.csv')