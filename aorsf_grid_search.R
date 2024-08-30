sim_data <- readRDS("sim_survival_data.rds")
head(sim_data$data)

library(aorsf)
library(survival)
library(caret)

cross_validate_aorsf <- function(data, k, n_tree, n_split, n_retry, mtry, split_rule){
  
  folds <- createFolds(data$y, k = k, list = TRUE, returnTrain = TRUE)
  c_index_values <- c()
  
  for (i in 1:k){
    train_data <- data[folds[[i]], ]
    test_data <- data[-folds[[i]], ]
    
    aorsf_fit <- orsf(data = train_data,
                      formula = Surv(y, failed) ~ . ,
                      n_tree = n_tree,
                      n_split = n_split,
                      n_retry = n_retry,
                      mtry = mtry,
                      split_rule = split_rule)
    
    test_predictions <- predict(aorsf_fit, new_data = test_data, pred_type = "risk")
    
    c_index <- survConcordance(Surv(test_data$y, test_data$failed) ~ test_predictions)$concordance
    
    c_index_values <- c(c_index_values, c_index)
  }
  return(mean(c_index_values))
}



n_tree_values <- c(250, 500, 1000, 2000)
n_split_values <- c(3, 5, 10, 15)
n_retry_values <- c(0, 1, 2, 3)
mtry_values <- c(1, 2)
split_rule_values <- c("logrank", "cstat")

best_cindex <- 0
best_parameters <- list()

for (n_tree in n_tree_values){
  for (n_split in n_split_values){
    for (n_retry in n_retry_values){
      for (mtry in mtry_values){
        for (split_rule in split_rule_values){
          
          set.seed(712)
          
          cindex <- cross_validate_aorsf(data = sim_data$data,
                                         k = 10,
                                         n_tree = n_tree,
                                         n_split = n_split,
                                         n_retry = n_retry,
                                         mtry = mtry,
                                         split_rule = split_rule)
          
          if (cindex > best_cindex){
            best_cindex <- cindex
            best_parameters <- list(n_tree = n_tree, n_split = n_split, n_retry = n_retry, mtry = mtry, split_rule = split_rule)
          }
        }
      }
    }
  }
}

set.seed(712)
best_model <-orsf(data = sim_data$data,
                           formula = Surv(y, failed) ~ . ,
                           n_tree = best_parameters$n_tree,
                           n_split = best_parameters$n_split,
                           n_retry = best_parameters$n_retry,
                           mtry = best_parameters$mtry,
                           split_rule = best_parameters$split_rule)

saveRDS(best_model, file = "aorsf_best_model.rds")
print(best_parameters)
