sim_data <- readRDS("sim_survival_data.rds")
head(sim_data$data)

library(randomForestSRC)
library(survival)
library(caret)

cross_validate_rsf <- function(data, k, ntree, nodesize, mtry, nsplit){
  
  folds <- createFolds(data$y, k=k, list = TRUE, returnTrain = TRUE)
  cindex_values <- c()
  
  for (i in 1:k){
    train_data <- data[folds[[i]], ]
    test_data <- data[-folds[[i]], ]
    
    RSF_fit <- rfsrc(Surv(y, failed) ~ sex,
                     data = train_data,
                     ntree = ntree,
                     nodesize = nodesize,
                     mtry = mtry,
                     nsplit = nsplit,
                     splitrule = "logrank")
    
    cindex <- get.cindex(test_data$y, test_data$failed, predict(RSF_fit, newdata = test_data)$predicted)
    cindex_values <- c(cindex_values, cindex)
  }
  return(mean(cindex_values))
}



ntree_values <- c(500, 1000, 2000, 3000)
nodesize_values <- c(2, 3, 7, 10, 15)
mtry_values <- c(1, 2, 3, 4, 5)
nsplit_values <- c(10, 20, 50, 100)

best_cindex <- 0 
best_parameters <- list()

for (ntree in ntree_values){
  for (nodesize in nodesize_values){
    for (mtry in mtry_values){
      for (nsplit in nsplit_values){
        
        set.seed(710)
        cindex <- cross_validate_rsf(data = sim_data$data,
                                     k = 5,
                                     ntree = ntree,
                                     nodesize = nodesize,
                                     mtry = mtry,
                                     nsplit = nsplit)
        
        if (cindex > best_cindex){
          best_cindex <- cindex
          best_parameters <- list(ntree = ntree, nodesize = nodesize, mtry = mtry, nsplit = nsplit)
        }
      }
    }
  }
}

set.seed(710)
best_model <- rfsrc(Surv(y, failed) ~ sex,
                    data = sim_data$data,
                    ntree = best_parameters$ntree,
                    nodesize = best_parameters$nodesize,
                    mtry = best_parameters$mtry,
                    nsplit = best_parameters$nsplit,
                    splitrule = "logrank")

saveRDS(best_model, file = "rsf_best_model.rds")
print(best_parameters)





