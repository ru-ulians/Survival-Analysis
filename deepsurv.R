library(dnn)
library(survival)
library(caret)


# Build a deep neural network model
dnn_model <- dNNmodel(N = 4,                      # Number of training sample
                      units = c(128, 64, 32, 1),  # Number of nodes in each layer
                      optimizer = "adam",
                      input_shape = 4,            # Number of covariates
                      activation = c("relu", "relu", "relu", "sigmoid") 
)

# Load data 
sim_data <- readRDS("sim_survival_data.rds")

# Hyperparam grid
epochs_val <- c(100,200,300,400)
batch_size_val <- c(16, 32, 64, 128)
lr_rates_val <- c(0.00001, 0.0001, 0.001, 0.01)
alpha_val <- c(0.25, 0.5, 0.7, 0.9)
lambda_val <- c(0.0001, 0.001, 0.01)

best_cindex <- 0
best_params <- list()
best_model <- NULL

for (epochs in epochs_val){
  for (batch_size in batch_size_val){
    for (lr_rate in lr_rates_val){
      for (alpha in alpha_val){
        for (lambda in lambda_val){
          set.seed(721)
          
          fit <- deepSurv(Surv(y, failed) ~ . ,
                          model = dnn_model,
                          data = sim_data$data,
                          epochs = epochs,
                          batch_size = batch_size,
                          lr_rate = lr_rate,
                          alpha = alpha,
                          lambda = lambda
                          )
          
          output <- capture.output(print(fit))
          c_index_line <- grep("Concordance index:", output, value = TRUE)
          c_index <- as.numeric(sub("Concordance index: ", "", c_index_line))
          
          if (c_index > best_cindex){
            best_cindex <- c_index
            best_model <- fit
            best_params <- list(epochs=epochs, batch_size=batch_size, lr_rate=lr_rate, alpha=alpha, lambda=lambda)
          }
        }
      }
    }
  }
}

saveRDS(best_model, file = "deepsurv_best_model.rds")

print(best_params)





