setwd("~/Desktop/Kaggle Competition/house-prices-advanced-regression-techniques")
set.seed(123)
options(max.print=100000)

# libraries
library(randomForest)

# data uploading
dataset_train = read.csv("train.csv", header = TRUE)
dataset_test = read.csv("test.csv", header = TRUE)

# data preparation
dataset_train = dataset_train[,-1];
str(dataset_train)
dataset_test = dataset_test[,-1];
str(dataset_test)

# MISSING VALUES IMPUTATION --------------------------------------------------------------------------------------

# imputation of NAs by means of proximity measures
dataset_train = rfImpute(dataset_train[,1:79], dataset_train[,80], iter = 4, ntree = 200)
# dataset_test = rfImpute(dataset_test[,1:79], dataset_test[,80], iter = 10, ntree = 200)

dataset_train

# adjusting the position of the target 
dataset_train$SalePrice = dataset_train[,1]
dataset_train = dataset_train[,-1]
# dataset_rnd_test_imp$COUNT = dataset_rnd_test_imp[,1]
# dataset_rnd_test_imp = dataset_rnd_test_imp[,-1]


# MODELING -------------------------------------------------------------------------------------------------------

# RANDOM FOREST WITH MANUAL ADJUSTMENT

# optimal number of trees
ntree_setting_imp = randomForest(SalePrice ~ ., dataset_train, ntree = 1500,
                                 replace = TRUE, importance = TRUE, proximity = TRUE, 
                                 oob.prox = TRUE, keep.inbag = TRUE)
optimal_ntree_imp = as.numeric(which.min(ntree_setting_imp$mse))

# optimal size of inputs subset
mtry_imp <- c()
for (i in seq(1, 79, by = 1)) {
  mtry_setting_imp = randomForest(SalePrice ~ ., dataset_train, mtry = i,
                                  replace = TRUE)
  mtry_imp <- append(mtry_imp, mtry_setting_imp[["mse"]][500])
}
seq_mtry = seq(1, 79, by = 1)
matrix_mtry_imp = rbind(mtry_imp, seq_mtry)
matrix_mtry_imp = matrix_mtry_imp[,which.min(mtry_imp)]
optimal_mtry_imp = as.numeric(matrix_mtry_imp[2])

# optimal size of a node
nodesize_imp <- c()
for (i in seq(2, 30, by = 2)) {
  nodesize_setting_imp = randomForest(SalePrice ~ ., dataset_train, nodesize = i,
                                      replace = TRUE)
  nodesize_imp <- append(nodesize_imp, nodesize_setting_imp[["mse"]][500])
}
seq_nodesize = seq(2, 30, by = 2)
matrix_nodesize_imp = rbind(nodesize_imp, seq_nodesize)
matrix_nodesize_imp = matrix_nodesize_imp[,which.min(nodesize_imp)] 
optimal_nodesize_imp = as.numeric(matrix_nodesize_imp[2])

# optimal size of bootstrapped sample
samplesize_imp <- c()
for (i in seq(1200,1460, by = 10)) {
  sampsize_setting_imp = randomForest(SalePrice ~ ., dataset_train, sampsize = i,
                                      replace = TRUE)
  samplesize_imp <- append(samplesize_imp, sampsize_setting_imp[["mse"]][500])
}
seq_sampsize = seq(1200,1460, by = 10)
matrix_samplesize_imp = rbind(samplesize_imp, seq_sampsize)
matrix_samplesize_imp = matrix_samplesize_imp[,which.min(samplesize_imp)] 
optimal_sampsize_imp = as.numeric(matrix_samplesize_imp[2])

# optimal number of terminal nodes
maxnodes_imp <- c()
for (i in seq(30, 200, by = 5)) {
  maxnodes_setting_imp = randomForest(SalePrice ~ ., dataset_train, maxnodes = i,
                                      replace = TRUE)
  maxnodes_imp <- append(maxnodes_imp, maxnodes_setting_imp[["mse"]][500])
}
seq_maxnodes = seq(30, 200, by = 5)
matrix_maxnodes_imp = rbind(maxnodes_imp, seq_maxnodes)
matrix_maxnodes_imp = matrix_maxnodes_imp[,which.min(maxnodes_imp)] 
optimal_maxnodes_imp = as.numeric(matrix_maxnodes_imp[2])

# plotting the results to understand the behavior
plot(ntree_setting_imp$mse, type = "l", 
     ylab = "OOB_MSE", xlab = "ntree")
plot(seq(1, 79, by = 1), mtry_imp, 
     xlab = "mtry", ylab = "OOB_MSE", type = "l")
plot(seq(2, 30, by = 2), nodesize_imp, 
     xlab = "nodesize", ylab = "OOB_MSE", type = "l")
plot(seq(1200,1460, by = 10), samplesize_imp, 
     xlab = "sampsize", ylab = "OOB_MSE", type = "l")
plot(seq(30, 200, by = 5), maxnodes_imp, 
     xlab = "maxnodes", ylab = "OOB_MSE", type = "l")


# -------------------------------------------------------------------------------------------
# using the visual evidence to select a smart grid
# -------------------------------------------------------------------------------------------

# RANDOM FOREST WITH GRID SEARCH

parameters_grid_imp = expand.grid(
  ntree = seq(810, 830, by = 10),
  mtry = seq(16, 24, by = 2),
  nodesize  = seq(6, 2, by = -2),
  sampsize = seq(1360, 1420, by = 10),
  maxnodes = seq(190, 200, by = 5)
)

nrow(parameters_grid_imp)

for(i in 1:nrow(parameters_grid_imp)) {
  
  # train model
  model_imp = randomForest(
    formula         = SalePrice ~ ., 
    data            = dataset_train, 
    ntree           = parameters_grid_imp$ntree[i],
    mtry            = parameters_grid_imp$mtry[i],
    nodesize        = parameters_grid_imp$nodesize[i],
    sampsize        = parameters_grid_imp$sampsize[i],
    maxnodes        = parameters_grid_imp$maxnodes[i],
    replace = TRUE
  )
  
  # add OOB error to grid
  parameters_grid_imp$OOB_MSE[i] = tail(model_imp$mse, 1)
}

# showing the best combinations in terms of OOB mean squared error
parameters_grid_imp %>% 
  dplyr::arrange(OOB_MSE) %>%
  head(10)

# ordering the grid according to the lowest OOB_MSE
parameters_grid_imp = parameters_grid_imp[order(parameters_grid_imp$OOB_MSE),]
head(parameters_grid_imp)

# train model
imprf_grid = randomForest(
  formula         = SalePrice ~ ., 
  data            = dataset_train, 
  ntree           = parameters_grid_imp[1,1],
  mtry            = parameters_grid_imp[1,2],
  nodesize        = parameters_grid_imp[1,3],
  sampsize       = parameters_grid_imp[1,4],
  maxnodes        = parameters_grid_imp[1,5],
  replace = TRUE,
  nPerm = 4, 
  importance = TRUE, 
  proximity = TRUE, 
  oob.prox = TRUE, 
  keep.inbag = TRUE
)

# train model with known values
imprf_grid = randomForest(
  formula         = SalePrice ~ ., 
  data            = dataset_train, 
  ntree           = 820,
  mtry            = 24,
  nodesize        = 6,
  sampsize       = 1380,
  maxnodes        = 200,
  replace = TRUE,
  nPerm = 4, 
  importance = TRUE, 
  proximity = TRUE, 
  oob.prox = TRUE, 
  keep.inbag = TRUE
)

common <- intersect(names(dataset_train), names(dataset_test)) 
for (p in common) { 
  if (class(dataset_train[[p]]) == "factor") { 
    levels(dataset_test[[p]]) <- levels(dataset_train[[p]]) 
  } 
}


dataset_test = na.roughfix(dataset_test)

dataset_test$predictions = predict(imprf_grid, newdata = dataset_test)
  
predictions = dataset_test$predictions
index = seq(from=1461, to=2919, by=1)
predictions = cbind(index, predictions)
colnames(predictions) = c("Id","SalePrice")

write.csv(predictions, "first_attempt.csv")

# -------------------------------------------------------------------------------------------

