features <- names(pml_testing[,colSums(is.na(pml_testing)) == 0])[8:59]

# Only use features used in testing cases.
pml_training <- pml_training[,c(features,"classe")]
pml_testing <- pml_testing[,c(features,"problem_id")]

dim(pml_training); dim(pml_testing);

set.seed(12345)

inTrain <- createDataPartition(pml_training$classe, p=0.6, list=FALSE)
training <- pml_training[inTrain,]
testing <- pml_training[-inTrain,]

dim(training); dim(testing);

modFitPML <- rpart(classe ~ ., data = training, method="class")
fancyRpartPlot(modFitPML)
set.seed(12345)

prediction <- predict(modFitPML, testing, type = "class")
confusionMatrix(prediction, testing$classe)

table(training$classe)
prop.table(table(training$classe))

require(magrittr)

asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], 
                                                   asNumeric))
training %>%
  subset(select = -c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, 
                     cvtd_timestamp, new_window, num_window, classe)) %>%
  factorsNumeric -> training[ , 8:159]

propmiss <- function(dataframe) {
  sapply(dataframe,function(x) 
    data.frame(nmiss=sum(is.na(x)), 
               n=length(x), 
               propmiss=sum(is.na(x))/length(x)))
}

propmiss(training)
n.missing <- sum(sapply(training, function(x) sum(is.na(x))/length(x) >= 0.05))
training <- training[, sapply(training, function(x) sum(is.na(x))/length(x) < 0.05)]

require(knitr)

vars_left <- data.frame(Device <- character(length = 52), 
                        Predictor <- colnames(training[8:59]))
names(vars_left) <- c("Device", "Predictor")
vars_left$Device <- as.character(vars_left$Device)
vars_left$Predictor <- as.character(vars_left$Predictor)

vars_left$Device[grepl("belt", vars_left$Predictor, ignore.case = TRUE)] <- "Belt"
vars_left$Device[grepl("arm", vars_left$Predictor, ignore.case = TRUE)] <- "Arm"
vars_left$Device[grepl("dumbbell", vars_left$Predictor, ignore.case = TRUE)] <- "Dumbbell"
vars_left$Device[grepl("forearm", vars_left$Predictor, ignore.case = TRUE)] <- "Forearm"

vars_left$Predictor <- gsub("_belt|_arm|_dumbbell|_forearm", "", vars_left$Predictor)
kable(table(vars_left$Predictor, vars_left$Device))
require(caret)

nzv <- nearZeroVar(training[8:59], saveMetrics = TRUE)
nzv
spec.cor <- function (dat, r, ...) { 
  x <- cor(dat, ...) 
  x[upper.tri(x, TRUE)] <- NA 
  i <- which(abs(x) >= r, arr.ind = TRUE) 
  data.frame(matrix(colnames(x)[as.vector(i)], ncol = 2), value = x[i]) 
} 

spec.cor(training[ , 8:59], 0.8)
set.seed(567)
folds <- createFolds(training$classe, k = 10, list = TRUE, returnTrain = TRUE)
accuracies.dt <- c()
for (i in 1:10) {
  model <- train(classe ~ ., data=training[folds[[i]], 8:60], method = "rpart")
  predictions <- predict(model, training[-folds[[i]],8:59])
  accuracies.dt <- c(accuracies.dt, 
                     confusionMatrix(predictions, training[-folds[[i]], 8:60]$classe)$overall[[1]])
  set.seed(567)
  accuracies.rf <- c()
  for (i in 1:10) {
    model <- train(classe ~ ., data=training[folds[[i]], 8:60], method = "rf")
    predictions <- predict(model, training[-folds[[i]],8:59])
    accuracies.rf <- c(accuracies.rf, 
                       confusionMatrix(predictions, training[-folds[[i]], 8:60]$classe)$overall[[1]])
  }
  require(knitr)
  
  vars_left <- data.frame(Device <- character(length = 52), 
                          Predictor <- colnames(training[8:59]))
  names(vars_left) <- c("Device", "Predictor")
  vars_left$Device <- as.character(vars_left$Device)
  vars_left$Predictor <- as.character(vars_left$Predictor)
  
  vars_left$Device[grepl("belt", vars_left$Predictor, ignore.case = TRUE)] <- "Belt"
  vars_left$Device[grepl("arm", vars_left$Predictor, ignore.case = TRUE)] <- "Arm"
  vars_left$Device[grepl("dumbbell", vars_left$Predictor, ignore.case = TRUE)] <- "Dumbbell"
  vars_left$Device[grepl("forearm", vars_left$Predictor, ignore.case = TRUE)] <- "Forearm"
  
  vars_left$Predictor <- gsub("_belt|_arm|_dumbbell|_forearm", "", vars_left$Predictor)
  kable(table(vars_left$Predictor, vars_left$Device))  
  require(caret)
  
  nzv <- nearZeroVar(training[8:59], saveMetrics = TRUE)
  nzv  
  spec.cor <- function (dat, r, ...) { 
    x <- cor(dat, ...) 
    x[upper.tri(x, TRUE)] <- NA 
    i <- which(abs(x) >= r, arr.ind = TRUE) 
    data.frame(matrix(colnames(x)[as.vector(i)], ncol = 2), value = x[i]) 
  } 
  
  spec.cor(training[ , 8:59], 0.8)  
  set.seed(567)
  folds <- createFolds(training$classe, k = 10, list = TRUE, returnTrain = TRUE)
  accuracies.dt <- c()
  for (i in 1:10) {
    model <- train(classe ~ ., data=training[folds[[i]], 8:60], method = "rpart")
    predictions <- predict(model, training[-folds[[i]],8:59])
    accuracies.dt <- c(accuracies.dt, 
                       confusionMatrix(predictions, training[-folds[[i]], 8:60]$classe)$overall[[1]])
  }
  set.seed(567)
  accuracies.rf <- c()
  for (i in 1:10) {
    model <- train(classe ~ ., data=training[folds[[i]], 8:60], method = "rf")
    predictions <- predict(model, training[-folds[[i]],8:59])
    accuracies.rf <- c(accuracies.rf, 
                       confusionMatrix(predictions, training[-folds[[i]], 8:60]$classe)$overall[[1]])
  }  
  table(testing$classe)
  prop.table(table(testing$classe))

  require(magrittr)
  
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], 
                                                     asNumeric))
  testing %>%
    subset(select = -c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, 
                       cvtd_timestamp, new_window, num_window, classe)) %>%
    factorsNumeric -> testing[ , 8:159]
  # Build model on whole training set
  set.seed(567)
  model <- train(classe ~ ., data = training[ , 8:60], method = "rf")
  # Make predictions on testing set
  predictions <- predict(model, testing[,8:159])
  # Summarise results
  confusionMatrix(predictions, testing$classe)  
  varImp(model)
  d.plots <- function(predictor, data, title, label) {
    qplot(predictor, colour = classe, data = data, geom = "density") + 
      xlab(label) +
      ylab("Density") +
      ggtitle(title) +
      theme_bw()
  }
  
  g1 <- d.plots(training$roll_belt, training, "Belt Roll (Training)", "Belt roll")
  g2 <- d.plots(testing$roll_belt, testing, "Belt Roll (Testing)", "Belt roll")
  g3 <- d.plots(training$pitch_forearm, training, "Forearm Pitch (Training)", "Forearm pitch")
  g4 <- d.plots(testing$pitch_forearm, testing, "Forearm Pitch (Testing)", "Forearm pitch")
  g5 <- d.plots(training$yaw_belt, training, "Belt Yaw (Training)", "Belt yaw")
  g6 <- d.plots(testing$yaw_belt, testing, "Belt Yaw (Testing)", "Belt yaw")
  
  require(gridExtra)
  grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 3, ncol = 2)  

  require(magrittr)
  
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], 
                                                     asNumeric))
  validation %>%
    subset(select = -c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, 
                       cvtd_timestamp, new_window, num_window, problem_id)) %>%
    factorsNumeric -> validation[ , 8:159]
  # Make predictions on validation set
  predictions <- predict(model, validation[,8:159])
  # Print predictions
  data.frame(problem_id = validation$problem_id, prediction = predictions)  
  