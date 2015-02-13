# -----------------------------------------------------------
# kNN classifier from a matrix of sorted distances
# -----------------------------------------------------------
#' kNN classifier
#'
#' Classify the input with a k nearest neighbors classifier.
#' 
#' @param TrainData A data frame with n columns of features.
#' @param trainclass A vector of classes corresponding to the data in TrainData.
#' @param LpSortIndex A matrix with indexes of the neighbours sorted by distance. Each row corresponds to a data point in testing set. Each column must correspond to the kth nearest neighbour in the testing set.
#' @param TestDataClass A vector of classes corresponding to the data in the test set.
#' @param k An array of numeric values corresponding to the order of the nearest neighbour classifier.
#' 
#' @return Returns a list with the predicted class and/or accuracy according to the matrix of indexes LpSortIndex. If TestDataclass is not provided only the predicted class is returned.
#' 
#' @export
#' @import assertthat 
#' @examples
#' Data <- mtcars[,c("mpg", "wt", "cyl")]
#' nTrain <- 25
#' p <- 2
#' k <- 1
#' TrainData <- Data[1:nTrain, c("mpg", "wt")]
#' TrainDataClass <- Data[1:nTrain, "cyl"]
#' TestData <- Data[(nTrain+1):nrow(mtcars), c("mpg", "wt")]
#' TestDataClass <- Data[(nTrain+1):nrow(mtcars), "cyl"]
#' 
#' # Calculate distante between each element o the TestData and the training set
#' TestSortedDist <- SortDist(TrainData,TestData,2)
#' 
#' kNN.out <- kNNClass(TrainData,
#'                     TrainDataClass,
#'                     TestSortedDist,
#'                     TestDataClass, k=k)


kNNClass <- function(TrainData, trainclass, LpSortIndex, TestDataClass=NULL, k=1){

  # test the inputs
  not_empty(TrainData); not_empty(trainclass); not_empty(LpSortIndex);
  assert_that(is.numeric(k))  
  assert_that(nrow(TrainData)==length(trainclass))  
  
  # If the label of the test data is not known compute a single value of k
  if (is.null(TestDataClass) && length(k)>1){
    k <- k[1]
  }
  
  accuracy <- rep(0,1,length(k))
  unicLabel <- unique(trainclass)
  
  if (is.null(TestDataClass)){
    kNNs <- as.data.frame(LpSortIndex[,1:k]) #Choose the k-NN indexes
    classes <- apply(kNNs, 2, function(x) trainclass[x]) #Find the class of the NN
    counts <- matrix(0,nrow(classes),length(unicLabel))
    
    for (j in 1:length(unicLabel)){
      counts[,j] <- rowSums(classes==unicLabel[j]) #Count each class
    }
    
    predictedClasses <- apply(counts,1, function(x) which.max(x)) #Select more frequent class
    predictedClasses <- unicLabel[predictedClasses] #Substitute class value
    out <- list(PredictedClasses = predictedClasses)
    
  } else {
    for (i in 1:length(k)){
      kNNs <- as.data.frame(LpSortIndex[,1:k[i]])
      classes <- apply(kNNs, 2, function(x) trainclass[x])
      counts <- matrix(0,nrow(classes),length(unicLabel))
      
      for (j in 1:length(unicLabel)){
        counts[,j] <- rowSums(classes==unicLabel[j])
      }
      
      predictedClasses <- apply(counts,1, function(x) which.max(x))
      predictedClasses <- unicLabel[predictedClasses]
      # Compute accuracy
      accuracy[i] <- mean(predictedClasses==TestDataClass)
    }
    
    if (length(k)>1){
      out <- list(accuracy = accuracy)
    } else {
      out <- list(PredictedClasses = predictedClasses, accuracy = accuracy)
    }
  }
  
  return(out)
}