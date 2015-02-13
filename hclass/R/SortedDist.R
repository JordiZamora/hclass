#-----------------------------------------------------------
# L_n distance calculation and sorting
#-----------------------------------------------------------
#' Sorted L_n distances
#'
#' Calculate and sort the distance between all pairs of data points.
#' 
#' @param TrainData A data frame with n columns of features.
#' @param TestData A data frame with the same number of columns of TrainData. If TestData is not provided, SortDist calculates the distance between all pairs of points in TrainData.
#' @param p Order of the L_p distance metric to be used.
#' 
#' @return A matrix with indexes of the neighbours sorted by distance. Each row corresponds to a data point in TestData (or TrainData if TestData is not provided). Each column corresponds to the kth nearest neighbour in TestData.
#' 
#' @export
#' @import assertthat 
#' @examples
#' Data <- mtcars[,c("mpg", "wt", "cyl")]
#' nTrain <- 25
#' p <- 2
#' TrainData <- Data[1:nTrain, c("mpg", "wt")]
#' TestData <- Data[(nTrain+1):nrow(mtcars), c("mpg", "wt")]
#' 
#' #Calculate the distance between elements in the Training Set
#' TrainSortedDist <- SortDist(TrainData, p=2)
#' # or Calculate distante between each element o the TestData and the training set
#' TestSortedDist <- SortDist(TrainData,TestData,2)

SortDist <- function (TrainData,TestData=NULL,p=2){
  
  # test the inputs
  not_empty(TrainData);
  assert_that(p %in% c(1, 2, Inf))  
  
  #Define the probe data
  if (is.null(TestData)){
    probe <- TrainData
  } else {
    probe <- TestData
  }
  
  noProbe <- nrow(probe)  
  noObs <- nrow(TrainData)
  Lpdist <- matrix(0,noProbe,noObs)
  
  # Compute the distances from probe data to train data under Lp metric
  if (p!=Inf){
    for (column in 1:ncol(TrainData)){
      Lpdist <- Lpdist + outer(probe[,column], 
                               TrainData[,column],
                               function(a,b) abs(a-b)^p)
    }
    Lpdist <- (Lpdist)^(1/p)
  } else {
    for (column in 1:ncol(TrainData)){
      Lpdist <- pmax(Lpdist, outer(probe[,column], 
                                   TrainData[,column],
                                   function(a,b) abs(a-b)))
    }
  }
  
  # Compute the indexes of the NN to the train data and sort them 
  if (is.null(TestData)){
    LpSortIndex <- t(apply(Lpdist,1,order))[,2:ncol(Lpdist)]
  } else {
    LpSortIndex <- t(apply(Lpdist,1,order))
  }
  
  return(LpSortIndex)
}