library(FactoMineR)
library(psych)

createCompaniesAsRowsDataset <- function(filenames,dir){
  
  # initializing global.dataset with the first name of the list filenames
  dataset  <- read.csv(paste0(dir,filenames[1],'_cotization.csv'),sep = ';',row.names = 1)
  global.dataset <-t(dataset$IncrementDay)
  colnames(global.dataset) <- row.names(dataset)
  
  # reading the rest of the files, transposing them, keeping the interesting row and adding to the main dataset
  for(i in filenames[2:length(filenames)]){
    dir.file <- paste0(dir,i,'_cotization.csv')
    dataset <- read.csv(dir.file,sep=';',row.names=1)
    dataset.transposed <- t(dataset$IncrementDay)
    colnames(dataset.transposed) <- row.names(dataset)
    global.dataset <- rbind(global.dataset,dataset.transposed)
    
  }
  row.names(global.dataset) <- filenames
  
  return(global.dataset)
}

generateBenefitContinuousDay <- function(Y, prefix, filenames, amount, com = 0.25) {
  
  values <- as.data.frame(Y[, which(colnames(Y)==paste0('IncrementDay',filenames[1]))])
  colnames(values) <- filenames[1]
  
  predictions <- as.data.frame(Y[, which(colnames(Y)==paste0(prefix,filenames[1]))])
  colnames(predictions) <- filenames[1]
  
  for(name in filenames[-c(1)]){
    new_column <- as.data.frame(Y[, which(colnames(Y)==paste0('IncrementDay',name))])
    colnames(new_column) <- name
    values <- cbind(values, new_column)
    
    new_column <- as.data.frame(Y[, which(colnames(Y)==paste0(prefix,name))])
    
    colnames(new_column) <- name
    predictions <- cbind(predictions, new_column)
  }
  
  cum_increment <- 1
  for(row in 1:nrow(values)){
    choice <- sort(predictions[row,], decreasing = TRUE)[1:amount]
    choiced_names <- colnames(choice)
    real_values <- values[row,][choiced_names]
    real_increment <- rowMeans(real_values)
    cum_increment <- cum_increment * (1 + (real_increment / 100))
  }
  
  return(cum_increment)
}

addColumn <- function(Y, newCol, colName) {
  if(colName %in% colnames(Y)) {
    Y[,colName] <- NULL
  }
  newColDat <- as.data.frame(newCol)
  colnames(newColDat) <- colName
  Y <- cbind(Y, newColDat)
  return(Y)
}

get_outliers <- function(new_individuals, per_outliers = 0.2) {
  amount_outliers <- round(nrow(new_individuals) * per_outliers)
  #print(amount_outliers)
  m_dist <- mahalanobis(new_individuals, colMeans(new_individuals), cov(new_individuals))
  m_dist <- sort(m_dist)
  print(m_dist)
  outliers <- m_dist[(nrow(new_individuals) - amount_outliers + 1):nrow(new_individuals)]
  outlier.index <- which(rownames(new_individuals) %in% names(outliers))
  #print(outlier.index)
  return(outlier.index)
}

get_nd <- function(pca.results, percentage = 0.95) {
  cum.sum.eig <- cumsum(pca.results$eig[,1])
  cum.sum.eig.norm <- cum.sum.eig / tail(cum.sum.eig, n=1)
  return(which(cum.sum.eig.norm>=0.95)[1])
}

generate_outliers <- function(dataset_center, perc_outliers = 0.2, limit = 100){
  #perc_outliers <- 0.2
  #limit <- 10
  
  pca.results <- PCA(dataset_center, ncp = ncol(dataset_center))
  
  # deciding the number of principal components we want to take
  cum.sum.eig <- cumsum(pca.results$eig[,1])
  cum.sum.eig.norm <- cum.sum.eig / tail(cum.sum.eig, n=1)
  # we decide to retain as many eigenvalues as needed in order to have 80% of the total 
  # Inertia
  
  amount_outliers <- round(nrow(dataset_center) * perc_outliers)
  nd <- get_nd(pca.results, percentage = 0.95)
  counter <- rep(0, length(filenames))
  
  new_individuals <- pca.results$ind$coord[,1:nd]
  iter <- 0
  
  pos.outliers <- get_outliers(new_individuals[,1:nd], per_outliers = perc_outliers)
  
  while(iter < limit){
    
    weights <- rep(1,nrow(dataset_center))
    for(i in pos.outliers){
      counter[i] <- counter[i] + 1
      weights[i] <- 0.0001 
    }
    
    pca.results <- PCA(dataset_center,ncp = ncol(dataset_center),row.w=weights)
    nd <- get_nd(pca.results, percentage = 0.95)
    print(nd)
    new_individuals <- pca.results$ind$coord[,1:nd]
    
    pre.outliers <- pos.outliers
    pos.outliers <- get_outliers(new_individuals[,1:nd], per_outliers = perc_outliers)
    print(pos.outliers)
    
    if(all.equal(pre.outliers, pos.outliers) == TRUE){
      break
    }
    print(iter)
    print(counter)
    iter <- iter + 1
  }
  
  if(iter == limit){
    return(counter)
  }
  
  return(pos.outliers)
  
}
