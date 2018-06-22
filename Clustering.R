rm(list=ls())
setwd("/Users/miqueltubaupires/Desktop/ML_project/")
source('usefulFunctions.R')

library(xtable)
  # PCA + Clusterring

  filenames <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "XOM", "GE", "GS", "HD", "INTC", "IBM", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "VZ", "V", "WMT", "DIS")
  dir <- '/Users/miqueltubaupires/Desktop/ML_project/' # change this whenever the user wants to run it locally 
  dataset <- createCompaniesAsRowsDataset(filenames,dir)
  dataset_centered = data.frame(scale(dataset,scale = FALSE))
  
  # outliers detection
  # outliers.position <- generate_outliers(dataset_centered, perc_outliers = 0.2, limit = 100)
  # there are no outliers
  
  # centering PCA
  library(FactoMineR)
  
  pca.results <- PCA(dataset_centered,ncp = ncol(dataset_centered))
  
  # deciding the number of principal components we want to take
  cum.sum.eig <- cumsum(pca.results$eig[,1])
  cum.sum.eig.norm <- cum.sum.eig / tail(cum.sum.eig, n=1)
  # we decide to retain as many eigenvalues as needed in order to have 80% of the total 
  # Inertia
  nd <- which(cum.sum.eig.norm>=0.95)[1] 

  # continuing with PCA
  p <- ncol(dataset)
  
  Psi <- pca.results$ind$coord[,1:nd]
  Phi <- pca.results$var$coord[,1:nd]
  
  # summary of the pca
  summary(pca.results)

  # analyzing the influence of each observation/company to the principal components
  company.prin.comp.df <- data.frame(pca.results$ind$cos2)
  perc.pc <- round(pca.results$eig[,1]/tail(cum.sum.eig, n=1)*100,2)
  
  mid.var.pos <- which(cum.sum.eig.norm>=0.33)[1] # let's analyze the most significant principal components
  
  for(i in length(1:mid.var.pos)){
    aux.table <- data.frame(cbind(company.prin.comp.df[,i],filenames))
    colnames(aux.table) <- c(paste('Var. explained =',perc.pc[i]),'company')
    row.names(aux.table) <- filenames
    aux.table <- data.frame(aux.table[with(aux.table, order(aux.table[,1],decreasing = T)), ])
    xtable(aux.table[1:5,]) # printing the 5 most significant for each PC
  }
  
# ----------------------------------------------------------------------------------------------------------------------------- #  
  # CLUSTERING
  
  # applying hclustering 
  dist.matrix <- dist(Psi)
  hc<- stats::hclust(dist.matrix,method="ward.D2")
  plot(hc)
  barplot(hc$height,xlab = 'number of clusters',names.arg = 29:2)
  
  # we ee 6 and 14 are good candidates
  
  nc = 6
  c1 <- cutree(hc,nc)
  
  # LETS SEE THE PARTITION VISUALLY
  plot(Psi[,1],Psi[,2],type="n",main="Clustering of expenses in 6 classes",xlab = '1r Prin Comp',ylab='2n Prin Comp')
  text(Psi[,1],Psi[,2],col=c1,labels=names(c1),cex = 0.6) 
  abline(h=0,v=0,col="gray") 
  legend("topleft",c("c1","c2","c3","c4","c5","c6"),pch=20,col=c(1:6))
  
  c2 <- cutree(hc,14)
  
  # LETS SEE THE PARTITION VISUALLY
  plot(Psi[,1],Psi[,2],type="n",main="Clustering of expenses in 14 classes",xlab = '1r Prin Comp',ylab='2n Prin Comp')
  text(Psi[,1],Psi[,2],col=c2,labels=names(c2),cex = 0.6) 
  abline(h=0,v=0,col="gray") 
  legend("topleft",c("c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13","c14"),pch=20,col=c(1:14))
  
# ----------------------------------------------------------------------------------------------------------------------------- #  
  # creating indo company table for better visualizing the clusters
  
  company.names <- read.csv('/Users/miqueltubaupires/Desktop/Projecte/yahoo_stickers.csv',header = F)
  
  for(i in 1:length(filenames)){
    pos <- which(company.names$V1==filenames[i])
    if(i==1){
      aux.table <- data.frame(company.names[pos,])
    }
    else{
      aux.table <- rbind(aux.table,company.names[pos,])
    }
    
  }
  
  row.names(aux.table) <- 1:29
  clusters <- list(c1,c2)
  tables <- list()
  k <- c(6,14)
  for(i in 1:length(k)){
    tables[[i]] <- cbind(aux.table[,-c(3,5)],clusters[i])
    colnames(tables[[i]]) <- c('Acronym','Name','Industry',paste0('h=',k[i]))
    # sorting by the cluster
    adhoc.table <- tables[[i]]
    adhoc.table <- adhoc.table[with(adhoc.table, order(adhoc.table[,4])), ]
    tables[[i]] <-  adhoc.table
    write.csv(file=paste0('Company_information',k[i],'.csv'), x=tables[[i]])
    
  }

