#' Pagerank implementation
#'
#' This pagerank coding attemps to generate the scoring for a stored similarity matrix
#' @param X similarity matrix of class matrix
#' @param alpha  probability at each node to be changed
#' @param max_iter number of iterations for the power function
#' @param tol float number for convergence tolerance
#' @importFrom Matrix nnzero
#' @importFrom readr read_csv
#' @importFrom methods as
#' @importFrom SparseM as.matrix.csr
#' @importFrom textTinyR sparse_Sums
#' @export
#'

#Similarity Matrix - Must be translated into a sparseMatrix, because it is to heavy to handle

pagerankR<-function(X,alpha=0.85,max_iter=50,tol=1e-10){
  n=ncol(X)

  #With compression, otherwise will be huge
  sparseMX<-as(X,"sparseMatrix")
  sumRowS<-sparse_Sums(sparseMX, rowSums = TRUE)

  nozero=which(sumRowS!=0)

  #Identify dangle scoring


  for(i in nozero){
    (sparseMX[i:i+1,]=sparseMX[i:i+1,]*(1/sumRowS[i]))
    sr=sparse_Sums(sparseMX, rowSums = TRUE)
    dangle<-ifelse(sr==0,1.0/n,0)
  }

  initial_guess=1/n
  scores= matrix(1,nrow=n)/n
  #dim(scores)
  dangleM=matrix(dangle,nrow = n)
  dim(dangleM)


  for (k in seq(1,max_iter, by=1)){
    print(i)
    prev_scores = scores
    (dim(prev_scores))
    #Here we have the detail, we are calling the sparseMatrix data nxn
    #We need all to be numeric
    scores = (alpha * ((scores)*sparseMX@x + (t(dangleM)%*%(prev_scores)))+ (1 - alpha) * sum(prev_scores) / n)

    scores_max =max(abs(scores))

    if (scores_max == 0.0){
      scores_max = 1.0
    }

    err = abs(max(matrix(scores)  - as.vector(prev_scores))) / scores_max
    if (err < n * tol){
      (scores)
    }
  }
  return (scores)
}







