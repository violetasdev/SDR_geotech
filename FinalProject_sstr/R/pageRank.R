#' Pagerank implementation
#'
#' This pagerank coding attemps to generate the scoring for a stored similarity matrix
#' @importFrom Matrix nnzero,sparse_Sums
#' @importFrom readr read_csv
#' @importFrom SparseM as.matrix.csr
#' @importFrom textTinyR sparse_Sums
#' @export
#'


#Similarity Matrix - Must be translated into a sparseMatrix, because it is to heavy to handle
library(SparseM)
library(readr)
library('Matrix')
library(textTinyR)
library(igraph)

X<- data.matrix(read_csv(system.file("extdata/foo.csv", package = "sstr")))

alpha=0.85
max_iter=200
tol=1e-10

pagerankR<-function(X,alpha=0.85,max_iter=50,tol=1e-10){
  n=ncol(X)

  #With compression, otherwise will be huge
  sparseMX<-as(X,"sparseMatrix")

  g<-graph_from_adjacency_matrix(sparseMX, mode = "directed")
  prr<-page_rank(g)$vector
  pgm=matrix(prr,nrow=length(prr))

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
  dim(sparseMX@x)


  for (k in seq(1,max_iter, by=1)){
    print(k)
    prev_scores = scores
    print (dim(prev_scores))
    #Here we have the detail, we are calling the sparseMatrix data nxn
    #We need all to be numeric
    scores = (alpha * ((scores[,])*sparseMX@x + (t(dangleM)%*%(prev_scores)))+ (1 - alpha) * sum(prev_scores) / n)
    scores= matrix(scores,nrow=length(scores))
    scores_max =max(abs(scores))

    if (scores_max == 0.0){
      scores_max = 1.0
    }

    err = abs(max(matrix(scores)  - as.vector(prev_scores))) / scores_max
    if (err < n * tol){
      (scores)
    }
  }
  (scores)
}







