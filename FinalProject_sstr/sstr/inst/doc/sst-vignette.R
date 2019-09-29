## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE-----------------------------------------------------------
library(readr)
dataset <- data.matrix(read.csv(system.file("extdata/foo.csv", package = "sstr")))

## ----echo=TRUE-----------------------------------------------------------
dim(dataset)

## ----echo=TRUE-----------------------------------------------------------
library(readr)
scoreSimMa <- data.matrix(read_csv(system.file("extdata/scores2.csv", package = "sstr")))

## ----echo=TRUE-----------------------------------------------------------
dim(scoreSimMa)

## ----echo=TRUE-----------------------------------------------------------
start<-function(){
  start.time <- Sys.time()
  return(start.time)
}

## ----echo=TRUE-----------------------------------------------------------
end<-function(sttime){
  end.time <- Sys.time()
  time.takenG <- end.time - sttime
  return(time.takenG)
}

## ----echo=TRUE-----------------------------------------------------------
#Create L as the number of nodes
construct_l<-function(sm_sparse){
  return(matrix(1,nrow=nrow(sm_sparse),ncol = ncol(sm_sparse)))
}

#Create X as the assigments with maximun confidence value
construct_x<-function(sm_sparse){
  return(matrix(0,nrow=nrow(sm_sparse),ncol = ncol(sm_sparse)))
}

## ----echo=TRUE-----------------------------------------------------------
metricm_size<-function(sm_sparse,sketch_map_size){
  m_map_size=(nrow(sm_sparse)/sketch_map_size)
  return(m_map_size)
}

## ----echo=TRUE-----------------------------------------------------------
maxScoreIndx<-function(scorematrix){
  maxScore=max(scorematrix)
  maxind=(which(scorematrix == maxScore, arr.ind = TRUE))[1][1]
  return(maxind)
}

## ----echo=TRUE-----------------------------------------------------------

library(SparseM)
library('Matrix')

simat<-function(path,sketch_map_size,scoressm){

  #create storage lists
  sm_feat_list = c()
  mm_feat_list = c()

  start_time=start()

  #Sparse the Matrix by row
  sm_sparse=as.matrix.csr(path)
  metric_map_size=metricm_size(sm_sparse,sketch_map_size)
  #Main analysis objects
  L=construct_l(sm_sparse)
  X=construct_x(sm_sparse)

  #Fetch the compatible ones
  while(max(scoressm)>0 & nnzero(L)!=0){

    maxind=maxScoreIndx(scoressm)

    sm_feat_id=round((maxind/metric_map_size),0)
    sm_feat_list=append(sm_feat_list,sm_feat_id)

    mm_feat_id=maxind%%metric_map_size
    mm_feat_list=append(mm_feat_list,mm_feat_id)

    scoressm[maxind,]=-1
    L[maxind,]=0

    for (i in seq(0,ncol(sm_sparse), by=1)){
      if(round(i/metric_map_size,0)==sm_feat_id){
        L[i,]=0
        scoressm[i,]=-1
      }

      if(i%%metric_map_size==mm_feat_id){
        L[i,]=0
        scoressm[i,]=-1
      }
    }

    X[maxind,]=1
  }

  #results<-r_sstr(sketch_map_indexes=sm_feat_list, metric_map_indexes=mm_feat_list, time=end(start_time))

  results<-constructClass(smi=sm_feat_list,mmi=mm_feat_list,t=end(start_time))
  return(results)
}

## ----echo=TRUE-----------------------------------------------------------
library(igraph)

pr_scores<-function(simMatrix){

  sparseMatrix<-as(simMatrix,"sparseMatrix")

  g<-graph_from_adjacency_matrix(sparseMatrix, mode = "directed")
  prr<-page_rank(g)$vector
  scoresSimMa2=matrix(prr,nrow=length(prr))

  return(scoresSimMa2)
}


## ----setup---------------------------------------------------------------
library(sstr)

## ----echo=TRUE-----------------------------------------------------------
library(readr)
# From the datasample
dataset <- data.matrix(read.csv(system.file("extdata/foo.csv", package = "sstr")))
scoreSimMa <- data.matrix(read_csv(system.file("extdata/scores2.csv", package = "sstr")))

# From the function code
scorePR<-pr_scores(dataset)

## ----echo=TRUE-----------------------------------------------------------
smap_size=41

## ----echo=TRUE-----------------------------------------------------------
#?simat

# Data Sample
(result_object_sstr<-simat(path=dataset,sketch_map_size=smap_size,scoressm=scoreSimMa))

# Page Rank Code
(result_object_sstr2<-simat(path=dataset,sketch_map_size=smap_size,scoressm=scorePR))


