#' Construct sstr object
#'
#' Return an object of class sstr
#' @param smi list with input graph A sketch map with affinity candidates
#' @param mmi list with input graph B metric map with affinity candidates
#' @param t object from end function
#' @importFrom methods setClass
#' @return r_sstr an sstr class object
#' @examples
#' start()
#' @export
#'

constructClass<-function(smi=0,mmi=0,t=0){
  r_sstr<-structure(list("sketch_map_indexes"=smi, "metric_map_indexes"=mmi, "time"=t), class = "sst_r")
  #r_sstr<-setClass("sstr", slots=c("sketch_map_indexes","metric_map_indexes","time"))
  return(r_sstr)
}

#' Start execution time pointer
#'
#' Return the time in which the process start running
#'
#' @return system start time
#' @export
#' @examples
#' start()

start<-function(){
  start.time <- Sys.time()
  return(start.time)
}

#' End execution time pointer
#'
#' Return the total execution time in which the process start running
#'
#' @param sttime object from start
#' @return system total execution time in segs
#' @export
#' @examples
#' init<-start()
#' (finish<-end(init))

end<-function(sttime){
  end.time <- Sys.time()
  time.takenG <- end.time - sttime
  return(time.takenG)
}


#' Fetch max affinity value
#'
#' Return the index of the maximum value in the scorematrix
#'
#' @param scorematrix object handed
#' @export

maxScoreIndx<-function(scorematrix){
  maxScore=max(scorematrix)
  maxind=(which(scorematrix == maxScore, arr.ind = TRUE))[1][1]
  return(maxind)
}

#' Calculate Metric Map Size
#'
#' Return the size of output graph for matching the input graph
#'
#' @param sm_sparse sparse similarity matrix object handed by the user
#' @param sketch_map_size parameter handed by the user accordingly to the input graph
#' @export

metricm_size<-function(sm_sparse,sketch_map_size){
  m_map_size=(nrow(sm_sparse)/sketch_map_size)
  return(m_map_size)
}

#' Create L as the number of nodes
#'
#' Returns an object of class matrix for storing the hypotetical affinity scored candidate pairs for the input graphs
#'
#' @param sm_sparse sparse similarity matrix object handed by the user
#' @export

construct_l<-function(sm_sparse){
  return(matrix(1,nrow=nrow(sm_sparse),ncol = ncol(sm_sparse)))
}

#' Create X as the assigments with maximun confidence value
#'
#' Returns an object of class matrix for storing the highest affinity scored candidate pairs for the input graphs
#'
#' @param sm_sparse sparse similarity matrix object handed by the user
#' @export

construct_x<-function(sm_sparse){
  return(matrix(0,nrow=nrow(sm_sparse),ncol = ncol(sm_sparse)))
}

#' Select the features from a similarity matrix with the highest confidence level
#'
#' Uses the function sst_pagerank from the package sstr to calculate the highest affinity scores between two graphs described in a input similarity matrix
#'
#' @param path similarity matrix of class matrix
#' @param sketch_map_size size of input graph of class numeric
#' @param scoressm calculated affinity scores for the similarity matrix
#' @return list object containing highest affinity candidate pairs for two input graphs
#' @importFrom Matrix nnzero
#' @importFrom readr read_csv
#' @importFrom SparseM as.matrix.csr
#' @export
#' @examples
#' library(readr)
#' dataset <- data.matrix(read.csv(system.file("extdata/foo.csv", package = "sstr")))
#' scoreSimMa<- data.matrix(read_csv(system.file("extdata/scores2.csv", package = "sstr")))
#' smap_size=41
#' (resulted_matrix<-simat(path=dataset,sketch_map_size=smap_size,scoressm=scoreSimMa))

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
