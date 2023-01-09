#' @title Row Minimum Method
#'
#' @description In the row minimum method, the first row that is the lowest cost cell is exhausted. The objective is to allocate the maximum either at the first source or demand at the destinations or to satisfy both. This process must be continued for all the other reduced transportation costs until and unless the supply and demand are satisfied.
#'
#' @param ex_matrix A cost matrix where last column must be the supply and last row must be the demand. Input matrix should not have any missing values (NA), otherwise function will throw an error. It should be balanced i.e. total demand must be equal to total supply.
#'
#' @return A List which contain the allocation matrix and the total optimized cost.
#'
#' @examples
#' #Input matrix where last row is the Demand and last column is the Supply
#' ex_matrix=data.frame(D1=c(6,3,4,20),E1=c(4,8,4,95),F1=c(1,7,2,35),
#'                      Supply=c(50,40,60,150),row.names = c("A1","B1","C1","Demand"))
#' RMM(ex_matrix)
#'
#' @export
RMM<-function(ex_matrix){
  if(sum(is.na(ex_matrix))>0){
    stop("Your matrix has NA values")
  }
  Demand=as.numeric(ex_matrix[nrow(ex_matrix),-ncol(ex_matrix)])
  Supply=as.numeric(ex_matrix[-nrow(ex_matrix),ncol(ex_matrix)])
  High_Values=max(ex_matrix) + 999999999
  Alloc_Matrix=ex_matrix[-nrow(ex_matrix),-ncol(ex_matrix)]
  ex_matrix=Alloc_Matrix
  Alloc_Matrix[,]=0
  Total_Cost=0
  Total_alloc=0
  tr=1
  while(sum(Supply) != 0 & sum(Demand) != 0){
    tc=which(ex_matrix[tr,]==min(ex_matrix[tr,]))[1]
    min_curr=min(Demand[tc],Supply[tr])
    Demand[tc]=Demand[tc] - min_curr
    Supply[tr]=Supply[tr] - min_curr
    Alloc_Matrix[tr,tc]=min_curr
    Total_Cost=Total_Cost+(min_curr*ex_matrix[tr,tc])
    if(Demand[tc]==0){
      ex_matrix[,tc]=rep(High_Values,nrow(ex_matrix))
      tc=tc+1
    }else if(Demand[tc]==Supply[tr]){
      ex_matrix[tr,]=rep(High_Values,ncol(ex_matrix))
      ex_matrix[,tc]=rep(High_Values,nrow(ex_matrix))
      tr=tr+1
      tc=tc+1
    }else{
      ex_matrix[tr,]=rep(High_Values,ncol(ex_matrix))
      tr=tr+1
    }
    Total_alloc=Total_alloc+1
  }
  output=list()
  output$Alloc_Matrix=Alloc_Matrix
  output$Total_Cost=Total_Cost

  #If Supply and Demand are not same
  if(sum(Demand) != 0){
    output$Dummy_demand=sum(Demand)
  }else if(sum(Supply) != 0){
    output$Dummy_supply=sum(Supply)
  }
  if(Total_alloc < (nrow(Alloc_Matrix) + ncol(Alloc_Matrix)-1)){
    warning("Degenracy in Transporation Problem Occurred")
  }
  return(output)
}
