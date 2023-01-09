#' @title Least Cost Method
#'
#' @description The main objective is to minimize the total transportation cost, transport as much as possible through those routes (cells) where the unit transportation cost is lowest. This method takes into account the minimum cost of transportation for obtaining the initial solution.
#'
#' @param ex_matrix A cost matrix where last column must be the supply and last row must be the demand. Input matrix should not have any missing values (NA), otherwise function will throw an error. It should be balanced i.e. total demand must be equal to total supply.
#'
#' @return A List which contain the allocation matrix and the total optimized cost.
#'
#' @examples
#' #Input matrix where last row is the Demand and last column is the Supply
#' ex_matrix=data.frame(D1=c(6,3,4,20),E1=c(4,8,4,95),F1=c(1,7,2,35),
#'                      Supply=c(50,40,60,150),row.names = c("A1","B1","C1","Demand"))
#' LCM(ex_matrix)
#'
#' @export
LCM<-function(ex_matrix){
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
  while(sum(Supply) != 0 & sum(Demand) != 0){
    tc=which(ex_matrix==min(ex_matrix),arr.ind=TRUE)[1,2]  #column of minimum value
    tr=which(ex_matrix==min(ex_matrix),arr.ind=TRUE)[1,1]  #row of minimum value
    min_curr=min(Demand[tc],Supply[tr])
    Demand[tc]=Demand[tc] - min_curr
    Supply[tr]=Supply[tr] - min_curr
    Alloc_Matrix[tr,tc]=min_curr
    Total_Cost=Total_Cost+(min_curr*ex_matrix[tr,tc])
    if(Demand[tc]==0){
      ex_matrix[,tc]=rep(High_Values,nrow(ex_matrix))
    }else if(Demand[tc]==Supply[tr]){
      ex_matrix[tr,]=rep(High_Values,ncol(ex_matrix))
      ex_matrix[,tc]=rep(High_Values,nrow(ex_matrix))
    }else{
      ex_matrix[tr,]=rep(High_Values,ncol(ex_matrix))
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

