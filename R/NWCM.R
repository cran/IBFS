#' @title North-West Corner Method
#'
#' @description This method does not take into account the cost of transportation on any route of transportation.
#'
#' @param ex_matrix A cost matrix where last column must be the supply and last row must be the demand. Input matrix should not have any missing values (NA), otherwise function will throw an error. It should be balanced i.e. total demand must be equal to total supply.
#'
#' @return A List which contain the allocation matrix and the total optimized cost.
#'
#' @examples
#' #Input matrix where last row is the Demand and last column is the Supply
#' ex_matrix=data.frame(D1=c(6,3,4,20),E1=c(4,8,4,95),F1=c(1,7,2,35),
#'                      Supply=c(50,40,60,150),row.names = c("A1","B1","C1","Demand"))
#' NWCM(ex_matrix)
#'
#' @export
NWCM<-function(ex_matrix){
  if(sum(is.na(ex_matrix))>0){
    stop("Your matrix has NA values")
  }
  Alloc_Matrix=ex_matrix[-nrow(ex_matrix),-ncol(ex_matrix)]
  Alloc_Matrix[,]=0
  tr=1
  tc=1
  Total_Cost=0
  Total_alloc=0
  colnames(ex_matrix)[ncol(ex_matrix)]="Supply"
  while(sum(ex_matrix[nrow(ex_matrix),]) != 0 & sum(ex_matrix[,ncol(ex_matrix)]) != 0){
    min_curr=min(ex_matrix[tr,ncol(ex_matrix)],ex_matrix[nrow(ex_matrix),tc])
    ex_matrix[tr,ncol(ex_matrix)]=ex_matrix[tr,ncol(ex_matrix)] - min_curr
    ex_matrix[nrow(ex_matrix),tc]=ex_matrix[nrow(ex_matrix),tc] - min_curr
    Alloc_Matrix[tr,tc]=min_curr
    Total_Cost=Total_Cost+(min_curr*ex_matrix[tr,tc])
    if(ex_matrix[nrow(ex_matrix),tc]==0){
      tc=tc+1
    }else if(ex_matrix[tr,ncol(ex_matrix)]==ex_matrix[nrow(ex_matrix),tc]){
      tr=tr+1
      tc=tc+1
    }else{
      tr=tr+1
    }
    ex_matrix[nrow(ex_matrix),ncol(ex_matrix)]=sum(ex_matrix$Supply[-nrow(ex_matrix)])
    Total_alloc=Total_alloc+1
  }
  output=list()
  output$Alloc_Matrix=Alloc_Matrix
  output$Total_Cost=Total_Cost

  #If Supply and Demand are not same
  if(sum(ex_matrix[nrow(ex_matrix),]) != 0){
    output$Dummy_demand=sum(ex_matrix[nrow(ex_matrix),])
  }else if(sum(ex_matrix[,ncol(ex_matrix)]) != 0){
    output$Dummy_supply=sum(ex_matrix[,ncol(ex_matrix)])
  }
  if(Total_alloc < (nrow(Alloc_Matrix) + ncol(Alloc_Matrix)-1)){
    warning("Degenracy in Transporation Problem Occurred")
  }

  return(output)
}
