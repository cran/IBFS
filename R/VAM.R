#' @title Vogel's Approximation Method
#'
#' @description Vogel's approximation method is preferred over NWCM and LCM methods. In this method allocation is made on the basis of the opportunity cost that would have been incurred if the allocation in certain cells with minimum unit transportation cost were missed. Hence, allocations are made in such a way that the penelty cost is minimized. An initial solution obtained by using this method is nearer to an optimal solution or is the optimal solution itself.
#'
#' @param ex_matrix A cost matrix where last column must be the supply and last row must be the demand. Input matrix should not have any missing values (NA), otherwise function will throw an error. It should be balanced i.e. total demand must be equal to total supply.
#'
#' @return A List which contain the allocation matrix and the total optimized cost.
#'
#' @examples
#' #Input matrix where last row is the Demand and last column is the Supply
#' ex_matrix=data.frame(D1=c(6,3,4,20),E1=c(4,8,4,95),F1=c(1,7,2,35),
#'                      Supply=c(50,40,60,150),row.names = c("A1","B1","C1","Demand"))
#' VAM(ex_matrix)
#'
#' @export
VAM<-function(ex_matrix){
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
    if(sum(colSums(is.na(ex_matrix))<nrow(ex_matrix))==1 |
       sum(rowSums(is.na(ex_matrix))<ncol(ex_matrix))==1){
      ex_matrix[is.na(ex_matrix)]<-High_Values
      tc=which.min(apply(ex_matrix,MARGIN=2,min))
      tr=which.min(apply(ex_matrix,MARGIN=1,min))
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
      ex_matrix[ex_matrix==High_Values]<-NA
    }else{
      R=c()
      C=c()
      for(i in 1:nrow(ex_matrix)){
        rs<-sort(as.numeric(ex_matrix[i,]))
        R[i]<-rs[2]-rs[1]
      }
      for(i in 1:ncol(ex_matrix)){
        cs<-sort(as.numeric(ex_matrix[,i]))
        C[i]<-cs[2]-cs[1]
      }

      if(max(R,na.rm=T)>=max(C,na.rm=T)){
        tr<-which(R==max(R,na.rm=T))[1]
        tc<-which(ex_matrix[tr,]==min(ex_matrix[tr,],na.rm=T))
      }else{
        tc<-which(C==max(C,na.rm=T))[1]
        tr<-which(ex_matrix[,tc]==min(ex_matrix[,tc],na.rm=T))
      }

      min_curr=min(Demand[tc],Supply[tr],na.rm=T)

      Demand[tc]=Demand[tc] - min_curr
      Supply[tr]=Supply[tr] - min_curr
      Alloc_Matrix[tr,tc]=min_curr
      Total_Cost=Total_Cost+(min_curr*ex_matrix[tr,tc])
      if(Demand[tc]==0){
        ex_matrix[,tc]=rep(NA,nrow(ex_matrix))
      }else if(Demand[tc]==Supply[tr]){
        ex_matrix[tr,]=rep(NA,ncol(ex_matrix))
        ex_matrix[,tc]=rep(NA,nrow(ex_matrix))
      }else{
        ex_matrix[tr,]=rep(NA,ncol(ex_matrix))
      }
      Total_alloc=Total_alloc+1
    }
  }

  output=list()
  output$Alloc_Matrix=Alloc_Matrix
  output$Total_Cost=Total_Cost

  #If Supply and Demand are not same
  if(sum(Demand) != 0){
    output$Dummy_demand=sum(Demand)
  } else if(sum(Supply) != 0){
    output$Dummy_supply=sum(Supply)
  }
  if(Total_alloc < (nrow(Alloc_Matrix) + ncol(Alloc_Matrix)-1)){
    warning("Degenracy in Transporation Problem Occurred")
  }
  return(output)
}

