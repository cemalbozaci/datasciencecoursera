source(file = "~/cemal/useful_functions_bycemal/basic_data_manuplation.R")

MyData <- read.csv(file="M:\\coursera\\hospital-data.csv", header=TRUE, sep=",")
MyData2 <- read.csv(file="M:\\coursera\\outcome-of-care-measures.csv", header=TRUE, sep=",")

zaa = MyData2[,c(1,2,11,17,23)]
zaa2 = merge(zaa,MyData,all.x = 1,all.y = 0,by.x="Provider.Number",by.y="Provider.Number")
colnames(zaa2)[3:5] <- c('HA','HF','PN')

zaa2[,c(3,4,5)] <- as.numeric(as.character(unlist(zaa2[, c(3,4,5)])))

best = function(state,outcome) {
  
  if (!(outcome %in% c("heart attack","heart failure","pneumonia")) ){
    print("invalid outcome")
    return(NULL)
  }
  if (!(state %in%  zaa2$State)){
    print("invalid state")
    return(NULL)
  }
  
  if(outcome == "heart attack"){
    min_val = min(zaa2[which(zaa2$State==state),3],na.rm = T)
    min(as.character(zaa2[which(zaa2$HA==min_val & zaa2$State == state ),2]),na.rm=T)
  }
  else if(outcome == "heart failure"){
    min_val = min(zaa2[which(zaa2$State==state),4],na.rm = T)
    min(as.character(zaa2[which(zaa2$HF==min_val & zaa2$State == state),2]),na.rm=T)
  }
  else if(outcome == "pneumonia"){
    min_val = min(zaa2[which(zaa2$State==state),5],na.rm = T)
    min(as.character(zaa2[which(zaa2$PN==min_val & zaa2$State == state),2]),na.rm=T)
  }
  
}


best("XX","pneumonia")
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")




rankhospital = function(state,outcome,sira){
  
  if (!(outcome %in% c("heart attack","heart failure","pneumonia")) ){
    print("invalid outcome")
    return(NULL)
  }
  if (!(state %in%  zaa2$State)){
    print("invalid state")
    return(NULL)
  }
  
  if(outcome == "heart attack"){
    zaa3 = zaa2[order(zaa2[,3],zaa2[,2]),]  
    zaa3 = zaa3[complete.cases(zaa3[,3]),]
    zaa3 = zaa3[which(zaa3$State==state),]
    zaa3$rnk = 1:nrow(zaa3)
    
    if(sira == "best") return(zaa3[which(zaa3$rnk == 1),2])  
    else if(sira == "worst") return(zaa3[which(zaa3$rnk == nrow(zaa3)),2])    
    else if(sira > nrow(zaa3)){
      print("sira gt available rnk") 
      return(NULL) 
    }
    else return(zaa3[which(zaa3$rnk == sira),2])
  }
  
  if(outcome == "heart failure"){
    zaa3 = zaa2[order(zaa2[,4],zaa2[,2]),]  
    zaa3 = zaa3[complete.cases(zaa3[,4]),]
    zaa3 = zaa3[which(zaa3$State==state),]
    zaa3$rnk = 1:nrow(zaa3)
    
    if(sira == "best") return(zaa3[which(zaa3$rnk == 1),2])  
    else if(sira == "worst") return(zaa3[which(zaa3$rnk == nrow(zaa3)),2])    
    else if(sira > nrow(zaa3)){
      print("sira gt available rnk") 
      return(NULL) 
    }
    else return(zaa3[which(zaa3$rnk == sira),2])
  }
  
  if(outcome == "pneumonia"){
    zaa3 = zaa2[order(zaa2[,5],zaa2[,2]),]  
    zaa3 = zaa3[complete.cases(zaa3[,5]),]
    zaa3 = zaa3[which(zaa3$State==state),]
    zaa3$rnk = 1:nrow(zaa3)
    
    if(sira == "best") return(zaa3[which(zaa3$rnk == 1),2])  
    else if(sira == "worst") return(zaa3[which(zaa3$rnk == nrow(zaa3)),2])    
    else if(sira > nrow(zaa3)){
      print("sira gt available rnk") 
      return(NULL) 
    }
    else return(zaa3[which(zaa3$rnk == sira),2])
  }
  
}


rankhospital("MD", "pneumonia", "worst")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)


rankall = function(outcome,sira){
  
  if (!(outcome %in% c("heart attack","heart failure","pneumonia")) ){
    print("invalid outcome")
    return(NULL)
  }
  
  if(outcome == "heart attack"){
    zaa3 = zaa2[order(zaa2[,11],zaa2[,3],zaa2[,2]),]  
    zaa3 = zaa3[complete.cases(zaa3[,3]),]
    
    zaa3$rnk <- sapply(1:nrow(zaa3), 
                       function(i) sum(zaa3[1:i, c('State')]==zaa3$State[i]))    
    
    if(sira == "best") zaa4 = zaa3[which(zaa3$rnk == 1),c(2,11)]  
    else if(sira == "worst"){
      zaa3 = zaa2[order(zaa2[,11],zaa2[,3],zaa2[,2],decreasing = TRUE),] 
      zaa3$rnk <- sapply(1:nrow(zaa3), 
                         function(i) sum(zaa3[1:i, c('State')]==zaa3$State[i])) 
      zaa4 = zaa3[which(zaa3$rnk == 1),c(2,11)]    
    }
    else zaa4 = zaa3[which(zaa3$rnk == sira),c(2,11)]
  }
  
  if(outcome == "heart failure"){
    zaa3 = zaa2[order(zaa2[,11],zaa2[,4],zaa2[,2]),]  
    zaa3 = zaa3[complete.cases(zaa3[,4]),]
    
    zaa3$rnk <- sapply(1:nrow(zaa3), 
                       function(i) sum(zaa3[1:i, c('State')]==zaa3$State[i]))    
    
    if(sira == "best") zaa4 = zaa3[which(zaa3$rnk == 1),c(2,11)]  
    else if(sira == "worst"){
      zaa3 = zaa2[order(zaa2[,11],zaa2[,4],zaa2[,2],decreasing = TRUE),] 
      zaa3$rnk <- sapply(1:nrow(zaa3), 
                         function(i) sum(zaa3[1:i, c('State')]==zaa3$State[i])) 
      zaa4 = zaa3[which(zaa3$rnk == 1),c(2,11)]    
    }
    else zaa4 = zaa3[which(zaa3$rnk == sira),c(2,11)]
  }
  
  if(outcome == "pneumonia"){
    zaa3 = zaa2[order(zaa2[,11],zaa2[,5],zaa2[,2]),]  
    zaa3 = zaa3[complete.cases(zaa3[,5]),]
    
    zaa3$rnk <- sapply(1:nrow(zaa3), 
                       function(i) sum(zaa3[1:i, c('State')]==zaa3$State[i]))    
    
    if(sira == "best") zaa4 = zaa3[which(zaa3$rnk == 1),c(2,11)]  
    else if(sira == "worst"){
      zaa3 = zaa2[order(zaa2[,11],zaa2[,5],zaa2[,2],decreasing = TRUE),] 
      zaa3$rnk <- sapply(1:nrow(zaa3), 
                         function(i) sum(zaa3[1:i, c('State')]==zaa3$State[i])) 
      zaa4 = zaa3[which(zaa3$rnk == 1),c(2,11)]    
    }
    else zaa4 = zaa3[which(zaa3$rnk == sira),c(2,11)]
  }
  
  assign("zaa4", zaa4, envir = .GlobalEnv) 
  
}  

rankall("pneumonia","best")
r <- rankall("heart attack", 4)
r <- rankall("pneumonia", "worst")
r <- rankall("heart failure", 10)

View(r)

