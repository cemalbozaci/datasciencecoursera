getwd()


file_name = paste(formatC(1, width=3, flag="0"),".csv",sep="")
directory = "specdata"
temp_data = read.csv(paste("./",directory,"/",file_name,sep=""),header = TRUE)

pltmean <- function(directory,type,id=1:332){
  temp_list = list()
  for (i in id){
    file_name = paste(formatC(i, width=3, flag="0"),".csv",sep="")
    temp_data = read.csv(paste("./",directory,"/",file_name,sep=""),header = TRUE)
    temp_list[[i]] = temp_data
  }
  all_data = do.call(rbind,temp_list)  
  mean(all_data[,type],na.rm = TRUE)
}

pltmean(directory = "specdata",type="nitrate",id=70:72)


complete <- function(directory,id=1:332){
  all_data = data.frame(id=integer(),nobs=integer)
  for (i in id){
    file_name = paste(formatC(i, width=3, flag="0"),".csv",sep="")
    temp_data = read.csv(paste("./",directory,"/",file_name,sep=""),header = TRUE)
    temp_complete=complete.cases(temp_data)
    complete_count=table(temp_complete)["TRUE"]
    temp_list=data.frame(id=i,complete_count)
    all_data=rbind(all_data,temp_list,make.row.names=FALSE )
  }
  return(all_data)
}

complete(directory = "specdata",id=1:3)

corr <- function(directory,threshold = 0){
  all_cor = c()
  number_of_files <- length(list.files(directory, pattern="*.csv", full.names=TRUE))
  for (i in 1:number_of_files){
    file_name = paste(formatC(i, width=3, flag="0"),".csv",sep="")
    temp_data = read.csv(paste("./",directory,"/",file_name,sep=""),header = TRUE)
    temp_complete=complete.cases(temp_data)
    complete_count=table(temp_complete)["TRUE"]
    if(!is.na(complete_count)){
     if (complete_count>threshold){
     temp_cor=cor(temp_data[temp_complete,2],temp_data[temp_complete,3])  
     all_cor = c(all_cor,temp_cor)
     }
    }
  }
  return(all_cor)
 }

cr=corr(directory="specdata",threshold = 150)
head(cr)
summary(cr)




