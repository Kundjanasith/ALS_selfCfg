file <- read.csv('data/ratings.csv')
for( i in 1:71500){
    if(i%%500==0||i==1){
       print(i)
       a <- subset(file,file$user_id>=1&file$user_id<=i)
       output_path <- paste("data_R/",i,".csv",sep="")
       write.csv(a,file=output_path,row.names=FALSE)
    }
}
