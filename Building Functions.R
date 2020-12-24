
trial_improve <- function(data,v=c(),wd="")
  setwd(wd)
{
  if(length(v)==0)
  {
    if(!is.data.frame(data))
      stop("The given object is not a data frame")
    
    for(i in 1:ncol(data))
    {
      if(is.numeric(data[,i]))
      {
        
        png(paste(names(data)[i], ".png", sep="")) 
        
        par(mfrow=c(2,1))
        boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
                ylab = names(data)[i], col = "maroon", border = "grey5",
                horizontal = T)
        
        hist(data[,i], main = paste("Histogram of", names(data)[i]), 
             xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
        
        
        
        dev.off()  
        
      }
      {
        mytable <- table(data[,i])
        png(paste(names(data)[i], ".png", sep=""))
        par(mfrow=c(2,1))
        avg = c()
        piepercent=c()
        for(i in 1:length(mytable))
        {
          avg[i]<-mytable[i]
        }
        for(i in 1:length(mytable))
        {
          piepercent[i]<- round(100*mytable[i]/sum(avg), 1)
        }
        
        avg
        piepercent
        lbls
        lbls <- paste(names(mytable), "\n", mytable, sep="")
        
        pie(piepercent,labels = lbls,main="Pie Chart of Species\n (with sample sizes)")
        
        barplot(avg,names.arg = lbls,col="blue",main = "Barplot of Species")
        
        dev.off()
        
      }
      
    }
  }
  if(length(v)!=0)
  {
    if(!is.data.frame(data))
      stop("The given object is not a data frame")
    mydata=data.frame(data[,v]) #
    for(i in 1:ncol(mydata))
    {
      if(is.numeric(mydata[,i]))
      {
        
        png(paste(names(mydata)[i], ".png", sep="")) #NOTE this step
        
        par(mfrow=c(2,1))
        boxplot(mydata[,i], main = paste("Boxplot of", names(mydata)[i]), 
                ylab = names(mydata)[i], col = "maroon", border = "grey5",
                horizontal = T)
        
        hist(mydata[,i], main = paste("Histogram of", names(mydata)[i]), 
             xlab = names(mydata)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
        
        
        
        dev.off()  #NOTE this step
        
      }
      {
        mytable <- table(data[,i])
        png(paste(names(data)[i], ".png", sep=""))
        par(mfrow=c(2,1))
        avg = c()
        piepercent=c()
        for(i in 1:length(mytable))
        {
          avg[i]<-mytable[i]
        }
        for(i in 1:length(mytable))
        {
          piepercent[i]<- round(100*mytable[i]/sum(avg), 1)
        }
        
        avg
        piepercent
        lbls
        lbls <- paste(names(mytable), "\n", mytable, sep="")
        
        pie(piepercent,labels = lbls,main="Pie Chart of Species\n (with sample sizes)")
        
        barplot(avg,names.arg = lbls,col="blue",main = "Barplot of Species")
        
        dev.off()
        
      }
      
    }
  }
}





  
  
  
  
  
  