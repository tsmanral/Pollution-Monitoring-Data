setwd("~/Desktop/Online Coursera/Coursera-R-Programming/week2/")


pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
	m<-c()
	as.numeric(m);
	removeNA=TRUE
	for(i in id) {
		a<-i%/%10
		if(a==0) {
			id1<-paste(directory,"/00",i,".csv",sep="")
			   }
		else if((a>0)&&(a<10)) {
			id1<-paste(directory,"/0",i,".csv",sep="")
					}
		else {
			id1<-paste(directory,"/",i,".csv",sep="")
		     }
			data<-read.csv(id1)
			mea<-data[,pollutant]
			m<-c(m,mea)
			   }
	mean(m,na.rm=removeNA)
}

# tests
pollutantmean("specdata", "sulfate", 1:10) == 4.064128
pollutantmean("specdata", "nitrate", 70:72) == 1.706047
pollutantmean("specdata", "nitrate", 23) == 1.280833
