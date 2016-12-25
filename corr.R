setwd("~/Desktop/Online Coursera/Coursera-R-Programming/week2/")
source("complete.R")
corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
        m<-c()
	  as.numeric(m)
	  removeNA=TRUE
	  data<-complete(directory,1:332)
	  nobs<-data$nobs
	  id<-data$id[nobs > threshold]
	  as.numeric(id)
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
                        d<-read.csv(id1)
                        m<-c(m,cor(d$sulfate,d$nitrate,use="complete.obs"))
                        }
       m
}


# tests
##cr<-corr("specdata",150)
##head(cr)
##[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
##summary(cr)
##    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##-0.21060 -0.04999  0.09463  0.12530  0.26840  0.76310 

##cr<-corr("specdata",400)
##head(cr)
##[1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
##summary(cr)
##    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##-0.17620 -0.03109  0.10020  0.13970  0.26850  0.76310 

##cr<-corr("specdata",5000)
##summary(cr)
##Length  Class   Mode 
##     0   NULL   NULL 
##length(cr)
##[1] 0

##cr<-corr("specdata")
##summary(cr)
##    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##-1.00000 -0.05282  0.10720  0.13680  0.27830  1.00000 
##length(cr)
##[1] 323
