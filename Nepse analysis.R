# USING THE DATA STARTING FROM 1997

Nepse<-read.csv(file.choose())
head(Nepse)
tail(Nepse)

Nepse$X<-NULL
Nepse$X.1<-NULL

#Converting the values from factors to characters and then to numbers

Nepse[,2]<-as.numeric(as.character(Nepse[,2]))

head(Nepse)

#Calculating log returns

Nepse$log_returns<-diff(log(Nepse$Index.value))

#ERROR
  #Solution is to make a vector in which we can add one more cell i.e
    # NA. Hence, NA + our n-1 return from the calculation

Nepse$log_returns<-c(NA,diff(log(Nepse$Index.value)))
head(Nepse)

#Rounding the returns to 4 places after decimal

Nepse$log_returns<-round(Nepse$log_returns,6)
head(Nepse)
tail(Nepse)
library(dplyr)

?mean
logreturns_mean<-mean(Nepse[,"log_returns"], na.rm = TRUE)
logreturns_mean
arith_mean<-mean(Nepse[,"Percentage.Change"],na.rm=TRUE)
arith_mean
#OR it can also be obtained using this: 

mean(Nepse$log_returns,na.rm=TRUE)
head(Nepse)

#Calculating Standard deviation

Nepse_sd<-sd(Nepse$log_returns,na.rm=TRUE)
Nepse_sd

sd(Nepse$Percentage.Change,na.rm=TRUE)
# As you can see from the results, the difference between 
  #sd using log returns and sd using arithmetic returns is 
    # is not so big. However, we shall always use log returns

# Now lets annualize our average daily returns

head(Nepse)
library(PerformanceAnalytics)
Return.annualized(Nepse$log_returns,scale=238,geometric=TRUE)



# What is your annualized avg SD of NEPSE?

head(Nepse)
StdDev.annualized(Nepse$log_returns,scale=238)


# Now, since we already know the mean and sd of the daily returns,
  # Show the dispersion of returns visually, preferably in a histogram

library(ggplot2)
ggplot(data=Nepse,aes(x=log_returns,y=Index.value))
head(Nepse)
# Since, the above code returned a blank plot, we
  # shall be adding some geometry in the code

ggplot(data=Nepse,aes(x=log_returns,y=Index.value)) +
  geom_point()

#Now, lets add more parameters to this geometry. We will start
# with adding colors
#change in plans: color was not possible. maybe, its coz of the structure of
# data itself. But, anyways lets do according to size.
    
q<-ggplot(data=Nepse,aes(x=log_returns,y=Index.value,
                         size=Index.value))
  
q+geom_point()


#We shall be adding more layers now

q+geom_line()


q+geom_point(aes(size=log_returns))

q+geom_point(aes(color=log_returns))

# However, remember that despite the fact that we are overrding the layers,
# the original q remains the same

q+geom_point()

#lets try another one where we change the x and y axis

q+geom_point(aes(x=Index.value,y=log_returns))+
  xlab("Index value") + ylab("log returns")

#Finally, we have come to the point where we will actually
# draw a histogram

s<-ggplot(data=Nepse,
          aes(x=log_returns))
s+geom_histogram(binwidth = 0.001)



#############################################################

#AGAIN NEPSE BUT DATA STARTING FROM 02/02/2014


Nepse1<-read.csv(file.choose())
head(Nepse1)
tail(Nepse1)


#Converting the values from factors to characters and then to numbers

Nepse1[,2]<-as.numeric(as.character(Nepse1[,2]))

head(Nepse1)


#Calculating log returns

Nepse1$log_returns<-c(NA,diff(log(Nepse1$Index.value)))
head(Nepse1)

#Rounding the returns to 4 places after decimal

Nepse1$log_returns<-round(Nepse1$log_returns,6)
head(Nepse1)
tail(Nepse1)
library(dplyr)

?mean
logreturns_mean<-mean(Nepse[,"log_returns"], na.rm = TRUE)
logreturns_mean
arith_mean<-mean(Nepse[,"Percentage.Change"],na.rm=TRUE)
arith_mean
#OR it can also be obtained using this: 

mean(Nepse1$log_returns,na.rm=TRUE)
head(Nepse1)

#Calculating Standard deviation

Nepse1_sd<-sd(Nepse1$log_returns,na.rm=TRUE)
Nepse1_sd

# Now lets annualize our average daily returns

head(Nepse)
library(PerformanceAnalytics)
Return.annualized(Nepse1$log_returns,scale=238,
                  geometric=TRUE)



# What is your annualized avg SD of NEPSE?

head(Nepse1)
StdDev.annualized(Nepse1$log_returns,scale=238)


# Now, since we already know the mean and sd of the daily returns,
# Show the dispersion of returns visually, preferably in a histogram

s1<-ggplot(data=Nepse1,
          aes(x=log_returns))
s1+geom_histogram(binwidth = 0.001,col="blue")

#Also plotting how log returns and index value interact
head(Nepse1)
ni<-ggplot(data=Nepse1,aes(x=Index.value,
                               y=log_returns))
ni+geom_point()

####################################################

#Now starting from 04/04/2011


Nepse2<-read.csv(file.choose())
head(Nepse2)
tail(Nepse2)


#Converting the values from factors to characters and then to numbers

Nepse2[,2]<-as.numeric(as.character(Nepse2[,2]))

head(Nepse2)

library(psych)

#Calculating log returns

Nepse2$log_returns<-c(NA,diff(log(Nepse2$Index.value)))
head(Nepse2)

#Rounding the returns to 5 places after decimal

Nepse2$log_returns<-round(Nepse2$log_returns,5)
head(Nepse2)
tail(Nepse2)

describe(Nepse2$log_returns)
summary(Nepse2$log_returns)

library(dplyr)

?mean
logreturns_mean<-mean(Nepse2[,"log_returns"], na.rm = TRUE)
logreturns_mean

#OR it can also be obtained using this: 

mean(Nepse2$log_returns,na.rm=TRUE)
head(Nepse2)

#Calculating Standard deviation

Nepse2_sd<-sd(Nepse2$log_returns,na.rm=TRUE)
Nepse2_sd

# Now lets annualize our average daily returns

head(Nepse2)
library(PerformanceAnalytics)
Return.annualized(Nepse2$log_returns,scale=238,
                  geometric=TRUE)



# What is your annualized avg SD of NEPSE?

head(Nepse2)
StdDev.annualized(Nepse2$log_returns,scale=238)


# Now, since we already know the mean and sd of the daily returns,
# Show the dispersion of returns visually, preferably in a histogram

library(ggplot2)
s2<-ggplot(data=Nepse2,
           aes(x=log_returns))
s2+geom_histogram(binwidth = 0.001,col="blue")

#Also plotting how log returns and index value interact
tail(Nepse2)
head(Nepse2)
ni2<-ggplot(data=Nepse2,aes(x=Index.value,
                           y=log_returns))
ni2+geom_point()



