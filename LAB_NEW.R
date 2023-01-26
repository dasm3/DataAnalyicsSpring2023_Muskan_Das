#LAB1 Assignment 1
install.packages("readxl") # installing the package
library(readxl)
getwd()
setwd("C:/Users/user/Documents/Data Analytics/New_LAB")
setwd()
EPI_DF <- read_excel("EPI2010_Data.xls",sheet="EPI2010_all countries")
EPI_data <- read.csv("EPI2010_data_.csv")
EPI_data$EPI

View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI_data$EPI

#EPI2010_data_$EPI #print the column name EPI
tf <- is.na(EPI_data$EPI) #records true values(if val=NA)
E <- EPI_data$EPI[!tf] #filters NON NA values(creates a new array)

summary(EPI_data$EPI) # gives the names(EPI_data$EPI)

#name(EPI_data$EPI) #(name fn doesnt work)

#help("fivenum")
#fivenum(EPI,na.rm = TRUE)

help(stem)
stem(EPI_data$EPI) #the stem fun. doesnt o/p the col EPI

help(hist)
hist(EPI_data$EPI)

X <- rnorm(EPI)
X1 <- rnorm(EPI_regions)
df <- data.frame(X,X1)
df
#library(Hmisc)
#hist.data.frame(df)
#stem(X) #converted to rnorm and then worked
fivenum(EPI_data$EPI,na.rm = TRUE)

#hist(EPI_data$X.13, seq(30.,95.,1.0), probability = TRUE)

help(SJ)
lines(density(EPI_data$EPI,na.rm = TRUE, bw=1))
rug(EPI_data$EPI) #doubt
rug(quantile(EPI_data$EPI), col = 2, lwd = 2.5) #creates tick marks in the base plot


plot(ecdf(EPI_data$EPI), do.points=FALSE, verticals = TRUE) #tried with EPI col and X1
par(pty="s") #Quantiles
qqnorm(EPI_data$EPI); 
qqline(EPI_data$EPI)

qqplot(qt(ppoints(250), df=5), EPI_data$EPI, xlab="Q-Q plot for tdsn")
qqline(EPI_data$EPI)

boxplot(EPI_data$EPI,DALY) 

#------------X-----------------X----------------
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI_data$EPI_regions

#EPI2010_data_$EPI #print the column name EPI
tf <- is.na(EPI_data$EPI_regions) #records true values(if val=NA)
E <- EPI_data$EPI_regions[!tf] #filters NON NA values(creates a new array)

summary(EPI_data$EPI_regions) # gives the names(EPI_data$EPI)


#help("fivenum")
#fivenum(EPI,na.rm = TRUE)

help(stem)
stem(EPI_data$EPI_regions) #the stem fun. doesnt o/p the col EPI

help(hist)
hist(EPI_data$EPI_regions)

X <- rnorm(EPI)
X1 <- rnorm(EPI_regions)
df <- data.frame(X,X1)
df
#library(Hmisc)
#hist.data.frame(df)
#stem(X) #converted to rnorm and then worked
fivenum(X1,na.rm = TRUE)

#hist(EPI_data$X.13, seq(30.,95.,1.0), probability = TRUE)

help(SJ)
lines(density(X1,na.rm = TRUE, bw=1))
rug(X1) #doubt
rug(quantile(X1), col = 2, lwd = 2.5) #creates tick marks in the base plot


plot(ecdf(EPI_regions), do.points=FALSE, verticals = TRUE) #tried with EPI col and X1
par(pty="s") #Quantiles
qqnorm(X1); 
qqline(X1)

qqplot(qt(ppoints(250), df=5), X1, xlab="Q-Q plot for tdsn")
qqline(X1)

boxplot(X1) 
