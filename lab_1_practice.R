#LAB1 Assignment 1
install.packages("readxl") # installing the package
library(readxl)
getwd()
setwd("C:/Users/user/Desktop/Data Analytics/New_LAB")

EPI_DF <- read_excel("EPI2010_Data.xls",sheet="EPI2010_all countries")
EPI_data <- read.csv("EPI2010_data_.csv")

View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI

#EPI2010_data_$EPI #print the column name EPI
tf <- is.na(EPI_data$X.13) #records true values(if val=NA)
E <- EPI_data$X.13[!tf] #filters NON NA values(creates a new array)

summary(EPI) # gives the names(EPI_data$EPI)

install.packages("name")
library(name)
name(EPI) #(name fn doesnt work)

#help("fivenum")
#fivenum(EPI,na.rm = TRUE)

help(stem)
stem(EPI) #the stem fun. doesnt o/p the col EPI

help(hist)
hist(EPI)

X <- rnorm(EPI)
X1 <- rnorm(EPI_regions)
df <- data.frame(X,X1)
df
library(Hmisc)
hist.data.frame(df)
stem(X) #converted to rnorm and then worked
fivenum(X,na.rm = TRUE)

#hist(EPI_data$X.13, seq(30.,95.,1.0), probability = TRUE)

help(SJ)
lines(density(X,na.rm = TRUE, bw=1))
rug(X) #doubt
rug(quantile(X), col = 2, lwd = 2.5) #creates tick marks in the base plot


plot(ecdf(X), do.points=FALSE, verticals = TRUE) #tried with EPI col and X1
par(pty="s") #Quantiles
qqnorm(X); 
qqline(X)

qqplot(qt(ppoints(250), df=5), X, xlab="Q-Q plot for tdsn")
qqline(X)

boxplot(EPI,DALY) 
