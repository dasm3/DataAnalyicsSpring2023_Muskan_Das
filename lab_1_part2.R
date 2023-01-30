#Lab_1-2

getwd()
setwd("C:/Users/user/Desktop/Lally(Renesslaer)/Spring_Sem/Data Analytics/New_LAB")
install.packages("readxl") # installing the package
library(readxl)
EPI_DF <- read_excel("EPI2010_data.xls",sheet="EPI2010_all countries")
EPI_data <- read.csv("EPI2010_data_.csv")
EPI_data <- read.csv(file.choose(), header=TRUE)
EPI_data
View(EPI_data)

#names(EPI_data) <- as.matrix(EPI_data[1, ]) #to print the first col that has been deleted
#EPI_data <- EPI_data[-1, ]
#EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character(x)))
#EPI_data  
#View(EPI_data)

attach(EPI_data)
fix(EPI_data)
EPI_data$EPI
tf <- is.na(EPI_data$EPI)
E <- EPI_data$EPI[!tf]
summary(EPI_data$EPI)
fivenum(EPI_data$EPI, na.rm = TRUE)

stem(EPI_data$EPI)
hist(EPI_data$EPI)
hist(EPI_data$EPI, seq(30., 95., 1.0), prob=TRUE)

lines(density(EPI_data$EPI, na.rm = TRUE, bw=1))
lines(density(EPI_data$EPI, na.rm = TRUE, bw="SJ"))
rug(EPI_data$EPI)
plot(ecdf(EPI_data$EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$EPI); qqline(EPI_data$EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab = "Q-Q plot for t dsn")
qqline(x)


#DALY
EPI_data$DALY
tf <- is.na(EPI_data$DALY)
E <- EPI_data$DALY[!tf]
summary(EPI_data$DALY)
fivenum(EPI_data$DALY, na.rm = TRUE)

stem(EPI_data$DALY)
hist(EPI_data$DALY)


lines(density(EPI_data$DALY, na.rm = TRUE, bw=1))
lines(density(EPI_data$DALY, na.rm = TRUE, bw="SJ"))
rug(EPI_data$DALY)
plot(ecdf(EPI_data$DALY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$DALY); qqline(EPI_data$DALY)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab = "Q-Q plot for t dsn")
qqline(x)

#WATER_H

EPI_data$WATER_H
tf <- is.na(EPI_data$WATER_H)
E <- EPI_data$WATER_H[!tf]
summary(EPI_data$WATER_H)
fivenum(EPI_data$WATER_H, na.rm = TRUE)

stem(EPI_data$WATER_H)
hist(EPI_data$WATER_H)


lines(density(EPI_data$WATER_H, na.rm = TRUE, bw=1))
lines(density(EPI_data$WATER_H, na.rm = TRUE, bw="SJ"))
rug(EPI_data$WATER_H)
plot(ecdf(EPI_data$WATER_H), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$WATER_H); qqline(EPI_data$DALY)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_data$ENVHEALTH), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$ENVHEALTH); qqline(EPI_data$ENVHEALTH)
x <- seq(30,95,1)

plot(ecdf(EPI_data$ECOSYSTEM), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$ECOSYSTEM); qqline(EPI_data$ECOSYSTEM)
x <- seq(30,95,1)

plot(ecdf(EPI_data$AIR_E), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$AIR_E); qqline(EPI_data$AIR_E)
x <- seq(30,95,1)

plot(ecdf(EPI_data$AIR_H), do.points=FALSE, verticals = TRUE)
par(pty="s")
qqnorm(EPI_data$AIR_H); qqline(EPI_data$AIR_H)
x <- seq(30,95,1)

plot(ecdf(EPI_data$BIODIVERSITY), do.points=FALSE, verticals = TRUE)
par(pty="s")
qqnorm(EPI_data$BIODIVERSITY); qqline(EPI_data$BIODIVERSITY)
x <- seq(30,95,1)



#COMPARISIONS
boxplot(EPI,DALY)
qqplot(EPI,DALY)
boxplot(EPI,ENVHEALTH)
qqplot(EPI,ENVHEALTH)
qqplot(EPI,ECOSYSTEM)
qqplot(EPI,BIODIVERSITY)
qqplot(EPI,AIR_H)
qqplot(DALY,AIR_E)
qqplot(ENVHEALTH,ECOSYSTEM)


#Filtering
EPILand <- EPI_data$EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)


#DOUBT - HOW TO FILTER GEO_REGION AND EPI_REGION DATA
EPI_GEO <- EPI_data$EPI[!Desert]
EGEO <- EPI_GEO[!is.na(EPI_GEO)]
hist(EGEO)
hist(EGEO, seq(30., 95., 1.0), prob=TRUE)

EPI_data$GEO_subregion
tf <- is.na(EPI_data$GEO_subregion)
E <- EPI_data$GEO_subregion[!tf]
summary(EPI_data$GEO_subregion)

filter(EPI_data,GEO_subregion=='Western Europe')
filter(EPI_data,EPI_regions=='Europe')
#---------X---------X---------X----------
#PART-2

plot(ecdf(EPI_data$EPI), do.points=FALSE, verticals=TRUE)
help("qqnorm")
par(pty="s")
qqnorm(EPI_data$EPI); qqline(EPI_data$EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x,xlab="Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_data$EPI), do.points=FALSE, verticals = TRUE)
plot(ecdf(EPI_data$EPI), do.points=TRUE, verticals = TRUE)
par(pty="s")
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI)

x <- seq(30,95,1)
x
x1 <- seq(30,95,2)
x1
x1 <- seq(30,96,2)
x1
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot")
qqline(x)
library(dplyr)
help("dplyr-package")
