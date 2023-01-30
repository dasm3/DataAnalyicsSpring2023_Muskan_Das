getwd()
setwd("C:/Users/user/Desktop/Lally(Renesslaer)/Spring_Sem/Data Analytics/New_LAB")

multivar <- read.csv("multivariate.csv")
multivar <- read.csv(file.choose(), header=TRUE)
multivar
View(multivar)

head(multivar)
attach(multivar)
help(lm) # 1.caaries out regresssion, 2. fits linear and multivariate
mm <- lm(Homeowners~Immigrant) #mm-r object
summary(mm)$coef
plot(Homeowners~Immigrant)

help("abline") #adds one-more straight lines to a plot
abline(mm)
abline(mm,col=2,lwd=3)




#try later
abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients
