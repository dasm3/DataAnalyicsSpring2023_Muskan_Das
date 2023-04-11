#SVM_Exercise

data("iris")
head(iris)
str(iris)

library(ggplot2)
library(e1071)

qplot(Petal.Length, Petal.Width, data=iris, color=Species)

help("svm")
Model_SVM <- svm(Species~., data=iris)
summary(Model_SVM)

plot(Model_SVM, data=iris, Petal.Width~Petal.Length, slice= list(Sepal.Width=3, Sepal.Length=4))

#Prediction
pred <- predict(Model_SVM, iris)

#Creation of Table
Table_new <- table(Predicted = pred, Actual = iris$Species)
Table_new


#Model1 accuracy calc.
Model_SVMaccuracy = sum(diag(Table_new))/sum(Table_new)
Model_SVMaccuracy

#The miscalc. rate
Miscal_Rate <- 1- Model_SVMaccuracy
Miscal_Rate

#Model2 Kernel-linear

Model_SVM2 <- svm(Species~., data=iris, kernel="linear")

summary(Model_SVM2)
#ploting the secon model
#Interpretation of the plot
plot(Model_SVM2, data=iris, Petal.Width~Petal.Length, slice = list(Sepal.Width=3, Sepal.Length=4))


#prediction second model
pred1 <- predict(Model_SVM2, iris)

#table
table_new1 <- table(Predicted = pred1, Actual=iris$Species)
table_new1

#accuracy
Model_SVM2accu <- sum(diag(table_new1))/sum(table_new1)

#misclassification
Model_SVM2misclassi <- 1- Model_SVM2accu

Model_SVM2accu
Model_SVM2misclassi


#Model3 Kernel-polynomial

Model_SVM3 <- svm(Species~., data=iris, kernel="polynomial")

summary(Model_SVM3)
#ploting the secon model
#Interpretation of the plot
plot(Model_SVM3, data=iris, Petal.Width~Petal.Length, slice = list(Sepal.Width=3, Sepal.Length=4))


#prediction second model
pred2 <- predict(Model_SVM3, iris)

#table
table_new2 <- table(Predicted = pred2, Actual=iris$Species)
table_new2

#accuracy
Model_SVM3accu <- sum(diag(table_new2))/sum(table_new2)

#misclassification
Model_SVM3misclassi <- 1- Model_SVM3accu

Model_SVM3accu
Model_SVM3misclassi
