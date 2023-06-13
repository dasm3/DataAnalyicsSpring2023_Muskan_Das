#Project

EDA


getwd()
setwd('C:/Users/user/Desktop/Lally(Renesslaer)/Spring_Sem/Data Analytics/Project/diseases_csv')
AFG1 <- read.csv("afg_place4_admin1_population_density.csv")#population density
AFG2 <- read.csv("afg_place4_admin1_climate_obs.csv")#climate according to the area and population density


library(dplyr) # for data manipulation
library(ggplot2) # for data visualization
library(data.table) # for fast data processing

head(AFG1)
summary(AFG1) # summary statistics for each variable
summary(AFG2)
str(AFG1) # structure of the data
str(AFG2)

library(ggplot2)
ggplot(AFG1, aes(x = AFG1$TotalLA)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "lightblue") +
  labs(title = "Population Density Distribution",
       x = "Population Density",
       y = "Count")

library(ggplot2)
ggplot(AFG2, aes(x = AFG2$TotalLA)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "lightblue") +
  labs(title = "Climate Distribution",
       x = "Climate Change",
       y = "Count")
attach(AFG1)
AFG1_new <- AFG1[c('UrbanPop','RuralPop','TotalPop','UrbanLA','RuralLA','TotalLA')]
cor_matrix <- cor(AFG1_new)
cor_matrix1 <- data.frame(cor_matrix)
ggplot(data = cor_matrix1, aes(x = rownames(cor_matrix1), y = colnames(cor_matrix1), fill = cor_matrix1)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Correlation Matrix Heatmap")



#scatter plots 
plot(x = AFG1_new$TotalPop, y = AFG1_new$TotalLA)
plot(x = AFG2$TotalPop, y = AFG2$TotalLA)

#boxplots
boxplot(AFG1$TotalPop, AFG1$TotalLA)
boxplot(AFG2$TotalPop, AFG2$TotalLA)

install.packages("corrplot")
library(corrplot)
AFG2_new <- AFG2[c('UrbanPop','RuralPop','TotalPop','UrbanLA','RuralLA','TotalLA')]


my_corr <- cor(AFG1_new, y=NULL, use = "everything", method = c("pearson","kendall", "spearman"))
corrplot(my_corr, method = "color", type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)

my_corr2 <- cor(AFG2_new, y=NULL, use = "everything", method = c("pearson","kendall", "spearman"))
corrplot(my_corr2, method = "color", type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)
