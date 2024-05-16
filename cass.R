state_data <- as.data.frame(state.x77)
state_data

View(state_data)
pairs(state_data)
cor(state_data)

names(state_data)[names(state_data) == "Life exp"] <- "Life_Exp"
names(state_data)[names(state_data) == "HS Grad"] <- "HS_Grad"
state_data

library(e1071)
windows(20,12)
pairs(state_data, smooth=FALSE, scale=FALSE, density=TRUE,ellipses=FALSE, method="spearman",
      pch=21, lm=FALSE,cor=TRUE, jiggle =FALSE, factor=2, hist.col=4,stars=TRUE,ci =TRUE)
warnings()

windows(20,12)
par(mfrow=c(4,3))
scatter.smooth(x=state_data$Population, y=state_data$Murder,
               main="Correlation of population  ~Murder",
               xlab="Population(,000",
               ylab="Murder")
scatter.smooth(x=state_data$Income, y=state_data$Murder,
               main="Correlation of Income  ~Murder",
               xlab="Income",
               ylab="Murder%")
scatter.smooth(x=state_data$Illiteracy, y=state_data$Murder,
               main="Correlation of Illiteracy  ~Murder",
               xlab="Illiteracy",
               ylab="Murder%")
scatter.smooth(x=state_data$`Life Exp`, y=state_data$Murder,
               main="Correlation of Life_Exp  ~Murder",
               xlab="Life_Exp",
               ylab="Murder%")
scatter.smooth(x=state_data$HS_Grad, y=state_data$Murder,
               main="Correlation of HS_Grad  ~Murder",
               xlab="HS_Grad",
               ylab="Murder%")
scatter.smooth(x=state_data$Frost, y=state_data$Murder,
               main="Correlation of Frost  ~Murder",
               xlab="Frost",
               ylab="Murder%")
scatter.smooth(x=state_data$Area, y=state_data$Murder,
               main="Correlation of Area  ~Murder",
               xlab="Area",
               ylab="Murder%")
View(state_data)

cor(state_data)
paste("Correlation for murder and forst:",round(cor(state_data$Murder,state_data$Frost),2))
paste("Correlation for murder and population:",round(cor(state_data$Murder,state_data$Population),2))
paste("Correlation for murder and income:",round(cor(state_data$Murder,state_data$Income),2))
paste("Correlation for murder and illiteracy:",round(cor(state_data$Murder,state_data$Illiteracy),2))
paste("Correlation for murder and Life Exp:",round(cor(state_data$Murder,state_data$`Life Exp`),2))
paste("Correlation for murder and hs_grad:",round(cor(state_data$Murder,state_data$HS_Grad),2))
paste("Correlation for murder and area:",round(cor(state_data$Murder,state_data$Area),2))


windows(16,20)

par(mfrow = c(3, 3))

boxplot(state_data$Population, main = "Population")

boxplot(state_data$Income, main = "Income")

boxplot(state_data$Illiteracy, main = "Illiteracy")

boxplot(state_data$`Life Exp`, main = "Life Expectancy")

boxplot(state_data$Murder, main = "Murder")

boxplot(state_data$HS_Grad, main = "HS Graduation")

boxplot(state_data$Frost, main = "Frost")

boxplot(state_data$Area, main = "Area")

outlier_values <-boxplot.stats(state_data$Population)$out
paste0("Population outliers: " , paste0(outlier_values, sep= ","))

#Removing outliers
states<-subset(state_data, state_data$Population!=9111
               &state_data$Population!=10735)
states

states <- state_data[!outliers,state_data$Income]

library(e1071)
windows(20,12)
par(mfrow = c(3, 3))
 plot(density(state_data$Murder),
     main = "Density plot: Murder",
     ylab="Frequency", xlab ="Murder",
     sub= paste0("Skewness:" ,round(e1071::skewness(state_data$Murder),2)))
     polygon(density(state_data$Murder),col="red")

     plot(density(state_data$Population),
          main = "Density plot: Population",
          ylab="Frequency", xlab ="Popuation",
          sub= paste0("Skewness:" ,round(e1071::skewness(state_data$Population),2)))
     polygon(density(state_data$Population),col="red")
     
     plot(density(state_data$Income),
          main = "Density plot: Income",
          ylab="Frequency", xlab ="Income",
          sub= paste0("Skewness:" ,round(e1071::skewness(state_data$Income),2)))
     polygon(density(state_data$Income),col="red")
     
     plot(density(state_data$Illiteracy),
          main = "Density plot: Illiteracy",
          ylab="Frequency", xlab ="Illiteracy",
          sub= paste0("Skewness:" ,round(e1071::skewness(state_data$Illiteracy),2)))
     polygon(density(state_data$Illiteracy),col="red")
     
     plot(density(state_data$`Life Exp`),
          main = "Density plot: `Life Exp`",
          ylab="Frequency", xlab ="`Life Exp`",
          sub= paste0("Skewness:" ,round(e1071::skewness(state_data$`Life Exp`),2)))
     polygon(density(state_data$`Life Exp`),col="red")
     
     plot(density(state_data$HS_Grad),
          main = "Density plot: `HS_Grad`",
          ylab="Frequency", xlab ="`Life Exp`",
          sub= paste0("Skewness:" ,round(e1071::skewness(state_data$HS_Grad),2)))
     polygon(density(state_data$HS_Grad),col="red")
     
     plot(density(state_data$Frost),
          main = "Density plot: Frost",
          ylab="Frequency", xlab ="Frost",
          sub= paste0("Skewness:" ,round(e1071::skewness(state_data$Frost),2)))
     polygon(density(state_data$Frost),col="red")
     
     plot(density(state_data$Area),
          main = "Density plot: Area",
          ylab="Frequency", xlab ="Area",
          sub= paste0("Skewness:" ,round(e1071::skewness(state_data$Area),2)))
     polygon(density(state_data$Area),col="red")
     
     

#Skewness of less than(<) -1 or greater than (>) 1 = highly skewed
#-1 to -0.5 and 0.5 to 1 = moderately skewed
#skewness of -0.5 to 0.5 = approx symetrical
     
paste0("skewness for murder :" ,round(e1071::skewness(state_data$Murder),2))
paste0("skewness for Population :" ,round(e1071::skewness(state_data$Population),2))
paste0("skewness for Life exp :" ,round(e1071::skewness(state_data$`Life Exp`),2))
paste0("skewness for Income :" ,round(e1071::skewness(state_data$Income),2))
paste0("skewness for Hs_Grad :" ,round(e1071::skewness(state_data$HS_Grad),2))
paste0("skewness for Frost :" ,round(e1071::skewness(state_data$Frost),2))
paste0("skewness for Area :" ,round(e1071::skewness(state_data$Area),2))

shapiro.test(state_data$Population)
shapiro.test(state_data$Murder)
shapiro.test(state_data$Area)
shapiro.test(state_data$Income)
shapiro.test(state_data$`Life Exp`)
shapiro.test(state_data$HS_Grad)
shapiro.test(state_data$Frost)

attach(states)
install.packages("MASS")
library(MASS)
View(states)
box_cox_transform<-boxcox(Murder~Population)
box_cox_transform
windows(20,16)
lambda <- box_cox_transform$x[which.max(box_cox_transform$y)]
lambda
normalise_Population <-(Murder^lambda-1)/lambda
normalise_Population

hist(normalise_Population)
shapiro.test(normalise_Population)

states$Population_new <-normalise_Population
shapiro.test(states$Population_new)
states$Income_new <-normalise_Population
shapiro.test(states$Income_new)
states$Illiteracy_new<-normalise_Population
shapiro.test(states$Illiteracy_new)
states$Murder_new <-normalise_Population
shapiro.test(states$Murder_new)
states$HS_Grad_new <-normalise_Population
shapiro.test(states$HS_Grad_new)
states$Frost_new <-normalise_Population
shapiro.test(states$HS_Grad_new)
states$Area_new <-normalise_Population
shapiro.test(states$Area_new)
View(states)

str(states)
attach(states)
model_1<-lm(Murder~Population_new+Income_new+Illiteracy_new+
              Life_Exp_new+HS_Grad_new+Area_new+Frost_new)
model_1



