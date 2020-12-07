
#to plot the histogram of our dataset, which shows the correlation between ladder score and logged GDP

X2020 <- read.csv("World Happiness Report 2020.csv")
plot(X2020$Ladder.score , X2020$Logged.GDP.per.capita ,
     ylab = "GDP" , 
     xlab = "Ladder score" , 
     main = "histogram of happiness data of 2020" , 
     type = "h" ,
     col = c("red","orange","yellow","Green","blue","violet"))



