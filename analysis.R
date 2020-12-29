
library(ggplot2)
library(dplyr)

# Step1: importing the csv file world happiness report 2020 into variable whr
whr<- read.csv("World Happiness report 2020.csv")
#View(whr)
# Step2: Simplifying the name of the columns into whr 
names(whr)[1]<-"country"
names(whr)[2]<-"region"
names(whr)[3]<-"ladder_score"
names(whr)[7]<-"GDP"

#colnames(whr)
# Step3 Removing unused columns: 
whr_data <- whr %>% select(-lowerwhisker,-upperwhisker,
                           -Social.support,
                           -Explained.by..Log.GDP.per.capita,
                           -Explained.by..Social.support,
                           -Explained.by..Healthy.life.expectancy,     
                           -Explained.by..Freedom.to.make.life.choices,
                           -Explained.by..Generosity,                  
                           -Explained.by..Perceptions.of.corruption,
                           -Dystopia...residual)



whr_dataset <- whr_data %>%
  dplyr::group_by(region) %>%
  summarise(ladder_score = mean(ladder_score),GDP = mean(GDP))


#geom_barplot()
#plot Bar graph between mean of ladder score per Region and GDP to analysis the correlation between them.
whr_dataset$region <- factor(whr_dataset$region,
                             levels = rev(whr_dataset$region[order(-whr_dataset$ladder_score)]))
ggplot(whr_dataset, aes(x=region, 
                        y=GDP, fill=region)) + geom_bar(stat="identity") + labs(title = "Mean of Ladder_score of Regions Vs GDP for year 2020", subtitle = "") + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, hjust=1))
#boxplot(whr$Ladder_score~Country.name,data = whr$GDP, xlab= "Ladder score",ylab ="GDP", main= "Ladder score(dependent) per Logged GDP per capita(Independent)",col = "orange",border = "brown",horizontal = TRUE,notch = TRUE,varwidth=TRUE)






#to plot the histogram of our dataset, which shows the correlation between ladder score and logged GDP

X2020 <- read.csv("World Happiness Report 2020.csv")
vrb <- lm(X2020$Logged.GDP.per.capita ~ X2020$Ladder.score, data= X2020)
plot(X2020$Ladder.score , X2020$Logged.GDP.per.capita ,
     ylab = "GDP" , 
     xlab = "Ladder score" , 
     main = "histogram of happiness data of 2020" , 
     type = "h" ,
     col = c("green")) 
curve(dnorm(x, mean=mean(X2020$Ladder.score), sd=sd(X2020$Ladder.score)), add=TRUE, col="red")

