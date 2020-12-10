
#Scatter Plot


df <- read.csv("World Happiness Report 2020.csv")
Logged_GDP_per_capita<-df$Logged.GDP.per.capita
Ladder_score<- df$Ladder.score
vrb <- lm(df$Logged.GDP.per.capita ~ df$Ladder.score, data= df)
plot(df$Ladder.score, df$Logged.GDP.per.capita, xlab= 'Ladder Score', ylab='Logged GDP per capita', main="Correlation between Ladder score and Logged GDP per capita", col=c("green","blue")) + abline(vrb, col="red", lwd = 2)
dev.copy(png,'scatter.png')


#Bar plot

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
#plot Bar graph between mean of ladder score per Region and GDP
whr_dataset$region <- factor(whr_dataset$region,
                             levels = rev(whr_dataset$region[order(-whr_dataset$ladder_score)]))
ggplot(whr_dataset, aes(ladder_score, 
                        y=GDP, fill=region)) + geom_bar(stat="identity") + labs(title = "Mean of Ladder_score of Regions Vs GDP for year 2020", subtitle = "") + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, hjust=1))


