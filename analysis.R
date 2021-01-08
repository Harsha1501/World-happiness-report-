library(ggplot2)
library(dplyr)
#importing libraries
whr<- read.csv("World Happiness report 2020.csv")

#Using Pearson's r method to check the correlation
x<- as.numeric(whr$Logged.GDP.per.capita)
y<-as.numeric(whr$Ladder.score)
cor(x[!is.na(x)],y[!is.na(x)],method = "pearson")
#Value is 0.7753744

#To found of the slope and the intercept

lm(formula = y[!is.na(x)]~x[!is.na(x)])
 
# Performing the test to find out the p-value

cor.test(x[!is.na(x)], y[!is.na(x)], method="pearson")


#p-value < 2.2e-16 which is less than 0.05 so we can reject
#null hypothesis and process towards the alternative hypothesis

# ploting diffrent graphs for finding more evidence for alternative hypothesis


#Loading Data-set into a variable called df1.
df1 <- read.csv("World Happiness report 2020.csv")

#Plotting the Scatter-plot using the appropriate columns as per our Research Question 
vrb <- lm(df1$Logged.GDP.per.capita ~ df1$Ladder.score, data=df1)
plot(df1$Ladder.score , df1$Logged.GDP.per.capita , xlab = 'Ladder Score', ylab='Logged GDP per capita', main="Correlation between Ladder score and Logged GDP per capita", col=c("green","blue")) + abline(vrb, col="red", lwd = 2)
dev.copy(png,'plot1.png') #Saving the plot

#Correlation-Coefficient for above plot 
x<-df1$Ladder.score
y<-df1$Logged.GDP.per.capita

cor_coef <- cor.test(x[!is.na(x)],y[!is.na(y)], method = c("pearson"))
print(cor_coef)


whr<- read.csv("World Happiness report 2020.csv")
#View(whr)
#Step2: Simplifying the name of the columns into whr 

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
dev.copy(png,'plot2.png')
