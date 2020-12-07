#Ridhit bhasin
#importing the csv file world happiness report 2020 into variable whr
whr<- read.csv("World Happiness report 2020.csv")
View(whr)
#Simplifying the name of the columns into whr 
names(whr)[3]<-"Ladder_score"
names(whr)[7]<-"GDP"
#plot Box graph between ladder score and GDP
boxplot(whr$Ladder_score~Country.name,data = whr$GDP, xlab= "Ladder score",ylab ="GDP", main= "Ladder score(dependent) per Logged GDP per capita(Independent)",col = "orange",border = "brown",
        horizontal = TRUE,notch = TRUE,varwidth=TRUE)

