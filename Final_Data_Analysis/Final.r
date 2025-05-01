library(reshape2)
library(ggplot2)
# read in the table used
FoodData = read.csv('fruit-and-vegetable-consumption-in-california-residents-20122013.csv',
                    sep = ',',
                    header = TRUE)


SugarData = read.csv('sugar-sweetened-beverage-consumption-in-california-residents-20122013.csv',
                     sep = ',',
                     header = TRUE)


#prepare and join the data to wanted to rows of the datasets 

FoodObesityRates =  data.frame(subset(FoodData, Category == "Overweight Status" & Type == "Overweight/Obese"))
View(FoodObesityRates)

SugarObesityRates =  data.frame(subset(SugarData, Category == "Overweight Status" & Type == "Overweight/Obese"))
View(SugarObesityRates)


# I have continue break down the file where it had differnt factors in place
adolescentSugarStatus = data.frame(subset(SugarData, Age.Group =="Adolescent (12-17)"))

adolescentFoodStatus = data.frame(subset(FoodData, Age.Group == "Adolescent (12-17)"))



adultSugarStatus = data.frame(subset(SugarData, Age.Group == "Adult (18+)"))

adultFoodStatus = data.frame(subset(FoodData, Age.Group == "Adult (18+)"))



childSugarStatus = data.frame(subset(SugarData, Age.Group == "Child (6-11)"))

childFoodStatus = data.frame(subset(FoodData, Age.Group == "Child (6-11)"))
  
# perform some type of analysis with the correlation with some modeling techinque
  #rename some columns the datasets
  colnames(FoodObesityRates)[4] = "Type"
  colnames(FoodObesityRates)[5] = "Food_more_serving_fruit_vegetables"
  colnames(SugarObesityRates)[5] = "Mean_Serving_Sugar_Intake"

# The first question: 
food_fit = lm(data=FoodObesityRates,formula= Year ~ Food_more_serving_fruit_vegetables)
summary(food_fit)

sugar_fit = lm(data=SugarObesityRates,formula= Year ~ Mean_Serving_Sugar_Intake)
summary(sugar_fit)



# Develop at least two visualization in support of your analysis and conclusions 
Foodplot = ggplot(food_fit, aes(x=Year,y=Food_more_serving_fruit_vegetables)) +
  geom_point(color="darkgreen", size = 4) +
  geom_line() +
  xlab("Year") +
  ylab("Fruit_vegtables consumption of 5 or more servings") +
  ggtitle("Healthly Food consumption with Obesity with the Year of 2012-2013")

# I wanted to see what the different between age groups with thoses consuming 5 or more serving of fruit and vegtables
FoodDivsion = ggplot(FoodObesityRates, aes(x=Age.Group,y=Food_more_serving_fruit_vegetables)) +
  geom_bar(stat="identity", fill= "lightblue") + 
  xlab("Age Group") +
  ylab("Fruit_vegtables consumption of 5 or more servings") +
  ggtitle("Healthly Food consumption with Obesity status between Age Group")

Sugarplot = ggplot(sugar_fit, aes(x=Year,y=Mean_Serving_Sugar_Intake)) +
  geom_point(color="orange", size = 4) +
  geom_line() +
  xlab("Year") +
  ylab("Average serving of sugar consumption ") +
  ggtitle("Sugar Drinks consumption with Obesity with the Year of 2012-2013")

print(Sugarplot)
print(Foodplot)
#print(FoodDivsion)


# Store the products of collating, analysis, and visualizations
write.table(FoodObesityRates, file="FoodObesityRates.csv", sep=",")
write.table(SugarObesityRates, file="SugarObesityRates.csv", sep=",")
save(food_fit, file="food_fit.Rdata")
save(sugar_fit, file="sugar_fit.Rdata")
